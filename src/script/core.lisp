;;;; src/script/core.lisp — the scripting context and the node-wrapping kernel.
;;;;
;;;; One CONTEXT per document holds the shuttle realm, the weft DOM, the shared
;;;; DOM prototypes (Node/Element/Document/Text/Comment/…), the node<->wrapper
;;;; identity maps, the computed-style cache, and the macrotask (timer) queue.
;;;; Every wrapper is a plain JS object whose [[Prototype]] carries the DOM
;;;; methods; the wrapper's backing weft node is found through OBJ-NODES, so the
;;;; prototype methods read `this` and recover the live node.
(in-package #:weft.script)

(defun %split-char (ch s)
  "Split S on character CH into a list of non-empty substrings."
  (loop with start = 0 with out = '()
        for i = (position ch s :start start)
        do (let ((piece (subseq s start (or i (length s)))))
             (when (plusp (length piece)) (push piece out)))
           (if i (setf start (1+ i)) (return (nreverse out)))))

(defstruct (context (:constructor %make-context))
  realm                     ; the shuttle realm (one per document)
  document                  ; the weft DOM document (an h:dnode of kind :document)
  document-obj              ; the JS `document` host object
  (node-objs (make-hash-table :test 'eq))  ; weft dnode -> its JS wrapper (identity)
  (obj-nodes (make-hash-table :test 'eq))  ; JS wrapper -> weft dnode
  (protos nil)              ; plist: :node :element :document :text :comment :fragment
                            ;        :nodelist :style :event :window
  (css "")                  ; author CSS string (for getComputedStyle recompute)
  (styles (make-hash-table :test 'eq)) ; document dnode -> its computed-style hash
  (width 800)               ; layout width used for style resolution
  (timers nil)              ; pending macrotasks: list of TIMER structs
  (timer-seq 0)             ; monotonic timer id source
  (now 0)                   ; virtual clock (ms) for the timer queue
  (start-real-time (get-internal-real-time)) ; navigation-start (High Resolution Time origin)
  (perf-marks (make-hash-table :test 'equal)) ; performance.mark name -> most-recent start time (ms)
  (perf-entries nil)        ; performance timeline entries, oldest first (mark/measure host objects)
  (iframe-docs (make-hash-table :test 'eq)) ; iframe/object dnode -> its content document
  (iframe-windows (make-hash-table :test 'eq)) ; iframe/object dnode -> its content window object
  (frame-fragments (make-hash-table :test 'eq)) ; content document -> its URL fragment (sans '#'), for :target
  (canvas-ctxs (make-hash-table :test 'eq)) ; <canvas> dnode -> its CanvasRenderingContext2D host object
  (listeners (make-hash-table :test 'eq))   ; dnode -> list of (type listener capture) entries
  (events (make-hash-table :test 'eq))      ; event wrapper object -> its EVT struct
  (traversal (make-hash-table :test 'eq))   ; NodeIterator/TreeWalker wrapper -> its state
  (ranges (make-hash-table :test 'eq))      ; Range wrapper -> its RG struct
  (ns-info (make-hash-table :test 'eq))     ; element dnode -> (:ns uri :prefix p :local l) for createElementNS
  (attr-recs (make-hash-table :test 'eq))   ; attribute cons-cell (qname . value) -> ATTR-REC (namespace/owner metadata)
  (attr-objs (make-hash-table :test 'eq))   ; attribute cons-cell -> its JS Attr wrapper (identity)
  (attr-of (make-hash-table :test 'eq))     ; JS Attr wrapper -> its ATTR-REC
  (owner-docs (make-hash-table :test 'eq))  ; created node -> its owner document (even while detached)
  (doc-content-types (make-hash-table :test 'eq)) ; document dnode -> its contentType (default text/html)
  (blank-url-docs (make-hash-table :test 'eq)) ; document dnode -> t when its URL is "about:blank" (createHTMLDocument/createDocument)
  (xml-documents (make-hash-table :test 'eq)) ; document dnode -> t when it is an XMLDocument (createDocument; not a DOMParser result)
  (input-values (make-hash-table :test 'eq)) ; <input> node -> its independent value property
  (focus nil)               ; the focused element dnode; NIL means the body (document.activeElement)
  ;; Text selection per control: node -> (start end . direction).  ONE table,
  ;; because there is one selection — `input.selectionStart' and the caret the
  ;; user watches move under the arrow keys are the same thing, and storing them
  ;; separately is how they drift apart.  A caret is the collapsed case (s = e).
  (selections (make-hash-table :test 'eq))
  ;; <select> nodes whose selectedness was explicitly cleared (selectedIndex=-1),
  ;; suppressing the "ask for a reset" auto-selection of the first option.  On the
  ;; context, not in a closure, because the shell picks options too.
  (deselected (make-hash-table :test 'eq))
  (edit-start nil)          ; the focused control's value when it gained focus (for `change')
  (shadow-roots (make-hash-table :test 'eq)) ; element dnode -> shadow root (DocumentFragment dnode)
  (on-handlers (make-hash-table :test 'eq))  ; node -> (equal hash "type" -> handler fn)
  (write-buffers (make-hash-table :test 'eq)) ; document node -> pending document.write buffer (open())
  (base "")                 ; base URL/directory for resolving subresource references
  (loader nil)              ; (ctx url) -> (values kind content); NIL disables file/network loads
  (cookie "")               ; document.cookie backing store
  (current-script nil)      ; the <script> node currently executing (for document.write)
  (ran-scripts (make-hash-table :test 'eq)) ; <script> nodes already executed (run once)
  ;; ---- MutationObserver (DOM §4.3) ----
  (mo-enabled nil)          ; T once any observe() ran — the zero-observer fast-path gate
  (mo-list nil)             ; every MO struct in this context (creation order, newest first)
  (mo-objs (make-hash-table :test 'eq)) ; MO wrapper object -> its MO struct
  (mo-regs (make-hash-table :test 'eq)) ; node -> list of MO-REG registered observers
  (mo-microtask-queued nil) ; the "mutation observer microtask queued" flag (DOM §4.3.3)
  (raf-count 0)             ; requestAnimationFrame frames served (budget vs runaway loops)
  (dirty nil))              ; a DOM mutation happened; styles cache is stale

;;; ---- MutationObserver dynamic state ---------------------------------------
;;; *CTX* is the context in effect while JS runs (scripts, timers, event handlers,
;;; the mutation-observer microtask).  Mutation sites in dom.lisp reach the active
;;; observer registry through it without threading CTX to every primitive.
;;; *MO-SUPPRESS* suppresses the primitive childList records while a coalescing
;;; algorithm ("replace all" / "replace") emits its own single record.
(defvar *ctx* nil)
(defvar *mo-suppress* nil)

;;; ---- small value helpers --------------------------------------------------
(declaim (inline num jbool opt))
(defun num (x) (float x 1d0))
(defun jbool (x) (if x js:*true* js:*false*))
(defun opt (x) (or x js:*null*))            ; nil -> JS null
(defun arg (args n) (let ((c (nthcdr n args))) (if c (car c) js:*undefined*)))
(defun jstr (v) (js:to-string v))
(defun truthy (v) (js:js-truthy v))
(defun nanp (x) (and (floatp x) (/= x x)))
(defun nullish (x) (or (eq x js:*null*) (eq x js:*undefined*)))
(defun js-int (v)
  "A CL integer from JS value V; NaN/Infinity -> 0 (truncate would signal)."
  (let ((n (js:to-number v)))
    (if (and (floatp n) (or (/= n n) (sb-ext:float-infinity-p n))) 0 (truncate n))))
(defun int-arg (args n) "A CL integer from the Nth arg (NaN/Infinity -> 0)."
  (js-int (arg args n)))

(defun proto (ctx key) (getf (context-protos ctx) key))
(defun (setf proto) (v ctx key) (setf (getf (context-protos ctx) key) v))

;;; ---- text selection state (shared by the IDL and the editor) ---------------
;;; Raw storage only: no clamping, because the length to clamp against is a
;;; control's current value and computing that needs the forms code, which is
;;; loaded much later.  Both callers clamp on the way in.

(defun node-selection (ctx node)
  "NODE's stored selection as (values start end direction).  An untouched
   control reads 0, 0, \"none\" — HTML's initial selection is collapsed at the
   START of the value, not at its end."
  (let ((s (gethash node (context-selections ctx))))
    (if s (values (first s) (second s) (cddr s)) (values 0 0 "none"))))

(defun set-node-selection (ctx node start end dir)
  "Store NODE's selection.  Returns T only when it actually MOVED — the
   fire-if-changed test the `select' event depends on."
  (multiple-value-bind (os oe od) (node-selection ctx node)
    (unless (and (eql os start) (eql oe end) (equal od dir))
      (setf (gethash node (context-selections ctx)) (list* start end dir))
      t)))

(defun make-validity-state (ctx)
  "A fresh, empty ValidityState host object.  The caller puts the eleven flags;
   what this contributes is the shared prototype carrying
   @@toStringTag = \"ValidityState\".

   That tag is what `assert_class_string' reads (Object.prototype.toString.call),
   and every element's `validity' getter built a bare Object, so the whole
   constraint-validation surface answered \"[object Object]\".
   radio-valueMissing.html asserts the class string FIRST in all six of its
   subtests, so it scored 0/6 before a single flag was looked at."
  (let ((p (or (proto ctx :validitystate)
               (setf (proto ctx :validitystate)
                     (let* ((realm (context-realm ctx))
                            (o (js:make-object
                                :proto (js:eval-script realm "Object.prototype"))))
                       (js:put o (js:eval-script realm "Symbol.toStringTag")
                               "ValidityState"
                               :writable nil :enumerable nil :configurable t)
                       o)))))
    (js:make-object :proto p)))

;;; ---- node <-> wrapper -----------------------------------------------------
(defun node-of (ctx obj)
  "The weft dnode backing wrapper OBJ, or NIL (also for the window/document-less)."
  (and (js:js-object-p obj) (gethash obj (context-obj-nodes ctx))))

(defun proto-key-for (node)
  (case (h:dnode-kind node)
    (:element (if (eq (h:dnode-namespace node) :svg) :svg-element :element))
    (:document :document) (:text :text) (:cdata :text)
    (:comment :comment) (:processing-instruction :pi)
    (:fragment :fragment) (:doctype :doctype) (t :node)))

(defun wrapper-proto (ctx node)
  "The JS prototype for NODE's wrapper.  Element wrappers get their per-tag HTML
   interface prototype (HTMLDivElement.prototype, …); everything else keys off
   PROTO-KEY-FOR."
  (cond ((eq (h:dnode-kind node) :element)
         (element-wrapper-proto ctx node))
        ;; An XMLDocument (implementation.createDocument) wrapper gets the
        ;; XMLDocument.prototype (DOM §XMLDocument); a DOMParser XML document is a
        ;; plain Document.
        ((and (eq (h:dnode-kind node) :document)
              (gethash node (context-xml-documents ctx))
              (proto ctx :xmldocument))
         (proto ctx :xmldocument))
        (t (proto ctx (proto-key-for node)))))

;;; ---- per-element exotic behaviour (indexed / named access) ----------------
;;; A few HTML interfaces are indexed collections in their own right —
;;; `select[2]`, `form[0]` — which a PROTOTYPE cannot express: the index is a
;;; property of the instance, not of HTMLSelectElement.prototype.  Feature files
;;; register an installer here and WRAP hangs the internal traps it returns on
;;; the fresh wrapper.
;;;
;;; Return a plist of shuttle internal traps.  A :get-own-property trap alone is
;;; NOT enough: shuttle's ordinary-get reads the property table directly rather
;;; than calling [[GetOwnProperty]], so the descriptor trap shows up in
;;; Object.getOwnPropertyDescriptor and `in` but never in `sel[0]`.  Install :get
;;; as well — and have it DELEGATE to (js::ordinary-get o key rcv) for everything
;;; it does not claim, or the element loses its own expandos (`el.myThing = 1`)
;;; and its prototype chain along with them.
(defvar *element-exotics* (make-hash-table :test 'equal)
  "lowercase tag name -> (lambda (ctx node obj) ...) -> plist of internal traps.")

(defun register-element-exotic (tag fn)
  (setf (gethash (string-downcase tag) *element-exotics*) fn))

(defun apply-element-exotic (ctx node obj)
  (when (and (plusp (hash-table-count *element-exotics*))
             (eq (h:dnode-kind node) :element))
    ;; dnode-name is already lowercase for HTML elements, and WRAP is hot enough
    ;; that a per-element string-downcase would be pure allocation.
    (let ((fn (gethash (h:dnode-name node) *element-exotics*)))
      (when fn
        (loop for (k v) on (funcall fn ctx node obj) by #'cddr
              do (setf (getf (js::js-object-internal obj) k) v)))))
  obj)

(defun wrap (ctx node)
  "The JS wrapper for weft NODE, memoized (DOM object identity). NIL -> JS null."
  (cond ((null node) js:*null*)
        ((js:js-object-p node) node)   ; already a JS object (e.g. window as an event target)
        (t (or (gethash node (context-node-objs ctx))
               (let ((obj (js:make-object :proto (wrapper-proto ctx node))))
                 (setf (gethash node (context-node-objs ctx)) obj
                       (gethash obj (context-obj-nodes ctx)) node)
                 (apply-element-exotic ctx node obj))))))

(defun require-node (ctx obj)
  (or (node-of ctx obj)
      (js:js-throw (js:make-native-error "TypeError" "not a DOM node"))))

;;; ---- method / accessor installation on a proto ----------------------------
(defmacro defmethod* (ctx target name len (this args) &body body)
  "Install a native method NAME on prototype TARGET (evaluated)."
  `(js:put ,target ,name
           (js:native-function (context-realm ,ctx) ,name
             (lambda (,this ,args) (declare (ignorable ,this ,args)) ,@body) ,len)
           :enumerable nil :writable t :configurable t))

(defmacro defget (ctx target name (this) &body body)
  "Install a read-only accessor NAME on prototype TARGET."
  `(js:put-accessor ,target ,name
     :get (js:native-function (context-realm ,ctx) (concatenate 'string "get " ,name)
            (lambda (,this ignore) (declare (ignore ignore) (ignorable ,this)) ,@body) 0)
     :enumerable t :configurable t))

(defmacro defgetset (ctx target name (this) getter (sval) setter)
  "Install a get/set accessor NAME on prototype TARGET."
  `(js:put-accessor ,target ,name
     :get (js:native-function (context-realm ,ctx) (concatenate 'string "get " ,name)
            (lambda (,this ig) (declare (ignore ig) (ignorable ,this)) ,getter) 0)
     :set (js:native-function (context-realm ,ctx) (concatenate 'string "set " ,name)
            (lambda (,this a) (let ((,sval (arg a 0))) (declare (ignorable ,this ,sval)) ,setter js:*undefined*)) 1)
     :enumerable t :configurable t))

;;; ---- tag-gated installation, chaining to whatever was there ---------------
;;; Every feature file installs onto ONE shared element prototype, so
;;; re-registering a generic name (checkValidity, value, labels, willValidate)
;;; replaces the sibling file's version for EVERY element, not just the one you
;;; care about — and a wrapper that returns js:*undefined* off-tag is exactly as
;;; destructive as never defining it.  Measured once: a <select> file took its
;;; own oracle unit 6 -> 36 passing subtests while destroying 131 across five
;;; other units.  These variants gate on the tag name and DELEGATE to whatever
;;; was installed before for every other element, so both files keep working.
;;; Prefer them to defmethod*/defget/defgetset for any name you did not invent.

(defun element-tag-p (ctx this tag)
  "True when THIS wraps an element whose tag name is TAG (a lowercase string)."
  (let ((node (node-of ctx this)))
    (and node (string= (h:dnode-name node) tag))))

(defun previous-method (target name)
  "The callable currently installed as data property NAME on TARGET, else NIL."
  (let ((d (js:js-get-own-property target name)))
    (when (and d (not (js::prop-accessor d)))
      (let ((v (js::prop-value d))) (and (js:js-callable-p v) v)))))

(defun previous-accessor (target name)
  "(values getter setter) for accessor NAME on TARGET; NILs when absent."
  (let ((d (js:js-get-own-property target name)))
    (if (and d (js::prop-accessor d))
        (values (js::prop-get d) (js::prop-set d))
        (values nil nil))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun tagged-body (body)
    "Split BODY into (values declarations forms).  The tag gate puts the body in
     an evaluated position (one branch of an IF), where a leading DECLARE is a
     hard error, so the declarations are lifted into a LOCALLY around it.  IGNORE
     becomes IGNORABLE: the fall-through branch still reads the arguments the
     body asked to ignore."
    (flet ((soften (decl)
             (cons 'declare
                   (mapcar (lambda (spec)
                             (if (and (consp spec) (eq (car spec) 'ignore))
                                 (cons 'ignorable (cdr spec))
                                 spec))
                           (cdr decl)))))
      (let ((decls '()))
        (loop while (and body (consp (car body)) (eq (caar body) 'declare))
              do (push (soften (pop body)) decls))
        (values (nreverse decls) body)))))

(defmacro defmethod-for (ctx target tag name len (this args) &body body)
  "Install method NAME for elements tagged TAG; other elements fall through to
   the method that was already installed (or undefined if there was none)."
  (multiple-value-bind (decls forms) (tagged-body body)
    (let ((prev (gensym "PREV")))
      `(let ((,prev (previous-method ,target ,name)))
         (defmethod* ,ctx ,target ,name ,len (,this ,args)
           (if (element-tag-p ,ctx ,this ,tag)
               (locally ,@decls ,@forms)
               (if ,prev (js:js-call ,prev ,this ,args) js:*undefined*)))))))

(defmacro defget-for (ctx target tag name (this) &body body)
  "Install read-only accessor NAME for elements tagged TAG; others fall through."
  (multiple-value-bind (decls forms) (tagged-body body)
    (let ((pg (gensym "PG")) (ps (gensym "PS")))
      `(multiple-value-bind (,pg ,ps) (previous-accessor ,target ,name)
         (js:put-accessor ,target ,name
           :get (js:native-function (context-realm ,ctx)
                                    (concatenate 'string "get " ,name)
                  (lambda (,this ig) (declare (ignore ig) (ignorable ,this))
                    (if (element-tag-p ,ctx ,this ,tag)
                        (locally ,@decls ,@forms)
                        (if ,pg (js:js-call ,pg ,this '()) js:*undefined*)))
                  0)
           :set ,ps :enumerable t :configurable t)))))

(defmacro defgetset-for (ctx target tag name (this) getter (sval) setter)
  "Install a get/set accessor NAME for elements tagged TAG; others fall through
   to the accessor that was already installed."
  (let ((pg (gensym "PG")) (ps (gensym "PS")) (a (gensym "A")))
    `(multiple-value-bind (,pg ,ps) (previous-accessor ,target ,name)
       (js:put-accessor ,target ,name
         :get (js:native-function (context-realm ,ctx)
                                  (concatenate 'string "get " ,name)
                (lambda (,this ig) (declare (ignore ig) (ignorable ,this))
                  (if (element-tag-p ,ctx ,this ,tag)
                      ,getter
                      (if ,pg (js:js-call ,pg ,this '()) js:*undefined*)))
                0)
         :set (js:native-function (context-realm ,ctx)
                                  (concatenate 'string "set " ,name)
                (lambda (,this ,a)
                  (if (element-tag-p ,ctx ,this ,tag)
                      (let ((,sval (arg ,a 0))) (declare (ignorable ,this ,sval)) ,setter)
                      (when ,ps (js:js-call ,ps ,this (list (arg ,a 0)))))
                  js:*undefined*)
                1)
         :enumerable t :configurable t))))

;;; ---- element-prototype extension hook -------------------------------------
;;; Disjoint feature files (HTMLInputElement IDL surface: valueAsNumber,
;;; validity, selection, …) each self-register an installer here, keyed by name
;;; so a reload replaces rather than duplicates.  install-element-proto funcalls
;;; them all after the built-in accessors, with the same (ctx ep) it uses itself.
(defvar *element-proto-extensions* (make-hash-table :test 'eq)
  "KEY (keyword) -> (lambda (ctx ep) ...) run at the end of install-element-proto.")

(defun register-element-proto-extension (key fn)
  (setf (gethash key *element-proto-extensions*) fn))

(defun run-element-proto-extensions (ctx ep)
  (maphash (lambda (k fn) (declare (ignore k)) (funcall fn ctx ep))
           *element-proto-extensions*))

;;; ---- camelCase <-> dashed CSS property names ------------------------------
(defun camel->dash (s)
  "whiteSpace -> white-space ; cssFloat -> css-float (special-cased by caller)."
  (with-output-to-string (o)
    (loop for c across s
          do (if (upper-case-p c)
                 (progn (write-char #\- o) (write-char (char-downcase c) o))
                 (write-char c o)))))

(defun dash->camel (s)
  "white-space -> whiteSpace."
  (with-output-to-string (o)
    (let ((up nil))
      (loop for c across s
            do (cond ((char= c #\-) (setf up t))
                     (up (write-char (char-upcase c) o) (setf up nil))
                     (t (write-char c o)))))))

;;; ---- Web Font Loader convention -------------------------------------------
;;; Many themes (WordPress/Jetpack, Typekit) load fonts through the Web Font
;;; Loader JS library rather than CSS @font-face: an inline script sets a
;;; `WebFontConfig` global, an async webfont.js reads it, fetches the font CSS,
;;; injects @font-face and flips the <html> class from `wf-loading` to
;;; `wf-active` (which the theme's `.wf-active .title{font-family:…}` rules gate
;;; on).  The library's font *watcher* can never observe activation in a headless
;;; render, so it hangs at `wf-loading`; the embedder replays the load instead.

(defun web-font-config (ctx)
  "Read WebFontConfig from CTX's realm.  Returns (values API-URL FAMILIES) — FAMILIES
a list of Google-font specs (e.g. \"Fondamento:r:latin,latin-ext\") — or NIL when the
page declares no such config."
  (let ((s (ignore-errors
             (js:eval-script (context-realm ctx)
               "(function(){try{var c=(typeof WebFontConfig!=='undefined')&&WebFontConfig;if(!c||!c.google||!c.google.families||!c.google.families.length)return '';return (c.api_url||'https://fonts.googleapis.com/css')+String.fromCharCode(1)+c.google.families.join(String.fromCharCode(2));}catch(e){return ''}})()"))))
    (when (and (stringp s) (plusp (length s)))
      (let ((sep (position (code-char 1) s)))
        (when sep
          (values (subseq s 0 sep) (%split-char (code-char 2) (subseq s (1+ sep)))))))))
