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
  (iframe-docs (make-hash-table :test 'eq)) ; iframe/object dnode -> its content document
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

(defun wrap (ctx node)
  "The JS wrapper for weft NODE, memoized (DOM object identity). NIL -> JS null."
  (cond ((null node) js:*null*)
        ((js:js-object-p node) node)   ; already a JS object (e.g. window as an event target)
        (t (or (gethash node (context-node-objs ctx))
               (let ((obj (js:make-object :proto (wrapper-proto ctx node))))
                 (setf (gethash node (context-node-objs ctx)) obj
                       (gethash obj (context-obj-nodes ctx)) node)
                 obj)))))

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
