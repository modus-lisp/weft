;;;; src/script/core.lisp — the scripting context and the node-wrapping kernel.
;;;;
;;;; One CONTEXT per document holds the shuttle realm, the weft DOM, the shared
;;;; DOM prototypes (Node/Element/Document/Text/Comment/…), the node<->wrapper
;;;; identity maps, the computed-style cache, and the macrotask (timer) queue.
;;;; Every wrapper is a plain JS object whose [[Prototype]] carries the DOM
;;;; methods; the wrapper's backing weft node is found through OBJ-NODES, so the
;;;; prototype methods read `this` and recover the live node.
(in-package #:weft.script)

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
  (listeners (make-hash-table :test 'eq))   ; dnode -> list of (type listener capture) entries
  (events (make-hash-table :test 'eq))      ; event wrapper object -> its EVT struct
  (traversal (make-hash-table :test 'eq))   ; NodeIterator/TreeWalker wrapper -> its state
  (ranges (make-hash-table :test 'eq))      ; Range wrapper -> its RG struct
  (ns-info (make-hash-table :test 'eq))     ; element dnode -> (:ns uri :prefix p :local l) for createElementNS
  (owner-docs (make-hash-table :test 'eq))  ; created node -> its owner document (even while detached)
  (current-script nil)      ; the <script> node currently executing (for document.write)
  (dirty nil))              ; a DOM mutation happened; styles cache is stale

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
    (:element :element) (:document :document) (:text :text)
    (:comment :comment) (:fragment :fragment) (t :node)))

(defun wrap (ctx node)
  "The JS wrapper for weft NODE, memoized (DOM object identity). NIL -> JS null."
  (cond ((null node) js:*null*)
        (t (or (gethash node (context-node-objs ctx))
               (let ((obj (js:make-object :proto (proto ctx (proto-key-for node)))))
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
