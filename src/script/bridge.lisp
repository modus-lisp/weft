;;;; src/script/bridge.lisp — M1 of the weft/shuttle scripting seam.
;;;;
;;;; A per-document shuttle realm, a `document` host object whose
;;;; getElementById returns an element host object backed by the weft DOM node,
;;;; and the element's [[Set]] trap for textContent — which mutates the backing
;;;; node and marks the document for relayout. Inline <script> nodes are then
;;;; run against the realm after parse and before layout, so the render reflects
;;;; whatever the script did.
;;;;
;;;; This is deliberately the minimum that proves the architecture end-to-end;
;;;; the DOM/CSSOM/Events surface grows on top of these same primitives.
(in-package #:weft.script)

;;; ---------------------------------------------------------------------------
;;; The per-document context
;;; ---------------------------------------------------------------------------
(defstruct (context (:constructor %make-context))
  realm                     ; the shuttle realm (one per document)
  document                  ; the weft DOM document (an h:dnode of kind :document)
  node-objs                 ; EQ hash: weft dnode -> its element host object (identity)
  document-obj              ; the JS `document` host object
  (dirty nil))              ; set by a mutating trap; a relayout is owed

(defun js-arg (args n)
  "The Nth positional argument of a native call, defaulting to undefined."
  (let ((c (nthcdr n args))) (if c (car c) js:*undefined*)))

;;; ---------------------------------------------------------------------------
;;; textContent
;;; ---------------------------------------------------------------------------
(defun set-text-content (node text)
  "The textContent setter: detach NODE's children and, if TEXT is non-empty,
   give it a single text child holding TEXT (DOM `Node.textContent`)."
  (let ((children (h:dnode-children node)))
    (loop for c across children do (setf (h:dnode-parent c) nil))
    (setf (fill-pointer children) 0)
    (when (plusp (length text))
      (h:dom-append node (h:make-text text)))))

;;; ---------------------------------------------------------------------------
;;; Element host objects (backed by a weft DOM node)
;;; ---------------------------------------------------------------------------
(defun element-set (ctx node object key value)
  "The element's [[Set]] trap. textContent mutates the backing weft node and
   marks the document dirty (a relayout is owed); any other property is stored
   as an ordinary own data property so scripts can stash state on a node."
  (cond
    ((and (stringp key) (string= key "textContent"))
     (set-text-content node (js:to-string value))
     (setf (context-dirty ctx) t)
     js:*true*)
    (t
     (js:js-define-own-property
      object key (list :value value :writable t :enumerable t :configurable t))
     js:*true*)))

(defun element-object (ctx node)
  "The element host object for weft node NODE, memoized so the same node always
   yields the same JS object (DOM object identity: el === el)."
  (or (gethash node (context-node-objs ctx))
      (setf (gethash node (context-node-objs ctx))
            (js:make-host-object
             (context-realm ctx)
             :set (lambda (object key value receiver)
                    (declare (ignore receiver))
                    (element-set ctx node object key value))))))

;;; ---------------------------------------------------------------------------
;;; The document host object + realm construction
;;; ---------------------------------------------------------------------------
(defun install-document (ctx)
  "Build the JS `document` object and its methods, and the `window`/`document`
   globals."
  (let* ((realm (context-realm ctx))
         (document (context-document ctx))
         (docobj (js:make-host-object realm)))
    (setf (context-document-obj ctx) docobj)
    (js:put docobj "getElementById"
            (js:native-function realm "getElementById"
              (lambda (this args)
                (declare (ignore this))
                (let ((el (dom:get-element-by-id
                           document (js:to-string (js-arg args 0)))))
                  (if el (element-object ctx el) js:*null*)))
              1))
    (js:define-global realm "document" docobj)
    ;; window === globalThis (the realm global object).
    (js:define-global realm "window" (js:eval-script realm "globalThis"))
    docobj))

(defun make-context (document)
  "Create a fresh scripting context for a parsed weft DOCUMENT: a realm with the
   DOM bindings installed."
  (let ((ctx (%make-context :realm (js:make-realm)
                            :document document
                            :node-objs (make-hash-table :test 'eq))))
    (install-document ctx)
    ctx))

;;; ---------------------------------------------------------------------------
;;; Running inline <script>
;;; ---------------------------------------------------------------------------
(defun run-inline-scripts (ctx)
  "Execute every inline <script> (no src attribute) in document order against
   CTX's realm. A script error is reported but does not abort the render — a
   broken script must not take the page down (browser behavior)."
  (let ((realm (context-realm ctx)))
    (dolist (script (dom:get-elements-by-tag-name (context-document ctx) "script"))
      (unless (dom:has-attribute script "src")
        (let ((source (dom:text-content script)))
          (when (plusp (length source))
            (handler-case (js:eval-script realm source)
              (js:shuttle-error (e)
                (format *error-output* "~&weft.script: uncaught ~a~%" e))
              (error (e)
                (format *error-output* "~&weft.script: script error: ~a~%" e))))))))
  ctx)

;;; ---------------------------------------------------------------------------
;;; The scripted render entry points
;;; ---------------------------------------------------------------------------
(defun render-scripted-to-canvas (html css width &rest keys)
  "Like weft.render:render-to-canvas, but run inline <script> against the parsed
   DOM before layout. Returns (values canvas context) — the context exposes the
   realm and document for inspection. Passing HTML with no <script> renders
   byte-identically to render-to-canvas."
  (let (ctx)
    (values
     (apply #'r:render-to-canvas html css width
            :before-layout (lambda (doc)
                             (setf ctx (make-context doc))
                             (run-inline-scripts ctx))
            keys)
     ctx)))

(defun render-scripted-to-png (html css width path &rest keys)
  "Render HTML+CSS (running inline <script>) at WIDTH px and save a PNG.
   Returns (values path context)."
  (multiple-value-bind (cv ctx)
      (apply #'render-scripted-to-canvas html css width keys)
    (r:write-png cv path)
    (values path ctx)))
