;;;; src/script/bridge.lisp — the orchestrator: realm + DOM bindings, the script
;;;; runner (inline and data: URL <script>), and the scripted render entry points.
(in-package #:weft.script)

;;; ---------------------------------------------------------------------------
;;; Realm + DOM binding construction
;;; ---------------------------------------------------------------------------
(defun install-globals (ctx)
  (let* ((realm (context-realm ctx))
         (window (proto ctx :window))
         (docobj (wrap ctx (context-document ctx))))
    (setf (context-document-obj ctx) docobj)
    (js:define-global realm "document" docobj)
    (js:define-global realm "window" window)
    (js:define-global realm "self" window)
    ;; A trivial navigator so feature-detecting scripts don't throw.
    (let ((nav (js:make-object :proto (js:eval-script realm "Object.prototype"))))
      (js:put nav "userAgent" "weft")
      (js:define-global realm "navigator" nav))
    ;; Interface objects carrying the Node type constants (Node.COMMENT_NODE …),
    ;; with .prototype pointing at the shared prototypes.
    (flet ((iface (name proto-key)
             (let ((o (js:make-object :proto (js:eval-script realm "Function.prototype"))))
               (js:put o "prototype" (proto ctx proto-key) :enumerable nil)
               (js:define-global realm name o) o)))
      (let ((node-iface (iface "Node" :node)))
        (dolist (pair '(("ELEMENT_NODE" . 1) ("ATTRIBUTE_NODE" . 2) ("TEXT_NODE" . 3)
                        ("CDATA_SECTION_NODE" . 4) ("ENTITY_REFERENCE_NODE" . 5)
                        ("ENTITY_NODE" . 6) ("PROCESSING_INSTRUCTION_NODE" . 7)
                        ("COMMENT_NODE" . 8) ("DOCUMENT_NODE" . 9) ("DOCUMENT_TYPE_NODE" . 10)
                        ("DOCUMENT_FRAGMENT_NODE" . 11) ("NOTATION_NODE" . 12)))
          (js:put node-iface (car pair) (num (cdr pair)) :enumerable nil :writable nil :configurable nil)))
      (iface "Element" :element) (iface "Document" :document))
    docobj))

(defun make-context (document &key (css "") (width 800) (base "") loader)
  "Create a fresh scripting context for a parsed weft DOCUMENT: a realm with the
   full DOM/CSSOM/Events surface installed and document/window globals bound.
   BASE + LOADER supply the subresource pipeline (LOADER is (ctx url) -> (values
   kind content); data: URLs are handled without it)."
  (let* ((realm (js:make-realm))
         (ctx (%make-context :realm realm :document document :css css :width width
                             :base base :loader loader))
         (op (js:eval-script realm "Object.prototype"))
         (np (js:make-object :proto op))     ; Node.prototype
         (ep (js:make-object :proto np))     ; Element.prototype
         (dp (js:make-object :proto np))     ; Document.prototype
         (cp (js:make-object :proto np))     ; CharacterData (Text/Comment)
         (evp (js:make-object :proto op))    ; Event.prototype
         (dtp (js:make-object :proto np))    ; DocumentType.prototype
         (window (js:eval-script realm "globalThis")))
    (setf (context-protos ctx)
          (list :node np :element ep :document dp :text cp :comment cp
                :fragment np :event evp :doctype dtp :window window))
    (install-node-proto ctx np)
    (install-element-proto ctx ep)
    (install-document-proto ctx dp)
    (install-doctype-proto ctx dtp)
    (install-chardata-proto ctx cp)
    (install-on-handlers ctx ep)
    (install-on-handlers ctx dp)
    (install-table-interfaces ctx ep)
    (register-parsed-inline-handlers ctx)
    (install-event-proto ctx evp)
    (install-events ctx np)
    (install-cssom ctx)
    (install-traversal ctx)
    (install-range ctx)
    (install-timers ctx)
    (install-globals ctx)
    ctx))

;; Retained from M1: the element wrapper accessor.
(defun element-object (ctx node) (wrap ctx node))

;;; ---------------------------------------------------------------------------
;;; Running <script>
;;; ---------------------------------------------------------------------------
(defun script-source (ctx script)
  "The JavaScript source of a <script> element: its inline text, a decoded
   data: URL src, or a src fetched through the context loader (or NIL)."
  (let ((src (dom:get-attribute script "src")))
    (cond ((null src) (dom:text-content script))
          ((and (>= (length src) 5) (string-equal (subseq src 0 5) "data:"))
           (data-url-script src))
          (t (multiple-value-bind (kind content) (load-resource ctx src)
               (declare (ignore kind))
               (and (stringp content) content))))))

(defun run-inline-scripts (ctx)
  "Execute every runnable <script> in document order against CTX's realm. A
   script error is reported but does not abort (browser behavior)."
  (let ((realm (context-realm ctx)))
    (dolist (script (dom:get-elements-by-tag-name (context-document ctx) "script"))
      (let ((source (script-source ctx script)))
        (when (and source (plusp (length source)))
          (setf (context-current-script ctx) script)
          (handler-case (js:eval-script realm source)
            (js:shuttle-error (e)
              (format *error-output* "~&weft.script: uncaught ~a~%" e))
            (error (e)
              (format *error-output* "~&weft.script: script error: ~a~%" e)))))))
  (setf (context-current-script ctx) nil)
  ctx)

;;; ---------------------------------------------------------------------------
;;; Scripted render
;;; ---------------------------------------------------------------------------
(defun render-scripted-to-canvas (html css width &rest keys)
  "Like weft.render:render-to-canvas, but run inline <script> against the parsed
   DOM (then drain timers + microtasks) before layout. Returns (values canvas
   context). A page with no <script> renders byte-identically to render-to-canvas."
  (let (ctx)
    (values
     (apply #'r:render-to-canvas html css width
            :before-layout (lambda (doc)
                             (setf ctx (make-context doc :css (or css "") :width width))
                             (run-inline-scripts ctx)
                             (run-event-loop ctx :max-tasks 10000))
            keys)
     ctx)))

(defun render-scripted-to-png (html css width path &rest keys)
  "Render HTML+CSS (running inline <script>) at WIDTH px and save a PNG.
   Returns (values path context)."
  (multiple-value-bind (cv ctx)
      (apply #'render-scripted-to-canvas html css width keys)
    (r:write-png cv path)
    (values path ctx)))
