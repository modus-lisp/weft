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
    ;; A trivial navigator + location so feature-detecting scripts don't throw.
    (let ((nav (js:make-object :proto (js:eval-script realm "Object.prototype"))))
      (js:put nav "userAgent" "weft")
      (js:define-global realm "navigator" nav))
    docobj))

(defun make-context (document &key (css "") (width 800))
  "Create a fresh scripting context for a parsed weft DOCUMENT: a realm with the
   full DOM/CSSOM/Events surface installed and document/window globals bound."
  (let* ((realm (js:make-realm))
         (ctx (%make-context :realm realm :document document :css css :width width))
         (op (js:eval-script realm "Object.prototype"))
         (np (js:make-object :proto op))     ; Node.prototype
         (ep (js:make-object :proto np))     ; Element.prototype
         (dp (js:make-object :proto np))     ; Document.prototype
         (cp (js:make-object :proto np))     ; CharacterData (Text/Comment)
         (evp (js:make-object :proto op))    ; Event.prototype
         (window (js:eval-script realm "globalThis")))
    (setf (context-protos ctx)
          (list :node np :element ep :document dp :text cp :comment cp
                :fragment np :event evp :window window))
    (install-node-proto ctx np)
    (install-element-proto ctx ep)
    (install-document-proto ctx dp)
    (install-chardata-proto ctx cp)
    (install-event-proto ctx evp)
    (install-events ctx np)
    (install-cssom ctx)
    (install-timers ctx)
    (install-globals ctx)
    ctx))

;; Retained from M1: the element wrapper accessor.
(defun element-object (ctx node) (wrap ctx node))

;;; ---------------------------------------------------------------------------
;;; data: URL script decoding (Acid3 loads several script bodies this way)
;;; ---------------------------------------------------------------------------
(defun percent-decode (s)
  "Percent-decode S to a CL string (bytes interpreted as Latin-1/ASCII)."
  (with-output-to-string (o)
    (let ((i 0) (n (length s)))
      (loop while (< i n)
            do (let ((c (char s i)))
                 (cond ((and (char= c #\%) (< (+ i 2) n)
                             (digit-char-p (char s (+ i 1)) 16)
                             (digit-char-p (char s (+ i 2)) 16))
                        (write-char (code-char (parse-integer s :start (1+ i) :end (+ i 3) :radix 16)) o)
                        (incf i 3))
                       (t (write-char c o) (incf i))))))))

(defparameter +b64+ "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
(defun base64-decode (s)
  "Decode base64 S (ignoring any non-alphabet chars, e.g. whitespace) to a string."
  (let ((bits 0) (nbits 0) (out (make-string-output-stream)))
    (loop for c across s
          for v = (position c +b64+)
          when v
            do (setf bits (logior (ash bits 6) v)) (incf nbits 6)
               (when (>= nbits 8)
                 (decf nbits 8)
                 (write-char (code-char (logand (ash bits (- nbits)) #xFF)) out)))
    (get-output-stream-string out)))

(defun data-url-script (src)
  "The JavaScript source carried by a data: URL SRC (text or ;base64), or NIL."
  (let ((body (subseq src 5)))                     ; drop "data:"
    (let ((comma (position #\, body)))
      (when comma
        (let ((meta (subseq body 0 comma)) (data (subseq body (1+ comma))))
          (if (search ";base64" meta)
              (base64-decode (percent-decode data))
              (percent-decode data)))))))

;;; ---------------------------------------------------------------------------
;;; Running <script>
;;; ---------------------------------------------------------------------------
(defun script-source (script)
  "The JavaScript source of a <script> element: its inline text, or a decoded
   data: URL src. Returns NIL for an (unsupported) external network src."
  (let ((src (dom:get-attribute script "src")))
    (cond ((null src) (dom:text-content script))
          ((and (>= (length src) 5) (string-equal (subseq src 0 5) "data:"))
           (data-url-script src))
          (t nil))))

(defun run-inline-scripts (ctx)
  "Execute every runnable <script> in document order against CTX's realm. A
   script error is reported but does not abort (browser behavior)."
  (let ((realm (context-realm ctx)))
    (dolist (script (dom:get-elements-by-tag-name (context-document ctx) "script"))
      (let ((source (script-source script)))
        (when (and source (plusp (length source)))
          (handler-case (js:eval-script realm source)
            (js:shuttle-error (e)
              (format *error-output* "~&weft.script: uncaught ~a~%" e))
            (error (e)
              (format *error-output* "~&weft.script: script error: ~a~%" e)))))))
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
