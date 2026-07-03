;;;; src/script/bridge.lisp — the orchestrator: realm + DOM bindings, the script
;;;; runner (inline and data: URL <script>), and the scripted render entry points.
(in-package #:weft.script)

;;; ---------------------------------------------------------------------------
;;; Realm + DOM binding construction
;;; ---------------------------------------------------------------------------
(defun make-location (ctx)
  "A Location object parsed from the document's base URL."
  (let* ((realm (context-realm ctx)) (base (context-base ctx))
         (u (and (plusp (length base)) (ignore-errors (weft.url:parse base))))
         (loc (js:make-object :proto (js:eval-script realm "Object.prototype"))))
    (macrolet ((field (k form) `(js:put loc ,k (or (and u ,form) ""))))
      (field "href" (weft.url:href u))
      (field "protocol" (weft.url:protocol u))
      (field "host" (weft.url:host-str u))
      (field "hostname" (weft.url:hostname u))
      (field "port" (weft.url:port-str u))
      (field "pathname" (weft.url:pathname-str u))
      (field "search" (weft.url:search-str u))
      (field "hash" (weft.url:hash-str u))
      (field "origin" (weft.url:origin u)))
    (dolist (m '("reload" "replace" "assign"))
      (js:put loc m (js:native-function realm m (lambda (a b) (declare (ignore a b)) js:*undefined*) 0)))
    (js:put loc "toString"
            (js:native-function realm "toString" (lambda (this a) (declare (ignore a)) (js:js-get this "href")) 0))
    loc))

(defun install-globals (ctx)
  (let* ((realm (context-realm ctx))
         (window (proto ctx :window))
         (docobj (wrap ctx (context-document ctx))))
    (setf (context-document-obj ctx) docobj)
    (js:define-global realm "document" docobj)
    (js:define-global realm "window" window)
    (js:define-global realm "self" window)
    ;; navigator — the fields feature-detecting scripts read.
    (let ((nav (js:make-object :proto (js:eval-script realm "Object.prototype"))))
      (js:put nav "userAgent" "Mozilla/5.0 (weft)")
      (js:put nav "appName" "Netscape") (js:put nav "appVersion" "5.0 (weft)")
      (js:put nav "platform" "Lisp") (js:put nav "vendor" "weft")
      (js:put nav "language" "en-US") (js:put nav "languages" (js:eval-script realm "['en-US','en']"))
      (js:put nav "onLine" js:*true*) (js:put nav "cookieEnabled" js:*true*)
      (js:put nav "doNotTrack" js:*null*)
      (js:define-global realm "navigator" nav))
    ;; location — parsed from the document base URL.
    (let ((loc (make-location ctx)))
      (js:define-global realm "location" loc)
      (js:put window "location" loc)
      (js:put docobj "location" loc))
    ;; history / storage — enough surface that scripts don't throw.
    (let ((hist (js:make-object :proto (js:eval-script realm "Object.prototype"))))
      (js:put hist "length" 1.0)
      (js:put hist "state" js:*null*)
      (js:put hist "scrollRestoration" "auto")
      (dolist (m '("pushState" "replaceState" "back" "forward" "go"))
        (js:put hist m (js:native-function realm m (lambda (this args) (declare (ignore this args)) js:*undefined*) 0)))
      (js:define-global realm "history" hist))
    (flet ((storage ()
             (js:eval-script realm
               "(function(){var s={},o={getItem:function(k){return k in s?s[k]:null;},setItem:function(k,v){s[k]=String(v);},removeItem:function(k){delete s[k];},clear:function(){s={};},key:function(i){return Object.keys(s)[i]||null;}};Object.defineProperty(o,'length',{get:function(){return Object.keys(s).length;}});return o;})()")))
      (js:define-global realm "localStorage" (storage))
      (js:define-global realm "sessionStorage" (storage)))
    ;; a few window methods real pages call
    (dolist (m '("scrollTo" "scroll" "scrollBy" "focus" "blur" "print" "close" "open"
                 "alert" "resizeTo" "moveTo" "requestAnimationFrame" "cancelAnimationFrame"))
      (js:define-global realm m (js:native-function realm m (lambda (this args) (declare (ignore this args)) js:*undefined*) 0)))
    (js:define-global realm "matchMedia"
      (js:native-function realm "matchMedia"
        (lambda (this args) (declare (ignore this))
          (let ((o (js:make-object :proto (js:eval-script realm "Object.prototype"))))
            (js:put o "matches" js:*false*) (js:put o "media" (jstr (arg args 0)))
            (js:put o "addListener" (js:native-function realm "addListener" (lambda (a b) (declare (ignore a b)) js:*undefined*) 0))
            o)) 1))
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
    ;; Pull external <link rel=stylesheet> sheets into the DOM before scripts
    ;; run, so the cascade and document.styleSheets reflect the resolved CSS.
    (inline-external-stylesheets ctx)
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

(defparameter +classic-js-types+
  '("text/javascript" "application/javascript" "text/ecmascript"
    "application/ecmascript" "application/x-javascript" "application/x-ecmascript"
    "text/jscript" "text/livescript" "text/javascript1.0" "text/javascript1.1"
    "text/javascript1.2" "text/javascript1.3" "text/javascript1.4" "text/javascript1.5"))

(defun classic-javascript-p (script)
  "True if SCRIPT is a classic JavaScript block (executed).  A data block such as
   application/ld+json or text/template — or a module — is not run."
  (let ((type (dom:get-attribute script "type")))
    (or (null type) (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return) type)))
        (member (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return) type))
                +classic-js-types+ :test #'string=))))

(defun run-inline-scripts (ctx)
  "Execute every classic-JavaScript <script> in document order against CTX's
   realm.  A script error is reported but does not abort (browser behavior);
   non-JS <script> blocks (JSON-LD, templates, modules) are skipped."
  (let ((realm (context-realm ctx)))
    (dolist (script (dom:get-elements-by-tag-name (context-document ctx) "script"))
      (let ((source (and (classic-javascript-p script) (script-source ctx script))))
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
(defun render-scripted-to-canvas (html css width &rest keys &key base loader &allow-other-keys)
  "Like weft.render:render-to-canvas, but run inline <script> against the parsed
   DOM (then drain timers + microtasks) before layout. Returns (values canvas
   context). BASE/LOADER configure the subresource pipeline; the remaining keys
   pass through to render-to-canvas.  A scriptless page renders byte-identically."
  (let (ctx
        (render-keys (loop for (k v) on keys by #'cddr
                           unless (member k '(:base :loader)) collect k and collect v)))
    (values
     (apply #'r:render-to-canvas html css width
            :before-layout (lambda (doc)
                             (setf ctx (make-context doc :css (or css "") :width width
                                                     :base (or base "") :loader loader))
                             (run-inline-scripts ctx)
                             (run-event-loop ctx :max-tasks 10000))
            render-keys)
     ctx)))

(defun render-scripted-to-png (html css width path &rest keys)
  "Render HTML+CSS (running inline <script>) at WIDTH px and save a PNG.
   Returns (values path context)."
  (multiple-value-bind (cv ctx)
      (apply #'render-scripted-to-canvas html css width keys)
    (r:write-png cv path)
    (values path ctx)))
