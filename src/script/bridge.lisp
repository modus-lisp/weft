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
    ;; Single browsing context: parent/top/frames resolve to this window, so a
    ;; frame document's script (run in this realm) reaches the parent globals.
    (js:define-global realm "parent" window)
    (js:define-global realm "top" window)
    (js:define-global realm "frames" window)
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
    ;; URL / URLSearchParams / XMLHttpRequest / IntersectionObserver / ResizeObserver —
    ;; Web APIs real pages depend on.  URL parsing is delegated to weft.url (WHATWG)
    ;; through a native helper; the rest are JS polyfills.  IntersectionObserver reports
    ;; every observed element as intersecting on the next tick, so lazy-image loaders
    ;; (NYT, CNN, …) swap in their real src/background before the final render instead
    ;; of leaving grey placeholders a static render would never scroll into view.
    (js:define-global realm "__weft_url_parse"
      (js:native-function realm "__weft_url_parse"
        (lambda (this args) (declare (ignore this))
          (let* ((s (jstr (arg args 0)))
                 (bv (arg args 1))
                 (b (if (or (js:js-undefined-p bv) (eq bv js:*null*)) nil (jstr bv)))
                 (u (ignore-errors (weft.url:parse s b))))
            (if u
                (let ((o (js:make-object :proto (js:eval-script realm "Object.prototype"))))
                  (flet ((f (k v) (js:put o k (or v ""))))
                    (f "href" (weft.url:href u))       (f "protocol" (weft.url:protocol u))
                    (f "host" (weft.url:host-str u))    (f "hostname" (weft.url:hostname u))
                    (f "port" (weft.url:port-str u))    (f "pathname" (weft.url:pathname-str u))
                    (f "search" (weft.url:search-str u))(f "hash" (weft.url:hash-str u))
                    (f "origin" (ignore-errors (weft.url:origin u))))
                  o)
                js:*null*)))
        2))
    (js:eval-script realm "(function(G){
  function dec(x){try{return decodeURIComponent(String(x).replace(/\\+/g,' '));}catch(e){return x;}}
  function USP(init){this._e=[];var self=this;
    if(typeof init==='string'){var s=init.charAt(0)==='?'?init.slice(1):init;
      if(s)s.split('&').forEach(function(p){if(!p)return;var i=p.indexOf('='),k=i<0?p:p.slice(0,i),v=i<0?'':p.slice(i+1);self._e.push([dec(k),dec(v)]);});}
    else if(init&&typeof init==='object'){Object.keys(init).forEach(function(k){self._e.push([k,String(init[k])]);});}}
  USP.prototype.append=function(k,v){this._e.push([String(k),String(v)]);};
  USP.prototype['delete']=function(k){this._e=this._e.filter(function(e){return e[0]!==k;});};
  USP.prototype.get=function(k){for(var i=0;i<this._e.length;i++)if(this._e[i][0]===k)return this._e[i][1];return null;};
  USP.prototype.getAll=function(k){return this._e.filter(function(e){return e[0]===k;}).map(function(e){return e[1];});};
  USP.prototype.has=function(k){return this.get(k)!==null;};
  USP.prototype.set=function(k,v){var f=false;this._e=this._e.filter(function(e){if(e[0]===k){if(f)return false;f=true;e[1]=String(v);}return true;});if(!f)this._e.push([String(k),String(v)]);};
  USP.prototype.forEach=function(cb,t){var self=this;this._e.forEach(function(e){cb.call(t,e[1],e[0],self);});};
  USP.prototype.keys=function(){return this._e.map(function(e){return e[0];});};
  USP.prototype.values=function(){return this._e.map(function(e){return e[1];});};
  USP.prototype.toString=function(){return this._e.map(function(e){return encodeURIComponent(e[0])+'='+encodeURIComponent(e[1]);}).join('&');};
  G.URLSearchParams=USP;
  function URL(url,base){var p=__weft_url_parse(String(url),(base===undefined||base===null)?undefined:String(base));
    if(!p)throw new TypeError('Invalid URL: '+url);
    this.href=p.href;this.protocol=p.protocol;this.host=p.host;this.hostname=p.hostname;this.port=p.port;
    this.pathname=p.pathname;this.search=p.search;this.hash=p.hash;this.origin=p.origin;this.searchParams=new USP(p.search);}
  URL.prototype.toString=function(){return this.href;};URL.prototype.toJSON=function(){return this.href;};
  G.URL=URL;
  function XHR(){this.readyState=0;this.status=0;this.responseText='';this.response=null;this.onreadystatechange=null;this.onload=null;this.onerror=null;}
  XHR.prototype.open=function(){this.readyState=1;};XHR.prototype.setRequestHeader=function(){};
  XHR.prototype.overrideMimeType=function(){};XHR.prototype.getResponseHeader=function(){return null;};
  XHR.prototype.getAllResponseHeaders=function(){return '';};XHR.prototype.abort=function(){};XHR.prototype.send=function(){};
  XHR.prototype.addEventListener=function(){};XHR.prototype.removeEventListener=function(){};
  G.XMLHttpRequest=XHR;
  function IO(cb){this._cb=cb;}
  IO.prototype.observe=function(el){var cb=this._cb,self=this;setTimeout(function(){try{cb([{target:el,isIntersecting:true,intersectionRatio:1,boundingClientRect:{top:0,left:0,bottom:0,right:0,width:0,height:0,x:0,y:0},intersectionRect:{top:0,left:0,bottom:0,right:0,width:0,height:0},rootBounds:null,time:0}],self);}catch(e){}},0);};
  IO.prototype.unobserve=function(){};IO.prototype.disconnect=function(){};IO.prototype.takeRecords=function(){return [];};
  G.IntersectionObserver=IO;
  function RO(cb){this._cb=cb;}RO.prototype.observe=function(){};RO.prototype.unobserve=function(){};RO.prototype.disconnect=function(){};
  G.ResizeObserver=RO;
  function Image(w,h){var e=document.createElement('img');if(w!==undefined)e.width=w;if(h!==undefined)e.height=h;return e;}
  G.Image=Image;
  function mkctor(nat){var w=function(t,i){return nat(t,i);};w.prototype=nat.prototype;return w;}
  ['Event','CustomEvent','UIEvent','MouseEvent','KeyboardEvent'].forEach(function(nm){if(typeof G[nm]==='function')G[nm]=mkctor(G[nm]);});
})(globalThis);")
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
         (svgep (js:make-object :proto ep))  ; SVGElement.prototype (inherits Element)
         (window (js:eval-script realm "globalThis")))
    (setf (context-protos ctx)
          (list :node np :element ep :document dp :text cp :comment cp
                :fragment np :event evp :doctype dtp :svg-element svgep :window window))
    (install-node-proto ctx np)
    (install-element-proto ctx ep)
    (install-svg-element-proto ctx svgep)
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
    (install-window-events ctx)
    ctx))

(defun fire-lifecycle-events (ctx)
  "Signal that parsing is done: DOMContentLoaded (on document, and on window) then
   window `load`.  Real pages gate their startup on these — e.g. skin/menu code
   that runs on ready.  Each is followed by draining the task queue so handlers
   they schedule (module init, deferred work) get to run."
  (dispatch-event ctx (context-document ctx) (make-event-object ctx "DOMContentLoaded" nil))
  (fire-window-event ctx "DOMContentLoaded")
  (run-event-loop ctx :max-tasks 10000)
  (fire-window-event ctx "load")
  (run-event-loop ctx :max-tasks 10000))

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

(defun execute-script (ctx script)
  "Run one classic-JavaScript SCRIPT node once against CTX's realm.  Marks it so
   it never runs twice (re-insertion is a no-op); a script error is reported but
   does not abort (browser behavior).  Returns T if it ran."
  (unless (or (gethash script (context-ran-scripts ctx))
              (not (classic-javascript-p script)))
    (setf (gethash script (context-ran-scripts ctx)) t)
    (let ((source (script-source ctx script)))
      (when (and source (plusp (length source)))
        (let ((saved (context-current-script ctx)))
          (setf (context-current-script ctx) script)
          (unwind-protect
               (handler-case (js:eval-script (context-realm ctx) source)
                 (js:shuttle-error (e)
                   (format *error-output* "~&weft.script: uncaught ~a~%" e))
                 (error (e)
                   (format *error-output* "~&weft.script: script error: ~a~%" e)))
            (setf (context-current-script ctx) saved)))
        t))))

(defun run-inline-scripts (ctx)
  "Execute every classic-JavaScript <script> present at parse time, in document
   order.  Non-JS <script> blocks (JSON-LD, templates, modules) are skipped."
  (dolist (script (dom:get-elements-by-tag-name (context-document ctx) "script"))
    (execute-script ctx script))
  ctx)

(defun connected-p (ctx node)
  "True when NODE is in CTX's live document tree."
  (loop for n = node then (h:dnode-parent n)
        while n thereis (eq n (context-document ctx))))

(defun run-inserted-scripts (ctx node)
  "Browser 'a script element is inserted into a document' behavior: after NODE is
   inserted (and connected), execute any not-yet-run classic <script> it brings
   in — this is how a page's module loader (e.g. MediaWiki ResourceLoader) pulls
   code by appending <script src>.  A load event is fired so onload chains run."
  (when (and (context-loader ctx) (connected-p ctx node))
    (labels ((walk (n)
               (when (eq (h:dnode-kind n) :element)
                 (when (and (string-equal (h:dnode-name n) "script")
                            (execute-script ctx n))
                   (fire-event-later ctx n "load"))
                 (loop for c across (h:dnode-children n) do (walk c)))))
      (walk node))))

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
                             (run-event-loop ctx :max-tasks 10000)
                             (fire-lifecycle-events ctx))
            render-keys)
     ctx)))

(defun render-scripted-to-png (html css width path &rest keys)
  "Render HTML+CSS (running inline <script>) at WIDTH px and save a PNG.
   Returns (values path context)."
  (multiple-value-bind (cv ctx)
      (apply #'render-scripted-to-canvas html css width keys)
    (r:write-png cv path)
    (values path ctx)))
