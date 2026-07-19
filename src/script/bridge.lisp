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

;;; High Resolution Time (W3C) + Performance Timeline (W3C).  performance.now()
;;; is a monotonic clock in fractional milliseconds relative to this context's
;;; navigation start (the CONTEXT-START-REAL-TIME captured at make-context);
;;; timeOrigin is that same instant expressed as Unix-epoch ms.  The timeline
;;; (mark/measure/getEntries*), the legacy PerformanceTiming/PerformanceNavigation
;;; objects, and the PerformanceObserver stub are layered in JS on those two
;;; native primitives.
(defun install-performance (ctx)
  (let ((realm (context-realm ctx)))
    (js:define-global realm "__weft_perf_now"
      (js:native-function realm "now"
        (lambda (this args) (declare (ignore this args))
          (num (* 1000d0 (/ (float (- (get-internal-real-time)
                                       (context-start-real-time ctx)) 1d0)
                            internal-time-units-per-second))))
        0))
    ;; timeOrigin: navigation start as Unix-epoch ms (2208988800 = universal->Unix).
    (js:define-global realm "__weft_perf_time_origin"
      (num (* 1000d0 (- (get-universal-time) 2208988800))))
    (js:eval-script realm "(function(G){
  var now=G.__weft_perf_now, t0=G.__weft_perf_time_origin;
  try{delete G.__weft_perf_now;delete G.__weft_perf_time_origin;}catch(e){}
  function Entry(name,type,start,dur){this.name=String(name);this.entryType=type;this.startTime=start;this.duration=dur;}
  Entry.prototype.toJSON=function(){return {name:this.name,entryType:this.entryType,startTime:this.startTime,duration:this.duration};};
  var entries=[];
  function markStart(name){for(var i=entries.length-1;i>=0;i--)if(entries[i].name===name&&entries[i].entryType==='mark')return entries[i].startTime;return null;}
  function resolve(v){if(v==null)return null;if(typeof v==='number')return v;var m=markStart(String(v));return m==null?0:m;}
  var perf={};
  perf.now=function(){return now();};
  perf.timeOrigin=t0;
  perf.mark=function(name,opt){var s=(opt&&opt.startTime!=null)?opt.startTime:now();var e=new Entry(name,'mark',s,0);entries.push(e);return e;};
  perf.measure=function(name,start,end){
    var sv,ev;
    if(start&&typeof start==='object'){
      var so=resolve(start.start),eo=resolve(start.end),du=start.duration;
      if(so!=null){sv=so;ev=(eo!=null)?eo:(du!=null?so+du:now());}
      else if(eo!=null){ev=eo;sv=(du!=null)?eo-du:0;}
      else{sv=0;ev=now();}
    } else {
      sv=resolve(start);ev=resolve(end);
      if(sv==null)sv=0;if(ev==null)ev=now();
    }
    var e=new Entry(name,'measure',sv,ev-sv);entries.push(e);return e;};
  perf.getEntries=function(){return entries.slice();};
  perf.getEntriesByType=function(t){t=String(t);return entries.filter(function(e){return e.entryType===t;});};
  perf.getEntriesByName=function(n,t){n=String(n);return entries.filter(function(e){return e.name===n&&(t===undefined||e.entryType===String(t));});};
  perf.clearMarks=function(n){entries=entries.filter(function(e){return e.entryType!=='mark'||(n!==undefined&&e.name!==String(n));});};
  perf.clearMeasures=function(n){entries=entries.filter(function(e){return e.entryType!=='measure'||(n!==undefined&&e.name!==String(n));});};
  perf.clearResourceTimings=function(){};
  perf.setResourceTimingBufferSize=function(){};
  perf.addEventListener=function(){};perf.removeEventListener=function(){};perf.dispatchEvent=function(){return false;};
  var nav0=Math.floor(t0);
  perf.timing={navigationStart:nav0,unloadEventStart:0,unloadEventEnd:0,redirectStart:0,redirectEnd:0,
    fetchStart:nav0,domainLookupStart:nav0,domainLookupEnd:nav0,connectStart:nav0,connectEnd:nav0,
    secureConnectionStart:0,requestStart:nav0,responseStart:nav0,responseEnd:nav0,
    domLoading:nav0,domInteractive:nav0,domContentLoadedEventStart:nav0,domContentLoadedEventEnd:nav0,
    domComplete:nav0,loadEventStart:nav0,loadEventEnd:nav0,
    toJSON:function(){var o={};for(var k in this)if(typeof this[k]!=='function')o[k]=this[k];return o;}};
  perf.navigation={type:0,redirectCount:0,TYPE_NAVIGATE:0,TYPE_RELOAD:1,TYPE_BACK_FORWARD:2,TYPE_RESERVED:255,
    toJSON:function(){return {type:this.type,redirectCount:this.redirectCount};}};
  perf.toJSON=function(){return {timeOrigin:t0,timing:this.timing.toJSON(),navigation:this.navigation.toJSON()};};
  perf.eventCounts={size:0,get:function(){return 0;}};
  perf.memory={jsHeapSizeLimit:0,totalJSHeapSize:0,usedJSHeapSize:0};
  G.performance=perf;
  function PerformanceObserver(cb){this._cb=cb;}
  PerformanceObserver.prototype.observe=function(){};
  PerformanceObserver.prototype.disconnect=function(){};
  PerformanceObserver.prototype.takeRecords=function(){return [];};
  PerformanceObserver.supportedEntryTypes=['mark','measure','navigation','resource','paint','longtask'];
  G.PerformanceObserver=PerformanceObserver;
  function PerformanceObserverEntryList(){}
  G.PerformanceObserverEntryList=PerformanceObserverEntryList;
})(globalThis);")))

(defun install-globals (ctx)
  (let* ((realm (context-realm ctx))
         (window (proto ctx :window))
         (docobj (wrap ctx (context-document ctx))))
    (setf (context-document-obj ctx) docobj)
    (js:define-global realm "document" docobj)
    (js:define-global realm "window" window)
    (js:define-global realm "self" window)
    ;; HTML §7.3.3 named access on the Window object (fallback below real globals).
    (install-window-named-access ctx window)
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
      (js:put nav "hardwareConcurrency" (num 4))
      ;; sendBeacon (W3C Beacon API): a static render has no live network, so it
      ;; queues nothing but reports success — the near-universal analytics
      ;; transport path.  Scripts wrap `navigator.sendBeacon.bind(navigator)`, so
      ;; its mere presence keeps that instrumentation from throwing.
      (js:put nav "sendBeacon"
              (js:native-function realm "sendBeacon"
                (lambda (this args) (declare (ignore this args)) js:*true*) 1))
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
                 "alert" "resizeTo" "moveTo"))
      (js:define-global realm m (js:native-function realm m (lambda (this args) (declare (ignore this args)) js:*undefined*) 0)))
    ;; requestIdleCallback (W3C Background Tasks): run the callback as a deferred
    ;; task with a timeRemaining()=~50ms IdleDeadline.  cancelIdleCallback cancels
    ;; the pending timer.  Analytics/hydration code schedules low-priority work here.
    (js:define-global realm "requestIdleCallback"
      (js:native-function realm "requestIdleCallback"
        (lambda (this args) (declare (ignore this))
          (let ((cb (arg args 0)))
            (if (js:js-callable-p cb)
                (num (schedule-timer ctx
                       (lambda ()
                         (let ((dl (js:make-object :proto (js:eval-script realm "Object.prototype"))))
                           (js:put dl "didTimeout" js:*false*)
                           (js:put dl "timeRemaining"
                                   (js:native-function realm "timeRemaining"
                                     (lambda (a b) (declare (ignore a b)) (num 50d0)) 0))
                           (js:invoke (context-realm ctx) cb js:*undefined* (list dl))))
                       1 nil))
                (num 0)))) 1))
    (js:define-global realm "cancelIdleCallback"
      (js:native-function realm "cancelIdleCallback"
        (lambda (this args) (declare (ignore this)) (cancel-timer ctx (int-arg args 0)) js:*undefined*) 1))
    (js:define-global realm "matchMedia"
      (js:native-function realm "matchMedia"
        (lambda (this args) (declare (ignore this))
          (let ((o (js:make-object :proto (js:eval-script realm "Object.prototype"))))
            (js:put o "matches" js:*false*) (js:put o "media" (jstr (arg args 0)))
            (js:put o "addListener" (js:native-function realm "addListener" (lambda (a b) (declare (ignore a b)) js:*undefined*) 0))
            o)) 1))
    ;; Per WebIDL an interface object is itself a callable function object: this
    ;; is what makes `x instanceof Node` (RHS must be callable) work, and what
    ;; lets the constructable interfaces (Document/Text/Comment/DocumentFragment,
    ;; DOM §"Interface") be invoked with `new`.  Each carries .prototype pointing
    ;; at the shared prototype and (for Node) the nodeType constants.
    (flet ((iface (name proto-key &optional construct)
             ;; CONSTRUCT (this args)->node builds an instance; abstract
             ;; interfaces (no CONSTRUCT) throw "Illegal constructor" per WebIDL.
             (let ((f (js:native-function realm name
                        (or construct
                            (lambda (this args) (declare (ignore this args))
                              (js:js-throw (js:make-native-error
                                            "TypeError" "Illegal constructor"))))
                        0)))
               (js:put f "prototype" (proto ctx proto-key) :enumerable nil :writable nil)
               (js:define-global realm name f) f)))
      (let ((node-iface (iface "Node" :node)))
        (dolist (pair '(("ELEMENT_NODE" . 1) ("ATTRIBUTE_NODE" . 2) ("TEXT_NODE" . 3)
                        ("CDATA_SECTION_NODE" . 4) ("ENTITY_REFERENCE_NODE" . 5)
                        ("ENTITY_NODE" . 6) ("PROCESSING_INSTRUCTION_NODE" . 7)
                        ("COMMENT_NODE" . 8) ("DOCUMENT_NODE" . 9) ("DOCUMENT_TYPE_NODE" . 10)
                        ("DOCUMENT_FRAGMENT_NODE" . 11) ("NOTATION_NODE" . 12)
                        ("DOCUMENT_POSITION_DISCONNECTED" . 1)
                        ("DOCUMENT_POSITION_PRECEDING" . 2) ("DOCUMENT_POSITION_FOLLOWING" . 4)
                        ("DOCUMENT_POSITION_CONTAINS" . 8) ("DOCUMENT_POSITION_CONTAINED_BY" . 16)
                        ("DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC" . 32)))
          (js:put node-iface (car pair) (num (cdr pair)) :enumerable nil :writable nil :configurable nil)))
      (iface "Element" :element)
      (iface "CharacterData" :text)
      (iface "DocumentType" :doctype)
      (iface "Attr" :attr)
      (iface "NamedNodeMap" :namednodemap)
      (iface "DOMImplementation" :domimplementation)
      ;; HTMLElement / HTMLUnknownElement / per-tag interfaces (HTML §): abstract
      ;; constructors whose .prototype links the wrapper chain so instanceof works.
      ;; HTMLElement's constructor inherits Element's; each specific one inherits
      ;; HTMLElement's, matching WebIDL interface-object inheritance.
      (let ((element-ctor (js:eval-script realm "Element"))
            (htmlelement-ctor nil))
        (dolist (pair (getf (context-protos ctx) :html-ifaces))
          (let* ((name (car pair)) (protoobj (cdr pair))
                 (f (js:native-function realm name
                      (lambda (this args) (declare (ignore this args))
                        (js:js-throw (js:make-native-error "TypeError" "Illegal constructor")))
                      0)))
            (js:put f "prototype" protoobj :enumerable nil :writable nil :configurable nil)
            (js:put protoobj "constructor" f :enumerable nil :writable t :configurable t)
            (when (string= name "HTMLElement") (setf htmlelement-ctor f))
            (setf (js:js-object-proto f) (if (string= name "HTMLElement") element-ctor
                                             (or htmlelement-ctor element-ctor)))
            (js:define-global realm name f))))
      ;; Constructable node interfaces (DOM §Document/Text/Comment/DocumentFragment).
      (let ((doc-iface (iface "Document" :document
                              (lambda (this args) (declare (ignore this args))
                                ;; new Document() has content type application/xml and is
                                ;; not an HTML document (DOM §Document constructor), so
                                ;; createElement preserves case / uses no namespace.
                                (let ((d (h:make-document)))
                                  (setf (gethash d (context-doc-content-types ctx)) "application/xml")
                                  (wrap ctx d))))))
        ;; XMLDocument (DOM §XMLDocument): a non-constructable interface whose
        ;; prototype inherits Document.prototype and whose interface object
        ;; inherits Document (so `x instanceof XMLDocument` implies Document).
        (let ((xdoc-iface (iface "XMLDocument" :xmldocument)))
          (setf (js:js-object-proto xdoc-iface) doc-iface)
          (js:put (proto ctx :xmldocument) "constructor" xdoc-iface
                  :enumerable nil :writable t :configurable t)))
      (iface "Text" :text
             (lambda (this args) (declare (ignore this))
               (new-node ctx docobj
                         (h:make-text (if (js:js-undefined-p (arg args 0)) ""
                                          (jstr (arg args 0)))))))
      (iface "Comment" :comment
             (lambda (this args) (declare (ignore this))
               (new-node ctx docobj
                         (h:make-comment (if (js:js-undefined-p (arg args 0)) ""
                                             (jstr (arg args 0)))))))
      (iface "DocumentFragment" :fragment
             (lambda (this args) (declare (ignore this args))
               (new-node ctx docobj (h:make-fragment))))
      ;; ProcessingInstruction (abstract): its prototype chains CharacterData so
      ;; `pi instanceof ProcessingInstruction` and cloneNode's type check hold.
      (iface "ProcessingInstruction" :pi)
      ;; Live-collection interfaces (abstract): make `coll instanceof
      ;; NodeList/HTMLCollection` hold.  Their prototypes back make-collection.
      (iface "NodeList" :nodelist)
      (iface "HTMLCollection" :htmlcollection)
      (js:put (proto ctx :nodelist) (js:eval-script realm "Symbol.toStringTag")
              "NodeList" :enumerable nil :writable nil :configurable t)
      (js:put (proto ctx :htmlcollection) (js:eval-script realm "Symbol.toStringTag")
              "HTMLCollection" :enumerable nil :writable nil :configurable t)
      ;; NodeList is iterable (DOM §); per WebIDL its values/keys/entries/forEach
      ;; and @@iterator are the very same functions as Array.prototype's (the tests
      ;; assert `list.keys === Array.prototype.keys`).  They are generic over any
      ;; array-like, so they run against the live length/index [[Get]] traps.
      (let* ((nlp (proto ctx :nodelist))
             (arrp (js:eval-script realm "Array.prototype"))
             (sym (js:eval-script realm "Symbol.iterator")))
        (dolist (m '("values" "keys" "entries" "forEach"))
          (js:put nlp m (js:js-get arrp m) :enumerable nil :writable t :configurable t))
        (js:put nlp sym (js:js-get arrp sym) :enumerable nil :writable t :configurable t)))
    ;; URL / URLSearchParams / XMLHttpRequest / IntersectionObserver / ResizeObserver —
    ;; Web APIs real pages depend on.  URL parsing is delegated to weft.url (WHATWG)
    ;; through a native helper; the rest are JS polyfills.  IntersectionObserver /
    ;; ResizeObserver are inert stubs — firing them "intersecting" up front makes a
    ;; heavy SPA (NYT) eagerly load its entire below-the-fold image pipeline (and each
    ;; onload cascades into more), which costs seconds for content outside the viewport;
    ;; above-the-fold lazy <img>s already resolve via IMG-SOURCE-URL's data-src read.
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
    ;; DOMParser backing: HTML types go through weft's WHATWG tree builder; XML
    ;; types (text/xml, application/xml, application/xhtml+xml, image/svg+xml) go
    ;; through the namespace-aware XML parser, which yields a plain Document (not an
    ;; XMLDocument) or a parsererror document (DOMParser §).
    (js:define-global realm "__weft_parse_document"
      (js:native-function realm "__weft_parse_document"
        (lambda (this args) (declare (ignore this))
          (let ((src (jstr (arg args 0)))
                (type (if (>= (length args) 2) (jstr (arg args 1)) "text/html")))
            (if (member type '("text/xml" "application/xml"
                               "application/xhtml+xml" "image/svg+xml")
                        :test #'string=)
                (parse-xml-document ctx src type)
                (let ((doc (h:parse-html src)))
                  (unless (or (zerop (length type)) (string-equal type "text/html"))
                    (setf (gethash doc (context-doc-content-types ctx)) type))
                  (wrap ctx doc)))))
        2))
    ;; XMLSerializer backing (DOM Parsing & Serialization §2.2): the XML
    ;; serialization algorithm runs natively over the weft node.
    (js:define-global realm "__weft_xml_serialize"
      (js:native-function realm "__weft_xml_serialize"
        (lambda (this args) (declare (ignore this))
          (let* ((obj (arg args 0)) (node (node-of ctx obj)))
            (if node (xml-serialize-root ctx node) "")))
        1))
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
  IO.prototype.observe=function(){};IO.prototype.unobserve=function(){};IO.prototype.disconnect=function(){};IO.prototype.takeRecords=function(){return [];};
  G.IntersectionObserver=IO;IO.prototype.root=null;IO.prototype.rootMargin='0px';IO.prototype.thresholds=[0];
  function RO(cb){this._cb=cb;}RO.prototype.observe=function(){};RO.prototype.unobserve=function(){};RO.prototype.disconnect=function(){};
  G.ResizeObserver=RO;
  function Image(w,h){var e=document.createElement('img');if(w!==undefined)e.width=w;if(h!==undefined)e.height=h;return e;}
  G.Image=Image;
  function Headers(init){this._h={};var self=this;if(init&&typeof init==='object'&&typeof init.forEach!=='function'){Object.keys(init).forEach(function(k){self._h[String(k).toLowerCase()]=String(init[k]);});}}
  Headers.prototype.get=function(k){var v=this._h[String(k).toLowerCase()];return v===undefined?null:v;};
  Headers.prototype.set=function(k,v){this._h[String(k).toLowerCase()]=String(v);};Headers.prototype.append=Headers.prototype.set;
  Headers.prototype.has=function(k){return String(k).toLowerCase() in this._h;};Headers.prototype['delete']=function(k){delete this._h[String(k).toLowerCase()];};
  Headers.prototype.forEach=function(cb,t){var self=this;Object.keys(this._h).forEach(function(k){cb.call(t,self._h[k],k,self);});};
  G.Headers=Headers;
  function Response(body,init){init=init||{};this._body=body==null?'':String(body);this.status=init.status||200;this.ok=this.status>=200&&this.status<300;this.statusText=init.statusText||'';this.headers=new Headers(init.headers);this.url='';this.redirected=false;this.type='basic';this.bodyUsed=false;}
  Response.prototype.text=function(){return Promise.resolve(this._body);};
  Response.prototype.json=function(){var b=this._body;return new Promise(function(res,rej){try{res(JSON.parse(b||'null'));}catch(e){rej(e);}});};
  Response.prototype.clone=function(){return this;};Response.prototype.arrayBuffer=function(){return Promise.resolve(null);};Response.prototype.blob=function(){return Promise.resolve(null);};
  G.Response=Response;
  function Request(u,o){o=o||{};this.url=String(u);this.method=o.method||'GET';this.headers=new Headers(o.headers);this.credentials=o.credentials||'same-origin';}
  G.Request=Request;
  // A static render has no live network for scripts: reject fetch like an offline
  // browser so a page degrades to its server-rendered content instead of throwing
  // an uncaught ReferenceError that aborts hydration mid-flight.
  function fetch(u,o){return Promise.reject(new TypeError('Failed to fetch (static render)'));}
  G.fetch=fetch;
  function DOMParser(){}
  var DP_TYPES={'text/html':1,'text/xml':1,'application/xml':1,'application/xhtml+xml':1,'image/svg+xml':1};
  DOMParser.prototype.parseFromString=function(str,type){
    var t=String(type);
    if(!DP_TYPES[t])throw new TypeError('The provided value is not a valid enum value of type SupportedType.');
    return __weft_parse_document(String(str),t);};
  G.DOMParser=DOMParser;
  function XMLSerializer(){}
  XMLSerializer.prototype.serializeToString=function(node){return __weft_xml_serialize(node);};
  G.XMLSerializer=XMLSerializer;
  // DOMException (WebIDL): a real interface so `e instanceof DOMException` and
  // testharness's `e.constructor === DOMException` (assert_throws_dom) hold.
  var DE_NAME_CODE={IndexSizeError:1,HierarchyRequestError:3,WrongDocumentError:4,InvalidCharacterError:5,NoModificationAllowedError:7,NotFoundError:8,NotSupportedError:9,InUseAttributeError:10,InvalidStateError:11,SyntaxError:12,InvalidModificationError:13,NamespaceError:14,InvalidAccessError:15,TypeMismatchError:17,SecurityError:18,NetworkError:19,AbortError:20,URLMismatchError:21,QuotaExceededError:22,TimeoutError:23,InvalidNodeTypeError:24,DataCloneError:25};
  function DOMException(message,name){this.message=(message===undefined?'':String(message));this.name=(name===undefined?'Error':String(name));this.code=(DE_NAME_CODE[this.name]||0);}
  DOMException.prototype=Object.create(Error.prototype);
  DOMException.prototype.constructor=DOMException;
  DOMException.prototype.name='Error';DOMException.prototype.message='';DOMException.prototype.code=0;
  DOMException.prototype.toString=function(){return this.name+': '+this.message;};
  var DE_CODES={INDEX_SIZE_ERR:1,DOMSTRING_SIZE_ERR:2,HIERARCHY_REQUEST_ERR:3,WRONG_DOCUMENT_ERR:4,INVALID_CHARACTER_ERR:5,NO_DATA_ALLOWED_ERR:6,NO_MODIFICATION_ALLOWED_ERR:7,NOT_FOUND_ERR:8,NOT_SUPPORTED_ERR:9,INUSE_ATTRIBUTE_ERR:10,INVALID_STATE_ERR:11,SYNTAX_ERR:12,INVALID_MODIFICATION_ERR:13,NAMESPACE_ERR:14,INVALID_ACCESS_ERR:15,VALIDATION_ERR:16,TYPE_MISMATCH_ERR:17,SECURITY_ERR:18,NETWORK_ERR:19,ABORT_ERR:20,URL_MISMATCH_ERR:21,QUOTA_EXCEEDED_ERR:22,TIMEOUT_ERR:23,INVALID_NODE_TYPE_ERR:24,DATA_CLONE_ERR:25};
  Object.keys(DE_CODES).forEach(function(k){DOMException[k]=DE_CODES[k];DOMException.prototype[k]=DE_CODES[k];});
  G.DOMException=DOMException;
  if(typeof G.queueMicrotask!=='function'){G.queueMicrotask=function(cb){Promise.resolve().then(cb);};}
  // btoa/atob — HTML base64 utility methods (§ Atob and Btoa).  btoa serializes a
  // binary string (each code unit a byte, 0..255) to base64; atob does the inverse.
  if(typeof G.btoa!=='function'){
    var B64='ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
    G.btoa=function(s){s=String(s);var out='';
      for(var i=0;i<s.length;i++){if(s.charCodeAt(i)>255)throw new DOMException('The string to be encoded contains characters outside of the Latin1 range.','InvalidCharacterError');}
      for(var j=0;j<s.length;j+=3){
        var b0=s.charCodeAt(j),b1=j+1<s.length?s.charCodeAt(j+1):0,b2=j+2<s.length?s.charCodeAt(j+2):0;
        out+=B64.charAt(b0>>2)+B64.charAt((b0&3)<<4|b1>>4)+
             (j+1<s.length?B64.charAt((b1&15)<<2|b2>>6):'=')+
             (j+2<s.length?B64.charAt(b2&63):'=');}
      return out;};
    G.atob=function(s){s=String(s).replace(/[ \t\n\f\r]/g,'');
      if(s.length%4===1)throw new DOMException('The string to be decoded is not correctly encoded.','InvalidCharacterError');
      s=s.replace(/=+$/,'');var out='',bits=0,nbits=0;
      for(var i=0;i<s.length;i++){var c=B64.indexOf(s.charAt(i));
        if(c<0)throw new DOMException('The string to be decoded is not correctly encoded.','InvalidCharacterError');
        bits=bits<<6|c;nbits+=6;if(nbits>=8){nbits-=8;out+=String.fromCharCode(bits>>nbits&255);}}
      return out;};
  }
  // process: a minimal Node-ism many bundlers leave in browser output — the
  // near-universal `process.env.NODE_ENV` guard and a few common stubs.  Not a
  // Node runtime; just enough that `typeof process==='object'` and the env read
  // don't throw an uncaught TypeError that aborts a page's bootstrap.
  if(typeof G.process==='undefined'){
    var proc={env:{NODE_ENV:'production'},platform:'browser',arch:'',version:'',versions:{},
      browser:true,argv:[],pid:0,title:'browser',
      nextTick:function(fn){if(typeof fn!=='function')throw new TypeError('callback is not a function');
        var a=Array.prototype.slice.call(arguments,1);G.queueMicrotask(function(){fn.apply(undefined,a);});},
      cwd:function(){return '/';},chdir:function(){},umask:function(){return 0;},
      on:function(){return proc;},once:function(){return proc;},off:function(){return proc;},
      addListener:function(){return proc;},removeListener:function(){return proc;},
      emit:function(){return false;},listeners:function(){return [];},
      binding:function(){throw new Error('process.binding is not supported');}};
    G.process=proc;
  }
  function mkctor(nat){var w=function(t,i){return nat(t,i);};w.prototype=nat.prototype;return w;}
  ['Event','CustomEvent','UIEvent','MouseEvent','KeyboardEvent',
   'Document','Text','Comment','DocumentFragment'].forEach(function(nm){if(typeof G[nm]==='function')G[nm]=mkctor(G[nm]);});
})(globalThis);")
    docobj))

;;; The HTML element interface map (HTML §"Elements in the DOM"): each specific
;;; interface owns a set of tag names; every other known tag uses HTMLElement and
;;; unknown/custom tags use HTMLUnknownElement.
(defparameter *html-tag-interfaces*
  '(("HTMLHtmlElement" "html") ("HTMLHeadElement" "head") ("HTMLTitleElement" "title")
    ("HTMLBaseElement" "base") ("HTMLLinkElement" "link") ("HTMLMetaElement" "meta")
    ("HTMLStyleElement" "style") ("HTMLBodyElement" "body")
    ("HTMLHeadingElement" "h1" "h2" "h3" "h4" "h5" "h6")
    ("HTMLParagraphElement" "p") ("HTMLHRElement" "hr")
    ("HTMLPreElement" "pre" "listing" "xmp") ("HTMLQuoteElement" "blockquote" "q")
    ("HTMLOListElement" "ol") ("HTMLUListElement" "ul") ("HTMLLIElement" "li")
    ("HTMLDListElement" "dl") ("HTMLDivElement" "div") ("HTMLAnchorElement" "a")
    ("HTMLDataElement" "data") ("HTMLTimeElement" "time") ("HTMLSpanElement" "span")
    ("HTMLBRElement" "br") ("HTMLModElement" "ins" "del") ("HTMLPictureElement" "picture")
    ("HTMLSourceElement" "source") ("HTMLImageElement" "img") ("HTMLIFrameElement" "iframe")
    ("HTMLEmbedElement" "embed") ("HTMLObjectElement" "object") ("HTMLParamElement" "param")
    ("HTMLVideoElement" "video") ("HTMLAudioElement" "audio") ("HTMLTrackElement" "track")
    ("HTMLMapElement" "map") ("HTMLAreaElement" "area") ("HTMLTableElement" "table")
    ("HTMLTableCaptionElement" "caption") ("HTMLTableColElement" "col" "colgroup")
    ("HTMLTableSectionElement" "tbody" "thead" "tfoot") ("HTMLTableRowElement" "tr")
    ("HTMLTableCellElement" "td" "th") ("HTMLFormElement" "form") ("HTMLLabelElement" "label")
    ("HTMLInputElement" "input") ("HTMLButtonElement" "button") ("HTMLSelectElement" "select")
    ("HTMLDataListElement" "datalist") ("HTMLOptGroupElement" "optgroup")
    ("HTMLOptionElement" "option") ("HTMLTextAreaElement" "textarea")
    ("HTMLOutputElement" "output") ("HTMLProgressElement" "progress") ("HTMLMeterElement" "meter")
    ("HTMLFieldSetElement" "fieldset") ("HTMLLegendElement" "legend")
    ("HTMLDetailsElement" "details") ("HTMLDialogElement" "dialog") ("HTMLScriptElement" "script")
    ("HTMLTemplateElement" "template") ("HTMLSlotElement" "slot") ("HTMLCanvasElement" "canvas")
    ("HTMLMenuElement" "menu") ("HTMLFontElement" "font") ("HTMLDirectoryElement" "dir")
    ("HTMLFrameElement" "frame") ("HTMLFrameSetElement" "frameset") ("HTMLMarqueeElement" "marquee")))

(defparameter *html-generic-tags*
  '("abbr" "address" "article" "aside" "b" "bdi" "bdo" "cite" "code" "dd" "dfn"
    "dt" "em" "figcaption" "figure" "footer" "header" "hgroup" "i" "kbd" "main"
    "mark" "nav" "noscript" "rp" "rt" "ruby" "s" "samp" "section" "small" "strong"
    "sub" "summary" "sup" "u" "var" "wbr" "acronym" "basefont" "big" "center"
    "nobr" "noembed" "noframes" "plaintext" "strike" "tt")
  "Known HTML tags whose interface is the generic HTMLElement.")

(defun build-html-interfaces (ctx ep)
  "Create the HTMLElement / HTMLUnknownElement / per-tag prototypes and register
   them in CONTEXT-PROTOS.  Returns an alist of interface-name -> prototype so the
   caller can expose each as a global interface object."
  (let* ((hep (js:make-object :proto ep))    ; HTMLElement.prototype (<- Element)
         (hup (js:make-object :proto hep))    ; HTMLUnknownElement.prototype (<- HTMLElement)
         (tagmap (make-hash-table :test 'equal))
         (ifaces (list (cons "HTMLElement" hep) (cons "HTMLUnknownElement" hup))))
    (dolist (entry *html-tag-interfaces*)
      (let ((proto (js:make-object :proto hep)))
        (push (cons (car entry) proto) ifaces)
        (dolist (tag (cdr entry)) (setf (gethash tag tagmap) proto))))
    (dolist (tag *html-generic-tags*) (setf (gethash tag tagmap) hep))
    (setf (getf (context-protos ctx) :html-element) hep
          (getf (context-protos ctx) :html-unknown) hup
          (getf (context-protos ctx) :html-tag-protos) tagmap)
    (nreverse ifaces)))

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
         (xdp (js:make-object :proto dp))    ; XMLDocument.prototype (<- Document)
         (cp (js:make-object :proto np))     ; CharacterData (Text/Comment)
         (pip (js:make-object :proto cp))    ; ProcessingInstruction.prototype (<- CharacterData)
         (evp (js:make-object :proto op))    ; Event.prototype
         (dtp (js:make-object :proto np))    ; DocumentType.prototype
         (fp (js:make-object :proto np))     ; DocumentFragment.prototype (inherits Node)
         (svgep (js:make-object :proto ep))  ; SVGElement.prototype (inherits Element)
         (nlp (js:make-object :proto op))    ; NodeList.prototype
         (hcp (js:make-object :proto op))    ; HTMLCollection.prototype
         (ap (js:make-object :proto np))     ; Attr.prototype (inherits Node)
         (nnmp (js:make-object :proto op))   ; NamedNodeMap.prototype
         (dip (js:make-object :proto op))    ; DOMImplementation.prototype
         (window (js:eval-script realm "globalThis")))
    (setf (context-protos ctx)
          (list :node np :element ep :document dp :text cp :comment cp :pi pip
                :fragment fp :event evp :doctype dtp :svg-element svgep :window window
                :nodelist nlp :htmlcollection hcp :attr ap :namednodemap nnmp
                :domimplementation dip :xmldocument xdp))
    ;; Per-tag HTML element interface prototypes (HTMLDivElement.prototype, …) so
    ;; that `document.createElement('div') instanceof HTMLDivElement` holds.
    (setf (getf (context-protos ctx) :html-ifaces) (build-html-interfaces ctx ep))
    (install-node-proto ctx np)
    (install-element-proto ctx ep)
    (install-svg-element-proto ctx svgep)
    (install-document-proto ctx dp)
    (install-doctype-proto ctx dtp)
    (install-attr-proto ctx ap)
    (install-namednodemap-proto ctx nnmp)
    (install-dom-implementation-proto ctx dip)
    (install-chardata-proto ctx cp)
    ;; ParentNode (DOM §4.2.6) on Element + Document; ChildNode (§4.2.7) on
    ;; Element, CharacterData (Text/Comment/…) and DocumentType.  (DocumentFragment
    ;; shares the Node prototype NP here, so it is intentionally left out to avoid
    ;; leaking these methods onto every node.)
    (install-parent-node-methods ctx ep)
    (install-parent-node-methods ctx dp)
    (install-fragment-proto ctx fp)
    (install-child-node-methods ctx ep)
    (install-child-node-methods ctx cp)
    (install-child-node-methods ctx dtp)
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
    (install-performance ctx)
    (install-window-events ctx)
    (install-mutation-observer ctx)
    ;; Also set the GLOBAL value of *ctx* (not just the per-entry dynamic binding):
    ;; shuttle runs async-function bodies on a worker thread that does not inherit
    ;; the main thread's dynamic binding, so mutation sites reached from an async
    ;; callback fall back to this global to find the active observer registry.
    (setf *ctx* ctx)
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
        (let ((saved (context-current-script ctx))
              (*ctx* ctx))
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
