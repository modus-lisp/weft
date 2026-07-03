;;;; src/script/loader.lisp — the subresource + document loading pipeline.
;;;;
;;;; This is where the page reaches back out for the things it references: a
;;;; frame's document, a script's source, a data: URI.  data: URIs are decoded
;;;; inline; everything else goes through the context's LOADER (a host closure,
;;;; e.g. local files for the test harness or weft/fetch for a live page).  A
;;;; completed load fires a `load` event on the macrotask queue, so on* handlers
;;;; and addEventListener('load', …) run in the normal event loop.
(in-package #:weft.script)

;;; ---- percent + base64 decoding (data: URIs) -------------------------------
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

;;; ---- data: URIs -----------------------------------------------------------
(defun kind-from-mime (mime)
  (cond ((null mime) :text)
        ((search "html" mime) :html)
        ((or (search "xml" mime) (search "svg" mime)) :xml)
        ((search "css" mime) :css)
        ((search "javascript" mime) :js)
        ((search "ecmascript" mime) :js)
        ((search "image/" mime) :image)
        (t :text)))

(defun decode-data-url (url)
  "Decode data:[<mime>][;base64],<data> -> (values kind content-string)."
  (let* ((body (subseq url 5)) (comma (position #\, body)))
    (if (null comma) (values :text "")
        (let* ((meta (subseq body 0 comma)) (data (subseq body (1+ comma)))
               (mime (let ((semi (position #\; meta))) (if semi (subseq meta 0 semi) meta)))
               (content (if (search ";base64" meta)
                            (base64-decode (percent-decode data))
                            (percent-decode data))))
          (values (kind-from-mime (if (plusp (length mime)) mime nil)) content)))))

(defun resolve-url (ctx url)
  "Resolve URL against the context's base to an absolute URL (unchanged if there
   is no base or resolution fails)."
  (let ((base (context-base ctx)))
    (if (and (plusp (length base)) (stringp url) (plusp (length url)))
        (let ((u (ignore-errors (weft.url:parse url base)))) (if u (weft.url:href u) url))
        url)))

(defun load-resource (ctx url)
  "Resolve URL and load it. Returns (values kind content) — kind is
   :html/:xml/:css/:js/:image/:text, content a string; (values nil nil) if the
   resource can't be loaded."
  (cond
    ((null url) (values nil nil))
    ((and (>= (length url) 5) (string-equal (subseq url 0 5) "data:"))
     (decode-data-url url))
    ((context-loader ctx) (funcall (context-loader ctx) ctx url))
    (t (values nil nil))))

;;; ---- external stylesheets (<link rel=stylesheet> + @import) ---------------
;;; A page's <link rel=stylesheet href> and any @import inside a fetched sheet
;;; are pulled through the loader (data: URIs decoded inline), resolved against
;;; the document / sheet base, and spliced into the DOM as <style> elements so
;;; the ordinary cascade picks them up at the link's source position.  Only a
;;; resource the loader reports as :css is applied — a sheet served with the
;;; wrong MIME (text/html) is ignored, as a standards-mode browser would.  Any
;;; failure (network, oversized, non-CSS, malformed) degrades to no styling.
(defparameter *max-stylesheet-bytes* 4000000
  "Fetched sheets larger than this are ignored rather than risk stalling layout.")
(defparameter *max-import-depth* 5
  "How deep an @import chain is followed before giving up.")

(defun resolve-against (base url)
  "Resolve URL against BASE to an absolute URL (URL unchanged if resolution fails)."
  (if (and (stringp base) (plusp (length base)) (stringp url) (plusp (length url)))
      (let ((u (ignore-errors (weft.url:parse url base)))) (if u (weft.url:href u) url))
      url))

(defun parse-import-prelude (s)
  "Parse an @import prelude S (the text between `@import` and `;`).  Returns
   (values url media-text) or (values nil nil) when malformed."
  (let ((s (string-trim '(#\Space #\Tab #\Newline #\Return) s)))
    (cond
      ((zerop (length s)) (values nil nil))
      ((or (char= (char s 0) #\") (char= (char s 0) #\'))
       (let ((close (position (char s 0) s :start 1)))
         (if close (values (subseq s 1 close) (string-trim " " (subseq s (1+ close))))
             (values nil nil))))
      ((and (>= (length s) 4) (string-equal (subseq s 0 4) "url("))
       (let ((close (position #\) s)))
         (if close (values (string-trim '(#\Space #\Tab #\" #\') (subseq s 4 close))
                           (string-trim " " (subseq s (1+ close))))
             (values nil nil))))
      (t (values nil nil)))))

(defun expand-imports (ctx css base depth)
  "Replace @import rules in CSS with the imported sheets' text, resolved against
   BASE and recursively expanded, wrapping a media-qualified import in @media so
   the media engine evaluates it.  A failed/non-CSS import drops to nothing."
  (with-output-to-string (out)
    (let ((i 0) (n (length css)))
      (loop while (< i n) do
        (let ((at (search "@import" css :start2 i :test #'char-equal)))
          (cond
            ((null at) (write-string css out :start i) (setf i n))
            (t
             (write-string css out :start i :end at)
             (let* ((semi (position #\; css :start at))
                    (prelude (subseq css (+ at 7) (or semi n))))
               (multiple-value-bind (url media) (parse-import-prelude prelude)
                 (when url
                   (let ((text (fetch-stylesheet ctx (resolve-against base url) (1+ depth))))
                     (when text
                       (if (plusp (length (string-trim '(#\Space #\Tab #\Newline) (or media ""))))
                           (format out "@media ~a {~%~a~%}~%" media text)
                           (progn (write-string text out) (terpri out)))))))
               (setf i (if semi (1+ semi) n))))))))))

(defun fetch-stylesheet (ctx abs-url depth)
  "Load the CSS at ABS-URL through the loader and expand its @imports.  Applies
   only when the loader reports kind :css and the sheet is within the size cap.
   Returns the expanded CSS text or NIL.  Never signals."
  (when (> depth *max-import-depth*) (return-from fetch-stylesheet nil))
  (handler-case
      (multiple-value-bind (kind content) (load-resource ctx abs-url)
        (when (and (eq kind :css) (stringp content)
                   (<= (length content) *max-stylesheet-bytes*))
          (expand-imports ctx content abs-url depth)))
    (error () nil)))

(defun external-stylesheet-link-p (node)
  "True for a <link rel=stylesheet href> whose href is a non-data URL to fetch."
  (and (eq (h:dnode-kind node) :element)
       (string-equal (h:dnode-name node) "link")
       (let ((rel (cdr (assoc "rel" (h:dnode-attrs node) :test #'string-equal))))
         (and rel (search "stylesheet" (string-downcase rel))))
       (let ((href (cdr (assoc "href" (h:dnode-attrs node) :test #'string-equal))))
         (and href (plusp (length href))
              (not (and (>= (length href) 5) (string-equal (subseq href 0 5) "data:")))))))

(defun splice-external-link (ctx link)
  "Fetch LINK's external sheet and, on success, rewrite LINK in place into a
   <style> element carrying the resolved CSS (wrapped in @media when the link
   has a `media` attribute).  Node identity and source position are preserved,
   so the cascade and document.styleSheets see one sheet where the link was."
  (let* ((href (cdr (assoc "href" (h:dnode-attrs link) :test #'string-equal)))
         (media (cdr (assoc "media" (h:dnode-attrs link) :test #'string-equal)))
         (css (fetch-stylesheet ctx (resolve-against (context-base ctx) href) 0)))
    (when css
      (let ((text (if (and media (plusp (length (string-trim '(#\Space #\Tab #\Newline) media))))
                      (format nil "@media ~a {~%~a~%}" media css)
                      css)))
        (setf (h:dnode-name link) "style")
        (setf (fill-pointer (h:dnode-children link)) 0)
        (h:dom-append link (h:make-text text))))))

(defun inline-external-stylesheets (ctx)
  "Fetch every <link rel=stylesheet href> (non-data) through the loader and
   splice the resolved CSS into the DOM.  A no-op without a loader.  Never
   signals: any failed sheet simply leaves that link unstyled."
  (when (context-loader ctx)
    (labels ((walk (node)
               (when (and (eq (h:dnode-kind node) :element)
                          (external-stylesheet-link-p node))
                 (ignore-errors (splice-external-link ctx node)))
               (loop for c across (copy-seq (h:dnode-children node)) do (walk c))))
      (walk (context-document ctx)))))

;;; ---- on* event-handler properties -----------------------------------------
(defun on-table (ctx node)
  (or (gethash node (context-on-handlers ctx))
      (setf (gethash node (context-on-handlers ctx)) (make-hash-table :test 'equal))))

(defun get-on-handler (ctx node type)
  (or (gethash type (on-table ctx node)) js:*null*))

(defun set-on-handler (ctx node type fn)
  "Set node's on<TYPE> handler. The first assignment registers one persistent
   listener that dispatches whatever handler is current, so reassigning just
   updates the table."
  (let* ((tbl (on-table ctx node)) (had (nth-value 1 (gethash type tbl))))
    (if (js:js-callable-p fn) (setf (gethash type tbl) fn) (remhash type tbl))
    (when (and (not had) (js:js-callable-p fn))
      (add-listener ctx node type
        (js:native-function (context-realm ctx) (concatenate 'string "on" type)
          (lambda (this args)
            (let ((h (gethash type (on-table ctx node))))
              (if (js:js-callable-p h) (js:js-call h this args) js:*undefined*)))
          1)
        nil))))

(defparameter +on-events+
  '("load" "error" "click" "submit" "change" "focus" "blur" "input"
    "keydown" "keyup" "keypress" "mousedown" "mouseup" "mouseover" "mouseout"
    "mousemove" "dblclick" "contextmenu" "scroll" "resize" "unload"))

(defun on-event-attr-p (name)
  "True if NAME is an on<event> content attribute (onclick, onload, …)."
  (and (> (length name) 2) (string-equal (subseq name 0 2) "on")
       (member (string-downcase (subseq name 2)) +on-events+ :test #'string=)))

(defun register-inline-handler (ctx node name code)
  "Compile an on<event>=\"CODE\" content attribute into an event handler.  The
   handler body runs with `event` in scope (the classic inline-handler form)."
  (let ((fn (ignore-errors
             (js:eval-script (context-realm ctx)
                             (format nil "(function(event){~a~%})" code)))))
    (when (js:js-callable-p fn)
      (set-on-handler ctx node (string-downcase (subseq name 2)) fn))))

(defun install-on-handlers (ctx target)
  "Install on<event> accessor properties on prototype TARGET."
  (dolist (type +on-events+)
    (let ((type type) (prop (concatenate 'string "on" type)))
      (js:put-accessor target prop
        :get (js:native-function (context-realm ctx) (concatenate 'string "get " prop)
               (lambda (this ig) (declare (ignore ig))
                 (let ((n (node-of ctx this))) (if n (get-on-handler ctx n type) js:*null*)))
               0)
        :set (js:native-function (context-realm ctx) (concatenate 'string "set " prop)
               (lambda (this a)
                 (let ((n (node-of ctx this)))
                   (when n (set-on-handler ctx n type (arg a 0))))
                 js:*undefined*)
               1)
        :enumerable t :configurable t))))

(defun register-parsed-inline-handlers (ctx)
  "Register on<event> handlers already present as attributes in the parsed DOM
   (a real page's <button onclick=\"…\">)."
  (labels ((walk (node)
             (when (eq (h:dnode-kind node) :element)
               (dolist (a (h:dnode-attrs node))
                 (when (on-event-attr-p (car a))
                   (register-inline-handler ctx node (car a) (cdr a)))))
             (loop for c across (h:dnode-children node) do (walk c))))
    (walk (context-document ctx))))

;;; ---- loading a browsing context (iframe / object) -------------------------
(defun fire-event-later (ctx node type)
  "Queue a TYPE event to be dispatched on NODE as a macrotask."
  (schedule-task ctx
                 (lambda ()
                   (let ((ev (make-event-object ctx type nil)))
                     (dispatch-event ctx node ev)))))

(defun parse-into-document (content)
  "Build a document from CONTENT (an HTML/XML source string)."
  (if (stringp content) (h:parse-html content)
      (let ((d (h:make-document))) (h:dom-append d (h:make-element "html")) d)))

(defun load-frame (ctx element url)
  "Load URL into ELEMENT's (iframe/object) content document and queue its load
   event. A missing/opaque resource still yields an empty document and a load
   event (as a browser navigates a frame to an error page). Never signals — a
   load failure must not take the page's script down."
  (let ((content (handler-case (nth-value 1 (load-resource ctx url)) (error () nil))))
    (setf (gethash element (context-iframe-docs ctx))
          (if content (parse-into-document content)
              (let ((d (h:make-document))) (h:dom-append d (h:make-element "html")) d)))
    (fire-event-later ctx element "load")))

;;; ---- document.open / write / close (full document replacement) ------------
(defun document-replace (ctx doc source)
  "Replace DOC's children with the tree parsed from SOURCE (the document.write /
   document.open+write+close path when writing a whole document)."
  (let ((parsed (h:parse-html source)))
    (loop for c across (h:dnode-children doc) do (setf (h:dnode-parent c) nil))
    (setf (fill-pointer (h:dnode-children doc)) 0)
    (loop for c across (copy-seq (h:dnode-children parsed))
          do (h:dom-remove c) (h:dom-append doc c))
    (setf (context-dirty ctx) t)))
