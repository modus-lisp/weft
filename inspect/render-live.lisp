;;;; inspect/render-live.lisp — the capstone: render a LIVE HTTPS page to a PNG.
;;;;
;;;; Drives the whole stack end to end over the real web, in pure Common Lisp:
;;;;   seal TLS 1.3 (validated cert) -> HTTP/1.1 -> HTML parse -> CSS cascade
;;;;   (incl. external stylesheets fetched over HTTPS) -> JavaScript -> layout
;;;;   -> paint -> PNG.  The document *and* its <link rel=stylesheet> sheets are
;;;;   fetched through weft.fetch, so https:// subresources travel over seal too.
;;;;   sbcl --script inspect/render-live.lisp [url out.png [width]]
(require :asdf)
(push (truename (merge-pathnames "../" (directory-namestring *load-truename*)))
      asdf:*central-registry*)
(handler-bind ((warning #'muffle-warning))
  (asdf:load-system "weft/script")
  (asdf:load-system "weft/fetch"))

(defpackage #:weft.render-live
  (:use #:cl) (:local-nicknames (#:s #:weft.script) (#:r #:weft.render)
                                (#:f #:weft.fetch))
  (:export #:render #:main))
(in-package #:weft.render-live)

(defun content-kind (headers url)
  "Classify a fetched resource by Content-Type, falling back to URL extension."
  (let ((ct (f:get-header headers "content-type")))
    (cond
      ((and ct (search "css" ct :test #'char-equal)) :css)
      ((and ct (search "html" ct :test #'char-equal)) :html)
      ((and ct (or (search "javascript" ct :test #'char-equal)
                   (search "ecmascript" ct :test #'char-equal))) :js)
      ((and ct (or (search "svg" ct :test #'char-equal)
                   (search "xml" ct :test #'char-equal))) :xml)
      ((and ct (search "image/" ct :test #'char-equal)) :image)
      ((search ".css" url :test #'char-equal) :css)
      ((search ".js" url :test #'char-equal) :js)
      ((search "only=scripts" url :test #'char-equal) :js)   ; MediaWiki load.php
      (t :text))))

(defun js-enabled-p ()
  "External JavaScript is opt-in here (env WEFT_JS): the default renders stay a
   deterministic CSS-only reference, since running a site's live JS is slow and
   non-deterministic.  The interactive shell (loom) turns it on for real browsing."
  (let ((v (uiop:getenv "WEFT_JS"))) (and v (plusp (length v)) (not (string= v "0")))))

(defun images-enabled-p ()
  "Network <img> loading is opt-in here (env WEFT_IMG, or implied by WEFT_JS): the
   default references stay offline/deterministic."
  (or (js-enabled-p)
      (let ((v (uiop:getenv "WEFT_IMG"))) (and v (plusp (length v)) (not (string= v "0"))))))

(defun make-image-loader (base)
  "An (url) -> (values bytes mime) image fetcher over seal, resolving relative and
   protocol-relative URLs against BASE.  NIL bytes on any failure."
  (lambda (url)
    (handler-case
        (let ((abs (cond ((and (>= (length url) 2) (string= (subseq url 0 2) "//"))
                          (concatenate 'string "https:" url))
                         (t (let ((u (ignore-errors (weft.url:parse url base))))
                              (if u (weft.url:href u) url))))))
          (let ((resp (f:fetch abs)))
            (when (and resp (<= 200 (f:response-status resp) 299))
              (values (f:response-body resp)
                      (f:get-header (f:response-headers resp) "content-type")))))
      (error () (values nil nil)))))

(defun https-subresource-loader (base)
  "A subresource loader (ctx url) -> (values kind content) backed by weft.fetch,
   so external stylesheets — and, when WEFT_JS is set, scripts — travel over seal
   TLS (relative URLs resolved against BASE).  Degrades to (values nil nil) on any
   fetch/TLS failure."
  (lambda (ctx url) (declare (ignore ctx))
    (handler-case
        (let ((abs (let ((u (ignore-errors (weft.url:parse url base))))
                     (if u (weft.url:href u) url))))
          (multiple-value-bind (text cs resp) (f:fetch-text abs)
            (declare (ignore cs))
            (if (and text (<= 200 (f:response-status resp) 299))
                (let ((k (content-kind (f:response-headers resp) abs)))
                  (cond ((eq k :css) (values :css text))
                        ((and (eq k :js) (js-enabled-p)) (values :js text))
                        (t (values nil nil))))
                (values nil nil))))
      (error () (values nil nil)))))

(defun render (url out &optional (width 1024))
  "Fetch URL over HTTPS and render it to OUT: external CSS (and, when WEFT_JS is
   set, external JavaScript) travel over seal TLS."
  (multiple-value-bind (html cs resp) (f:fetch-text url)
    (declare (ignore cs))
    (let ((base (f:response-url resp))          ; the final URL is the document base
          (r:*image-loader* (and (images-enabled-p) (make-image-loader (f:response-url resp)))))
      (format t "~&fetched ~a  (HTTP ~d, base ~a, ~:d chars)~a~a~%"
              url (f:response-status resp) base (length html)
              (if (js-enabled-p) "  [JS on]" "") (if (images-enabled-p) "  [images on]" ""))
      (multiple-value-bind (path ctx)
          (s:render-scripted-to-png html "" width out
                                    :min-height 400 :max-height 8000
                                    :base base :loader (https-subresource-loader base))
        (declare (ignore ctx))
        (multiple-value-bind (cv) (s:render-scripted-to-canvas html "" width
                                                               :base base :loader (https-subresource-loader base))
          (format t "rendered -> ~a  (~dx~d, ink=~,3f)~%"
                  path (r:canvas-width cv) (r:canvas-height cv) (r:canvas-ink cv)))
        path))))

(defun main ()
  (let ((args (rest sb-ext:*posix-argv*)))
    (if args
        (render (first args) (or (second args) "/tmp/weft-live.png")
                (if (third args) (parse-integer (third args)) 1024))
        ;; default: the capstone proofs — real HTTPS pages whose styling lives in
        ;; external stylesheets fetched over HTTPS (HN's news.css; IANA / W3C's
        ;; sheets).  A full Wikipedia article is here too: its deeply nested
        ;; infobox/table DOM once overran weft's intrinsic-width layout recursion
        ;; (exponential in table-nesting depth); with that memoised it renders in
        ;; bounded time on a normal stack.
        (let ((dir (asdf:system-relative-pathname "weft" "inspect/")))
          (dolist (job '(("https://news.ycombinator.com/" "live-hn.png" 1024)
                         ("https://www.iana.org/" "live-iana.png" 1000)
                         ("https://www.w3.org/" "live-w3c.png" 1000)
                         ("https://en.wikipedia.org/wiki/HTTPS" "live-wikipedia-https.png" 1024)))
            (destructuring-bind (url name width) job
              (handler-case
                  (render url (namestring (merge-pathnames name dir)) width)
                (error (e) (format t "~&~a: FAILED cleanly -> ~a~%" url e)))))))))

(main)
