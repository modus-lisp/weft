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
      (t :text))))

(defun https-css-loader ()
  "A subresource loader (ctx url) -> (values kind content) backed by weft.fetch,
   so external stylesheets (http:// and https://) are pulled over seal TLS.
   Serves ONLY :css — a deterministic styling render, no external JS/frames —
   and degrades to (values nil nil) on any fetch/TLS failure."
  (lambda (ctx url) (declare (ignore ctx))
    (handler-case
        (multiple-value-bind (text cs resp) (f:fetch-text url)
          (declare (ignore cs))
          (if (and text (<= 200 (f:response-status resp) 299)
                   (eq (content-kind (f:response-headers resp) url) :css))
              (values :css text)
              (values nil nil)))
      (error () (values nil nil)))))

(defun render (url out &optional (width 1024))
  "Fetch URL over HTTPS and render it (scripted, external CSS over TLS) to OUT."
  (multiple-value-bind (html cs resp) (f:fetch-text url)
    (declare (ignore cs))
    (let ((base (f:response-url resp)))         ; the final URL is the document base
      (format t "~&fetched ~a  (HTTP ~d, base ~a, ~:d chars)~%"
              url (f:response-status resp) base (length html))
      (multiple-value-bind (path ctx)
          (s:render-scripted-to-png html "" width out
                                    :min-height 400 :max-height 8000
                                    :base base :loader (https-css-loader))
        (declare (ignore ctx))
        (multiple-value-bind (cv) (s:render-scripted-to-canvas html "" width
                                                               :base base :loader (https-css-loader))
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
        ;; sheets).  (A full Wikipedia article fetches fine over seal but its very
        ;; deeply nested infobox/table DOM overruns weft's preferred-width layout
        ;; recursion — a pre-existing layout depth limit, not a fetch/TLS one.)
        (let ((dir (asdf:system-relative-pathname "weft" "inspect/")))
          (dolist (job '(("https://news.ycombinator.com/" "live-hn.png" 1024)
                         ("https://www.iana.org/" "live-iana.png" 1000)
                         ("https://www.w3.org/" "live-w3c.png" 1000)))
            (destructuring-bind (url name width) job
              (handler-case
                  (render url (namestring (merge-pathnames name dir)) width)
                (error (e) (format t "~&~a: FAILED cleanly -> ~a~%" url e)))))))))

(main)
