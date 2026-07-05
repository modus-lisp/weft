;;;; inspect/wpt-render.lisp — batch-render WPT reftest pages to PNG for the reftest
;;;; runner (inspect/wpt-run.py).  Reads a job manifest of TAB-separated
;;;;   <html-path> <out-png> <wpt-root>
;;;; lines on the command line (a file), loads weft ONCE, and renders each page with
;;;; a file-backed subresource loader (relative and /root-relative CSS/support files
;;;; resolved against the test dir / WPT root).  One process for the whole batch.
(require :asdf)
(push (truename (merge-pathnames "../" (directory-namestring *load-truename*)))
      asdf:*central-registry*)
(handler-bind ((warning #'muffle-warning))
  (asdf:load-system "weft/script"))

(defpackage #:weft.wpt (:use #:cl) (:local-nicknames (#:s #:weft.script) (#:r #:weft.render)))
(in-package #:weft.wpt)

(defun read-file-string (path)
  (with-open-file (in path :external-format :utf-8 :if-does-not-exist nil)
    (and in (let ((s (make-string (file-length in)))) (subseq s 0 (read-sequence s in))))))

(defun strip-query (url)
  (subseq url 0 (or (position #\? url) (position #\# url) (length url))))

(defun file-loader (test-dir wpt-root)
  "A subresource loader: resolve URL to a local file (root-relative /… against
   WPT-ROOT, else relative to TEST-DIR) and serve it as :css / :text."
  (lambda (ctx url) (declare (ignore ctx))
    (handler-case
        (let* ((clean (strip-query url))
               (path (if (and (plusp (length clean)) (char= (char clean 0) #\/))
                         (merge-pathnames (subseq clean 1) wpt-root)
                         (merge-pathnames clean test-dir)))
               (content (and (probe-file path) (read-file-string path))))
          (if content
              (values (if (search ".css" clean :test #'char-equal) :css :text) content)
              (values nil nil)))
      (error () (values nil nil)))))

(defun render-one (html-path out-png wpt-root)
  (let* ((html (read-file-string html-path))
         (test-dir (directory-namestring (truename html-path)))
         (base (format nil "file://~a" (namestring (truename html-path)))))
    (handler-case
        (s:render-scripted-to-png html "" 800 out-png
                                  :min-height 600 :max-height 2000
                                  :base base :loader (file-loader test-dir wpt-root))
      (error (e) (format *error-output* "~&render error ~a: ~a~%" html-path e) nil))))

(defun split-tabs (line)
  (loop for start = 0 then (1+ pos)
        for pos = (position #\Tab line :start start)
        collect (subseq line start (or pos (length line)))
        while pos))

(let ((manifest (second sb-ext:*posix-argv*)))
  (with-open-file (m manifest :external-format :utf-8)
    (loop for line = (read-line m nil) while line
          when (plusp (length line)) do
            (destructuring-bind (html out root) (split-tabs line)
              (render-one html out (truename root)))))
  (format t "done~%"))
