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

(defun resolve-local-path (url test-dir wpt-root)
  "Resolve a subresource URL to a local file path: strip a file:// scheme, then a
root-relative /… resolves against WPT-ROOT, else relative to TEST-DIR."
  (let* ((clean (strip-query url))
         (clean (if (and (>= (length clean) 7) (string-equal (subseq clean 0 7) "file://"))
                    (subseq clean 7) clean)))
    (cond ((and (plusp (length clean)) (char= (char clean 0) #\/) (probe-file clean)) clean)
          ((and (plusp (length clean)) (char= (char clean 0) #\/)) (merge-pathnames (subseq clean 1) wpt-root))
          (t (merge-pathnames clean test-dir)))))

(defun file-loader (test-dir wpt-root)
  "A subresource loader for CSS/text: resolve URL to a local file and serve it."
  (lambda (ctx url) (declare (ignore ctx))
    (handler-case
        (let* ((path (resolve-local-path url test-dir wpt-root))
               (content (and (probe-file path) (read-file-string path))))
          (if content
              (values (if (search ".css" url :test #'char-equal) :css :text) content)
              (values nil nil)))
      (error () (values nil nil)))))

(defun read-file-bytes (path)
  (with-open-file (in path :element-type '(unsigned-byte 8) :if-does-not-exist nil)
    (and in (let ((v (make-array (file-length in) :element-type '(unsigned-byte 8)))) (read-sequence v in) v))))

(defun mime-for (path)
  (let ((p (string-downcase (namestring path))))
    (cond ((search ".png" p) "image/png") ((or (search ".jpg" p) (search ".jpeg" p)) "image/jpeg")
          ((search ".gif" p) "image/gif") ((search ".svg" p) "image/svg+xml")
          ((search ".webp" p) "image/webp") ((search ".bmp" p) "image/bmp")
          (t "application/octet-stream"))))

(defun file-image-loader (test-dir wpt-root)
  "An (url) -> (values bytes mime) loader for <img>, reading the image file's raw
bytes from disk so weft decodes it (or at least learns its intrinsic size) — the
same content the reference sees, so image reftests compare like-for-like."
  (lambda (url)
    (handler-case
        (let* ((path (resolve-local-path url test-dir wpt-root))
               (bytes (and (probe-file path) (read-file-bytes path))))
          (if bytes (values bytes (mime-for path)) (values nil nil)))
      (error () (values nil nil)))))

(defun render-one (html-path out-png wpt-root)
  (let* ((html (read-file-string html-path))
         (test-dir (directory-namestring (truename html-path)))
         (base (format nil "file://~a" (namestring (truename html-path)))))
    (handler-case
        ;; bind the <img> loader for the whole render so images load from disk.
        (let ((r:*image-loader* (file-image-loader test-dir wpt-root)))
          (s:render-scripted-to-png html "" 800 out-png
                                    :min-height 600 :max-height 2000
                                    :base base :loader (file-loader test-dir wpt-root)))
      (error (e) (format *error-output* "~&render error ~a: ~a~%" html-path e) nil))))

(defun split-tabs (line)
  (loop for start = 0 then (1+ pos)
        for pos = (position #\Tab line :start start)
        collect (subseq line start (or pos (length line)))
        while pos))

(defun register-ahem (wpt-root)
  "Register the WPT Ahem test font so `font-family:Ahem` resolves with its exact
   1em-square metrics — most CSS reftests use it and mismatch the fallback otherwise."
  (let ((path (merge-pathnames "fonts/Ahem.ttf" wpt-root)))
    (when (probe-file path)
      (with-open-file (in path :element-type '(unsigned-byte 8))
        (let ((b (make-array (file-length in) :element-type '(unsigned-byte 8))))
          (read-sequence b in)
          (r:register-font "Ahem" b))))))

(let ((manifest (second sb-ext:*posix-argv*)) (ahem-done nil))
  (with-open-file (m manifest :external-format :utf-8)
    (loop for line = (read-line m nil) while line
          when (plusp (length line)) do
            (destructuring-bind (html out root) (split-tabs line)
              (let ((root (truename root)))
                (unless ahem-done (register-ahem root) (setf ahem-done t))
                (render-one html out root)))))
  (format t "done~%"))
