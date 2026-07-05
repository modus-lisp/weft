;;;; inspect/wpt-harness.lisp — batch-run WPT testharness.js tests through weft.
;;;;
;;;; testharness.js tests are the assert-based JS suite that covers the non-visual
;;;; browser: DOM, HTML parsing, URL, encoding, events, …  weft runs them through
;;;; shuttle.  Reads a manifest of TAB-separated  <html-path> <out-json> <wpt-root>
;;;; lines, loads weft ONCE, and for each test runs its scripts (testharness.js and
;;;; the test itself, resolved by a file-backed loader), drains the task queue, and
;;;; writes the per-subtest results (name + status) as JSON to <out-json>.
(require :asdf)
(push (truename (merge-pathnames "../" (directory-namestring *load-truename*)))
      asdf:*central-registry*)
(handler-bind ((warning #'muffle-warning))
  (asdf:load-system "weft/script"))

(defpackage #:weft.wpth (:use #:cl) (:local-nicknames (#:s #:weft.script) (#:js #:shuttle)))
(in-package #:weft.wpth)

(defun read-file-string (path)
  (with-open-file (in path :external-format :utf-8 :if-does-not-exist nil)
    (and in (let ((s (make-string (file-length in)))) (subseq s 0 (read-sequence s in))))))

(defun strip-query (url) (subseq url 0 (or (position #\? url) (position #\# url) (length url))))

(defparameter +report+
  ;; testharness.js hides its results object and delivers them via a completion
  ;; callback; testharnessreport.js is where a runner registers one.  Replace it
  ;; with ours (and turn off the DOM output path we don't need), so window.__wpt
  ;; holds [name, status] for every subtest once the harness completes.
  "setup({output:false});window.__wpt=null;add_completion_callback(function(ts,st){
     window.__wpt=ts.map(function(t){return [String(t.name),t.status,String(t.message||'')];});});")

(defun harness-loader (test-dir wpt-root)
  "Serve testharness.js / test scripts as :js, resolving /root-relative against
   WPT-ROOT else relative to TEST-DIR.  testharnessreport.js becomes +REPORT+."
  (lambda (ctx url) (declare (ignore ctx))
    (handler-case
        (let ((clean (strip-query url)))
          (if (search "testharnessreport" clean)
              (values :js +report+)
              (let* ((path (if (and (plusp (length clean)) (char= (char clean 0) #\/))
                               (merge-pathnames (subseq clean 1) wpt-root)
                               (merge-pathnames clean test-dir)))
                     (content (and (probe-file path) (read-file-string path))))
                (if content (values :js content) (values nil nil)))))
      (error () (values nil nil)))))

(defparameter +dump+ "window.__wpt ? JSON.stringify(window.__wpt) : '[]'")

(defun run-one (html-path out-json wpt-root)
  (handler-case
      (let* ((html (read-file-string html-path))
             (doc (weft.html:parse-html html))
             (base (format nil "file://~a" (namestring (truename html-path))))
             (test-dir (directory-namestring (truename html-path)))
             (ctx (s:make-context doc :base base :width 800
                                  :loader (harness-loader test-dir wpt-root)))
             (realm (s:context-realm ctx)))
        (s:run-inline-scripts ctx)
        (s:run-event-loop ctx :max-tasks 200000)
        (s:fire-lifecycle-events ctx)
        (s:run-event-loop ctx :max-tasks 200000)
        (let ((json (js:eval-script realm +dump+)))
          (with-open-file (o out-json :direction :output :if-exists :supersede :external-format :utf-8)
            (write-string (if (stringp json) json "[]") o))))
    (error (e)
      (with-open-file (o out-json :direction :output :if-exists :supersede :external-format :utf-8)
        (format o "{\"error\":~s}" (princ-to-string e))))))

(defun split-tabs (line)
  (loop for start = 0 then (1+ pos) for pos = (position #\Tab line :start start)
        collect (subseq line start (or pos (length line))) while pos))

(let ((manifest (second sb-ext:*posix-argv*)))
  (with-open-file (m manifest :external-format :utf-8)
    (loop for line = (read-line m nil) while line
          when (plusp (length line)) do
            (destructuring-bind (html out root) (split-tabs line)
              (run-one html out (truename root)))))
  (format t "done~%"))
