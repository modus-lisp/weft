;;;; inspect/wpt-oracle.lisp — a per-UNIT WPT oracle for the swarm (any subtree).
;;;;
;;;; Generalises forms-oracle.lisp: point it at any WPT directory and it runs every
;;;; testharness.js file under it through weft (via shuttle) and prints ONE tally
;;;; line a worker can loop against:
;;;;
;;;;     UNIT <subdir>: <P> passed, <F> failed   (of <T> subtests over <K> files)
;;;;
;;;; The engine loads from whatever ASDF resolves (the worker's isolated copy under
;;;; CL_SOURCE_REGISTRY); the READ-ONLY WPT data is at $WPT_ROOT (default ~/wpt).
;;;;
;;;;   WPT_SUBDIR=dom/ranges WPT_ROOT=~/wpt \
;;;;     sbcl --non-interactive --load inspect/wpt-oracle.lisp
(require :asdf)
(handler-bind ((warning #'muffle-warning))
  (asdf:load-system "weft/script"))

(defpackage #:weft.wpt-oracle
  (:use #:cl) (:local-nicknames (#:s #:weft.script) (#:js #:shuttle))
  (:export #:run))
(in-package #:weft.wpt-oracle)

(defparameter *wpt-root*
  (truename (or (uiop:getenv "WPT_ROOT")
                (merge-pathnames "wpt/" (user-homedir-pathname)))))

(defun read-file-string (path)
  (with-open-file (in path :external-format :utf-8 :if-does-not-exist nil)
    (and in (let ((s (make-string (file-length in)))) (subseq s 0 (read-sequence s in))))))

(defun strip-query (url) (subseq url 0 (or (position #\? url) (position #\# url) (length url))))

(defun testharness-file-p (path)
  "A runnable testharness.js file: includes the harness + report, and is not a
   .window.js/.worker.js generated variant (weft has no worker/window infra)."
  (let ((head (ignore-errors (with-open-file (in path :external-format :utf-8) (let ((s (make-string 2000))) (subseq s 0 (read-sequence s in)))))))
    (and head (search "testharness.js" head) (search "testharnessreport" head)
         (not (search ".window.js" (namestring path)))
         (not (search ".worker.js" (namestring path))))))

(defun find-tests (subdir)
  (let ((base (merge-pathnames (concatenate 'string subdir "/") *wpt-root*)))
    (remove-if-not #'testharness-file-p
                   (directory (merge-pathnames "**/*.html" base)))))

(defparameter +report+
  "setup({output:false});window.__wpt=null;add_completion_callback(function(ts,st){
     window.__wpt=ts.map(function(t){return [String(t.name),t.status,String(t.message||'')];});});")

(defun harness-loader (test-dir)
  (lambda (ctx url) (declare (ignore ctx))
    (handler-case
        (let ((clean (strip-query url)))
          (if (search "testharnessreport" clean)
              (values :js +report+)
              (let* ((path (if (and (plusp (length clean)) (char= (char clean 0) #\/))
                               (merge-pathnames (subseq clean 1) *wpt-root*)
                               (merge-pathnames clean test-dir)))
                     (content (and (probe-file path) (read-file-string path))))
                (if content (values :js content) (values nil nil)))))
      (error () (values nil nil)))))

(defparameter +dump+
  "(function(){var a=window.__wpt||[];var p=0;for(var i=0;i<a.length;i++){if(a[i][1]===0)p++;}return p+' '+a.length;})()")

(defun parse-two-ints (str)
  (when (stringp str)
    (let ((sp (position #\Space str)))
      (when sp (values (parse-integer str :end sp :junk-allowed t)
                       (parse-integer str :start (1+ sp) :junk-allowed t))))))

(defun run-one (html-path)
  (handler-case
      (let* ((html (read-file-string html-path))
             (doc (weft.html:parse-html html))
             (base (format nil "file://~a" (namestring (truename html-path))))
             (test-dir (directory-namestring (truename html-path)))
             (ctx (s:make-context doc :base base :width 800 :loader (harness-loader test-dir)))
             (realm (s:context-realm ctx)))
        (s:run-inline-scripts ctx)
        (s:run-event-loop ctx :max-tasks 200000)
        (s:fire-lifecycle-events ctx)
        (s:run-event-loop ctx :max-tasks 200000)
        (multiple-value-bind (p n) (parse-two-ints (js:eval-script realm +dump+))
          (if (and n (plusp n)) (values p n) (values 0 1))))     ; no subtests = 1 fail (not free)
    (error () (values 0 1))))

(defun run (&optional (subdir (uiop:getenv "WPT_SUBDIR")))
  (let ((files (find-tests subdir)) (tp 0) (tn 0) (k 0))
    (dolist (f files)
      (multiple-value-bind (p n) (run-one f) (incf tp p) (incf tn n) (incf k)))
    (format t "~&UNIT ~a: ~d passed, ~d failed   (of ~d subtests over ~d files)~%"
            subdir tp (- tn tp) tn k)
    (finish-output)))

;; run immediately when loaded as a script with WPT_SUBDIR set
(when (uiop:getenv "WPT_SUBDIR") (run))
