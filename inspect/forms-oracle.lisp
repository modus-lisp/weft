;;;; inspect/forms-oracle.lisp — per-unit WPT oracle for the forms swarm.
;;;;
;;;; Runs a curated set of html/semantics/forms WPT testharness.js files through
;;;; weft (via shuttle) for one feature UNIT and prints a single tally line:
;;;;
;;;;     UNIT <unit>: <P> passed, <F> failed   (of <N> subtests over <K> files)
;;;;
;;;; A swarm worker loops this until "0 failed" (or minimises F).  The engine is
;;;; loaded from whatever ASDF resolves (the worker's isolated copy under
;;;; CL_SOURCE_REGISTRY); the READ-ONLY WPT test data lives at $WPT_ROOT (default
;;;; ~/wpt) — an absolute path outside the copy, never symlinked in.
;;;;
;;;;   sbcl --non-interactive --eval '(asdf:load-system "weft/script")' \
;;;;        --load inspect/forms-oracle.lisp --eval '(weft.forms-oracle:run "valueasnumber")'
(require :asdf)
(handler-bind ((warning #'muffle-warning))
  (asdf:load-system "weft/script"))

(defpackage #:weft.forms-oracle
  (:use #:cl) (:local-nicknames (#:s #:weft.script) (#:js #:shuttle))
  (:export #:run))
(in-package #:weft.forms-oracle)

(defparameter *wpt-root*
  (truename (or (uiop:getenv "WPT_ROOT")
                (merge-pathnames "wpt/" (user-homedir-pathname)))))

(defparameter *input-dir* "html/semantics/forms/the-input-element/")

;;; unit -> list of test-file basenames under *input-dir*.
(defparameter *units*
  '(("valueasnumber" . ("input-valueasnumber.html" "input-valueasnumber-stepping.html"
                        "input-valueasnumber-typeerror.html" "input-valueasnumber-invalidstateerr.html"))
    ("valueasdate"   . ("input-valueasdate.html" "input-valueasdate-typeerror.html"
                        "input-valueasdate-invalidstateerr.html" "input-valueasdate-stepping.html"
                        "datetime-local-valueasdate.html"))
    ("stepupdown"    . ("input-stepup.html" "input-stepdown.html" "input-stepdown-02.html"))
    ("selection"     . ("selection.html"))
    ("validity"      . ("input-validity.html" "input-checkvalidity.html"
                        "input-setcustomvalidity.html" "input-validationmessage.html"))
    ("labels"        . ("input-labels.html"))))

(defun read-file-string (path)
  (with-open-file (in path :external-format :utf-8 :if-does-not-exist nil)
    (and in (let ((s (make-string (file-length in)))) (subseq s 0 (read-sequence s in))))))

(defun strip-query (url) (subseq url 0 (or (position #\? url) (position #\# url) (length url))))

(defparameter +report+
  "setup({output:false});window.__wpt=null;add_completion_callback(function(ts,st){
     window.__wpt=ts.map(function(t){return [String(t.name),t.status,String(t.message||'')];});});")

(defun harness-loader (test-dir wpt-root)
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

;;; JS-side tally: "<passed> <total>" so no Lisp JSON parser is needed.
(defparameter +dump+
  "(function(){var a=window.__wpt||[];var p=0;for(var i=0;i<a.length;i++){if(a[i][1]===0)p++;}return p+' '+a.length;})()")

(defun parse-two-ints (s)
  (when (stringp s)
    (let ((sp (position #\Space s)))
      (when sp
        (let ((p (parse-integer s :end sp :junk-allowed t))
              (n (parse-integer s :start (1+ sp) :junk-allowed t)))
          (when (and p n) (values p n)))))))

;;; Run one file; return (values passed total error-string).  A file that errors
;;; before any subtest registers counts as 1 failed (so a hard crash isn't free).
(defun run-one (html-path)
  (handler-case
      (let* ((html (read-file-string html-path))
             (doc (weft.html:parse-html html))
             (base (format nil "file://~a" (namestring (truename html-path))))
             (test-dir (directory-namestring (truename html-path)))
             (ctx (s:make-context doc :base base :width 800
                                  :loader (harness-loader test-dir *wpt-root*)))
             (realm (s:context-realm ctx)))
        (s:run-inline-scripts ctx)
        (s:run-event-loop ctx :max-tasks 200000)
        (s:fire-lifecycle-events ctx)
        (s:run-event-loop ctx :max-tasks 200000)
        (let ((out (js:eval-script realm +dump+)))
          (multiple-value-bind (p n) (parse-two-ints out)
            (if (and n (plusp n))
                (values p n nil)
                (values 0 1 "no-subtests")))))
    (error (e) (values 0 1 (princ-to-string e)))))

(defun run (unit)
  (let ((files (cdr (assoc unit *units* :test #'string=))))
    (unless files (format t "~&unknown unit ~a~%" unit) (return-from run))
    (let ((tp 0) (tn 0) (k 0))
      (dolist (f files)
        (let ((path (merge-pathnames (concatenate 'string *input-dir* f) *wpt-root*)))
          (if (probe-file path)
              (multiple-value-bind (p n err) (run-one path)
                (incf tp p) (incf tn n) (incf k)
                (format t "~&  ~40a ~3d/~3d~@[  [~a]~]~%" f p n
                        (and err (subseq err 0 (min 50 (length err))))))
              (format t "~&  ~40a MISSING~%" f))))
      (format t "~&UNIT ~a: ~d passed, ~d failed   (of ~d subtests over ~d files)~%"
              unit tp (- tn tp) tn k)
      (finish-output))))
