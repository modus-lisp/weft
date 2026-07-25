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
;;; A broken edit (unbalanced parens, a bad form) makes LOAD-SYSTEM signal, and
;;; under --disable-debugger that kills the process with a backtrace and no
;;; tally at all — the worst possible feedback, since it looks nothing like a
;;; score.  Catch it and report it as what it is: everything failing, with the
;;; compiler's complaint on the UNIT line.
;;; (Set before IN-PACKAGE, so it must be named in a package that exists now.)
(defvar cl-user::*oracle-load-error* nil)
(handler-case
    (handler-bind ((warning #'muffle-warning))
      (asdf:load-system "weft/script"))
  (error (e) (setf cl-user::*oracle-load-error* (princ-to-string e))))

(defpackage #:weft.forms-oracle
  (:use #:cl) (:local-nicknames (#:s #:weft.script) (#:js #:shuttle))
  (:export #:run))
(in-package #:weft.forms-oracle)

(defparameter *wpt-root*
  (truename (or (uiop:getenv "WPT_ROOT")
                (merge-pathnames "wpt/" (user-homedir-pathname)))))

(defparameter *input-dir* "html/semantics/forms/the-input-element/")

(defparameter *select-dir* "html/semantics/forms/the-select-element/")

(defparameter *textarea-dir* "html/semantics/forms/the-textarea-element/")

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
    ("labels"        . ("input-labels.html"))
    ("select"        . ("select-value.html" "select-multiple.html" "select-add.html"
                        "select-remove.html" "select-selectedOptions.html"
                        "select-selectedOptions-nesting.window.js"
                        "selected-index.html" "select-named-getter.html"
                        "common-HTMLOptionsCollection.html"
                        "common-HTMLOptionsCollection-add.html"
                        "common-HTMLOptionsCollection-namedItem.html"
                        "select-validity.html" "select-add-optgroup.html"
                        "select-toggle-multiple.html"
                        "select-setting-value-from-js-updates-visible-state.html"
                        "select-restore-invalid-option.html"))
    ("textarea"      . ("value-defaultValue-textContent.html" "textarea-textLength.html"
                        "textarea-type.html" "textarea-maxlength.html" "textarea-minlength.html"
                        "wrap-reflect-1a.html" "wrap-reflect-1b.html"
                        "wrap-enumerated-ascii-case-insensitive.html"
                        "textarea-setcustomvalidity.html" "change-to-empty-value.html"))))

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

;;; ---- pinned denominators --------------------------------------------------
;;; A file that throws PART WAY through registers only the subtests it reached,
;;; so its total shrinks.  Scored naively that makes "failed" go DOWN when you
;;; break a file — the cheapest way to improve the number is to crash the test,
;;; and an agent optimising the printed tally will find that gradient.  So the
;;; denominator is pinned: the most subtests this file has EVER registered here,
;;; persisted next to the oracle.  Subtests that go missing are counted failed.
;;; Monotonic, so a file that legitimately gets FURTHER (registering more) just
;;; raises its own bar.

(defparameter *expected-file*
  (merge-pathnames "forms-oracle-expected.sexp"
                   (or *load-truename* *default-pathname-defaults*)))

(defun load-expected ()
  (handler-case
      (with-open-file (s *expected-file* :if-does-not-exist nil)
        (and s (let ((*read-eval* nil)) (read s nil nil))))
    (error () nil)))

(defun save-expected (alist)
  (handler-case
      (with-open-file (s *expected-file* :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create)
        (format s ";;; auto-maintained by forms-oracle.lisp — pinned subtest~
                 ~%;;; denominators (file . most-subtests-ever-seen).~%~s~%" alist))
    (error () nil)))

;;; ---- keep-best ratchet ----------------------------------------------------
;;; A worker that runs out of budget mid-repair ends on a file that may not even
;;; compile, so the whole run scores zero however good its best moment was.  If
;;; ORACLE_KEEP names the file the unit owns, every improvement is snapshotted
;;; beside it; the harness can restore that snapshot when the run ends worse.

(defun keep-path () (uiop:getenv "ORACLE_KEEP"))

(defun best-score-path (unit)
  (format nil "~a.best-~a.score" (keep-path) unit))

;;; ---- sentinel units -------------------------------------------------------
;;; Every unit installs its behaviour onto ONE shared element prototype (see
;;; register-element-proto-extension), so a unit's file can silently clobber a
;;; sibling's method — and a per-unit oracle is blind to exactly that.  Measured:
;;; a select worker scored 6 -> 36 on its own unit while taking selection 42 -> 2
;;; and textarea 22 -> 11.  A big local win, badly net negative, and nothing in
;;; its feedback loop could see it.  So every run also scores the OTHER units and
;;; compares against a pinned best-ever, and the number that ratchets is the SUM.

(defparameter *bests-file*
  (merge-pathnames "forms-oracle-bests.sexp"
                   (or *load-truename* *default-pathname-defaults*)))

(defun load-bests ()
  (handler-case
      (with-open-file (s *bests-file* :if-does-not-exist nil)
        (and s (let ((*read-eval* nil)) (read s nil nil))))
    (error () nil)))

(defun save-bests (alist)
  (handler-case
      (with-open-file (s *bests-file* :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
        (format s ";;; auto-maintained by forms-oracle.lisp — best-ever passing~
                 ~%;;; subtests per unit (unit . passed).~%~s~%"
                (sort (copy-list alist) #'string< :key #'car)))
    (error () nil)))

(defun sentinel-units (unit)
  "The other units to score alongside UNIT. ORACLE_SENTINELS overrides: a comma
   list of unit names, or \"none\" to skip (faster, but blind to collateral)."
  (let ((env (uiop:getenv "ORACLE_SENTINELS")))
    (cond ((null env) (remove unit (mapcar #'car *units*) :test #'string=))
          ((string-equal env "none") nil)
          (t (remove unit (uiop:split-string env :separator ",")
                     :test #'string=)))))

(defun read-best (unit)
  (handler-case
      (with-open-file (s (best-score-path unit) :if-does-not-exist nil)
        (and s (read s nil nil)))
    (error () nil)))

(defun maybe-keep-best (unit passed)
  "Snapshot the owned file whenever PASSED beats the best seen. Returns a note.
   PASSED is the TOTAL across all scored units, not the target unit's own tally —
   a file that wins locally by breaking a sibling must not be what we keep."
  (let ((keep (keep-path)))
    (when (and keep (probe-file keep))
      (let ((best (read-best unit)))
        (when (or (null best) (> passed best))
          (handler-case
              (progn
                (uiop:copy-file keep (format nil "~a.best-~a" keep unit))
                (with-open-file (s (best-score-path unit) :direction :output
                                                          :if-exists :supersede
                                                          :if-does-not-exist :create)
                  (prin1 passed s))
                (format nil "new best (~d passed) snapshotted" passed))
            (error () nil)))))))

(defun unit-dir (unit)
  (cond ((string= unit "select") *select-dir*)
        ((string= unit "textarea") *textarea-dir*)
        (t *input-dir*)))

(defun score-unit (unit expected &key verbose)
  "Run UNIT's files against the loaded engine.
   Returns (values passed pinned-total file-count missing-subtests expected)."
  (let ((tp 0) (tn 0) (k 0) (short 0) (dir (unit-dir unit)))
    (dolist (f (cdr (assoc unit *units* :test #'string=)))
      (let ((path (merge-pathnames (concatenate 'string dir f) *wpt-root*)))
        (if (probe-file path)
            (multiple-value-bind (p n err) (run-one path)
              (let* ((pin (max n (or (cdr (assoc f expected :test #'string=)) 0)))
                     (missing (- pin n)))
                (setf expected (cons (cons f pin)
                                     (remove f expected :key #'car :test #'string=)))
                (incf tp p) (incf tn pin) (incf k) (incf short missing)
                (when verbose
                  (format t "~&  ~40a ~3d/~3d~@[  [~a]~]~@[  ~a~]~%" f p pin
                          (and err (subseq err 0 (min 50 (length err))))
                          (when (plusp missing)
                            (format nil "<<< ~d subtest~:p never ran — the file ~
                                         aborted; that is ~d failures, not ~d fewer"
                                    missing missing missing))))))
            (when verbose (format t "~&  ~40a MISSING~%" f)))))
    (values tp tn k short expected)))

(defun run (unit)
  (let ((files (cdr (assoc unit *units* :test #'string=))))
    (unless files (format t "~&unknown unit ~a~%" unit) (return-from run))
    (when cl-user::*oracle-load-error*
      (let ((pin (reduce #'+ (load-expected) :key #'cdr :initial-value 0)))
        (format t "~&UNIT ~a: 0 passed, ~d failed   (THE ENGINE DOES NOT COMPILE)~%~
                   ~&TOTAL 0 passed, best-ever ~d, REGRESSION -~d~%~
                   ~&Nothing ran. Fix the source error first — everything else ~
                   is noise until it compiles:~%~a~%"
                unit (max pin 1)
                (reduce #'+ (load-bests) :key #'cdr :initial-value 0)
                (reduce #'+ (load-bests) :key #'cdr :initial-value 0)
                cl-user::*oracle-load-error*))
      (finish-output)
      (return-from run))
    (let* ((expected (load-expected))
           (bests (load-bests))
           tp tn k short)
      (multiple-value-setq (tp tn k short expected)
        (score-unit unit expected :verbose t))
      (format t "~&UNIT ~a: ~d passed, ~d failed   (of ~d subtests over ~d files)~%"
              unit tp (- tn tp) tn k)
      (when (plusp short)
        (format t "~&NOTE: ~d subtest~:p did not run at all because a file threw ~
                   part way through. They are scored as failures. Fix the ~
                   exception — do not let a file abort.~%" short))
      ;; Sentinels: the other units, which share the element prototype with this
      ;; one.  Their scores are not yours to spend.
      (let ((sentinels (sentinel-units unit))
            (total tp) (best-total 0) (lost 0))
        (dolist (u sentinels)
          (multiple-value-bind (p n kk sh e) (score-unit u expected)
            (declare (ignore n kk sh))
            (setf expected e)
            (let* ((best (or (cdr (assoc u bests :test #'string=)) p))
                   (delta (- p best)))
              (incf total p)
              (when (minusp delta) (incf lost (- delta)))
              (format t "~&SENTINEL ~12a ~3d passed (best ~3d)~@[   ~a~]~%" u p best
                      (when (minusp delta)
                        (format nil "<<< REGRESSION ~d — you broke this unit" delta)))
              (setf bests (cons (cons u (max p best))
                                (remove u bests :key #'car :test #'string=))))))
        ;; The target unit ratchets too, but only its best-ever is pinned; the
        ;; number that decides "better" is the SUM across every unit.
        (let ((ubest (or (cdr (assoc unit bests :test #'string=)) tp)))
          (setf bests (cons (cons unit (max tp ubest))
                            (remove unit bests :key #'car :test #'string=))))
        (setf best-total (reduce #'+ bests :key #'cdr :initial-value 0))
        (save-expected expected)
        (save-bests bests)
        (format t "~&TOTAL ~d passed across ~d units, best-ever ~d~@[   ~a~]~%"
                total (1+ (length sentinels)) best-total
                (when (plusp lost)
                  (format nil "<<< ~d subtest~:p LOST in other units" lost)))
        (when (plusp lost)
          (format t "~&Every unit installs onto the SAME shared element prototype. ~
                     A method you register unconditionally — or one that returns ~
                     undefined for elements that are not yours — replaces the ~
                     sibling unit's version. Guard by element/tag, or delegate to ~
                     the function that was already installed. TOTAL is the score ~
                     that counts; a gain on ~a paid for out of another unit is ~
                     not a gain.~%" unit))
        (let ((kept (maybe-keep-best unit total)))
          (when kept (format t "~&  [~a]~%" kept))))
      (finish-output))))
