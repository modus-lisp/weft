;;;; inspect/acid3.lisp — run Acid3 in weft+shuttle and read its self-reported score.
;;;;
;;;; Acid3 (inspect/vectors/pages/acid3/) defines 100 subtests and a runner,
;;;; update(), that executes them one per setTimeout and increments a global
;;;; `score` (mirrored into the #score element).  This harness parses the page,
;;;; runs its scripts against the bridge, kicks off update(), drains the
;;;; macrotask + microtask queues, then reports the score and the failure log.
;;;;   sbcl --script inspect/acid3.lisp          # score + first failures
;;;;   sbcl --script inspect/acid3.lisp -v       # full failure log
(require :asdf)
(push (truename (merge-pathnames "../" (directory-namestring *load-truename*)))
      asdf:*central-registry*)
(asdf:load-system "weft/script")

(defpackage #:weft.acid3
  (:use #:cl) (:local-nicknames (#:s #:weft.script) (#:js #:shuttle) (#:h #:weft.html))
  (:export #:run))
(in-package #:weft.acid3)

(defun slurp (p) (with-open-file (in p :external-format :latin-1)
                   (let ((s (make-string (file-length in)))) (subseq s 0 (read-sequence s in)))))

(defun page-path ()
  (asdf:system-relative-pathname "weft" "inspect/vectors/pages/acid3/index.html"))

(defun jval (realm src)
  (handler-case (js:to-string (js:eval-script realm src))
    (error (e) (format nil "<err ~a>" e))))

(defun local-loader (base-dir)
  "A subresource loader that reads files relative to BASE-DIR (the vendored Acid3
   directory). Missing files return NIL so the frame still gets an empty document."
  (lambda (ctx url) (declare (ignore ctx))
    (let* ((clean (subseq url 0 (or (position #\? url) (position #\# url) (length url))))
           (path (ignore-errors (merge-pathnames clean base-dir))))
      (if (and path (ignore-errors (probe-file path)))
          (values :html (slurp path))
          (values nil nil)))))

(defun run (&key verbose)
  (let* ((html (slurp (page-path)))
         (dir (directory-namestring (page-path)))
         (doc (h:parse-html html))
         ;; The page's own URL is the base for resolving relative references;
         ;; the loader maps those references back onto the vendored files.
         (ctx (s:make-context doc :base "http://acid3.acidtests.org/" :loader (local-loader dir)))
         (realm (s:context-realm ctx)))
    ;; run all inline + data: URL <script> (defines tests[], update, startTime, …)
    (s:run-inline-scripts ctx)
    ;; body onload = "update()": kick off the runner, then drive the timer chain.
    (handler-case (js:eval-script realm "update()")
      (error (e) (format t "~&update() threw: ~a~%" e)))
    (s:run-event-loop ctx :max-tasks 2000000)
    (let* ((score (jval realm "String(score)"))
           (total (jval realm "String(tests.length)"))
           (index (jval realm "String(index)"))
           (shown (jval realm "String(document.getElementById('score').firstChild.data)"))
           (log (jval realm "String(log)")))
      (format t "~&================ Acid3 (weft + shuttle) ================~%")
      (format t "score: ~a / ~a   (runner reached test ~a; #score shows ~a)~%"
              score total index shown)
      (let ((lines (remove "" (uiop:split-string log :separator (string #\Newline)) :test #'string=)))
        (format t "failures logged: ~a~%" (length lines))
        (let ((show (if verbose lines (subseq lines 0 (min 25 (length lines))))))
          (dolist (l show) (format t "  ~a~%" l)))
        (when (and (not verbose) (> (length lines) 25))
          (format t "  … (~a more; pass -v for all)~%" (- (length lines) 25))))
      (ignore-errors (parse-integer score)))))

(defparameter +acid3-floor+ 100
  "The score this gate refuses to drop below.

   A CAVEAT THAT MATTERS: this is Acid3's SELF-REPORTED score, and 100 here is not
   the same claim as \"weft passes Acid3\".  Several subtests check behaviours the
   web has since retired, which a current browser also does not implement; the real
   conformance claim is the Chrome-REFERENCED render, not this counter.  As a
   REGRESSION floor it is still exactly right: whatever the number means, it must
   not fall.")

;; A GATE MUST BE ABLE TO FAIL.  This harness printed its score and returned it --
;; and for a long time that score was the string \"[object Object]\", because a
;; global `score` collided with the #score element and the engine lost the variable
;; (fixed in shuttle: a declaration must ask about storage, not resolution).  The
;; harness reported the nonsense happily and asserted only that the runner \"reached
;; test 100\", which is a far weaker claim than it looks.  A wrong value that does
;; not fail is worse than a crash: it reads as a working gate forever.
(let ((score (run :verbose (member "-v" (uiop:command-line-arguments) :test #'string=))))
  (cond
    ((not (integerp score))
     (format t "~&ACID3 GATE FAILED: score is not a number (~s) -- the page's own~%~
                  counter did not survive being read.~%" score)
     (uiop:quit 1))
    ((< score +acid3-floor+)
     (format t "~&ACID3 GATE FAILED: score ~d, floor ~d~%" score +acid3-floor+)
     (uiop:quit 1))
    (t (format t "~&Acid3 gate: PASS (score ~d >= floor ~d)~%" score +acid3-floor+))))
