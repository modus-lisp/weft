;;;; inspect/acid-test.lisp — Acid2/Acid3 as permanent regression + progress tests.
;;;;
;;;; weft renders Acid2 at 99.9% pixel-match vs a real browser (the face fully
;;;; assembles); the conformance number is measured by inspect/acid2-reftest.py +
;;;; inspect/acid2-layout-diff.py (vs Chromium ground truth), not here. Acid3 is
;;;; ~99% JavaScript and CANNOT run until the JS engine (P4) lands.
;;;; This gate itself is a ROBUSTNESS guard + progress signal:
;;;;   (1) the build fails if rendering the vendored test pages raises an error;
;;;;   (2) it reports each render's ink coverage + dimensions for visibility.
(defpackage #:weft.acid.test
  (:use #:cl) (:local-nicknames (#:r #:weft.render)) (:export #:run))
(in-package #:weft.acid.test)

(defun slurp (path)
  (with-open-file (s path :external-format :utf-8)
    (let ((str (make-string (file-length s)))) (subseq str 0 (read-sequence str s)))))

(defun render-one (name file width &optional scroll-to)
  "Render a vendored test page; return (values ok ink width height) and write a PNG."
  (handler-case
      (let* ((html (slurp (asdf:system-relative-pathname "weft" (format nil "inspect/vectors/acid/~a" file))))
             (cv (r:render-to-canvas html nil width :min-height 200 :scroll-to scroll-to))
             (out (asdf:system-relative-pathname "weft" (format nil "inspect/vectors/acid/~a-weft.png" name))))
        (r:write-png cv out)
        (values t (r:canvas-ink cv) (r:canvas-width cv) (r:canvas-height cv)))
    (error (e) (values nil (princ-to-string e) 0 0))))

(defun run ()
  (let ((fails 0))
    (format t "~&=== weft Acid gate (robustness + progress, NOT a pass/fail claim) ===~%")
    ;; Acid2: its intro links to #top; a real browser navigates there, scrolling
    ;; the picture to the top of the overflow:hidden viewport.  We do the same.
    (dolist (test '(("acid2" "acid2.html" 700 "top")
                    ("acid3" "acid3.html" 800 nil)))
      (destructuring-bind (name file width scroll-to) test
        (multiple-value-bind (ok ink w h) (render-one name file width scroll-to)
          (if ok
              (format t "  ok   ~a renders: ~dx~d, ink ~,1f%% (~a)~%" name w h (* 100 ink)
                      "progress signal; not a conformance score")
              (progn (incf fails) (format t "  FAIL ~a render ERROR: ~a~%" name ink))))))
    (format t "~%~:[render error(s) — see above~;both render without error~]~%" (zerop fails))
    (format t "Reminder: Acid2 needs :before/:after + data-URI images; Acid3 needs JS (P4).~%")
    (values (if (zerop fails) 1 0) fails)))
