;;;; inspect/acid-test.lisp — Acid2/Acid3 as permanent regression + progress tests.
;;;;
;;;; weft does NOT pass Acid2 (it needs generated content, data-URI image
;;;; decoding, and pixel-exact positioning) and CANNOT run Acid3 (no JS engine).
;;;; So this is NOT a pass/fail conformance claim — it is:
;;;;   (1) a robustness guard: rendering the vendored test pages must not error;
;;;;   (2) a progress tracker: it reports each render's ink coverage (fraction of
;;;;       painted pixels) and dimensions, so improvements/regressions are visible
;;;;       over time without pretending we pass.
;;;; The gate only FAILS the build if a render raises an error.
(defpackage #:weft.acid.test
  (:use #:cl) (:local-nicknames (#:r #:weft.render)) (:export #:run))
(in-package #:weft.acid.test)

(defun slurp (path)
  (with-open-file (s path :external-format :utf-8)
    (let ((str (make-string (file-length s)))) (subseq str 0 (read-sequence str s)))))

(defun render-one (name file width)
  "Render a vendored test page; return (values ok ink width height) and write a PNG."
  (handler-case
      (let* ((html (slurp (asdf:system-relative-pathname "weft" (format nil "inspect/vectors/acid/~a" file))))
             (cv (r:render-to-canvas html nil width :min-height 200))
             (out (asdf:system-relative-pathname "weft" (format nil "inspect/vectors/acid/~a-weft.png" name))))
        (r:write-png cv out)
        (values t (r:canvas-ink cv) (r:canvas-width cv) (r:canvas-height cv)))
    (error (e) (values nil (princ-to-string e) 0 0))))

(defun run ()
  (let ((fails 0))
    (format t "~&=== weft Acid gate (robustness + progress, NOT a pass/fail claim) ===~%")
    (dolist (test '(("acid2" "acid2.html" 700)
                    ("acid3" "acid3.html" 800)))
      (destructuring-bind (name file width) test
        (multiple-value-bind (ok ink w h) (render-one name file width)
          (if ok
              (format t "  ok   ~a renders: ~dx~d, ink ~,1f%% (~a)~%" name w h (* 100 ink)
                      "progress signal; not a conformance score")
              (progn (incf fails) (format t "  FAIL ~a render ERROR: ~a~%" name ink))))))
    (format t "~%~:[render error(s) — see above~;both render without error~]~%" (zerop fails))
    (format t "Reminder: Acid2 needs :before/:after + data-URI images; Acid3 needs JS (P4).~%")
    (values (if (zerop fails) 1 0) fails)))
