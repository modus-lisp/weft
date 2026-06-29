;;;; src/css/white-space.lisp — <white-space> keyword.
(in-package #:weft.css)
(define-value-parser "white-space" (s)
  (let ((k (ascii-downcase (css-trim s))))
    (if (member k '("normal" "nowrap" "pre" "pre-wrap" "pre-line") :test #'string=) k :invalid)))
