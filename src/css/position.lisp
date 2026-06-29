;;;; src/css/position.lisp
(in-package #:weft.css)
(define-value-parser "position" (s)
  (let ((trimmed (css-trim s))
        (keywords '("static" "relative" "absolute" "fixed" "sticky")))
    (if (member (ascii-downcase trimmed) keywords :test #'string=)
        (ascii-downcase trimmed)
        :invalid)))