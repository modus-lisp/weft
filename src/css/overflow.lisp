;;;; src/css/overflow.lisp
(in-package #:weft.css)
(define-value-parser "overflow" (s)
  (let* ((trimmed (css-trim s))
         (lowered (ascii-downcase trimmed)))
    (if (find lowered '("visible" "hidden" "scroll" "auto" "clip") :test #'string=)
        lowered
        :invalid)))