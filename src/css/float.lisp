;;;; src/css/float.lisp — CSS <float> value parser.
(in-package #:weft.css)
(define-value-parser "float" (s)
  (let ((lower (ascii-downcase (css-trim s))))
    (if (member lower '("none" "left" "right") :test #'string=)
        lower
        :invalid)))