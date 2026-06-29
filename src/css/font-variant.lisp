;;;; src/css/font-variant.lisp — <font-variant> keyword.
(in-package #:weft.css)
(define-value-parser "font-variant" (s)
  (let ((k (ascii-downcase (css-trim s))))
    (if (member k '("normal" "small-caps") :test #'string=) k :invalid)))
