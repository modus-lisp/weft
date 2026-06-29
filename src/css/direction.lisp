;;;; src/css/direction.lisp — <direction> keyword.
(in-package #:weft.css)
(define-value-parser "direction" (s)
  (let ((k (ascii-downcase (css-trim s))))
    (if (member k '("ltr" "rtl") :test #'string=) k :invalid)))
