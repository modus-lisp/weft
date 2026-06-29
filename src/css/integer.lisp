;;;; src/css/integer.lisp
(in-package #:weft.css)
(define-value-parser "integer" (s) (declare (ignore s)) :invalid)
