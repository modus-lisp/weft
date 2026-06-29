;;;; src/css/percentage.lisp
(in-package #:weft.css)
(define-value-parser "percentage" (s) (declare (ignore s)) :invalid)
