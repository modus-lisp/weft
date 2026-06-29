;;;; src/css/url.lisp
(in-package #:weft.css)
(define-value-parser "url" (s) (declare (ignore s)) :invalid)
