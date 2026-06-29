;;;; src/css/z-index.lisp
(in-package #:weft.css)
(define-value-parser "z-index" (s)
  (let ((k (ascii-downcase (css-trim s))))
    (if (string= k "auto") (list "auto")
        (let ((v (ignore-errors (parse-integer k)))) (if v (list v) :invalid)))))
