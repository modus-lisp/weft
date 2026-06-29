;;;; src/css/vertical-align.lisp
(in-package #:weft.css)
(define-value-parser "vertical-align" (s)
  (let ((k (ascii-downcase (css-trim s))))
    (cond ((member k '("baseline" "top" "middle" "bottom" "sub" "super") :test #'string=) (list k))
          ((%dim-pxem% k))
          (t :invalid))))
