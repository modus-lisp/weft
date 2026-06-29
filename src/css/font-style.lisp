;;;; src/css/font-style.lisp — <font-style> keyword.
(in-package #:weft.css)
(define-value-parser "font-style" (s)
  (let ((k (ascii-downcase (css-trim s))))
    (if (member k '("normal" "italic" "oblique") :test #'string=) k :invalid)))
