;;;; src/css/box-sizing.lisp — CSS box-sizing value parser
(in-package #:weft.css)

(define-value-parser "box-sizing" (s)
  (let ((lower (ascii-downcase (css-trim s))))
    (if (or (string= lower "content-box") (string= lower "border-box"))
        lower
        :invalid)))