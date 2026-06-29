;;;; src/css/visibility.lisp — <visibility> keyword.
(in-package #:weft.css)
(define-value-parser "visibility" (s)
  (let ((k (ascii-downcase (css-trim s))))
    (if (member k '("visible" "hidden" "collapse") :test #'string=) k :invalid)))
