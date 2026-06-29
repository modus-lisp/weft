;;;; src/css/cursor.lisp — <cursor> keyword.
(in-package #:weft.css)
(define-value-parser "cursor" (s)
  (let ((k (ascii-downcase (css-trim s))))
    (if (member k '("auto" "pointer" "default" "text" "move" "wait" "help" "not-allowed") :test #'string=) k :invalid)))
