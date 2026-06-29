;;;; src/css/background-repeat.lisp
(in-package #:weft.css)

(define-value-parser "background-repeat" (s)
  (let ((trimmed (css-trim (ascii-downcase s))))
    (if (member trimmed '("repeat" "repeat-x" "repeat-y" "no-repeat" "space" "round") :test #'string=)
        trimmed
        :invalid)))