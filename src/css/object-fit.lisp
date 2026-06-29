;;;; src/css/object-fit.lisp
(in-package #:weft.css)

(define-value-parser "object-fit" (s)
  (let ((trimmed (css-trim s)))
    (if (string= trimmed "")
        :invalid
        (let ((lowered (ascii-downcase trimmed)))
          (if (member lowered '("fill" "contain" "cover" "none" "scale-down")
                      :test #'string=)
              lowered
              :invalid)))))