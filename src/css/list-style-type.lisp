;;;; src/css/list-style-type.lisp
(in-package #:weft.css)
(define-value-parser "list-style-type" (s)
  (let ((trimmed (css-trim s)))
    (if (string= trimmed "")
        :invalid
        (let ((lowered (ascii-downcase trimmed)))
          (if (find lowered '("disc" "circle" "square" "decimal"
                              "lower-alpha" "upper-alpha"
                              "lower-roman" "upper-roman" "none")
                    :test #'string=)
              lowered
              :invalid)))))