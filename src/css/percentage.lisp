;;;; src/css/percentage.lisp — <percentage> value parser.
(in-package #:weft.css)
(define-value-parser "percentage" (s)
  (let ((tt (css-trim s)))
    (if (and (plusp (length tt)) (char= (char tt (1- (length tt))) #\%))
        (let ((v (parse-value "number" (subseq tt 0 (1- (length tt))))))
          (if (numberp v) (float v) :invalid))
        :invalid)))
