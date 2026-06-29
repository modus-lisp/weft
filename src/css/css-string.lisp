;;;; src/css/css-string.lisp — <string> value parser (strip matching outer quotes).
(in-package #:weft.css)
(define-value-parser "css-string" (s)
  (let ((tt (css-trim s)))
    (if (and (>= (length tt) 2)
             (member (char tt 0) '(#\" #\'))
             (char= (char tt (1- (length tt))) (char tt 0)))
        (subseq tt 1 (1- (length tt)))
        :invalid)))
