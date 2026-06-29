;;;; src/css/order.lisp
(in-package #:weft.css)
(define-value-parser "order" (s)
  (let* ((trimmed (css-trim s))
         (len (length trimmed)))
    (if (zerop len)
        :invalid
        (block nil
          (let ((start 0)
                (negative nil)
                (first-char (char trimmed 0)))
            (cond ((char= first-char #\+) (setf start 1))
                  ((char= first-char #\-) (setf start 1 negative t))
                  ((digit-char-p first-char))
                  (t (return :invalid)))
            (when (>= start len)
              (return :invalid))
            (let ((val 0))
              (loop for i from start below len
                    for d = (digit-char-p (char trimmed i))
                    do (if (null d)
                           (return :invalid)
                           (setf val (+ (* val 10) d)))
                    finally (return (if negative (- val) val)))))))))