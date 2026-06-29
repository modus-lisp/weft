;;;; src/css/number.lisp
(in-package #:weft.css)

(define-value-parser "number" (s)
  (block nil
    (let* ((len (length s))
           (pos 0)
           (sign 1))
      ;; Optional leading sign
      (when (< pos len)
        (let ((c (char s pos)))
          (cond ((char= c #\-) (setf sign -1) (incf pos))
                ((char= c #\+) (incf pos)))))
      ;; Must have at least one character remaining
      (when (>= pos len) (return :invalid))
      ;; Integer part (digits before '.' or 'e'/'E')
      (let ((int-part 0)
            (has-int nil))
        (loop while (and (< pos len) (digit-char-p (char s pos)))
              do (setf has-int t
                       int-part (+ (* int-part 10)
                                   (digit-char-p (char s pos)))
                       pos (1+ pos)))
        ;; Fractional part
        (let ((frac 0)
              (frac-div 1)
              (has-frac nil))
          (when (and (< pos len) (char= (char s pos) #\.))
            (incf pos)
            (loop while (and (< pos len) (digit-char-p (char s pos)))
                  do (setf has-frac t
                           frac (+ (* frac 10)
                                   (digit-char-p (char s pos)))
                           frac-div (* frac-div 10)
                           pos (1+ pos))))
          ;; Must have at least some digits
          (unless (or has-int has-frac) (return :invalid))
          ;; Build base value
          (let ((value (+ (float int-part)
                          (/ (float frac) frac-div))))
            ;; Optional exponent
            (when (and (< pos len)
                       (or (char= (char s pos) #\e)
                           (char= (char s pos) #\E)))
              (incf pos)
              (let ((exp-sign 1)
                    (exp-val 0)
                    (has-exp nil))
                (when (< pos len)
                  (let ((c (char s pos)))
                    (cond ((char= c #\-) (setf exp-sign -1) (incf pos))
                          ((char= c #\+) (incf pos)))))
                (loop while (and (< pos len) (digit-char-p (char s pos)))
                      do (setf has-exp t
                               exp-val (+ (* exp-val 10)
                                          (digit-char-p (char s pos)))
                               pos (1+ pos)))
                (unless has-exp (return :invalid))
                (setf value (* value (expt 10.0 (* exp-sign exp-val))))))
            ;; Must consume entire input
            (unless (= pos len) (return :invalid))
            ;; Apply sign and return
            (* sign value)))))))