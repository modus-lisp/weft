;;;; src/css/integer.lisp — <integer> value parser.
(in-package #:weft.css)

(define-value-parser "integer" (s)
  (block nil
    (let* ((s (css-trim s))
           (len (length s))
           (pos 0)
           (sign 1))
      ;; Empty string
      (when (zerop len) (return :invalid))
      ;; Optional leading sign
      (let ((c (char s pos)))
        (cond ((char= c #\-) (setf sign -1) (incf pos))
              ((char= c #\+) (incf pos))))
      ;; Must have at least one digit after optional sign
      (when (>= pos len) (return :invalid))
      ;; Parse digits
      (let ((value 0)
            (has-digit nil))
        (loop while (and (< pos len) (digit-char-p (char s pos)))
              do (setf has-digit t
                       value (+ (* value 10)
                                (digit-char-p (char s pos)))
                       pos (1+ pos)))
        (unless has-digit (return :invalid))
        ;; Must consume entire input
        (unless (= pos len) (return :invalid))
        (* sign value)))))