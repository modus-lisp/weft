;;;; src/css/text-indent.lisp
(in-package #:weft.css)

(define-value-parser "text-indent" (s)
  (block nil
    (let* ((trimmed (css-trim s))
           (len (length trimmed)))
      (when (zerop len) (return :invalid))
      (let ((i 0)
            (has-digit nil))
        ;; optional leading sign
        (when (< i len)
          (let ((c (char trimmed i)))
            (when (or (char= c #\+) (char= c #\-))
              (incf i))))
        ;; integer part
        (loop while (and (< i len) (digit-char-p (char trimmed i)))
              do (setf has-digit t) (incf i))
        ;; optional dot and fractional part
        (when (and (< i len) (char= (char trimmed i) #\.))
          (incf i)
          (loop while (and (< i len) (digit-char-p (char trimmed i)))
                do (setf has-digit t) (incf i)))
        (unless has-digit (return :invalid))
        ;; parse the numeric portion
        (let* ((*read-eval* nil)
               (num (read-from-string (subseq trimmed 0 i))))
          (if (= i len)
              ;; no unit suffix — only bare 0 is valid
              (if (= num 0)
                  (return (list 0.0 ""))
                  (return :invalid))
              ;; there is a unit suffix
              (let ((rest (subseq trimmed i)))
                (if (and (= (length rest) 1) (char= (char rest 0) #\%))
                    (return (list (float num) "%"))
                    (if (loop for c across rest always (alpha-char-p c))
                        (return (list (float num) (ascii-downcase rest)))
                        (return :invalid))))))))))