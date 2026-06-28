;;;; src/encoding/gbk.lisp — gbk decoder (GBK).
(in-package #:weft.encoding)

(define-decoder "gbk" (bytes)
  (let ((n (length bytes))
        (out (make-string-output-stream))
        (single *gbk-single*)
        (double *gbk-double*))
    (loop for i from 0 below n
          for b = (aref bytes i)
          for cp = (gethash b single)
          do (cond (cp
                    (write-char (if (= cp #xFFFD) +replacement+ (code-char cp)) out))
                   ((and (< (+ i 1) n)
                         (let ((cp2 (gethash (logior (ash b 8) (aref bytes (+ i 1))) double)))
                           (when cp2
                             (write-char (if (= cp2 #xFFFD) +replacement+ (code-char cp2)) out)
                             (incf i)
                             t))))
                   (t
                    (write-char +replacement+ out))))
    (get-output-stream-string out)))