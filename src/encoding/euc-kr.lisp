;;;; src/encoding/euc-kr.lisp — euc-kr decoder
(in-package #:weft.encoding)

(define-decoder "euc-kr" (bytes)
  (let ((n (length bytes))
        (out (make-string-output-stream)))
    (loop with i = 0
          while (< i n)
          do (let ((b (aref bytes i)))
               (multiple-value-bind (cp found) (gethash b *euc-kr-single*)
                 (cond (found
                        (write-char (code-char cp) out)
                        (incf i))
                       ((and (< (+ i 1) n)
                             (multiple-value-bind (cp2 found2)
                                 (gethash (logior (ash b 8) (aref bytes (+ i 1)))
                                          *euc-kr-double*)
                               (and found2
                                    (write-char (code-char cp2) out)
                                    t)))
                        (incf i 2))
                       (t
                        (write-char (code-char #xfffd) out)
                        (incf i))))))
    (get-output-stream-string out)))