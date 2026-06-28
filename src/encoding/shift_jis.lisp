;;;; src/encoding/shift_jis.lisp — shift_jis decoder
(in-package #:weft.encoding)

(define-decoder "shift_jis" (bytes)
  (let ((n (length bytes))
        (out (make-string-output-stream)))
    (loop for i from 0 below n do
      (let* ((b (aref bytes i))
             (cp (gethash b *shift_jis-single*)))
        (if cp
            (write-char (code-char cp) out)
            (if (and (< (+ i 1) n)
                     (gethash (logior (ash b 8) (aref bytes (1+ i)))
                              *shift_jis-double*))
                (let ((cp2 (gethash (logior (ash b 8) (aref bytes (1+ i)))
                                    *shift_jis-double*)))
                  (write-char (code-char cp2) out)
                  (incf i))
                (write-char +replacement+ out)))))
    (get-output-stream-string out)))