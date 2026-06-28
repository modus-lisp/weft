;;;; src/encoding/utf-16le.lisp — UTF-16LE decoder (WHATWG / Unicode).
(in-package #:weft.encoding)

(define-decoder "utf-16le" (bytes)
  (let ((out (make-string-output-stream))
        (n (length bytes))
        (i 0))
    (flet ((emit (cp) (write-char (code-char cp) out)))
      (loop while (< i n) do
        (if (< (- n i) 2)
            ;; Odd trailing byte → FFFD
            (progn (emit #xfffd) (incf i))
            (let* ((lo (aref bytes i))
                   (hi (aref bytes (1+ i)))
                   (code-unit (logior lo (ash hi 8))))
              (cond
                ((<= #xd800 code-unit #xdbff)
                 ;; High surrogate: need next pair as low surrogate
                 (if (< (- n (+ i 2)) 2)
                     ;; Truncated surrogate pair — emit FFFD, consume rest
                     (progn (emit #xfffd) (setf i n))
                     (let* ((lo2 (aref bytes (+ i 2)))
                            (hi2 (aref bytes (+ i 3)))
                            (low-surr (logior lo2 (ash hi2 8))))
                       (if (<= #xdc00 low-surr #xdfff)
                           ;; Valid surrogate pair → supplementary code point
                           (let ((supp (+ #x10000
                                          (ash (- code-unit #xd800) 10)
                                          (- low-surr #xdc00))))
                             (emit supp)
                             (incf i 4))
                           ;; High surrogate not followed by low surrogate
                           (progn (emit #xfffd) (incf i 2))))))
                ((<= #xdc00 code-unit #xdfff)
                 ;; Lone low surrogate
                 (emit #xfffd) (incf i 2))
                (t
                 ;; Regular BMP code unit
                 (emit code-unit) (incf i 2)))))))
    (get-output-stream-string out)))