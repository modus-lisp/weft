;;;; src/encoding/gb18030.lisp — GB18030 decoder (tables + 4-byte linear ranges).
;;;;
;;;; 1-byte (ASCII) and 2-byte via *gb18030-single* / *gb18030-double*; the
;;;; 4-byte form (b1 81-FE, b2 30-39, b3 81-FE, b4 30-39) maps a pointer through
;;;; *gb18030-ranges* (cp = cs + (p - ps) for the largest run-start ps <= p).
(in-package #:weft.encoding)

(defun gb18030-range-cp (p)
  "Codepoint for a 4-byte pointer P via the linear-range table, or NIL."
  (let ((v *gb18030-ranges*) (lo 0) (hi (1- (length *gb18030-ranges*))) (best nil))
    (loop while (<= lo hi) do
      (let* ((mid (ash (+ lo hi) -1)) (ps (car (aref v mid))))
        (if (<= ps p) (progn (setf best mid lo (1+ mid))) (setf hi (1- mid)))))
    (when best
      (let ((cp (+ (cdr (aref v best)) (- p (car (aref v best))))))
        (when (<= cp #x10ffff) cp)))))

(define-decoder "gb18030" (bytes)
  (let ((n (length bytes)) (out (make-string-output-stream)) (i 0))
    (flet ((emit (cp) (write-char (code-char cp) out)))
      (loop while (< i n) do
        (let ((b (aref bytes i)))
          (cond
            ;; 1-byte (ASCII)
            ((gethash b *gb18030-single*) (emit (gethash b *gb18030-single*)) (incf i))
            ;; 4-byte: lead 0x81-0xFE then a digit 0x30-0x39
            ((and (<= #x81 b #xfe) (< (+ i 1) n) (<= #x30 (aref bytes (+ i 1)) #x39))
             (if (>= (+ i 3) n)
                 (progn (emit #xfffd) (setf i n))    ; incomplete at EOF -> consume remainder
                 (let* ((b3 (aref bytes (+ i 2))) (b4 (aref bytes (+ i 3)))
                        (cp (and (<= #x81 b3 #xfe) (<= #x30 b4 #x39)
                                 (gb18030-range-cp
                                  (+ (* (- b #x81) 12600) (* (- (aref bytes (+ i 1)) #x30) 1260)
                                     (* (- b3 #x81) 10) (- b4 #x30))))))
                   (if cp (progn (emit cp) (incf i 4))
                       (progn (emit #xfffd) (incf i))))))   ; bad structure/pointer -> consume lead, reprocess
            ;; 2-byte
            ((and (< (+ i 1) n) (gethash (logior (ash b 8) (aref bytes (+ i 1))) *gb18030-double*))
             (emit (gethash (logior (ash b 8) (aref bytes (+ i 1))) *gb18030-double*)) (incf i 2))
            (t (emit #xfffd) (incf i))))))
    (get-output-stream-string out)))
