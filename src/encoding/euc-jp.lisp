;;;; src/encoding/euc-jp.lisp — EUC-JP decoder (tables in euc-jp-table.lisp).
;;;; Single byte (ASCII + 0x8E half-width lead handled via *double*), 0x8F SS3
;;;; three-byte (JIS X 0212), and 0xA1-0xFE two-byte (JIS X 0208).  Error mode
;;;; matches the reference codec: an incomplete/invalid SS3 (0x8F) consumes
;;;; 0x8F plus the following byte as a single U+FFFD.
(in-package #:weft.encoding)

(define-decoder "euc-jp" (bytes)
  (let ((n (length bytes)) (out (make-string-output-stream)) (i 0))
    (loop while (< i n) do
      (let ((b (aref bytes i)))
        (multiple-value-bind (cp found) (gethash b *euc-jp-single*)
          (cond
            (found (write-char (code-char cp) out) (incf i))
            ((= b #x8f)
             ;; SS3: with two trailing bytes, look up the 3-byte char; if it is
             ;; not a defined sequence, consume ONLY 0x8F (one U+FFFD) and
             ;; reprocess.  With fewer than two trailing bytes it is incomplete:
             ;; one U+FFFD consuming the remainder.  (Matches the reference codec.)
             (cond
               ((< (+ i 2) n)
                (let ((cp3 (gethash (logior (ash b 16) (ash (aref bytes (+ i 1)) 8)
                                            (aref bytes (+ i 2)))
                                    *euc-jp-triple*)))
                  (if cp3
                      (progn (write-char (code-char cp3) out) (incf i 3))
                      (progn (write-char +replacement+ out) (incf i)))))
               (t (write-char +replacement+ out) (setf i n))))
            (t
             (let ((cp2 (and (< (+ i 1) n)
                             (gethash (logior (ash b 8) (aref bytes (+ i 1)))
                                      *euc-jp-double*))))
               (if cp2
                   (progn (write-char (code-char cp2) out) (incf i 2))
                   (progn (write-char +replacement+ out) (incf i)))))))))
    (get-output-stream-string out)))
