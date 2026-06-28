;;;; src/encoding/windows-1252.lisp — windows-1252 decoder (cp1252, single-byte).
(in-package #:weft.encoding)

;;; Windows-1252 code page: bytes 0x80-0x9F differ from ISO-8859-1.
;;; Unmapped bytes in that range (81, 8D, 8F, 90, 9D) yield U+FFFD.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +cp1252-table+
    (let ((a (make-array 256 :element-type '(unsigned-byte 32))))
      ;; Bytes 0x00..0x7F: ASCII
      (dotimes (b 128) (setf (aref a b) b))
      ;; Bytes 0x80..0x9F: Windows-1252 extension
      (setf (aref a #x80) #x20AC)   ; €
      (setf (aref a #x81) #xFFFD)
      (setf (aref a #x82) #x201A)   ; ‚
      (setf (aref a #x83) #x0192)   ; ƒ
      (setf (aref a #x84) #x201E)   ; „
      (setf (aref a #x85) #x2026)   ; …
      (setf (aref a #x86) #x2020)   ; †
      (setf (aref a #x87) #x2021)   ; ‡
      (setf (aref a #x88) #x02C6)   ; ˆ
      (setf (aref a #x89) #x2030)   ; ‰
      (setf (aref a #x8A) #x0160)   ; Š
      (setf (aref a #x8B) #x2039)   ; ‹
      (setf (aref a #x8C) #x0152)   ; Œ
      (setf (aref a #x8D) #xFFFD)
      (setf (aref a #x8E) #x017D)   ; Ž
      (setf (aref a #x8F) #xFFFD)
      (setf (aref a #x90) #xFFFD)
      (setf (aref a #x91) #x2018)   ; '
      (setf (aref a #x92) #x2019)   ; '
      (setf (aref a #x93) #x201C)   ; "
      (setf (aref a #x94) #x201D)   ; "
      (setf (aref a #x95) #x2022)   ; •
      (setf (aref a #x96) #x2013)   ; –
      (setf (aref a #x97) #x2014)   ; —
      (setf (aref a #x98) #x02DC)   ; ˜
      (setf (aref a #x99) #x2122)   ; ™
      (setf (aref a #x9A) #x0161)   ; š
      (setf (aref a #x9B) #x203A)   ; ›
      (setf (aref a #x9C) #x0153)   ; œ
      (setf (aref a #x9D) #xFFFD)
      (setf (aref a #x9E) #x017E)   ; ž
      (setf (aref a #x9F) #x0178)   ; Ÿ
      ;; Bytes 0xA0..0xFF: Latin-1 Supplement (ISO-8859-1)
      (dotimes (b 96) (setf (aref a (+ #xA0 b)) (+ #xA0 b)))
      a)))

(define-decoder "windows-1252" (bytes)
  (let ((out (make-string-output-stream))
        (table +cp1252-table+)
        (n (length bytes)))
    (dotimes (i n)
      (write-char (code-char (aref table (aref bytes i))) out))
    (get-output-stream-string out)))