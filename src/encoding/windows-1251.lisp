;;;; src/encoding/windows-1251.lisp — windows-1251 decoder (cp1251 / cyrillic).
(in-package #:weft.encoding)

(define-decoder "windows-1251" (bytes)
  (let ((out (make-string (length bytes)))
        (n (length bytes))
        (table
         (make-array 128
                     :element-type 'character
                     :initial-contents
                     (list
                      ;; 0x80-0x8f
                      (code-char #x0402) (code-char #x0403) (code-char #x201a)
                      (code-char #x0453) (code-char #x201e) (code-char #x2026)
                      (code-char #x2020) (code-char #x2021)
                      (code-char #x20ac) (code-char #x2030) (code-char #x0409)
                      (code-char #x2039) (code-char #x040a) (code-char #x040c)
                      (code-char #x040b) (code-char #x040f)
                      ;; 0x90-0x9f
                      (code-char #x0452) (code-char #x2018) (code-char #x2019)
                      (code-char #x201c) (code-char #x201d) (code-char #x2022)
                      (code-char #x2013) (code-char #x2014)
                      +replacement+   (code-char #x2122) (code-char #x0459)
                      (code-char #x203a) (code-char #x045a) (code-char #x045c)
                      (code-char #x045b) (code-char #x045f)
                      ;; 0xa0-0xaf
                      (code-char #x00a0) (code-char #x040e) (code-char #x045e)
                      (code-char #x0408) (code-char #x00a4) (code-char #x0490)
                      (code-char #x00a6) (code-char #x00a7)
                      (code-char #x0401) (code-char #x00a9) (code-char #x0404)
                      (code-char #x00ab) (code-char #x00ac) (code-char #x00ad)
                      (code-char #x00ae) (code-char #x0407)
                      ;; 0xb0-0xbf
                      (code-char #x00b0) (code-char #x00b1) (code-char #x0406)
                      (code-char #x0456) (code-char #x0491) (code-char #x00b5)
                      (code-char #x00b6) (code-char #x00b7)
                      (code-char #x0451) (code-char #x2116) (code-char #x0454)
                      (code-char #x00bb) (code-char #x0458) (code-char #x0405)
                      (code-char #x0455) (code-char #x0457)
                      ;; 0xc0-0xcf
                      (code-char #x0410) (code-char #x0411) (code-char #x0412)
                      (code-char #x0413) (code-char #x0414) (code-char #x0415)
                      (code-char #x0416) (code-char #x0417)
                      (code-char #x0418) (code-char #x0419) (code-char #x041a)
                      (code-char #x041b) (code-char #x041c) (code-char #x041d)
                      (code-char #x041e) (code-char #x041f)
                      ;; 0xd0-0xdf
                      (code-char #x0420) (code-char #x0421) (code-char #x0422)
                      (code-char #x0423) (code-char #x0424) (code-char #x0425)
                      (code-char #x0426) (code-char #x0427)
                      (code-char #x0428) (code-char #x0429) (code-char #x042a)
                      (code-char #x042b) (code-char #x042c) (code-char #x042d)
                      (code-char #x042e) (code-char #x042f)
                      ;; 0xe0-0xef
                      (code-char #x0430) (code-char #x0431) (code-char #x0432)
                      (code-char #x0433) (code-char #x0434) (code-char #x0435)
                      (code-char #x0436) (code-char #x0437)
                      (code-char #x0438) (code-char #x0439) (code-char #x043a)
                      (code-char #x043b) (code-char #x043c) (code-char #x043d)
                      (code-char #x043e) (code-char #x043f)
                      ;; 0xf0-0xff
                      (code-char #x0440) (code-char #x0441) (code-char #x0442)
                      (code-char #x0443) (code-char #x0444) (code-char #x0445)
                      (code-char #x0446) (code-char #x0447)
                      (code-char #x0448) (code-char #x0449) (code-char #x044a)
                      (code-char #x044b) (code-char #x044c) (code-char #x044d)
                      (code-char #x044e) (code-char #x044f)))))
    (dotimes (i n)
      (let ((b (aref bytes i)))
        (setf (char out i)
              (if (<= b #x7f)
                  (code-char b)
                  (aref table (- b #x80))))))
    out))