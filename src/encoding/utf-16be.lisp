;;;; src/encoding/utf-16be.lisp — UTF-16BE decoder (WHATWG Encoding).
(in-package #:weft.encoding)

(define-decoder "utf-16be" (bytes)
  (let ((out (make-string-output-stream))
        (n (length bytes))
        (i 0)
        (lead-surrogate nil))
    (flet ((emit (cp) (write-char (code-char cp) out)))
      (loop while (< i n) do
        ;; We need a complete code unit (2 bytes).
        ;; If fewer than 2 bytes remain, this is an error.
        (if (> (- n i) 1)
            (let ((hi (aref bytes i))
                  (lo (aref bytes (1+ i))))
              (let ((cp (logior (ash hi 8) lo)))
                (if lead-surrogate
                    ;; We are expecting a low surrogate
                    (if (<= #xdc00 cp #xdfff)
                        ;; Valid pair
                        (let ((decoded (+ #x10000
                                          (logior (ash (- lead-surrogate #xd800) 10)
                                                  (- cp #xdc00)))))
                          (setf lead-surrogate nil)
                          (emit decoded)
                          (incf i 2))
                        ;; Not a low surrogate — emit FFFD and reprocess cp
                        (progn
                          (setf lead-surrogate nil)
                          (emit #xfffd)
                          ;; Don't advance i; reprocess this code unit
                          ))
                    (cond
                      ;; High surrogate
                      ((<= #xd800 cp #xdbff)
                       (setf lead-surrogate cp)
                       (incf i 2))
                      ;; Unpaired low surrogate
                      ((<= #xdc00 cp #xdfff)
                       (emit #xfffd)
                       (incf i 2))
                      ;; Valid BMP code point
                      (t
                       (emit cp)
                       (incf i 2))))))
            ;; Less than 2 bytes left at end of input.
            ;; Per WHATWG spec: if lead surrogate is set, emit one U+FFFD
            ;; for the entire malformed sequence (surrogate + incomplete code unit).
            ;; Otherwise emit one U+FFFD for the trailing byte.
            (progn
              (when lead-surrogate
                (setf lead-surrogate nil))
              (emit #xfffd)
              (incf i 1))))
      ;; Flush any pending lead surrogate at end of input.
      (when lead-surrogate
        (emit #xfffd)))
    (get-output-stream-string out)))