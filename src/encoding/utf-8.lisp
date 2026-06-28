;;;; src/encoding/utf-8.lisp — UTF-8 decoder (WHATWG / Unicode maximal-subpart).
;;;; Reference unit written by hand to validate the kernel + oracle harness.
(in-package #:weft.encoding)

(define-decoder "utf-8" (bytes)
  (let ((out (make-string-output-stream))
        (n (length bytes)) (i 0)
        (cp 0) (needed 0) (seen 0) (lower #x80) (upper #xbf))
    (flet ((emit (c) (write-char (code-char c) out))
           (reset () (setf needed 0 seen 0 cp 0 lower #x80 upper #xbf)))
      (loop while (< i n) do
        (let ((b (aref bytes i)))
          (cond
            ((zerop needed)
             (cond
               ((<= b #x7f) (emit b))
               ((<= #xc2 b #xdf) (setf needed 1 cp (logand b #x1f)))
               ((<= #xe0 b #xef)
                (setf needed 2 cp (logand b #x0f))
                (when (= b #xe0) (setf lower #xa0))
                (when (= b #xed) (setf upper #x9f)))
               ((<= #xf0 b #xf4)
                (setf needed 3 cp (logand b #x07))
                (when (= b #xf0) (setf lower #x90))
                (when (= b #xf4) (setf upper #x8f)))
               (t (emit #xfffd)))             ; invalid lead byte
             (incf i))
            (t
             (if (<= lower b upper)
                 (progn
                   (setf cp (logior (ash cp 6) (logand b #x3f))
                         lower #x80 upper #xbf)
                   (incf seen) (incf i)
                   (when (= seen needed) (emit cp) (reset)))
                 ;; ill-formed continuation: emit FFFD and REPROCESS this byte
                 (progn (emit #xfffd) (reset)))))))
      (when (plusp needed) (emit #xfffd)))     ; truncated at EOF
    (get-output-stream-string out)))
