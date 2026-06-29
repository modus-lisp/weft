;;;; src/css/time.lisp — CSS <time> value parser
(in-package #:weft.css)

(define-value-parser "time" (s)
  (block nil
    (let* ((trimmed (css-trim s))
           (lower (ascii-downcase trimmed))
           (len (length lower)))
      (when (zerop len) (return :invalid))
      (let ((unit)
            (num-end))
        (cond
          ((and (>= len 2)
                (char= (char lower (- len 2)) #\m)
                (char= (char lower (1- len)) #\s))
           (setf unit "ms" num-end (- len 2)))
          ((char= (char lower (1- len)) #\s)
           (setf unit "s" num-end (1- len)))
          (t (return :invalid)))
        (when (zerop num-end) (return :invalid))
        (let ((num-str (subseq lower 0 num-end)))
          (handler-case
              (let ((*read-eval* nil))
                (multiple-value-bind (val end) (read-from-string num-str)
                  (if (and (numberp val) (= end (length num-str)))
                      (list (float val 1.0f0) unit)
                      :invalid)))
            (error () :invalid)))))))