;;;; src/css/opacity.lisp
(in-package #:weft.css)

(define-value-parser "opacity" (s)
  (let ((s (css-trim (ascii-downcase s))))
    (if (zerop (length s))
        :invalid
        (let* ((len (length s))
               (has-pct (char= (char s (1- len)) #\%))
               (num-s (if has-pct (subseq s 0 (1- len)) s)))
          (if (zerop (length num-s))
              :invalid
              (handler-case
                  (let ((*read-eval* nil))
                    (multiple-value-bind (val pos)
                        (read-from-string num-s)
                      (if (and (numberp val) (= pos (length num-s)))
                          (let ((v (float val)))
                            (if has-pct
                                (max 0.0 (min 1.0 (/ v 100)))
                                (max 0.0 (min 1.0 v))))
                          :invalid)))
                (error () :invalid)))))))