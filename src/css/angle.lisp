;;;; src/css/angle.lisp
(in-package #:weft.css)

(define-value-parser "angle" (s)
  (let* ((s (css-trim s))
         (s (ascii-downcase s)))
    (cond
      ;; Bare "0" and "0.0" -> (list 0.0 "deg")
      ((or (string= s "0") (string= s "0.0"))
       (list 0.0 "deg"))
      (t
       ;; Try to find a valid angle unit at the end of the string
       (let ((unit nil)
             (num-str nil))
         (dolist (u '("deg" "grad" "rad" "turn"))
           (let ((pos (search u s :from-end t)))
             (when (and pos
                        (= (+ pos (length u)) (length s)))
               (setf unit u
                     num-str (subseq s 0 pos))
               (return))))
         (if (null unit)
             :invalid
             ;; Parse the numeric prefix
             (handler-case
                 (let ((val (read-from-string num-str)))
                   (if (numberp val)
                       (list (float val 0.0) unit)
                       :invalid))
               (error () :invalid))))))))