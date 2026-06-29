;;;; src/css/letter-spacing.lisp
(in-package #:weft.css)
(defun %dim-pxem% (k)
  (let ((unit (cond ((and (>= (length k) 1) (char= (char k (1- (length k))) #\%)) "%")
                    ((and (>= (length k) 2) (string= (subseq k (- (length k) 2)) "px")) "px")
                    ((and (>= (length k) 2) (string= (subseq k (- (length k) 2)) "em")) "em") (t nil))))
    (when unit
      (let* ((ns (subseq k 0 (- (length k) (length unit)))) (n (ignore-errors (read-from-string ns))))
        (when (numberp n) (list (float n) unit))))))
(define-value-parser "letter-spacing" (s)
  (let ((k (ascii-downcase (css-trim s))))
    (if (string= k "normal") (list "normal") (or (%dim-pxem% k) :invalid))))
