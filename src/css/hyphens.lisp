;;;; src/css/hyphens.lisp — the `hyphens` property (CSS Text 3 §6.1).
(in-package #:weft.css)
(define-value-parser "hyphens" (s)
  (let ((v (ascii-downcase (css-trim s))))
    (cond ((string= v "none") "none")
          ((string= v "manual") "manual")
          ((string= v "auto") "auto")
          (t :invalid))))
