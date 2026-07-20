;;;; src/css/aspect-ratio.lisp — CSS Sizing 4 `aspect-ratio: auto || <ratio>`.
(in-package #:weft.css)

(defun %ar-number (s)
  "A non-negative decimal number from string S as a double, or NIL."
  (let ((s (css-trim s)))
    (when (and (plusp (length s))
               (every (lambda (c) (or (digit-char-p c) (member c '(#\. #\e #\E #\+ #\-)))) s))
      (ignore-errors
        (let ((*read-default-float-format* 'double-float))
          (let ((v (read-from-string s))) (and (realp v) (float v 1d0))))))))

(define-value-parser "aspect-ratio" (s)
  "auto | <ratio> | auto <ratio>, where <ratio> is `<number> [ / <number> ]?`.
Returns the width/height ratio as a double (explicit ratio — applies to the
box-sizing box), (:AUTO . ratio) when the `auto` keyword accompanies the ratio
(the ratio applies to the CONTENT box; a replaced element with a natural ratio
prefers that instead — CSS Sizing 4 §aspect-ratio), :AUTO (prefer the intrinsic
ratio, no explicit ratio), or :INVALID."
  (let ((str (ascii-downcase (css-trim s))))
    (if (string= str "")
        :invalid
        (let* ((tokens (loop with start = 0
                             for i = (position #\Space str :start start)
                             for tok = (string-trim '(#\Space #\Tab) (subseq str start (or i (length str))))
                             when (plusp (length tok)) collect tok
                             do (if i (setf start (1+ i)) (loop-finish))))
               (nonauto (remove "auto" tokens :test #'string=))
               (has-auto (< (length nonauto) (length tokens))))
          (cond
            ((null nonauto) (if has-auto :auto :invalid))
            (t (let* ((joined (apply #'concatenate 'string nonauto))
                      (slash (position #\/ joined))
                      (w (%ar-number (if slash (subseq joined 0 slash) joined)))
                      (h (if slash (%ar-number (subseq joined (1+ slash))) 1d0)))
                 (if (and w h (plusp w) (plusp h))
                     (let ((r (/ w h))) (if has-auto (cons :auto r) r))
                     :invalid))))))))
