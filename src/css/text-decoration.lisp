;;;; src/css/text-decoration.lisp — <text-decoration> value parser.
(in-package #:weft.css)

(define-value-parser "text-decoration" (s)
  (block nil
    (let* ((s (css-trim s))
           (len (length s)))
      (when (zerop len) (return :invalid))
      ;; Split on whitespace
      (let ((parts '())
            (start 0)
            (i 0))
        (loop while (< i len)
              do (let ((ch (char s i)))
                   (cond ((member ch '(#\Space #\Tab #\Newline #\Return #\Page))
                          (when (> i start)
                            (push (subseq s start i) parts))
                          (setf start (1+ i))
                          (incf i))
                         (t (incf i)))))
        (when (> len start)
          (push (subseq s start len) parts))
        (setf parts (nreverse parts))
        (when (null parts) (return :invalid))
        ;; Downcase and validate each keyword
        (let ((seen-none nil)
              (result '()))
          (dolist (part parts)
            (let ((kw (ascii-downcase part)))
              (cond
                ((string= kw "none") (setf seen-none t))
                ((member kw '("underline" "overline" "line-through" "blink") :test #'string=)
                 (push kw result))
                (t (return :invalid)))))
          (setf result (nreverse result))
          ;; "none" must appear alone
          (when seen-none
            (if (null result)
                (return '("none"))
                (return :invalid)))
          ;; Must have at least one keyword
          (when (null result) (return :invalid))
          result)))))