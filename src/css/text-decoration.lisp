;;;; src/css/text-decoration.lisp — <text-decoration> value parser.
(in-package #:weft.css)
(define-value-parser "text-decoration" (s)
  (let ((tt (ascii-downcase (css-trim s))))
    (cond
      ((string= tt "none") (list "none"))
      (t (let ((toks '()) (b (make-string-output-stream)) (any nil))
           (flet ((flush () (when any
                              (let ((w (get-output-stream-string b)))
                                (when (member w '("underline" "overline" "line-through" "blink") :test #'string=)
                                  (push w toks)))
                              (setf any nil b (make-string-output-stream)))))
             (loop for c across tt do
               (if (member c '(#\Space #\Tab #\Newline)) (flush)
                   (progn (write-char c b) (setf any t))))
             (flush))
           (if toks (nreverse toks) :invalid))))))
