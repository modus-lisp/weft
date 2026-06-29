;;;; src/css/border-radius.lisp — 1-4 radii -> list of (value unit).
(in-package #:weft.css)
(defun %br-token (p)
  (let ((unit (cond ((and (>= (length p) 1) (char= (char p (1- (length p))) #\%)) "%")
                    ((and (>= (length p) 2) (string= (subseq p (- (length p) 2)) "px")) "px")
                    (t nil))))
    (when unit
      (let* ((numstr (if (string= unit "%") (subseq p 0 (1- (length p))) (subseq p 0 (- (length p) 2))))
             (num (ignore-errors (read-from-string numstr))))
        (when (numberp num) (list (float num) unit))))))
(define-value-parser "border-radius" (s)
  (let* ((tt (ascii-downcase (css-trim s)))
         (parts (split-ws-css tt)))
    (if (or (null parts) (> (length parts) 4)) :invalid
        (let ((toks (mapcar #'%br-token parts)))
          (if (member nil toks) :invalid toks)))))
(defun split-ws-css (tt)
  (let ((out '()) (b (make-string-output-stream)) (any nil))
    (loop for c across tt do
      (if (member c '(#\Space #\Tab #\Newline #\Return))
          (when any (push (get-output-stream-string b) out) (setf any nil b (make-string-output-stream)))
          (progn (write-char c b) (setf any t))))
    (when any (push (get-output-stream-string b) out))
    (nreverse out)))
