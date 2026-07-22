;;;; src/css/transform.lisp — transform list -> list of (fn arg...).
(in-package #:weft.css)
(define-value-parser "transform" (s)
  (let ((tt (ascii-downcase (css-trim s))))
    (if (string= tt "none") (list "none")
        (let ((out '()) (i 0) (n (length tt)) (bad nil))
          (loop while (and (< i n) (not bad)) do
            (loop while (and (< i n) (member (char tt i) '(#\Space #\Tab #\Newline))) do (incf i))
            (when (< i n)
              (let ((name-start i))
                (loop while (and (< i n) (alphanumericp (char tt i))) do (incf i))
                (if (and (< i n) (char= (char tt i) #\() (> i name-start))
                    (let ((fn (subseq tt name-start i)) (arg-start (1+ i)))
                      (incf i)
                      (loop while (and (< i n) (not (char= (char tt i) #\)))) do (incf i))
                      (let ((args (mapcar (lambda (a) (string-trim '(#\Space) a))
                                          (remove "" (split-comma (subseq tt arg-start i)) :test #'string=))))
                        (push (cons fn args) out))
                      (when (< i n) (incf i)))
                    (setf bad t)))))
          (if (or bad (null out)) :invalid (nreverse out))))))
(defun parse-individual-transform (kind value)
  "Parse a `translate`/`rotate`/`scale` individual-property VALUE (KIND :translate/
:rotate/:scale) into the equivalent transform-function form ((fn arg…)) or NIL for
`none`/empty (CSS Transforms 2 §3).  translate -> ((\"translate\" x [y])); scale ->
((\"scale\" x [y])); rotate -> ((\"rotate\" angle)) — a leading x/y/z axis keyword is
consumed and only the z (default) axis produces a 2D rotation, x/y degrade to none."
  (let* ((v (ascii-downcase (css-trim value)))
         (toks (remove "" (split-ws v) :test #'string=)))
    (cond
      ((or (null toks) (string= v "none") (string= v "initial") (string= v "unset")) nil)
      ((eq kind :translate) (list (cons "translate" (subseq toks 0 (min 2 (length toks))))))
      ((eq kind :scale) (list (cons "scale" (subseq toks 0 (min 2 (length toks))))))
      ((eq kind :rotate)
       ;; optional leading axis keyword (x/y/z); only z (or none) is a 2D rotation.
       (let ((axis nil) (rest toks))
         (when (member (first toks) '("x" "y" "z") :test #'string=)
           (setf axis (first toks) rest (rest toks)))
         (cond ((null rest) nil)
               ((and axis (not (string= axis "z"))) nil)  ; x/y axis: no 2D effect here
               (t (list (list "rotate" (first rest)))))))
      (t nil))))

(defun cstyle-effective-transform (cs)
  "The effective transform-function list for CS combining the individual translate/
rotate/scale properties with the `transform` list, in spec order (translate, rotate,
scale, then transform) — CSS Transforms 2 §3.  NIL when none apply."
  (let ((tr (cstyle-translate cs)) (ro (cstyle-rotate cs))
        (sc (cstyle-scale cs)) (tf (cstyle-transform cs)))
    (if (or tr ro sc)
        (append tr ro sc (unless (equal tf '("none")) tf))
        (and tf (not (equal tf '("none"))) tf))))

(defun split-comma (s)
  (let ((out '()) (b (make-string-output-stream)) (any nil))
    (loop for c across s do
      (if (char= c #\,) (progn (push (get-output-stream-string b) out) (setf b (make-string-output-stream) any nil))
          (progn (write-char c b) (setf any t))))
    (let ((last (get-output-stream-string b))) (when (or any (plusp (length last))) (push last out)))
    (nreverse out)))
