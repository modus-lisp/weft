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

;;; ---- filter (CSS Filter Effects 1 §filter) ------------------------------

(defun %filter-number (s)
  "Read S as a plain number, or NIL."
  (when (plusp (length s))
    (handler-case
        (let ((*read-eval* nil))
          (multiple-value-bind (v pos) (read-from-string s)
            (and (numberp v) (= pos (length s)) (float v 1.0))))
      (error () nil))))

(defun %filter-amount (arg default)
  "Parse a filter amount ARG (<number>|<percentage>) -> fraction; DEFAULT if empty."
  (let ((arg (css-trim arg)))
    (if (zerop (length arg))
        default
        (let* ((pct (char= (char arg (1- (length arg))) #\%))
               (ns (if pct (subseq arg 0 (1- (length arg))) arg))
               (v (%filter-number ns)))
          (when v (if pct (/ v 100.0) v))))))

(defun %filter-length (arg)
  "Parse a filter length ARG (a blur radius, px) -> non-negative px, or NIL."
  (let ((arg (css-trim arg)))
    (when (plusp (length arg))
      (let ((v (cond ((and (>= (length arg) 2)
                           (string= (subseq arg (- (length arg) 2)) "px"))
                      (%filter-number (subseq arg 0 (- (length arg) 2))))
                     (t (%filter-number arg)))))  ; bare number tolerated
        (when v (max 0.0 v))))))

(defun %parse-filter-func (name arg)
  "Parse one filter function NAME(ARG) -> (OP . VALUE), or NIL if unsupported/invalid."
  (cond
    ((string= name "blur")
     (let ((px (%filter-length arg))) (when px (cons :blur px))))
    ((member name '("grayscale" "sepia" "invert" "opacity") :test #'string=)
     (let ((a (%filter-amount arg 1.0)))
       (when a (cons (intern (string-upcase name) :keyword) (max 0.0 (min 1.0 a))))))
    ((member name '("brightness" "contrast" "saturate") :test #'string=)
     (let ((a (%filter-amount arg 1.0)))
       (when a (cons (intern (string-upcase name) :keyword) (max 0.0 a)))))
    (t nil)))

(defun parse-filter (value)
  "Parse a CSS filter list into an ordered list of (OP . VALUE) (see CSTYLE-FILTER).
Returns NIL for none/empty, :inherit for inherit, or :invalid on a parse error."
  (let ((s (css-trim (ascii-downcase value))))
    (cond
      ((zerop (length s)) nil)
      ((string= s "none") nil)
      ((string= s "inherit") :inherit)
      ((member s '("initial" "unset" "revert") :test #'string=) nil)
      (t
       (let ((out '()) (i 0) (n (length s)))
         (loop
           (loop while (and (< i n)
                            (member (char s i) '(#\Space #\Tab #\Newline #\Return #\,)))
                 do (incf i))
           (when (>= i n) (return))
           (let ((lp (position #\( s :start i)))
             (unless lp (return-from parse-filter :invalid))
             (let* ((name (css-trim (subseq s i lp)))
                    (rp (position #\) s :start lp)))
               (unless rp (return-from parse-filter :invalid))
               (let ((f (%parse-filter-func name (subseq s (1+ lp) rp))))
                 (unless f (return-from parse-filter :invalid))
                 (push f out))
               (setf i (1+ rp)))))
         (nreverse out))))))