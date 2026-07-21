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

;;; ---- clip-path (CSS Masking 1 §clip-path, basic shapes) -----------------

(defun %clip-lp (s fs)
  "Parse a length-percentage token -> px float, (:pct . N), or NIL.  FS is the
font-size for em/relative units; percentages are deferred to paint (box-relative)."
  (let ((s (css-trim s)))
    (when (plusp (length s))
      (if (char= (char s (1- (length s))) #\%)
          (let ((v (%filter-number (subseq s 0 (1- (length s))))))
            (when v (cons :pct v)))
          (let ((px (resolve-len s fs)))
            (when (numberp px) (float px 1.0)))))))

(defun %clip-pos1 (tok axis fs)
  "Resolve one position keyword/length TOK on AXIS (:x or :y) -> px-or-(:pct.N)."
  (let ((tok (css-trim (ascii-downcase tok))))
    (cond ((string= tok "center") (cons :pct 50))
          ((and (eq axis :x) (string= tok "left")) 0.0)
          ((and (eq axis :x) (string= tok "right")) (cons :pct 100))
          ((and (eq axis :y) (string= tok "top")) 0.0)
          ((and (eq axis :y) (string= tok "bottom")) (cons :pct 100))
          (t (%clip-lp tok fs)))))

(defun %clip-position (s fs)
  "Parse a <position> (1 or 2 tokens after `at`) -> (cx . cy), each px-or-(:pct.N).
Defaults to centre (50% 50%)."
  (let ((toks (remove "" (uiop-split s) :test #'string=)))
    (cond ((null toks) (cons (cons :pct 50) (cons :pct 50)))
          ((= (length toks) 1)
           ;; a single token names one axis (keyword) or is a horizontal length
           (let ((tk (ascii-downcase (css-trim (first toks)))))
             (cond ((member tk '("top" "bottom") :test #'string=)
                    (cons (cons :pct 50) (%clip-pos1 tk :y fs)))
                   (t (cons (%clip-pos1 tk :x fs) (cons :pct 50))))))
          (t (cons (%clip-pos1 (first toks) :x fs)
                   (%clip-pos1 (second toks) :y fs))))))

(defun uiop-split (s)
  "Split S on ASCII whitespace runs into non-empty tokens."
  (let ((out '()) (start nil) (n (length s)))
    (dotimes (i n)
      (let ((ws (member (char s i) '(#\Space #\Tab #\Newline #\Return))))
        (cond ((and (not ws) (null start)) (setf start i))
              ((and ws start) (push (subseq s start i) out) (setf start nil)))))
    (when start (push (subseq s start n) out))
    (nreverse out)))

(defun %clip-radius (s fs)
  "Parse a <shape-radius>: length-percentage | closest-side | farthest-side | NIL."
  (let ((s (css-trim (ascii-downcase s))))
    (cond ((zerop (length s)) :closest-side)
          ((string= s "closest-side") :closest-side)
          ((string= s "farthest-side") :farthest-side)
          (t (%clip-lp s fs)))))

(defun parse-clip-path (value fs)
  "Parse a CSS clip-path basic-shape value (CSS Masking 1 §clip-path) into the
CSTYLE-CLIP-PATH form.  Supports inset()/circle()/ellipse()/polygon(); an unknown
value, url(), or none yields NIL (no clip)."
  (let ((s (css-trim (ascii-downcase value))))
    (cond
      ((zerop (length s)) nil)
      ((member s '("none" "initial" "unset" "revert" "inherit") :test #'string=) nil)
      (t
       (let ((lp (position #\( s)) (rp (position #\) s :from-end t)))
         (unless (and lp rp (< lp rp)) (return-from parse-clip-path nil))
         (let ((name (css-trim (subseq s 0 lp)))
               (args (css-trim (subseq s (1+ lp) rp))))
           (cond
             ((string= name "inset")
              ;; inset( <lp>{1,4} [ round <border-radius> ]? ) — the `round` radii are
              ;; parsed off but not modelled (sharp-cornered inset rect).
              (let* ((rpos (search " round " args))
                     (sides (if rpos (subseq args 0 rpos) args))
                     (toks (uiop-split sides))
                     (vs (mapcar (lambda (tk) (%clip-lp tk fs)) toks)))
                (when (and vs (every #'identity vs))
                  (destructuring-bind (top &optional right bottom left) vs
                    (let ((r (or right top)))
                      (list :inset top r (or bottom top) (or left r)))))))
             ((member name '("circle" "ellipse") :test #'string=)
              (let* ((apos (search " at " args))
                     (rad-s (if apos (subseq args 0 apos) args))
                     (pos-s (if apos (subseq args (+ apos 4)) ""))
                     (pos (%clip-position pos-s fs))
                     (rtoks (uiop-split rad-s)))
                (if (string= name "circle")
                    (list :circle (if rtoks (%clip-radius (first rtoks) fs) :closest-side)
                          (car pos) (cdr pos))
                    (list :ellipse
                          (if rtoks (%clip-radius (first rtoks) fs) :closest-side)
                          (if (cdr rtoks) (%clip-radius (second rtoks) fs) :closest-side)
                          (car pos) (cdr pos)))))
             ((string= name "polygon")
              ;; polygon( [<fill-rule>,]? <lp> <lp> # )
              (let* ((parts (mapcar #'css-trim
                                    (split-string-on args #\,)))
                     ;; drop a leading fill-rule
                     (parts (if (and parts (member (first parts)
                                                   '("nonzero" "evenodd") :test #'string=))
                                (rest parts) parts))
                     (pts (loop for p in parts
                                for tk = (uiop-split p)
                                when (>= (length tk) 2)
                                  collect (cons (%clip-lp (first tk) fs)
                                                (%clip-lp (second tk) fs)))))
                (when (>= (length pts) 3) (list* :polygon pts))))
             (t nil))))))))

(defun split-string-on (s ch)
  "Split S on character CH into a list of substrings (empty parts kept)."
  (let ((out '()) (start 0))
    (dotimes (i (length s))
      (when (char= (char s i) ch)
        (push (subseq s start i) out) (setf start (1+ i))))
    (push (subseq s start) out)
    (nreverse out)))

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