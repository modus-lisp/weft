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
    ((string= name "drop-shadow") (%parse-drop-shadow arg))
    (t nil)))

(defun %parse-drop-shadow (arg)
  "Parse drop-shadow(offx offy [blur] [color]) -> (:drop-shadow dx dy blur r g b a),
with A the shadow-colour alpha as a 0..1 fraction (CSS Filter Effects 1 §drop-shadow).
A missing colour defaults to currentColor, approximated as black."
  (let ((toks (ws-split-top (css-trim arg))) (color nil) (lens '()) (fs 16.0))
    (dolist (tok toks)
      (cond ((and (null color) (gcolor-token-p tok)) (setf color (gcolor tok)))
            (t (let ((px (resolve-len tok fs)))
                 (if (numberp px) (push px lens)
                     (return-from %parse-drop-shadow nil))))))
    (setf lens (nreverse lens))
    (when (and (>= (length lens) 2) (<= (length lens) 3))
      (let* ((c (if (and color (listp color)) color '(0 0 0)))
             (araw (if (>= (length c) 4) (fourth c) 255))
             (a (if (> araw 1.0) (/ araw 255.0) araw)))
        (list :drop-shadow (float (first lens) 1.0) (float (second lens) 1.0)
              (max 0.0 (float (or (third lens) 0.0) 1.0))
              (float (first c) 1.0) (float (second c) 1.0) (float (third c) 1.0)
              (float a 1.0))))))

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

(defun %svg-path-tokens (d)
  "Tokenize an SVG path data string D into command chars and float numbers; NIL on an
unexpected character."
  (let ((out '()) (i 0) (n (length d)))
    (loop while (< i n) do
      (let ((c (char d i)))
        (cond ((member c '(#\Space #\Tab #\Newline #\Return #\,)) (incf i))
              ((alpha-char-p c) (push c out) (incf i))
              ((or (digit-char-p c) (member c '(#\- #\+ #\.)))
               (let ((j i))
                 (when (member (char d j) '(#\- #\+)) (incf j))
                 (loop while (and (< j n) (or (digit-char-p (char d j)) (char= (char d j) #\.)))
                       do (incf j))
                 (let ((v (%filter-number (subseq d i j))))
                   (unless v (return-from %svg-path-tokens nil))
                   (push (float v 1.0) out))
                 (setf i j)))
              (t (return-from %svg-path-tokens nil)))))
    (nreverse out)))

(defun %svg-path-polygon (d)
  "Flatten an SVG path (the straight-line M/m L/l H/h V/v Z/z subset, absolute and
relative) to a polygon point list ((x . y) ... in px).  NIL if a curve/arc command
 (C/S/Q/T/A) appears, so an unsupported path degrades to no clip rather than a wrong one."
  (let ((toks (%svg-path-tokens d)))
    (unless toks (return-from %svg-path-polygon nil))
    (let ((pts '()) (cx 0.0) (cy 0.0) (sx 0.0) (sy 0.0) (cmd nil) (started nil))
      (macrolet ((num () `(let ((v (pop toks)))
                            (unless (numberp v) (return-from %svg-path-polygon nil)) v)))
        (loop while toks do
          (when (characterp (car toks)) (setf cmd (pop toks)))
          (unless cmd (return-from %svg-path-polygon nil))
          (case cmd
            ((#\M #\m)
             (let ((x (num)) (y (num)))
               (if (and started (char= cmd #\m)) (setf cx (+ cx x) cy (+ cy y))
                   (setf cx x cy y)))
             (setf sx cx sy cy started t)
             (push (cons cx cy) pts)
             (setf cmd (if (char= cmd #\m) #\l #\L)))   ; extra pairs after M are L
            ((#\L #\l)
             (let ((x (num)) (y (num)))
               (if (char= cmd #\l) (setf cx (+ cx x) cy (+ cy y)) (setf cx x cy y)))
             (push (cons cx cy) pts))
            ((#\H #\h)
             (let ((x (num))) (setf cx (if (char= cmd #\h) (+ cx x) x)))
             (push (cons cx cy) pts))
            ((#\V #\v)
             (let ((y (num))) (setf cy (if (char= cmd #\v) (+ cy y) y)))
             (push (cons cx cy) pts))
            ((#\Z #\z) (setf cx sx cy sy))
            (t (return-from %svg-path-polygon nil)))))
      (let ((pts (nreverse pts)))
        (when (>= (length pts) 3) (list* :polygon pts))))))

(defun parse-clip-path (value fs)
  "Parse a CSS clip-path basic-shape value (CSS Masking 1 §clip-path) into the
CSTYLE-CLIP-PATH form.  Supports inset()/circle()/ellipse()/polygon(); an unknown
value, url(), or none yields NIL (no clip)."
  (let* ((raw (css-trim value))
         (s (ascii-downcase raw)))
    (cond
      ((zerop (length s)) nil)
      ((member s '("none" "initial" "unset" "revert" "inherit") :test #'string=) nil)
      ;; path( [<fill-rule>,]? <string> ) — SVG path data (case-sensitive, so parse the
      ;; ORIGINAL value).  Only the straight-line subset is flattened to a polygon.
      ((and (>= (length s) 5) (string= (subseq s 0 5) "path("))
       (let ((lp (position #\( raw)) (rp (position #\) raw :from-end t)))
         (when (and lp rp (< lp rp))
           (let* ((inner (css-trim (subseq raw (1+ lp) rp)))
                  (comma (position #\, inner))
                  (inner (if (and comma
                                  (member (css-trim (subseq inner 0 comma))
                                          '("nonzero" "evenodd") :test #'string-equal))
                             (css-trim (subseq inner (1+ comma))) inner))
                  (dstr (if (and (>= (length inner) 2)
                                 (member (char inner 0) '(#\' #\"))
                                 (char= (char inner (1- (length inner))) (char inner 0)))
                            (subseq inner 1 (1- (length inner))) inner)))
             (%svg-path-polygon dstr)))))
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