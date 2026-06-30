;;;; src/css/style.lisp — the cascade + computed style.
;;;;
;;;; Applies a UA stylesheet + author stylesheet(s) + inline style attributes to
;;;; a DOM, producing a computed CSTYLE per element (resolved lengths in px,
;;;; colors as (r g b a), inheritance handled).  A focused property set, enough
;;;; for block/inline layout + paint.
(in-package #:weft.css)

(defstruct cstyle
  (display "inline") (color '(0 0 0 1.0)) background
  (font-size 16.0) (font-weight 400) (line-height 1.2)
  (width :auto) (height :auto)
  (margin-top 0.0) (margin-right 0.0) (margin-bottom 0.0) (margin-left 0.0)
  (padding-top 0.0) (padding-right 0.0) (padding-bottom 0.0) (padding-left 0.0)
  (border-top-width 0.0) (border-right-width 0.0) (border-bottom-width 0.0) (border-left-width 0.0)
  ;; per-edge border colors (NIL = fall back to BORDER-COLOR) and styles
  ;; (NIL = treat as solid; "none"/"hidden" suppress the edge).
  (border-top-color nil) (border-right-color nil) (border-bottom-color nil) (border-left-color nil)
  (border-top-style nil) (border-right-style nil) (border-bottom-style nil) (border-left-style nil)
  (border-color '(0 0 0 1.0)) (text-align "left") (white-space "normal")
  (text-decoration nil) (list-style "disc")
  (max-width :none) (min-width 0.0) (margin-left-auto nil) (margin-right-auto nil)
  (float "none") (clear "none") (position "static") (box-sizing "content-box") (overflow "visible")
  (flex-direction "row") (justify-content "flex-start") (align-items "stretch")
  (flex-wrap "nowrap") (flex-grow 0.0) (flex-shrink 1.0) (flex-basis "auto") (gap 0.0)
  (top :auto) (left :auto) (right :auto) (bottom :auto) (z-index 0)
  (bg-gradient nil)   ; (dir from-rgba to-rgba), dir :vertical | :horizontal
  (bg-image nil)      ; raw url() string of a background image (data: URI), decoded at paint
  (bg-repeat "repeat") ; repeat | repeat-x | repeat-y | no-repeat
  (bg-position nil)   ; ((xval xunit) (yval yunit)) or NIL = 0,0
  (bg-attachment "scroll") ; scroll | fixed (fixed images are not painted; see paint)
  (min-height 0.0) (max-height :none)
  (content nil))      ; generated-content string for ::before/::after (NIL = no box)

(defparameter *inherited* '(:color :font-size :font-weight :line-height :text-align :white-space))

;;; ---- UA defaults --------------------------------------------------------
(defparameter *block-tags*
  '("html" "body" "div" "p" "h1" "h2" "h3" "h4" "h5" "h6" "ul" "ol" "li"
    "section" "article" "header" "footer" "nav" "aside" "main" "figure"
    "blockquote" "pre" "table" "tr" "form" "hr" "address" "dl" "dt" "dd"))
(defparameter *none-tags* '("head" "title" "meta" "link" "style" "script" "base"))

(defun ua-style (tag parent-cs)
  "UA-default CSTYLE for TAG, inheriting from PARENT-CS."
  (let ((cs (make-cstyle)))
    ;; inherit
    (when parent-cs
      (setf (cstyle-color cs) (cstyle-color parent-cs)
            (cstyle-font-size cs) (cstyle-font-size parent-cs)
            (cstyle-font-weight cs) (cstyle-font-weight parent-cs)
            (cstyle-line-height cs) (cstyle-line-height parent-cs)
            (cstyle-text-align cs) (cstyle-text-align parent-cs)
            (cstyle-white-space cs) (cstyle-white-space parent-cs)))
    (cond ((member tag *none-tags* :test #'string=) (setf (cstyle-display cs) "none"))
          ((string= tag "li") (setf (cstyle-display cs) "list-item"))
          ((string= tag "table") (setf (cstyle-display cs) "table"))
          ((string= tag "tr") (setf (cstyle-display cs) "table-row"))
          ((member tag '("td" "th") :test #'string=) (setf (cstyle-display cs) "table-cell"))
          ((member tag '("thead" "tbody" "tfoot") :test #'string=) (setf (cstyle-display cs) "table-row-group"))
          ((member tag *block-tags* :test #'string=) (setf (cstyle-display cs) "block"))
          (t (setf (cstyle-display cs) "inline")))
    (when (string= tag "th") (setf (cstyle-font-weight cs) 700 (cstyle-text-align cs) "center"))
    (when (member tag '("td" "th") :test #'string=) (set-padding cs 2.0))
    ;; a few UA margins / sizes
    (cond
      ((string= tag "body") (set-margin cs 8.0))
      ((member tag '("p" "ul" "ol" "blockquote" "dl") :test #'string=) (setf (cstyle-margin-top cs) 16.0 (cstyle-margin-bottom cs) 16.0))
      ((string= tag "h1") (setf (cstyle-font-size cs) 32.0 (cstyle-font-weight cs) 700 (cstyle-margin-top cs) 21.0 (cstyle-margin-bottom cs) 21.0))
      ((string= tag "h2") (setf (cstyle-font-size cs) 24.0 (cstyle-font-weight cs) 700 (cstyle-margin-top cs) 20.0 (cstyle-margin-bottom cs) 20.0))
      ((string= tag "h3") (setf (cstyle-font-size cs) 19.0 (cstyle-font-weight cs) 700 (cstyle-margin-top cs) 18.0 (cstyle-margin-bottom cs) 18.0))
      ((member tag '("h4" "h5" "h6") :test #'string=) (setf (cstyle-font-weight cs) 700 (cstyle-margin-top cs) 16.0 (cstyle-margin-bottom cs) 16.0))
      ((member tag '("b" "strong") :test #'string=) (setf (cstyle-font-weight cs) 700))
      ((member tag '("a") :test #'string=) (setf (cstyle-color cs) '(0 0 238 1.0) (cstyle-text-decoration cs) '("underline")))
      ((string= tag "img") (setf (cstyle-display cs) "inline-block" (cstyle-background cs) '(228 228 232 1.0)
                                 (cstyle-border-top-width cs) 1.0 (cstyle-border-right-width cs) 1.0
                                 (cstyle-border-bottom-width cs) 1.0 (cstyle-border-left-width cs) 1.0
                                 (cstyle-border-color cs) '(170 170 180 1.0) (cstyle-color cs) '(110 110 120 1.0)))
      ((string= tag "li") (setf (cstyle-margin-left cs) 24.0))
      ((string= tag "pre") (setf (cstyle-white-space cs) "pre")))
    cs))

(defun set-margin (cs v) (setf (cstyle-margin-top cs) v (cstyle-margin-right cs) v
                               (cstyle-margin-bottom cs) v (cstyle-margin-left cs) v))
(defun set-padding (cs v) (setf (cstyle-padding-top cs) v (cstyle-padding-right cs) v
                                (cstyle-padding-bottom cs) v (cstyle-padding-left cs) v))

;;; ---- value resolution ---------------------------------------------------
(defun resolve-len (text font-size &optional (auto-ok nil))
  "Resolve a length string to px (float), or :auto, or NIL if unparseable."
  (let ((tt (string-downcase (string-trim '(#\Space) text))))
    (cond
      ((and auto-ok (string= tt "auto")) :auto)
      ((string= tt "0") 0.0)
      (t (let ((v (parse-value "length" tt)))
           (if (and (listp v) (= 2 (length v)))
               (let ((num (float (first v))) (unit (second v)))
                 (cond ((string= unit "px") num)
                       ((string= unit "em") (* num font-size))
                       ((string= unit "rem") (* num 16.0))
                       ;; absolute units at the CSS reference 96px/in
                       ((string= unit "in") (* num 96.0))
                       ((string= unit "cm") (* num 37.795))    ; 96/2.54
                       ((string= unit "mm") (* num 3.7795))    ; 96/25.4
                       ((string= unit "q")  (* num 0.94488))   ; 96/101.6 (quarter-mm)
                       ((string= unit "pt") (* num 1.33333))   ; 96/72
                       ((string= unit "pc") (* num 16.0))      ; 12pt
                       ((member unit '("" ) :test #'string=) num)
                       (t num)))   ; treat unknown abs units as px-ish
               nil))))))

(defun line-height-multiplier (value font-size)
  "Parse a line-height VALUE into a multiplier of FONT-SIZE (weft stores
line-height as a number that LAYOUT multiplies by font-size).  A bare <number>
IS the multiplier; `normal` -> 1.2; a <percentage> -> its fraction; a <length>
-> length/font-size (CSS 2.1 10.8.1).  Returns NIL when unparseable."
  (let ((tt (string-downcase (string-trim '(#\Space) value))))
    (cond ((string= tt "normal") 1.2)
          ((and (plusp (length tt)) (char= (char tt (1- (length tt))) #\%))
           (let ((n (ignore-errors (read-from-string (subseq tt 0 (1- (length tt)))))))
             (when (realp n) (/ (float n) 100.0))))
          (t (let ((n (ignore-errors (let ((*read-eval* nil)) (read-from-string tt)))))
               (if (realp n)
                   (float n)
                   (let ((px (resolve-len tt font-size)))
                     (when (and (numberp px) (plusp font-size)) (/ px font-size)))))))))

(defun parse-size (text font-size auto-ok)
  "Parse a width/height value -> px number | :auto | (:percent N) | NIL."
  (let ((tt (string-downcase (string-trim '(#\Space) text))))
    (cond
      ((and auto-ok (string= tt "auto")) :auto)
      ((and (plusp (length tt)) (char= (char tt (1- (length tt))) #\%))
       (let ((n (ignore-errors (read-from-string (subseq tt 0 (1- (length tt))))))) (when (numberp n) (list :percent (float n)))))
      (t (resolve-len tt font-size)))))

(defun resolve-size (spec avail)
  "Resolve a parse-size result against AVAIL (containing-block px).  :auto/NIL -> NIL."
  (cond ((numberp spec) spec)
        ((and (consp spec) (eq (first spec) :percent)) (* avail (/ (second spec) 100.0)))
        (t nil)))

(defun resolve-height (spec avail-h)
  "Resolve a height parse-size SPEC against the containing-block height AVAIL-H
per CSS 2.1 10.5: a percentage resolves only when AVAIL-H is a definite number;
otherwise it computes to auto (NIL).  A plain px number resolves to itself."
  (cond ((numberp spec) spec)
        ((and (consp spec) (eq (first spec) :percent) (numberp avail-h))
         (* avail-h (/ (second spec) 100.0)))
        (t nil)))

(defun resolve-min-height (spec avail-h)
  "CSS 2.1 10.7 min-height: a length resolves to itself; a percentage resolves
against AVAIL-H if definite, else computes to 0."
  (cond ((numberp spec) spec)
        ((and (consp spec) (eq (first spec) :percent))
         (if (numberp avail-h) (* avail-h (/ (second spec) 100.0)) 0.0))
        (t 0.0)))

(defun resolve-max-height (spec avail-h)
  "CSS 2.1 10.7 max-height: a length resolves to itself; a percentage resolves
against AVAIL-H if definite, else computes to none (NIL = no ceiling)."
  (cond ((numberp spec) spec)
        ((and (consp spec) (eq (first spec) :percent) (numberp avail-h))
         (* avail-h (/ (second spec) 100.0)))
        (t nil)))

(defun resolve-color (text)
  (let ((v (parse-value "color" text))) (if (and (listp v) (>= (length v) 3)) v nil)))

(defun parse-linear-gradient (value)
  "Parse a simple 2-stop linear-gradient(...) -> (dir from-rgba to-rgba), or NIL."
  (let* ((s (string-downcase (string-trim '(#\Space) value)))
         (p (search "linear-gradient(" s)))
    (when p
      (let* ((open (+ p (length "linear-gradient(")))
             (close (position #\) s :from-end t))
             (inner (and close (> close open) (subseq s open close))))
        (when inner
          (let* ((parts (mapcar (lambda (x) (string-trim '(#\Space) x)) (comma-split-top inner)))
                 (dir :vertical) (colors parts))
            (when (and parts (or (search "deg" (first parts)) (search "to " (first parts))))
              (let ((d (first parts)))
                (setf dir (cond ((or (search "to right" d) (search "to left" d) (search "90deg" d) (search "270deg" d)) :horizontal)
                                (t :vertical))))
              (setf colors (rest parts)))
            (let ((cs (remove nil (mapcar #'resolve-color colors))))
              (when (>= (length cs) 2) (list dir (first cs) (car (last cs)))))))))))

(defun comma-split-top (s)
  "Split S on commas not inside parens."
  (let ((out '()) (depth 0) (start 0))
    (dotimes (i (length s))
      (case (char s i) (#\( (incf depth)) (#\) (decf depth))
        (#\, (when (zerop depth) (push (subseq s start i) out) (setf start (1+ i))))))
    (push (subseq s start) out) (nreverse out)))

(defun parse-content (value)
  "Parse a 'content' value into a generated string, or NIL (none/normal/no box).
Handles 'string' / \"string\" (incl. an empty string -> an empty but present
box, marked by the empty string) and concatenated string tokens; non-string
values (attr(), counters, images) yield an empty box."
  (let ((v (string-trim '(#\Space #\Tab #\Newline) value)))
    (cond ((or (string-equal v "none") (string-equal v "normal")) nil)
          ((and (plusp (length v)) (member (char v 0) '(#\' #\")))
           (with-output-to-string (out)
             (let ((i 0) (n (length v)))
               (loop while (< i n) do
                 (let ((q (char v i)))
                   (if (member q '(#\' #\"))
                       (let ((j (1+ i)))
                         (loop while (and (< j n) (char/= (char v j) q)) do
                           (when (and (char= (char v j) #\\) (< (1+ j) n)) (incf j))
                           (write-char (char v j) out) (incf j))
                         (setf i (1+ j)))
                       (incf i)))))))
          (t ""))))   ; attr()/counter()/url() -> present-but-empty box

(defun apply-font-shorthand (cs value parent-cs)
  "Best-effort `font` shorthand: [style|variant|weight ...] <size>[/<line-height>]
<family>.  Sets font-size (the pivot token: the first <length>/<percentage>),
plus optional line-height after '/', and any leading style/weight keywords.
Ignores the system-font keywords (caption/icon/...)."
  (let* ((v (string-trim '(#\Space #\Tab #\Newline) value))
         (toks (remove "" (split-ws v) :test #'string=)))
    (when (and toks (not (member (string-downcase (first toks))
                                 '("caption" "icon" "menu" "message-box" "small-caption" "status-bar"
                                   "inherit" "initial" "unset") :test #'string=)))
      ;; the size token is the first <length>/<percentage>: a digit plus a unit,
      ;; '%' or '/'.  A bare integer (e.g. 700) is a weight, not a size.
      (let ((size-pos (position-if (lambda (t0)
                                     (and (some #'digit-char-p t0)
                                          (some (lambda (c) (or (alpha-char-p c) (char= c #\%) (char= c #\/))) t0)))
                                   toks)))
        (when size-pos
          ;; preceding keywords: style / weight
          (dolist (k (subseq toks 0 size-pos))
            (let ((kl (string-downcase k)))
              (cond ((member kl '("bold" "bolder") :test #'string=) (setf (cstyle-font-weight cs) 700))
                    ((every #'digit-char-p kl) (setf (cstyle-font-weight cs) (parse-integer kl))))))
          ;; size[/line-height]
          (let* ((stok (nth size-pos toks)) (slash (position #\/ stok))
                 (size-s (if slash (subseq stok 0 slash) stok))
                 (lh-s (when slash (subseq stok (1+ slash))))
                 (base (if parent-cs (cstyle-font-size parent-cs) 16.0)))
            (cond ((search "%" size-s) (let ((p (parse-value "percentage" size-s)))
                                         (when (numberp p) (setf (cstyle-font-size cs) (* base (/ p 100.0))))))
                  (t (let ((px (resolve-len size-s base))) (when px (setf (cstyle-font-size cs) px)))))
            (when (and lh-s (plusp (length lh-s)))
              (let ((m (line-height-multiplier lh-s (cstyle-font-size cs))))
                (when m (setf (cstyle-line-height cs) m))))))))))

(defun split-ws (s)
  "Split S on runs of ASCII whitespace."
  (let ((out '()) (start nil) (n (length s)))
    (dotimes (i n)
      (let ((ws (member (char s i) '(#\Space #\Tab #\Newline #\Return #\Page))))
        (cond ((and ws start) (push (subseq s start i) out) (setf start nil))
              ((not (or ws start)) (setf start i)))))
    (when start (push (subseq s start n) out))
    (nreverse out)))

(defun apply-decl (cs prop value parent-cs)
  "Apply one declaration to CSTYLE CS (best-effort)."
  (let ((fs (cstyle-font-size cs)))
    (macrolet ((len (&optional auto) `(resolve-len value fs ,auto)))
      (cond
        ((string= prop "display") (setf (cstyle-display cs) (string-downcase (string-trim '(#\Space) value))))
        ((string= prop "content") (setf (cstyle-content cs) (parse-content value)))
        ((string= prop "color") (let ((c (resolve-color value))) (when c (setf (cstyle-color cs) c))))
        ((member prop '("background-color" "background" "background-image") :test #'string=)
         (let ((grad (parse-linear-gradient value))
               (url (extract-css-url value))
               (tok (string-downcase (string-trim '(#\Space) (first-token value)))))
           (cond (grad (setf (cstyle-bg-gradient cs) grad))
                 ;; `none`/`transparent` clear any background set by an earlier rule
                 ((member tok '("none" "transparent") :test #'string=)
                  (setf (cstyle-background cs) nil (cstyle-bg-gradient cs) nil (cstyle-bg-image cs) nil))
                 (t (let ((c (resolve-color (first-token value)))) (when c (setf (cstyle-background cs) c)))))
           ;; capture a url() image (data: URI) from `background`/`background-image`
           (when url
             (setf (cstyle-bg-image cs) url)
             (when (string= prop "background")
               ;; pull repeat/attachment keywords out of the shorthand
               (let ((toks (css-background-tokens value)))
                 (when (member "fixed" toks :test #'string=) (setf (cstyle-bg-attachment cs) "fixed"))
                 (let ((r (find-if (lambda (tk) (member tk '("repeat" "repeat-x" "repeat-y" "no-repeat") :test #'string=)) toks)))
                   (when r (setf (cstyle-bg-repeat cs) r)))
                 ;; pull a background-position (length/percent/keyword pair, e.g.
                 ;; the `1px 0` in Acid2's eye images) out of the shorthand
                 (let ((postoks (remove-if-not #'bg-position-token-p toks)))
                   (when postoks
                     (let ((v (parse-value "background-position"
                                           (format nil "~{~a~^ ~}" postoks))))
                       (when (and (consp v) (not (eq v :invalid)))
                         (setf (cstyle-bg-position cs) v))))))))))
        ((string= prop "background-repeat")
         (let ((v (parse-value "background-repeat" value))) (when (stringp v) (setf (cstyle-bg-repeat cs) v))))
        ((string= prop "background-position")
         (let ((v (parse-value "background-position" value))) (when (and (consp v) (not (eq v :invalid))) (setf (cstyle-bg-position cs) v))))
        ((string= prop "background-attachment")
         (setf (cstyle-bg-attachment cs) (string-downcase (string-trim '(#\Space) value))))
        ((string= prop "font-size")
         (let ((base (if parent-cs (cstyle-font-size parent-cs) 16.0)))
           (cond ((search "%" value) (let ((p (parse-value "percentage" value))) (when (numberp p) (setf (cstyle-font-size cs) (* base (/ p 100.0))))))
                 (t (let ((px (resolve-len value base))) (when px (setf (cstyle-font-size cs) px)))))))
        ((string= prop "font") (apply-font-shorthand cs value parent-cs))
        ((string= prop "font-weight")
         (setf (cstyle-font-weight cs)
               (cond ((string-equal value "bold") 700) ((string-equal value "normal") 400)
                     ((ignore-errors (parse-integer (string-trim '(#\Space) value)))) (t 400))))
        ((string= prop "line-height") (let ((m (line-height-multiplier value (cstyle-font-size cs)))) (when m (setf (cstyle-line-height cs) m))))
        ((string= prop "text-align") (setf (cstyle-text-align cs) (string-downcase (string-trim '(#\Space) value))))
        ((member prop '("text-decoration" "text-decoration-line") :test #'string=)
         (let ((v (parse-value "text-decoration" value))) (when (listp v) (setf (cstyle-text-decoration cs) v))))
        ((string= prop "list-style-type")
         (let ((v (parse-value "list-style-type" value))) (when (stringp v) (setf (cstyle-list-style cs) v))))
        ((string= prop "white-space") (setf (cstyle-white-space cs) (string-downcase (string-trim '(#\Space) value))))
        ((string= prop "width") (let ((w (parse-size value fs t))) (when w (setf (cstyle-width cs) w))))
        ((string= prop "height") (let ((h (parse-size value fs t))) (when h (setf (cstyle-height cs) h))))
        ((string= prop "max-width") (if (string-equal (string-trim '(#\Space) value) "none") (setf (cstyle-max-width cs) :none)
                                        (let ((w (parse-size value fs nil))) (when w (setf (cstyle-max-width cs) w)))))
        ((string= prop "min-width") (let ((w (parse-size value fs nil))) (when w (setf (cstyle-min-width cs) w))))
        ((string= prop "min-height") (let ((h (parse-size value fs nil)))   ; px or (:percent N)
                                       (when (or (numberp h) (consp h)) (setf (cstyle-min-height cs) h))))
        ((string= prop "max-height") (if (string-equal (string-trim '(#\Space) value) "none") (setf (cstyle-max-height cs) :none)
                                         (let ((h (parse-size value fs nil)))   ; px or (:percent N)
                                           (when (or (numberp h) (consp h)) (setf (cstyle-max-height cs) h)))))
        ((string= prop "float")
         ;; `float:inherit` (used by Acid2's smile) copies the parent's computed
         ;; float; otherwise parse the keyword normally.
         (if (string-equal (string-trim '(#\Space #\Tab #\Newline) value) "inherit")
             (when parent-cs (setf (cstyle-float cs) (cstyle-float parent-cs)))
             (let ((v (parse-value "float" value))) (when (stringp v) (setf (cstyle-float cs) v)))))
        ((string= prop "clear") (setf (cstyle-clear cs) (string-downcase (string-trim '(#\Space) value))))
        ((string= prop "position") (let ((v (parse-value "position" value))) (when (stringp v) (setf (cstyle-position cs) v))))
        ((string= prop "box-sizing") (let ((v (parse-value "box-sizing" value))) (when (stringp v) (setf (cstyle-box-sizing cs) v))))
        ((member prop '("overflow" "overflow-x" "overflow-y") :test #'string=)
         (let ((v (parse-value "overflow" value))) (when (stringp v) (setf (cstyle-overflow cs) v))))
        ((string= prop "flex-direction") (setf (cstyle-flex-direction cs) (string-downcase (string-trim '(#\Space) value))))
        ((string= prop "flex-wrap") (setf (cstyle-flex-wrap cs) (string-downcase (string-trim '(#\Space) value))))
        ((string= prop "justify-content") (setf (cstyle-justify-content cs) (string-downcase (string-trim '(#\Space) value))))
        ((string= prop "align-items") (setf (cstyle-align-items cs) (string-downcase (string-trim '(#\Space) value))))
        ((member prop '("gap" "column-gap" "row-gap") :test #'string=) (let ((v (len))) (when v (setf (cstyle-gap cs) v))))
        ((string= prop "flex-grow") (let ((v (ignore-errors (read-from-string value)))) (when (numberp v) (setf (cstyle-flex-grow cs) (float v)))))
        ((string= prop "flex-shrink") (let ((v (ignore-errors (read-from-string value)))) (when (numberp v) (setf (cstyle-flex-shrink cs) (float v)))))
        ((string= prop "flex-basis") (setf (cstyle-flex-basis cs) (string-downcase (string-trim '(#\Space) value))))
        ((member prop '("top" "left" "right" "bottom") :test #'string=)
         (let* ((v (if (string-equal (string-trim '(#\Space) value) "auto") :auto (len)))
                (slot (cond ((string= prop "top") '(setf cstyle-top)) ((string= prop "left") '(setf cstyle-left))
                            ((string= prop "right") '(setf cstyle-right)) (t '(setf cstyle-bottom)))))
           (declare (ignore slot))
           (when (or (eq v :auto) (numberp v))
             (cond ((string= prop "top") (setf (cstyle-top cs) v)) ((string= prop "left") (setf (cstyle-left cs) v))
                   ((string= prop "right") (setf (cstyle-right cs) v)) (t (setf (cstyle-bottom cs) v))))))
        ((string= prop "z-index") (let ((v (parse-value "z-index" value)))
                                    (when (and (listp v) (integerp (first v))) (setf (cstyle-z-index cs) (first v)))))
        ((string= prop "flex")
         (let ((v (parse-value "flex" value)))
           (when (and (listp v) (= 3 (length v)))
             (setf (cstyle-flex-grow cs) (float (first v)) (cstyle-flex-shrink cs) (float (second v))
                   (cstyle-flex-basis cs) (string-downcase (string (third v)))))))
        ((string= prop "margin")
         (let ((parts (split-tokens (string-trim '(#\Space) value))))
           ;; horizontal auto -> centering flags (e.g. "0 auto")
           (when (>= (length parts) 2)
             (when (string-equal (second parts) "auto") (setf (cstyle-margin-left-auto cs) t (cstyle-margin-right-auto cs) t)))
           (when (and (= (length parts) 1) (string-equal (first parts) "auto"))
             (setf (cstyle-margin-left-auto cs) t (cstyle-margin-right-auto cs) t))
           (apply-box value fs cs #'(setf cstyle-margin-top) #'(setf cstyle-margin-right) #'(setf cstyle-margin-bottom) #'(setf cstyle-margin-left))))
        ((string= prop "padding") (apply-box value fs cs #'(setf cstyle-padding-top) #'(setf cstyle-padding-right) #'(setf cstyle-padding-bottom) #'(setf cstyle-padding-left)))
        ((string= prop "margin-top") (let ((v (len))) (when v (setf (cstyle-margin-top cs) v))))
        ((string= prop "margin-right") (if (string-equal (string-trim '(#\Space) value) "auto") (setf (cstyle-margin-right-auto cs) t) (let ((v (len))) (when v (setf (cstyle-margin-right cs) v)))))
        ((string= prop "margin-bottom") (let ((v (len))) (when v (setf (cstyle-margin-bottom cs) v))))
        ((string= prop "margin-left") (if (string-equal (string-trim '(#\Space) value) "auto") (setf (cstyle-margin-left-auto cs) t) (let ((v (len))) (when v (setf (cstyle-margin-left cs) v)))))
        ((string= prop "padding-top") (let ((v (len))) (when v (setf (cstyle-padding-top cs) v))))
        ((string= prop "padding-right") (let ((v (len))) (when v (setf (cstyle-padding-right cs) v))))
        ((string= prop "padding-bottom") (let ((v (len))) (when v (setf (cstyle-padding-bottom cs) v))))
        ((string= prop "padding-left") (let ((v (len))) (when v (setf (cstyle-padding-left cs) v))))
        ((string= prop "border-color")
         (apply-color-box value cs #'(setf cstyle-border-top-color) #'(setf cstyle-border-right-color)
                          #'(setf cstyle-border-bottom-color) #'(setf cstyle-border-left-color)))
        ((string= prop "border-top-color") (let ((c (resolve-border-color value cs))) (when c (setf (cstyle-border-top-color cs) c))))
        ((string= prop "border-right-color") (let ((c (resolve-border-color value cs))) (when c (setf (cstyle-border-right-color cs) c))))
        ((string= prop "border-bottom-color") (let ((c (resolve-border-color value cs))) (when c (setf (cstyle-border-bottom-color cs) c))))
        ((string= prop "border-left-color") (let ((c (resolve-border-color value cs))) (when c (setf (cstyle-border-left-color cs) c))))
        ((string= prop "border-style")
         (apply-style-box value cs #'(setf cstyle-border-top-style) #'(setf cstyle-border-right-style)
                          #'(setf cstyle-border-bottom-style) #'(setf cstyle-border-left-style)))
        ((string= prop "border-top-style") (when (border-style-token-p value) (setf (cstyle-border-top-style cs) (string-downcase (string-trim '(#\Space) value)))))
        ((string= prop "border-right-style") (when (border-style-token-p value) (setf (cstyle-border-right-style cs) (string-downcase (string-trim '(#\Space) value)))))
        ((string= prop "border-bottom-style") (when (border-style-token-p value) (setf (cstyle-border-bottom-style cs) (string-downcase (string-trim '(#\Space) value)))))
        ((string= prop "border-left-style") (when (border-style-token-p value) (setf (cstyle-border-left-style cs) (string-downcase (string-trim '(#\Space) value)))))
        ((member prop '("border" "border-top" "border-bottom" "border-left" "border-right") :test #'string=)
         (apply-border cs prop value fs))
        ((string= prop "border-width") (let ((v (len))) (when v (setf (cstyle-border-top-width cs) v (cstyle-border-right-width cs) v (cstyle-border-bottom-width cs) v (cstyle-border-left-width cs) v))))))))

(defun first-token (s) (let ((p (position #\Space (string-trim '(#\Space) s)))) (if p (subseq (string-trim '(#\Space) s) 0 p) (string-trim '(#\Space) s))))

(defun extract-css-url (value)
  "Return the URL inside the first url(...) in VALUE (surrounding quotes stripped),
or NIL.  Stops at the first ')' — data: URIs use base64/percent-encoding and never
contain a literal ')'.  The URL is returned raw (e.g. still %-encoded)."
  (let ((p (search "url(" value :test #'char-equal)))
    (when p
      (let* ((start (+ p 4))
             (end (position #\) value :start start)))
        (when end
          (let ((inner (string-trim '(#\Space #\Tab #\Newline #\Return) (subseq value start end))))
            (when (and (>= (length inner) 2)
                       (let ((c0 (char inner 0)))
                         (and (or (char= c0 #\") (char= c0 #\')) (char= c0 (char inner (1- (length inner)))))))
              (setf inner (subseq inner 1 (1- (length inner)))))
            (when (plusp (length inner)) inner)))))))

(defun bg-position-token-p (tok)
  "True when TOK could be a background-position component (a length/percentage or
a position keyword), so it can be pulled out of a `background` shorthand."
  (or (member tok '("left" "right" "top" "bottom" "center") :test #'string=)
      (and (plusp (length tok))
           (let ((c (char tok 0)))
             (or (digit-char-p c) (char= c #\.) (char= c #\-) (char= c #\+))))))

(defun css-background-tokens (value)
  "Whitespace-split a `background` shorthand VALUE into lowercase tokens, with the
url(...) chunk removed (so its contents aren't mistaken for keywords)."
  (let* ((p (search "url(" value :test #'char-equal))
         (v (if p
                (let ((end (position #\) value :start (+ p 4))))
                  (if end (concatenate 'string (subseq value 0 p) " " (subseq value (1+ end))) value))
                value)))
    (remove "" (split-ws (string-downcase v)) :test #'string=)))

(defun split-tokens (s)
  (remove "" (loop with start = 0 for i from 0 to (length s)
                   when (or (= i (length s)) (char= (char s i) #\Space))
                     collect (prog1 (subseq s start i) (setf start (1+ i)))) :test #'string=))

(defun apply-box (value fs cs top right bottom left)
  "Apply a 1-4 value box shorthand (top right bottom left CSS order)."
  (let* ((parts (split-tokens (string-trim '(#\Space) value)))
         (vals (mapcar (lambda (p) (or (resolve-len p fs) 0.0)) parts)))
    (destructuring-bind (&optional (a 0.0) (b a) (c a) (d b)) vals
      (funcall top a cs) (funcall right b cs) (funcall bottom c cs) (funcall left d cs))))

(defparameter *border-styles*
  '("none" "hidden" "solid" "dotted" "dashed" "double" "groove" "ridge" "inset" "outset"))
(defun border-style-token-p (tok)
  (member (string-trim '(#\Space) tok) *border-styles* :test #'string-equal))
(defun border-edge-painted-p (style)
  "True when an edge with STYLE (string or NIL) paints; NIL defaults to solid."
  (not (and style (member style '("none" "hidden") :test #'string-equal))))

(defun apply-style-box (value cs top right bottom left)
  "Apply a 1-4 value border-style shorthand (top right bottom left CSS order)."
  (let* ((parts (remove-if-not #'border-style-token-p (split-tokens (string-trim '(#\Space) value))))
         (vals (mapcar #'string-downcase parts)))
    (when vals
      (destructuring-bind (&optional (a "none") (b a) (c a) (d b)) vals
        (funcall top a cs) (funcall right b cs) (funcall bottom c cs) (funcall left d cs)))))

(defun resolve-border-color (tok cs)
  "Resolve a border color token; \"currentcolor\" maps to the element's color."
  (if (string-equal (string-trim '(#\Space) tok) "currentcolor")
      (cstyle-color cs)
      (resolve-color tok)))

(defun apply-color-box (value cs top right bottom left)
  "Apply a 1-4 value border-color shorthand (top right bottom left CSS order).
Invalid/unparseable components are left as NIL (= fall back to BORDER-COLOR)."
  (let* ((parts (split-tokens (string-trim '(#\Space) value)))
         (vals (mapcar (lambda (p) (resolve-border-color p cs)) parts)))
    (when vals
      (destructuring-bind (&optional a (b a) (c a) (d b)) vals
        (funcall top a cs) (funcall right b cs) (funcall bottom c cs) (funcall left d cs)))))

(defun apply-border (cs prop value fs)
  (let ((w 0.0) (col nil) (sty nil))
    (dolist (tok (split-tokens value))
      (let ((px (resolve-len tok fs)))
        (cond ((numberp px) (setf w px))
              ((member tok '("thin" "medium" "thick") :test #'string-equal) (setf w (cond ((string-equal tok "thin") 1.0) ((string-equal tok "thick") 5.0) (t 3.0))))
              ((border-style-token-p tok) (setf sty (string-downcase tok)))
              ((resolve-border-color tok cs) (setf col (resolve-border-color tok cs))))))
    (flet ((setw (side) (case side (:t (setf (cstyle-border-top-width cs) w)) (:r (setf (cstyle-border-right-width cs) w))
                          (:b (setf (cstyle-border-bottom-width cs) w)) (:l (setf (cstyle-border-left-width cs) w))))
           (setc (side) (when col (case side (:t (setf (cstyle-border-top-color cs) col)) (:r (setf (cstyle-border-right-color cs) col))
                          (:b (setf (cstyle-border-bottom-color cs) col)) (:l (setf (cstyle-border-left-color cs) col)))))
           (sets (side) (when sty (case side (:t (setf (cstyle-border-top-style cs) sty)) (:r (setf (cstyle-border-right-style cs) sty))
                          (:b (setf (cstyle-border-bottom-style cs) sty)) (:l (setf (cstyle-border-left-style cs) sty))))))
      (cond ((string= prop "border") (mapc (lambda (s) (setw s) (setc s) (sets s)) '(:t :r :b :l)))
            ((string= prop "border-top") (setw :t) (setc :t) (sets :t))
            ((string= prop "border-bottom") (setw :b) (setc :b) (sets :b))
            ((string= prop "border-left") (setw :l) (setc :l) (sets :l))
            ((string= prop "border-right") (setw :r) (setc :r) (sets :r))))))

;;; ---- the cascade --------------------------------------------------------
(defun compute-styles (document stylesheet)
  "Compute a CSTYLE for every element under DOCUMENT, applying STYLESHEET (a list
of CSS-RULEs).  Returns a hash-table element->CSTYLE."
  (let ((styles (make-hash-table :test 'equal))
        ;; pre-parse selectors once, tagging rules with (match-cx pseudo spec order decls).
        ;; pseudo = NIL | :before | :after; match-cx is the cx to match (pseudo-element stripped).
        (rules (loop for r in stylesheet for order from 0
                     append (loop for cx in (parse-selector-list (css-rule-selector r))
                                  collect (multiple-value-bind (pe mcx) (cx-pseudo-element cx)
                                            (list mcx pe (specificity cx) order (css-rule-decls r)))))))
    (labels ((sort-matched (matched)
               (stable-sort (nreverse matched)
                            (lambda (x y) (or (spec< (first x) (first y))
                                              (and (equal (first x) (first y)) (< (second x) (second y)))))))
             (pseudo-style (parent-cs matched)
               "Build a CSTYLE for a ::before/::after box, or NIL if no content."
               (when matched
                 (let ((cs (make-cstyle :color (cstyle-color parent-cs) :font-size (cstyle-font-size parent-cs)
                                        :font-weight (cstyle-font-weight parent-cs) :line-height (cstyle-line-height parent-cs))))
                   (dolist (m (sort-matched matched))
                     (dolist (d (third m)) (apply-decl cs (css-decl-prop d) (css-decl-value d) parent-cs)))
                   (and (cstyle-content cs) cs))))
             (walk (n parent-cs)
               (when (eq (weft.html:dnode-kind n) :element)
                 (let* ((tag (string-downcase (weft.html:dnode-name n)))
                        (cs (ua-style tag parent-cs)))
                   ;; collect matching author rules, splitting element vs pseudo-element
                   (let ((matched '()) (m-before '()) (m-after '()))
                     (dolist (ru rules)
                       (destructuring-bind (cx pe spec order decls) ru
                         (when (match-complex (cx-compounds cx) (cx-combs cx) (1- (length (cx-compounds cx))) n)
                           (case pe
                             (:before (push (list spec order decls) m-before))
                             (:after  (push (list spec order decls) m-after))
                             (t       (push (list spec order decls) matched))))))
                     (dolist (m (sort-matched matched))
                       (dolist (d (third m)) (apply-decl cs (css-decl-prop d) (css-decl-value d) parent-cs)))
                     ;; inline style attribute (wins)
                     (let ((inline (el-attr n "style")))
                       (when inline
                         (dolist (pv (parse-inline inline))
                           (apply-decl cs (car pv) (cdr pv) parent-cs))))
                     (setf (gethash n styles) cs)
                     ;; generated content (computed after the element's own style is known)
                     (let ((bs (pseudo-style cs m-before)) (as (pseudo-style cs m-after)))
                       (when bs (setf (gethash (cons n :before) styles) bs))
                       (when as (setf (gethash (cons n :after) styles) as))))
                   (loop for c across (weft.html:dnode-children n) do (walk c cs))))))
      (loop for c across (weft.html:dnode-children document) do (walk c nil)))
    styles))

(defun spec< (a b)
  (cond ((< (first a) (first b)) t) ((> (first a) (first b)) nil)
        ((< (second a) (second b)) t) ((> (second a) (second b)) nil)
        (t (< (third a) (third b)))))

(defun parse-inline (s)
  "Parse an inline style attribute 'a:b; c:d' into ((a . b) ...)."
  (loop for chunk in (split-semi s)
        for cp = (position #\: chunk)
        when cp collect (cons (string-downcase (string-trim '(#\Space) (subseq chunk 0 cp)))
                              (string-trim '(#\Space) (subseq chunk (1+ cp))))))
(defun split-semi (s)
  (loop with start = 0 for i from 0 to (length s)
        when (or (= i (length s)) (char= (char s i) #\;))
          collect (prog1 (subseq s start i) (setf start (1+ i)))))
