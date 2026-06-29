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
  (border-color '(0 0 0 1.0)) (text-align "left") (white-space "normal")
  (text-decoration nil) (list-style "disc")
  (max-width :none) (min-width 0.0) (margin-left-auto nil) (margin-right-auto nil)
  (float "none") (clear "none") (position "static") (box-sizing "content-box")
  (flex-direction "row") (justify-content "flex-start") (align-items "stretch")
  (flex-wrap "nowrap") (flex-grow 0.0) (flex-shrink 1.0) (flex-basis "auto") (gap 0.0)
  (top :auto) (left :auto) (right :auto) (bottom :auto) (z-index 0)
  (bg-gradient nil))   ; (dir from-rgba to-rgba), dir :vertical | :horizontal

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
                       ((string= unit "pt") (* num 1.3333))
                       ((member unit '("" ) :test #'string=) num)
                       (t num)))   ; treat unknown abs units as px-ish
               nil))))))

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

(defun apply-decl (cs prop value parent-cs)
  "Apply one declaration to CSTYLE CS (best-effort)."
  (let ((fs (cstyle-font-size cs)))
    (macrolet ((len (&optional auto) `(resolve-len value fs ,auto)))
      (cond
        ((string= prop "display") (setf (cstyle-display cs) (string-downcase (string-trim '(#\Space) value))))
        ((string= prop "color") (let ((c (resolve-color value))) (when c (setf (cstyle-color cs) c))))
        ((member prop '("background-color" "background" "background-image") :test #'string=)
         (let ((grad (parse-linear-gradient value)))
           (if grad (setf (cstyle-bg-gradient cs) grad)
               (let ((c (resolve-color (first-token value)))) (when c (setf (cstyle-background cs) c))))))
        ((string= prop "font-size")
         (let ((base (if parent-cs (cstyle-font-size parent-cs) 16.0)))
           (cond ((search "%" value) (let ((p (parse-value "percentage" value))) (when (numberp p) (setf (cstyle-font-size cs) (* base (/ p 100.0))))))
                 (t (let ((px (resolve-len value base))) (when px (setf (cstyle-font-size cs) px)))))))
        ((string= prop "font-weight")
         (setf (cstyle-font-weight cs)
               (cond ((string-equal value "bold") 700) ((string-equal value "normal") 400)
                     ((ignore-errors (parse-integer (string-trim '(#\Space) value)))) (t 400))))
        ((string= prop "line-height") (let ((n (ignore-errors (read-from-string value)))) (when (numberp n) (setf (cstyle-line-height cs) (float n)))))
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
        ((string= prop "float") (let ((v (parse-value "float" value))) (when (stringp v) (setf (cstyle-float cs) v))))
        ((string= prop "clear") (setf (cstyle-clear cs) (string-downcase (string-trim '(#\Space) value))))
        ((string= prop "position") (let ((v (parse-value "position" value))) (when (stringp v) (setf (cstyle-position cs) v))))
        ((string= prop "box-sizing") (let ((v (parse-value "box-sizing" value))) (when (stringp v) (setf (cstyle-box-sizing cs) v))))
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
        ((string= prop "border-color") (let ((c (resolve-color value))) (when c (setf (cstyle-border-color cs) c))))
        ((member prop '("border" "border-top" "border-bottom" "border-left" "border-right") :test #'string=)
         (apply-border cs prop value fs))
        ((string= prop "border-width") (let ((v (len))) (when v (setf (cstyle-border-top-width cs) v (cstyle-border-right-width cs) v (cstyle-border-bottom-width cs) v (cstyle-border-left-width cs) v))))))))

(defun first-token (s) (let ((p (position #\Space (string-trim '(#\Space) s)))) (if p (subseq (string-trim '(#\Space) s) 0 p) (string-trim '(#\Space) s))))

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

(defun apply-border (cs prop value fs)
  (let ((w 0.0) (col nil))
    (dolist (tok (split-tokens value))
      (let ((px (resolve-len tok fs)))
        (cond ((numberp px) (setf w px))
              ((member tok '("thin" "medium" "thick") :test #'string-equal) (setf w (cond ((string-equal tok "thin") 1.0) ((string-equal tok "thick") 5.0) (t 3.0))))
              ((resolve-color tok) (setf col (resolve-color tok))))))
    (when col (setf (cstyle-border-color cs) col))
    (flet ((setw (side) (case side (:t (setf (cstyle-border-top-width cs) w)) (:r (setf (cstyle-border-right-width cs) w))
                          (:b (setf (cstyle-border-bottom-width cs) w)) (:l (setf (cstyle-border-left-width cs) w)))))
      (cond ((string= prop "border") (setw :t) (setw :r) (setw :b) (setw :l))
            ((string= prop "border-top") (setw :t)) ((string= prop "border-bottom") (setw :b))
            ((string= prop "border-left") (setw :l)) ((string= prop "border-right") (setw :r))))))

;;; ---- the cascade --------------------------------------------------------
(defun compute-styles (document stylesheet)
  "Compute a CSTYLE for every element under DOCUMENT, applying STYLESHEET (a list
of CSS-RULEs).  Returns a hash-table element->CSTYLE."
  (let ((styles (make-hash-table :test 'eq))
        ;; pre-parse selectors once, tagging rules with (specificity order decls)
        (rules (loop for r in stylesheet for order from 0
                     append (loop for cx in (parse-selector-list (css-rule-selector r))
                                  collect (list cx (specificity cx) order (css-rule-decls r))))))
    (labels ((walk (n parent-cs)
               (when (eq (weft.html:dnode-kind n) :element)
                 (let* ((tag (string-downcase (weft.html:dnode-name n)))
                        (cs (ua-style tag parent-cs)))
                   ;; collect matching author rules
                   (let ((matched '()))
                     (dolist (ru rules)
                       (destructuring-bind (cx spec order decls) ru
                         (when (match-complex (cx-compounds cx) (cx-combs cx) (1- (length (cx-compounds cx))) n)
                           (push (list spec order decls) matched))))
                     ;; sort by specificity then source order (ascending => later wins)
                     (setf matched (stable-sort (nreverse matched)
                                                (lambda (x y) (or (spec< (first x) (first y))
                                                                  (and (equal (first x) (first y)) (< (second x) (second y)))))))
                     (dolist (m matched)
                       (dolist (d (third m)) (apply-decl cs (css-decl-prop d) (css-decl-value d) parent-cs))))
                   ;; inline style attribute (wins)
                   (let ((inline (el-attr n "style")))
                     (when inline
                       (dolist (pv (parse-inline inline))
                         (apply-decl cs (car pv) (cdr pv) parent-cs))))
                   (setf (gethash n styles) cs)
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
