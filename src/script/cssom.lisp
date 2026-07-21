;;;; src/script/cssom.lisp — the CSSOM surface: getComputedStyle + element.style.
;;;;
;;;; weft already resolves the cascade into cstyle structs; getComputedStyle
;;;; hands those back to script as a read-only style object keyed by camelCase
;;;; property. element.style is a small live inline-declaration object that
;;;; writes back through the element's `style` attribute so it re-cascades.
(in-package #:weft.script)

;;; ---- value formatting -----------------------------------------------------
(defun num->css (v)
  "Serialize a CSS number per CSSOM: an integral value as an integer, otherwise
   the shortest round-tripping decimal.  Never leak Lisp float syntax (the `d0`
   exponent marker) into the JS/CSS string — binding *read-default-float-format*
   to double-float suppresses it."
  (if (and (realp v) (< (abs v) 1d15) (= v (truncate v)))
      (princ-to-string (truncate v))
      (let ((*read-default-float-format* 'double-float))
        (princ-to-string (float v 1d0)))))

(defun px (v)
  (cond ((eq v :auto) "auto") ((eq v :none) "none") ((eq v :normal) "normal")
        ((numberp v) (concatenate 'string (num->css v) "px"))
        ((stringp v) v) ((null v) "") (t (princ-to-string v))))

(defun round-channel (x)
  "Round an sRGB 8-bit channel value: nearest integer, ties away from zero
   (Chrome/CSSOM), unlike CL ROUND's ties-to-even (which yields 76 for 76.5
   where CSS Color serialization wants 77)."
  (let ((x (max 0 (min 255 x))))
    ;; 1e-6 absorbs double round-off so an exact half (e.g. 127.5 that computes as
    ;; 127.4999999) still rounds up as Chrome does; far below the 1.0 gap between
    ;; adjacent 8-bit boundaries, so it never mis-rounds a genuine value.
    (if (minusp x) (- (floor (+ (- x) 1/2 1d-6))) (floor (+ x 1/2 1d-6)))))

(defun serialize-alpha (a)
  "Serialize an <alpha-value> per CSSOM sRGB serialization: the shortest decimal
   (0-3 places) that rounds to the same 8-bit alpha (round(a*255)).  Avoids the
   single-float leak of NUM->CSS on a stored float alpha (0.2f0 -> 0.2, not
   0.20000000298...)."
  (let* ((a (max 0d0 (min 1d0 (float a 1d0))))
         (a255 (round (* a 255))))
    (loop for places from 0 to 3
          for scale = (expt 10 places)
          for cand = (/ (round (* (/ a255 255d0) scale)) scale)
          when (= (round (* cand 255)) a255)
            return (num->css (float cand 1d0))
          finally (return (num->css a)))))

(defun rgb-str (c)
  (if (and (consp c) (>= (length c) 3))
      (destructuring-bind (r g b &optional (a 1.0)) c
        (if (and a (< a 1))
            (format nil "rgba(~a, ~a, ~a, ~a)" (round-channel r) (round-channel g)
                    (round-channel b) (serialize-alpha a))
            (format nil "rgb(~a, ~a, ~a)" (round-channel r) (round-channel g) (round-channel b))))
      ""))

;;; ---- modern <color> computed-value serialization (CSS Color 4 §12/§15) ------
;;; getComputedStyle serializes a color in the space it was authored in when that
;;; space is not plain sRGB legacy: lab/lch/oklab/oklch keep their function form
;;; with resolved components; hwb/hsl keep their function form ONLY when a channel
;;; is `none` (else they resolve to sRGB rgb()/rgba()).  weft's cascade flattens
;;; every color to an sRGB rgba list, losing the authored space, so this path
;;; re-derives the computed serialization from the element's SPECIFIED value
;;; string.  Read-only: it never touches the cascade or paint (pixel-neutral).

(defun %css-nan-p (x) (and (floatp x) (sb-ext:float-nan-p x)))
(defun %nan0 (x) (if (%css-nan-p x) 0d0 (float x 1d0)))
(defun %clampf (x lo hi) (max lo (min hi (%nan0 x))))

(defun fmt-color-num (x)
  "Serialize a resolved lab/lch/hwb component to at most 4 decimal places,
   integral when whole (CSS Color component serialization)."
  (let ((r (/ (fround (* (%nan0 x) 10000d0)) 10000d0)))
    (num->css (if (zerop r) 0 r))))

(defun %ncalc-tokens (s pctref)
  "Tokenize a numeric calc-body / bare component S: numbers with optional
   %/angle unit (resolved to a double against PCTREF / degrees), the operators
   + - * / and parens, and the idents nan/infinity/pi/e.  Signals on a bad unit
   or ident (caller catches -> defer)."
  (let ((toks '()) (i 0) (n (length s)))
    (loop while (< i n) do
      (let ((c (char s i)))
        (cond
          ((member c '(#\Space #\Tab #\Newline #\Return)) (incf i))
          ((char= c #\() (push :lp toks) (incf i))
          ((char= c #\)) (push :rp toks) (incf i))
          ((char= c #\+) (push :add toks) (incf i))
          ((char= c #\-) (push :sub toks) (incf i))
          ((char= c #\*) (push :mul toks) (incf i))
          ((char= c #\/) (push :div toks) (incf i))
          ((char= c #\,) (push :comma toks) (incf i))
          ((or (digit-char-p c) (char= c #\.))
           (let ((start i))
             (loop while (and (< i n) (let ((d (char s i)))
                                        (or (digit-char-p d) (char= d #\.)))) do (incf i))
             (let ((num (let ((*read-default-float-format* 'double-float))
                          (float (read-from-string (subseq s start i)) 1d0)))
                   (ustart i))
               (loop while (and (< i n) (or (alpha-char-p (char s i)) (char= (char s i) #\%)))
                     do (incf i))
               (let ((unit (subseq s ustart i)))
                 (push (cond ((zerop (length unit)) num)
                             ((string= unit "%") (* (/ num 100d0) pctref))
                             ((string= unit "deg") num)
                             ((string= unit "grad") (* num 0.9d0))
                             ((string= unit "rad") (* num (/ 180d0 pi)))
                             ((string= unit "turn") (* num 360d0))
                             (t (error "unit")))
                       toks)))))
          ((alpha-char-p c)
           (let ((start i))
             (loop while (and (< i n) (alpha-char-p (char s i))) do (incf i))
             (let ((id (string-downcase (subseq s start i))))
               (push (cond ((string= id "nan") (- sb-ext:double-float-positive-infinity
                                                   sb-ext:double-float-positive-infinity))
                           ((string= id "infinity") sb-ext:double-float-positive-infinity)
                           ((string= id "pi") pi)
                           ((string= id "e") (exp 1d0))
                           (t (error "ident")))
                     toks))))
          (t (error "tok")))))
    (coerce (nreverse toks) 'vector)))

(defun %eval-num-calc (str pctref)
  "Evaluate a numeric calc-body / bare component STR to a double (possibly
   NaN/Inf), resolving % against PCTREF and angles to degrees; NIL on failure."
  (ignore-errors
   (sb-int:with-float-traps-masked (:invalid :divide-by-zero :overflow)
     (let ((tv (%ncalc-tokens str pctref)) (pos 0))
       (labels ((peek () (when (< pos (length tv)) (aref tv pos)))
                (next () (prog1 (aref tv pos) (incf pos)))
                (factor ()
                  (let ((tk (peek)))
                    (cond ((eq tk :sub) (next) (- (factor)))
                          ((eq tk :add) (next) (factor))
                          ((eq tk :lp) (next)
                           (prog1 (expr) (unless (eq (peek) :rp) (error "paren")) (next)))
                          ((numberp tk) (next) tk)
                          (t (error "factor")))))
                (term ()
                  (let ((v (factor)))
                    (loop for tk = (peek)
                          while (member tk '(:mul :div)) do (next)
                          (setf v (if (eq tk :mul) (* v (factor)) (/ v (factor)))))
                    v))
                (expr ()
                  (let ((v (term)))
                    (loop for tk = (peek)
                          while (member tk '(:add :sub)) do (next)
                          (setf v (if (eq tk :add) (+ v (term)) (- v (term)))))
                    v)))
         (let ((v (expr))) (when (= pos (length tv)) v)))))))

(defun %resolve-color-comp (str pctref)
  "Resolve a modern-color component STR to :none, a double, or NIL (parse fail)."
  (let ((s (string-trim '(#\Space #\Tab #\Newline #\Return) str)))
    (cond ((zerop (length s)) nil)
          ((string-equal s "none") :none)
          ((and (> (length s) 5) (string-equal (subseq s 0 5) "calc("))
           (%eval-num-calc (subseq s 5 (1- (length s))) pctref))
          (t (%eval-num-calc s pctref)))))

(defun %resolve-alpha (str)
  "Resolve an alpha component STR (NIL -> 1) to :none, a double, or NIL."
  (cond ((null str) 1d0)
        (t (let ((s (string-trim '(#\Space #\Tab #\Newline #\Return) str)))
             (cond ((zerop (length s)) 1d0)
                   ((string-equal s "none") :none)
                   ((char= (char s (1- (length s))) #\%)
                    (let ((v (%resolve-color-comp (subseq s 0 (1- (length s))) 1d0)))
                      (and (numberp v) (/ v 100d0))))
                   (t (%resolve-color-comp s 1d0)))))))

(defun %fin (v) (if (eq v :none) "none" (fmt-color-num v)))

(defun %alpha-tail (a)
  "The ` / alpha` serialization tail for a resolved alpha A (:none | double);
   empty string when A is exactly 1 (CSSOM omits a fully-opaque alpha)."
  (cond ((eq a :none) " / none")
        ((= a 1d0) "")
        (t (concatenate 'string " / " (serialize-alpha a)))))

(defun %serialize-modern-lab (kind comps alpha)
  (when (= (length comps) 3)
    (let* ((oklab (member kind '(:oklab :oklch)))
           (chromatic (member kind '(:lch :oklch)))
           (lmax (if oklab 1d0 100d0))
           (abref (if oklab 0.4d0 125d0))
           (cref (if oklab 0.4d0 150d0))
           (l (%resolve-color-comp (first comps) (if oklab 1d0 100d0)))
           (m (%resolve-color-comp (second comps) (if chromatic cref abref)))
           (h (%resolve-color-comp (third comps) (if chromatic 1d0 abref)))
           (a (%resolve-alpha alpha)))
      (when (and l m h a)
        (let ((ls (if (eq l :none) :none (%clampf l 0d0 lmax)))
              (ms (cond ((eq m :none) :none) (chromatic (max 0d0 (%nan0 m))) (t (%nan0 m))))
              (hs (cond ((eq h :none) :none) (chromatic (mod (%nan0 h) 360d0)) (t (%nan0 h))))
              (as (if (eq a :none) :none (%clampf a 0d0 1d0))))
          (format nil "~a(~a ~a ~a~a)" (string-downcase (symbol-name kind))
                  (%fin ls) (%fin ms) (%fin hs) (%alpha-tail as)))))))

(defun %pure-hue-rgb (h)
  "Fully-saturated sRGB (values r g b) in [0,1] for hue H (deg) — hsl(H 100% 50%)."
  (let* ((hp (/ (mod h 360d0) 60d0))
         (x (- 1d0 (abs (- (mod hp 2d0) 1d0)))))
    (cond ((< hp 1d0) (values 1d0 x 0d0))
          ((< hp 2d0) (values x 1d0 0d0))
          ((< hp 3d0) (values 0d0 1d0 x))
          ((< hp 4d0) (values 0d0 x 1d0))
          ((< hp 5d0) (values x 0d0 1d0))
          (t (values 1d0 0d0 x)))))

(defun %hwb->srgb (h w b)
  "hwb H(deg) W B(fractions [0,1]) -> sRGB (values r g b) in [0,1]."
  (if (>= (+ w b) 1d0)
      (let ((g (/ w (+ w b)))) (values g g g))
      (multiple-value-bind (r g bl) (%pure-hue-rgb h)
        (let ((f (- 1d0 w b)))
          (values (+ (* r f) w) (+ (* g f) w) (+ (* bl f) w))))))

(defun %serialize-modern-hwb (comps alpha)
  (when (= (length comps) 3)
    (let ((h (%resolve-color-comp (first comps) 1d0))
          (w (%resolve-color-comp (second comps) 1d0))
          (b (%resolve-color-comp (third comps) 1d0))
          (a (%resolve-alpha alpha)))
      (when (and h w b a)
        (if (or (eq h :none) (eq w :none) (eq b :none) (eq a :none))
            (format nil "hwb(~a ~a ~a~a)"
                    (if (eq h :none) "none" (fmt-color-num (mod (%nan0 h) 360d0)))
                    (if (eq w :none) "none" (format nil "~a%" (fmt-color-num (* 100 (%clampf w 0d0 1d0)))))
                    (if (eq b :none) "none" (format nil "~a%" (fmt-color-num (* 100 (%clampf b 0d0 1d0)))))
                    (%alpha-tail (if (eq a :none) :none (%clampf a 0d0 1d0))))
            (multiple-value-bind (r g bl)
                (%hwb->srgb (mod (%nan0 h) 360d0) (%clampf w 0d0 1d0) (%clampf b 0d0 1d0))
              (rgb-str (list (* 255 r) (* 255 g) (* 255 bl) (%clampf a 0d0 1d0)))))))))

(defun %serialize-modern-hsl-none (comps alpha)
  "hsl/hsla with a `none` channel keeps hsl() form; else NIL (defer to rgba)."
  (when (= (length comps) 3)
    (let ((h (%resolve-color-comp (first comps) 1d0))
          (sv (%resolve-color-comp (second comps) 1d0))
          (lv (%resolve-color-comp (third comps) 1d0))
          (a (%resolve-alpha alpha)))
      (when (and h sv lv a
                 (or (eq h :none) (eq sv :none) (eq lv :none) (eq a :none)))
        (format nil "hsl(~a ~a ~a~a)"
                (if (eq h :none) "none" (fmt-color-num (mod (%nan0 h) 360d0)))
                (if (eq sv :none) "none" (format nil "~a%" (fmt-color-num (* 100 (%clampf sv 0d0 1d0)))))
                (if (eq lv :none) "none" (format nil "~a%" (fmt-color-num (* 100 (%clampf lv 0d0 1d0)))))
                (%alpha-tail (if (eq a :none) :none (%clampf a 0d0 1d0))))))))

(defparameter +color-fn-spaces+
  '("srgb" "srgb-linear" "a98-rgb" "rec2020" "prophoto-rgb"
    "display-p3" "display-p3-linear" "xyz" "xyz-d50" "xyz-d65")
  "Predefined color() color spaces (CSS Color 4 §predefined + §xyz).")

(defun %color-fn-space-out (space)
  "The serialized space name: `xyz` aliases to `xyz-d65` (CSS Color 4 §serializing)."
  (if (string= space "xyz") "xyz-d65" space))

(defun %serialize-color-function (comps alpha)
  "Serialize a color(<space> c1 c2 c3 [/ a]) value.  COMPS = (space c1 c2 c3),
   ALPHA the alpha token or NIL.  Each channel resolves via %resolve-color-comp
   (percentage/100, `none` kept, calc evaluated); channels are NOT gamut-clamped
   (CSS Color 4 §serializing-color-function-values keeps authored magnitudes),
   only alpha is clamped to [0,1].  NIL on failure.  Serves BOTH the specified
   (non-calc) and computed (calc-evaluated) paths identically."
  (when (= (length comps) 4)
    (let ((space (string-downcase (first comps))))
      (when (member space +color-fn-spaces+ :test #'string=)
        (let ((c1 (%resolve-color-comp (second comps) 1d0))
              (c2 (%resolve-color-comp (third comps) 1d0))
              (c3 (%resolve-color-comp (fourth comps) 1d0))
              (a (%resolve-alpha alpha)))
          (when (and c1 c2 c3 a)
            (format nil "color(~a ~a ~a ~a~a)"
                    (%color-fn-space-out space)
                    (%fin c1) (%fin c2) (%fin c3)
                    (%alpha-tail (if (eq a :none) :none (%clampf a 0d0 1d0))))))))))

(defun %computed-modern-color (spec)
  "Computed-value serialization of a modern <color> SPEC, or NIL to defer to the
   sRGB rgba path (keyword/hex/named/legacy rgb/hsl-or-hwb without none)."
  (ignore-errors
   (let* ((v (string-trim '(#\Space #\Tab #\Newline #\Return) spec))
          (lower (string-downcase v))
          (paren (position #\( lower)))
     (when paren
       (let* ((fname (subseq lower 0 paren))
              (interior (css::%color-fn-interior v fname)))
         (when interior
           (multiple-value-bind (comps alpha) (css::%split-color-components interior)
             (cond
               ((string= fname "lab") (%serialize-modern-lab :lab comps alpha))
               ((string= fname "lch") (%serialize-modern-lab :lch comps alpha))
               ((string= fname "oklab") (%serialize-modern-lab :oklab comps alpha))
               ((string= fname "oklch") (%serialize-modern-lab :oklch comps alpha))
               ((string= fname "hwb") (%serialize-modern-hwb comps alpha))
               ((string= fname "color") (%serialize-color-function comps alpha))
               ((member fname '("hsl" "hsla") :test #'string=)
                (%serialize-modern-hsl-none comps alpha))
               (t nil)))))))))

(defun box-shorthand (top right bottom left)
  "Serialize a 4-edge box shorthand (border-color/-style) to the fewest values per
   CSSOM: drop LEFT when it equals RIGHT, then BOTTOM when it equals TOP, then
   RIGHT when it equals TOP (top / top-right / top-right-bottom / all four)."
  (let ((vs (list top right bottom left)))
    (when (string= left right) (setf vs (subseq vs 0 3))
      (when (string= bottom top) (setf vs (subseq vs 0 2))
        (when (string= right top) (setf vs (subseq vs 0 1)))))
    (format nil "~{~a~^ ~}" vs)))

;;; ---- specified-value canonicalization (CSSOM setProperty) -----------------
;;; test_valid_value / test_invalid_value require element.style to canonicalize a
;;; property's SPECIFIED value and DROP invalid declarations.  We route only the
;;; setProperty / style[prop]= path (never the cascade) through the property's
;;; grammar, reusing weft's cascade value parsers so "valid" here means exactly
;;; what layout accepts.  A value we cannot prove valid is stored VERBATIM (NIL
;;; result) — never rejected — so canonicalization can never drop a declaration
;;; the cascade would have honoured (guards the pixel/HN gates).

(defparameter +css-wide-keywords+
  '("inherit" "initial" "unset" "revert" "revert-layer"))

(defparameter +system-colors+
  ;; CSS Color 4 §system colors — valid <color>s that serialize lowercased.
  '("activetext" "buttonborder" "buttonface" "buttontext" "canvas"
    "canvastext" "field" "fieldtext" "graytext" "highlight" "highlighttext"
    "linktext" "mark" "marktext" "visitedtext" "selecteditem"
    "selecteditemtext" "accentcolor" "accentcolortext"))

(defparameter +color-props+
  '("color" "background-color" "border-top-color" "border-right-color"
    "border-bottom-color" "border-left-color" "outline-color"
    "text-decoration-color" "column-rule-color" "caret-color"
    "border-block-start-color" "border-block-end-color"
    "border-inline-start-color" "border-inline-end-color"
    "text-emphasis-color" "-webkit-text-fill-color" "-webkit-text-stroke-color"))

(defparameter +known-color-functions+
  '("rgb" "rgba" "hsl" "hsla" "hwb" "lab" "lch" "oklab" "oklch"
    "color" "color-mix" "color-contrast" "light-dark" "device-cmyk")
  "The CSS Color 4/5 <color> function names.  A color-typed value whose function
   name is outside this set (alpha(), hwba(), …) is not a <color> and is rejected.")

(defun %prefix-p (s prefix)
  (and (>= (length s) (length prefix)) (string= s prefix :end1 (length prefix))))

(defun %risky-color-tokens-p (lower)
  "Color FUNCTIONS whose canonical serialization weft would get wrong: `none`
   components keep the function form (hsl(none ...) stays hsl), relative colors
   (rgb(from ...)) and calc()/var() are not resolved.  Such values are stored
   verbatim rather than mis-serialized.  Gated on a `(` so the bare keyword
   `none` (an invalid <color>) still reaches the reject path."
  (and (find #\( lower)
       (or (search "none" lower) (search "calc" lower) (search "var(" lower)
           (search "from" lower)             ; relative color, e.g. rgb(from red r g b)
           (search "min(" lower) (search "max(" lower) (search "clamp(" lower))))

(defun %num-token-p (s)
  "T iff S is a bare CSS <number> token (no unit) — rejects idents (`eggs`) and
   dimensions (`0deg`)."
  (and (plusp (length s))
       (let ((v (ignore-errors
                 (let ((*read-eval* nil) (*read-default-float-format* 'double-float))
                   (multiple-value-bind (val pos) (read-from-string s nil nil)
                     (and (eql pos (length s)) val))))))
         (realp v))))

(defun %color-fn-comp-class (s)
  "Classify a color()/modern channel token: :none | :calc | :percent | :number |
   :bad.  A `(` token (calc/var/sign/…) is :calc (kept verbatim, not reordered)."
  (let ((s (string-trim '(#\Space #\Tab #\Newline #\Return) s)))
    (cond ((zerop (length s)) :bad)
          ((string-equal s "none") :none)
          ((find #\( s) :calc)
          ((and (char= (char s (1- (length s))) #\%)
                (%num-token-p (subseq s 0 (1- (length s))))) :percent)
          ((%num-token-p s) :number)
          (t :bad))))

(defun %color-fn-alpha-class (s)
  "Classify a color() alpha slot: :absent (no `/`) | :none | :calc | :percent |
   :number | :bad.  Multiple whitespace-separated tokens (junk after alpha) -> :bad."
  (cond ((null s) :absent)
        (t (let ((s (string-trim '(#\Space #\Tab #\Newline #\Return) s)))
             (cond ((zerop (length s)) :bad)
                   ((find #\( s) :calc)
                   ((find-if (lambda (c) (member c '(#\Space #\Tab #\Newline))) s) :bad)
                   (t (%color-fn-comp-class s)))))))

(defun %canon-color-function (v)
  "Canonical specified serialization of a color() function V (CSS Color 4
   §color-function): a canonical string, :invalid, or NIL (verbatim — a channel
   or the alpha uses calc/var, which weft does not reorder)."
  (let ((interior (css::%color-fn-interior v "color")))
    (unless interior (return-from %canon-color-function :invalid))
    (when (find #\, interior) (return-from %canon-color-function :invalid))
    (multiple-value-bind (comps alpha-str) (css::%split-color-components interior)
      ;; COMPS[0] is the space; exactly three channels must follow.
      (unless (and (= (length comps) 4)
                   (member (string-downcase (first comps)) +color-fn-spaces+ :test #'string=))
        (return-from %canon-color-function :invalid))
      (let ((classes (mapcar #'%color-fn-comp-class (rest comps)))
            (alpha-class (%color-fn-alpha-class alpha-str)))
        (when (or (member :bad classes) (eq alpha-class :bad))
          (return-from %canon-color-function :invalid))
        (if (or (member :calc classes) (eq alpha-class :calc))
            nil                         ; keep verbatim (no calc reordering)
            (or (%serialize-color-function comps alpha-str) :invalid))))))

(defun %specified-lab (v fname)
  "Specified-value serialization of a non-calc lab/lch/oklab/oklch <color> V; NIL
   on failure (caller stores verbatim)."
  (ignore-errors
   (let ((interior (css::%color-fn-interior v fname)))
     (when interior
       (multiple-value-bind (comps alpha) (css::%split-color-components interior)
         (%serialize-modern-lab
          (cond ((string= fname "lab") :lab) ((string= fname "lch") :lch)
                ((string= fname "oklab") :oklab) (t :oklch))
          comps alpha))))))

(defun canon-color-value (value)
  "Canonicalize a <color> specified VALUE (CSS Color 4 / CSSOM serialization).
   Named colors, system colors, currentcolor and transparent serialize as their
   lowercased keyword; hex and rgb()/hsl() normalize to rgb()/rgba().  Returns a
   canonical string, :invalid (reject), or NIL (store verbatim — for forms weft
   doesn't fully model: lab/oklch/hwb/color()/color-mix/light-dark/relative)."
  (let* ((v (string-trim '(#\Space #\Tab #\Newline #\Return) value))
         (lower (string-downcase v)))
    (cond
      ((zerop (length v)) :invalid)
      ((member lower +css-wide-keywords+ :test #'string=) lower)
      ((string= lower "currentcolor") "currentcolor")
      ((string= lower "transparent") "transparent")
      ((member lower +system-colors+ :test #'string=) lower)
      ((gethash lower css::*named-colors*) lower)
      ((char= (char v 0) #\#)
       (let ((c (css:parse-value "color" v))) (if (consp c) (rgb-str c) :invalid)))
      ;; Any remaining value with no function parens is not a valid <color>: every
      ;; paren-free color (keyword, named/system color, hex) was handled above, so a
      ;; bare identifier, number, or multi-token run (`black white`) -> reject.
      ((not (find #\( v)) :invalid)
      (t
       ;; A function <color>.  Its name must be one of the CSS Color 4/5 color
       ;; functions — anything else (alpha(), hwba(), a stray ident) is a proven
       ;; error (CSS Color 4 §4).  sRGB legacy/modern functions are parsed and
       ;; serialized (a parser failure rejects), except none/from(relative)/calc
       ;; forms which weft keeps verbatim.  Modern space-syntax-only functions
       ;; (hwb/lab/lch/oklab/oklch) must not contain a comma; other known
       ;; functions weft doesn't fully serialize are stored verbatim.
       (let ((fname (subseq lower 0 (position #\( lower))))
         (cond
           ((not (member fname +known-color-functions+ :test #'string=)) :invalid)
           ((string= fname "color") (%canon-color-function v))
           ((member fname '("rgb" "rgba" "hsl" "hsla") :test #'string=)
            (if (%risky-color-tokens-p lower) nil
                (let ((c (css:parse-value "color" v))) (if (consp c) (rgb-str c) :invalid))))
           ((and (member fname '("hwb" "lab" "lch" "oklab" "oklch") :test #'string=)
                 (find #\, v))
            :invalid)
           ;; lab/lch/oklab/oklch SPECIFIED serialization equals the computed form
           ;; for non-calc inputs (percentages resolved, L clamped, chroma >= 0,
           ;; hue mod 360, alpha [0,1] omitted at 1).  calc()/var()/sign()/relative
           ;; keep authored calc/reference forms weft doesn't serialize -> verbatim.
           ((and (member fname '("lab" "lch" "oklab" "oklch") :test #'string=)
                 (not (search "calc(" lower)) (not (search "var(" lower))
                 (not (search "sign(" lower)) (not (search "min(" lower))
                 (not (search "max(" lower)) (not (search "clamp(" lower))
                 (not (search "from" lower)))
            (or (%specified-lab v fname) nil))
           (t nil)))))))

;;; ---- calc() simplification + serialization (CSS Values 4 §10.9/§10.10) -----
;;; A dedicated write-path simplifier that keeps units SYMBOLIC (the cascade's
;;; EVAL-CALC collapses every length to px+pct and so cannot serialize
;;; `calc(10px + 1em)` as `calc(1em + 10px)`).  It parses a math function into a
;;; linear combination of units (a Sum), folding same-unit terms and constant
;;; min()/max()/clamp(), then serializes per §10.10 (number, then percentage, then
;;; dimensions sorted ASCII case-insensitively by unit).  Absolute lengths
;;; canonicalise to px with EXACT rational factors so `1pc + 1in` folds to `112px`
;;; with no float noise.  Anything not modelled (var()/env(), advanced functions,
;;; symbolic min/max, malformed input) returns :BAIL -> the value is stored
;;; verbatim (never wrongly dropped); only a proven type error returns :INVALID.

(defparameter +calc-abs-len+
  '(("px" . 1) ("cm" . 4800/127) ("mm" . 480/127) ("q" . 120/127)
    ("in" . 96) ("pt" . 4/3) ("pc" . 16))
  "Absolute length units -> exact px factor (CSS Values 4 §5.2); all fold to px.")

(defparameter +calc-rel-len+
  '("em" "rem" "ex" "rex" "cap" "rcap" "ch" "rch" "ic" "ric" "lh" "rlh"
    "vw" "vh" "vi" "vb" "vmin" "vmax"
    "svw" "svh" "svi" "svb" "svmin" "svmax"
    "lvw" "lvh" "lvi" "lvb" "lvmin" "lvmax"
    "dvw" "dvh" "dvi" "dvb" "dvmin" "dvmax"
    "cqw" "cqh" "cqi" "cqb" "cqmin" "cqmax")
  "Relative length units that stay symbolic (each keeps its own unit key).")

(defun calc-parse-rational (str)
  "Parse an unsigned decimal STR (\"25.4\", \".5\", \"10\") to an EXACT rational."
  (let ((dot (position #\. str)))
    (if dot
        (let* ((ip (subseq str 0 dot)) (fp (subseq str (1+ dot)))
               (iv (if (plusp (length ip)) (parse-integer ip) 0))
               (fv (if (plusp (length fp)) (parse-integer fp) 0)))
          (+ iv (/ fv (expt 10 (length fp)))))
        (parse-integer str))))

(defun calc-lex (s)
  "Tokenize math-function interior S -> list of :plus/:minus/:star/:slash/:lparen/
   :rparen/:comma, (:num rational unit) and (:ident name); :BAD on any unmodelled
   character (caller then bails)."
  (let ((toks '()) (i 0) (n (length s)))
    (loop while (< i n) do
      (let ((c (char s i)))
        (cond
          ((member c '(#\Space #\Tab #\Newline #\Return)) (incf i))
          ((char= c #\() (push :lparen toks) (incf i))
          ((char= c #\)) (push :rparen toks) (incf i))
          ((char= c #\,) (push :comma toks) (incf i))
          ((char= c #\+) (push :plus toks) (incf i))
          ((char= c #\-) (push :minus toks) (incf i))
          ((char= c #\*) (push :star toks) (incf i))
          ((and (char= c #\/) (< (1+ i) n) (char= (char s (1+ i)) #\*))
           ;; CSS comment /* ... */ is whitespace-equivalent (CSS Syntax §4.3.2)
           (let ((end (search "*/" s :start2 (+ i 2))))
             (if end (setf i (+ end 2)) (return-from calc-lex :bad))))
          ((char= c #\/) (push :slash toks) (incf i))
          ((or (digit-char-p c)
               (and (char= c #\.) (< (1+ i) n) (digit-char-p (char s (1+ i)))))
           (let ((j i))
             (loop while (and (< j n) (or (digit-char-p (char s j)) (char= (char s j) #\.)))
                   do (incf j))
             (let ((numstr (subseq s i j)) (unit ""))
               (setf i j)
               (cond ((and (< i n) (char= (char s i) #\%)) (setf unit "%") (incf i))
                     (t (let ((k i))
                          (loop while (and (< k n) (alpha-char-p (char s k))) do (incf k))
                          (setf unit (string-downcase (subseq s i k)) i k))))
               (push (list :num (calc-parse-rational numstr) unit) toks))))
          ((alpha-char-p c)
           (let ((j i))
             (loop while (and (< j n) (alpha-char-p (char s j))) do (incf j))
             (push (list :ident (string-downcase (subseq s i j))) toks) (setf i j)))
          (t (return-from calc-lex :bad)))))
    (nreverse toks)))

;;; A simplified value (cval) is either (:map . alist(unit . rational)) — a Sum —
;;; or the symbol :INVALID (proven type error) or :BAIL (unmodelled -> verbatim).
(defun cval-map (alist) (cons :map alist))
(defun cval-map-p (x) (and (consp x) (eq (car x) :map)))
(defun cval-alist (x) (cdr x))

(defun calc-merge (a b sign)
  "Merge two Sum alists, B scaled by SIGN (+1/-1).  Keys may be unit strings or
   symbolic min()/max()/clamp() nodes, so compare with EQUAL (never string=,
   which would error on a node key)."
  (let ((out (mapcar (lambda (kv) (cons (car kv) (cdr kv))) a)))
    (dolist (kv b out)
      (let ((cell (assoc (car kv) out :test #'equal)))
        (if cell (incf (cdr cell) (* sign (cdr kv)))
            (push (cons (car kv) (* sign (cdr kv))) out))))))

(defun calc-scale (alist k)
  (mapcar (lambda (kv) (cons (car kv) (* (cdr kv) k))) alist))

(defun calc-number-value (alist)
  "The rational when ALIST is a pure number ({} or {\"\":n}), else NIL."
  (cond ((null alist) 0)
        ((and (null (cdr alist)) (equal (caar alist) "")) (cdar alist))
        (t nil)))

(defun calc-either (a b)
  "Propagate a non-map cval: :INVALID dominates :BAIL; NIL when both are maps."
  (cond ((or (eq a :invalid) (eq b :invalid)) :invalid)
        ((or (eq a :bail) (eq b :bail)) :bail)
        (t nil)))

(defun calc-add (a b sign)
  (or (calc-either a b) (cval-map (calc-merge (cval-alist a) (cval-alist b) sign))))

(defun calc-scaled-cval (alist k)
  "Scale Sum ALIST by scalar K, but :BAIL when the result is a MULTI-TERM Sum that
   still contains a symbolic min/max/clamp node.  Distributing a scalar across such
   a Sum would discard the canonical `n * (…)` Product form the spec preserves —
   e.g. 2*(min(a,b)+max(c,d)) stays a Product, not 2·min + 2·max (§10.9).  A lone
   scaled node (n * min(…)) is fine and stays."
  (let ((s (calc-scale alist k)))
    (if (and (cdr s) (some (lambda (kv) (calc-key-node-p (car kv))) s))
        :bail
        (cval-map s))))

(defun calc-mul (a b)
  (or (calc-either a b)
      (let ((na (calc-number-value (cval-alist a)))
            (nb (calc-number-value (cval-alist b))))
        (cond (nb (calc-scaled-cval (cval-alist a) nb))
              (na (calc-scaled-cval (cval-alist b) na))
              (t :bail)))))            ; dimension * dimension -> not modelled

(defun calc-div (a b)
  (or (calc-either a b)
      (let ((nb (calc-number-value (cval-alist b))))
        (cond ((null nb) :bail)        ; divide by a dimension -> not modelled
              ((zerop nb) :bail)       ; divide by zero -> infinity, not modelled
              (t (calc-scaled-cval (cval-alist a) (/ 1 nb)))))))

(defun calc-num->cval (rational unit)
  "A single numeric token -> a Sum, folding absolute lengths to px; unknown units
   -> :BAIL (not modelled)."
  (let ((abs (assoc unit +calc-abs-len+ :test #'string=)))
    (cond ((string= unit "") (cval-map (list (cons "" rational))))
          ((string= unit "%") (cval-map (list (cons "%" rational))))
          (abs (cval-map (list (cons "px" (* rational (cdr abs))))))
          ((member unit +calc-rel-len+ :test #'string=)
           (cval-map (list (cons unit rational))))
          (t :bail))))

(defun calc-single-term (alist)
  "(unit . coeff) when ALIST has exactly one term (or number 0 when empty), else NIL."
  (cond ((null alist) (cons "" 0))
        ((null (cdr alist)) (first alist))
        (t nil)))

(defun calc-comparison (name args)
  "Simplify min()/max()/clamp() over its ARGS (cvals).  A one-argument min()/max()
   reduces to its argument; when every argument reduces to a single-term Sum of the
   SAME real unit the comparison folds to one term; otherwise the function is kept
   SYMBOLIC as a node term (:node name arg-alists) with coefficient 1, so it can be
   summed/scaled and serialized per CSS Values 4 §10.10.  :INVALID is propagated;
   an argument weft cannot model (:BAIL) leaves the whole value verbatim."
  (cond
    ((some (lambda (a) (eq a :invalid)) args) :invalid)
    ((some (lambda (a) (not (cval-map-p a))) args) :bail)
    ((null args) :bail)
    ((and (string= name "clamp") (/= (length args) 3)) :bail)
    ((and (member name '("min" "max") :test #'string=) (= (length args) 1))
     (first args))                       ; min(x)/max(x) == x
    (t (let ((terms (mapcar (lambda (a) (calc-single-term (cval-alist a))) args)))
         (if (and (notany #'null terms)
                  (every (lambda (tm) (stringp (car tm))) terms)
                  ;; Percentages are NOT folded: their basis is unknown at computed
                  ;; time and may be negative, so min(1%,2%) cannot be ordered
                  ;; (CSS Values 4 §10.9) — keep such comparisons symbolic.
                  (not (string= (car (first terms)) "%"))
                  (let ((u (car (first terms))))
                    (every (lambda (tm) (string= (car tm) u)) terms)))
             ;; all arguments are one-term sums of the same real unit -> fold
             (let ((unit (car (first terms))) (vals (mapcar #'cdr terms)))
               (cval-map (list (cons unit
                 (cond ((string= name "min") (reduce #'min vals))
                       ((string= name "max") (reduce #'max vals))
                       (t (max (first vals) (min (second vals) (third vals)))))))))
             ;; heterogeneous -> keep symbolic (args in source order)
             (cval-map (list (cons (list :node name (mapcar #'cval-alist args)) 1))))))))

(defun calc-eval-tokens (toks)
  "Recursive-descent parse + simplify a token list -> a cval."
  (let ((tv (coerce toks 'vector)) (pos 0))
    (labels ((peek () (when (< pos (length tv)) (aref tv pos)))
             (kind (tk) (if (consp tk) (car tk) tk))
             (advance () (prog1 (aref tv pos) (incf pos)))
             (parse-expr ()
               (let ((left (parse-term)))
                 (loop for tk = (peek)
                       while (member (kind tk) '(:plus :minus))
                       do (advance)
                          (setf left (calc-add left (parse-term)
                                                (if (eq (kind tk) :plus) 1 -1))))
                 left))
             (parse-term ()
               (let ((left (parse-factor)))
                 (loop for tk = (peek)
                       while (member (kind tk) '(:star :slash))
                       do (advance)
                          (setf left (if (eq (kind tk) :star)
                                         (calc-mul left (parse-factor))
                                         (calc-div left (parse-factor)))))
                 left))
             (parse-factor ()
               (let ((tk (peek)))
                 (cond ((eq (kind tk) :minus) (advance)
                        (let ((v (parse-factor)))
                          (if (cval-map-p v) (cval-map (calc-scale (cval-alist v) -1)) v)))
                       ((eq (kind tk) :plus) (advance) (parse-factor))
                       (t (parse-primary)))))
             (parse-primary ()
               (let ((tk (peek)))
                 (cond
                   ((null tk) :bail)
                   ((eq (kind tk) :num) (advance) (calc-num->cval (second tk) (third tk)))
                   ((eq (kind tk) :lparen) (advance)
                    (let ((v (parse-expr)))
                      (if (and (peek) (eq (kind (peek)) :rparen)) (progn (advance) v) :bail)))
                   ((eq (kind tk) :ident) (parse-function))
                   (t :bail))))
             (parse-args ()
               (advance)                     ; consume the '('
               (let ((args '()))
                 (loop
                   (push (parse-expr) args)
                   (let ((tk (peek)))
                     (cond ((and tk (eq (kind tk) :comma)) (advance))
                           ((and tk (eq (kind tk) :rparen)) (advance)
                            (return (nreverse args)))
                           (t (return :bad)))))))
             (parse-function ()
               (let ((name (second (advance))))
                 (if (and (peek) (eq (kind (peek)) :lparen))
                     (let ((args (parse-args)))
                       (cond
                         ((not (listp args)) :bail)
                         ((string= name "calc")
                          (if (= 1 (length args)) (first args) :bail))
                         ((member name '("min" "max" "clamp") :test #'string=)
                          (calc-comparison name args))
                         (t :bail)))       ; var()/env()/advanced funcs -> verbatim
                     :bail))))
      (let ((v (parse-expr)))
        (if (= pos (length tv)) v :bail)))))

(defun simplify-math-function (value)
  "Parse+simplify a top-level math function VALUE (calc/min/max/clamp) -> a cval.
   Non-math or unmodelled input -> :BAIL."
  (handler-case
      (let* ((v (string-trim '(#\Space #\Tab #\Newline #\Return) value))
             (lower (string-downcase v)))
        (if (or (%prefix-p lower "calc(") (%prefix-p lower "min(")
                (%prefix-p lower "max(") (%prefix-p lower "clamp("))
            (let ((toks (calc-lex v)))
              (if (eq toks :bad) :bail (calc-eval-tokens toks)))
            :bail))
    (error () :bail)))

(defun calc-term-str (coeff unit)
  (concatenate 'string (num->css coeff) (if (string= unit "") "" unit)))

(defun calc-key-node-p (key)
  "A Sum key that is a symbolic min()/max()/clamp() node, not a unit string."
  (and (consp key) (eq (car key) :node)))

(defun calc-node-serialize (node)
  "Serialize a symbolic comparison NODE (:node name arg-alists) as name(a, b, …),
   each argument serialized as a bare calc body (no wrapper) in source order."
  (format nil "~a(~{~a~^, ~})"
          (second node) (mapcar #'calc-serialize-sum (third node))))

(defun calc-abs-term-str (key coeff)
  "Serialize one Sum term with a NON-NEGATIVE COEFF.  A unit term is <coeff><unit>;
   a symbolic node with coeff 1 is the bare node, otherwise (<coeff> * node)."
  (if (calc-key-node-p key)
      (if (= coeff 1)
          (calc-node-serialize key)
          (concatenate 'string "(" (num->css coeff) " * " (calc-node-serialize key) ")"))
      (calc-term-str coeff key)))

(defun calc-serialize-sum (alist)
  "Serialize a simplified Sum ALIST to the calc() BODY string (CSS Values 4 §10.10):
   number first, then percentage, then dimensions sorted ASCII case-insensitively by
   unit, then symbolic min/max/clamp nodes sorted by serialization; terms joined by
   ' + ' / ' - '."
  (let* ((num (assoc "" alist :test #'equal))
         (pct (assoc "%" alist :test #'equal))
         (dims (sort (remove-if (lambda (kv) (or (calc-key-node-p (car kv))
                                                 (member (car kv) '("" "%") :test #'equal)))
                                (copy-list alist))
                     #'string< :key #'car))
         (nodes (sort (remove-if-not (lambda (kv) (calc-key-node-p (car kv))) (copy-list alist))
                      #'string< :key (lambda (kv) (calc-node-serialize (car kv)))))
         (ordered (append (and num (list num)) (and pct (list pct)) dims nodes)))
    (with-output-to-string (o)
      (loop for kv in ordered for first = t then nil
            for coeff = (cdr kv) for key = (car kv)
            do (cond (first (when (minusp coeff) (write-string "-" o))
                            (write-string (calc-abs-term-str key (abs coeff)) o))
                     ((minusp coeff)
                      (write-string " - " o) (write-string (calc-abs-term-str key (- coeff)) o))
                     (t (write-string " + " o) (write-string (calc-abs-term-str key coeff) o)))))))

(defun canon-calc-length (value)
  "Canonicalize a math function VALUE for a <length-percentage> property.  Returns a
   'calc(...)' string, :INVALID (a bare number in a length Sum is a proven type
   error), or NIL (bail -> store verbatim)."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
    (if (string= trimmed "0")
        "0px"                            ; unitless 0 <length> serializes as 0px (CSSOM)
        (let ((cv (simplify-math-function value)))
          (cond ((eq cv :invalid) :invalid)
                ((not (cval-map-p cv)) nil)
                (t (let* ((alist (cval-alist cv))
                          (pxcell (assoc "px" alist :test #'equal)))
                     (cond ((null alist) nil)
                           ((assoc "" alist :test #'equal) :invalid)  ; number + length
                           ;; A non-integer px coefficient (from an absolute-unit
                           ;; conversion like 1cm -> 4800/127 px) does not round-trip
                           ;; through weft's single-float length parser, so a used/
                           ;; computed re-resolution of our calc() would diverge from
                           ;; the cascade's direct resolution.  Keep such values
                           ;; verbatim (the cascade already serializes them correctly).
                           ((and pxcell (not (integerp (cdr pxcell)))) nil)
                           ;; A value that is exactly one symbolic min/max/clamp node
                           ;; serializes bare when its coeff is 1 (no calc() wrapper),
                           ;; else as calc(<coeff> * node) with no inner parens (§10.10).
                           ((and (= (length alist) 1) (calc-key-node-p (caar alist)))
                            (let ((coeff (cdar alist)) (node (caar alist)))
                              (if (eql coeff 1)
                                  (calc-node-serialize node)
                                  (concatenate 'string "calc(" (num->css coeff) " * "
                                               (calc-node-serialize node) ")"))))
                           (t (concatenate 'string
                                           "calc(" (calc-serialize-sum alist) ")"))))))))))

(defun canon-calc-opacity (value)
  "Canonicalize a math function VALUE for opacity (<number> | <percentage>).  A
   length term -> :INVALID; a number+percentage mix -> NIL (verbatim)."
  (let ((cv (simplify-math-function value)))
    (cond ((eq cv :invalid) :invalid)
          ((not (cval-map-p cv)) nil)
          (t (let* ((alist (cval-alist cv))
                    (keys (mapcar #'car alist)))
               (cond ((null alist) nil)
                     ;; A symbolic min/max/clamp term (e.g. opacity: min(1, 50%)) is a
                     ;; valid <number>|<percentage> weft resolves elsewhere — keep it
                     ;; verbatim rather than risk a wrong canonical form.
                     ((some #'calc-key-node-p keys) nil)
                     ((some (lambda (k) (not (member k '("" "%") :test #'equal))) keys)
                      :invalid)
                     ((and (member "" keys :test #'equal) (member "%" keys :test #'equal))
                      nil)
                     (t (concatenate 'string "calc(" (calc-serialize-sum alist) ")"))))))))

(defparameter +length-calc-props+
  '("width" "height" "min-width" "max-width" "min-height" "max-height"
    "top" "left" "right" "bottom"
    "margin-top" "margin-right" "margin-bottom" "margin-left"
    "padding-top" "padding-right" "padding-bottom" "padding-left"
    "inline-size" "block-size" "min-inline-size" "max-inline-size"
    "min-block-size" "max-block-size"
    "inset-block-start" "inset-block-end" "inset-inline-start" "inset-inline-end"
    "margin-block-start" "margin-block-end" "margin-inline-start" "margin-inline-end"
    "padding-block-start" "padding-block-end" "padding-inline-start" "padding-inline-end"
    "text-indent")
  "Properties whose <length-percentage> calc() write-path we canonicalize; non-math
   values fall through to :BAIL -> stored verbatim, so no other value is affected.")

;;; ---- sizing-property grammar rejection (CSS Sizing 3 §width/height) --------
;;; width/height/min-*/max-* accept a CLOSED grammar: a size keyword, or a
;;; NON-NEGATIVE <length-percentage> / fit-content()/ math function.  On the
;;; element.style WRITE path we reject anything provably outside it so
;;; test_invalid_value passes (declaration dropped -> getPropertyValue "").  The
;;; grammar is closed, so a token that is neither a recognised keyword nor a
;;; non-negative <length-percentage> is a proven error (negatives, unitless
;;; non-zero, multi-token, `none`/`auto` on the wrong property, stray idents).

(defparameter +sizing-props+
  '("width" "height" "min-width" "max-width" "min-height" "max-height"
    "inline-size" "block-size" "min-inline-size" "max-inline-size"
    "min-block-size" "max-block-size")
  "Properties taking `auto|none | <length-percentage [0,∞]> | min-content |
   max-content | fit-content(<length-percentage>)` — the closed sizing grammar.")

(defparameter +sizing-keywords+
  '("min-content" "max-content" "fit-content" "stretch" "content"
    "-webkit-fill-available" "-webkit-min-content" "-webkit-max-content"
    "-webkit-fit-content" "-moz-min-content" "-moz-max-content"
    "-moz-fit-content" "-moz-available")
  "Intrinsic-size keywords valid for every sizing property (auto/none are gated
   per-property: auto on width/height/min-*, none on max-*).")

(defun %numeric-start-p (s)
  (and (plusp (length s))
       (let ((c (char s 0))) (or (digit-char-p c) (member c '(#\. #\+ #\-))))))

(defun sizing-length-percentage (lower)
  "Classify LOWER as a non-negative <length-percentage>: :VALID; :INVALID (parses
   numerically but is negative, unitless non-zero, or trailing garbage); or
   :NOT-LP (not numeric at all — a keyword the caller handles/ rejects)."
  (if (and (plusp (length lower)) (char= (char lower (1- (length lower))) #\%))
      (let ((n (css:parse-value "number" (subseq lower 0 (1- (length lower))))))
        (cond ((not (numberp n)) :invalid)
              ((minusp n) :invalid)
              (t :valid)))
      (let ((l (css:parse-value "length" lower)))
        (cond ((eq l :invalid) (if (%numeric-start-p lower) :invalid :not-lp))
              ((minusp (first l)) :invalid)
              (t :valid)))))

(defun sizing-fit-content-arg-valid-p (arg)
  "The interior of fit-content(<length-percentage>) — a non-negative length/
   percentage or a math function."
  (let* ((a (string-trim '(#\Space #\Tab #\Newline #\Return) arg))
         (lower (string-downcase a)))
    (if (or (%prefix-p lower "calc(") (%prefix-p lower "min(")
            (%prefix-p lower "max(") (%prefix-p lower "clamp("))
        (not (eq (canon-calc-length a) :invalid))
        (eq (sizing-length-percentage lower) :valid))))

(defun canon-sizing-value (dashed value)
  "Canonicalize/validate a sizing property VALUE.  Returns a canonical string,
   :INVALID (drop the declaration), or NIL (store verbatim)."
  (let* ((v (string-trim '(#\Space #\Tab #\Newline #\Return) value))
         (lower (string-downcase v))
         (maxp (%prefix-p dashed "max-")))
    (cond
      ((zerop (length v)) :invalid)
      ((member lower +css-wide-keywords+ :test #'string=) lower)
      ((or (%prefix-p lower "calc(") (%prefix-p lower "min(")
           (%prefix-p lower "max(") (%prefix-p lower "clamp("))
       (canon-calc-length value))
      ((string= lower "0") "0px")            ; unitless 0 <length> serializes as 0px
      ((string= lower "auto") (if maxp :invalid lower))
      ((string= lower "none") (if maxp lower :invalid))
      ((member lower +sizing-keywords+ :test #'string=) lower)
      ((and (%prefix-p lower "fit-content(")
            (char= (char v (1- (length v))) #\)))
       (if (sizing-fit-content-arg-valid-p (subseq v 12 (1- (length v)))) nil :invalid))
      ((eq (sizing-length-percentage lower) :valid) nil)   ; valid l-p -> verbatim
      (t :invalid))))                                       ; provably outside the grammar

;;; ---- background/border keyword & size grammar rejection (CSS Backgrounds 3) --
;;; Closed keyword enums (per background layer, comma-separated) and the
;;; <bg-size> grammar.  On the element.style write path a value outside the
;;; grammar is dropped so test_invalid_value passes; a value containing a
;;; function is left verbatim (never risk mis-parsing a gradient/var()/calc()).

(defparameter +bg-attachment-kw+ '("scroll" "fixed" "local"))
(defparameter +bg-box-kw+ '("border-box" "padding-box" "content-box"))
(defparameter +bg-clip-kw+
  '("border-box" "padding-box" "content-box" "border-area" "text"))
(defparameter +bg-repeat-kw+ '("repeat" "space" "round" "no-repeat"))
(defparameter +border-style-kw+
  '("none" "hidden" "dotted" "dashed" "solid" "double" "groove" "ridge"
    "inset" "outset"))

(defun %ws-tokens (s)
  (remove "" (uiop:split-string s :separator '(#\Space #\Tab #\Newline #\Return))
          :test #'string=))

(defun %comma-layers (s)
  (mapcar (lambda (x) (string-trim '(#\Space #\Tab) x)) (uiop:split-string s :separator ",")))

(defun bg-layer-valid-p (dashed layer)
  "Validate one comma-separated LAYER (lowercased) of background property DASHED."
  (let ((toks (%ws-tokens layer)))
    (cond
      ((null toks) nil)
      ((string= dashed "background-attachment")
       (and (= 1 (length toks)) (member (first toks) +bg-attachment-kw+ :test #'string=)))
      ((string= dashed "background-origin")
       (and (= 1 (length toks)) (member (first toks) +bg-box-kw+ :test #'string=)))
      ((string= dashed "background-clip")
       (and (<= (length toks) 2)
            (every (lambda (tk) (member tk +bg-clip-kw+ :test #'string=)) toks)))
      ((string= dashed "background-repeat")
       (cond ((= 1 (length toks))
              (or (member (first toks) '("repeat-x" "repeat-y") :test #'string=)
                  (member (first toks) +bg-repeat-kw+ :test #'string=)))
             ((= 2 (length toks))
              (every (lambda (tk) (member tk +bg-repeat-kw+ :test #'string=)) toks))
             (t nil)))
      ((string= dashed "background-size")
       (cond ((= 1 (length toks))
              (or (member (first toks) '("cover" "contain") :test #'string=)
                  (string= (first toks) "auto")
                  (eq (sizing-length-percentage (first toks)) :valid)))
             ((= 2 (length toks))
              (every (lambda (tk) (or (string= tk "auto")
                                      (eq (sizing-length-percentage tk) :valid)))
                     toks))
             (t nil)))
      (t nil))))

(defun canon-bg-keyword-value (dashed value)
  "Validate a comma-layered background keyword/size property; NIL (verbatim) when
   every layer is valid, :INVALID otherwise, keyword for a css-wide value."
  (let* ((v (string-trim '(#\Space #\Tab #\Newline #\Return) value))
         (lower (string-downcase v)))
    (cond
      ((zerop (length v)) :invalid)
      ((member lower +css-wide-keywords+ :test #'string=) lower)
      ((find #\( v) nil)                    ; functions (var/calc/gradient) -> verbatim
      (t (let ((layers (%comma-layers lower)))
           (if (and layers (every (lambda (l) (bg-layer-valid-p dashed l)) layers))
               nil :invalid))))))

(defparameter +bg-keyword-props+
  '("background-attachment" "background-origin" "background-clip"
    "background-repeat" "background-size"))

(defun canon-border-style-value (dashed value)
  "border-style (1-4 keywords) and border-<side>-style (exactly 1).  A keyword
   outside the <line-style> set, or too many values, is rejected."
  (let* ((v (string-trim '(#\Space #\Tab #\Newline #\Return) value))
         (lower (string-downcase v)))
    (cond
      ((zerop (length v)) :invalid)
      ((member lower +css-wide-keywords+ :test #'string=) lower)
      ((find #\( v) nil)
      (t (let ((toks (%ws-tokens lower))
               (maxn (if (string= dashed "border-style") 4 1)))
           (if (and toks (<= (length toks) maxn)
                    (every (lambda (tk) (member tk +border-style-kw+ :test #'string=)) toks))
               nil :invalid))))))

(defparameter +border-style-props+
  '("border-style" "border-top-style" "border-right-style"
    "border-bottom-style" "border-left-style"
    "border-block-start-style" "border-block-end-style"
    "border-inline-start-style" "border-inline-end-style"))

;;; ---- border-image-* longhand grammar rejection (CSS Backgrounds 3 §6) ------
;;; Each border-image longhand has a small closed grammar.  On the element.style
;;; WRITE path we reject values with a PROVABLY invalid piece (a negative number/
;;; percentage, a percentage/keyword where the grammar forbids it, or too many
;;; components) so test_invalid_value passes.  Anything containing `(` (url()/
;;; gradient/calc()/var()/image-set()) is stored VERBATIM — never risk a valid
;;; value.  A numeric token with an unrecognised unit classifies :unknown (kept),
;;; so the check can never drop a value the cascade would honour.
(defparameter +border-image-repeat-kw+ '("stretch" "repeat" "round" "space"))
(defparameter +border-image-props+
  '("border-image-source" "border-image-slice" "border-image-width"
    "border-image-outset" "border-image-repeat"))

(defun %top-level-comma-p (s)
  "True when S has a comma outside any parentheses."
  (let ((depth 0))
    (loop for c across s do
      (case c (#\( (incf depth)) (#\) (when (plusp depth) (decf depth)))
        (#\, (when (zerop depth) (return t)))))))

(defun %bi-ident-p (tok)
  (and (plusp (length tok))
       (every (lambda (c) (or (char<= #\a c #\z) (char= c #\-))) tok)))

(defun %bi-classify (tok kws percent-ok length-ok)
  "Classify one border-image numeric/keyword TOK: :ok, :bad, or :unknown.  KWS =
   the allowed bare keywords; PERCENT-OK / LENGTH-OK gate <percentage>/<length>."
  (cond
    ((%bi-ident-p tok) (if (member tok kws :test #'string=) :ok :bad))
    ((and (plusp (length tok)) (char= (char tok (1- (length tok))) #\%))
     (let ((n (css:parse-value "number" (subseq tok 0 (1- (length tok))))))
       (cond ((not (numberp n)) :unknown)
             ((minusp n) :bad)
             (percent-ok :ok)
             (t :bad))))
    (t (let ((n (css:parse-value "number" tok)))
         (cond ((and (numberp n) (minusp n)) :bad)
               ((numberp n) :ok)                      ; non-negative <number>
               (t (let ((l (css:parse-value "length" tok)))
                    (cond ((and (consp l) (numberp (first l)) (minusp (first l))) :bad)
                          ((consp l) (if length-ok :ok :bad))
                          (t :unknown)))))))))

(defun %bi-list-ok (toks kws percent-ok length-ok)
  "1-4 components, none PROVABLY bad -> NIL (accept); else :INVALID."
  (cond ((or (null toks) (> (length toks) 4)) :invalid)
        ((some (lambda (tk) (eq :bad (%bi-classify tk kws percent-ok length-ok))) toks) :invalid)
        (t nil)))

(defun canon-border-image-value (dashed value)
  "Validate a border-image longhand VALUE.  NIL = accept (store verbatim),
   :INVALID = drop the declaration."
  (let* ((v (string-trim '(#\Space #\Tab #\Newline #\Return) value))
         (lower (string-downcase v)))
    (cond
      ((zerop (length v)) :invalid)
      ((member lower +css-wide-keywords+ :test #'string=) nil)
      ((%top-level-comma-p v) :invalid)          ; no longhand takes a comma list
      ((find #\( v) nil)                          ; url()/gradient/calc()/var() -> verbatim
      ((string= dashed "border-image-source")
       (if (string= lower "none") nil :invalid))  ; <image> forms all carry `(`
      ((string= dashed "border-image-repeat")
       (let ((toks (%ws-tokens lower)))
         (if (and toks (<= (length toks) 2)
                  (every (lambda (tk) (member tk +border-image-repeat-kw+ :test #'string=)) toks))
             nil :invalid)))
      ((string= dashed "border-image-slice")
       (let* ((toks (%ws-tokens lower))
              (fills (count "fill" toks :test #'string=))
              (nums (remove "fill" toks :test #'string=)))
         (cond ((null toks) :invalid)
               ((> fills 1) :invalid)
               ;; `fill` is only valid at the start or end of the slice list
               ((and (= fills 1) (not (or (string= (first toks) "fill")
                                          (string= (car (last toks)) "fill")))) :invalid)
               ;; <number>|<percentage> only (no <length>), 1-4, non-negative
               (t (%bi-list-ok nums '() t nil)))))
      ((string= dashed "border-image-width")
       (%bi-list-ok (%ws-tokens lower) '("auto") t t))
      ((string= dashed "border-image-outset")   ; <length>|<number>, no % / auto
       (%bi-list-ok (%ws-tokens lower) '() nil t))
      (t nil))))

(defun parse-num-double (s)
  "Read an already-validated CSS <number> string S as a DOUBLE (so 0.7 serializes
   canonically as \"0.7\", not the single-float 0.699999988), or NIL if it does not
   read cleanly.  A leading '.'/'+' is normalized for the Lisp reader."
  (let* ((s (string-trim '(#\Space #\Tab #\Newline #\Return) s))
         (s (cond ((and (plusp (length s)) (char= (char s 0) #\.)) (concatenate 'string "0" s))
                  ((and (>= (length s) 2) (char= (char s 0) #\+) (char= (char s 1) #\.))
                   (concatenate 'string "+0" (subseq s 1)))
                  (t s))))
    (ignore-errors
      (let ((*read-eval* nil) (*read-default-float-format* 'double-float))
        (multiple-value-bind (v pos) (read-from-string s)
          (and (realp v) (= pos (length s)) (float v 1d0)))))))

(defun canon-opacity-value (value)
  "Canonicalize an <opacity-value> = <number> | <percentage> (CSS Color 4 §opacity).
   The specified value keeps a number as-is and folds a percentage to its number
   (50% -> 0.5), unclamped.  calc()/min()/max()/clamp() are left verbatim (weft
   has no calc serializer).  Returns a string, :invalid, or NIL (verbatim)."
  (let* ((v (string-trim '(#\Space #\Tab #\Newline #\Return) value))
         (lower (string-downcase v)))
    (cond
      ((zerop (length v)) :invalid)
      ((member lower +css-wide-keywords+ :test #'string=) lower)
      ((or (%prefix-p lower "calc(") (%prefix-p lower "min(")
           (%prefix-p lower "max(") (%prefix-p lower "clamp("))
       (canon-calc-opacity value))
      ((or (search "calc" lower) (search "min(" lower) (search "max(" lower)
           (search "clamp(" lower) (search "var(" lower)) nil)
      ((char= (char v (1- (length v))) #\%)
       (let* ((ns (subseq v 0 (1- (length v))))
              (n (css:parse-value "number" ns)))
         (if (numberp n) (num->css (/ (or (parse-num-double ns) (float n 1d0)) 100)) :invalid)))
      (t (let ((n (css:parse-value "number" v)))
           (if (numberp n) (num->css (or (parse-num-double v) (float n 1d0))) :invalid))))))

(defun canon-declaration (dashed value)
  "Canonical specified-value serialization for property DASHED given raw VALUE.
   :invalid -> drop the declaration; a string -> store it; NIL -> store verbatim."
  (cond
    ((member dashed +color-props+ :test #'string=) (canon-color-value value))
    ((string= dashed "opacity") (canon-opacity-value value))
    ((member dashed +bg-keyword-props+ :test #'string=) (canon-bg-keyword-value dashed value))
    ((member dashed +border-style-props+ :test #'string=) (canon-border-style-value dashed value))
    ((member dashed +border-image-props+ :test #'string=) (canon-border-image-value dashed value))
    ((member dashed +sizing-props+ :test #'string=) (canon-sizing-value dashed value))
    ((member dashed +length-calc-props+ :test #'string=) (canon-calc-length value))
    (t nil)))

(defparameter +computed-props+
  '("display" "white-space" "position" "float" "clear" "overflow" "text-align"
    "cursor" "text-transform" "box-sizing" "font-style" "z-index" "font-weight"
    "font-size" "line-height" "color" "width" "height" "min-width" "max-width"
    "min-height" "max-height" "margin-top" "margin-right" "margin-bottom"
    "margin-left" "padding-top" "padding-right" "padding-bottom" "padding-left"
    "top" "left" "right" "bottom" "visibility" "background-color" "opacity"
    "border-top-color" "border-right-color" "border-bottom-color" "border-left-color"
    "border-color" "border-top-style" "border-right-style" "border-bottom-style"
    "border-left-style" "border-style")
  "Dashed property names getComputedStyle resolves; `property in getComputedStyle(el)`
   must report true for these (test_computed_value's support guard, CSSOM).")

(defun computed-prop-p (dashed)
  (and (member dashed +computed-props+ :test #'string=) t))

(defun computed-prop (cs dashed)
  "A JS string for computed property DASHED off cstyle CS."
  (macrolet ((s (accessor) `(,accessor cs)))
    (cond
      ((string= dashed "display") (s css:cstyle-display))
      ((string= dashed "white-space") (s css:cstyle-white-space))
      ((string= dashed "position") (s css:cstyle-position))
      ((string= dashed "float") (s css:cstyle-float))
      ((string= dashed "clear") (s css:cstyle-clear))
      ((string= dashed "overflow") (s css:cstyle-overflow))
      ((string= dashed "text-align") (s css:cstyle-text-align))
      ((string= dashed "cursor") (s css:cstyle-cursor))
      ((string= dashed "text-transform") (s css:cstyle-text-transform))
      ((string= dashed "box-sizing") (s css:cstyle-box-sizing))
      ((string= dashed "font-style") (s css:cstyle-font-style))
      ((string= dashed "z-index") (let ((z (s css:cstyle-z-index)))
                                    (if (integerp z) (princ-to-string z) (px z))))
      ((string= dashed "font-weight") (princ-to-string (s css:cstyle-font-weight)))
      ((string= dashed "font-size") (px (s css:cstyle-font-size)))
      ((string= dashed "line-height") (px (s css:cstyle-line-height)))
      ((string= dashed "color") (rgb-str (s css:cstyle-color)))
      ((string= dashed "visibility") (s css:cstyle-visibility))
      ((string= dashed "background-color")
       (rgb-str (or (s css:cstyle-background) '(0 0 0 0))))
      ((string= dashed "opacity") (num->css (s css:cstyle-opacity)))
      ;; Resolved border colors: an unset edge falls back to BORDER-COLOR (CSS
      ;; Backgrounds 3); currentcolor was resolved to the used <color> at cascade.
      ((string= dashed "border-top-color")
       (rgb-str (or (s css:cstyle-border-top-color) (s css:cstyle-border-color))))
      ((string= dashed "border-right-color")
       (rgb-str (or (s css:cstyle-border-right-color) (s css:cstyle-border-color))))
      ((string= dashed "border-bottom-color")
       (rgb-str (or (s css:cstyle-border-bottom-color) (s css:cstyle-border-color))))
      ((string= dashed "border-left-color")
       (rgb-str (or (s css:cstyle-border-left-color) (s css:cstyle-border-color))))
      ((string= dashed "border-color")
       (box-shorthand (rgb-str (or (s css:cstyle-border-top-color) (s css:cstyle-border-color)))
                      (rgb-str (or (s css:cstyle-border-right-color) (s css:cstyle-border-color)))
                      (rgb-str (or (s css:cstyle-border-bottom-color) (s css:cstyle-border-color)))
                      (rgb-str (or (s css:cstyle-border-left-color) (s css:cstyle-border-color)))))
      ((string= dashed "border-top-style") (or (s css:cstyle-border-top-style) "none"))
      ((string= dashed "border-right-style") (or (s css:cstyle-border-right-style) "none"))
      ((string= dashed "border-bottom-style") (or (s css:cstyle-border-bottom-style) "none"))
      ((string= dashed "border-left-style") (or (s css:cstyle-border-left-style) "none"))
      ((string= dashed "border-style")
       (box-shorthand (or (s css:cstyle-border-top-style) "none")
                      (or (s css:cstyle-border-right-style) "none")
                      (or (s css:cstyle-border-bottom-style) "none")
                      (or (s css:cstyle-border-left-style) "none")))
      ((string= dashed "width") (px (s css:cstyle-width)))
      ((string= dashed "height") (px (s css:cstyle-height)))
      ((string= dashed "min-width") (px (s css:cstyle-min-width)))
      ((string= dashed "max-width") (px (s css:cstyle-max-width)))
      ((string= dashed "min-height") (px (s css:cstyle-min-height)))
      ((string= dashed "max-height") (px (s css:cstyle-max-height)))
      ((string= dashed "margin-top") (px (s css:cstyle-margin-top)))
      ((string= dashed "margin-right") (px (s css:cstyle-margin-right)))
      ((string= dashed "margin-bottom") (px (s css:cstyle-margin-bottom)))
      ((string= dashed "margin-left") (px (s css:cstyle-margin-left)))
      ((string= dashed "padding-top") (px (css::resolve-pad (s css:cstyle-padding-top) nil)))
      ((string= dashed "padding-right") (px (css::resolve-pad (s css:cstyle-padding-right) nil)))
      ((string= dashed "padding-bottom") (px (css::resolve-pad (s css:cstyle-padding-bottom) nil)))
      ((string= dashed "padding-left") (px (css::resolve-pad (s css:cstyle-padding-left) nil)))
      ((string= dashed "top") (px (s css:cstyle-top)))
      ((string= dashed "left") (px (s css:cstyle-left)))
      ((string= dashed "right") (px (s css:cstyle-right)))
      ((string= dashed "bottom") (px (s css:cstyle-bottom)))
      (t ""))))

(defun %inline-specified (node dashed)
  "The element's SPECIFIED inline value for property DASHED, or NIL."
  (and (eq (h:dnode-kind node) :element)
       (cdr (assoc dashed (parse-inline-style (get-attr node "style")) :test #'string=))))

(defun computed-prop* (node cs dashed)
  "Computed value for DASHED, re-deriving modern-<color> serialization from the
   element's specified value when the authored color space is not sRGB-legacy."
  (if (member dashed +color-props+ :test #'string=)
      (or (let ((spec (%inline-specified node dashed)))
            (and spec (%computed-modern-color spec)))
          (computed-prop cs dashed))
      (computed-prop cs dashed)))

(defun prop->dashed (key)
  (cond ((string= key "cssFloat") "float")
        ((string= key "float") "float")
        (t (camel->dash key))))

;;; ---- getComputedStyle -----------------------------------------------------
(defun %inline-length-px (style prop)
  "The pixel value of PROP in inline STYLE (e.g. width: 100px -> 100.0), or NIL."
  (let ((cell (assoc prop (parse-inline-style style) :test #'string=)))
    (when cell
      (let* ((v (cdr cell))
             (end (position-if-not (lambda (c) (or (digit-char-p c) (member c '(#\. #\-)))) v)))
        (ignore-errors (float (read-from-string (subseq v 0 (or end (length v))))))))))

(defun frame-viewport (ctx doc)
  "The (width height) media viewport of a subframe DOC, from its owning iframe/
   object element's inline size; 0x0 when unknown."
  (let ((el (block found
              (maphash (lambda (k v) (when (eq v doc) (return-from found k)))
                       (context-iframe-docs ctx))
              nil)))
    (if el
        (let ((style (or (get-attr el "style") "")))
          (values (or (%inline-length-px style "width") 0.0)
                  (or (%inline-length-px style "height") 0.0)))
        (values 0.0 0.0))))

(defun document-styles (ctx doc)
  "The computed-style hash for document DOC (cached; recomputed after a
   DOM/attr mutation marks the context dirty)."
  (when (context-dirty ctx)
    (clrhash (context-styles ctx))
    (setf (context-dirty ctx) nil))
  (or (gethash doc (context-styles ctx))
      ;; @media evaluates against the document's viewport: the layout width for
      ;; the top document, and a subframe document's owning iframe/object box.
      ;; The prelude is evaluated while the sheet is parsed, so bind the viewport
      ;; around parsing too.
      (multiple-value-bind (fw fh) (frame-viewport ctx doc)
       (let ((css::*viewport-w* (if (eq doc (context-document ctx)) (float (context-width ctx)) fw))
             (css::*viewport-h* (if (eq doc (context-document ctx)) 600.0 fh)))
        (let ((sheet (css:parse-stylesheet
                      (concatenate 'string
                                   (if (eq doc (context-document ctx)) (or (context-css ctx) "") "")
                                   (string #\Newline)
                                   (weft.render::collect-stylesheets doc)))))
          (setf (gethash doc (context-styles ctx)) (css:compute-styles doc sheet)))))))

(defun owner-document (node)
  (loop for p = node then (h:dnode-parent p)
        while p when (eq (h:dnode-kind p) :document) return p))

(defun computed-style-object (ctx node)
  (let* ((realm (context-realm ctx))
         (getprop (js:native-function realm "getPropertyValue"
                    (lambda (this a) (declare (ignore this))
                      (let* ((doc (owner-document node)) (cs (and doc (gethash node (document-styles ctx doc)))))
                        (if cs (computed-prop* node cs (string-downcase (jstr (arg a 0)))) "")))
                    1)))
    (js:make-host-object realm
      :has (lambda (o key)
             (let ((key (js:to-property-key key)))
               (if (and (stringp key)
                        (or (string= key "getPropertyValue")
                            (computed-prop-p (prop->dashed key))))
                   js:*true*
                   (js::ordinary-has o key))))
      :get (lambda (o key rcv) (declare (ignore rcv))
             (setf key (js:to-property-key key))
             (cond
               ((not (stringp key)) (js:js-get (js:js-object-proto o) key o))
               ((string= key "getPropertyValue") getprop)
               (t (let* ((doc (owner-document node))
                         (cs (and doc (gethash node (document-styles ctx doc)))))
                    (if cs (computed-prop* node cs (prop->dashed key))
                        (js:js-get (js:js-object-proto o) key o)))))))))

;;; ---- element.style (inline declarations) ----------------------------------
(defun parse-inline-style (str)
  "STR like \"a: b; c: d\" -> alist of (dashed-name . value)."
  (let ((out '()))
    (dolist (decl (uiop:split-string (or str "") :separator ";") (nreverse out))
      (let ((c (position #\: decl)))
        (when c
          (let ((k (string-downcase (string-trim " " (subseq decl 0 c))))
                (v (string-trim " " (subseq decl (1+ c)))))
            (when (plusp (length k)) (push (cons k v) out))))))))

(defun serialize-inline-style (alist)
  (with-output-to-string (o)
    (loop for (k . v) in alist for first = t then nil
          do (unless first (write-string " " o))
             (format o "~a: ~a;" k v))))

(defun element-style-object (ctx element)
  "The live CSSStyleDeclaration for ELEMENT's inline style.  Property access
   (camelCase or bracketed dashed name) and the CSSOM methods getPropertyValue/
   setProperty/removeProperty/item all read and write through the `style`
   attribute so mutations re-cascade (CSSOM §CSSStyleDeclaration)."
  (let ((realm (context-realm ctx)))
    (labels ((decls () (parse-inline-style (get-attr element "style")))
             (store (alist)
               (set-attr element "style" (serialize-inline-style alist))
               (setf (context-dirty ctx) t))
             (get-prop (name)
               (let ((cell (assoc (string-downcase name) (decls) :test #'string=)))
                 (if cell (cdr cell) "")))
             (set-prop (name val)
               ;; Setting the empty string removes the declaration (CSSOM §setProperty).
               ;; A parseable value is stored in its canonical form; a value proven
               ;; invalid is ignored (existing declaration untouched); anything else
               ;; is stored verbatim (CSSOM §setProperty / CSS value serialization).
               (let* ((d (string-downcase name)) (alist (decls))
                      (cell (assoc d alist :test #'string=)))
                 (cond ((zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return) (or val ""))))
                        (when cell (store (remove cell alist))))
                       (t (let ((canon (canon-declaration d (or val ""))))
                            (unless (eq canon :invalid)
                              (let ((stored (if (stringp canon) canon val)))
                                (cond (cell (setf (cdr cell) stored) (store alist))
                                      (t (store (append alist (list (cons d stored)))))))))))))
             (remove-prop (name)
               (let* ((d (string-downcase name)) (alist (decls))
                      (cell (assoc d alist :test #'string=)))
                 (when cell (store (remove cell alist))))))
      (let ((f-get (js:native-function realm "getPropertyValue"
                     (lambda (this a) (declare (ignore this))
                       (get-prop (jstr (arg a 0)))) 1))
            (f-set (js:native-function realm "setProperty"
                     (lambda (this a) (declare (ignore this))
                       (set-prop (jstr (arg a 0)) (jstr (arg a 1))) js:*undefined*) 2))
            (f-rem (js:native-function realm "removeProperty"
                     (lambda (this a) (declare (ignore this))
                       (let ((old (get-prop (jstr (arg a 0)))))
                         (remove-prop (jstr (arg a 0))) old)) 1))
            (f-pri (js:native-function realm "getPropertyPriority"
                     (lambda (this a) (declare (ignore this a)) "") 1))
            (f-item (js:native-function realm "item"
                      (lambda (this a) (declare (ignore this))
                        (let ((al (decls)) (i (int-arg a 0)))
                          (if (and (>= i 0) (< i (length al))) (car (nth i al)) ""))) 1)))
        (js:make-host-object realm
          :get (lambda (o key rcv) (declare (ignore rcv))
                 (setf key (js:to-property-key key))
                 (cond
                   ((not (stringp key)) (js:js-get (js:js-object-proto o) key o))
                   ((string= key "cssText") (or (get-attr element "style") ""))
                   ((string= key "length") (num (length (decls))))
                   ((string= key "getPropertyValue") f-get)
                   ((string= key "setProperty") f-set)
                   ((string= key "removeProperty") f-rem)
                   ((string= key "getPropertyPriority") f-pri)
                   ((string= key "item") f-item)
                   ((index-string-p key)          ; numeric index -> property name
                    (let ((al (decls)) (i (parse-integer key)))
                      (if (< i (length al)) (car (nth i al)) "")))
                   (t (let ((cell (assoc (prop->dashed key) (decls) :test #'string=)))
                        (if cell (cdr cell) "")))))
          :set (lambda (o key v rcv) (declare (ignore o rcv))
                 (cond
                   ((string= key "cssText") (set-attr element "style" (jstr v))
                    (setf (context-dirty ctx) t))
                   ((stringp key)
                    (set-prop (prop->dashed key) (jstr v))))
                 js:*true*))))))

;;; ---- CSSOM: document.styleSheets / CSSStyleSheet / CSSRuleList ------------
(defun stylesheet-owner-p (el)
  (or (string= (h:dnode-name el) "style")
      (and (string= (h:dnode-name el) "link")
           (let ((rel (dom:get-attribute el "rel"))) (and rel (search "stylesheet" (string-downcase rel)))))))

(defun style-elements (doc)
  (remove-if-not #'stylesheet-owner-p (dom:get-elements-by-tag-name doc "*")))

(defun computed-px (ctx node prop)
  "The integer pixel value of computed PROP on NODE (0 if auto/none/absent)."
  (let* ((doc (owner-document node)) (cs (and doc (gethash node (document-styles ctx doc)))))
    (if cs (let* ((v (computed-prop cs prop))
                  (end (position-if-not (lambda (c) (or (digit-char-p c) (char= c #\.))) v)))
             (or (ignore-errors (round (read-from-string (subseq v 0 (or end (length v)))))) 0))
        0)))

(defun sheet-rule-count (owner)
  (length (css:parse-stylesheet (dom:text-content owner))))

(defun make-rule-list (ctx owner)
  "A live CSSRuleList over OWNER (<style>)'s current rules."
  (let ((realm (context-realm ctx)))
    (js:make-host-object realm
      :get (lambda (o key rcv) (declare (ignore rcv))
             (let ((key (js:to-property-key key)))
               (cond ((and (stringp key) (string= key "length")) (num (sheet-rule-count owner)))
                     ((index-string-p key)
                      (let* ((rules (css:parse-stylesheet (dom:text-content owner)))
                             (i (parse-integer key)))
                        (if (< i (length rules)) (make-css-rule ctx (nth i rules)) js:*undefined*)))
                     (t (js:js-get (js:js-object-proto o) key o))))))))

(defun make-css-rule (ctx rule)
  (let ((o (js:make-host-object (context-realm ctx))))
    (js:put o "type" 1.0)
    (js:put o "selectorText" (or (css:css-rule-selector rule) ""))
    (js:put o "cssText"
            (format nil "~a { ~{~a: ~a;~^ ~} }" (or (css:css-rule-selector rule) "")
                    (loop for d in (css:css-rule-decls rule)
                          collect (css:css-decl-prop d) collect (css:css-decl-value d))))
    o))

(defun make-stylesheet-object (ctx owner)
  (let* ((realm (context-realm ctx)) (sheet (js:make-host-object realm)))
    (js:put sheet "ownerNode" (wrap ctx owner))
    (js:put sheet "href" (let ((h (dom:get-attribute owner "href")))
                           (if (and h (string= (h:dnode-name owner) "link")) h js:*null*)))
    (js:put sheet "type" "text/css")
    (js:put sheet "title" js:*null*)
    (js:put sheet "cssRules" (make-rule-list ctx owner))
    (js:put sheet "rules" (make-rule-list ctx owner))
    (defmethod* ctx sheet "insertRule" 2 (this a)
      ;; append the rule to the owner's text (last => wins for equal specificity);
      ;; the harness inserts at the end.
      (h:dom-append owner (h:make-text (jstr (arg a 0))))
      (setf (context-dirty ctx) t)
      (num (int-arg a 1)))
    (defmethod* ctx sheet "deleteRule" 1 (this a) js:*undefined*)
    sheet))

(defun make-stylesheet-list (ctx doc)
  (let ((realm (context-realm ctx)))
    (js:make-host-object realm
      :get (lambda (o key rcv) (declare (ignore rcv))
             (let ((key (js:to-property-key key)) (sheets (style-elements doc)))
               (cond ((and (stringp key) (string= key "length")) (num (length sheets)))
                     ((index-string-p key)
                      (let ((i (parse-integer key)))
                        (if (< i (length sheets)) (make-stylesheet-object ctx (nth i sheets)) js:*undefined*)))
                     (t (js:js-get (js:js-object-proto o) key o))))))))

;;; ---- the CSS namespace object (CSSOM §The CSS interface) ------------------
(defun css-escape (s)
  "CSS.escape: serialize S as a CSS identifier (CSSOM §serialize an identifier)."
  (with-output-to-string (o)
    (loop with n = (length s)
          for i from 0 below n
          for c = (char s i)
          for cc = (char-code c)
          do (cond
               ((= cc 0) (write-char (code-char #xFFFD) o))
               ((or (<= 1 cc #x1F) (= cc #x7F))
                (format o "\\~(~x~) " cc))
               ((and (= i 0) (<= #x30 cc #x39))
                (format o "\\~(~x~) " cc))
               ((and (= i 1) (<= #x30 cc #x39) (char= (char s 0) #\-))
                (format o "\\~(~x~) " cc))
               ((and (= i 0) (= n 1) (char= c #\-))
                (write-string "\\-" o))
               ((or (>= cc #x80) (char= c #\-) (char= c #\_)
                    (<= #x30 cc #x39) (<= #x41 cc #x5A) (<= #x61 cc #x7A))
                (write-char c o))
               (t (write-char #\\ o) (write-char c o))))))

(defun css-supports-decl-p (property value)
  "True if declaration PROPERTY:VALUE is well-formed (CSS.supports 2-arg form).
   Parsed through the real CSS parser so malformed input is rejected; unknown
   properties are accepted (weft doesn't maintain a full property registry)."
  (and (stringp property) (stringp value)
       (plusp (length (string-trim '(#\Space #\Tab) property)))
       (plusp (length (string-trim '(#\Space #\Tab) value)))
       (not (find #\; value)) (not (find #\{ value)) (not (find #\} value))
       (handler-case
           (let* ((sheet (css:parse-stylesheet
                          (format nil "*{~a:~a}" property value)))
                  (decls (and sheet (css:css-rule-decls (first sheet)))))
             (and decls
                  (some (lambda (d)
                          (and (string-equal (css:css-decl-prop d)
                                             (string-trim '(#\Space #\Tab) property))
                               (plusp (length (string-trim '(#\Space #\Tab)
                                                           (css:css-decl-value d))))))
                        decls)))
         (error () nil))))

(defun css-supports-condition-p (text)
  "CSS.supports 1-arg form: a supports-condition string, e.g. \"(display: flex)\".
   Handles a single parenthesised declaration; compound and/or/selector() forms
   fall through to NIL (weft doesn't evaluate them)."
  (let ((s (string-trim '(#\Space #\Tab) (or text ""))))
    (when (and (plusp (length s)) (char= (char s 0) #\() (char= (char s (1- (length s))) #\)))
      (let* ((inner (subseq s 1 (1- (length s))))
             (colon (position #\: inner)))
        (when (and colon (not (search ") and " inner)) (not (search ") or " inner)))
          (css-supports-decl-p (subseq inner 0 colon) (subseq inner (1+ colon))))))))

(defun make-css-namespace (realm)
  (let ((css (js:make-host-object realm)))
    (js:put css "escape"
            (js:native-function realm "escape"
              (lambda (this a) (declare (ignore this)) (css-escape (jstr (arg a 0)))) 1)
            :enumerable nil)
    (js:put css "supports"
            (js:native-function realm "supports"
              (lambda (this a) (declare (ignore this))
                (if (js:js-undefined-p (arg a 1))
                    (if (css-supports-condition-p (jstr (arg a 0))) js:*true* js:*false*)
                    (if (css-supports-decl-p (jstr (arg a 0)) (jstr (arg a 1))) js:*true* js:*false*)))
              2)
            :enumerable nil)
    css))

(defun install-cssom (ctx)
  (let* ((realm (context-realm ctx))
         (gcs (js:native-function realm "getComputedStyle"
                (lambda (this args) (declare (ignore this))
                  (let ((node (node-of ctx (arg args 0))))
                    (if node (computed-style-object ctx node)
                        (js:make-host-object realm))))
                2))
         (css (make-css-namespace realm)))
    (js:define-global realm "getComputedStyle" gcs)
    (js:define-global realm "CSS" css)
    (when (proto ctx :window)
      (js:put (proto ctx :window) "getComputedStyle" gcs :enumerable nil)
      (js:put (proto ctx :window) "CSS" css :enumerable nil))))
