;;;; src/css/style.lisp — the cascade + computed style.
;;;;
;;;; Applies a UA stylesheet + author stylesheet(s) + inline style attributes to
;;;; a DOM, producing a computed CSTYLE per element (resolved lengths in px,
;;;; colors as (r g b a), inheritance handled).  A focused property set, enough
;;;; for block/inline layout + paint.
(in-package #:weft.css)

(defstruct cstyle
  (display "inline") (color '(0 0 0 1.0)) background
  (font-size 16.0) (font-weight 400) (line-height :normal)
  (font-family nil)   ; ordered list of lowercased family names, NIL = generic sans-serif
  (font-style "normal") ; normal | italic | oblique
  (width :auto) (height :auto)
  (margin-top 0.0) (margin-right 0.0) (margin-bottom 0.0) (margin-left 0.0)
  (padding-top 0.0) (padding-right 0.0) (padding-bottom 0.0) (padding-left 0.0)
  (border-top-width 0.0) (border-right-width 0.0) (border-bottom-width 0.0) (border-left-width 0.0)
  ;; per-edge border colors (NIL = fall back to BORDER-COLOR) and styles
  ;; (NIL = treat as solid; "none"/"hidden" suppress the edge).
  (border-top-color nil) (border-right-color nil) (border-bottom-color nil) (border-left-color nil)
  (border-top-style nil) (border-right-style nil) (border-bottom-style nil) (border-left-style nil)
  (border-color '(0 0 0 1.0)) (text-align "left") (white-space "normal")
  ;; CSS Backgrounds 3 §5.5 border-radius: each corner a (H . V) pair of the
  ;; horizontal and vertical radius, each a px float or a (:percent N) form kept
  ;; symbolic so paint can resolve % against the box width (H) / height (V).
  ;; NIL corner = no rounding (0).  Not inherited.
  (border-tl-radius nil) (border-tr-radius nil) (border-br-radius nil) (border-bl-radius nil)
  ;; border-collapse (inherited): separate | collapse.  A collapsed table (and its
  ;; internal table elements) ignore border-radius (CSS Backgrounds 3 §5.5).
  (border-collapse "separate")
  ;; CSS-UI outline: a ring painted just outside the border edge; does NOT
  ;; affect layout.  OUTLINE-COLOR NIL = currentColor.  OUTLINE-STYLE NIL/"none"
  ;; = no outline.  OUTLINE-OFFSET px (may be negative) is the gap between the
  ;; border edge and the inner edge of the outline.
  (outline-width 3.0) (outline-style nil) (outline-color nil) (outline-offset 0.0)
  ;; CSS-UI-4 accent-color (inherited); NIL = auto.  An `outline-color: auto`
  ;; (the :auto sentinel) with `outline-style: auto` resolves to accent-color.
  (accent-color nil)
  (text-decoration nil) (list-style "disc")
  (max-width :none) (min-width 0.0) (margin-left-auto nil) (margin-right-auto nil)
  (margin-top-auto nil) (margin-bottom-auto nil)   ; auto block-axis margins (flex/grid)
  ;; deferred percentage margins (NIL = none) — (:percent N) forms kept symbolic so
  ;; layout can resolve them against the containing block inline size (CSS 2.1 §8.3).
  (margin-top-pct nil) (margin-right-pct nil) (margin-bottom-pct nil) (margin-left-pct nil)
  (float "none") (clear "none") (position "static") (box-sizing "content-box") (overflow "visible")
  ;; per-axis overflow (css-overflow-3): OVERFLOW above is the combined value used
  ;; for BFC/clip-context detection (non-visible if EITHER axis clips); these carry
  ;; the axis values so paint can clip one axis while letting the other overflow.
  (overflow-x "visible") (overflow-y "visible")
  ;; css-overflow-3 overflow-clip-margin: (BOX . LEN-px), BOX = :content|:padding|:border
  ;; (the reference box the clip is taken from), LEN extends it outward.  NIL = default
  ;; (padding box, 0px).  Applies only to overflow:clip.
  (overflow-clip-margin nil)
  (vertical-align nil)  ; NIL=baseline | ("top"|"middle"|"bottom"|"sub"|"super") | (num "px"|"em"|"%") — not inherited
  (flex-direction "row") (justify-content "flex-start") (align-items "stretch") (align-content "stretch")
  (flex-wrap "nowrap") (flex-grow 0.0) (flex-shrink 1.0) (flex-basis "auto") (order 0) (gap 0.0)
  ;; CSS Grid (raw template/placement strings, parsed at layout time; see grid.lisp)
  (grid-template-columns nil) (grid-template-rows nil) (grid-auto-rows nil) (grid-auto-columns nil)
  (grid-auto-flow "row") (grid-column nil) (grid-row nil)
  (grid-area nil) (grid-template-areas nil)   ; item area name; container NAME->(r0 c0 rspan cspan) map
  (row-gap 0.0) (column-gap 0.0)          ; distinct gaps (the `gap` slot mirrors row-gap for flex)
  ;; CSS Multi-column (css3-multicol): NIL count/width = auto.
  (column-count nil) (column-width nil) (column-fill "balance")
  (column-span "none")                    ; none | all (a spanner interrupts the columns)
  ;; CSS Multicol Level 2 (column-height / column-wrap): stored only to detect that
  ;; an L2 feature is in play so the L1 column flow can decline to fragment it.
  (column-height nil) (column-wrap nil)
  (justify-items "stretch") (justify-self "auto") (align-self "auto")
  (top :auto) (left :auto) (right :auto) (bottom :auto) (z-index 0)
  (bg-gradient nil)   ; (dir from-rgba to-rgba), dir :vertical | :horizontal
  (bg-image nil)      ; raw url() string of a background image (data: URI), decoded at paint
  (bg-repeat "repeat") ; repeat | repeat-x | repeat-y | no-repeat
  (bg-position nil)   ; ((xval xunit) (yval yunit)) or NIL = 0,0
  (bg-size nil)       ; NIL(auto) | :contain | :cover | (w-spec h-spec); spec = px | (:percent N) | :auto
  (bg-attachment "scroll") ; scroll | fixed (fixed images are not painted; see paint)
  (bg-origin "padding-box") ; background positioning area: border-box | padding-box | content-box
  (bg-clip "border-box")    ; background painting area:    border-box | padding-box | content-box
  (bg-clip-list nil)        ; per-layer background-clip list when comma-valued (else NIL)
  (bg-layers 1)             ; number of background layers (from background-image commas)
  (object-fit "fill") ; fill | contain | cover | none | scale-down — how a replaced element's content fills its box
  (writing-mode "horizontal-tb") ; horizontal-tb | vertical-rl | vertical-lr (inherited)
  (direction "ltr")   ; ltr | rtl (inherited)
  (aspect-ratio nil)  ; preferred width/height ratio (a double), or NIL (auto/intrinsic)
  ;; CSS Sizing 4 §aspect-ratio: T when the ratio came with the `auto` keyword
  ;; (`aspect-ratio: auto <ratio>`), in which case the ratio applies to the CONTENT
  ;; box regardless of box-sizing; an explicit ratio (NIL here) applies to the
  ;; box-sizing box.  Not inherited.
  (aspect-ratio-auto nil)
  (min-height 0.0) (max-height :none)
  (cursor "auto")     ; CSS cursor keyword (inherited)
  (text-transform "none") ; none | capitalize | uppercase | lowercase (inherited)
  (hyphens "manual")  ; none | manual | auto (inherited); auto = automatic hyphenation
  (visibility "visible")  ; visible | hidden | collapse (inherited); hidden keeps the box but paints nothing
  (letter-spacing 0.0)    ; extra px after each glyph (inherited)
  (word-spacing 0.0)      ; extra px at each inter-word space (inherited)
  (text-indent 0.0)   ; first-line indent px (inherited)
  (overflow-wrap "normal") ; normal | break-word | anywhere (inherited)
  (word-break "normal")    ; normal | break-all | keep-all (inherited)
  ;; CSS Transforms: TRANSFORM is a list of (fn arg...) (e.g. ("translate" "10px" "5px")),
  ;; NIL = none.  TRANSFORM-ORIGIN is ((val unit) (val unit)) or NIL (= 50% 50%).
  (transform nil) (transform-origin nil)
  ;; CSS Backgrounds 3 §7 box-shadow: a list of shadows, first listed = topmost.
  ;; Each shadow is (INSET OFFX OFFY BLUR SPREAD COLOR): INSET boolean, OFFX/OFFY/
  ;; BLUR/SPREAD px floats (BLUR>=0), COLOR an (r g b a) list or :currentcolor.
  ;; NIL = none.  Not inherited.
  (box-shadow nil)
  ;; CSS Text Decoration 3 §text-shadow: a list of shadows, first listed = topmost
  ;; (all painted BEHIND the text).  Each shadow is (OFFX OFFY BLUR COLOR): OFFX/OFFY/
  ;; BLUR px floats (BLUR>=0), COLOR an (r g b a) list or :currentcolor.  NIL = none.
  ;; Inherited (unlike box-shadow).  Blur is painted sharp (no gaussian) for now.
  (text-shadow nil)
  ;; CSS 2.1 §12.4 counters: each an alist of (name . integer) — NOT inherited.
  (counter-reset nil) (counter-increment nil)
  ;; CSS 2.1 §12.3.1 quotes: a vector of (open . close) string pairs by nesting
  ;; depth (NIL = UA default / not set), used to resolve open-quote/close-quote.
  ;; Inherited.
  (quotes nil)
  ;; CSS 2.1 §17.4.1 caption-side: top | bottom (inherited); applies to
  ;; table-caption boxes, positioning the caption above or below the table box.
  (caption-side "top")
  ;; CSS Color 4 §opacity: the clamped [0,1] alpha multiplier for the whole box.
  ;; opacity < 1 is composited as group opacity in paint (see PAINT-OPACITY): the
  ;; subtree is rendered to offscreen buffers and blended over the backdrop at this
  ;; alpha, so it is layout-neutral but affects paint.
  (opacity 1.0)
  ;; CSS Containment 3 §container: an element with CONTAINER-TYPE "size" or
  ;; "inline-size" establishes a query container (size containment on the queried
  ;; axis) that descendant @container rules resolve against.  CONTAINER-NAME is a
  ;; list of lowercased idents (NIL = unnamed).  Not inherited.
  (container-type "normal") (container-name nil)
  (content nil))      ; generated-content string (or (:tmpl seg...) template) for ::before/::after (NIL = no box)

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
            (cstyle-font-family cs) (cstyle-font-family parent-cs)
            (cstyle-font-style cs) (cstyle-font-style parent-cs)
            (cstyle-text-align cs) (cstyle-text-align parent-cs)
            (cstyle-white-space cs) (cstyle-white-space parent-cs)
            (cstyle-cursor cs) (cstyle-cursor parent-cs)
            (cstyle-text-transform cs) (cstyle-text-transform parent-cs)
            (cstyle-hyphens cs) (cstyle-hyphens parent-cs)
            (cstyle-visibility cs) (cstyle-visibility parent-cs)
            (cstyle-letter-spacing cs) (cstyle-letter-spacing parent-cs)
            (cstyle-word-spacing cs) (cstyle-word-spacing parent-cs)
            (cstyle-text-indent cs) (cstyle-text-indent parent-cs)
            (cstyle-overflow-wrap cs) (cstyle-overflow-wrap parent-cs)
            (cstyle-word-break cs) (cstyle-word-break parent-cs)
            (cstyle-writing-mode cs) (cstyle-writing-mode parent-cs)
            (cstyle-direction cs) (cstyle-direction parent-cs)
            (cstyle-quotes cs) (cstyle-quotes parent-cs)
            (cstyle-caption-side cs) (cstyle-caption-side parent-cs)
            (cstyle-border-collapse cs) (cstyle-border-collapse parent-cs)
            (cstyle-list-style cs) (cstyle-list-style parent-cs)
            (cstyle-accent-color cs) (cstyle-accent-color parent-cs)
            (cstyle-text-shadow cs) (cstyle-text-shadow parent-cs)))
    (cond ((member tag *none-tags* :test #'string=) (setf (cstyle-display cs) "none"))
          ((string= tag "li") (setf (cstyle-display cs) "list-item"))
          ((string= tag "table") (setf (cstyle-display cs) "table"))
          ((string= tag "tr") (setf (cstyle-display cs) "table-row"))
          ((member tag '("td" "th") :test #'string=) (setf (cstyle-display cs) "table-cell"))
          ((member tag '("thead" "tbody" "tfoot") :test #'string=) (setf (cstyle-display cs) "table-row-group"))
          ;; <center> is a block whose block-level children are horizontally centered
          ;; (applied as margin:auto in the cascade).  It does NOT set text-align:center
          ;; here — that inherits and would wrongly centre all descendant text.
          ((string= tag "center") (setf (cstyle-display cs) "block"))
          ((member tag *block-tags* :test #'string=) (setf (cstyle-display cs) "block"))
          (t (setf (cstyle-display cs) "inline")))
    (when (string= tag "th") (setf (cstyle-font-weight cs) 700 (cstyle-text-align cs) "center"))
    (when (member tag '("td" "th") :test #'string=) (set-padding cs 2.0))
    ;; UA margins / sizes / padding — matched to the browser default stylesheet
    ;; (measured via getComputedStyle on bare elements).
    (cond
      ((string= tag "body") (set-margin cs 8.0))
      ;; lists: the indent is padding-left:40 on the list (not a margin on the li),
      ;; so the marker sits in that padding, outside the li content box.
      ((member tag '("ul" "ol" "menu" "dir") :test #'string=)
       (setf (cstyle-margin-top cs) 16.0 (cstyle-margin-bottom cs) 16.0 (cstyle-padding-left cs) 40.0))
      ;; blockquote/figure indent 40 on both sides.
      ((member tag '("blockquote" "figure") :test #'string=)
       (setf (cstyle-margin-top cs) 16.0 (cstyle-margin-bottom cs) 16.0
             (cstyle-margin-left cs) 40.0 (cstyle-margin-right cs) 40.0))
      ((string= tag "dd") (setf (cstyle-margin-left cs) 40.0))
      ((member tag '("p" "dl") :test #'string=) (setf (cstyle-margin-top cs) 16.0 (cstyle-margin-bottom cs) 16.0))
      ((string= tag "h1") (setf (cstyle-font-size cs) 32.0 (cstyle-font-weight cs) 700 (cstyle-margin-top cs) 21.0 (cstyle-margin-bottom cs) 21.0))
      ((string= tag "h2") (setf (cstyle-font-size cs) 24.0 (cstyle-font-weight cs) 700 (cstyle-margin-top cs) 20.0 (cstyle-margin-bottom cs) 20.0))
      ((string= tag "h3") (setf (cstyle-font-size cs) 18.0 (cstyle-font-weight cs) 700 (cstyle-margin-top cs) 18.0 (cstyle-margin-bottom cs) 18.0))
      ((string= tag "h4") (setf (cstyle-font-weight cs) 700 (cstyle-margin-top cs) 21.0 (cstyle-margin-bottom cs) 21.0))
      ((string= tag "h5") (setf (cstyle-font-size cs) 13.0 (cstyle-font-weight cs) 700 (cstyle-margin-top cs) 22.0 (cstyle-margin-bottom cs) 22.0))
      ((string= tag "h6") (setf (cstyle-font-size cs) 11.0 (cstyle-font-weight cs) 700 (cstyle-margin-top cs) 25.0 (cstyle-margin-bottom cs) 25.0))
      ((member tag '("b" "strong") :test #'string=) (setf (cstyle-font-weight cs) 700))
      ((member tag '("a") :test #'string=) (setf (cstyle-color cs) '(0 0 238 1.0) (cstyle-text-decoration cs) '("underline")))
      ((string= tag "mark") (setf (cstyle-background cs) '(255 255 0 1.0) (cstyle-color cs) '(0 0 0 1.0)))   ; UA highlight
      ((string= tag "img") (setf (cstyle-display cs) "inline-block" (cstyle-background cs) '(228 228 232 1.0)
                                 (cstyle-border-top-width cs) 1.0 (cstyle-border-right-width cs) 1.0
                                 (cstyle-border-bottom-width cs) 1.0 (cstyle-border-left-width cs) 1.0
                                 (cstyle-border-color cs) '(170 170 180 1.0) (cstyle-color cs) '(110 110 120 1.0)))
      ((string= tag "hr") (setf (cstyle-margin-top cs) 8.0 (cstyle-margin-bottom cs) 8.0))
      ((string= tag "pre") (setf (cstyle-white-space cs) "pre" (cstyle-font-size cs) 13.0
                                 (cstyle-margin-top cs) 13.0 (cstyle-margin-bottom cs) 13.0)))
    ;; UA monospace default for code-ish elements (browsers use monospace here).
    (when (member tag '("pre" "code" "tt" "kbd" "samp") :test #'string=)
      (setf (cstyle-font-family cs) '("monospace")))
    ;; italic default for emphasis/citation elements.
    (when (member tag '("i" "em" "cite" "var" "dfn" "address") :test #'string=)
      (setf (cstyle-font-style cs) "italic"))
    cs))

(defun set-margin (cs v) (setf (cstyle-margin-top cs) v (cstyle-margin-right cs) v
                               (cstyle-margin-bottom cs) v (cstyle-margin-left cs) v))
(defun set-padding (cs v) (setf (cstyle-padding-top cs) v (cstyle-padding-right cs) v
                                (cstyle-padding-bottom cs) v (cstyle-padding-left cs) v))

;;; ---- value resolution ---------------------------------------------------
(defvar *viewport-w* nil "Initial containing block width in px (for vw/vmin/vmax); NIL until set by the renderer.")
(defvar *viewport-h* nil "Viewport height in px (for vh/vmin/vmax); NIL until set by the renderer.")
(defvar *resolve-family* nil
  "Font-family list of the element whose declaration is currently being resolved, so
ex/ch use the right per-face metric (WPT's Ahem: x-height 0.8em, char advance 1.0em).")
(defun ahem-family-p (fam)
  "True when font-family list FAM names the Ahem test font (exact x-height/advance)."
  (and (listp fam)
       (some (lambda (f) (and (stringp f)
                              (string-equal (string-trim '(#\Space #\" #\') f) "Ahem")))
             fam)))

(defun resolve-len (text font-size &optional (auto-ok nil))
  "Resolve a length string to px (float), or :auto, or NIL if unparseable."
  (let ((tt (string-downcase (string-trim '(#\Space) text))))
    (cond
      ((and auto-ok (string= tt "auto")) :auto)
      ((string= tt "0") 0.0)
      ((and (> (length tt) 5) (string= (subseq tt 0 5) "calc("))
       (multiple-value-bind (px pct)
           (eval-calc (string-trim '(#\Space) (subseq tt 5 (1- (length tt)))) font-size)
         ;; a pure length collapses to px now; a percentage-bearing calc stays a
         ;; deferred (:calc px pct) form that RESOLVE-SIZE/-HEIGHT finish at layout.
         (cond ((null px) nil)
               ((zerop pct) px)
               (t (list :calc px pct)))))
      ;; min()/max()/clamp() comparison functions (CSS Values 4 §10): each argument
      ;; is a calc-style linear form; the result is deferred when any argument bears
      ;; a percentage, else collapsed to px immediately.
      ((and (> (length tt) 4) (string= (subseq tt 0 4) "min(")) (parse-math-fn :mmin tt 4 font-size))
      ((and (> (length tt) 4) (string= (subseq tt 0 4) "max(")) (parse-math-fn :mmax tt 4 font-size))
      ((and (> (length tt) 6) (string= (subseq tt 0 6) "clamp(")) (parse-math-fn :mclamp tt 6 font-size))
      (t (let ((v (parse-value "length" tt)))
           (if (and (listp v) (= 2 (length v)))
               (let ((num (float (first v))) (unit (second v)))
                 (cond ((string= unit "px") num)
                       ((string= unit "em") (* num font-size))
                       ((string= unit "rem") (* num 16.0))
                       ;; absolute units at the CSS reference 96px/in
                       ;; exact rational factors (not rounded decimals) so a calc()
                       ;; write-path serializer folding these to px stays bit-identical
                       ;; with the cascade's resolution (used/computed re-resolution).
                       ((string= unit "in") (* num 96))
                       ((string= unit "cm") (* num 4800/127))   ; 96/2.54
                       ((string= unit "mm") (* num 480/127))    ; 96/25.4
                       ((string= unit "q")  (* num 120/127))    ; 96/101.6 (quarter-mm)
                       ((string= unit "pt") (* num 4/3))        ; 96/72
                       ((string= unit "pc") (* num 16))         ; 12pt
                       ;; viewport units (resolve against the viewport when known)
                       ((and (string= unit "vw") *viewport-w*) (* num (/ *viewport-w* 100.0)))
                       ((and (string= unit "vh") *viewport-h*) (* num (/ *viewport-h* 100.0)))
                       ((and (string= unit "vmin") *viewport-w* *viewport-h*) (* num (/ (min *viewport-w* *viewport-h*) 100.0)))
                       ((and (string= unit "vmax") *viewport-w* *viewport-h*) (* num (/ (max *viewport-w* *viewport-h*) 100.0)))
                       ;; font-relative units: ch is the "0" advance, ex the x-height —
                       ;; both ~0.5em for typical fonts (we approximate rather than
                       ;; measure the face at cascade time).  `65ch` (a common prose
                       ;; max-width) would otherwise be read as 65px and wrap every word.
                       ;; ch = advance of "0" (~0.55em for weft's Liberation faces),
                       ;; ex = x-height (~0.5em); approximated rather than measured.
                       ;; use exact rational factors so an Ahem 1ex/1ch lands on an
                       ;; exact pixel (0.8*20px = 16.0, not 16.00000024): the float
                       ;; noise otherwise makes a mitered single-side border rasterise
                       ;; 1px taller than the fill-rect path it must align with.
                       ((string= unit "ch") (* num font-size (if (ahem-family-p *resolve-family*) 1 11/20)))
                       ((string= unit "ex") (* num font-size (if (ahem-family-p *resolve-family*) 4/5 1/2)))
                       ((member unit '("" ) :test #'string=) num)
                       (t num)))   ; treat unknown abs units as px-ish
               nil))))))

(defun calc-tokenize (s)
  "Split a calc() expression into operand and operator (+ - * /) tokens.  Handles
tight operators (Tailwind's `calc(var(--spacing)*8)`, its var already substituted)
and spaced ones; a leading '-' on an operand is a negative number, a '-' after an
operand is subtraction."
  (let ((toks '()) (i 0) (n (length s)))
    (flet ((operand-p (x) (and x (not (member x '("+" "-" "*" "/" "(") :test #'string=)))))
      (loop while (< i n) do
        (let ((c (char s i)))
          (cond
            ((member c '(#\Space #\Tab #\Newline)) (incf i))
            ((member c '(#\+ #\* #\/ #\( #\))) (push (string c) toks) (incf i))
            ((char= c #\-)
             (if (operand-p (first toks))
                 (progn (push "-" toks) (incf i))          ; subtraction
                 (let ((j (1+ i)))                          ; negative number
                   (loop while (and (< j n) (or (alphanumericp (char s j)) (member (char s j) '(#\. #\%)))) do (incf j))
                   (push (subseq s i j) toks) (setf i j))))
            (t (let ((j i))
                 (loop while (and (< j n) (or (alphanumericp (char s j)) (member (char s j) '(#\. #\%)))) do (incf j))
                 (push (subseq s i (max j (1+ i))) toks) (setf i (max j (1+ i)))))))))
    (nreverse toks)))

(defun eval-calc (inner font-size)
  "Evaluate a calc() interior INNER to a linear length form: (values PX PCT), the
value being PX plus PCT% of the containing block (PCT 0 for a pure length).
Percentages stay symbolic so they resolve at layout against the real basis.  A
scalar (unitless number) multiplies/divides a length; length*length or number+length
is invalid (NIL).  * and / bind tighter than + and - (Values §10), and nested
parentheses are collapsed innermost-first before the flat passes."
  (labels ((operand (tk)
             ;; a scalar NUMBER, or a length form (PX . PCT), or :bad
             (cond ((and (plusp (length tk)) (char= (char tk (1- (length tk))) #\%))
                    (let ((n (ignore-errors (read-from-string (subseq tk 0 (1- (length tk)))))))
                      (if (realp n) (cons 0.0 (float n)) :bad)))
                   ((every (lambda (c) (or (digit-char-p c) (member c '(#\. #\-)))) tk)
                    (let ((n (ignore-errors (read-from-string tk)))) (if (realp n) (float n) :bad)))
                   (t (let ((v (resolve-len tk font-size))) (if (numberp v) (cons (float v) 0.0) :bad)))))
           (mul (a b) (cond ((and (numberp a) (numberp b)) (* a b))
                            ((and (consp a) (numberp b)) (cons (* (car a) b) (* (cdr a) b)))
                            ((and (numberp a) (consp b)) (cons (* a (car b)) (* a (cdr b))))
                            (t :bad)))
           (dvd (a b) (cond ((and (numberp a) (numberp b)) (if (zerop b) 0.0 (/ a b)))
                            ((and (consp a) (numberp b) (not (zerop b))) (cons (/ (car a) b) (/ (cdr a) b)))
                            (t :bad)))
           (pls (a b s) (cond ((and (numberp a) (numberp b)) (+ a (* s b)))
                              ((and (consp a) (consp b)) (cons (+ (car a) (* s (car b))) (+ (cdr a) (* s (cdr b)))))
                              (t :bad)))
           ;; reduce a paren-free token list (values + operator strings) to one value,
           ;; else :bad.  * and / first, then + and - (left to right within each).
           (reduce-flat (s)
             (flet ((pass (s ops fn)
                      (loop for pos = (position-if (lambda (x) (member x ops :test #'equal)) s)
                            while (and pos (> pos 0) (< pos (1- (length s)))) do
                              (let ((r (funcall fn (nth (1- pos) s) (nth pos s) (nth (1+ pos) s))))
                                (when (eq r :bad) (return-from pass :bad))
                                (setf s (append (subseq s 0 (1- pos)) (list r) (subseq s (+ pos 2)))))
                            finally (return s))))
               (let ((s (pass s '("*" "/") (lambda (a op b) (if (string= op "*") (mul a b) (dvd a b))))))
                 (if (eq s :bad) :bad
                     (let ((s (pass s '("+" "-") (lambda (a op b) (pls a b (if (string= op "+") 1 -1))))))
                       (cond ((eq s :bad) :bad)
                             ((and s (= 1 (length s))) (first s))
                             (t :bad))))))))
    (let ((seq (mapcar (lambda (tk) (if (member tk '("+" "-" "*" "/" "(" ")") :test #'string=) tk (operand tk)))
                       (calc-tokenize inner))))
      (when (and seq (notany (lambda (x) (eq x :bad)) seq))
        ;; collapse the innermost parenthesised group (the last "(" — no "(" lies
        ;; between it and its ")") until none remain, then reduce what is left.
        (loop for open = (position "(" seq :test #'equal :from-end t)
              while open do
                (let ((close (position ")" seq :test #'equal :start open)))
                  (unless close (return-from eval-calc nil))
                  (let ((val (reduce-flat (subseq seq (1+ open) close))))
                    (when (eq val :bad) (return-from eval-calc nil))
                    (setf seq (append (subseq seq 0 open) (list val) (subseq seq (1+ close)))))))
        (let ((v (reduce-flat seq)))
          (unless (eq v :bad)
            (cond ((consp v) (values (float (car v)) (float (cdr v))))
                  ((numberp v) (values (float v) 0.0)))))))))

(defun split-top-commas (s)
  "Split S on commas that sit at paren depth 0 (so nested min()/calc() args stay
intact), trimming each piece."
  (let ((parts '()) (start 0) (depth 0))
    (dotimes (i (length s))
      (case (char s i)
        (#\( (incf depth)) (#\) (decf depth))
        (#\, (when (zerop depth)
               (push (string-trim '(#\Space #\Tab #\Newline) (subseq s start i)) parts)
               (setf start (1+ i))))))
    (push (string-trim '(#\Space #\Tab #\Newline) (subseq s start)) parts)
    (nreverse parts)))

(defun parse-math-fn (kind tt prefix-len font-size)
  "Parse a min()/max()/clamp() length function TT (KIND :mmin/:mmax/:mclamp) into a
deferred form (KIND (px . pct) ...), or a plain px number when every argument is a
pure length, or NIL.  clamp() requires exactly three arguments."
  (let* ((inner (subseq tt prefix-len (1- (length tt))))
         (args (split-top-commas inner))
         (forms (mapcar (lambda (a) (multiple-value-bind (px pct) (eval-calc a font-size)
                                      (and px (cons px pct))))
                        args)))
    (when (and forms (notany #'null forms)
               (or (not (eq kind :mclamp)) (= 3 (length forms))))
      (if (every (lambda (f) (zerop (cdr f))) forms)   ; no percentages: fold now
          (let ((vs (mapcar #'car forms)))
            (case kind (:mmin (reduce #'min vs)) (:mmax (reduce #'max vs))
                  (:mclamp (max (first vs) (min (second vs) (third vs))))))
          (cons kind forms)))))

(defun resolve-deferred (spec avail)
  "Finish a deferred length form — (:calc px pct) or (:mmin/:mmax/:mclamp (px . pct)…)
— against the containing-block size AVAIL (px), or NIL when SPEC is not deferred or
AVAIL is indefinite."
  (when (and (consp spec) (numberp avail))
    (flet ((lin (f) (+ (car f) (* avail (/ (cdr f) 100.0)))))
      (case (car spec)
        (:calc (+ (second spec) (* avail (/ (third spec) 100.0))))
        (:mmin (reduce #'min (mapcar #'lin (cdr spec))))
        (:mmax (reduce #'max (mapcar #'lin (cdr spec))))
        (:mclamp (let ((vs (mapcar #'lin (cdr spec)))) (max (first vs) (min (second vs) (third vs)))))))))

(defun compute-opacity (value font-size)
  "Resolve an opacity/alpha VALUE to a clamped [0,1] number, or NIL when it is not
a valid <number>|<percentage>|calc()|min()|max()|clamp() (CSS Color 4 §opacity).
opacity carries no lengths, so a percentage resolves against a basis of 1
(100% -> 1)."
  (let* ((tt (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
         (len (length tt)))
    (labels (;; Serialize in DOUBLE: single-float rounding of e.g. 0.7 would print
             ;; as 0.6999999881 (CSSOM wants "0.7"), so keep the value double.
             (clamp01 (x) (max 0d0 (min 1d0 (float x 1d0))))
             (num (s) (ignore-errors
                        (let ((*read-eval* nil) (*read-default-float-format* 'double-float))
                          (multiple-value-bind (v pos) (read-from-string s)
                            (and (realp v) (= pos (length s)) v)))))
             ;; A single opacity operand -> unclamped double, or NIL (percentage
             ;; basis 1: 50% -> 0.5); used to fold min/max/clamp of constants.
             (arg (s) (let* ((s (string-trim '(#\Space #\Tab #\Newline) s)) (l (length s)))
                        (if (and (plusp l) (char= (char s (1- l)) #\%))
                            (let ((n (num (subseq s 0 (1- l))))) (and n (/ (float n 1d0) 100)))
                            (let ((n (num s))) (and n (float n 1d0))))))
             (fold (kind tt prefix)
               (let ((vs (mapcar #'arg (split-top-commas (subseq tt prefix (1- len))))))
                 (cond ((or (null vs) (some #'null vs)
                            (and (eq kind :mclamp) (/= 3 (length vs))))
                        ;; fall back to the shared (single-float) length machinery
                        (let ((r (parse-math-fn kind tt prefix font-size)))
                          (cond ((numberp r) (clamp01 r))
                                ((consp r) (let ((v (resolve-deferred r 1.0))) (and v (clamp01 v))))
                                (t nil))))
                       (t (clamp01 (ecase kind
                                     (:mmin (reduce #'min vs)) (:mmax (reduce #'max vs))
                                     (:mclamp (max (first vs) (min (second vs) (third vs)))))))))))
      (cond
        ((zerop len) nil)
        ((and (> len 5) (string= (subseq tt 0 5) "calc("))
         (let* ((inner (string-trim '(#\Space) (subseq tt 5 (1- len))))
                (const (arg inner)))
           ;; A folded/constant calc (the common case after write-path canon,
           ;; e.g. calc(0.7)) parses in double for an exact CSSOM serialization;
           ;; a still-symbolic calc falls back to the shared length evaluator.
           (if const (clamp01 const)
               (multiple-value-bind (px pct) (eval-calc inner font-size)
                 (and px (clamp01 (+ px (/ pct 100.0))))))))
        ((and (> len 4) (string= (subseq tt 0 4) "min(")) (fold :mmin tt 4))
        ((and (> len 4) (string= (subseq tt 0 4) "max(")) (fold :mmax tt 4))
        ((and (> len 6) (string= (subseq tt 0 6) "clamp(")) (fold :mclamp tt 6))
        ((char= (char tt (1- len)) #\%)
         (let ((n (num (subseq tt 0 (1- len))))) (and n (clamp01 (/ n 100.0)))))
        (t (let ((n (num tt))) (and n (clamp01 n))))))))

(defun line-height-multiplier (value font-size)
  "Parse a line-height VALUE into a multiplier of FONT-SIZE (weft stores
line-height as a number that LAYOUT multiplies by font-size, or the keyword
:NORMAL which LAYOUT resolves from the font's real metrics).  A bare <number>
IS the multiplier; `normal` -> :NORMAL; a <percentage> -> its fraction; a <length>
-> length/font-size (CSS 2.1 10.8.1).  Returns NIL when unparseable."
  (let ((tt (string-downcase (string-trim '(#\Space) value))))
    (cond ((string= tt "normal") :normal)
          ((and (plusp (length tt)) (char= (char tt (1- (length tt))) #\%))
           (let ((n (ignore-errors (read-from-string (subseq tt 0 (1- (length tt)))))))
             (when (realp n) (/ (float n) 100.0))))
          (t (let ((n (ignore-errors (let ((*read-eval* nil)) (read-from-string tt)))))
               (if (realp n)
                   (float n)
                   (let ((px (resolve-len tt font-size)))
                     (when (and (numberp px) (plusp font-size)) (/ px font-size)))))))))

(defun parse-size (text font-size auto-ok)
  "Parse a width/height value -> px number | :auto | (:percent N) |
:min-content | :max-content | :fit-content | NIL."
  (let ((tt (string-downcase (string-trim '(#\Space) text))))
    (cond
      ((and auto-ok (string= tt "auto")) :auto)
      ((string= tt "min-content") :min-content)
      ((string= tt "max-content") :max-content)
      ((or (string= tt "fit-content") (string= tt "-webkit-fit-content")) :fit-content)
      ((and (plusp (length tt)) (char= (char tt (1- (length tt))) #\%))
       (let ((n (ignore-errors (read-from-string (subseq tt 0 (1- (length tt))))))) (when (numberp n) (list :percent (float n)))))
      (t (resolve-len tt font-size)))))

(defun resolve-bg-pos (v fs)
  "Resolve font/absolute length units in a parsed background-position V
\(((xval xunit) (yval yunit))) to px against font-size FS, so the painter — which
only understands px and % — honours e.g. `background-position: 0 1em`.  Percentages
and unitless 0 are left as-is."
  (flet ((comp (c)
           (if (and (consp c) (>= (length c) 2))
               (let ((unit (second c)))
                 (if (member unit '("%" "") :test #'string=)
                     c
                     ;; format the magnitude as a single-float so it never prints a
                     ;; `d0` exponent that resolve-len cannot re-parse (`1.0d0em`).
                     (let ((px (resolve-len (format nil "~a~a" (float (first c) 1.0f0) unit) fs)))
                       (if (numberp px) (list (float px 0d0) "px") c))))
               c)))
    (if (and (consp v) (consp (first v)))
        (mapcar #'comp v)
        v)))

(defun size-nonneg-p (spec)
  "NIL when SPEC (a PARSE-SIZE result) is a negative length or negative percentage.
Negative values are invalid for width/height/min-/max- (CSS 2.1 §10.2/10.4/10.5/10.7)
so the declaration is dropped and the prior cascaded value is kept."
  (not (or (and (numberp spec) (minusp spec))
           (and (consp spec) (eq (first spec) :percent) (numberp (second spec)) (minusp (second spec))))))

(defun bg-size-comp (tok fs)
  "One background-size component -> :auto | px number | (:percent N)."
  (cond ((string= tok "auto") :auto)
        ((and (plusp (length tok)) (char= (char tok (1- (length tok))) #\%))
         (let ((n (ignore-errors (read-from-string (subseq tok 0 (1- (length tok)))))))
           (when (numberp n) (list :percent (float n)))))
        (t (resolve-len tok fs))))

(defun parse-bg-size (value fs)
  "Parse background-size -> :contain | :cover | (w-spec h-spec) | NIL (auto).  weft
paints one background layer, so a comma-separated multi-layer value uses its first
layer (CSS Backgrounds 3 §3.9)."
  (let ((v (string-downcase (string-trim '(#\Space #\Tab #\Newline)
                                         (or (first (split-top-commas value)) value)))))
    (cond ((string= v "contain") :contain)
          ((string= v "cover") :cover)
          ((or (string= v "auto") (string= v "")) nil)
          (t (let* ((parts (remove "" (split-ws v) :test #'string=))
                    (w (and parts (bg-size-comp (first parts) fs)))
                    (h (if (second parts) (bg-size-comp (second parts) fs) :auto)))
               (and w (list w h)))))))

(defun resolve-size (spec avail)
  "Resolve a parse-size result against AVAIL (containing-block px).  :auto/NIL -> NIL.
A percentage resolves only when AVAIL is a definite number; against an indefinite
containing block it computes to auto (NIL), per CSS 2.1 10.2/10.5."
  (cond ((numberp spec) spec)
        ((and (consp spec) (eq (first spec) :percent) (numberp avail))
         (* avail (/ (second spec) 100.0)))
        (t (resolve-deferred spec avail))))

(defun resolve-height (spec avail-h)
  "Resolve a height parse-size SPEC against the containing-block height AVAIL-H
per CSS 2.1 10.5: a percentage resolves only when AVAIL-H is a definite number;
otherwise it computes to auto (NIL).  A plain px number resolves to itself."
  (cond ((numberp spec) spec)
        ((and (consp spec) (eq (first spec) :percent) (numberp avail-h))
         (* avail-h (/ (second spec) 100.0)))
        (t (resolve-deferred spec avail-h))))

(defun resolve-min-height (spec avail-h)
  "CSS 2.1 10.7 min-height: a length resolves to itself; a percentage resolves
against AVAIL-H if definite, else computes to 0."
  (cond ((numberp spec) spec)
        ((and (consp spec) (eq (first spec) :percent))
         (if (numberp avail-h) (* avail-h (/ (second spec) 100.0)) 0.0))
        ((and (consp spec) (member (first spec) '(:calc :mmin :mmax :mclamp)))
         (or (resolve-deferred spec avail-h) (resolve-deferred spec 0.0) 0.0))
        (t 0.0)))

(defun resolve-max-height (spec avail-h)
  "CSS 2.1 10.7 max-height: a length resolves to itself; a percentage resolves
against AVAIL-H if definite, else computes to none (NIL = no ceiling)."
  (cond ((numberp spec) spec)
        ((and (consp spec) (eq (first spec) :percent) (numberp avail-h))
         (* avail-h (/ (second spec) 100.0)))
        (t (resolve-deferred spec avail-h))))

(defun resolve-color (text)
  (let ((v (parse-value "color" text))) (if (and (listp v) (>= (length v) 3)) v nil)))

;;;; ---- CSS gradients (CSS Images 3 §3) --------------------------------------
;;;; A gradient value parses into a structured, render-package-consumable form:
;;;;   (:linear DIR STOPS REPEATING)
;;;;   (:radial SHAPE SIZE POS STOPS REPEATING)
;;;;   (:conic FROM-DEG POS STOPS REPEATING)
;;;; DIR   = (:angle deg) | (:corner h v)  h∈{:left :right nil} v∈{:top :bottom nil}
;;;; SHAPE = :circle | :ellipse
;;;; SIZE  = (:extent kw) | (:len rx ry)   kw∈{:closest-side :farthest-side :closest-corner :farthest-corner}
;;;; POS   = (xspec yspec)  each (:pct f) | (:px n)
;;;; STOPS = list of (:c (r g b a) posspec) | (:h posspec)
;;;;         posspec = nil | (:pct f) | (:px n) | (:deg d)   (deg only for conic)
;;;; Positions are resolved to px/fraction/deg by the rasteriser against the box.

(defun matching-paren (s open-idx)
  "Index of the close paren matching the open paren at OPEN-IDX, or NIL."
  (let ((depth 0) (n (length s)))
    (loop for i from open-idx below n do
      (cond ((char= (char s i) #\() (incf depth))
            ((char= (char s i) #\)) (decf depth)
             (when (zerop depth) (return-from matching-paren i)))))
    nil))

(defun ws-split-top (s)
  "Split S on whitespace runs not inside parens (keeps rgb(...) tokens whole)."
  (let ((out '()) (depth 0) (start nil) (n (length s)))
    (dotimes (i n)
      (let ((c (char s i)))
        (cond ((char= c #\() (incf depth) (unless start (setf start i)))
              ((char= c #\)) (decf depth))
              ((and (zerop depth) (member c '(#\Space #\Tab #\Newline)))
               (when start (push (subseq s start i) out) (setf start nil)))
              (t (unless start (setf start i))))))
    (when start (push (subseq s start n) out))
    (nreverse out)))

(defun angle->deg (a)
  "Convert a parsed angle (num unit) to degrees."
  (let ((num (float (first a) 1.0)) (unit (second a)))
    (cond ((string= unit "grad") (* num 0.9))
          ((string= unit "rad")  (* num (/ 180.0 (float pi 1.0))))
          ((string= unit "turn") (* num 360.0))
          (t num))))

(defun angle-tok-p (tok)
  "True when TOK ends with an angle unit (deg/grad/rad/turn)."
  (let ((s (string-downcase tok)))
    (some (lambda (u) (let ((p (search u s :from-end t)))
                        (and p (= (+ p (length u)) (length s)))))
          '("deg" "grad" "rad" "turn"))))

(defun gcolor-token-p (tok)
  "True when TOK is a <color> (or currentcolor) — used to tell a leading gradient
configuration item (direction/shape/position) apart from a first color stop."
  (or (resolve-color tok)
      (string-equal (string-trim '(#\Space) tok) "currentcolor")))

(defun gcolor (tok)
  "Resolve a stop color token to (r g b a), or :currentcolor, or NIL."
  (if (string-equal (string-trim '(#\Space) tok) "currentcolor")
      :currentcolor
      (resolve-color tok)))

;;;; ---- box-shadow (CSS Backgrounds 3 §7) ------------------------------------
(defun parse-one-shadow (s fs)
  "Parse one comma-separated box-shadow item S -> (INSET OFFX OFFY BLUR SPREAD COLOR),
or NIL if invalid.  Order-flexible per §7: an optional `inset` keyword, 2-4 lengths
\(offset-x offset-y [blur] [spread]) and an optional <color> (default currentColor)."
  (let ((toks (ws-split-top (string-trim '(#\Space #\Tab #\Newline) s)))
        (inset nil) (color nil) (lens '()))
    (dolist (tok toks)
      (cond ((string-equal tok "inset") (setf inset t))
            ((and (null color) (gcolor-token-p tok)) (setf color (gcolor tok)))
            (t (let ((px (resolve-len tok fs)))
                 (if (numberp px)
                     (push px lens)
                     (return-from parse-one-shadow nil))))))  ; unknown token -> invalid
    (setf lens (nreverse lens))
    (when (and (>= (length lens) 2) (<= (length lens) 4))
      (list inset (first lens) (second lens)
            (max 0.0 (or (third lens) 0.0))    ; blur-radius (>= 0)
            (or (fourth lens) 0.0)             ; spread-radius
            (or color :currentcolor)))))

(defun parse-box-shadow (value fs)
  "Parse a box-shadow property VALUE -> list of shadows (topmost first), or NIL for
`none` / invalid (an invalid item invalidates the whole declaration, §7)."
  (let ((v (string-trim '(#\Space #\Tab #\Newline) value)))
    (if (or (string= v "") (string-equal v "none"))
        nil
        (let ((shs (mapcar (lambda (s) (parse-one-shadow s fs)) (split-top-commas v))))
          (if (member nil shs) nil shs)))))

;;;; ---- text-shadow (CSS Text Decoration 3 §text-shadow) --------------------
(defun parse-one-text-shadow (s fs)
  "Parse one comma-separated text-shadow item S -> (OFFX OFFY BLUR COLOR), or NIL if
invalid.  Order-flexible: an optional <color> and 2-3 lengths (offset-x offset-y
[blur]); no inset/spread (unlike box-shadow)."
  (let ((toks (ws-split-top (string-trim '(#\Space #\Tab #\Newline) s)))
        (color nil) (lens '()))
    (dolist (tok toks)
      (cond ((and (null color) (gcolor-token-p tok)) (setf color (gcolor tok)))
            (t (let ((px (resolve-len tok fs)))
                 (if (numberp px)
                     (push px lens)
                     (return-from parse-one-text-shadow nil))))))
    (setf lens (nreverse lens))
    (when (and (>= (length lens) 2) (<= (length lens) 3))
      (list (first lens) (second lens)
            (max 0.0 (or (third lens) 0.0))    ; blur-radius (>= 0)
            (or color :currentcolor)))))

(defun parse-text-shadow (value fs)
  "Parse a text-shadow property VALUE -> list of shadows (topmost first), or NIL for
`none` / invalid (an invalid item invalidates the whole declaration)."
  (let ((v (string-trim '(#\Space #\Tab #\Newline) value)))
    (if (or (string= v "") (string-equal v "none"))
        nil
        (let ((shs (mapcar (lambda (s) (parse-one-text-shadow s fs)) (split-top-commas v))))
          (if (member nil shs) nil shs)))))

(defun gstop-pos (tok type fs)
  "Parse a stop position token -> (:pct f) | (:px n) | (:deg d) | NIL."
  (let ((tk (string-downcase (string-trim '(#\Space #\Tab) tok))))
    (cond
      ((zerop (length tk)) nil)
      ((char= (char tk (1- (length tk))) #\%)
       (let ((v (ignore-errors (read-from-string (subseq tk 0 (1- (length tk)))))))
         (when (numberp v)
           (if (eq type :conic) (list :deg (* (/ (float v 1.0) 100.0) 360.0))
               (list :pct (/ (float v 1.0) 100.0))))))
      ((angle-tok-p tk)
       (let ((a (parse-value "angle" tk)))
         (when (consp a) (list :deg (angle->deg a)))))
      (t (let ((px (resolve-len tk fs))) (when (numberp px) (list :px px)))))))

(defun parse-gstops (items type fs)
  "Parse a color-stop list (ITEMS, comma-split) -> list of (:c color pos)/(:h pos)."
  (let ((stops '()))
    (dolist (it items)
      (let ((toks (ws-split-top (string-trim '(#\Space #\Tab #\Newline) it))))
        (when toks
          (let ((c (gcolor (first toks))))
            (if c
                (let ((p1 (and (second toks) (gstop-pos (second toks) type fs)))
                      (p2 (and (third toks) (gstop-pos (third toks) type fs))))
                  (push (list :c c p1) stops)
                  (when p2 (push (list :c c p2) stops)))
                (let ((p (gstop-pos (first toks) type fs)))
                  (when p (push (list :h p) stops))))))))
    (nreverse stops)))

(defun strip-color-interp (item)
  "Drop a trailing `in <colorspace ...>` color-interpolation-method (rendered in
sRGB regardless) from a gradient configuration ITEM."
  (let* ((toks (ws-split-top (string-trim '(#\Space) item)))
         (pos (position "in" toks :test #'string-equal)))
    (if pos (format nil "~{~a~^ ~}" (subseq toks 0 pos)) item)))

(defun gconfig-p (item)
  "True when the first comma item is a gradient configuration (direction/shape/
position/color-interpolation) rather than the first color stop."
  (let ((toks (ws-split-top (string-trim '(#\Space) item))))
    (and toks (not (gcolor-token-p (first toks))))))

(defparameter *default-gpos* '((:pct 0.5) (:pct 0.5)))

(defun parse-linear-dir (item)
  "Parse a linear-gradient direction -> (:angle deg) | (:corner h v)."
  (let* ((s (string-downcase (string-trim '(#\Space) item)))
         (toks (ws-split-top s)))
    (cond
      ((and toks (string= (first toks) "to"))
       (let (h v)
         (dolist (k (rest toks))
           (cond ((string= k "left") (setf h :left)) ((string= k "right") (setf h :right))
                 ((string= k "top") (setf v :top)) ((string= k "bottom") (setf v :bottom))))
         (if (and h v) (list :corner h v)
             (list :angle (cond ((eq v :top) 0.0) ((eq v :bottom) 180.0)
                                ((eq h :left) 270.0) ((eq h :right) 90.0) (t 180.0))))))
      ((and toks (angle-tok-p (first toks)))
       (let ((a (parse-value "angle" (first toks))))
         (list :angle (if (consp a) (angle->deg a) 180.0))))
      (t (list :angle 180.0)))))

(defun pos-comp (tk fs)
  "One background-position-style component keyword/length -> (:pct f) | (:px n)."
  (cond ((string= tk "center") (list :pct 0.5))
        ((string= tk "left")   (list :pct 0.0)) ((string= tk "right")  (list :pct 1.0))
        ((string= tk "top")    (list :pct 0.0)) ((string= tk "bottom") (list :pct 1.0))
        (t (or (gstop-pos tk :radial fs) (list :pct 0.5)))))

(defun parse-gpos (toks fs)
  "Parse a gradient position (the tokens after `at`) -> (xspec yspec)."
  (cond
    ((null toks) *default-gpos*)
    ((= (length toks) 1)
     (let ((tk (first toks)))
       (cond ((member tk '("top" "bottom") :test #'string=) (list (list :pct 0.5) (pos-comp tk fs)))
             (t (list (pos-comp tk fs) (list :pct 0.5))))))
    (t (let* ((a (first toks)) (b (second toks))
              (swap (or (member a '("top" "bottom") :test #'string=)
                        (member b '("left" "right") :test #'string=))))
         (if swap (list (pos-comp b fs) (pos-comp a fs))
             (list (pos-comp a fs) (pos-comp b fs)))))))

(defun parse-radial-cfg (item fs)
  "Parse a radial-gradient configuration -> (shape size pos)."
  (let* ((toks (ws-split-top (string-downcase (string-trim '(#\Space) item))))
         (shape nil) (size nil) (pos *default-gpos*) (lens '())
         (atpos (position "at" toks :test #'string=)))
    (when atpos
      (setf pos (parse-gpos (subseq toks (1+ atpos)) fs)
            toks (subseq toks 0 atpos)))
    (dolist (tk toks)
      (cond ((string= tk "circle") (setf shape :circle))
            ((string= tk "ellipse") (setf shape :ellipse))
            ((member tk '("closest-side" "closest-corner" "farthest-side" "farthest-corner") :test #'string=)
             (setf size (list :extent (intern (string-upcase tk) :keyword))))
            (t (let ((p (gstop-pos tk :radial fs))) (when p (push p lens))))))
    (setf lens (nreverse lens))
    (when lens
      (setf size (list :len (first lens) (or (second lens) (first lens)))))
    (list (or shape (if (and lens (= (length lens) 1)) :circle :ellipse))
          (or size (list :extent :farthest-corner))
          pos)))

(defun parse-conic-cfg (item fs)
  "Parse a conic-gradient configuration -> (from-deg pos)."
  (let* ((toks (ws-split-top (string-downcase (string-trim '(#\Space) item))))
         (from 0.0) (pos *default-gpos*)
         (atpos (position "at" toks :test #'string=)))
    (when atpos
      (setf pos (parse-gpos (subseq toks (1+ atpos)) fs)
            toks (subseq toks 0 atpos)))
    (let ((fp (position "from" toks :test #'string=)))
      (when (and fp (nth (1+ fp) toks))
        (let ((a (parse-value "angle" (nth (1+ fp) toks))))
          (when (consp a) (setf from (angle->deg a))))))
    (list from pos)))

(defun parse-gradient (value fs)
  "Parse a linear/radial/conic-gradient() (and repeating-) into a structured form
\(see the header comment above), or NIL."
  (let* ((s (string-trim '(#\Space #\Tab #\Newline) value))
         (low (string-downcase s)))
    (multiple-value-bind (type repeating fname)
        (cond ((search "repeating-linear-gradient(" low) (values :linear t "repeating-linear-gradient("))
              ((search "linear-gradient(" low)           (values :linear nil "linear-gradient("))
              ((search "repeating-radial-gradient(" low) (values :radial t "repeating-radial-gradient("))
              ((search "radial-gradient(" low)           (values :radial nil "radial-gradient("))
              ((search "repeating-conic-gradient(" low)  (values :conic t "repeating-conic-gradient("))
              ((search "conic-gradient(" low)            (values :conic nil "conic-gradient("))
              (t (values nil nil nil)))
      (when type
        (let* ((p (search fname low))
               (open (+ p (length fname)))
               (close (matching-paren s (1- open)))
               (inner (and close (> close open) (subseq s open close))))
          (when inner
            (let* ((items (mapcar (lambda (x) (string-trim '(#\Space #\Tab #\Newline) x))
                                  (comma-split-top inner)))
                   (first-item (first items))
                   (is-cfg (and first-item (gconfig-p first-item)))
                   (cfg (and is-cfg (strip-color-interp first-item)))
                   (stop-items (if is-cfg (rest items) items))
                   (stops (parse-gstops stop-items type fs)))
              (when (>= (length stops) 1)
                (ecase type
                  (:linear (list :linear (if cfg (parse-linear-dir cfg) '(:angle 180.0))
                                 stops repeating))
                  (:radial (destructuring-bind (shape size pos)
                               (if cfg (parse-radial-cfg cfg fs)
                                   (list :ellipse '(:extent :farthest-corner) *default-gpos*))
                             (list :radial shape size pos stops repeating)))
                  (:conic (destructuring-bind (from pos)
                              (if cfg (parse-conic-cfg cfg fs) (list 0.0 *default-gpos*))
                            (list :conic from pos stops repeating))))))))))))

(defun gradient-stops (grad)
  "The color-stop list of a parsed gradient GRAD, by type."
  (case (first grad) (:linear (third grad)) (:radial (fifth grad)) (:conic (fourth grad))))

(defun gradient-solid-color (grad)
  "When GRAD's color stops are all one color (a `linear-gradient(C,C)` solid fill),
that color (r g b a); else NIL.  Used to fold a solid bottom background layer into
background-color so it paints under the upper image layers."
  (let ((cols (loop for s in (gradient-stops grad) when (eq (first s) :c) collect (second s))))
    (when (and cols (every (lambda (c) (equal c (first cols))) cols)) (first cols))))

(defun comma-split-top (s)
  "Split S on commas not inside parens."
  (let ((out '()) (depth 0) (start 0))
    (dotimes (i (length s))
      (case (char s i) (#\( (incf depth)) (#\) (decf depth))
        (#\, (when (zerop depth) (push (subseq s start i) out) (setf start (1+ i))))))
    (push (subseq s start) out) (nreverse out)))

(defun %read-css-string (v i)
  "Read a quoted string starting at index I in V (V[I] is the quote).  Returns
\(values decoded-string next-index)."
  (let ((q (char v i)) (n (length v)))
    (with-output-to-string (out)
      (let ((j (1+ i)))
        (loop while (and (< j n) (char/= (char v j) q)) do
          (when (and (char= (char v j) #\\) (< (1+ j) n)) (incf j))
          (write-char (char v j) out) (incf j))
        (return-from %read-css-string (values (get-output-stream-string out) (1+ j)))))))

(defun parse-content (value)
  "Parse a 'content' value into a generated STRING, a TEMPLATE (:tmpl seg ...) when
it references counter()/counters(), or NIL (none/normal/no box).  A template
segment is a literal string, (:counter name style) or (:counters name sep style);
it is resolved to a string once counter values are known (RESOLVE-COUNTERS).  CSS
escapes in quoted runs are already decoded by the tokenizer's string reader."
  (let ((v (string-trim '(#\Space #\Tab #\Newline) value)))
    (cond
      ((or (string-equal v "none") (string-equal v "normal")) nil)
      ((not (or (search "counter(" v :test #'char-equal)
                (search "counters(" v :test #'char-equal)
                (search "attr(" v :test #'char-equal)
                (search "open-quote" v :test #'char-equal)
                (search "close-quote" v :test #'char-equal)))
       ;; no counter reference: concatenated quoted strings -> a flat string
       ;; (attr()/url() and other bare tokens still yield an empty-but-present box).
       (if (and (plusp (length v)) (member (char v 0) '(#\' #\")))
           (with-output-to-string (out)
             (let ((i 0) (n (length v)))
               (loop while (< i n) do
                 (if (member (char v i) '(#\' #\"))
                     (multiple-value-bind (s j) (%read-css-string v i)
                       (write-string s out) (setf i j))
                     (incf i)))))
           ""))
      (t
       ;; template: walk the value emitting string / counter / counters segments.
       (let ((segs '()) (i 0) (n (length v)))
         (flet ((ci-prefix (p) (and (<= (+ i (length p)) n)
                                    (string-equal p (subseq v i (+ i (length p)))))))
           (loop while (< i n) do
             (let ((c (char v i)))
               (cond
                 ((member c '(#\' #\"))
                  (multiple-value-bind (s j) (%read-css-string v i)
                    (push s segs) (setf i j)))
                 ((ci-prefix "counters(")
                  (let* ((close (or (position #\) v :start i) n))
                         (args (subseq v (+ i 9) close))
                         (parts (split-counter-args args)))
                    ;; the counter NAME is case-sensitive (CSS 2.1 §12.4); only the
                    ;; optional list-style keyword is case-insensitive.
                    (push (list :counters (string-trim '(#\Space) (first parts))
                                (or (second parts) "")
                                (and (third parts) (string-downcase (string-trim '(#\Space) (third parts)))))
                          segs)
                    (setf i (1+ close))))
                 ((ci-prefix "counter(")
                  (let* ((close (or (position #\) v :start i) n))
                         (args (subseq v (+ i 8) close))
                         (parts (split-counter-args args)))
                    (push (list :counter (string-trim '(#\Space) (first parts))
                                (and (second parts) (string-downcase (string-trim '(#\Space) (second parts)))))
                          segs)
                    (setf i (1+ close))))
                 ((ci-prefix "attr(")
                  ;; attr(<name> [<type>]? [, <fallback>]?) — CSS 2.1 uses attr(name)
                  ;; yielding the attribute's string value (empty when absent).
                  (let* ((close (or (position #\) v :start i) n))
                         (args (subseq v (+ i 5) close))
                         (parts (split-counter-args args))
                         (head (string-trim '(#\Space #\Tab) (or (first parts) "")))
                         ;; strip an optional type keyword after the name
                         (name (subseq head 0 (or (position-if (lambda (c) (member c '(#\Space #\Tab))) head)
                                                  (length head))))
                         (fallback (and (second parts) (string-trim '(#\Space #\Tab) (second parts)))))
                    (push (list :attr name (or fallback "")) segs)
                    (setf i (1+ close))))
                 ((ci-prefix "no-open-quote")  (push '(:quote :no-open) segs)  (incf i 13))
                 ((ci-prefix "no-close-quote") (push '(:quote :no-close) segs) (incf i 14))
                 ((ci-prefix "open-quote")     (push '(:quote :open) segs)     (incf i 10))
                 ((ci-prefix "close-quote")    (push '(:quote :close) segs)    (incf i 11))
                 (t (incf i))))))
         (cons :tmpl (nreverse segs)))))))

(defun parse-quotes (value parent-cs)
  "Parse the 'quotes' property (CSS 2.1 §12.3.1): `none` -> #() (open/close-quote
emit nothing), `inherit` -> the parent's value, else a run of quoted strings
paired into a vector of (open . close) by nesting depth."
  (let ((v (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
    (cond
      ((string-equal v "inherit") (and parent-cs (cstyle-quotes parent-cs)))
      ((string-equal v "none") #())
      ((or (string-equal v "auto") (zerop (length v))) nil)
      (t (let ((strs '()) (i 0) (n (length v)))
           (loop while (< i n) do
             (if (member (char v i) '(#\' #\"))
                 (multiple-value-bind (s j) (%read-css-string v i) (push s strs) (setf i j))
                 (incf i)))
           (let* ((flat (nreverse strs)) (pairs '()))
             (loop while (cdr flat) do
               (push (cons (first flat) (second flat)) pairs) (setf flat (cddr flat)))
             (if pairs (coerce (nreverse pairs) 'vector) nil)))))))

(defun split-counter-args (s)
  "Split a counter()/counters() argument list S on top-level commas, honouring
quoted strings (so the separator string may contain a comma).  Returns a list of
trimmed argument strings; a quoted arg is returned decoded without its quotes."
  (let ((args '()) (i 0) (n (length s)) (start 0))
    (flet ((emit (end)
             (let ((a (string-trim '(#\Space #\Tab #\Newline) (subseq s start end))))
               (if (and (plusp (length a)) (member (char a 0) '(#\' #\")))
                   (push (%read-css-string a 0) args)
                   (push a args)))))
      (loop while (< i n) do
        (let ((c (char s i)))
          (cond ((member c '(#\' #\"))
                 (multiple-value-bind (str j) (%read-css-string s i)
                   (declare (ignore str)) (setf i j)))
                ((char= c #\,) (emit i) (setf start (1+ i)) (incf i))
                (t (incf i)))))
      (emit n)
      (nreverse args))))

(defun parse-counter-ops (value)
  "Parse a `counter-reset`/`counter-increment` value into an alist of
\(name . integer).  `none` -> NIL.  A bare name defaults to DEFAULT (0 for reset,
1 for increment), applied by the caller."
  (let ((v (string-trim '(#\Space #\Tab #\Newline) value)))
    (when (and (plusp (length v)) (not (string-equal v "none")))
      (let ((toks (remove "" (uiop:split-string v :separator '(#\Space #\Tab #\Newline)) :test #'string=))
            (ops '()) (i 0))
        (loop while (< i (length toks)) do
          (let* ((name (nth i toks))
                 (next (and (< (1+ i) (length toks)) (nth (1+ i) toks)))
                 (num (and next (ignore-errors (parse-integer next)))))
            (if num (progn (push (cons name num) ops) (incf i 2))
                (progn (push (cons name :default) ops) (incf i)))))
        (nreverse ops)))))

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
                    ((member kl '("italic" "oblique") :test #'string=) (setf (cstyle-font-style cs) kl))
                    ((every #'digit-char-p kl) (setf (cstyle-font-weight cs) (parse-integer kl))))))
          ;; trailing tokens after size[/lh] are the font-family list
          (let ((fam-toks (subseq toks (1+ size-pos))))
            (when fam-toks
              (let ((v (parse-value "font-family" (format nil "~{~a~^ ~}" fam-toks))))
                (when (and (listp v) v) (setf (cstyle-font-family cs) v)))))
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

(defun split-slash (s)
  "Split S on '/' into substrings."
  (loop with start = 0 for i from 0 to (length s)
        when (or (= i (length s)) (char= (char s i) #\/))
          collect (prog1 (subseq s start i) (setf start (1+ i)))))

(defun parse-grid-areas (value)
  "Parse a `grid-template-areas` value ('gutter code' ...) into a hash NAME ->
(r0 c0 rspan cspan), 0-based (`.` cells are skipped), or NIL if it names nothing."
  (let ((rows '()) (i 0) (n (length value)))
    (loop while (< i n) do
      (let ((q (or (position #\' value :start i) (position #\" value :start i))))
        (if (null q) (return)
            (let ((end (position (char value q) value :start (1+ q))))
              (if (null end) (return)
                  (progn (push (split-ws (subseq value (1+ q) end)) rows) (setf i (1+ end))))))))
    (setf rows (nreverse rows))
    (let ((box (make-hash-table :test 'equal)))
      (loop for row in rows for r from 0 do
        (loop for name in row for c from 0 do
          (unless (string= name ".")
            (let ((b (gethash name box)))
              (setf (gethash name box)
                    (if b (list (min (first b) r) (min (second b) c) (max (third b) r) (max (fourth b) c))
                        (list r c r c)))))))
      (when (plusp (hash-table-count box))
        (let ((map (make-hash-table :test 'equal)))
          (maphash (lambda (k b)
                     (setf (gethash k map) (list (first b) (second b)
                                                 (1+ (- (third b) (first b))) (1+ (- (fourth b) (second b))))))
                   box)
          map)))))

(defun normalize-display (value)
  "Map a CSS Display 3 (§2.1) display value to weft's single-keyword model.  A
single legacy keyword (block, inline-block, flex, none, table-cell, contents, …)
passes through unchanged; a multi-keyword form <display-outside> <display-inside>
[|| list-item] is folded to the nearest legacy keyword — e.g. `inline flow-root`
-> inline-block, `block flex` -> flex, `inline table` -> inline-table, and any
`… list-item` -> list-item."
  (let ((toks (remove "" (split-ws (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
                      :test #'string=)))
    (cond
      ((null toks) "inline")
      ((null (cdr toks)) (first toks))                     ; single keyword: unchanged
      ((member "list-item" toks :test #'string=) "list-item")
      (t (let ((inline (member "inline" toks :test #'string=))
               (inside (cond ((member "flow-root" toks :test #'string=) "flow-root")
                             ((member "flex" toks :test #'string=) "flex")
                             ((member "grid" toks :test #'string=) "grid")
                             ((member "table" toks :test #'string=) "table")
                             (t "flow"))))
           (cond ((string= inside "flow")      (if inline "inline" "block"))
                 ((string= inside "flow-root") (if inline "inline-block" "flow-root"))
                 (inline (concatenate 'string "inline-" inside))
                 (t inside)))))))

(defun logical-box-remap (prop)
  "Map a CSS Logical 1 box property to physical one(s) for the default
horizontal-tb LTR flow: inline = horizontal (left/right), block = vertical
(top/bottom); *-start/-end map to left/top and right/bottom.  Returns
 (values MAPPED MODE): MAPPED a physical prop string (MODE :single) or a
 (P1 P2) pair with MODE :split (1-2 values split across the sides) or :dup
 (the whole value applied to both).  NIL when PROP is not a logical box prop."
  (macrolet ((pair (a b mode) `(values (list ,a ,b) ,mode)))
    (cond
      ((string= prop "margin-inline")  (pair "margin-left" "margin-right" :split))
      ((string= prop "margin-block")   (pair "margin-top" "margin-bottom" :split))
      ((string= prop "padding-inline") (pair "padding-left" "padding-right" :split))
      ((string= prop "padding-block")  (pair "padding-top" "padding-bottom" :split))
      ((string= prop "inset-inline")   (pair "left" "right" :split))
      ((string= prop "inset-block")    (pair "top" "bottom" :split))
      ((string= prop "border-inline")  (pair "border-left" "border-right" :dup))
      ((string= prop "border-block")   (pair "border-top" "border-bottom" :dup))
      ((string= prop "margin-inline-start")  (values "margin-left" :single))
      ((string= prop "margin-inline-end")    (values "margin-right" :single))
      ((string= prop "margin-block-start")   (values "margin-top" :single))
      ((string= prop "margin-block-end")     (values "margin-bottom" :single))
      ((string= prop "padding-inline-start") (values "padding-left" :single))
      ((string= prop "padding-inline-end")   (values "padding-right" :single))
      ((string= prop "padding-block-start")  (values "padding-top" :single))
      ((string= prop "padding-block-end")    (values "padding-bottom" :single))
      ((string= prop "border-inline-start")  (values "border-left" :single))
      ((string= prop "border-inline-end")    (values "border-right" :single))
      ((string= prop "border-block-start")   (values "border-top" :single))
      ((string= prop "border-block-end")     (values "border-bottom" :single))
      ((string= prop "inset-inline-start")   (values "left" :single))
      ((string= prop "inset-inline-end")     (values "right" :single))
      ((string= prop "inset-block-start")    (values "top" :single))
      ((string= prop "inset-block-end")      (values "bottom" :single))
      (t nil))))

;;;; ---- border-radius (CSS Backgrounds 3 §5.5) ------------------------------
(defun parse-radius-comp (str fs)
  "One border-radius length component -> px float (>=0) | (:percent N>=0) | NIL
\(unparseable/negative -> NIL, which invalidates the whole declaration)."
  (let ((tt (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return) str))))
    (cond ((zerop (length tt)) nil)
          ((char= (char tt (1- (length tt))) #\%)
           (let ((n (ignore-errors
                      (let ((*read-eval* nil)) (read-from-string (subseq tt 0 (1- (length tt))) nil nil)))))
             (and (realp n) (>= n 0) (list :percent (float n 1.0)))))
          (t (let ((v (resolve-len tt fs)))
               (and (numberp v) (>= v 0) (float v 1.0)))))))

(defun %radius-pick (lst i)
  "The i-th (0=TL 1=TR 2=BR 3=BL) component of a 1-4 element radius LST, per the
CSS shorthand replication rules (1->all; 2->TL/BR,TR/BL; 3->TL,TR/BL,BR)."
  (case (length lst)
    (1 (first lst))
    (2 (if (member i '(0 2)) (first lst) (second lst)))
    (3 (case i (0 (first lst)) ((1 3) (second lst)) (2 (third lst))))
    (4 (nth i lst))))

(defun apply-border-radius (cs value fs)
  "Apply the `border-radius` shorthand: 1-4 horizontal radii, optionally `/` then
1-4 vertical radii (default = horizontal).  Sets the four corner (H . V) pairs."
  (let* ((slash (position #\/ value))
         (hstr (if slash (subseq value 0 slash) value))
         (vstr (and slash (subseq value (1+ slash))))
         (h (mapcar (lambda (s) (parse-radius-comp s fs)) (split-tokens (string-trim '(#\Space) hstr))))
         (v (and vstr (mapcar (lambda (s) (parse-radius-comp s fs)) (split-tokens (string-trim '(#\Space) vstr))))))
    (when (and h (<= 1 (length h) 4) (notany #'null h)
               (or (null vstr) (and v (<= 1 (length v) 4) (notany #'null v))))
      (let ((hv (or v h)))
        (setf (cstyle-border-tl-radius cs) (cons (%radius-pick h 0) (%radius-pick hv 0))
              (cstyle-border-tr-radius cs) (cons (%radius-pick h 1) (%radius-pick hv 1))
              (cstyle-border-br-radius cs) (cons (%radius-pick h 2) (%radius-pick hv 2))
              (cstyle-border-bl-radius cs) (cons (%radius-pick h 3) (%radius-pick hv 3)))))))

(defun radius-inherit-p (value)
  (string-equal (string-trim '(#\Space #\Tab #\Newline #\Return) value) "inherit"))

(defun apply-corner-radius (cs setter value fs)
  "Apply a per-corner longhand (border-*-*-radius): 1 or 2 components (H [V])."
  (let* ((toks (split-tokens (string-trim '(#\Space) value)))
         (h (parse-radius-comp (or (first toks) "") fs))
         (v (if (second toks) (parse-radius-comp (second toks) fs) h)))
    (when (and h v) (funcall setter (cons h v) cs))))

(defun apply-decl (cs prop value parent-cs)
  "Apply one declaration to CSTYLE CS (best-effort)."
  ;; The element's font-family (inherited at init, or already set by an earlier `font`
  ;; declaration) drives ex/ch resolution for this declaration's lengths.
  (let ((*resolve-family* (cstyle-font-family cs)))
  ;; Logical box properties (margin/padding/border/inset -inline/-block[-start/end])
  ;; expand to their physical longhands before dispatch.
  (multiple-value-bind (mapped mode) (logical-box-remap prop)
    (when mapped
      (ecase mode
        (:single (apply-decl cs mapped value parent-cs))
        (:dup (apply-decl cs (first mapped) value parent-cs)
              (apply-decl cs (second mapped) value parent-cs))
        (:split (let ((toks (split-tokens (string-trim '(#\Space) value))))
                  (when toks
                    (apply-decl cs (first mapped) (first toks) parent-cs)
                    (apply-decl cs (second mapped) (or (second toks) (first toks)) parent-cs)))))
      (return-from apply-decl)))
  ;; Logical sizing properties (CSS Logical 1 §4.1) resolve to physical ones in the
  ;; default horizontal-tb writing mode, which covers effectively all content:
  ;; inline-size is width, block-size is height (and their min-/max- forms).
  (setf prop (cond ((string= prop "inline-size") "width")
                   ((string= prop "block-size") "height")
                   ((string= prop "min-inline-size") "min-width")
                   ((string= prop "max-inline-size") "max-width")
                   ((string= prop "min-block-size") "min-height")
                   ((string= prop "max-block-size") "max-height")
                   (t prop)))
  ;; Generic `inherit` for the box-size longhands (not otherwise inherited):
  ;; copy the parent's computed value (CSS 2.1 §6.2.1).
  (when (and parent-cs
             (string-equal (string-trim '(#\Space #\Tab #\Newline) value) "inherit")
             (member prop '("width" "height" "min-width" "max-width" "min-height" "max-height")
                     :test #'string=))
    (cond ((string= prop "width")      (setf (cstyle-width cs)      (cstyle-width parent-cs)))
          ((string= prop "height")     (setf (cstyle-height cs)     (cstyle-height parent-cs)))
          ((string= prop "min-width")  (setf (cstyle-min-width cs)  (cstyle-min-width parent-cs)))
          ((string= prop "max-width")  (setf (cstyle-max-width cs)  (cstyle-max-width parent-cs)))
          ((string= prop "min-height") (setf (cstyle-min-height cs) (cstyle-min-height parent-cs)))
          ((string= prop "max-height") (setf (cstyle-max-height cs) (cstyle-max-height parent-cs))))
    (return-from apply-decl))
  (let ((fs (cstyle-font-size cs)))
    (macrolet ((len (&optional auto) `(resolve-len value fs ,auto)))
      (cond
        ((string= prop "display")
         (setf (cstyle-display cs)
               (if (and parent-cs (string-equal (string-trim '(#\Space #\Tab #\Newline #\Return) value) "inherit"))
                   (cstyle-display parent-cs)
                   (normalize-display value))))
        ((string= prop "opacity")
         (let ((v (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
           (cond ((and parent-cs (string-equal v "inherit")) (setf (cstyle-opacity cs) (cstyle-opacity parent-cs)))
                 ((member v '("initial" "unset" "revert") :test #'string-equal) (setf (cstyle-opacity cs) 1.0))
                 (t (let ((o (compute-opacity v fs))) (when o (setf (cstyle-opacity cs) o)))))))
        ((string= prop "content") (setf (cstyle-content cs) (parse-content value)))
        ((string= prop "quotes") (setf (cstyle-quotes cs) (parse-quotes value parent-cs)))
        ((string= prop "counter-reset")
         (setf (cstyle-counter-reset cs)
               (mapcar (lambda (op) (cons (car op) (if (eq (cdr op) :default) 0 (cdr op))))
                       (parse-counter-ops value))))
        ((string= prop "counter-increment")
         (setf (cstyle-counter-increment cs)
               (mapcar (lambda (op) (cons (car op) (if (eq (cdr op) :default) 1 (cdr op))))
                       (parse-counter-ops value))))
        ((string= prop "color")
         (if (string-equal (string-trim '(#\Space) value) "inherit")
             (when parent-cs (setf (cstyle-color cs) (cstyle-color parent-cs)))   ; a{color:inherit} resets the UA link colour
             (let ((c (resolve-color value))) (when c (setf (cstyle-color cs) c)))))
        ((member prop '("background-color" "background" "background-image") :test #'string=)
         (let ((grad (parse-gradient value fs))
               (url (extract-css-url value))
               (tok (string-downcase (string-trim '(#\Space) (first-token value)))))
           ;; CSS 2.1 14.2.1 / 4.2: a `background` (or `background-color`)
           ;; declaration whose value carries two bare <color> tokens is invalid
           ;; and the whole declaration must be dropped, leaving any earlier
           ;; background intact (Acid2's `.parser { background: red pink; }` must
           ;; NOT override the prior `background: yellow`, or the parser box shows
           ;; the red error fill).  A url()/gradient background legitimately pairs
           ;; one colour with other tokens, so only count bare colour tokens.
           (when (and (not grad)
                      (>= (count-if (lambda (tk) (resolve-color tk))
                                    (remove "url" (css-background-tokens value) :test #'string=))
                          2))
             (return-from apply-decl))
           (cond (grad (setf (cstyle-bg-gradient cs) grad)
                       ;; multi-layer background (comma list): PARSE-GRADIENT returned the
                       ;; FIRST (top) layer.  When the LAST (bottom) layer is a solid
                       ;; gradient, fold its color into background-color so it paints under
                       ;; the top layer(s) — the common "image over a base color" idiom.
                       (let ((layers (split-top-commas value)))
                         (when (> (length layers) 1)
                           (let* ((lg (parse-gradient (car (last layers)) fs))
                                  (base (and lg (gradient-solid-color lg))))
                             (when base (setf (cstyle-background cs) base))))))
                 ;; `none`/`transparent` clear any background set by an earlier rule
                 ((member tok '("none" "transparent") :test #'string=)
                  (setf (cstyle-background cs) nil (cstyle-bg-gradient cs) nil (cstyle-bg-image cs) nil))
                 ;; the colour can sit anywhere in the shorthand, not just first —
                 ;; `background: url(…) no-repeat 1px white` still sets the bg colour.
                 (t (let ((c (some #'resolve-color (css-background-tokens value))))
                      (when c (setf (cstyle-background cs) c)))))
           ;; layer count = comma-separated groups of background-image (each layer),
           ;; so background-color uses the bottom (last) layer's background-clip.
           (when (member prop '("background" "background-image") :test #'string=)
             (setf (cstyle-bg-layers cs) (max 1 (length (split-top-commas value)))))
           ;; capture a url() image (data: URI) from `background`/`background-image`
           (when (or url (and grad (string= prop "background")))
             (when url (setf (cstyle-bg-image cs) url))
             (when (string= prop "background")
               ;; pull repeat/attachment keywords out of the shorthand
               (let ((toks (css-background-tokens value)))
                 (when (member "fixed" toks :test #'string=) (setf (cstyle-bg-attachment cs) "fixed"))
                 (let ((r (find-if (lambda (tk) (member tk '("repeat" "repeat-x" "repeat-y" "no-repeat") :test #'string=)) toks)))
                   (when r (setf (cstyle-bg-repeat cs) r)))
                 ;; box keywords in the shorthand: first = origin, second = clip;
                 ;; a lone box sets both (CSS Backgrounds 3 §2.1 shorthand).
                 (let ((boxes (remove-if-not (lambda (tk) (member tk '("border-box" "padding-box" "content-box") :test #'string=)) toks)))
                   (when boxes
                     (setf (cstyle-bg-origin cs) (first boxes)
                           (cstyle-bg-clip cs) (or (second boxes) (first boxes)))))
                 ;; pull a background-position [ / background-size ] out of the
                 ;; shorthand's first layer (CSS Backgrounds 3 §3.10 <bg-position>
                 ;; [ / <bg-size> ]).  Strip url() first so a slash inside
                 ;; url(path/img.png) isn't read as the position/size separator.
                 (let* ((layer1 (or (first (split-top-commas value)) value))
                        (up (search "url(" layer1 :test #'char-equal))
                        (layer1 (if up
                                    (let ((e (position #\) layer1 :start (+ up 4))))
                                      (if e (concatenate 'string (subseq layer1 0 up) " "
                                                         (subseq layer1 (1+ e)))
                                          layer1))
                                    layer1))
                        (slash (position #\/ layer1))
                        (pos-str (if slash (subseq layer1 0 slash) layer1))
                        (size-str (and slash (subseq layer1 (1+ slash)))))
                   (let ((postoks (remove-if-not #'bg-position-token-p
                                                 (css-background-tokens pos-str))))
                     (when postoks
                       (let ((v (parse-value "background-position"
                                             (format nil "~{~a~^ ~}" postoks))))
                         (when (and (consp v) (not (eq v :invalid)))
                           (setf (cstyle-bg-position cs) (resolve-bg-pos v fs))))))
                   (when size-str
                     ;; the tokens right after `/` form <bg-size>: contain|cover (one
                     ;; token) or up to two of <length-percentage>|auto; stop at the
                     ;; first non-size keyword (repeat, attachment, box, ...).
                     (let* ((stoks (css-background-tokens size-str))
                            (size-toks
                              (cond ((and stoks (member (first stoks) '("contain" "cover")
                                                        :test #'string=))
                                     (list (first stoks)))
                                    (t (loop for tk in stoks
                                             while (bg-size-comp tk fs)
                                             repeat 2 collect tk)))))
                       (when size-toks
                         (let ((sz (parse-bg-size (format nil "~{~a~^ ~}" size-toks) fs)))
                           (when sz (setf (cstyle-bg-size cs) sz))))))))))))
        ((string= prop "background-repeat")
         (let ((v (parse-value "background-repeat" value))) (when (stringp v) (setf (cstyle-bg-repeat cs) v))))
        ((string= prop "background-position")
         ;; weft paints one layer: a comma-separated multi-layer value uses its first.
         (let ((v (parse-value "background-position" (or (first (split-top-commas value)) value))))
           (when (and (consp v) (not (eq v :invalid))) (setf (cstyle-bg-position cs) (resolve-bg-pos v fs)))))
        ((string= prop "background-size") (setf (cstyle-bg-size cs) (parse-bg-size value fs)))
        ((string= prop "background-attachment")
         ;; weft paints one layer: use the first layer's attachment keyword.
         (setf (cstyle-bg-attachment cs)
               (string-downcase (string-trim '(#\Space) (or (first (split-top-commas value)) value)))))
        ((string= prop "background-origin")
         (let ((v (string-downcase (string-trim '(#\Space) value))))
           (when (member v '("border-box" "padding-box" "content-box") :test #'string=)
             (setf (cstyle-bg-origin cs) v))))
        ((string= prop "background-clip")
         (if (and parent-cs (string-equal (string-trim '(#\Space) value) "inherit"))
             ;; background-clip is not inherited by default; the `inherit` keyword
             ;; copies the parent's computed value (CSS Backgrounds 3 §2.2, CSS Cascade).
             (setf (cstyle-bg-clip cs) (cstyle-bg-clip parent-cs)
                   (cstyle-bg-clip-list cs) (cstyle-bg-clip-list parent-cs))
             (let ((vals (mapcar (lambda (p) (string-downcase (string-trim '(#\Space) p)))
                                 (split-top-commas value))))
               (when (and vals (every (lambda (v) (member v '("border-box" "padding-box" "content-box") :test #'string=)) vals))
                 (setf (cstyle-bg-clip cs) (first vals)
                       (cstyle-bg-clip-list cs) (if (cdr vals) vals nil))))))
        ((string= prop "object-fit")
         (let ((v (parse-value "object-fit" value))) (when (stringp v) (setf (cstyle-object-fit cs) v))))
        ((string= prop "aspect-ratio")
         (let ((v (parse-value "aspect-ratio" value)))
           (cond ((numberp v) (setf (cstyle-aspect-ratio cs) v (cstyle-aspect-ratio-auto cs) nil))
                 ;; (:auto . ratio): the ratio applies to the content box (CSS Sizing 4)
                 ((and (consp v) (eq (car v) :auto))
                  (setf (cstyle-aspect-ratio cs) (cdr v) (cstyle-aspect-ratio-auto cs) t))
                 ;; :auto (no ratio) / :invalid — no explicit preferred ratio
                 (t (setf (cstyle-aspect-ratio cs) nil (cstyle-aspect-ratio-auto cs) nil)))))
        ((string= prop "font-size")
         (let ((base (if parent-cs (cstyle-font-size parent-cs) 16.0)))
           (cond ((search "%" value) (let ((p (parse-value "percentage" value))) (when (numberp p) (setf (cstyle-font-size cs) (* base (/ p 100.0))))))
                 (t (let ((px (resolve-len value base))) (when px (setf (cstyle-font-size cs) px)))))))
        ((string= prop "font") (apply-font-shorthand cs value parent-cs))
        ((string= prop "font-family")
         (let ((v (parse-value "font-family" value)))
           (when (and (listp v) v) (setf (cstyle-font-family cs) v))))
        ((string= prop "font-style")
         (let ((v (parse-value "font-style" value)))
           (when (stringp v) (setf (cstyle-font-style cs) v))))
        ((string= prop "font-weight")
         (setf (cstyle-font-weight cs)
               (cond ((string-equal value "bold") 700) ((string-equal value "normal") 400)
                     ((ignore-errors (parse-integer (string-trim '(#\Space) value)))) (t 400))))
        ((string= prop "line-height") (let ((m (line-height-multiplier value (cstyle-font-size cs)))) (when m (setf (cstyle-line-height cs) m))))
        ((string= prop "text-align") (setf (cstyle-text-align cs) (string-downcase (string-trim '(#\Space) value))))
        ((member prop '("text-decoration" "text-decoration-line") :test #'string=)
         (if (string-equal (string-trim '(#\Space) value) "inherit")
             (when parent-cs (setf (cstyle-text-decoration cs) (cstyle-text-decoration parent-cs)))   ; a{text-decoration:inherit} drops the UA underline
             (let ((v (parse-value "text-decoration" value))) (when (listp v) (setf (cstyle-text-decoration cs) v)))))
        ((string= prop "list-style-type")
         (let ((v (parse-value "list-style-type" value))) (when (stringp v) (setf (cstyle-list-style cs) v))))
        ;; list-style shorthand: pick out the <list-style-type> keyword (position and
        ;; image components are ignored); `none` sets the marker type to none.
        ((string= prop "list-style")
         (dolist (tok (remove "" (split-ws (string-downcase value)) :test #'string=))
           (when (member tok '("none" "disc" "circle" "square" "decimal" "decimal-leading-zero"
                               "lower-alpha" "upper-alpha" "lower-latin" "upper-latin"
                               "lower-roman" "upper-roman" "lower-greek") :test #'string=)
             (setf (cstyle-list-style cs) tok))))
        ((string= prop "white-space")
         ;; validate against the keyword grammar so an invalid value (e.g. a
         ;; later `white-space: x-bogus`) is ignored and the last VALID value wins.
         (let ((v (parse-value "white-space" value))) (when (stringp v) (setf (cstyle-white-space cs) v))))
        ((string= prop "cursor")
         (let ((v (parse-value "cursor" value))) (when (stringp v) (setf (cstyle-cursor cs) v))))
        ((string= prop "text-transform")
         (let ((v (parse-value "text-transform" value))) (when (stringp v) (setf (cstyle-text-transform cs) v))))
        ((string= prop "hyphens")
         (let ((v (parse-value "hyphens" value))) (when (stringp v) (setf (cstyle-hyphens cs) v))))
        ((string= prop "vertical-align")
         (let ((v (parse-value "vertical-align" value))) (unless (eq v :invalid) (setf (cstyle-vertical-align cs) v))))
        ((string= prop "visibility")
         (let ((v (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return) value))))
           (when (member v '("visible" "hidden" "collapse") :test #'string=)
             (setf (cstyle-visibility cs) v))))
        ((string= prop "caption-side")
         (let ((v (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return) value))))
           (when (member v '("top" "bottom") :test #'string=)
             (setf (cstyle-caption-side cs) v))))
        ((member prop '("letter-spacing" "word-spacing") :test #'string=)
         (let ((v (if (string-equal (string-trim '(#\Space #\Tab #\Newline #\Return) value) "normal") 0.0
                      (parse-size value fs nil))))     ; a <length>; `normal` is 0
           (when (numberp v)
             (if (string= prop "letter-spacing")
                 (setf (cstyle-letter-spacing cs) (float v))
                 (setf (cstyle-word-spacing cs) (float v))))))
        ((string= prop "text-indent")
         (let ((v (parse-size value fs t)))  ; <length> or (:percent N)
           (when (or (numberp v) (consp v))
             (setf (cstyle-text-indent cs) v))))
        ((member prop '("overflow-wrap" "word-wrap") :test #'string=)  ; word-wrap is the legacy alias
         (let ((v (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return) value))))
           (when (member v '("normal" "break-word" "anywhere") :test #'string=)
             (setf (cstyle-overflow-wrap cs) v))))
        ((string= prop "word-break")
         (let ((v (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return) value))))
           (when (member v '("normal" "break-all" "keep-all") :test #'string=)
             (setf (cstyle-word-break cs) v))))
        ((string= prop "width") (let ((w (parse-size value fs t))) (when (and w (size-nonneg-p w)) (setf (cstyle-width cs) w))))
        ((string= prop "height") (let ((h (parse-size value fs t))) (when (and h (size-nonneg-p h)) (setf (cstyle-height cs) h))))
        ((string= prop "max-width") (if (string-equal (string-trim '(#\Space) value) "none") (setf (cstyle-max-width cs) :none)
                                        (let ((w (parse-size value fs nil))) (when (and w (size-nonneg-p w)) (setf (cstyle-max-width cs) w)))))
        ((string= prop "min-width") (let ((w (parse-size value fs nil))) (when (and w (size-nonneg-p w)) (setf (cstyle-min-width cs) w))))
        ((string= prop "min-height") (let ((h (parse-size value fs nil)))   ; px or (:percent N)
                                       (when (and (or (numberp h) (consp h)) (size-nonneg-p h)) (setf (cstyle-min-height cs) h))))
        ((string= prop "max-height") (if (string-equal (string-trim '(#\Space) value) "none") (setf (cstyle-max-height cs) :none)
                                         (let ((h (parse-size value fs nil)))   ; px or (:percent N)
                                           (when (and (or (numberp h) (consp h)) (size-nonneg-p h)) (setf (cstyle-max-height cs) h)))))
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
         (flet ((ovkw (tok) (let ((v (parse-value "overflow" tok))) (and (stringp v) v))))
           (cond
             ((string= prop "overflow-x")
              (let ((v (ovkw value))) (when v (setf (cstyle-overflow-x cs) v))))
             ((string= prop "overflow-y")
              (let ((v (ovkw value))) (when v (setf (cstyle-overflow-y cs) v))))
             (t ;; `overflow` shorthand: one or two <visible|hidden|clip|scroll|auto>
              (let* ((toks (remove "" (split-ws (string-downcase (string-trim '(#\Space) value))) :test #'string=))
                     (x (ovkw (or (first toks) "")))
                     (y (ovkw (or (second toks) (first toks) ""))))
                (when x (setf (cstyle-overflow-x cs) x))
                (when y (setf (cstyle-overflow-y cs) y)))))
           ;; recompute the combined value: non-visible when EITHER axis clips.
           (flet ((vis (o) (member o '("visible" nil) :test #'equal)))
             (let ((x (cstyle-overflow-x cs)) (y (cstyle-overflow-y cs)))
               (setf (cstyle-overflow cs)
                     (cond ((and (vis x) (vis y)) "visible")
                           ((not (vis y)) y)
                           (t x)))))))
        ((string= prop "overflow-clip-margin")
         (let ((box :padding) (len 0.0))
           (dolist (tok (split-tokens (string-downcase (string-trim '(#\Space) value))))
             (cond ((string= tok "content-box") (setf box :content))
                   ((string= tok "padding-box") (setf box :padding))
                   ((string= tok "border-box") (setf box :border))
                   (t (let ((px (resolve-len tok fs))) (when (and (numberp px) (>= px 0)) (setf len px))))))
           (setf (cstyle-overflow-clip-margin cs) (cons box len))))
        ((string= prop "flex-direction") (setf (cstyle-flex-direction cs) (string-downcase (string-trim '(#\Space) value))))
        ((string= prop "flex-wrap") (setf (cstyle-flex-wrap cs) (string-downcase (string-trim '(#\Space) value))))
        ((string= prop "flex-flow")   ; shorthand: <flex-direction> and/or <flex-wrap>
         (dolist (tok (remove "" (split-ws (string-downcase (string-trim '(#\Space) value))) :test #'string=))
           (cond ((member tok '("row" "row-reverse" "column" "column-reverse") :test #'string=)
                  (setf (cstyle-flex-direction cs) tok))
                 ((member tok '("nowrap" "wrap" "wrap-reverse") :test #'string=)
                  (setf (cstyle-flex-wrap cs) tok)))))
        ((string= prop "justify-content") (setf (cstyle-justify-content cs) (string-downcase (string-trim '(#\Space) value))))
        ((string= prop "align-items") (setf (cstyle-align-items cs) (string-downcase (string-trim '(#\Space) value))))
        ((string= prop "align-content") (setf (cstyle-align-content cs) (string-downcase (string-trim (list #\Space) value))))
        ((member prop '("justify-items" "justify-self" "align-self") :test #'string=)
         ;; grid box-alignment keywords; `normal` maps to stretch (the grid default).
         (let ((v (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return) value))))
           (when (string= v "normal") (setf v (if (string= prop "justify-items") "stretch" "auto")))
           (when (member v '("stretch" "start" "center" "end" "flex-start" "flex-end" "self-start" "self-end" "auto") :test #'string=)
             (cond ((string= prop "justify-items") (setf (cstyle-justify-items cs) v))
                   ((string= prop "justify-self") (setf (cstyle-justify-self cs) v))
                   (t (setf (cstyle-align-self cs) v))))))
        ((member prop '("grid-template-columns" "grid-template-rows" "grid-auto-rows" "grid-auto-columns") :test #'string=)
         ;; keep the raw track-list string; parse-track-list expands it at layout time.
         (let ((v (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return) value))))
           (unless (member v '("none" "" "auto") :test #'string=)
             (cond ((string= prop "grid-template-columns") (setf (cstyle-grid-template-columns cs) v))
                   ((string= prop "grid-template-rows") (setf (cstyle-grid-template-rows cs) v))
                   ((string= prop "grid-auto-columns") (setf (cstyle-grid-auto-columns cs) v))
                   (t (setf (cstyle-grid-auto-rows cs) v))))))
        ((string= prop "grid-template-areas")
         (let ((m (parse-grid-areas value)))
           (when m (setf (cstyle-grid-template-areas cs) m))))
        ((string= prop "grid-area")
         ;; `grid-area: <name>` (an ident referencing grid-template-areas) sets the
         ;; area name; the line-based form (`r / c / r2 / c2`) folds into row/column.
         (let ((v (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
           (if (find #\/ v)
               (let* ((p (split-slash v))
                      (rc (mapcar (lambda (s) (string-trim '(#\Space) s)) p)))
                 (when (>= (length rc) 2)
                   (setf (cstyle-grid-row cs) (format nil "~a / ~a" (first rc) (or (third rc) "auto"))
                         (cstyle-grid-column cs) (format nil "~a / ~a" (second rc) (or (fourth rc) "auto")))))
               (unless (member (string-downcase v) '("auto" "none" "") :test #'string=)
                 (setf (cstyle-grid-area cs) v)))))
        ((member prop '("grid-column" "grid-row") :test #'string=)
         (let ((v (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return) value))))
           (if (string= prop "grid-column") (setf (cstyle-grid-column cs) v) (setf (cstyle-grid-row cs) v))))
        ((member prop '("grid-column-start" "grid-column-end" "grid-row-start" "grid-row-end") :test #'string=)
         ;; fold the *-start/-end longhands into the `grid-column`/`grid-row` shorthand slot.
         (let* ((v (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
                (row (or (string= prop "grid-row-start") (string= prop "grid-row-end")))
                (end (or (string= prop "grid-column-end") (string= prop "grid-row-end")))
                (cur (or (if row (cstyle-grid-row cs) (cstyle-grid-column cs)) "auto / auto"))
                ;; split on the '/' (which may have no surrounding spaces — Tailwind
                ;; emits `grid-column:1/-1`), so an existing end line survives folding
                ;; in a start (e.g. col-span-full's -1 kept when col-start-5 sets start 5).
                (slash (position #\/ cur))
                (a (string-trim " " (if slash (subseq cur 0 slash) cur)))
                (b (if slash (string-trim " " (subseq cur (1+ slash))) "auto"))
                (new (if end (format nil "~a / ~a" a v) (format nil "~a / ~a" v b))))
           (if row (setf (cstyle-grid-row cs) new) (setf (cstyle-grid-column cs) new))))
        ((string= prop "grid-auto-flow")
         (let ((v (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return) value))))
           (when (search "column" v) (setf (cstyle-grid-auto-flow cs) "column"))
           (when (search "row" v) (setf (cstyle-grid-auto-flow cs) "row"))))
        ((string= prop "gap")   ; row-gap [column-gap]; single value applies to both
         (let* ((parts (remove "" (split-ws (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return) value))) :test #'string=))
                (r (and parts (resolve-len (first parts) fs)))
                (c (if (and (cdr parts) (resolve-len (second parts) fs)) (resolve-len (second parts) fs) r)))
           (when r (setf (cstyle-row-gap cs) r (cstyle-gap cs) r))
           (when c (setf (cstyle-column-gap cs) c))))
        ((string= prop "row-gap") (let ((v (len))) (when v (setf (cstyle-row-gap cs) v (cstyle-gap cs) v))))
        ((string= prop "column-gap")
         (let ((v (string-downcase (string-trim '(#\Space) value))))
           (cond ((string= v "normal") (setf (cstyle-column-gap cs) fs))   ; multicol normal gap = 1em
                 ((len) (setf (cstyle-column-gap cs) (len) (cstyle-gap cs) (len))))))
        ((string= prop "column-count")
         (let ((v (string-downcase (string-trim '(#\Space) value))))
           (setf (cstyle-column-count cs)
                 (if (string= v "auto") nil (ignore-errors (parse-integer v :junk-allowed t))))))
        ((string= prop "column-width")
         (let ((v (string-downcase (string-trim '(#\Space) value))))
           (setf (cstyle-column-width cs) (if (string= v "auto") nil (resolve-len v fs)))))
        ((string= prop "column-fill")
         (let ((v (string-downcase (string-trim '(#\Space) value))))
           (when (member v '("balance" "auto") :test #'string=) (setf (cstyle-column-fill cs) v))))
        ((string= prop "columns")   ; shorthand: <column-width> || <column-count>
         (let ((parts (remove "" (split-ws (string-downcase (string-trim '(#\Space) value))) :test #'string=)))
           (dolist (p parts)
             (cond ((string= p "auto"))
                   ((every #'digit-char-p p) (setf (cstyle-column-count cs) (parse-integer p)))
                   ((resolve-len p fs) (setf (cstyle-column-width cs) (resolve-len p fs)))))))
        ((string= prop "writing-mode")
         (let ((v (string-downcase (string-trim '(#\Space) value))))
           (when (member v '("horizontal-tb" "vertical-rl" "vertical-lr") :test #'string=)
             (setf (cstyle-writing-mode cs) v))))
        ((string= prop "direction")
         (let ((v (string-downcase (string-trim '(#\Space) value))))
           (when (member v '("ltr" "rtl") :test #'string=) (setf (cstyle-direction cs) v))))
        ;; CSS Containment 3 §container: establish a query container.
        ((string= prop "container-type")
         (let ((v (string-downcase (string-trim '(#\Space) value))))
           (when (member v '("normal" "size" "inline-size") :test #'string=)
             (setf (cstyle-container-type cs) v))))
        ((string= prop "container-name")
         (let ((v (string-downcase (string-trim '(#\Space) value))))
           (setf (cstyle-container-name cs)
                 (if (member v '("none" "" "initial") :test #'string=)
                     nil
                     (remove "" (split-ws v) :test #'string=)))))
        ((string= prop "container")
         ;; shorthand: <container-name> [ / <container-type> ]?
         (let* ((v (string-downcase (string-trim '(#\Space) value)))
                (slash (position #\/ v))
                (names (css-trim (subseq v 0 (or slash (length v)))))
                (type (and slash (css-trim (subseq v (1+ slash))))))
           (setf (cstyle-container-name cs)
                 (if (member names '("none" "") :test #'string=)
                     nil (remove "" (split-ws names) :test #'string=)))
           (setf (cstyle-container-type cs)
                 (if (and type (member type '("size" "inline-size") :test #'string=))
                     type "normal"))))
        ((string= prop "transform")
         (let ((tl (parse-value "transform" value)))
           (setf (cstyle-transform cs)
                 (if (and (listp tl) (not (equal tl '("none")))) tl nil))))
        ((string= prop "box-shadow")
         (let ((v (string-trim '(#\Space #\Tab #\Newline) value)))
           (cond ((string-equal v "inherit")
                  ;; box-shadow is not inherited by default; `inherit` copies the
                  ;; parent's computed shadow list (CSS 2.1 §6.2.1).
                  (setf (cstyle-box-shadow cs) (and parent-cs (cstyle-box-shadow parent-cs))))
                 ((or (string-equal v "initial") (string-equal v "unset"))
                  (setf (cstyle-box-shadow cs) nil))   ; initial == none for this longhand
                 (t (setf (cstyle-box-shadow cs) (parse-box-shadow value (cstyle-font-size cs)))))))
        ((string= prop "text-shadow")
         (let ((v (string-trim '(#\Space #\Tab #\Newline) value)))
           (cond ((or (string-equal v "inherit") (string-equal v "unset"))
                  ;; text-shadow IS inherited, so `unset` == `inherit`.
                  (setf (cstyle-text-shadow cs) (and parent-cs (cstyle-text-shadow parent-cs))))
                 ((string-equal v "initial")
                  (setf (cstyle-text-shadow cs) nil))   ; initial == none
                 (t (setf (cstyle-text-shadow cs) (parse-text-shadow value (cstyle-font-size cs)))))))
        ((string= prop "transform-origin")
         (let ((toks (remove "" (split-ws (string-downcase (string-trim '(#\Space) value))) :test #'string=)))
           (setf (cstyle-transform-origin cs) (and toks (subseq toks 0 (min 2 (length toks)))))))
        ((string= prop "column-span")
         (setf (cstyle-column-span cs)
               (if (string-equal (string-trim '(#\Space) value) "all") "all" "none")))
        ;; Multicol Level 2 — stored (non-NIL) only so the L1 flow can detect them.
        ((string= prop "column-height") (setf (cstyle-column-height cs) value))
        ((string= prop "column-wrap")
         (let ((v (string-downcase (string-trim '(#\Space) value))))
           (setf (cstyle-column-wrap cs) (if (string= v "nowrap") nil v))))
        ((string= prop "flex-grow") (let ((v (ignore-errors (read-from-string value)))) (when (numberp v) (setf (cstyle-flex-grow cs) (float v)))))
        ((string= prop "flex-shrink") (let ((v (ignore-errors (read-from-string value)))) (when (numberp v) (setf (cstyle-flex-shrink cs) (float v)))))
        ((string= prop "order") (let ((v (ignore-errors (parse-integer (string-trim '(#\Space) value))))) (when (integerp v) (setf (cstyle-order cs) v))))
        ((string= prop "flex-basis") (setf (cstyle-flex-basis cs) (string-downcase (string-trim '(#\Space) value))))
        ((member prop '("top" "left" "right" "bottom") :test #'string=)
         ;; a <percentage> inset (top:100%) is kept as (:percent N) — it resolves
         ;; against the containing block only where an inset consumer knows that box
         ;; (position:sticky); numeric consumers ignore the form as they did :auto.
         (let* ((tv (string-trim '(#\Space) value))
                (v (cond ((string-equal tv "auto") :auto)
                         ;; CSS-wide keywords: `initial`/`unset` reset a non-inherited
                         ;; inset to its initial value `auto` (`unset` == initial here);
                         ;; a dynamic `style.top = 'initial'` (CSS Position dynamic
                         ;; static-position tests) must drop a prior length back to the
                         ;; static position rather than being ignored.
                         ((member tv '("initial" "unset") :test #'string-equal) :auto)
                         ((and (> (length tv) 1) (char= (char tv (1- (length tv))) #\%))
                          (let ((n (ignore-errors (read-from-string (subseq tv 0 (1- (length tv)))))))
                            (if (numberp n) (list :percent (float n)) (len))))
                         (t (len)))))
           (when (or (eq v :auto) (numberp v) (and (consp v) (eq (car v) :percent)))
             (cond ((string= prop "top") (setf (cstyle-top cs) v)) ((string= prop "left") (setf (cstyle-left cs) v))
                   ((string= prop "right") (setf (cstyle-right cs) v)) (t (setf (cstyle-bottom cs) v))))))
        ;; the `inset` shorthand (CSS Position 3) sets top/right/bottom/left with the
        ;; usual 1-4 value shorthand expansion; each value is a length or `auto`.
        ((string= prop "inset")
         (let* ((parts (split-tokens (string-trim '(#\Space) value))) (n (length parts)))
           (when (and (>= n 1) (<= n 4))
             (flet ((val (tok) (if (string-equal tok "auto") :auto
                                   (let ((r (resolve-len tok fs))) (and (numberp r) r)))))
               (let ((tp (val (nth 0 parts)))
                     (rt (val (nth (if (>= n 2) 1 0) parts)))
                     (bt (val (nth (if (>= n 3) 2 0) parts)))
                     (lf (val (nth (cond ((= n 4) 3) ((>= n 2) 1) (t 0)) parts))))
                 (when tp (setf (cstyle-top cs) tp))
                 (when rt (setf (cstyle-right cs) rt))
                 (when bt (setf (cstyle-bottom cs) bt))
                 (when lf (setf (cstyle-left cs) lf)))))))
        ((string= prop "z-index") (let ((v (parse-value "z-index" value)))
                                    (when (and (listp v) (integerp (first v))) (setf (cstyle-z-index cs) (first v)))))
        ((string= prop "flex")
         ;; The `flex` shorthand (CSS Flexbox §7.2): one, two or three values, plus
         ;; the `none`/`auto`/`initial` keywords.  A bare number is flex-grow with
         ;; basis 0% (so Tailwind's `.flex-1{flex:1}` means 1 1 0%); a lone length
         ;; is the basis with grow 1.
         (let* ((val (string-downcase (string-trim '(#\Space) value))))
           (flet ((plain-num (s) (and (plusp (length s))
                                      (every (lambda (c) (or (digit-char-p c) (member c '(#\. #\-)))) s)
                                      (ignore-errors (float (read-from-string s))))))
             (cond
               ((string= val "none") (setf (cstyle-flex-grow cs) 0.0 (cstyle-flex-shrink cs) 0.0
                                           (cstyle-flex-basis cs) "auto"))
               ((string= val "initial") (setf (cstyle-flex-grow cs) 0.0 (cstyle-flex-shrink cs) 1.0
                                              (cstyle-flex-basis cs) "auto"))
               ((string= val "auto") (setf (cstyle-flex-grow cs) 1.0 (cstyle-flex-shrink cs) 1.0
                                           (cstyle-flex-basis cs) "auto"))
               (t (let ((grow nil) (shrink nil) (basis nil))
                    (dolist (tk (split-tokens val))
                      (let ((n (plain-num tk)))
                        (cond ((and n (null grow)) (setf grow n))
                              ((and n (null shrink)) (setf shrink n))
                              (t (setf basis tk)))))
                    (when grow (setf (cstyle-flex-grow cs) grow))
                    (setf (cstyle-flex-shrink cs) (or shrink 1.0))
                    (setf (cstyle-flex-basis cs) (or basis "0%"))   ; bare number -> basis 0%
                    (unless grow (setf (cstyle-flex-grow cs) 1.0))))))))   ; lone basis -> grow 1
        ((string= prop "margin")
         (let ((parts (split-tokens (string-trim '(#\Space) value))))
           ;; per-edge auto margins (flags used by flex/grid + block centering).  The
           ;; shorthand replaces all four edges, so clear every auto flag first, then
           ;; set the ones this value names.
           (setf (cstyle-margin-top-auto cs) nil (cstyle-margin-right-auto cs) nil
                 (cstyle-margin-bottom-auto cs) nil (cstyle-margin-left-auto cs) nil)
           (flet ((autop (i) (and (> (length parts) i) (string-equal (nth i parts) "auto"))))
             (case (length parts)
               (1 (when (autop 0) (setf (cstyle-margin-top-auto cs) t (cstyle-margin-right-auto cs) t
                                        (cstyle-margin-bottom-auto cs) t (cstyle-margin-left-auto cs) t)))
               (2 (when (autop 0) (setf (cstyle-margin-top-auto cs) t (cstyle-margin-bottom-auto cs) t))
                  (when (autop 1) (setf (cstyle-margin-left-auto cs) t (cstyle-margin-right-auto cs) t)))
               (3 (when (autop 0) (setf (cstyle-margin-top-auto cs) t))
                  (when (autop 1) (setf (cstyle-margin-left-auto cs) t (cstyle-margin-right-auto cs) t))
                  (when (autop 2) (setf (cstyle-margin-bottom-auto cs) t)))
               (t (when (autop 0) (setf (cstyle-margin-top-auto cs) t))
                  (when (autop 1) (setf (cstyle-margin-right-auto cs) t))
                  (when (autop 2) (setf (cstyle-margin-bottom-auto cs) t))
                  (when (autop 3) (setf (cstyle-margin-left-auto cs) t)))))
           (apply-box value fs cs #'(setf cstyle-margin-top) #'(setf cstyle-margin-right) #'(setf cstyle-margin-bottom) #'(setf cstyle-margin-left))))
        ((string= prop "padding") (apply-pad-box value fs cs #'(setf cstyle-padding-top) #'(setf cstyle-padding-right) #'(setf cstyle-padding-bottom) #'(setf cstyle-padding-left)))
        ((string= prop "margin-top") (if (string-equal (string-trim '(#\Space) value) "auto") (setf (cstyle-margin-top-auto cs) t) (let ((v (margin-len value fs))) (when v (set-margin-edge cs :top v) (setf (cstyle-margin-top-auto cs) nil)))))
        ((string= prop "margin-right") (if (string-equal (string-trim '(#\Space) value) "auto") (setf (cstyle-margin-right-auto cs) t) (let ((v (margin-len value fs))) (when v (set-margin-edge cs :right v) (setf (cstyle-margin-right-auto cs) nil)))))
        ((string= prop "margin-bottom") (if (string-equal (string-trim '(#\Space) value) "auto") (setf (cstyle-margin-bottom-auto cs) t) (let ((v (margin-len value fs))) (when v (set-margin-edge cs :bottom v) (setf (cstyle-margin-bottom-auto cs) nil)))))
        ((string= prop "margin-left") (if (string-equal (string-trim '(#\Space) value) "auto") (setf (cstyle-margin-left-auto cs) t) (let ((v (margin-len value fs))) (when v (set-margin-edge cs :left v) (setf (cstyle-margin-left-auto cs) nil)))))
        ((string= prop "padding-top") (setf (cstyle-padding-top cs) (pad-len value fs)))
        ((string= prop "padding-right") (setf (cstyle-padding-right cs) (pad-len value fs)))
        ((string= prop "padding-bottom") (setf (cstyle-padding-bottom cs) (pad-len value fs)))
        ((string= prop "padding-left") (setf (cstyle-padding-left cs) (pad-len value fs)))
        ((string= prop "border-collapse")
         (let ((v (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return) value))))
           (when (member v '("separate" "collapse") :test #'string=) (setf (cstyle-border-collapse cs) v))))
        ((string= prop "border-radius")
         (if (and parent-cs (radius-inherit-p value))
             (setf (cstyle-border-tl-radius cs) (cstyle-border-tl-radius parent-cs)
                   (cstyle-border-tr-radius cs) (cstyle-border-tr-radius parent-cs)
                   (cstyle-border-br-radius cs) (cstyle-border-br-radius parent-cs)
                   (cstyle-border-bl-radius cs) (cstyle-border-bl-radius parent-cs))
             (apply-border-radius cs value fs)))
        ((string= prop "border-top-left-radius")
         (if (and parent-cs (radius-inherit-p value)) (setf (cstyle-border-tl-radius cs) (cstyle-border-tl-radius parent-cs))
             (apply-corner-radius cs #'(setf cstyle-border-tl-radius) value fs)))
        ((string= prop "border-top-right-radius")
         (if (and parent-cs (radius-inherit-p value)) (setf (cstyle-border-tr-radius cs) (cstyle-border-tr-radius parent-cs))
             (apply-corner-radius cs #'(setf cstyle-border-tr-radius) value fs)))
        ((string= prop "border-bottom-right-radius")
         (if (and parent-cs (radius-inherit-p value)) (setf (cstyle-border-br-radius cs) (cstyle-border-br-radius parent-cs))
             (apply-corner-radius cs #'(setf cstyle-border-br-radius) value fs)))
        ((string= prop "border-bottom-left-radius")
         (if (and parent-cs (radius-inherit-p value)) (setf (cstyle-border-bl-radius cs) (cstyle-border-bl-radius parent-cs))
             (apply-corner-radius cs #'(setf cstyle-border-bl-radius) value fs)))
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
        ((string= prop "outline")
         (if (and parent-cs (string-equal (string-trim '(#\Space) value) "inherit"))
             (setf (cstyle-outline-width cs) (cstyle-outline-width parent-cs)
                   (cstyle-outline-style cs) (cstyle-outline-style parent-cs)
                   (cstyle-outline-color cs) (cstyle-outline-color parent-cs)
                   (cstyle-outline-offset cs) (cstyle-outline-offset parent-cs))
             (apply-outline cs value fs)))
        ((string= prop "outline-width")
         (if (and parent-cs (string-equal (string-trim '(#\Space) value) "inherit"))
             (setf (cstyle-outline-width cs) (cstyle-outline-width parent-cs))
             (let ((px (resolve-border-width-token (string-trim '(#\Space) value) fs)))
               (when (numberp px) (setf (cstyle-outline-width cs) px)))))
        ((string= prop "outline-style")
         (if (and parent-cs (string-equal (string-trim '(#\Space) value) "inherit"))
             (setf (cstyle-outline-style cs) (cstyle-outline-style parent-cs))
             (let ((v (string-downcase (string-trim '(#\Space) value))))
               (when (or (border-style-token-p v) (string= v "auto"))
                 (setf (cstyle-outline-style cs) v)))))
        ((string= prop "outline-color")
         (let ((v (string-trim '(#\Space) value)))
           (cond ((and parent-cs (string-equal v "inherit"))
                  (setf (cstyle-outline-color cs) (cstyle-outline-color parent-cs)))
                 ((string-equal v "invert")
                  (setf (cstyle-outline-color cs) nil))   ; treat invert as currentColor
                 ((string-equal v "auto")
                  (setf (cstyle-outline-color cs) :auto)) ; may resolve to accent-color
                 (t (let ((c (resolve-border-color v cs))) (when c (setf (cstyle-outline-color cs) c)))))))
        ((string= prop "accent-color")
         (let ((v (string-trim '(#\Space) value)))
           (cond ((and parent-cs (string-equal v "inherit"))
                  (setf (cstyle-accent-color cs) (cstyle-accent-color parent-cs)))
                 ((string-equal v "auto") (setf (cstyle-accent-color cs) nil))
                 (t (let ((c (resolve-border-color v cs))) (when c (setf (cstyle-accent-color cs) c)))))))
        ((string= prop "outline-offset")
         (if (and parent-cs (string-equal (string-trim '(#\Space) value) "inherit"))
             (setf (cstyle-outline-offset cs) (cstyle-outline-offset parent-cs))
             (let ((px (resolve-len (string-trim '(#\Space) value) fs)))
               (when (numberp px) (setf (cstyle-outline-offset cs) px)))))
        ((string= prop "border-width")
         ;; 1-4 value box shorthand (e.g. Acid2's `border-width: 0 2em`); each
         ;; value is a length or a thin/medium/thick keyword.
         (let ((vals (mapcar (lambda (tk) (resolve-border-width-token tk fs))
                             (split-tokens (string-trim '(#\Space) value)))))
           (when (and vals (every #'numberp vals))
             (destructuring-bind (&optional (a 0.0) (b a) (c a) (d b)) vals
               (setf (cstyle-border-top-width cs) a (cstyle-border-right-width cs) b
                     (cstyle-border-bottom-width cs) c (cstyle-border-left-width cs) d))))))))))

(defun first-token (s)
  "First whitespace-delimited token of S, but treating a parenthesised group as
part of one token — so rgb(238, 238, 238) / url(a b) stay intact (browsers
serialise every colour as rgb(r, g, b), which has spaces inside the parens)."
  (let* ((s (string-trim '(#\Space) s)) (depth 0) (n (length s)))
    (dotimes (i n s)
      (let ((c (char s i)))
        (cond ((char= c #\() (incf depth))
              ((char= c #\)) (when (plusp depth) (decf depth)))
              ((and (char= c #\Space) (zerop depth)) (return (subseq s 0 i))))))))

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
    ;; SPLIT-TOKENS (not SPLIT-WS) so a functional color keeps its internal
    ;; spaces — `background: rgba(0, 128, 0, 0.5)` is ONE color token, not four
    ;; unresolvable fragments (which left the shorthand's bg color unset).
    (remove "" (split-tokens (string-downcase v)) :test #'string=)))

(defun split-tokens (s)
  "Split S on spaces, but keep parenthesised groups intact — so a value like
`1px solid rgb(255, 102, 0)` yields (\"1px\" \"solid\" \"rgb(255, 102, 0)\")
rather than splitting the colour on the spaces inside its parens."
  (let ((out '()) (start 0) (depth 0) (n (length s)))
    (dotimes (i n)
      (let ((c (char s i)))
        (cond ((char= c #\() (incf depth))
              ((char= c #\)) (when (plusp depth) (decf depth)))
              ((and (char= c #\Space) (zerop depth))
               (when (> i start) (push (subseq s start i) out))
               (setf start (1+ i))))))
    (when (> n start) (push (subseq s start n) out))
    (nreverse out)))

(defun margin-len (tok fs)
  "A margin value: a percentage is kept symbolic as (:percent N) (may be negative) —
it resolves at layout against the containing block's inline size (CSS 2.1 §8.3) —
`auto` yields :auto; otherwise a resolved length, defaulting to 0."
  (let ((tt (string-trim '(#\Space #\Tab #\Newline) tok)))
    (cond ((string-equal tt "auto") :auto)
          ((and (> (length tt) 1) (char= (char tt (1- (length tt))) #\%))
           (let ((n (ignore-errors (read-from-string (subseq tt 0 (1- (length tt)))))))
             (if (realp n) (list :percent (float n)) 0.0)))
          (t (resolve-len tt fs)))))

(defun set-margin-edge (cs edge spec)
  "Store a parsed margin value SPEC (number | (:percent N) | :auto) into the numeric
and percentage slots for EDGE (:top/:right/:bottom/:left).  A percentage keeps its
symbolic form in the -pct slot (numeric slot 0 until layout resolves it); a length
clears the -pct slot.  Auto flags are managed by the caller."
  (multiple-value-bind (num pct)
      (if (and (consp spec) (eq (car spec) :percent))
          (values 0.0 spec)
          (values (if (numberp spec) spec 0.0) nil))
    (ecase edge
      (:top    (setf (cstyle-margin-top cs) num    (cstyle-margin-top-pct cs) pct))
      (:right  (setf (cstyle-margin-right cs) num   (cstyle-margin-right-pct cs) pct))
      (:bottom (setf (cstyle-margin-bottom cs) num  (cstyle-margin-bottom-pct cs) pct))
      (:left   (setf (cstyle-margin-left cs) num    (cstyle-margin-left-pct cs) pct)))))

(defun apply-box (value fs cs top right bottom left)
  "Apply the margin 1-4 value box shorthand (top right bottom left CSS order),
keeping percentages symbolic in the -pct slots.  TOP/RIGHT/BOTTOM/LEFT setters are
accepted for signature compatibility but percentages route through SET-MARGIN-EDGE."
  (declare (ignore top right bottom left))
  (let* ((parts (split-tokens (string-trim '(#\Space) value)))
         (vals (mapcar (lambda (p) (or (margin-len p fs) 0.0)) parts)))
    (destructuring-bind (&optional (a 0.0) (b a) (c a) (d b)) vals
      (set-margin-edge cs :top a) (set-margin-edge cs :right b)
      (set-margin-edge cs :bottom c) (set-margin-edge cs :left d))))

(defun pad-len (tok fs)
  "A padding value: a percentage is kept symbolic as (:percent N) — it resolves at
layout against the containing block's inline size (CSS 2.1 8.4) — otherwise a
resolved length (calc()/min()/max() included), defaulting to 0."
  (let ((tt (string-trim '(#\Space #\Tab #\Newline) tok)))
    (if (and (> (length tt) 1) (char= (char tt (1- (length tt))) #\%))
        (let ((n (ignore-errors (read-from-string (subseq tt 0 (1- (length tt)))))))
          (if (realp n) (list :percent (max 0.0 (float n))) 0.0))
        ;; used padding is never negative (CSS 2.1 §8.4): clamp a resolved length.
        (let ((v (resolve-len tt fs))) (if v (max 0.0 v) 0.0)))))

(defun apply-pad-box (value fs cs top right bottom left)
  "Like APPLY-BOX but each value may be a percentage (kept symbolic for layout)."
  (let* ((parts (split-tokens (string-trim '(#\Space) value)))
         (vals (mapcar (lambda (p) (pad-len p fs)) parts)))
    (destructuring-bind (&optional (a 0.0) (b a) (c a) (d b)) vals
      (funcall top a cs) (funcall right b cs) (funcall bottom c cs) (funcall left d cs))))

(defun resolve-pad (spec avail)
  "Used px of a padding slot SPEC (a number, (:percent N), or a deferred math form)
against the containing-block inline size AVAIL — padding percentages always resolve
against the inline size (CSS 2.1 8.4).  0 when the basis is indefinite or SPEC is
unparseable, so a caller with no width still gets a safe number."
  (cond ((numberp spec) spec)
        ((and (consp spec) (eq (car spec) :percent))
         (if (numberp avail) (* avail (/ (second spec) 100.0)) 0.0))
        ((consp spec) (or (resolve-deferred spec avail) 0.0))
        (t 0.0)))

(defun resolve-pct-margins (cs avail)
  "Resolve CS's deferred percentage margins against inline size AVAIL (px), writing
the used px into the numeric margin slots (CSS 2.1 §8.3: margin percentages resolve
against the containing block inline size, block-axis margins included).  Only edges
carrying a -pct form are touched, and the -pct forms are kept so this is idempotent."
  (when (numberp avail)
    (flet ((rz (pct) (* avail (/ (second pct) 100.0))))
      (let ((p (cstyle-margin-top-pct cs)))    (when p (setf (cstyle-margin-top cs)    (rz p))))
      (let ((p (cstyle-margin-right-pct cs)))  (when p (setf (cstyle-margin-right cs)  (rz p))))
      (let ((p (cstyle-margin-bottom-pct cs))) (when p (setf (cstyle-margin-bottom cs) (rz p))))
      (let ((p (cstyle-margin-left-pct cs)))   (when p (setf (cstyle-margin-left cs)   (rz p)))))))

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

(defun resolve-border-width-token (tok fs)
  "A single border-width value: thin/medium/thick keyword or a length."
  (cond ((string-equal tok "thin") 1.0)
        ((string-equal tok "medium") 3.0)
        ((string-equal tok "thick") 5.0)
        (t (resolve-len tok fs))))

(defun apply-border (cs prop value fs)
  (let ((w nil) (col nil) (sty nil))
    (dolist (tok (split-tokens value))
      (let ((px (resolve-len tok fs)))
        (cond ((numberp px) (setf w px))
              ((member tok '("thin" "medium" "thick") :test #'string-equal) (setf w (cond ((string-equal tok "thin") 1.0) ((string-equal tok "thick") 5.0) (t 3.0))))
              ((border-style-token-p tok) (setf sty (string-downcase tok)))
              ((resolve-border-color tok cs) (setf col (resolve-border-color tok cs))))))
    ;; an omitted width defaults to the initial `medium` (3px) when the border
    ;; has a visible style (CSS Backgrounds 3 §border-width): `border: solid` is
    ;; a 3px border, not a 0px one.  A `none`/`hidden` style keeps used width 0.
    (unless w (setf w (if (or (null sty) (member sty '("none" "hidden") :test #'string=)) 0.0 3.0)))
    ;; the `border` shorthand sets all three sub-properties: an omitted color
    ;; resets border-color to currentColor (the element's color), not the value a
    ;; prior rule left — so `border: 1px solid` over `border: 2em dotted red` is
    ;; solid black (currentColor), not solid red (CSS Backgrounds 3 §border).
    (unless col (setf col (cstyle-color cs)))
    ;; an omitted style resets to `none` (the initial value): `border: 1px blue`
    ;; paints nothing, not a leftover `solid` from a prior rule — Acid3's
    ;; `* { border: 1px blue }` must not draw a blue box round every element.
    (unless sty (setf sty "none"))
    (flet ((setw (side) (case side (:t (setf (cstyle-border-top-width cs) w)) (:r (setf (cstyle-border-right-width cs) w))
                          (:b (setf (cstyle-border-bottom-width cs) w)) (:l (setf (cstyle-border-left-width cs) w))))
           (setc (side) (case side (:t (setf (cstyle-border-top-color cs) col)) (:r (setf (cstyle-border-right-color cs) col))
                          (:b (setf (cstyle-border-bottom-color cs) col)) (:l (setf (cstyle-border-left-color cs) col))))
           (sets (side) (case side (:t (setf (cstyle-border-top-style cs) sty)) (:r (setf (cstyle-border-right-style cs) sty))
                          (:b (setf (cstyle-border-bottom-style cs) sty)) (:l (setf (cstyle-border-left-style cs) sty)))))
      (cond ((string= prop "border") (mapc (lambda (s) (setw s) (setc s) (sets s)) '(:t :r :b :l)))
            ((string= prop "border-top") (setw :t) (setc :t) (sets :t))
            ((string= prop "border-bottom") (setw :b) (setc :b) (sets :b))
            ((string= prop "border-left") (setw :l) (setc :l) (sets :l))
            ((string= prop "border-right") (setw :r) (setc :r) (sets :r))))))

(defun apply-outline (cs value fs)
  "The `outline` shorthand: width || style || color (CSS-UI §outline).  An omitted
color resets to currentColor (NIL sentinel); an omitted style resets to none."
  (let ((w nil) (col nil) (sty nil))
    (dolist (tok (split-tokens value))
      (let ((px (resolve-len tok fs)))
        (cond ((numberp px) (setf w px))
              ((member tok '("thin" "medium" "thick") :test #'string-equal)
               (setf w (cond ((string-equal tok "thin") 1.0) ((string-equal tok "thick") 5.0) (t 3.0))))
              ((or (border-style-token-p tok) (string-equal tok "auto")) (setf sty (string-downcase tok)))
              ((string-equal tok "invert"))       ; treat invert color as currentColor
              ((resolve-border-color tok cs) (setf col (resolve-border-color tok cs))))))
    (setf (cstyle-outline-width cs) (or w 3.0))
    ;; an omitted color is the initial `auto` (resolves to accent-color for an
    ;; auto-style outline, else currentColor); an explicit color wins.
    (setf (cstyle-outline-color cs) (or col :auto))
    (setf (cstyle-outline-style cs) (or sty "none"))))

(defun apply-presentational-hints (cs node)
  "Map legacy HTML presentational attributes to computed style (bgcolor, text,
width/height on table/replaced elements). Precedence is below author CSS, so
this is applied before author rules."
  (let ((tag (string-downcase (weft.html:dnode-name node))))
    (let ((bg (el-attr node "bgcolor")))
      (when bg (let ((c (resolve-color bg))) (when c (setf (cstyle-background cs) c)))))
    (when (string= tag "body")
      (let ((tx (el-attr node "text")))
        (when tx (let ((c (resolve-color tx))) (when c (setf (cstyle-color cs) c))))))
    (when (member tag '("table" "td" "th" "col" "colgroup" "hr" "iframe" "embed") :test #'string=)
      (let ((w (el-attr node "width")))
        (when w (let ((pw (parse-size w (cstyle-font-size cs) nil))) (when pw (setf (cstyle-width cs) pw)))))
      (let ((hh (el-attr node "height")))
        (when hh (let ((ph (parse-size hh (cstyle-font-size cs) nil))) (when ph (setf (cstyle-height cs) ph))))))
    ;; cellpadding=N on the nearest ancestor <table> sets each descendant cell's
    ;; padding to N px (HTML §11.3.1), below author CSS.  HN sets cellpadding=0, so
    ;; its story cells carry no padding and the row height comes from the votearrow.
    (when (member tag '("td" "th") :test #'string=)
      (let ((table (loop for a = (weft.html:dnode-parent node) then (weft.html:dnode-parent a)
                         while a
                         when (and (eq (weft.html:dnode-kind a) :element)
                                   (string-equal (weft.html:dnode-name a) "table"))
                           return a)))
        (when table
          (let ((cp (el-attr table "cellpadding")))
            (when cp
              (let ((n (ignore-errors (parse-integer (string-trim '(#\Space) cp) :junk-allowed t))))
                (when (and n (>= n 0))
                  (setf (cstyle-padding-top cs) n (cstyle-padding-right cs) n
                        (cstyle-padding-bottom cs) n (cstyle-padding-left cs) n))))))))
    ;; table cell / row / etc. align= -> text-align (HTML §14.3 presentational).
    (when (member tag '("td" "th" "tr" "thead" "tbody" "tfoot" "col" "colgroup") :test #'string=)
      (let ((al (el-attr node "align")))
        (when al (let ((a (string-downcase (string-trim '(#\Space) al))))
                   (when (member a '("left" "right" "center" "justify") :test #'string=)
                     (setf (cstyle-text-align cs) a))))))))

;;; ---- custom properties (CSS variables) ----------------------------------
(defun custom-prop-p (prop)
  "True for a `--name` custom-property declaration."
  (and (>= (length prop) 2) (char= (char prop 0) #\-) (char= (char prop 1) #\-)))

(defun resolve-vars (value vars)
  "Substitute var(--name[, fallback]) in VALUE using VARS (a hash --name -> string),
   resolving recursively; an unresolved var with no fallback yields empty (so the
   declaration usually becomes invalid, per CSS).  VARS may be NIL."
  (if (or (null vars) (null (search "var(" value)))
      value
      (with-output-to-string (out)
        (let ((i 0) (n (length value)))
          (loop
            (let ((p (search "var(" value :start2 i)))
              (cond
                ((null p) (write-string value out :start i) (return))
                (t (write-string value out :start i :end p)
                   (let ((depth 1) (k (+ p 4)) (end nil))
                     (loop while (< k n) do
                       (case (char value k)
                         (#\( (incf depth))
                         (#\) (decf depth) (when (zerop depth) (setf end k) (loop-finish))))
                       (incf k))
                     (if (null end)
                         (progn (write-string value out :start p) (return))
                         (let* ((inner (subseq value (+ p 4) end))
                                (comma (position #\, inner))
                                (name (string-trim '(#\Space #\Tab) (subseq inner 0 (or comma (length inner)))))
                                (fb (and comma (string-trim '(#\Space #\Tab) (subseq inner (1+ comma)))))
                                (v (gethash name vars)))
                           (write-string (cond (v v) (fb (resolve-vars fb vars)) (t "")) out)
                           ;; var() is a whole token; a following token may abut the
                           ;; `)` with no space (minified `var(--p-margin-y)0`), so
                           ;; separate them — extra whitespace is harmless in values.
                           (write-char #\Space out)
                           (setf i (1+ end)))))))))))))

;;; ---- the cascade --------------------------------------------------------
(defun font-face-rule-p (rule)
  "True when RULE is an at-rule sentinel captured by the parser (selector begins
with `@`, e.g. the `@font-face` descriptor set) rather than a qualified style rule."
  (let ((s (css-rule-selector rule)))
    (and (plusp (length s)) (char= (char s 0) #\@))))

(defun %font-face-src-urls (value)
  "Ordered list of url() targets in a @font-face `src` VALUE, best format first
(woff2 > woff > ttf/otf > unlabelled), skipping local().  E.g.
`url(a.woff2) format('woff2'), url(b.ttf) format('truetype')` -> (\"a.woff2\" \"b.ttf\")."
  (let ((items '()))
    ;; walk comma-separated src entries at top level (commas inside url()/format() are none here)
    (dolist (part (let ((parts '()) (start 0) (depth 0))
                    (dotimes (i (length value))
                      (case (char value i)
                        ((#\( #\[) (incf depth))
                        ((#\) #\]) (decf depth))
                        (#\, (when (<= depth 0) (push (subseq value start i) parts) (setf start (1+ i))))))
                    (push (subseq value start) parts)
                    (nreverse parts)))
      (let ((up (search "url(" part :test #'char-equal)))
        (when up
          (let* ((s (+ up 4))
                 (e (position #\) part :start s))
                 (url (and e (string-trim '(#\Space #\" #\' #\Tab) (subseq part s e))))
                 (fmt (let ((fp (search "format(" part :test #'char-equal)))
                        (when fp (string-downcase (string-trim '(#\Space #\" #\' #\))
                                                               (subseq part (+ fp 7)
                                                                       (or (position #\) part :start (+ fp 7))
                                                                           (length part)))))))))
            (when (and url (plusp (length url)))
              (push (cons url (or fmt "")) items))))))
    (setf items (nreverse items))
    ;; rank by format preference; keep source order within a rank
    (let ((rank (lambda (fmt) (cond ((search "woff2" fmt) 0) ((search "woff" fmt) 1)
                                    ((or (search "truetype" fmt) (search "opentype" fmt)
                                         (search "ttf" fmt) (search "otf" fmt)) 2)
                                    ((string= fmt "") 3) (t 4)))))
      (mapcar #'car (stable-sort items #'< :key (lambda (it) (funcall rank (cdr it))))))))

(defun collect-font-faces (stylesheet)
  "Extract @font-face descriptor sets from STYLESHEET (a list of CSS-RULEs) as a
list of plists (:family NAME :urls (url ...) :weight N :style KW) — NAME lowercased,
url list best-format-first, WEIGHT an integer (400 default; `bold`->700), STYLE one
of :normal|:italic.  Rules lacking a family or any src url are dropped."
  (loop for r in stylesheet
        when (and (font-face-rule-p r) (string-equal (css-rule-selector r) "@font-face"))
          nconc (let* ((decls (css-rule-decls r))
                       (get (lambda (p) (let ((d (find p decls :key #'css-decl-prop :test #'string-equal)))
                                          (and d (css-decl-value d)))))
                       (fam (funcall get "font-family"))
                       (src (funcall get "src"))
                       (wraw (funcall get "font-weight"))
                       (sraw (funcall get "font-style"))
                       (urls (and src (%font-face-src-urls src))))
                  (when (and fam urls)
                    (list (list :family (string-downcase (string-trim '(#\Space #\" #\' #\Tab) fam))
                                :urls urls
                                :weight (cond ((null wraw) 400)
                                              ((string-equal (string-trim '(#\Space) wraw) "bold") 700)
                                              ((string-equal (string-trim '(#\Space) wraw) "normal") 400)
                                              ((parse-integer wraw :junk-allowed t))
                                              (t 400))
                                :style (if (and sraw (search "italic" (string-downcase sraw))) :italic :normal)))))))

(defun int->roman (n upper)
  (if (or (<= n 0) (> n 3999)) (format nil "~d" n)
      (let ((vals '((1000 . "M") (900 . "CM") (500 . "D") (400 . "CD") (100 . "C")
                    (90 . "XC") (50 . "L") (40 . "XL") (10 . "X") (9 . "IX")
                    (5 . "V") (4 . "IV") (1 . "I")))
            (m n))
        (let ((r (with-output-to-string (s)
                   (dolist (p vals) (loop while (>= m (car p)) do (write-string (cdr p) s) (decf m (car p)))))))
          (if upper r (string-downcase r))))))

(defun int->alpha (n upper)
  (if (<= n 0) (format nil "~d" n)
      (let ((chars '()) (m n) (base (char-code (if upper #\A #\a))))
        (loop while (> m 0) do (decf m) (push (code-char (+ base (mod m 26))) chars) (setf m (floor m 26)))
        (coerce chars 'string))))

(defun int->greek (n)
  "Lower-greek alphabetic numbering (CSS 2.1 §12.4.1): base-24 over the classical
lowercase Greek letters α..ω, omitting final sigma (ς)."
  (if (<= n 0) (format nil "~d" n)
      (let ((letters "αβγδεζηθικλμνξοπρστυφχψω") (chars '()) (m n))
        (loop while (> m 0) do (decf m) (push (char letters (mod m 24)) chars) (setf m (floor m 24)))
        (coerce chars 'string))))

(defun int->additive (n table fallback)
  "Additive numeral system (CSS 2.1 §12.4.1 armenian/georgian): TABLE is a
descending list of (value . char); emit each char as many times as it divides
the remainder.  Values outside [1, max] fall back to FALLBACK (decimal)."
  (if (or (<= n 0) (> n (caar table))) (format nil "~d" n)
      (with-output-to-string (s)
        (declare (ignore fallback))
        (let ((m n))
          (dolist (p table)
            (loop while (>= m (car p)) do (write-char (cdr p) s) (decf m (car p))))))))

(defparameter *armenian-digits*
  ;; U+0531.. uppercase Armenian letters as numerals (ones/tens/hundreds/thousands)
  '((9000 . #\Ք) (8000 . #\Փ) (7000 . #\Ւ) (6000 . #\Ց) (5000 . #\Ր) (4000 . #\Տ) (3000 . #\Վ) (2000 . #\Ս) (1000 . #\Ռ)
    (900 . #\Ջ) (800 . #\Պ) (700 . #\Չ) (600 . #\Ո) (500 . #\Շ) (400 . #\Ն) (300 . #\Յ) (200 . #\Մ) (100 . #\Ճ)
    (90 . #\Ղ) (80 . #\Ձ) (70 . #\Հ) (60 . #\Կ) (50 . #\Ծ) (40 . #\Խ) (30 . #\Լ) (20 . #\Ի) (10 . #\Ժ)
    (9 . #\Թ) (8 . #\Ը) (7 . #\Է) (6 . #\Զ) (5 . #\Ե) (4 . #\Դ) (3 . #\Գ) (2 . #\Բ) (1 . #\Ա)))

(defparameter *georgian-digits*
  '((10000 . #\ჵ) (9000 . #\ჰ) (8000 . #\ჯ) (7000 . #\ჴ) (6000 . #\ხ) (5000 . #\ჭ) (4000 . #\წ) (3000 . #\ძ) (2000 . #\ც) (1000 . #\ჩ)
    (900 . #\შ) (800 . #\ყ) (700 . #\ღ) (600 . #\ქ) (500 . #\ფ) (400 . #\ჳ) (300 . #\ტ) (200 . #\ს) (100 . #\რ)
    (90 . #\ჟ) (80 . #\პ) (70 . #\ო) (60 . #\ჲ) (50 . #\ნ) (40 . #\მ) (30 . #\ლ) (20 . #\კ) (10 . #\ი)
    (9 . #\თ) (8 . #\ჱ) (7 . #\ზ) (6 . #\ვ) (5 . #\ე) (4 . #\დ) (3 . #\გ) (2 . #\ბ) (1 . #\ა)))

(defun format-counter (n style)
  "Format counter value N (an integer) with counter STYLE (a list-style-type
name; NIL = decimal), per CSS 2.1 §12.4.1."
  (cond ((null style) (format nil "~d" n))
        ((string= style "decimal") (format nil "~d" n))
        ((string= style "decimal-leading-zero") (if (and (>= n 0) (< n 10)) (format nil "0~d" n) (format nil "~d" n)))
        ((string= style "lower-roman") (int->roman n nil))
        ((string= style "upper-roman") (int->roman n t))
        ((member style '("lower-alpha" "lower-latin") :test #'string=) (int->alpha n nil))
        ((member style '("upper-alpha" "upper-latin") :test #'string=) (int->alpha n t))
        ((string= style "lower-greek") (int->greek n))
        ((string= style "armenian") (int->additive n *armenian-digits* nil))
        ((string= style "georgian") (int->additive n *georgian-digits* nil))
        ;; glyph list-styles render a fixed bullet, independent of N.
        ((string= style "disc") (string (code-char #x2022)))
        ((string= style "circle") (string (code-char #x25E6)))
        ((string= style "square") (string (code-char #x25AA)))
        ((string= style "none") "")
        (t (format nil "~d" n))))

(defun resolve-counters (document styles)
  "Assign CSS 2.1 §12.4 counter values in document order (a stack per counter
name, keyed by the DOM depth at which each was reset, so a reset's scope covers
the element, its following siblings and their descendants), then resolve every
content template (:tmpl ...) in STYLES to a final string."
  (let ((stacks (make-hash-table :test 'equal))   ; name -> list of (value . depth), innermost first
        (qdepth 0))                                ; CSS 2.1 §12.3.2 quote nesting depth (document-wide)
    (labels ((counter-val (name) (let ((s (gethash name stacks))) (if s (car (first s)) 0)))
             (do-reset (ops depth)
               (dolist (op ops)
                 (let* ((name (car op)) (val (cdr op)) (s (gethash name stacks)))
                   (loop while (and s (> (cdr (first s)) depth)) do (pop s))
                   (if (and s (= (cdr (first s)) depth))
                       (setf (car (first s)) val)
                       (push (cons val depth) s))
                   (setf (gethash name stacks) s))))
             (do-incr (ops)
               (dolist (op ops)
                 (let* ((name (car op)) (val (cdr op)) (s (gethash name stacks)))
                   (if s (incf (car (first s)) val)
                       (setf (gethash name stacks) (list (cons val 0)))))))  ; implicit root reset 0
             (pop-deeper (depth)
               (maphash (lambda (name s)
                          (loop while (and s (>= (cdr (first s)) depth)) do (pop s))
                          (setf (gethash name stacks) s))
                        stacks))
             (resolve (cs node)
               (when (and cs (consp (cstyle-content cs)) (eq (car (cstyle-content cs)) :tmpl))
                 (setf (cstyle-content cs)
                       (with-output-to-string (o)
                         (dolist (seg (rest (cstyle-content cs)))
                           (cond ((stringp seg) (write-string seg o))
                                 ((eq (car seg) :counter)
                                  (write-string (format-counter (counter-val (second seg)) (third seg)) o))
                                 ((eq (car seg) :attr)
                                  ;; attr() resolves against the pseudo's originating
                                  ;; element (CSS 2.1 §12.2); missing attr -> fallback/"".
                                  (let ((val (and node (el-attr node (second seg)))))
                                    (write-string (or val (third seg) "") o)))
                                 ((eq (car seg) :quote)
                                  ;; open/close-quote resolve against the element's
                                  ;; `quotes` at the current nesting depth (CSS 2.1 §12.3.2).
                                  (let* ((q (cstyle-quotes cs))
                                         (have (and q (plusp (length q))))
                                         (maxi (if have (1- (length q)) 0)))
                                    (ecase (second seg)
                                      (:open  (when have (write-string (car (aref q (min qdepth maxi))) o))
                                              (incf qdepth))
                                      (:no-open (incf qdepth))
                                      (:close (setf qdepth (max 0 (1- qdepth)))
                                              (when have (write-string (cdr (aref q (min qdepth maxi))) o)))
                                      (:no-close (setf qdepth (max 0 (1- qdepth)))))))
                                 ((eq (car seg) :counters)
                                  (loop for e in (reverse (gethash (second seg) stacks)) for first = t then nil do
                                    (unless first (write-string (third seg) o))
                                    (write-string (format-counter (car e) (fourth seg)) o)))))))))
             (none-p (cs) (and cs (string= (cstyle-display cs) "none")))
             (walk (n depth)
               (when (eq (weft.html:dnode-kind n) :element)
                 (let ((cs (gethash n styles))
                       (bcs (gethash (cons n :before) styles))
                       (acs (gethash (cons n :after) styles)))
                   ;; CSS 2.1 §12.4: a display:none element (and its whole subtree)
                   ;; generates no boxes, so it neither creates, resets nor increments
                   ;; a counter; likewise a non-generated (display:none) pseudo-element.
                   (unless (none-p cs)
                     (do-reset (cstyle-counter-reset cs) depth) (do-incr (cstyle-counter-increment cs)) (resolve cs n)
                     (when (and bcs (not (none-p bcs)))
                       (do-reset (cstyle-counter-reset bcs) (1+ depth)) (do-incr (cstyle-counter-increment bcs)) (resolve bcs n))
                     (loop for c across (weft.html:dnode-children n) do (walk c (1+ depth)))
                     (when (and acs (not (none-p acs)))
                       (do-reset (cstyle-counter-reset acs) (1+ depth)) (do-incr (cstyle-counter-increment acs)) (resolve acs n))
                     (pop-deeper (1+ depth)))))))
      (loop for c across (weft.html:dnode-children document) do (walk c 0)))))

;;; ---- @container query evaluation (CSS Containment 3) --------------------
;;; A container's measured size is carried as a plist (:pw :ph :iw :bh :fs :wm):
;;; PW/PH physical content-box width/height px (NIL = axis not available for this
;;; container-type), IW/BH logical inline/block sizes, FS the container font-size
;;; (for em), WM the writing-mode.  Evaluation is 3-valued (:true/:false/:unknown).

(defun cq-eval-feature (name op valstr size)
  "Evaluate one size feature against a container SIZE plist."
  (let* ((measured (cond ((string= name "width") (getf size :pw))
                         ((string= name "height") (getf size :ph))
                         ((string= name "inline-size") (getf size :iw))
                         ((string= name "block-size") (getf size :bh))
                         (t nil)))
         (want (and measured (resolve-len valstr (or (getf size :fs) 16.0)))))
    (if (and (numberp measured) (numberp want))
        (if (ecase op
              (:<  (< measured want))  (:<= (<= measured want))
              (:>  (> measured want))  (:>= (>= measured want))
              (:=  (= measured want)))
            :true :false)
        :unknown)))

(defun cq-eval (ast size)
  "3-valued evaluation of a container-condition AST against a container SIZE plist."
  (if (atom ast) :unknown
      (case (car ast)
        (:feature (cq-eval-feature (second ast) (third ast) (fourth ast) size))
        (:not (case (cq-eval (second ast) size) (:true :false) (:false :true) (t :unknown)))
        (:and (let ((r :true))
                (dolist (c (rest ast) r)
                  (case (cq-eval c size) (:false (return :false)) (:unknown (setf r :unknown))))))
        (:or  (let ((r :false))
                (dolist (c (rest ast) r)
                  (case (cq-eval c size) (:true (return :true)) (:unknown (setf r :unknown))))))
        (t :unknown))))

(defun cq-find-container (name ancestors)
  "Nearest query-container descriptor in ANCESTORS (innermost first) matching
NAME (or any container when NAME is NIL)."
  (find-if (lambda (d) (or (null name) (member name (second d) :test #'string=))) ancestors))

(defun cq-rule-matches-p (cq ancestors)
  "True when every @container query in CQ (list of (NAME . COND-AST)) evaluates
true against its nearest matching ancestor query container."
  (every (lambda (q)
           (let ((anc (cq-find-container (car q) ancestors)))
             ;; descriptor is (node names type . size-plist); the plist is CDDDR.
             (and anc (eq :true (cq-eval (cdr q) (cdddr anc))))))
         cq))

(defun compute-styles (document stylesheet &optional container-sizes)
  "Compute a CSTYLE for every element under DOCUMENT, applying STYLESHEET (a list
of CSS-RULEs).  Returns a hash-table element->CSTYLE.  When CONTAINER-SIZES (a
hash element->size-plist) is supplied, @container rules are evaluated against the
measured containers; otherwise their declarations are held back (not-yet-resolved)."
  (let ((styles (make-hash-table :test 'equal))
        (*el-classes-cache* (make-hash-table :test 'eq))   ; split each element's classes once this pass
        (*ancestor-bloom* (make-hash-table :test 'eq))     ; ancestor identifiers per element, for descendant/child skipping
        ;; pre-parse selectors once, tagging rules with (match-cx pseudo spec order decls).
        ;; pseudo = NIL | :before | :after; match-cx is the cx to match (pseudo-element stripped).
        (rindex (build-rindex
                 (loop for r in stylesheet for order from 0
                       ;; A style rule whose selector list contains ANY invalid
                       ;; selector is dropped in its entirety (CSS 2.1 §4.1.7 error
                       ;; recovery / Selectors 4 §3.1) — an invalid member does not
                       ;; merely drop itself and leave its valid siblings in force.
                       unless (or (font-face-rule-p r)   ; at-rule descriptor sets aren't selectors
                                  (not (selector-list-valid-p (css-rule-selector r))))
                       append (loop for cx in (parse-selector-list (css-rule-selector r))
                                    collect (multiple-value-bind (pe mcx) (cx-pseudo-element cx)
                                              (list mcx pe (specificity cx) order (css-rule-decls r)
                                                    (css-rule-container r))))))))
    (labels ((sort-matched (matched)
               (stable-sort (nreverse matched)
                            (lambda (x y) (or (spec< (first x) (first y))
                                              (and (equal (first x) (first y)) (< (second x) (second y)))))))
             (pseudo-style (parent-cs matched vars)
               "Build a CSTYLE for a ::before/::after box, or NIL if no content."
               (when matched
                 (let ((cs (make-cstyle :color (cstyle-color parent-cs) :font-size (cstyle-font-size parent-cs)
                                        :font-weight (cstyle-font-weight parent-cs) :line-height (cstyle-line-height parent-cs)
                                        :font-family (cstyle-font-family parent-cs) :font-style (cstyle-font-style parent-cs)
                                        :quotes (cstyle-quotes parent-cs))))
                   (dolist (m (sort-matched matched))
                     (dolist (d (third m))
                       (unless (custom-prop-p (css-decl-prop d))
                         (apply-decl cs (css-decl-prop d) (resolve-vars (css-decl-value d) vars) parent-cs))))
                   ;; An internal-table display on a generated box has no table to
                   ;; belong to, so it is wrapped in an anonymous table and becomes
                   ;; block-level (CSS 2.1 §17.2.1); rendered as a plain block box it
                   ;; matches browsers — the generated content lands on its own line.
                   ;; Exception: table-column / table-column-group boxes never render
                   ;; their content (CSS 2.1 §17.2), so the generated box is suppressed.
                   (let ((d (cstyle-display cs)))
                     (cond
                       ((member d '("table-column" "table-column-group") :test #'string=)
                        (return-from pseudo-style nil))
                       ((member d '("table-row-group" "table-header-group" "table-footer-group"
                                    "table-row" "table-cell" "table-caption")
                                :test #'string=)
                        (setf (cstyle-display cs) "block"))))
                   (and (cstyle-content cs) cs))))
             (fragment-style (parent-cs matched vars)
               "Build a CSTYLE for a ::first-letter/::first-line fragment (inherits
all of PARENT-CS, then overlays the matched pseudo-element declarations).  No
content gate — the fragment styles a slice of the element's own text."
               (when matched
                 (let ((cs (copy-cstyle parent-cs)))
                   ;; a fragment box is not itself a block/float/positioned box; it
                   ;; only restyles inline text, so drop inherited box-level bits.
                   (setf (cstyle-content cs) nil)
                   (dolist (m (sort-matched matched))
                     (dolist (d (third m))
                       (unless (custom-prop-p (css-decl-prop d))
                         (apply-decl cs (css-decl-prop d) (resolve-vars (css-decl-value d) vars) parent-cs))))
                   cs)))
             (walk (n parent-cs parent-vars anc-bloom cq-anc)
               (when (eq (weft.html:dnode-kind n) :element)
                 (setf (gethash n *ancestor-bloom*) anc-bloom)
                 (let* ((tag (string-downcase (weft.html:dnode-name n)))
                        (cs (ua-style tag parent-cs)))
                   ;; legacy presentational attributes (bgcolor, width, ...) — below author CSS
                   (apply-presentational-hints cs n)
                   ;; UA rule `[popover]:not(:popover-open) { display: none }` (HTML §popover
                   ;; / CSS Position 4): a popover element is not rendered until it is in the
                   ;; top layer (showPopover).  Weft does not track the open state, so a
                   ;; popover defaults to display:none; an author `display` rule still wins
                   ;; (this is a UA-tier default, applied before the author cascade below).
                   (when (el-attr n "popover")
                     (setf (cstyle-display cs) "none"))
                   ;; collect matching author rules, splitting element vs pseudo-element
                   (let ((matched '()) (m-before '()) (m-after '()) (m-first-letter '()) (m-first-line '()))
                     (map-candidate-rules
                      (lambda (ru)
                        (destructuring-bind (cx pe spec order decls cq) ru
                          (when (match-cx cx n)
                            (case pe
                              (:before (push (list spec order decls cq) m-before))
                              (:after  (push (list spec order decls cq) m-after))
                              (:first-letter (push (list spec order decls cq) m-first-letter))
                              (:first-line   (push (list spec order decls cq) m-first-line))
                              (t       (push (list spec order decls cq) matched))))))
                      rindex n tag)
                     ;; @container gating (CSS Containment 3): a rule stamped with a
                     ;; container query applies only when CONTAINER-SIZES is known and
                     ;; every enclosing query matches N's ancestor container.  With no
                     ;; sizes yet (initial pass), container rules are held back; an
                     ;; unconditional rule (CQ NIL) is always kept — a no-op on pages
                     ;; without @container, so the cascade is byte-identical there.
                     (flet ((cq-keep (m)
                              (let ((cq (fourth m)))
                                (or (null cq)
                                    (and container-sizes (cq-rule-matches-p cq cq-anc))))))
                       (setf matched (remove-if-not #'cq-keep matched)
                             m-before (remove-if-not #'cq-keep m-before)
                             m-after (remove-if-not #'cq-keep m-after)
                             m-first-letter (remove-if-not #'cq-keep m-first-letter)
                             m-first-line (remove-if-not #'cq-keep m-first-line)))
                     (let* ((sorted (sort-matched matched))
                            (inline (el-attr n "style"))
                            (inline-pvs (and inline (parse-inline inline)))
                            ;; custom properties (--name) inherit; only allocate a fresh
                            ;; environment when this element actually declares one.
                            (has-custom (or (some (lambda (m) (some (lambda (d) (custom-prop-p (css-decl-prop d))) (third m))) sorted)
                                            (some (lambda (pv) (custom-prop-p (first pv))) inline-pvs)))
                            (vars (if has-custom
                                      (let ((h (make-hash-table :test 'equal)))
                                        (when parent-vars (maphash (lambda (k v) (setf (gethash k h) v)) parent-vars))
                                        h)
                                      parent-vars)))
                       ;; pass 1: resolve the custom-property environment (cascade order)
                       (when has-custom
                         (dolist (m sorted)
                           (dolist (d (third m))
                             (when (custom-prop-p (css-decl-prop d))
                               (setf (gethash (css-decl-prop d) vars) (resolve-vars (css-decl-value d) vars)))))
                         (dolist (pv inline-pvs)
                           (when (custom-prop-p (first pv))
                             (setf (gethash (first pv) vars) (resolve-vars (second pv) vars)))))
                       ;; pass 2a: resolve font-size first, so em/ch on other
                       ;; properties use the element's FINAL font-size regardless of
                       ;; declaration order (a rule may set max-width:65ch before
                       ;; font-size).  Re-applying it in pass 2 is idempotent.
                       (flet ((font-decl-p (p) (or (string= p "font-size") (string= p "font"))))
                         (dolist (m sorted)
                           (dolist (d (third m))
                             (when (font-decl-p (css-decl-prop d))
                               (apply-decl cs (css-decl-prop d) (resolve-vars (css-decl-value d) vars) parent-cs))))
                         (dolist (pv inline-pvs)
                           (when (font-decl-p (first pv))
                             (apply-decl cs (first pv) (resolve-vars (second pv) vars) parent-cs))))
                       ;; pass 2: the cascade origins in priority order (CSS 2.1 6.4.1):
                       ;; author normal (by specificity) -> inline normal -> author
                       ;; !important (by specificity).  Splitting normal from !important
                       ;; is what lets a low-specificity `!important` beat a
                       ;; higher-specificity normal rule (Acid3's `* + * > * > p
                       ;; { border: 1px solid !important }` over `.buckets p`).
                       (flet ((apply-author (important)
                                (dolist (m sorted)
                                  (dolist (d (third m))
                                    (when (and (not (custom-prop-p (css-decl-prop d)))
                                               (eq (not (null (css-decl-important d))) important))
                                      (apply-decl cs (css-decl-prop d)
                                                  (resolve-vars (css-decl-value d) vars) parent-cs))))))
                         (flet ((apply-inline (important)
                                  (dolist (pv inline-pvs)
                                    (when (and (not (custom-prop-p (first pv)))
                                               (eq (not (null (third pv))) important))
                                      (apply-decl cs (first pv) (resolve-vars (second pv) vars) parent-cs)))))
                           (apply-author nil)                    ; author normal
                           (apply-inline nil)                    ; inline normal (beats author normal)
                           (apply-author t)                      ; author !important (beats all normal)
                           (apply-inline t)))                    ; inline !important (beats author !important)
                       ;; legacy <center>: horizontally center its block-level children
                       ;; (the -webkit-center behavior) unless the author set a margin.
                       (let ((p (weft.html:dnode-parent n)))
                         (when (and p (eq (weft.html:dnode-kind p) :element)
                                    (string-equal (weft.html:dnode-name p) "center")
                                    (member (cstyle-display cs) '("block" "table" "list-item" "flex" "grid")
                                            :test #'string=))
                           (setf (cstyle-margin-left-auto cs) t (cstyle-margin-right-auto cs) t)))
                       ;; CSS 2.1 §8.3 / §17.5: margins do not apply to the internal
                       ;; table boxes (row/row-group/column/column-group/cell), and
                       ;; padding does not apply to the non-cell internal table boxes
                       ;; (it DOES apply to table-cell).  Force them to zero so no
                       ;; layout path shifts these boxes (e.g. margin-left on a
                       ;; table-cell must not push the cell right).
                       (let ((d (cstyle-display cs)))
                         (when (member d '("table-row-group" "table-header-group"
                                           "table-footer-group" "table-row"
                                           "table-column-group" "table-column" "table-cell")
                                       :test #'string=)
                           (setf (cstyle-margin-top cs) 0.0 (cstyle-margin-bottom cs) 0.0
                                 (cstyle-margin-left cs) 0.0 (cstyle-margin-right cs) 0.0
                                 (cstyle-margin-top-auto cs) nil (cstyle-margin-bottom-auto cs) nil
                                 (cstyle-margin-left-auto cs) nil (cstyle-margin-right-auto cs) nil))
                         (when (member d '("table-row-group" "table-header-group"
                                           "table-footer-group" "table-row"
                                           "table-column-group" "table-column")
                                       :test #'string=)
                           (setf (cstyle-padding-top cs) 0.0 (cstyle-padding-bottom cs) 0.0
                                 (cstyle-padding-left cs) 0.0 (cstyle-padding-right cs) 0.0)))
                       (setf (gethash n styles) cs)
                       ;; generated content (computed after the element's own style is known)
                       (let ((bs (pseudo-style cs m-before vars)) (as (pseudo-style cs m-after vars))
                             (fl (fragment-style cs m-first-letter vars))
                             (fli (fragment-style cs m-first-line vars)))
                         (when bs (setf (gethash (cons n :before) styles) bs))
                         (when as (setf (gethash (cons n :after) styles) as))
                         (when fl (setf (gethash (cons n :first-letter) styles) fl))
                         (when fli (setf (gethash (cons n :first-line) styles) fli)))
                       (let ((child-bloom (logior anc-bloom (el-own-bloom n)))
                             ;; extend the query-container ancestor chain if N is a
                             ;; container (CSS Containment 3): descriptor is
                             ;; (node names type . size-plist) with the measured size
                             ;; from CONTAINER-SIZES (NIL until the post-layout pass).
                             (child-cq-anc
                               (if (member (cstyle-container-type cs) '("size" "inline-size")
                                           :test #'string=)
                                   (cons (list* n (cstyle-container-name cs) (cstyle-container-type cs)
                                                (and container-sizes (gethash n container-sizes)))
                                         cq-anc)
                                   cq-anc)))
                         (loop for c across (weft.html:dnode-children n)
                               do (walk c cs vars child-bloom child-cq-anc)))))))))
      (loop for c across (weft.html:dnode-children document) do (walk c nil nil 0 nil)))
    ;; second pass: assign counter values in document order and resolve every
    ;; content template that references counter()/counters() (CSS 2.1 §12.4).
    (resolve-counters document styles)
    styles))

(defun spec< (a b)
  (cond ((< (first a) (first b)) t) ((> (first a) (first b)) nil)
        ((< (second a) (second b)) t) ((> (second a) (second b)) nil)
        (t (< (third a) (third b)))))

(defparameter +css-ws+ '(#\Space #\Tab #\Newline #\Return #\Page)
  "CSS whitespace (CSS Syntax §3): trimmed around inline-style names and values so a
multi-line style attribute (a newline before a declaration) still parses.")
(defun strip-important (v)
  "Return (values VALUE-without-!important IMPORTANT-P) for a declaration value —
the trailing `! important` (whitespace-tolerant) marks it important (CSS 2.1 6.4.2)."
  (let ((bang (position #\! v :from-end t)))
    (if (and bang (string-equal "important" (string-trim +css-ws+ (subseq v (1+ bang)))))
        (values (string-trim +css-ws+ (subseq v 0 bang)) t)
        (values v nil))))

(defun parse-inline (s)
  "Parse an inline style attribute 'a:b; c:d !important' into (NAME VALUE IMPORTANT-P)
triples (VALUE has any trailing !important stripped)."
  (loop for chunk in (split-semi s)
        for cp = (position #\: chunk)
        for name = (and cp (string-trim +css-ws+ (subseq chunk 0 cp)))
        when cp collect
        (multiple-value-bind (val imp) (strip-important (string-trim +css-ws+ (subseq chunk (1+ cp))))
          ;; custom properties (--*) are case-sensitive; normal names are not
          (list (if (and (>= (length name) 2) (char= (char name 0) #\-) (char= (char name 1) #\-))
                    name (string-downcase name))
                val imp))))
(defun split-semi (s)
  (loop with start = 0 for i from 0 to (length s)
        when (or (= i (length s)) (char= (char s i) #\;))
          collect (prog1 (subseq s start i) (setf start (1+ i)))))
