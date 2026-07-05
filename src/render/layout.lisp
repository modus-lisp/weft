;;;; src/render/layout.lisp — block + inline-formatting layout, paint, render.
;;;;
;;;; Normal-flow layout: block-level boxes stacked vertically (margin/border/
;;;; padding), with INLINE formatting contexts that lay styled text runs into
;;;; line boxes — each fragment keeps its own color/weight/decoration, so bold,
;;;; links, and colored spans render correctly.  Mixed block+inline children are
;;;; grouped into anonymous inline runs.  List items get markers.  Painted to a
;;;; canvas and saved as PNG.
(in-package #:weft.render)

(defstruct lbox x y w h style node kind children marker img vpaint)   ; kind :block | :line; img = decoded IMG; vpaint = (cv x y w h) replaced-content painter
(defstruct frag x w text style node)                       ; a positioned styled text run on a line; node = source DOM element (for hit-testing)

(defvar *floats* nil "Page-scoped float list: each (side left right top bottom).")
(defvar *abs-pending* nil
  "Out-of-flow absolutely-positioned boxes awaiting resolution against the
nearest positioned ancestor: a list of (lbox . cstyle).  Rebound fresh by every
positioned element; the top-level binding (layout-tree) is the initial CB.")
(defvar *fixed-pending* nil
  "Out-of-flow fixed boxes awaiting resolution against the viewport: (lbox . cstyle).")

(defvar *intrinsic-cache* nil
  "Per-layout-pass memo for intrinsic-width measurement — MIN-CONTENT-WIDTH,
PREF-CONTENT-WIDTH and TABLE-COLUMN-MODEL.  Bound FRESH at the top of a full page
layout (LAYOUT-TREE) and never persists across renders.  Without it, a nested
table re-measures its whole subtree at every enclosing level (each cell is probed
by cell-max AND cell-min widths, and TABLE-COLUMN-MODEL is itself run by both
TABLE-NATURAL-WIDTH and LAYOUT-TABLE), so cost was exponential in nesting depth
(~5x/level).  Memoised, each (node,avail) is measured once and the recursion is
linear.  Keyed on a (TAG NODE . ROUNDED-AVAIL) list under EQUAL.")

(defmacro with-intrinsic-memo (key &body body)
  "Memoise BODY's (possibly multiple) values in *INTRINSIC-CACHE* under KEY.  A
no-op passthrough when the cache is unbound (measurement outside a layout pass)."
  (let ((k (gensym)) (hit (gensym)) (found (gensym)) (res (gensym)))
    `(let ((,k ,key))
       (if *intrinsic-cache*
           (multiple-value-bind (,hit ,found) (gethash ,k *intrinsic-cache*)
             (if ,found
                 (values-list ,hit)
                 (let ((,res (multiple-value-list (progn ,@body))))
                   (setf (gethash ,k *intrinsic-cache*) ,res)
                   (values-list ,res))))
           (progn ,@body)))))

(defun float-band (y h cleft cright)
  "Available (values left right) at the vertical band [y, y+h) after floats."
  (let ((left cleft) (right cright))
    (dolist (f *floats*)
      (destructuring-bind (side fl fr ft fb) f
        (when (and (< y fb) (> (+ y h) ft))            ; vertical overlap
          (if (eq side :left) (setf left (max left fr)) (setf right (min right fl))))))
    (values left right)))

(defun clear-y (y cleft cright sides)
  "Lowest y at or below Y clear of floats on SIDES (:left/:right list)."
  (declare (ignore cleft cright))
  (let ((yy y))
    (dolist (f *floats*)
      (destructuring-bind (side fl fr ft fb) f
        (declare (ignore fl fr ft))
        (when (member side sides) (setf yy (max yy fb)))))
    yy))

(defun st (styles node) (gethash node styles))
(defun child-elements (node)
  (loop for c across (h:dnode-children node) when (eq (h:dnode-kind c) :element) collect c))
(defun cdisplay (cs) (if cs (css:cstyle-display cs) "inline"))
(defun float-p (styles node)
  (and (eq (h:dnode-kind node) :element)
       (let ((cs (st styles node))) (and cs (member (css:cstyle-float cs) '("left" "right") :test #'string=)))))
(defun out-of-flow-p (styles node)
  "True when NODE is absolutely/fixed positioned — removed from normal flow, so it
   contributes nothing to its container's intrinsic (min/max-content) width."
  (and (eq (h:dnode-kind node) :element)
       (let ((cs (st styles node)))
         (and cs (member (css:cstyle-position cs) '("absolute" "fixed") :test #'string=)))))
(defun flex-row-p (cs)
  "True when CS is a flex container whose main axis is horizontal (a row)."
  (and cs (string= (cdisplay cs) "flex")
       (let ((d (css:cstyle-flex-direction cs)))
         (not (or (string= d "column") (string= d "column-reverse"))))))
(defparameter *block-displays* '("block" "list-item" "flex" "table" "flow-root" "grid"))
(defun inline-level-p (styles node)
  (case (h:dnode-kind node)
    (:text t)
    (:element (let ((cs (st styles node))) (and cs (not (member (cdisplay cs) (cons "none" *block-displays*) :test #'string=)))))
    (t nil)))
(defun block-level-p (styles node)
  (and (eq (h:dnode-kind node) :element)
       (let ((cs (st styles node))) (and cs (member (cdisplay cs) *block-displays* :test #'string=)))))
(defun table-box-p (styles node)
  "True when NODE is a display:table (or inline-table) box — one whose intrinsic
width is its COLUMN model, not its flattened inline content."
  (and (eq (h:dnode-kind node) :element)
       (let ((cs (st styles node)))
         (and cs (member (cdisplay cs) '("table" "inline-table") :test #'string=)))))

;;; ---- inline content: styled word runs -> line boxes --------------------
(defun collect-words (nodes styles default-style content-w)
  "Walk the inline content of NODES (a list of sibling inline-level nodes laid out
as one run) into inline tokens (PAYLOAD META SPACE GAP): PAYLOAD is a word string
with META its style, or :ATOMIC with META an lbox (inline-block / replaced).  SPACE
is T when collapsible whitespace preceded the token in the source.  Sharing the
whitespace state across the whole run is what keeps a space at a text/element
boundary — \"see \" <a>x</a> stays \"see x\" — while genuinely adjacent runs like
\"(\", <a>x</a>, \")\" still get none.  GAP is extra leading px from inline
horizontal margins on the enclosing element(s).  Use TOK-META/TOK-SPACE/TOK-GAP."
  (let ((words '()) (pend nil) (pend-px 0))        ; whitespace + inline-margin px before next token
    (labels ((emit1 (payload meta node)
               (push (list payload meta pend pend-px node) words) (setf pend nil pend-px 0))
             (atom! (lb node) (when lb (emit1 :atomic lb node)))
             (iedge (cs side)                        ; inline horizontal margin px on :left / :right
               (let ((v (if (eq side :left) (css:cstyle-margin-left cs) (css:cstyle-margin-right cs))))
                 (if (numberp v) (max 0 v) 0)))
             (emit-text (s style node)
               (let ((b (make-string-output-stream)) (any nil))
                 (flet ((flush () (when any
                                    (emit1 (apply-text-transform (get-output-stream-string b)
                                                                 (and style (css:cstyle-text-transform style)))
                                           style node)
                                    (setf any nil b (make-string-output-stream)))))
                   ;; white-space:pre-line / pre-wrap preserve newlines as forced
                   ;; line breaks (emitted as a :break token); other whitespace still
                   ;; collapses to a single inter-word space.
                   (let ((keep-nl (and style (member (css:cstyle-white-space style)
                                                     '("pre-line" "pre-wrap") :test #'string=))))
                     (loop for c across s do
                       (cond
                         ((and keep-nl (char= c #\Newline)) (flush) (push (list :break nil nil 0 node) words))
                         ((member c '(#\Space #\Tab #\Newline #\Return)) (flush) (setf pend t))
                         (t (write-char c b) (setf any t)))))
                   (flush))))
             (rec (n owner onode)
               (case (h:dnode-kind n)
                 (:text (emit-text (h:dnode-data n) owner onode))
                 (:element
                  (let ((cs (or (st styles n) owner)))
                    (cond
                      ((and cs (string= (cdisplay cs) "none")))
                      ;; out-of-flow (absolute/fixed) elements are not part of the
                      ;; inline run and must not contribute to a line's / a flex
                      ;; item's content width (they are placed separately).
                      ((and cs (member (css:cstyle-position cs) '("absolute" "fixed") :test #'string=)))
                      ((member (h:dnode-name n) '("script" "style") :test #'string=))
                      ;; Replaced elements (img, decodable <object>) render at their
                      ;; OWN intrinsic/attr size — checked BEFORE inline-block, because
                      ;; <img> is UA display:inline-block and must not fall into generic
                      ;; block layout, which collapses it to a ~0-height content box.
                      ((string= (h:dnode-name n) "img")
                       (let ((lb (img-box n cs))) (atom! lb n)))
                      ;; <object data="data:...image..."> that decodes renders as a
                      ;; replaced image (with its own background, e.g. Acid2's eye
                      ;; tile); otherwise it falls back to its child content.
                      ((and (string= (h:dnode-name n) "object") (object-data-image n))
                       (let ((lb (object-box n cs (object-data-image n))))
                         (atom! lb n)))
                      ;; <svg> / <canvas>: replaced vector content, sized to its
                      ;; intrinsic box (viewBox/width/height for SVG; width/height
                      ;; attrs for canvas) and composited during paint.
                      ((string= (h:dnode-name n) "svg")
                       (atom! (svg-box n cs) n))
                      ((string= (h:dnode-name n) "canvas")
                       (atom! (canvas-box n cs) n))
                      ((and cs (member (cdisplay cs) '("inline-block" "flex" "table") :test #'string=))
                       (multiple-value-bind (lb adv) (layout-node n styles 0 0 content-w)
                         (declare (ignore adv))
                         (atom! lb n)))
                      ;; A block-level element with an explicit px width AND height
                      ;; that appears in inline flow (e.g. HN's <div class=votearrow>
                      ;; 10x10 inside an inline <a>) is not part of the surrounding
                      ;; inline formatting context, but browsers still reserve its
                      ;; principal box: it becomes an atomic inline occupying its
                      ;; border-box plus margins and contributing that height to the
                      ;; line.  Scoped to definite w AND h so ordinary block children
                      ;; of an inline (rare) and Acid2's inline-blocks are untouched.
                      ((and cs (member (cdisplay cs) '("block" "list-item") :test #'string=)
                            (numberp (css:cstyle-width cs)) (numberp (css:cstyle-height cs)))
                       (multiple-value-bind (lb adv) (layout-node n styles 0 0 content-w)
                         (declare (ignore adv))
                         (when lb
                           (let ((ml (max 0 (css:cstyle-margin-left cs)))
                                 (mr (max 0 (css:cstyle-margin-right cs)))
                                 (mt (max 0 (css:cstyle-margin-top cs)))
                                 (mb (max 0 (css:cstyle-margin-bottom cs))))
                             ;; LAYOUT-NODE already positioned LB's border-box at
                             ;; (ml,mt); wrap it in a marginless atomic box sized to
                             ;; the full margin box so the line height counts margins.
                             (atom! (make-lbox :x 0 :y 0
                                               :w (+ ml (lbox-w lb) mr)
                                               :h (+ mt (lbox-h lb) mb)
                                               :kind :block :children (list lb))
                                    n)))))
                      (t                             ; generic inline: honor horizontal margins
                       (incf pend-px (iedge cs :left))
                       ;; ::before / ::after generated content on an inline element
                       ;; (e.g. .hlist li::after { content:"\a0 · " } separators).
                       ;; Block-level pseudo boxes are materialised in %LAYOUT-CORE;
                       ;; inline ones are emitted here as text in the enclosing run.
                       (let ((bcs (gethash (cons n :before) styles)))
                         (when (and bcs (css:cstyle-content bcs))
                           (emit-text (css:cstyle-content bcs) bcs n)))
                       (loop for c across (h:dnode-children n) do (rec c cs n))
                       (let ((acs (gethash (cons n :after) styles)))
                         (when (and acs (css:cstyle-content acs))
                           (emit-text (css:cstyle-content acs) acs n)))
                       (incf pend-px (iedge cs :right)))))))))
      (dolist (n nodes) (rec n (or (st styles n) default-style) n)))
    (nreverse words)))

;;; Inline-token accessors: (PAYLOAD META SPACE GAP) — PAYLOAD is (CAR tok) (a word
;;; string or :ATOMIC), META its style or lbox, SPACE whether whitespace preceded it,
;;; GAP extra leading px from the enclosing element's inline horizontal margins.
(declaim (inline tok-meta tok-space tok-gap tok-node))
(defun tok-meta (tok) (cadr tok))
(defun tok-space (tok) (caddr tok))
(defun tok-gap (tok) (cadddr tok))
(defun tok-node (tok) (fifth tok))      ; source DOM element node (for hit-testing)

(defun img-attr-num (node name)
  (let ((v (cdr (assoc name (h:dnode-attrs node) :test #'string-equal))))
    (when v (ignore-errors (parse-integer (string-trim '(#\Space #\p #\x) v) :junk-allowed t)))))

(defun img-box (node cs)
  "An atomic replaced box for <img>, sized to its BORDER box: the layout footprint
is the content WxH plus its border (and padding) widths, with the decoded image (or
an alt-text placeholder) painted in the content area inset by that border/padding.
Modelled on OBJECT-BOX so a bordered <img> (e.g. HN's `border:1px white` logo)
reserves the browser's real box (20x20 for an 18x18 + 1px border) instead of just
its content.  The decoded/CSS/HTML intrinsic size is the CONTENT size; else an
alt-text placeholder."
  (let* ((src (cdr (assoc "src" (h:dnode-attrs node) :test #'string-equal)))
         (decoded (cond
                    ((null src) nil)
                    ((and (>= (length src) 5) (string-equal (subseq src 0 5) "data:"))
                     (ignore-errors (decode-image src)))
                    ;; a network <img src> — fetched, decoded and cached through
                    ;; *IMAGE-LOADER* (NIL when running offline: stays a placeholder).
                    (t (fetch-image src))))
         (cw (let ((sw (css::resolve-size (css:cstyle-width cs) 300))) (and (numberp sw) sw)))
         (chh (let ((sh (css::resolve-size (css:cstyle-height cs) 200))) (and (numberp sh) sh)))
         (w (or cw (img-attr-num node "width") (and decoded (img-w decoded)) 120))
         (hh (or chh (img-attr-num node "height") (and decoded (img-h decoded)) 90))
         (alt (cdr (assoc "alt" (h:dnode-attrs node) :test #'string-equal)))
         (has-alt (and alt (plusp (length alt))))
         ;; An unfetchable image with NO alt text (HN's SVG logo, a 14x1 spacer
         ;; gif) is not a broken-image placeholder — the browser reserves the
         ;; declared box and shows nothing.  Drop the UA chrome (gray fill + 1px
         ;; border) so the box is exactly WxH and paints no visible gray box; a
         ;; spacer stays invisible and the logo footprint is just its own border.
         ;; The gray/bordered placeholder is kept only for a BROKEN image that
         ;; carries alt text; a decoded image replaces the placeholder outright, so
         ;; strip the UA chrome for it too — otherwise the gray fill shows through a
         ;; transparent image (e.g. an SVG's corners) instead of the page behind.
         (style (if (or decoded (not has-alt))
                    (let ((c (css::copy-cstyle cs)))
                      ;; Drop the UA gray placeholder FILL so the box shows nothing
                      ;; visible, but keep the declared footprint: strip the border
                      ;; only when it's the UA default gray — an AUTHOR border (e.g.
                      ;; HN's logo `border:1px white`) stays, so the box is exactly
                      ;; the browser's reserved size (declared WxH + author border).
                      (setf (css:cstyle-background c) nil (css:cstyle-bg-image c) nil)
                      ;; Compare the EFFECTIVE per-edge colors (BORDER-EDGE-RAW-COLOR),
                      ;; not the shorthand BORDER-COLOR slot: an author `border:1px
                      ;; white solid` sets the per-side colors (leaving the UA gray in
                      ;; the shorthand slot), so only strip when every edge is still the
                      ;; UA default gray — HN's white logo border survives (box 20x20).
                      (when (every (lambda (e) (equal (border-edge-raw-color cs e) '(170 170 180 1.0)))
                                   '(:t :r :b :l))
                        (setf (css:cstyle-border-top-width c) 0.0 (css:cstyle-border-right-width c) 0.0
                              (css:cstyle-border-bottom-width c) 0.0 (css:cstyle-border-left-width c) 0.0))
                      c)
                    cs))
         (bl (used-border style :l)) (br (used-border style :r))
         (bt (used-border style :t)) (bb (used-border style :b))
         (pl (max 0 (css:cstyle-padding-left style))) (pr (max 0 (css:cstyle-padding-right style)))
         (pt (max 0 (css:cstyle-padding-top style))) (pb (max 0 (css:cstyle-padding-bottom style)))
         ;; Inner content box holds the decoded pixels (or alt text); the outer
         ;; box paints the background + border around it, so strip both here.
         (inner (let ((c (css::copy-cstyle style)))
                  (setf (css:cstyle-background c) nil (css:cstyle-bg-image c) nil
                        (css:cstyle-bg-gradient c) nil
                        (css:cstyle-border-top-width c) 0 (css:cstyle-border-right-width c) 0
                        (css:cstyle-border-bottom-width c) 0 (css:cstyle-border-left-width c) 0)
                  c))
         (content (make-lbox :x (+ bl pl) :y (+ bt pt) :w w :h hh
                             ;; a dimensions-only image (header parsed, pixels not
                             ;; decoded) sizes the box but carries no bitmap to blit.
                             :style inner :kind :block :img (and decoded (img-rgba decoded) decoded)))
         (lb (make-lbox :x 0 :y 0 :w (+ bl pl w pr br) :h (+ bt pt hh pb bb)
                        :style style :node node :kind :block :children (list content))))
    (when (and (not decoded) has-alt)
      (setf (lbox-children content)
            (list (make-lbox :x 2 :y (max 0 (floor (- hh *font-h*) 2)) :w (- w 4) :h *font-h* :kind :line
                             :children (list (make-frag :x 2 :w (word-w alt cs) :text alt :style cs :node node))))))
    lb))

(defun object-data-image (node)
  "Decoded IMG for an <object> whose data attribute is a decodable image data:
URI, else NIL (so the object falls back to its child content)."
  (let ((data (cdr (assoc "data" (h:dnode-attrs node) :test #'string-equal))))
    (and data (>= (length data) 5) (string-equal (subseq data 0 5) "data:")
         (ignore-errors (decode-image (if (find #\% data) (percent-decode data) data))))))

(defun object-box (node cs decoded)
  "An atomic replaced box for an <object> rendering DECODED at its intrinsic size.
Carries the object's own style so its background (e.g. Acid2's fixed eye tile)
and borders paint behind/around the image."
  (declare (ignore node))
  (let* ((iw (img-w decoded)) (ih (img-h decoded))
         (bl (used-border cs :l)) (br (used-border cs :r))
         (bt (used-border cs :t)) (bb (used-border cs :b))
         (pl (max 0 (css:cstyle-padding-left cs))) (pr (max 0 (css:cstyle-padding-right cs)))
         (pt (max 0 (css:cstyle-padding-top cs))) (pb (max 0 (css:cstyle-padding-bottom cs)))
         (inner (let ((c (css::copy-cstyle cs)))
                  (setf (css:cstyle-background c) nil (css:cstyle-bg-image c) nil
                        (css:cstyle-bg-gradient c) nil
                        (css:cstyle-border-top-width c) 0 (css:cstyle-border-right-width c) 0
                        (css:cstyle-border-bottom-width c) 0 (css:cstyle-border-left-width c) 0)
                  c)))
    (make-lbox :x 0 :y 0 :w (+ bl pl iw pr br) :h (+ bt pt ih pb bb)
               :style cs :kind :block
               :children (list (make-lbox :x (+ bl pl) :y (+ bt pt) :w iw :h ih
                                          :style inner :kind :block :img decoded)))))

(defun svg-box (node cs)
  "An atomic replaced box for an inline <svg>: intrinsic size from a CSS px width/
height, else the SVG's own width/height attrs or viewBox extent (via stencil).
The subtree is rendered through stencil+gesso during paint."
  (let ((root (dnode->svg-node node)))
    (multiple-value-bind (iw ih) (st:svg-intrinsic-size root)
      (let* ((cw (let ((s (css::resolve-size (css:cstyle-width cs) 300))) (and (numberp s) s)))
             (chh (let ((s (css::resolve-size (css:cstyle-height cs) 150))) (and (numberp s) s)))
             (w (max 1 (round (or cw iw)))) (h (max 1 (round (or chh ih)))))
        (make-lbox :x 0 :y 0 :w w :h h :style cs :node node :kind :block
                   :vpaint (lambda (cv x y bw bh) (declare (ignore bw bh))
                             (paint-svg-box cv x y w h root)))))))

(defun canvas-box (node cs)
  "An atomic replaced box for <canvas>: sized to its width/height attrs (default
300x150).  Its gesso-backed buffer, drawn by page script, is blitted during paint."
  (let* ((w (max 1 (or (img-attr-num node "width")
                       (let ((s (css::resolve-size (css:cstyle-width cs) 300))) (and (numberp s) (round s)))
                       300)))
         (h (max 1 (or (img-attr-num node "height")
                       (let ((s (css::resolve-size (css:cstyle-height cs) 150))) (and (numberp s) (round s)))
                       150))))
    (make-lbox :x 0 :y 0 :w w :h h :style cs :node node :kind :block
               :vpaint (lambda (cv x y bw bh) (declare (ignore bw bh))
                         (paint-canvas-box cv x y node)))))

(defun make-pseudo-node (content)
  "A synthetic inline element carrying generated CONTENT as its only text child,
used to materialise a ::before/::after box in the normal layout flow."
  (let* ((v (make-array 1 :adjustable t :fill-pointer 0))
         (el (h::%dnode :kind :element :name "span" :children v)))
    (when (and content (plusp (length content)))
      (let ((txt (h::%dnode :kind :text :data content)))
        (setf (h:dnode-parent txt) el) (vector-push-extend txt v)))
    el))

(defun pseudo-kids (node styles)
  "Return (values before-node-or-nil after-node-or-nil), registering each
synthetic node's style in STYLES so the normal classifier handles it."
  (flet ((mk (which)
           (let ((pcs (gethash (cons node which) styles)))
             (when pcs
               (let ((pn (make-pseudo-node (css:cstyle-content pcs))))
                 (setf (gethash pn styles) pcs) pn)))))
    (values (mk :before) (mk :after))))

(defun style-size (style)
  "Font-size (px) of a token's style, defaulting to 16 when style is missing."
  (if style (css:cstyle-font-size style) 16))

(defun used-line-height (cs &optional (face (style-face cs)))
  "Used line-height in px for CS (CSS 2.1 10.8): font-size x multiplier.  An
explicit <number>/<length>/<percentage> multiplier is used as-is; `normal`
(stored as the keyword :NORMAL) resolves to the font's own metric factor —
(ascent+|descent|+line-gap)/upem from FACE — instead of a flat 1.2."
  (let ((lh (css:cstyle-line-height cs)) (fs (css:cstyle-font-size cs)))
    (* fs (if (eq lh :normal) (face-normal-lh-factor face) lh))))

(defun apply-text-transform (word transform)
  "Apply CSS text-transform to WORD (a whitespace-delimited token, so `capitalize`
   upper-cases the token's first character)."
  (cond ((or (null transform) (string= transform "none")) word)
        ((string= transform "uppercase") (string-upcase word))
        ((string= transform "lowercase") (string-downcase word))
        ((and (string= transform "capitalize") (plusp (length word)))
         (concatenate 'string (string (char-upcase (char word 0))) (subseq word 1)))
        (t word)))

(defun word-w (word &optional style)
  "Reserved width for WORD at its style's font-size, measured in the style's
resolved face — the width scribe will paint (falls back to the bitmap metric
inside MEASURE-TEXT-WIDTH on font failure).  Includes letter-spacing."
  (round (measure-text-width word (style-size style) (style-face style)
                             (if style (css:cstyle-letter-spacing style) 0))))

(defun space-w (&optional style)
  "Reserved inter-word space width at STYLE's font-size in its resolved face (the
font's space-glyph advance plus letter- and word-spacing)."
  (round (+ (measure-text-width " " (style-size style) (style-face style)
                                (if style (css:cstyle-letter-spacing style) 0))
            (if style (css:cstyle-word-spacing style) 0))))

(defun break-to-width (text style w)
  "Longest character prefix of TEXT (at least one char) whose painted width fits W px,
and the remainder.  Used by word-break:break-all and overflow-wrap:break-word to split
a word that would otherwise overflow.  Returns (values prefix rest)."
  (let ((n (length text)) (k 1))
    (loop while (and (< k n) (<= (word-w (subseq text 0 (1+ k)) style) w)) do (incf k))
    (values (subseq text 0 k) (subseq text k))))

(defun next-float-bottom (y)
  "Smallest float bottom strictly greater than Y, or NIL."
  (let ((best nil))
    (dolist (f *floats*)
      (let ((fb (fifth f))) (when (> fb y) (setf best (if best (min best fb) fb)))))
    best))

(defun layout-inline (words content-x start-y content-w base-cs)
  "Greedy-wrap WORDS into line boxes, flowing around active floats and honoring
text-align.  Returns (values line-boxes total-height)."
  (let* (;; Line-box height is the used line-height = font-size x line-height
         ;; (CSS 2.1 10.8).  The legacy *FONT-H* (bitmap glyph height) floor is
         ;; gone: with real scribe metrics a small font must honour its small
         ;; line box, e.g. Acid2's .chin (font-size 12, line-height 1em -> 12px,
         ;; not 13) so its single line clears the float just below it instead of
         ;; overlapping by 1px and being pushed a full float-height down.  Normal
         ;; 16px text (line-height 1.2 -> 19) is unaffected; only sub-13px lines
         ;; change, and toward the correct (tighter) value.  The used value is
         ;; TRUNCATED (floored) to the pixel, matching how the browser sizes a
         ;; line box: e.g. HN's 7pt subtext (9.33px x 1.15 normal = 10.73) yields
         ;; a 10px line, not 11 — a per-row 1px that otherwise compounds down the
         ;; 30-story list.  Integer / clean-fraction line-heights are unaffected.
         (lh (max 1 (floor (used-line-height base-cs))))
         (align (css:cstyle-text-align base-cs))
         (indent (let ((ti (css:cstyle-text-indent base-cs)))
                   (cond ((and (consp ti) (eq (car ti) :percent)) (* content-w (/ (second ti) 100.0)))
                         ((numberp ti) ti) (t 0))))
         (nowrap (member (css:cstyle-white-space base-cs) '("nowrap" "pre") :test #'string=))
         (cright (+ content-x content-w))
         (ws (coerce words 'vector)) (n (length ws)) (i 0)
         (lines '()) (y start-y) (h 0))
    (loop while (< i n) do
      ;; available band for this line; drop below a float if too narrow
      (multiple-value-bind (lx rx) (float-band y lh content-x cright)
        (loop while (and (< (- rx lx) (* 3 *font-w*)) (next-float-bottom y)) do
          (let ((ny (next-float-bottom y))) (incf h (- ny y)) (setf y ny)
            (multiple-value-setq (lx rx) (float-band y lh content-x cright))))
        (let* ((avail (- rx lx)) (cur '()) (cx (if (null lines) (+ lx indent) lx)) (line-h lh) (broke nil))
          (loop while (< i n) do
            (let ((wd (aref ws i)))
             (if (eq (car wd) :break)
                 (progn (incf i) (setf broke t) (return))   ; forced break: end this line here
             (let* ((atomic (eq (car wd) :atomic))
                   ;; leading gap = inter-word space (only where the source had
                   ;; whitespace and not line-start) + inline horizontal margins
                   (spb (and cur (tok-space wd)))
                   (sw (+ (if spb (space-w (if atomic base-cs (tok-meta wd))) 0)
                          (tok-gap wd)))
                   (ww (if atomic (lbox-w (tok-meta wd)) (word-w (car wd) (tok-meta wd))))
                   (need (+ sw ww)))
              ;; break-all keeps filling the current line (break the word here), so it
              ;; is exempt from the wrap-to-next-line check.
              (when (and cur (not nowrap) (> (+ (- cx lx) need) avail)
                         (not (and (not atomic)
                                   (string= (css:cstyle-word-break (tok-meta wd)) "break-all"))))
                (return))
              (when (and (> sw 0) (> (- cx lx) 0)) (incf cx sw))
              (let ((room (- avail (- cx lx))))
                (cond
                  ;; word-break:break-all (break anywhere) or overflow-wrap:break-word
                  ;; on a word too wide to fit any line: place the char-prefix that fits
                  ;; and requeue the remainder to start the next line.
                  ((and (not atomic) (not nowrap) (> ww room) (>= room 1)
                        (let ((wb (css:cstyle-word-break (tok-meta wd)))
                              (ow (css:cstyle-overflow-wrap (tok-meta wd))))
                          (or (string= wb "break-all")
                              (and (member ow '("break-word" "anywhere") :test #'string=)
                                   (> ww avail)))))
                   (multiple-value-bind (prefix rest) (break-to-width (car wd) (tok-meta wd) room)
                     (cond
                       ((zerop (length prefix))          ; nothing fits: wrap if the line has content
                        (if cur (return)
                            (progn (push (make-frag :x cx :w ww :text (car wd) :style (tok-meta wd) :node (tok-node wd)) cur)
                                   (incf cx ww) (incf i))))
                       (t (push (make-frag :x cx :w (word-w prefix (tok-meta wd)) :text prefix :style (tok-meta wd) :node (tok-node wd)) cur)
                          (cond ((plusp (length rest))
                                 (setf (aref ws i) (list rest (tok-meta wd) nil 0 (tok-node wd)))
                                 (return))               ; remainder -> next line
                                (t (incf cx (word-w prefix (tok-meta wd))) (incf i)))))))
                  (atomic
                   (let ((lb (tok-meta wd)))
                     (shift-box lb (round (- cx (lbox-x lb))) 0)
                     (setf line-h (max line-h (lbox-h lb))) (push lb cur))
                   (incf cx ww) (incf i))
                  (t (push (make-frag :x cx :w ww :text (car wd) :style (tok-meta wd) :node (tok-node wd)) cur)
                     (incf cx ww) (incf i))))))))
          (when (and (null cur) (not broke))     ; one item too wide for the band: force it
            (let* ((wd (aref ws i)))
              (if (eq (car wd) :atomic) (let ((lb (tok-meta wd))) (shift-box lb (round (- lx (lbox-x lb))) 0)
                                          (setf line-h (max line-h (lbox-h lb))) (push lb cur))
                  (push (make-frag :x lx :w (word-w (car wd) (tok-meta wd)) :text (car wd) :style (tok-meta wd) :node (tok-node wd)) cur))
              (incf i)))
          (let* ((items (nreverse cur))
                 (lastx (if items
                            (let ((it (car (last items)))) (if (frag-p it) (+ (frag-x it) (frag-w it)) (+ (lbox-x it) (lbox-w it))))
                            lx))    ; blank line (pre-line/pre-wrap forced break): empty line box
                 (used (- lastx lx))
                 (shift (cond ((string= align "center") (max 0 (floor (- avail used) 2)))
                              ((string= align "right") (max 0 (- avail used))) (t 0))))
            (when (plusp shift)
              (dolist (it items) (if (frag-p it) (incf (frag-x it) shift) (shift-box it shift 0))))
            (when (and (string= align "justify") (< i n))
              (let* ((frags (remove-if-not #'frag-p items))
                     (f (length frags)))
                (when (>= f 2)
                  (let ((extra (- avail used)))
                    (when (plusp extra)
                      (loop for j from 0 for it in frags
                            do (incf (frag-x it) (round (* j (/ extra (1- f)))))))))))
            (dolist (it items) (unless (frag-p it) (shift-box it 0 (round y))))  ; atomic to line y
            (push (make-lbox :x lx :y y :w avail :h line-h :kind :line :children items) lines))
          (incf y line-h) (incf h line-h))))
    (values (nreverse lines) h)))

(defun collect-raw (node)
  "Raw text of NODE preserving whitespace (for <pre>)."
  (with-output-to-string (o)
    (labels ((rec (n) (case (h:dnode-kind n) (:text (write-string (h:dnode-data n) o))
                        (:element (loop for c across (h:dnode-children n) do (rec c))))))
      (rec node))))
(defun split-newlines (s)
  (loop with start = 0 for i from 0 to (length s)
        when (or (= i (length s)) (char= (char s i) #\Newline))
          collect (prog1 (subseq s start i) (setf start (1+ i)))))
(defun has-block-children (styles node)
  (some (lambda (c) (block-level-p styles c))
        (loop for c across (h:dnode-children node) collect c)))

;;; ---- block layout -------------------------------------------------------
(defvar *layout-debug* nil)
(defparameter *max-layout-depth* 400
  "Cap on layout recursion depth.  Real pages nest far shallower; capping keeps a
pathological or hostile document (thousands of nested boxes) from exhausting the
control stack — the outer levels render and the runaway interior is dropped, rather
than the whole render crashing.")
(defvar *layout-depth* 0)
(defun layout-node (node styles x y avail-w &optional avail-h)
  "Resilient wrapper: a failing subtree degrades to an empty box, not a crash.
AVAIL-H is the containing-block height in px when definite, else NIL (CSS 2.1
10.5: a percentage height resolves against it only when definite)."
  (if (> *layout-depth* *max-layout-depth*)
      (values nil 0 0 0)                 ; too deep: drop this subtree instead of crashing
      (let ((*layout-depth* (1+ *layout-depth*)))
        (handler-case (%layout-node node styles x y avail-w avail-h)
          (error (e) (if *layout-debug* (error e) (values nil 0 0 0)))))))

(defun used-border (cs edge)
  "Used border width for EDGE (:t :r :b :l): the declared width, or 0 when that
edge's border-style is none/hidden (CSS 2.1 8.5.1 — a none/hidden border forces
the used width to 0).  Unlike the paint helper BORDER-EDGE-WIDTH this keeps
*transparent* borders at full width: transparent is a colour, not a style, so
those borders still occupy layout space (e.g. Acid2's smile `solid transparent`)."
  (multiple-value-bind (w sty)
      (case edge
        (:t (values (css:cstyle-border-top-width cs) (css:cstyle-border-top-style cs)))
        (:r (values (css:cstyle-border-right-width cs) (css:cstyle-border-right-style cs)))
        (:b (values (css:cstyle-border-bottom-width cs) (css:cstyle-border-bottom-style cs)))
        (:l (values (css:cstyle-border-left-width cs) (css:cstyle-border-left-style cs))))
    (if (css:border-edge-painted-p sty) w 0.0)))

(defun pad-box (lb cs)
  "Padding box (px py pw ph) of LB — the border-box minus borders.  This is the
containing block that absolutely-positioned descendants resolve against."
  (let ((bl (used-border cs :l)) (br (used-border cs :r))
        (bt (used-border cs :t)) (bb (used-border cs :b)))
    (list (+ (lbox-x lb) bl) (+ (lbox-y lb) bt)
          (max 0 (- (lbox-w lb) bl br)) (max 0 (- (lbox-h lb) bt bb)))))

(defun resolve-positioned (lb cb cs)
  "Shift out-of-flow box LB to its final position within containing block
CB=(px py pw ph) using top/left/right/bottom from CS.  When top (or left) is
:auto and so is bottom (right), the box keeps its static-flow position."
  (when (and lb cb)
    (destructuring-bind (px py pw ph) cb
      (let* ((left (css:cstyle-left cs)) (right (css:cstyle-right cs))
             (top (css:cstyle-top cs)) (bottom (css:cstyle-bottom cs))
             ;; CSS 2.1 10.6.4 / 9.3.2: top/left position the *margin* edge, so the
             ;; border box is offset inward by the box's own margin (the static
             ;; case already carries the margin in LBOX-X/Y, so leave it alone).
             (mt (css:cstyle-margin-top cs)) (mb (css:cstyle-margin-bottom cs))
             (ml (css:cstyle-margin-left cs)) (mr (css:cstyle-margin-right cs))
             (nx (cond ((numberp left)  (+ px left ml))
                       ((numberp right) (+ px (- pw (lbox-w lb) right mr)))
                       (t (lbox-x lb))))
             (ny (cond ((numberp top)    (+ py top mt))
                       ((numberp bottom) (+ py (- ph (lbox-h lb) bottom mb)))
                       (t (lbox-y lb)))))
        (shift-box lb (round (- nx (lbox-x lb))) (round (- ny (lbox-y lb))))))))

(defun %layout-node (node styles x y avail-w &optional avail-h)
  "Establish an absolute containing block for positioned elements, then lay the
node out.  A positioned element (relative/absolute/fixed) is the containing block
for its absolutely-positioned descendants; we collect those during subtree layout
and resolve them once this box's geometry is known (then a later unit-shift of
this box, if any, carries them along correctly)."
  (let ((cs (st styles node)))
    (if (and cs (member (css:cstyle-position cs) '("relative" "absolute" "fixed") :test #'string=))
        (let ((*abs-pending* nil)
              ;; Absolutely-positioned and fixed boxes establish a new block
              ;; formatting context (CSS 2.1 9.4.1): floats inside them must not
              ;; interact with floats in the surrounding BFC, and vice versa.
              ;; Rebind *FLOATS* to NIL so e.g. the float inside Acid2's absolute
              ;; .eyes box does not shove the .nose float sideways in the picture.
              (*floats* (if (member (css:cstyle-position cs) '("absolute" "fixed") :test #'string=)
                            nil *floats*)))
          (multiple-value-bind (lb adv mt-eff mb-eff mneg) (%layout-core node styles x y avail-w avail-h)
            ;; A box establishing a BFC contains its floats: with auto height it
            ;; grows so its bottom border edge sits below the lowest float's bottom
            ;; margin edge (CSS 2.1 10.6.7).  Acid2's `.first.one` is an absolute
            ;; auto-height block whose only content is a float — its height is that
            ;; float's 12px, not 0.  (Only the abs/fixed case rebinds *FLOATS*, so
            ;; here every entry was generated inside this box.)
            (when (and lb *floats*
                       (member (css:cstyle-position cs) '("absolute" "fixed") :test #'string=)
                       (null (css::resolve-height (css:cstyle-height cs) avail-h)))
              (let ((mfb (loop for f in *floats* maximize (fifth f)))
                    (bot (+ (lbox-y lb) (lbox-h lb))))
                (when (> mfb bot)
                  (setf (lbox-h lb) (- (+ mfb (used-border cs :b)) (lbox-y lb))))))
            (when (and lb *abs-pending*)
              (let ((cb (pad-box lb cs)))
                (dolist (p *abs-pending*) (resolve-positioned (car p) cb (cdr p)))))
            (values lb adv mt-eff mb-eff mneg)))
        (%layout-core node styles x y avail-w avail-h))))

(defun collapse-margins (&rest ms)
  "CSS 2.1 8.3.1 collapsed margin of MS: sum of the largest positive and the
most-negative margin.  {20,30}->30  {20,-10}->10  {-5,-8}->-8  {-20,30}->10."
  (+ (reduce #'max ms :initial-value 0)
     (reduce #'min ms :initial-value 0)))

(defun %layout-core (node styles x y avail-w &optional avail-h)
  "Lay out block-level NODE at (X,Y); AVAIL-W is the containing content width and
AVAIL-H the containing-block height (px when definite, else NIL).
Returns (values lbox advance-height)."
  (let ((cs (st styles node)))
    (when (or (null cs) (string= (cdisplay cs) "none")) (return-from %layout-core (values nil 0 0 0)))
    (let* ((mt (css:cstyle-margin-top cs)) (mb (css:cstyle-margin-bottom cs))
           (ml (css:cstyle-margin-left cs)) (mr (css:cstyle-margin-right cs))
           (pt (css:cstyle-padding-top cs)) (pb (css:cstyle-padding-bottom cs))
           (pl (css:cstyle-padding-left cs)) (pr (css:cstyle-padding-right cs))
           (bt (used-border cs :t)) (bb (used-border cs :b))
           (bl (used-border cs :l)) (br (used-border cs :r))
           (border-box (string= (css:cstyle-box-sizing cs) "border-box"))
           ;; a table-cell fills the column width its table assigned it (AVAIL-W):
           ;; the column model already folded in any specified cell width, so the
           ;; cell itself must ignore its own width and stretch to the column.
           (table-cell (string= (cdisplay cs) "table-cell"))
           (pad-bord (+ pl pr bl br))
           ;; explicit height (CSS 2.1 10.5): a px length, or a percentage resolved
           ;; against the containing-block height AVAIL-H when definite — else NIL
           ;; (the percentage computes to auto and content drives the height).
           (exp-h (css::resolve-height (css:cstyle-height cs) avail-h))    ; px or nil
           ;; content height handed to children as THEIR containing-block height:
           ;; this box's content-box height when its height is explicit, else NIL
           ;; (auto height is indefinite, so child percentage heights -> auto).
           (child-avail-h (when (numberp exp-h)
                            (max 0 (if (string= (css:cstyle-box-sizing cs) "border-box")
                                       (- exp-h (+ (css:cstyle-padding-top cs) (css:cstyle-padding-bottom cs)
                                                   (used-border cs :t) (used-border cs :b)))
                                       exp-h))))
           (spec-w (unless table-cell (css::resolve-size (css:cstyle-width cs) avail-w))) ; px or nil
           (max-w (css::resolve-size (css:cstyle-max-width cs) avail-w))
           (min-w (css::resolve-size (css:cstyle-min-width cs) avail-w))
           ;; a box with width:auto that is shrink-to-fit (CSS 10.3.7 / 10.3.9):
           ;; absolute/fixed boxes AND atomic inlines (inline-block/-flex/-table)
           ;; size to min(available, max-content), not the full available width —
           ;; else an auto-width inline-block (e.g. an icon) balloons to fill.
           (shrink (and (null spec-w)
                        (or (member (css:cstyle-position cs) '("absolute" "fixed") :test #'string=)
                            (member (cdisplay cs) '("inline-block" "inline-flex" "inline-table")
                                    :test #'string=))))
           ;; a display:table box with auto width is shrink-to-fit (CSS 17.5.2):
           ;; it sizes to the sum of its column widths, not the available width.
           (table-shrink (and (null spec-w) (string= (cdisplay cs) "table")))
           ;; border-box width of this element
           (width (let ((bw (cond ((numberp spec-w) (if border-box spec-w (+ spec-w pad-bord)))
                                  (shrink (min (- avail-w ml mr)
                                               (+ (pref-content-width node styles (- avail-w ml mr))
                                                  pad-bord)))
                                  (table-shrink
                                   (let ((nat (table-natural-width node styles (- avail-w ml mr))))
                                     (if (plusp nat)
                                         (min (- avail-w ml mr) (+ nat pad-bord))
                                         (- avail-w ml mr))))
                                  (t (- avail-w ml mr)))))
                    (when (numberp max-w) (setf bw (min bw (if border-box max-w (+ max-w pad-bord)))))
                    (when (numberp min-w) (setf bw (max bw (if border-box min-w (+ min-w pad-bord)))))
                    (max 0 bw)))
           ;; margin:auto centering when width is constrained
           (ml (if (and (css:cstyle-margin-left-auto cs) (css:cstyle-margin-right-auto cs)
                        (or (numberp spec-w) (numberp max-w)) (< width avail-w))
                   (max 0 (floor (- avail-w width) 2)) ml))
           (content-w (max 0 (- width pad-bord)))
           (box-x (+ x ml)) (box-y (+ y mt))
           (cx (+ box-x bl pl)) (cy (+ box-y bt pt))
           ;; a list-item generates a marker — UNLESS it's a direct child of a
           ;; table box (it gets wrapped in an anonymous cell and the marker is
           ;; suppressed, as browsers do; e.g. Acid2's display:table <ul> items).
           (list-item (and (string= (cdisplay cs) "list-item")
                           (not (let ((p (h:dnode-parent node)))
                                  (and p (eq (h:dnode-kind p) :element)
                                       (let ((pcs (st styles p)))
                                         (and pcs (member (cdisplay pcs)
                                                          '("table" "table-row" "table-row-group"
                                                            "table-header-group" "table-footer-group")
                                                          :test #'string=))))))))
           ;; effective (collapsed) outer margins reported up to the parent:
           ;; default to this box's own margins, raised by parent/child collapse.
           (mt-eff mt) (mb-eff mb)
           ;; most-negative collapsing margin anywhere in this box's adjoining set,
           ;; surfaced so a self-collapsing parent can keep it alive (see PREV-MB).
           (box-mneg (min mt mb))
           (children '()) (content-h 0))
      ;; <pre>/white-space:pre — preserve newlines, no wrapping
      (when (and (string= (css:cstyle-white-space cs) "pre") (not (has-block-children styles node)))
        (let* ((text (collect-raw node)) (yy cy)
               (lh (max *font-h* (round (used-line-height cs)))))
          (dolist (ln (split-newlines text))
            (push (make-lbox :x cx :y yy :w content-w :h lh :kind :line
                             :children (when (plusp (length ln))
                                         (list (make-frag :x cx :w (word-w ln cs) :text ln :style cs :node node))))
                  children)
            (incf yy lh) (incf content-h lh)))
        (let* ((box-h (+ content-h pt pb bt bb))
               (lb (make-lbox :x box-x :y box-y :w width :h box-h :style cs :node node
                              :kind :block :children (nreverse children))))
          (return-from %layout-core (values lb (+ mt box-h mb) mt mb))))
      ;; flex / table containers
      (when (member (cdisplay cs) '("flex" "table") :test #'string=)
        (multiple-value-bind (boxes ch)
            (if (string= (cdisplay cs) "flex")
                (layout-flex node styles cx cy content-w cs)
                (layout-table node styles cx cy content-w cs))
          (let* ((box-h (+ ch pt pb bt bb))
                 (lb (make-lbox :x box-x :y box-y :w width :h box-h :style cs :node node
                                :kind :block :children boxes)))
            (return-from %layout-core (values lb (+ mt box-h mb) mt mb)))))
      ;; classify children: anonymous-group consecutive inline-level nodes.
      ;; ::before / ::after generated boxes bracket the real children.
      ;; YY tracks the border-bottom edge of the last in-flow block placed (or
      ;; CY).  PREV-MB is that block's bottom margin, held pending so it can
      ;; collapse (CSS 2.1 8.3.1) with the next sibling's top margin instead of
      ;; simply summing.  NIL = no collapsible margin precedes the next block
      ;; (start of flow, or inline/clearance separates them).
      (let ((kids (multiple-value-bind (before after) (pseudo-kids node styles)
                    (append (when before (list before))
                            (coerce (h:dnode-children node) 'list)
                            (when after (list after)))))
            ;; PREV-MB is the pending collapsible margin of the last in-flow
            ;; block, kept as a (max-positive . min-negative) pair (NIL = none) so
            ;; that a negative margin which is momentarily dominated by a larger
            ;; positive one still survives to collapse with a later sibling
            ;; (CSS 2.1 8.3.1).  MIN-CN tracks the most-negative collapsing margin
            ;; anywhere inside, surfaced as MNEG for a self-collapsing parent.
            (group '()) (yy cy) (prev-mb nil) (content-started nil) (first-child-mt nil)
            (min-cn 0))
        (flet ((flush-inline ()
                 (when group
                   (let ((words (collect-words (nreverse group) styles cs content-w)))
                     (when words
                       ;; The preceding block's pending bottom margin applies in
                       ;; FULL before inline content — inline boxes have no
                       ;; collapsible margin, so it does not collapse away (a block
                       ;; followed by an inline sibling, e.g. an <h2> then its
                       ;; `.mw-editsection` [edit] span, keeps that margin between
                       ;; them).  Only non-empty inline runs count; whitespace that
                       ;; collapses to nothing leaves PREV-MB to meet the next block.
                       (when prev-mb
                         (let ((m (+ (max 0 (car prev-mb)) (min 0 (cdr prev-mb)))))
                           (incf yy m) (incf content-h m)))
                       (setf prev-mb nil)
                       (multiple-value-bind (lines lh-total) (layout-inline words cx yy content-w cs)
                         (dolist (l lines) (push l children))
                         (incf yy lh-total) (incf content-h lh-total)
                         (setf content-started t))))
                   (setf group '()))))
          (dolist (k kids)
            (let* ((kcs (st styles k))
                   (pos (and kcs (css:cstyle-position kcs))))
              (cond
                ((and kcs (member pos '("absolute" "fixed") :test #'string=))   ; out of flow
                 ;; Lay out at the static-flow point (this is the position used
                 ;; when top/left are auto), then defer final placement: collect
                 ;; against the nearest positioned ancestor (absolute) or the
                 ;; viewport (fixed).  Out-of-flow boxes never affect content-h.
                 (multiple-value-bind (lb adv) (layout-node k styles cx yy content-w child-avail-h)
                   (declare (ignore adv))
                   (when lb
                     (if (string= pos "fixed")
                         (push (cons lb kcs) *fixed-pending*)
                         (push (cons lb kcs) *abs-pending*))
                     (push lb children))))
                ((float-p styles k)                                              ; float
                 ;; A float's top margin edge sits at the position the next in-flow
                 ;; box would take: the preceding block's pending (collapsed) bottom
                 ;; margin still separates them (floats don't collapse, but they are
                 ;; placed AFTER that margin).  PREV-MB is preserved so it goes on to
                 ;; collapse with the block following the float (CSS 2.1 9.5 / 8.3.1).
                 (let* ((pend (if prev-mb (+ (max 0 (car prev-mb)) (min 0 (cdr prev-mb))) 0))
                        (lb (place-float k styles cx (+ cx content-w) (+ yy pend) content-w)))
                   (when lb (push lb children))))
                ((block-level-p styles k)
                 (flush-inline)
                 (let ((own-mt (if kcs (css:cstyle-margin-top kcs) 0))
                       (clear-sides (and kcs (member (css:cstyle-clear kcs) '("left" "right" "both") :test #'string=)
                                         (case (intern (string-upcase (css:cstyle-clear kcs)) :keyword)
                                           (:left '(:left)) (:right '(:right)) (t '(:left :right))))))
                     ;; lay the child out at YY, then SHIFT it so its border-top
                     ;; lands at FINAL-TOP (= YY+GAP, GAP = the collapsed adjoining
                     ;; margin; pushed down by clearance if a cleared box must drop
                     ;; below a float).  CMT/CMB are the child's *effective* margins
                     ;; (already collapsed with its own first/last child).
                     (multiple-value-bind (lb adv cmt cmb cmneg) (layout-node k styles cx yy content-w child-avail-h)
                       (declare (ignore adv))
                       (when lb
                         (let* ((cmt (or cmt own-mt))
                                (cmb (or cmb (if kcs (css:cstyle-margin-bottom kcs) 0)))
                                (cmneg (or cmneg 0))
                                ;; parent/first in-flow child top-margin collapse:
                                ;; only with zero top border AND padding, at the
                                ;; very start of flow (nothing precedes the child).
                                (top-collapse (and (not content-started)
                                                   (zerop bt) (zerop pt)))
                                (gap (cond (prev-mb (+ (max (car prev-mb) (max 0 cmt))
                                                       (min (cdr prev-mb) (min 0 cmt))))
                                           (top-collapse 0)   ; margin bubbles to parent's mt-eff
                                           (t cmt)))
                                (natural-top (+ yy gap))
                                ;; Clearance (CSS 2.1 9.5.2 / 8.3.1) only ever moves
                                ;; a cleared box DOWN to clear floats: if its natural
                                ;; (margin-collapsed) top is already at or below the
                                ;; relevant float bottom, clearance is zero/negative
                                ;; and the box stays at its natural position — the
                                ;; negative-clearance case Acid2's smile depends on.
                                (float-bottom (and clear-sides
                                                   (clear-y yy cx (+ cx content-w) clear-sides)))
                                (cleared (and float-bottom (> float-bottom (max yy natural-top))))
                                (final-top (if cleared float-bottom natural-top)))
                           (when (and top-collapse (not cleared)) (setf first-child-mt cmt))
                           (shift-box lb 0 (round (- final-top (lbox-y lb))))   ; flow placement
                           (push lb children)
                           (setf min-cn (min min-cn 0 cmt cmb cmneg))
                           (if (and (zerop (lbox-h lb)) (not cleared))
                               ;; Self-collapsing block (CSS 2.1 8.3.1): a zero-
                               ;; height, border/padding-free box has its top and
                               ;; bottom margins adjoining, so they collapse with
                               ;; BOTH the previous block's pending margin and the
                               ;; next block's top margin.  Do not advance the flow
                               ;; position; instead fold this box's margins (keeping
                               ;; the most-negative one, CMNEG) into the pending pair
                               ;; so the next sibling collapses through it (this is
                               ;; what lets Acid2's empty .empty box pull the smile
                               ;; up to clear-abut the nose instead of voiding below).
                               (setf prev-mb (cons (max (if prev-mb (car prev-mb) 0) (max 0 cmt cmb))
                                                   (min (if prev-mb (cdr prev-mb) 0) (min 0 cmt cmb cmneg))))
                               (let ((new-yy (+ (lbox-y lb) (lbox-h lb))))      ; border-bottom edge
                                 (incf content-h (- new-yy yy))
                                 (setf yy new-yy)
                                 (setf prev-mb (cons (max 0 cmb) (min 0 cmb))) ; held to collapse with next sibling
                                 (setf content-started t)))
                           ;; Relative offset is a VISUAL shift only (CSS 2.1 9.4.3):
                           ;; applied after flow bookkeeping above so it never feeds
                           ;; back into YY / CONTENT-H — Acid2's .smile div has
                           ;; bottom:-1em and must not stretch its parent by 12px.
                           (when (and kcs (string= pos "relative"))
                             (shift-box lb (round (cond ((numberp (css:cstyle-left kcs)) (css:cstyle-left kcs))
                                                        ((numberp (css:cstyle-right kcs)) (- (css:cstyle-right kcs))) (t 0)))
                                        (round (cond ((numberp (css:cstyle-top kcs)) (css:cstyle-top kcs))
                                                     ((numberp (css:cstyle-bottom kcs)) (- (css:cstyle-bottom kcs))) (t 0))))))))))
                ((or (eq (h:dnode-kind k) :text) (inline-level-p styles k)) (push k group)))))
          (flush-inline)
          ;; The last in-flow block's bottom margin (PREV-MB) was held back from
          ;; YY.  Parent/last-child collapse (CSS 2.1 8.3.1): when this box has
          ;; auto height and zero bottom border AND padding, that margin sticks
          ;; out below and collapses into MB-EFF; otherwise it is contained and
          ;; adds to the content height.
          (when prev-mb
            (let ((height-auto (not (numberp exp-h)))
                  (pm (+ (car prev-mb) (cdr prev-mb))))
              (if (and height-auto (zerop bb) (zerop pb))
                  (setf mb-eff (collapse-margins mb (car prev-mb) (cdr prev-mb)))
                  (incf content-h pm))))
          ;; parent/first-child collapse: first child's top margin bubbled up.
          (when first-child-mt
            (setf mt-eff (collapse-margins mt first-child-mt)))
          (setf box-mneg (min mt mb min-cn))))
      ;; A used content height is never negative (CSS 2.1 10.6.3): a self-
      ;; collapsing last child with a contained negative bottom margin (Acid2's
      ;; smile: strong margin-bottom:-1em inside em's top+bottom borders) drives
      ;; CONTENT-H below zero — clamp so the border-bottom sits at the content top,
      ;; not pulled up through it (gives em its 24px yellow-over-black mouth).
      (let* ((content-final (cond ((numberp exp-h) (if border-box (- exp-h pad-bord) exp-h)) (t (max 0 content-h))))
             (box-h0 (+ content-final pt pb bt bb))
             ;; min/max-height as box-height floor/ceiling (CSS 2.1 10.7): a
             ;; percentage resolves against the CB height AVAIL-H when definite,
             ;; else min-height->0 and max-height->none.  Apply max FIRST then min
             ;; so min-height always wins when the two conflict.
             (box-h (let ((bh box-h0)
                          (mn (css::resolve-min-height (css:cstyle-min-height cs) avail-h))
                          (mx (css::resolve-max-height (css:cstyle-max-height cs) avail-h)))
                      (when (numberp mx) (setf bh (min bh (+ mx pt pb bt bb))))
                      (when (and (numberp mn) (> mn 0)) (setf bh (max bh (+ mn pt pb bt bb))))
                      (max bh (if list-item *font-h* 0))))
             (lb (make-lbox :x box-x :y box-y :w width :h box-h
                            :style cs :node node :kind :block :children (nreverse children)
                            :marker (when list-item (css:cstyle-list-style cs)))))
        (values lb (+ mt (lbox-h lb) mb) mt-eff mb-eff box-mneg)))))

(defun shift-box (lb dx dy)
  "Recursively offset LB and its descendants by (DX,DY)."
  (when lb
    (incf (lbox-x lb) dx) (incf (lbox-y lb) dy)
    (if (eq (lbox-kind lb) :line)
        (dolist (it (lbox-children lb))
          (if (frag-p it) (incf (frag-x it) dx) (shift-box it dx dy)))
        (dolist (c (lbox-children lb)) (shift-box c dx dy)))))


(defun item-base (item styles content-w)
  "The flex base size of ITEM: its flex-basis (a length), else its used width, else
   — flex-basis auto/content — its content (max-content) size.  flex-grow only adds
   POSITIVE free space on top of this base (CSS 9.7); a grow item must still reserve
   its content, or it collapses to nothing when the line is already full."
  (let* ((cs (st styles item)) (basis (css:cstyle-flex-basis cs))
         (w (css:cstyle-width cs)))
    (cond
      ((and (stringp basis) (not (member basis '("auto" "content") :test #'string=)))
       (let ((v (css::resolve-len basis (css:cstyle-font-size cs)))) (if (numberp v) v 0)))
      ((numberp w) w)
      ;; width:N% on a flex item resolves against the line's available width.
      ((and (consp w) (eq (car w) :percent))
       (* content-w (/ (second w) 100.0)))
      ;; flex-basis auto with no width: the item's max-content size, measured by
      ;; the structural intrinsic pass (flex rows sum, block stacks take the widest,
      ;; out-of-flow is skipped) rather than the crude inline-flatten estimate.
      (t (min content-w (pref-content-width item styles content-w))))))

(defun layout-flex (node styles cx cy content-w base-cs)
  "Single-line flexbox layout.  Returns (values child-lboxes content-height)."
  (let* ((dir (css:cstyle-flex-direction base-cs))
         (row (not (or (string= dir "column") (string= dir "column-reverse"))))
         (justify (css:cstyle-justify-content base-cs))
         (align (css:cstyle-align-items base-cs))
         (gap (css:cstyle-gap base-cs))
         (items (remove-if-not (lambda (k) (let ((c (st styles k))) (and c (not (string= (css:cstyle-display c) "none"))))) (child-elements node)))
         ;; *-reverse lays the items out in reverse order along the main axis.
         (items (if (member dir '("row-reverse" "column-reverse") :test #'string=) (reverse items) items))
         (nitems (length items)))
    (when (zerop nitems) (return-from layout-flex (values nil 0)))
    (let* ((main-avail (if row content-w content-w))   ; column main size is intrinsic; treat width as cross
           (bases (mapcar (lambda (it) (if row (item-base it styles content-w) (item-base it styles content-w))) items))
           (total-gap (* gap (1- nitems)))
           (sum-base (+ (reduce #'+ bases) total-gap))
           (free (- main-avail sum-base))
           (grows (mapcar (lambda (it) (css:cstyle-flex-grow (st styles it))) items))
           (sum-grow (reduce #'+ grows))
           ;; flex-shrink is weighted by shrink-factor * base-size (CSS 9.7): an item
           ;; with a larger base gives up proportionally more of the negative free space.
           (shrinks (mapcar (lambda (it) (css:cstyle-flex-shrink (st styles it))) items))
           (scaled (mapcar #'* shrinks bases))
           (sum-scaled (reduce #'+ scaled))
           (sizes (cond ((and row (> free 0) (> sum-grow 0))
                         (mapcar (lambda (b g) (+ b (* free (/ g sum-grow)))) bases grows))
                        ((and row (< free 0) (> sum-scaled 0))
                         (mapcar (lambda (b sc) (max 0 (+ b (* free (/ sc sum-scaled)))))
                                 bases scaled))
                        (t bases))))
      (if row
          ;; ---- ROW ----
          (let* ((used (+ (reduce #'+ sizes) total-gap))
                 (extra (max 0 (- content-w used)))
                 (start (cond ((string= justify "center") (+ cx (/ extra 2)))
                              ((string= justify "flex-end") (+ cx extra)) (t cx)))
                 (between (cond ((and (string= justify "space-between") (> nitems 1)) (/ extra (1- nitems)))
                                ((string= justify "space-around") (/ extra nitems)) (t 0)))
                 (x (if (string= justify "space-around") (+ start (/ between 2)) start))
                 (boxes '()) (max-h 0))
            (loop for it in items for w in sizes do
              (multiple-value-bind (lb adv) (layout-node it styles (round x) cy (round w))
                (declare (ignore adv))
                (when lb (push lb boxes) (setf max-h (max max-h (lbox-h lb))))
                (incf x (+ w gap between))))
            (let ((boxes (nreverse boxes)))
              (dolist (lb boxes)                          ; cross-axis align
                (cond ((string= align "stretch") (setf (lbox-h lb) (max (lbox-h lb) max-h)))
                      ((string= align "center") (shift-box lb 0 (round (/ (- max-h (lbox-h lb)) 2))))
                      ((string= align "flex-end") (shift-box lb 0 (round (- max-h (lbox-h lb)))))))
              (values boxes max-h)))
          ;; ---- COLUMN ----
          (let ((y cy) (boxes '()) (max-w 0))
            (loop for it in items do
              (multiple-value-bind (lb adv) (layout-node it styles cx y content-w)
                (when lb (push lb boxes) (setf max-w (max max-w (lbox-w lb))))
                (incf y (+ adv gap))))
            (let ((boxes (nreverse boxes)))
              ;; cross-axis (horizontal) alignment of the column's items
              (dolist (lb boxes)
                (cond ((string= align "center") (shift-box lb (round (/ (- content-w (lbox-w lb)) 2)) 0))
                      ((string= align "flex-end") (shift-box lb (round (- content-w (lbox-w lb))) 0))))
              (values boxes (- y cy gap))))))))

(defun cell-like-p (c styles)
  "True when child C of a table participates as a cell — i.e. it is not itself an
internal-table box (row/row-group/column/caption) nor display:none.  Anything
else (table-cell, or a stray block/list-item/table) is wrapped in an anonymous
cell per CSS 17.2.1."
  (let ((d (cdisplay (st styles c))))
    (not (member d '("table-row" "table-row-group" "table-header-group"
                     "table-footer-group" "table-column" "table-column-group"
                     "table-caption" "none")
                 :test #'string=))))
(defun table-rows (node styles)
  "Collect <tr> rows directly under NODE or within row-groups.  When the table
has no explicit row boxes but does carry cell-like children, CSS 17.2.1 wraps
them in an anonymous table-row — represented here by the table NODE itself."
  (let ((rows '()))
    (dolist (c (child-elements node))
      (let ((d (cdisplay (st styles c))))
        (cond ((string= d "table-row") (push c rows))
              ((member d '("table-row-group" "table-header-group" "table-footer-group")
                       :test #'string=)
               (dolist (r (child-elements c)) (when (string= (cdisplay (st styles r)) "table-row") (push r rows)))))))
    (or (nreverse rows)
        (when (some (lambda (c) (cell-like-p c styles)) (child-elements node))
          (list node)))))
(defun row-cells (row styles &optional table)
  "Cells of ROW.  A real <tr> contributes its table-cell children; an anonymous
row (ROW eq the TABLE node) wraps every cell-like child as a cell."
  (if (and table (eq row table))
      (remove-if (lambda (c) (not (cell-like-p c styles))) (child-elements row))
      (remove-if-not (lambda (c) (string= (cdisplay (st styles c)) "table-cell")) (child-elements row))))

(defun cell-pad-bord (cs)
  "Left+right padding + border of a cell's box."
  (if cs (+ (css:cstyle-padding-left cs) (css:cstyle-padding-right cs)
            (used-border cs :l) (used-border cs :r))
      0))

(defun cell-colspan (cell)
  "The td/th colspan (>=1)."
  (let ((v (cdr (assoc "colspan" (h:dnode-attrs cell) :test #'string-equal))))
    (max 1 (or (and v (ignore-errors (parse-integer (string-trim '(#\Space) v) :junk-allowed t))) 1))))

(defun cell-lbox-valign-top-p (lb)
  "True when the cell box LB is TOP-aligned — legacy valign=\"top\" (HTML §14.3).
Such a cell keeps its content at the cell top when the row is taller; every other
cell (default vertical-align:baseline) sinks its single line to the cell bottom."
  (let ((node (lbox-node lb)))
    (and node (eq (h:dnode-kind node) :element)
         (let ((v (cdr (assoc "valign" (h:dnode-attrs node) :test #'string-equal))))
           (and v (string-equal (string-trim '(#\Space) v) "top"))))))

(defun cell-inline-only-p (lb)
  "True when cell box LB holds only inline content (its children are all line
boxes).  A single line of such content is the case CSS baseline-aligns to the
cell bottom; a cell with block children (HN's votearrow div) is left top-aligned."
  (let ((kids (lbox-children lb)))
    (and kids (every (lambda (c) (eq (lbox-kind c) :line)) kids))))

(defun baseline-sink-cell (lb rowh)
  "Sink a baseline-aligned cell's inline content to the bottom of the row.  A
table cell defaults to vertical-align:baseline; with a single line of text in a
cell stretched taller than that line (HN's title cell beside the 19px votearrow),
the browser drops the line to the cell's baseline near the bottom, not the middle.
Shift the content down by the slack so it tucks against the cell bottom; the box
itself is then stretched to ROWH by the caller.

The slack is measured against the cell's actual CONTENT height (its line boxes'
extent), not LBOX-H.  A cell with an explicit height smaller than its content
(HN's nav cell <td style=\"height:10px\"> holding a 15px line) has LBOX-H clamped
to that 10px minimum, so rowh-LBOX-H would over-sink the line by the shortfall —
dropping the nav text below the orange bar.  CSS treats the cell height as a
minimum: the content still occupies its own line height, so that is what the
line must tuck against the cell bottom by."
  (when (and (not (cell-lbox-valign-top-p lb))
             (cell-inline-only-p lb)
             (lbox-children lb))
    (let* ((content-bottom (loop for c in (lbox-children lb)
                                 maximize (+ (lbox-y c) (lbox-h c))))
           (content-h (- content-bottom (lbox-y lb)))
           (slack (- rowh content-h)))
      (when (> slack 0)
        (dolist (c (lbox-children lb)) (shift-box c 0 slack))))))

(defun min-inline-width (node styles cs content-w)
  "Min-content width of NODE's inline content: the widest single unbreakable
token (word or atomic box)."
  (let ((words (collect-words (coerce (h:dnode-children node) 'list) styles cs content-w)) (w 0))
    (dolist (wd words)
      (setf w (max w (if (eq (car wd) :atomic) (lbox-w (tok-meta wd)) (word-w (car wd) (tok-meta wd))))))
    w))

(defun min-content-width (node styles content-w &optional (depth 0))
  "Min-content CONTENT width of element NODE (widest unbreakable run)."
  (with-intrinsic-memo (list :min node (round content-w))
    (let ((cs (st styles node)))
      (cond
        ((or (not (eq (h:dnode-kind node) :element)) (null cs)) 0)
        ;; A table's min-content is the sum of its column MIN widths, measured by
        ;; the (memoised) column model — NOT its subtree flattened onto one inline
        ;; line, which would re-lay-out every nested table and blow up.
        ((table-box-p styles node) (min content-w (table-min-width node styles content-w)))
        (t
         (let ((block-kids (remove-if-not (lambda (k) (or (block-level-p styles k) (float-p styles k)))
                                          (child-elements node))))
           (min content-w
                (if block-kids
                     (loop for k in block-kids
                           maximize (+ (css:cstyle-padding-left (st styles k)) (css:cstyle-padding-right (st styles k))
                                       (used-border (st styles k) :l) (used-border (st styles k) :r)
                                       (min-content-width k styles content-w (1+ depth))))
                     (min-inline-width node styles cs content-w)))))))))

(defun cell-max-content-width (cell styles avail)
  "Max-content border-box width of a table CELL.  An explicit width is the target,
but never below the cell's unshrinkable min-content."
  (let* ((cs (st styles cell)) (w (and cs (css:cstyle-width cs))))
    (max 0 (+ (cell-pad-bord cs)
              (if (numberp w) (max w (min-content-width cell styles avail))
                  (pref-content-width cell styles avail))))))

(defun cell-min-content-width (cell styles avail)
  "Min-content border-box width of a table CELL.  An explicit width is honored as
a floor, but a cell can never be narrower than its unshrinkable content — e.g.
HN's logo cell is width:18px yet holds a 20px (bordered) <img>, so the column
must be 20, not 18 (matching how the browser widens the column to fit it)."
  (let* ((cs (st styles cell)) (w (and cs (css:cstyle-width cs))))
    (max 0 (+ (cell-pad-bord cs)
              (if (numberp w) (max w (min-content-width cell styles avail))
                  (min-content-width cell styles avail))))))

(defun cell-spec-width (cell styles)
  "Specified column width contributed by CELL: NIL, a border-box px number, or
a (:percent P) form."
  (let* ((cs (st styles cell)) (w (and cs (css:cstyle-width cs))))
    (cond ((numberp w) (+ w (cell-pad-bord cs)))
          ((and (consp w) (eq (car w) :percent)) (list :percent (second w)))
          (t nil))))

(defun spec-combine (a b)
  "Combine two column spec widths (prefer a fixed px over a percent, else max px)."
  (cond ((null a) b) ((null b) a)
        ((and (numberp a) (numberp b)) (max a b))
        ((numberp a) a) (t b)))

(defun resolve-spec (sp table-w)
  "Resolve a column spec (px | (:percent P)) to a border-box px width, or NIL."
  (cond ((numberp sp) sp)
        ((and (consp sp) (eq (car sp) :percent)) (* table-w (/ (second sp) 100.0)))
        (t nil)))

(defun table-column-model (node styles avail)
  "Analyse a display:table NODE's cells into per-column requirements.  Returns
 (values MAXS MINS SPECS NCOLS): parallel simple-vectors of border-box
max-content, min-content, and specified-width (NIL | px | (:percent P)) per
column, colspans distributed across the columns they span (CSS 2.1 17.5.2)."
  (with-intrinsic-memo (list :tcm node (round avail))
    (%table-column-model node styles avail)))

(defun %table-column-model (node styles avail)
  "Uncached core of TABLE-COLUMN-MODEL (see it)."
  (let* ((rows (table-rows node styles)) (recs '()) (ncols 0))
    (dolist (row rows)
      (let ((col 0))
        (dolist (cell (row-cells row styles node))
          (let ((span (cell-colspan cell)))
            (push (list cell col span) recs)
            (incf col span)
            (setf ncols (max ncols col))))))
    (setf recs (nreverse recs))
    (when (zerop ncols) (setf ncols 1))
    (let ((maxs (make-array ncols :initial-element 0.0))
          (mins (make-array ncols :initial-element 0.0))
          (specs (make-array ncols :initial-element nil)))
      ;; single-column cells first
      (dolist (r recs)
        (destructuring-bind (cell col span) r
          (when (= span 1)
            (setf (aref maxs col) (max (aref maxs col) (cell-max-content-width cell styles avail)))
            (setf (aref mins col) (max (aref mins col) (cell-min-content-width cell styles avail)))
            (setf (aref specs col) (spec-combine (aref specs col) (cell-spec-width cell styles))))))
      ;; spanning cells: widen the spanned columns if they don't already suffice
      (dolist (r recs)
        (destructuring-bind (cell col span) r
          (when (> span 1)
            (let* ((cols (loop for i from col below (min ncols (+ col span)) collect i))
                   (k (length cols)))
              (when (plusp k)
                (let ((cmax (cell-max-content-width cell styles avail))
                      (cmin (cell-min-content-width cell styles avail))
                      (cur-max (loop for i in cols sum (aref maxs i)))
                      (cur-min (loop for i in cols sum (aref mins i))))
                  (when (> cmax cur-max)
                    (let ((add (/ (- cmax cur-max) k))) (dolist (i cols) (incf (aref maxs i) add))))
                  (when (> cmin cur-min)
                    (let ((add (/ (- cmin cur-min) k))) (dolist (i cols) (incf (aref mins i) add))))))))))
      (values maxs mins specs ncols))))

(defun table-natural-width (node styles avail)
  "Shrink-to-fit CONTENT width of a display:table NODE: the sum of its column
max-content (or fixed) widths.  0 when it has no cells."
  (multiple-value-bind (maxs mins specs ncols) (table-column-model node styles avail)
    (declare (ignore mins))
    (loop for i below ncols
          sum (let ((sp (aref specs i)))
                (if (numberp sp) (max sp (aref maxs i)) (aref maxs i))))))

(defun table-min-width (node styles avail)
  "Min-content CONTENT width of a display:table NODE: the sum of its per-column
min-content (or fixed floor) widths.  0 when it has no cells."
  (multiple-value-bind (maxs mins specs ncols) (table-column-model node styles avail)
    (declare (ignore maxs))
    (loop for i below ncols
          sum (let ((sp (aref specs i)))
                (if (numberp sp) (max sp (aref mins i)) (aref mins i))))))

(defun fit-columns (rspecs maxs mins target ncols)
  "Fit per-column widths to a TARGET total (CSS 2.1 17.5.2 pragmatic).  RSPECS is
a list of resolved px specs (NIL for auto columns); MAXS/MINS are lists of
border-box max/min-content widths.  Auto columns absorb any surplus; a deficit
is taken proportionally from auto columns' shrink room (fixed columns kept)."
  (let ((w (make-array ncols)))
    (loop for i below ncols
          ;; a column is never narrower than its min-content, even a fixed-width
          ;; one: HN's logo cell is width:18 but holds a 20px <img>, so its column
          ;; must be >= 20 (the browser widens it to fit).
          do (setf (aref w i) (max (float (or (nth i rspecs) (nth i maxs) 0))
                                   (float (or (nth i mins) 0)))))
    (let ((sum (loop for i below ncols sum (aref w i))))
      (cond
        ((< (abs (- sum target)) 0.5))
        ((< sum target)                         ; grow: give surplus to auto cols
         (let* ((auto (loop for i below ncols unless (nth i rspecs) collect i))
                (idxs (or auto (loop for i below ncols collect i)))
                (basis (loop for i in idxs sum (max 1.0 (aref w i))))
                (extra (- target sum)))
           (loop for i in idxs
                 do (incf (aref w i) (* extra (/ (max 1.0 (aref w i)) basis))))))
        (t                                       ; shrink: pull from auto shrink room
         (let* ((room (loop for i below ncols
                            collect (if (nth i rspecs) 0.0
                                        (max 0.0 (- (aref w i) (float (or (nth i mins) 0)))))))
                (troom (reduce #'+ room))
                (deficit (- sum target)))
           (if (> troom 0.5)
               (loop for i below ncols
                     do (decf (aref w i) (* deficit (/ (nth i room) troom))))
               (loop for i below ncols
                     do (setf (aref w i) (* (aref w i) (/ target sum)))))))))
    (loop for i below ncols collect (max 1.0 (aref w i)))))

(defun layout-table (node styles cx cy content-w base-cs)
  "Automatic table layout (CSS 2.1 17.5.2): columns sized to their content (or a
specified width), rows stacked, cells stretched to row height.  Returns
 (values cell-lboxes content-height)."
  (let ((rows (table-rows node styles)))
    (when (null rows) (return-from layout-table (values nil 0)))
    (multiple-value-bind (maxs mins specs ncols) (table-column-model node styles content-w)
      (let* ((rspecs (loop for i below ncols collect (resolve-spec (aref specs i) content-w)))
             (colw (fit-columns rspecs
                                (loop for i below ncols collect (aref maxs i))
                                (loop for i below ncols collect (aref mins i))
                                content-w ncols))
             (colx (make-array (1+ ncols) :initial-element 0.0))
             (y cy) (boxes '()))
        (loop for i below ncols do (setf (aref colx (1+ i)) (+ (aref colx i) (nth i colw))))
        (dolist (row rows)
          (let ((cells (row-cells row styles node)) (rowh 0) (rowboxes '()) (col 0))
            (dolist (cell cells)
              (let* ((span (cell-colspan cell))
                     (x0 (aref colx (min ncols col)))
                     (x1 (aref colx (min ncols (+ col span))))
                     (cw (max 1 (round (- x1 x0)))))
                (multiple-value-bind (lb adv) (layout-node cell styles (round (+ cx x0)) y cw)
                  (declare (ignore adv))
                  (when lb (push lb rowboxes) (setf rowh (max rowh (lbox-h lb)))))
                (incf col span)))
            ;; Honor an explicit row height (CSS 2.1 17.5.3): the row is at least
            ;; as tall as its specified height.  This is what materialises HN's
            ;; empty 5px spacer <tr style="height:5px">, whose row-cells are empty
            ;; so its height would otherwise collapse to 0 and pack the stories.
            (let ((rcs (unless (eq row node) (st styles row))))
              (when rcs
                (let ((rh (css::resolve-height (css:cstyle-height rcs) nil)))
                  (when (and (numberp rh) (> rh rowh)) (setf rowh (round rh))))))
            (dolist (lb rowboxes)
              (baseline-sink-cell lb rowh)                   ; baseline cells: sink content to bottom
              (setf (lbox-h lb) rowh))                       ; stretch box to row height
            ;; A row with no cell boxes but a positive height (an empty spacer row)
            ;; still occupies its band; give it a box so it advances the flow and is
            ;; recorded/painted like the browser's tr box.
            (when (and (null rowboxes) (plusp rowh) (not (eq row node)))
              (push (make-lbox :x (round cx) :y y :w (round content-w) :h rowh
                               :style (st styles row) :node row :kind :block)
                    rowboxes))
            (setf boxes (nconc boxes (nreverse rowboxes)))
            (incf y rowh)))
        (values boxes (- y cy))))))

(defun pref-inline-width (node styles cs content-w)
  "Max-content width of NODE's inline content: word + atomic-box widths summed on
a single (unwrapped) line.  Measures NODE's CHILDREN — collecting NODE itself would
re-wrap an inline-block/atomic node as one atomic box laid out at the full available
width (an empty icon then reports content-w instead of ~0)."
  (let ((words (collect-words (coerce (h:dnode-children node) 'list) styles cs content-w))
        (w 0))
    (dolist (wd words)
      (incf w (+ (if (eq (car wd) :atomic) (lbox-w (tok-meta wd)) (word-w (car wd) (tok-meta wd)))
                 (tok-gap wd)
                 (if (tok-space wd) (space-w (if (eq (car wd) :atomic) cs (tok-meta wd))) 0))))
    w))

(defun pref-content-width (node styles content-w &optional (depth 0))
  "Shrink-to-fit preferred (max-content) CONTENT width of element NODE: the
widest of its block/float children's border-box widths, else its inline
max-content width.  Bounded by CONTENT-W so it stays resilient."
  (with-intrinsic-memo (list :pref node (round content-w))
    (let ((cs (st styles node)))
      (cond
        ((or (not (eq (h:dnode-kind node) :element)) (null cs)) 0)
        ;; A table's max-content is its natural (column-model) width — measured by
        ;; the memoised column model, not by flattening the whole subtree (and
        ;; re-laying-out every nested table) onto one inline line.
        ((table-box-p styles node) (min content-w (table-natural-width node styles content-w)))
        ;; A flex ROW's max-content is the SUM of its items' outer widths plus the
        ;; gaps between them (they sit side by side), NOT the max — an inline-block
        ;; heuristic that flattened a vertical menu onto one line and over-sized the
        ;; item badly.  Out-of-flow children contribute nothing.
        ((flex-row-p cs)
         (let ((items (remove-if (lambda (k) (out-of-flow-p styles k)) (child-elements node))))
           (min content-w
                (+ (loop for k in items sum (pref-border-width k styles content-w (1+ depth)))
                   (* (css:cstyle-gap cs) (max 0 (1- (length items))))))))
        (t
         (let ((block-kids (remove-if-not (lambda (k) (and (not (out-of-flow-p styles k))
                                                           (or (block-level-p styles k) (float-p styles k))))
                                          (child-elements node))))
           (min content-w
                (if block-kids
                    (loop for k in block-kids
                          maximize (pref-border-width k styles content-w (1+ depth)))
                    (pref-inline-width node styles cs content-w)))))))))

(defun pref-border-width (node styles content-w depth)
  "Preferred BORDER-box width (incl. margins) of NODE for shrink-to-fit sizing."
  (let* ((cs (st styles node))
         (w (and cs (css:cstyle-width cs))))
    (if (null cs) 0
        (+ (css:cstyle-margin-left cs) (css:cstyle-margin-right cs)
           (used-border cs :l) (used-border cs :r)
           (css:cstyle-padding-left cs) (css:cstyle-padding-right cs)
           (if (numberp w) w (pref-content-width node styles content-w depth))))))

(defun place-float (node styles cleft cright top content-w)
  "Position a floated NODE at the left/right edge within [CLEFT,CRIGHT], dropping
below existing floats if it does not fit.  Records it in *FLOATS*; returns its lbox."
  (let* ((cs (st styles node))
         (side (if (string= (css:cstyle-float cs) "left") :left :right))
         (ml (css:cstyle-margin-left cs)) (mr (css:cstyle-margin-right cs))
         (mb (css:cstyle-margin-bottom cs))
         (extra (+ (css:cstyle-padding-left cs) (css:cstyle-padding-right cs)
                   (used-border cs :l) (used-border cs :r)))
         ;; AVAIL-W is the available content width handed to LAYOUT-NODE; for an
         ;; auto-width float the box fills (avail - margins), so to shrink-wrap
         ;; we size it to the float's preferred content width + its own
         ;; padding/border/margins (CSS 10.3.5 shrink-to-fit), capped at content.
         (avail-w (let ((w (css:cstyle-width cs)))
                    (if (numberp w) (+ w ml mr extra)
                        (min content-w
                             (max 0 (+ (pref-content-width node styles content-w) extra ml mr))))))
         ;; A float with `clear` drops below the relevant existing floats before
         ;; it is placed (CSS 9.5.2): e.g. two `float:right; clear:right` sidebars
         ;; stack vertically rather than sitting side by side.
         (clr (css:cstyle-clear cs))
         (y (if (member clr '("left" "right" "both") :test #'string=)
                (clear-y top cleft cright
                         (cond ((string= clr "left") '(:left))
                               ((string= clr "right") '(:right))
                               (t '(:left :right))))
                top)))
    (setf avail-w (min avail-w content-w))
    ;; drop to a band wide enough for the float
    (loop (multiple-value-bind (lx rx) (float-band y 1 cleft cright)
            (if (>= (- rx lx) avail-w) (return)
                (let ((ny (next-float-bottom y))) (if (and ny (> ny y)) (setf y ny) (return))))))
    (multiple-value-bind (lx rx) (float-band y 1 cleft cright)
      (let ((fx (if (eq side :left) lx (- rx avail-w))))
        (multiple-value-bind (lb adv) (layout-node node styles fx y avail-w)
          (declare (ignore adv))
          (when lb
            ;; A float occupies (and content clears) its MARGIN box, not its
            ;; border box: the border-top edge already carries the float's top
            ;; margin (in LBOX-Y), and the bottom margin edge adds MB.  Honouring
            ;; this lets Acid2's nose float (margin -2em ... -1em) report a higher
            ;; bottom so the cleared smile abuts it instead of leaving a gap.
            (push (list side fx (+ fx avail-w) y (+ (lbox-y lb) (lbox-h lb) mb)) *floats*))
          lb)))))

(defun find-lbox-for-node (lb node)
  "Locate the block lbox whose source NODE is NODE, NIL if none."
  (when (and lb node)
    (if (eq (lbox-node lb) node) lb
        (when (eq (lbox-kind lb) :block)
          (some (lambda (c) (find-lbox-for-node c node)) (lbox-children lb))))))

(defun layout-tree (document styles width &optional viewport-height scroll-to)
  (let ((*floats* nil) (*abs-pending* nil) (*fixed-pending* nil)
        (*intrinsic-cache* (make-hash-table :test 'equal))
        (body (css:query-select document "body")))
    (when body
      ;; The initial containing block has the viewport height when the viewport
      ;; model is active (definite) — so body's percentage height resolves
      ;; against it (CSS 2.1 10.5); otherwise the page height is indefinite.
      (multiple-value-bind (root adv) (layout-node body styles 0 0 width viewport-height)
        (let* ((ph (if root (max 0 (+ (lbox-y root) (lbox-h root))) 0))
               (vph (or viewport-height ph))
               ;; Scroll the viewport to the SCROLL-TO anchor (e.g. Acid2's
               ;; intro links to #top; navigating there scrolls the picture to
               ;; the top of the clipped viewport — that is how the face lands
               ;; at the top-left, per the Acid2 guided tour).
               (anchor (and scroll-to viewport-height
                            (find-lbox-for-node
                             root (css:query-select document (format nil "#~a" scroll-to)))))
               ;; Scroll the anchor to the viewport top.  An overflow:hidden
               ;; viewport navigated to a fragment positions the target at the
               ;; top edge; unlike interactive scrolling it is NOT capped at the
               ;; scrollable-overflow bottom (max 0 (- ph vph)) — Acid2 relies on
               ;; #top reaching the very top so the fixed crown `p` lands on the
               ;; face it pins, even though little content trails below #top.
               (scroll-y (if anchor (max 0 (round (lbox-y anchor))) 0))
               ;; Absolutes with no positioned ancestor resolve against the
               ;; initial containing block (document origin, scrolls with the
               ;; page); fixed boxes resolve against the viewport (offset by the
               ;; current scroll so they stay pinned to the visible rectangle).
               (icb (list 0 0 width ph))
               (vp  (list 0 scroll-y width vph)))
          (dolist (p *abs-pending*)   (resolve-positioned (car p) icb (cdr p)))
          (dolist (p *fixed-pending*) (resolve-positioned (car p) vp (cdr p)))
          ;; Apply the scroll: shift the whole painted tree up so the anchor
          ;; (and fixed boxes, already placed at scroll-y+offset) land in view.
          (when (and root (plusp scroll-y)) (shift-box root 0 (- scroll-y))))
        (values root adv)))))

(defun root-clips-p (doc styles)
  "True when the root box (html, else body) establishes overflow clipping —
the condition under which the canvas becomes a fixed, viewport-sized rectangle
(html{overflow:hidden}, as Acid2 relies on) rather than growing to content."
  (flet ((clips (el)
           (let ((cs (and el (gethash el styles))))
             (and cs (member (css:cstyle-overflow cs) '("hidden" "clip" "scroll")
                             :test #'string=)))))
    (or (clips (css:query-select doc "html"))
        (clips (css:query-select doc "body")))))

;;; ---- paint --------------------------------------------------------------
(defun rgb (color) (list (first color) (second color) (third color)))

(defun lbox-positioned-p (lb)
  "True when LB is a positioned (relative/absolute/fixed) block box — one that
forms its own stacking level above in-flow siblings."
  (let ((cs (and (eq (lbox-kind lb) :block) (lbox-style lb))))
    (and cs (member (css:cstyle-position cs) '("relative" "absolute" "fixed") :test #'string=))))

(defun lbox-z (lb)
  (let ((cs (lbox-style lb))) (if cs (or (css:cstyle-z-index cs) 0) 0)))

(defun lbox-float-p (lb)
  "True when LB is a floated box (CSS float:left|right)."
  (let ((cs (and (eq (lbox-kind lb) :block) (lbox-style lb))))
    (and cs (member (css:cstyle-float cs) '("left" "right") :test #'string=))))

(defun lbox-inline-level-p (lb)
  "True when LB paints in the inline-level phase: a line box, or a block-kind box
whose display is an inline/inline-block kind (an atomic inline)."
  (or (eq (lbox-kind lb) :line)
      (let ((cs (lbox-style lb)))
        (and cs (member (css:cstyle-display cs)
                        '("inline" "inline-block" "inline-table" "inline-flex")
                        :test #'string=)))))

(defun lbox-img-anywhere-p (lb)
  "True when LB or any descendant carries a replaced image (the <object> image
box now nests its image in an inner child to inset it under its padding)."
  (and (lbox-p lb)
       (or (lbox-img lb)
           (some #'lbox-img-anywhere-p (lbox-children lb)))))

(defun block-has-replaced-inline-p (lb)
  "True when LB is a background-less block whose direct content is a line box
carrying a replaced atomic inline (an <img>/<object> image box).  Per CSS
appendix E such inline content paints in the inline phase — above sibling
floats — so the block must be ordered with the inlines, not the blocks (Acid2's
#eyes-a object must paint over the #eyes-b float to fuse the eye tiles)."
  (and (eq (lbox-kind lb) :block)
       (let ((cs (lbox-style lb)))
         (and cs (not (css:cstyle-background cs)) (not (css:cstyle-bg-image cs))))
       (some (lambda (c)
               (and (lbox-p c) (eq (lbox-kind c) :line)
                    (some (lambda (it) (and (lbox-p it) (lbox-img-anywhere-p it)))
                          (lbox-children c))))
             (lbox-children lb))))

(defun paint-children (cv children)
  "Paint CHILDREN in a simplified CSS stacking order (appendix E): negative
z-index positioned boxes; then, among IN-FLOW content, block-level boxes, then
floats, then inline-level boxes (each phase in tree order); then >=0 positioned
boxes by z-index.  The block<float<inline split is what lets a float's background
cover an earlier block sibling's background (Acid2's eyes: the float's yellow
must paint over #eyes-c's red) and keeps inline content on top of floats.  Equal
z-index keeps tree order (stable)."
  (let ((flow '()) (pos '()))
    (dolist (c children) (if (lbox-positioned-p c) (push c pos) (push c flow)))
    (setf flow (nreverse flow) pos (nreverse pos))
    (let ((blocks '()) (floats '()) (inlines '()))
      (dolist (c flow)
        (cond ((lbox-float-p c)        (push c floats))
              ((lbox-inline-level-p c) (push c inlines))
              ((block-has-replaced-inline-p c) (push c inlines))
              (t                       (push c blocks))))
      (let ((neg    (stable-sort (remove-if-not (lambda (c) (minusp (lbox-z c))) (copy-list pos))
                                 #'< :key #'lbox-z))
            (nonneg (stable-sort (remove-if     (lambda (c) (minusp (lbox-z c))) (copy-list pos))
                                 #'< :key #'lbox-z)))
        (dolist (c neg)              (paint-box cv c))
        (dolist (c (nreverse blocks))  (paint-box cv c))
        (dolist (c (nreverse floats))  (paint-box cv c))
        (dolist (c (nreverse inlines)) (paint-box cv c))
        (dolist (c nonneg)          (paint-box cv c))))))

(defun marker-glyph (kind) (cond ((string= kind "circle") "o") ((string= kind "square") "#")
                                 ((string= kind "none") "") (t "•")))

(defun bg-pos-offset (comp avail)
  "Resolve one background-position component COMP = (value unit) to a px offset
within AVAIL (= box-dim - image-dim).  Honors px and % (and 0); other units 0."
  (if (and (consp comp) (>= (length comp) 2))
      (let ((val (first comp)) (unit (second comp)))
        (cond ((string= unit "%") (round (* (/ val 100.0) (max 0 avail))))
              ((or (string= unit "px") (string= unit "")) (round val))
              (t 0)))
      0))

(defun paint-bg-image (cv lb cs url)
  "Decode the data: URI URL and tile it across LB's padding box honoring
background-repeat and a simple background-position.  Best-effort: an undecodable
image paints nothing (the bg color shows through)."
  (let* ((duri (if (find #\% url) (percent-decode url) url))
         (img (and (>= (length duri) 5) (string-equal (subseq duri 0 5) "data:")
                   (ignore-errors (decode-image duri)))))
    (when (and img (plusp (img-w img)) (plusp (img-h img)))
      (let* ((iw (img-w img)) (ih (img-h img))
             (rep (css:cstyle-bg-repeat cs))
             (repx (member rep '("repeat" "repeat-x") :test #'string=))
             (repy (member rep '("repeat" "repeat-y") :test #'string=))
             ;; padding box (inside the borders)
             (px0 (round (+ (lbox-x lb) (used-border cs :l))))
             (py0 (round (+ (lbox-y lb) (used-border cs :t))))
             (px1 (round (- (+ (lbox-x lb) (lbox-w lb)) (used-border cs :r))))
             (py1 (round (- (+ (lbox-y lb) (lbox-h lb)) (used-border cs :b))))
             (pos (css:cstyle-bg-position cs))
             (offx (if pos (bg-pos-offset (first pos) (- (- px1 px0) iw)) 0))
             (offy (if pos (bg-pos-offset (second pos) (- (- py1 py0) ih)) 0))
             (ox (+ px0 offx)) (oy (+ py0 offy)))
        (when (and (> px1 px0) (> py1 py0))
          ;; clip tiles to the padding box so they never bleed out
          (let ((*clip* (clip-intersect px0 py0 px1 py1)))
            ;; common case: a 1x1 image filling the box — paint a solid rect.
            (if (and (= iw 1) (= ih 1) (>= (aref (img-rgba img) 3) 255))
                (let ((r (aref (img-rgba img) 0)) (g (aref (img-rgba img) 1)) (b (aref (img-rgba img) 2)))
                  (fill-rect cv (if repx px0 ox) (if repy py0 oy)
                             (if repx (- px1 px0) 1) (if repy (- py1 py0) 1) (list r g b)))
                ;; general tiling
                (let ((startx (if repx (- ox (* iw (ceiling (- ox px0) iw))) ox))
                      (starty (if repy (- oy (* ih (ceiling (- oy py0) ih))) oy)))
                  (loop for ty = starty then (+ ty ih)
                        while (and (< ty py1) (or repy (= ty starty))) do
                    (loop for tx = startx then (+ tx iw)
                          while (and (< tx px1) (or repx (= tx startx))) do
                      (when (and (> (+ tx iw) px0) (> (+ ty ih) py0))
                        (blit-img cv img tx ty))))))))))))

(defun paint-bg-image-fixed (cv lb cs url)
  "Tile URL's image as a background-attachment:fixed background: the tile grid is
anchored to the VIEWPORT origin (canvas 0,0) plus the background-position offset,
NOT to LB's box — so overlapping fixed-bg elements share one continuous tiling
(this is what fuses Acid2's two offset 2x2 images into a solid yellow fill).  The
painting is clipped to LB's border box (default background-clip)."
  (let* ((duri (if (find #\% url) (percent-decode url) url))
         (img (and (>= (length duri) 5) (string-equal (subseq duri 0 5) "data:")
                   (ignore-errors (decode-image duri)))))
    (when (and img (plusp (img-w img)) (plusp (img-h img)))
      (let* ((iw (img-w img)) (ih (img-h img))
             (rep (css:cstyle-bg-repeat cs))
             (repx (member rep '("repeat" "repeat-x") :test #'string=))
             (repy (member rep '("repeat" "repeat-y") :test #'string=))
             ;; border box of the element (the clip region)
             (bx0 (round (lbox-x lb))) (by0 (round (lbox-y lb)))
             (bx1 (round (+ (lbox-x lb) (lbox-w lb))))
             (by1 (round (+ (lbox-y lb) (lbox-h lb))))
             ;; tile origin: anchored to the viewport (canvas 0,0) plus the
             ;; background-position offset, computed against the viewport (canvas).
             (pos (css:cstyle-bg-position cs))
             (offx (if pos (bg-pos-offset (first pos)  (- (canvas-width cv) iw)) 0))
             (offy (if pos (bg-pos-offset (second pos) (- (canvas-height cv) ih)) 0)))
        (when (and (> bx1 bx0) (> by1 by0))
          (let ((*clip* (clip-intersect bx0 by0 bx1 by1)))
            ;; walk the viewport-aligned tile grid over LB's border box
            (let ((startx (if repx (+ offx (* iw (floor (- bx0 offx) iw))) offx))
                  (starty (if repy (+ offy (* ih (floor (- by0 offy) ih))) offy)))
              (loop for ty = starty then (+ ty ih)
                    while (and (< ty by1) (or repy (= ty starty))) do
                (loop for tx = startx then (+ tx iw)
                      while (and (< tx bx1) (or repx (= tx startx))) do
                  (when (and (> (+ tx iw) bx0) (> (+ ty ih) by0))
                    (blit-img cv img tx ty)))))))))))

(defun border-edge-raw-color (cs edge)
  "The stored (r g b a) color for EDGE, falling back to BORDER-COLOR, or NIL."
  (or (case edge
        (:t (css:cstyle-border-top-color cs)) (:r (css:cstyle-border-right-color cs))
        (:b (css:cstyle-border-bottom-color cs)) (:l (css:cstyle-border-left-color cs)))
      (css:cstyle-border-color cs)))

(defun border-edge-color (cs edge)
  "RGB list for EDGE (:t :r :b :l): the per-edge color, falling back to BORDER-COLOR."
  (rgb (border-edge-raw-color cs edge)))

(defun border-edge-width (cs edge)
  "Effective border width for EDGE: the declared width, or 0 when the edge's
border-style is none/hidden, or when its color is fully transparent (alpha 0,
e.g. Acid2's smile spans whose left/right borders are `solid transparent`)."
  (multiple-value-bind (w sty)
      (case edge
        (:t (values (css:cstyle-border-top-width cs) (css:cstyle-border-top-style cs)))
        (:r (values (css:cstyle-border-right-width cs) (css:cstyle-border-right-style cs)))
        (:b (values (css:cstyle-border-bottom-width cs) (css:cstyle-border-bottom-style cs)))
        (:l (values (css:cstyle-border-left-width cs) (css:cstyle-border-left-style cs))))
    (let ((col (border-edge-raw-color cs edge)))
      (if (and (css:border-edge-painted-p sty)
               (not (and col (fourth col) (<= (fourth col) 0))))
          w 0.0))))

(defun paint-borders (cv lb cs)
  "Paint the four border edges, each with its own color.  Edges whose
border-style is none/hidden are suppressed (zero effective width).

When all painted edges share one color, the edges are drawn as overlapping
rectangles — pixel-identical to weft's original border painting.  When edge
colors differ, each edge is drawn as a mitered trapezoid whose outer side is
the full box edge and whose inner side is inset by the adjacent edges' widths,
so adjacent borders meet on the 45-degree corner diagonal.  For a small/0-size
box with thick borders this yields the classic triangles (e.g. CSS triangles)."
  (let* ((bt (border-edge-width cs :t)) (br (border-edge-width cs :r))
         (bb (border-edge-width cs :b)) (bl (border-edge-width cs :l))
         (x0 (lbox-x lb)) (y0 (lbox-y lb)) (w (lbox-w lb)) (h (lbox-h lb))
         (x1 (+ x0 w)) (y1 (+ y0 h))
         (ct (border-edge-color cs :t)) (crr (border-edge-color cs :r))
         (cb (border-edge-color cs :b)) (cl (border-edge-color cs :l)))
    (when (and (<= bt 0) (<= br 0) (<= bb 0) (<= bl 0)) (return-from paint-borders))
    (if (and (equal ct crr) (equal ct cb) (equal ct cl))
        ;; uniform color: overlapping rectangles, exactly as the original code.
        (progn
          (when (plusp bt) (fill-rect cv x0 y0 w bt ct))
          (when (plusp bb) (fill-rect cv x0 (- y1 bb) w bb cb))
          (when (plusp bl) (fill-rect cv x0 y0 bl h cl))
          (when (plusp br) (fill-rect cv (- x1 br) y0 br h crr)))
        ;; differing colors: mitered trapezoids (degenerate to triangles).
        (let ((ix0 (+ x0 bl)) (iy0 (+ y0 bt)) (ix1 (- x1 br)) (iy1 (- y1 bb)))
          (when (plusp bt) (fill-poly cv (list (cons x0 y0) (cons x1 y0) (cons ix1 iy0) (cons ix0 iy0)) ct))
          (when (plusp bb) (fill-poly cv (list (cons x0 y1) (cons ix0 iy1) (cons ix1 iy1) (cons x1 y1)) cb))
          (when (plusp bl) (fill-poly cv (list (cons x0 y0) (cons ix0 iy0) (cons ix0 iy1) (cons x0 y1)) cl))
          (when (plusp br) (fill-poly cv (list (cons x1 y0) (cons x1 y1) (cons ix1 iy1) (cons ix1 iy0)) crr))))))

(defun box-visible-p (cs)
  "NIL when CS is visibility:hidden/collapse — such a box paints nothing of its
   own (background, border, image, text) but keeps its layout space, and visible
   descendants (visibility:visible) still paint (visibility is inherited)."
  (not (and cs (member (css:cstyle-visibility cs) '("hidden" "collapse") :test #'string=))))

(defun paint-box (cv lb)
  (handler-case (%paint-box cv lb) (error () nil)))
(defun %paint-box (cv lb)
  (when lb
    (case (lbox-kind lb)
      (:block
       (let ((cs (lbox-style lb)))
        (when (box-visible-p cs)
         (cond ((css:cstyle-bg-gradient cs)
                (destructuring-bind (dir from to) (css:cstyle-bg-gradient cs)
                  (fill-gradient cv (lbox-x lb) (lbox-y lb) (lbox-w lb) (lbox-h lb) dir (rgb from) (rgb to))))
               ((css:cstyle-background cs)
                (fill-rect cv (lbox-x lb) (lbox-y lb) (lbox-w lb) (lbox-h lb) (rgb (css:cstyle-background cs)))))
         ;; CSS background image: over the bg color, under the borders, tiled and
         ;; clipped to this box's padding box.  Fixed-attachment images are not
         ;; painted (out of scope) — for Acid2 that is the correct result (their
         ;; images are positioned to the viewport, off this element).
         (when (css:cstyle-bg-image cs)
           (if (string-equal (css:cstyle-bg-attachment cs) "fixed")
               (paint-bg-image-fixed cv lb cs (css:cstyle-bg-image cs))
               (paint-bg-image cv lb cs (css:cstyle-bg-image cs))))
         (when (lbox-img lb)
           (blit-img cv (lbox-img lb) (round (lbox-x lb)) (round (lbox-y lb))
                     (round (lbox-w lb)) (round (lbox-h lb))))
         ;; Replaced vector content (inline <svg>, <canvas>): composite over the
         ;; box's background, under its borders.
         (when (lbox-vpaint lb)
           (funcall (lbox-vpaint lb) cv (round (lbox-x lb)) (round (lbox-y lb))
                    (round (lbox-w lb)) (round (lbox-h lb))))
         (paint-borders cv lb cs)
         (when (and (lbox-marker lb) (plusp (length (marker-glyph (lbox-marker lb)))))
           ;; the list marker (•, disc/circle/square) is painted via scribe so the
           ;; real bullet glyph renders (the 7x13 bitmap has none); it sits ~1.3em
           ;; left of the content, in the list's padding.
           (let ((fs (css:cstyle-font-size cs)))
             (draw-text-scribe cv (marker-glyph (lbox-marker lb))
                               (round (- (+ (lbox-x lb) (css:cstyle-padding-left cs)) (* 1.3 fs)))
                               (round (+ (lbox-y lb) (css:cstyle-padding-top cs)))
                               (round (used-line-height cs))
                               (rgb (css:cstyle-color cs)) fs))))
         ;; overflow:hidden/clip/scroll clips descendants to this box's padding box.
         (if (member (css:cstyle-overflow cs) '("hidden" "clip" "scroll") :test #'string=)
             (let ((*clip* (clip-intersect
                            (round (+ (lbox-x lb) (used-border cs :l)))
                            (round (+ (lbox-y lb) (used-border cs :t)))
                            (round (- (+ (lbox-x lb) (lbox-w lb)) (used-border cs :r)))
                            (round (- (+ (lbox-y lb) (lbox-h lb)) (used-border cs :b))))))
               (paint-children cv (lbox-children lb)))
             (paint-children cv (lbox-children lb)))))
      (:line
       (dolist (it (lbox-children lb))
         (if (frag-p it)
             (let ((cs (frag-style it)))
               ;; pass the line box geometry so scribe centers the real font
               ;; em-box (ascent+descent at font-size) within it.  A
               ;; visibility:hidden run occupies its space but paints no glyphs.
               (when (box-visible-p cs)
                 (draw-text-scribe cv (frag-text it) (round (frag-x it))
                          (lbox-y lb) (lbox-h lb)
                          (rgb (css:cstyle-color cs))
                          (css:cstyle-font-size cs)
                          :face (style-face cs)
                          :bold (>= (css:cstyle-font-weight cs) 600)
                          :letter-spacing (css:cstyle-letter-spacing cs)
                          :underline (member "underline" (css:cstyle-text-decoration cs) :test #'string=))))
             (paint-box cv it)))))))   ; atomic inline-block / img box

(defun percent-decode (s)
  "Decode %XX escapes in a URI component (leaves '+' literal — data: URIs are not
form-encoded)."
  (with-output-to-string (o)
    (let ((i 0) (n (length s)))
      (loop while (< i n) do
        (let ((c (char s i)))
          (if (and (char= c #\%) (< (+ i 2) n))
              (let ((hex (ignore-errors (parse-integer s :start (1+ i) :end (+ i 3) :radix 16))))
                (if hex (progn (write-char (code-char hex) o) (incf i 3))
                    (progn (write-char c o) (incf i))))
              (progn (write-char c o) (incf i))))))))

(defun link-stylesheet-css (href)
  "CSS text from a data: URI HREF of a <link rel=stylesheet>, else NIL.  Only
data: URIs are honoured (no network); base64 and percent-encoded payloads both."
  (when (and href (>= (length href) 5) (string-equal (subseq href 0 5) "data:"))
    (let ((comma (position #\, href)))
      (when comma
        (let ((meta (subseq href 5 comma)) (payload (subseq href (1+ comma))))
          (if (search ";base64" meta :test #'char-equal)
              (handler-case (map 'string #'code-char (base64-decode payload)) (error () nil))
              (percent-decode payload)))))))

(defun link-rel-stylesheet-p (n)
  (let ((rel (cdr (assoc "rel" (h:dnode-attrs n) :test #'string-equal))))
    (and (string-equal (h:dnode-name n) "link") rel
         (member "stylesheet" (css::split-ws (string-downcase rel)) :test #'string=))))

(defun collect-stylesheets (doc)
  "Concatenate author CSS in document order: <style> text and the CSS of
<link rel=stylesheet> data: URIs (so the later source order of an appendix sheet
wins cascade ties, as a browser would)."
  (with-output-to-string (o)
    (labels ((rec (n)
               (when (eq (h:dnode-kind n) :element)
                 (cond
                   ((string= (h:dnode-name n) "style")
                    (loop for c across (h:dnode-children n) when (eq (h:dnode-kind c) :text)
                          do (write-string (h:dnode-data c) o) (terpri o)))
                   ((link-rel-stylesheet-p n)
                    (let ((css (link-stylesheet-css
                                (cdr (assoc "href" (h:dnode-attrs n) :test #'string-equal)))))
                      (when css (write-string css o) (terpri o)))))
                 (loop for c across (h:dnode-children n) do (rec c)))))
      (loop for c across (h:dnode-children doc) do (rec c)))))

(defun render-to-canvas (html css width &key (min-height 200) (max-height 20000)
                                              (viewport-height 600) scroll-to before-layout)
  "Parse HTML, gather CSS, cascade, lay out at WIDTH px, paint.  Returns the
CANVAS.

Two height models:
  * Reader view (default): when the root does NOT clip overflow, the canvas
    grows to content height (clamped to MAX-HEIGHT) — full-page rendering.
  * Viewport model: when the root establishes overflow clipping
    (html/body {overflow:hidden|clip|scroll}), the canvas is a FIXED
    VIEWPORT-HEIGHT rectangle and all painting is clipped to it.  This is how
    a real browser tames Acid2's giant margins and lands fixed boxes at fixed
    viewport coordinates."
  (let* ((css::*viewport-w* (float width))                 ; vw / vmin / vmax basis
         (css::*viewport-h* (float (or viewport-height 600))) ; vh basis
         ;; Fresh per render: <canvas> buffers registered by the scripting seam
         ;; during BEFORE-LAYOUT are read back at paint; never leak across pages.
         (*element-canvas* (make-hash-table :test 'eq))
         (doc (h:parse-html html))
         ;; Optional pre-layout hook: a consumer (weft/script) runs inline
         ;; <script> against the freshly parsed DOM here, so the cascade and
         ;; layout below see the script-mutated tree.  Nil => byte-identical
         ;; to the scriptless render path.
         (pre-hook (when before-layout (funcall before-layout doc)))
         (sheet (css:parse-stylesheet (concatenate 'string (or css "") (string #\Newline)
                                                   (collect-stylesheets doc))))
         (styles (css:compute-styles doc sheet))
         (viewport-p (and viewport-height (root-clips-p doc styles)))
         (vph (and viewport-p (round viewport-height))))
    (declare (ignore pre-hook))
    (multiple-value-bind (root adv) (layout-tree doc styles width vph
                                                 (and viewport-p scroll-to))
      (declare (ignore adv))
      (let* ((content-h (if root (round (+ (lbox-y root) (lbox-h root) 8)) min-height))
             (height (if vph vph (min max-height (max min-height content-h))))
             (body (css:query-select doc "body"))
             (bg (let ((cs (and body (gethash body styles)))) (and cs (css:cstyle-background cs))))
             (cv (make-canvas width height (if bg (rgb bg) '(255 255 255)))))
        (if vph
            (let ((*clip* (clip-intersect 0 0 width vph)))
              (paint-box cv root))
            (paint-box cv root))
        cv))))

(defun canvas-ink (cv)
  "Fraction of pixels that differ from the top-left (background) color — a coarse
\"how much got painted\" signal for tracking rendering progress."
  (let* ((px (canvas-pixels cv)) (n (length px))
         (br (aref px 0)) (bg (aref px 1)) (bb (aref px 2)) (ink 0) (total (floor n 3)))
    (loop for i from 0 below n by 3
          unless (and (= (aref px i) br) (= (aref px (+ i 1)) bg) (= (aref px (+ i 2)) bb))
            do (incf ink))
    (if (plusp total) (/ ink (float total)) 0.0)))

(defun render-to-png (html css width path &key (min-height 200))
  "Render HTML+CSS at WIDTH px and save a PNG.  Returns (values path width height)."
  (let ((cv (render-to-canvas html css width :min-height min-height)))
    (write-png cv path)
    (values path width (canvas-height cv))))
