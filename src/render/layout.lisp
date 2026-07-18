;;;; src/render/layout.lisp — block + inline-formatting layout, paint, render.
;;;;
;;;; Normal-flow layout: block-level boxes stacked vertically (margin/border/
;;;; padding), with INLINE formatting contexts that lay styled text runs into
;;;; line boxes — each fragment keeps its own color/weight/decoration, so bold,
;;;; links, and colored spans render correctly.  Mixed block+inline children are
;;;; grouped into anonymous inline runs.  List items get markers.  Painted to a
;;;; canvas and saved as PNG.
(in-package #:weft.render)

(defstruct lbox x y w h style node kind children marker img vpaint baseline)   ; kind :block | :line; img = decoded IMG; vpaint = (cv x y w h) replaced-content painter; baseline = px offset from a :line box's top to its shared baseline (CSS 2.1 10.8)
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
(defun pseudo-out-of-flow-p (cs)
  "True when a ::before/::after computed style CS is absolutely/fixed positioned,
   so its generated box is removed from the enclosing inline run."
  (and cs (member (css:cstyle-position cs) '("absolute" "fixed") :test #'string=)))
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

(defvar *multicol-measuring* nil
  "Bound true while re-laying a multi-column element's content in a single column
to measure it, so the multicol dispatch in %LAYOUT-CORE does not recurse.")
(defun multicol-p (cs)
  "True when a box with computed style CS is a multi-column container: a block-level
box carrying a used column-count or column-width (css3-multicol §3), so its in-flow
content is fragmented into columns."
  (and cs
       (member (cdisplay cs) '("block" "flow-root" "list-item" "inline-block") :test #'string=)
       (or (and (css:cstyle-column-count cs) (plusp (css:cstyle-column-count cs)))
           (and (css:cstyle-column-width cs) (plusp (css:cstyle-column-width cs))))))
(defun multicol-l2-p (cs)
  "True when CS uses a Multicol Level 2 feature (column-height / column-wrap) that
the L1 column flow does not implement — so it should decline to fragment."
  (and cs (or (css:cstyle-column-height cs) (css:cstyle-column-wrap cs))))
(defun multicol-ancestor-p (node styles)
  "True when an ancestor element of NODE is itself a multi-column container — a
nested multicol the L1 column flow does not fragment across, so the inner one
declines to fragment too."
  (loop for p = (h:dnode-parent node) then (h:dnode-parent p)
        while (and p (eq (h:dnode-kind p) :element))
        thereis (multicol-p (st styles p))))
(defun multicol-unsupported-descendant-p (node styles)
  "True when NODE's subtree holds a construct the L1 column flow cannot place — a
column spanner (column-span:all) or a nested multi-column container — so it declines
to fragment and leaves the content in one column rather than mis-placing it."
  (labels ((scan (n)
             (some (lambda (c)
                     (and (eq (h:dnode-kind c) :element)
                          (let ((cs (st styles c)))
                            (and cs (or (string= (css:cstyle-column-span cs) "all")
                                        (multicol-p cs)
                                        (scan c))))))
                   (h:dnode-children n))))
    (scan node)))

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
  (let ((words '()) (pend nil) (pend-px 0)        ; whitespace + inline-margin px before next token
        (floats '()) (blocks '()))                ; float / block-level descendants hoisted out
    (labels ((emit1 (payload meta node)
               (push (list payload meta pend pend-px node) words) (setf pend nil pend-px 0))
             (amargin (cs side)                      ; atomic's OWN horizontal margin px (may be negative)
               (let ((v (if (eq side :left) (css:cstyle-margin-left cs) (css:cstyle-margin-right cs))))
                 (if (numberp v) v 0)))
             (atom! (lb node)
               ;; an atomic inline (inline-block / replaced) advances by its own
               ;; horizontal margins too — margin-left leads it, margin-right trails
               ;; into the next token's gap (CSS 2.1 §10.3.9 / §9.4.2).
               (when lb
                 (let ((cs (lbox-style lb)))
                   (when cs (incf pend-px (amargin cs :left)))
                   (emit1 :atomic lb node)
                   (when cs (setf pend-px (amargin cs :right))))))
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
                      ;; A floated descendant is out of flow (CSS 2.1 §9.5): hoist it
                      ;; so the enclosing block places it as a float and the inline
                      ;; content flows around it, rather than laying it out inline
                      ;; (which would give an empty run a phantom strut-height line).
                      ((and cs (member (css:cstyle-float cs) '("left" "right") :test #'string=))
                       (push n floats))
                      ((member (h:dnode-name n) '("script" "style") :test #'string=))
                      ;; <br> is a forced line break (HTML §4.5.27): it ends the
                      ;; current line box and starts a new one, emitted as the same
                      ;; :break token a preserved newline produces.  (Out-of-flow /
                      ;; floated / display:none <br>s are handled by the branches
                      ;; above and never reach here.)
                      ((string= (h:dnode-name n) "br")
                       (push (list :break nil nil 0 n) words))
                      ;; Replaced elements (img, decodable <object>) render at their
                      ;; OWN intrinsic/attr size — checked BEFORE inline-block, because
                      ;; <img> is UA display:inline-block and must not fall into generic
                      ;; block layout, which collapses it to a ~0-height content box.
                      ((string= (h:dnode-name n) "img")
                       (let ((lb (img-box n cs content-w))) (atom! lb n)))
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
                      ;; <iframe>: replaced by the nested document, never its fallback
                      ;; children — an empty box, not the "FAIL" text between its tags.
                      ((string= (h:dnode-name n) "iframe")
                       (atom! (replaced-box n cs content-w) n))
                      ((and cs (string= (cdisplay cs) "inline-block"))
                       (multiple-value-bind (lb adv) (layout-node n styles 0 0 content-w)
                         (declare (ignore adv))
                         (atom! lb n)))
                      ;; A block-level flex/table/grid inside an inline run is not an
                      ;; atomic inline: it breaks the run and lays out as a block
                      ;; (block-in-inline, CSS 2.1 §9.2.1.1).  Hoisted for the enclosing
                      ;; block to place, so a run of only such boxes leaves no line box.
                      ((and cs (member (cdisplay cs) '("flex" "table" "grid") :test #'string=))
                       (push n blocks))
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
                       ;; ...but an out-of-flow (absolute/fixed) pseudo is removed
                       ;; from the inline run, just like an out-of-flow real child
                       ;; (above): it is not part of the line's content flow.
                       (let ((bcs (gethash (cons n :before) styles)))
                         (when (and bcs (css:cstyle-content bcs) (not (pseudo-out-of-flow-p bcs)))
                           (emit-text (css:cstyle-content bcs) bcs n)))
                       (loop for c across (h:dnode-children n) do (rec c cs n))
                       (let ((acs (gethash (cons n :after) styles)))
                         (when (and acs (css:cstyle-content acs) (not (pseudo-out-of-flow-p acs)))
                           (emit-text (css:cstyle-content acs) acs n)))
                       (incf pend-px (iedge cs :right)))))))))
      (dolist (n nodes) (rec n (or (st styles n) default-style) n)))
    ;; extra values: floated and block-level descendants hoisted out of the run, in
    ;; document order, for the caller to place (FLUSH-INLINE).  Single-value callers
    ;; (intrinsic width) ignore them.
    (values (nreverse words) (nreverse floats) (nreverse blocks))))

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

(defun %srcset-url (srcset)
  "The first candidate URL in a SRCSET value (the URL up to its width/density
   descriptor)."
  (when srcset
    (let* ((s (string-left-trim '(#\Space #\Tab #\Newline #\,) srcset))
           (end (position-if (lambda (c) (member c '(#\Space #\Tab #\Newline #\,))) s))
           (u (subseq s 0 (or end (length s)))))
      (and (plusp (length u)) u))))

(defun img-source-url (node)
  "The image URL for an <img>: src, else the first srcset candidate, else the
   lazy-load data-src / data-srcset — many sites defer the real URL into a data-
   attribute until a script swaps it into src on scroll, which a static render never
   triggers."
  (flet ((a (name) (let ((v (cdr (assoc name (h:dnode-attrs node) :test #'string-equal))))
                     (and v (plusp (length (string-trim '(#\Space) v))) v))))
    (or (a "src") (%srcset-url (a "srcset")) (a "data-src") (%srcset-url (a "data-srcset")))))

(defun replaced-ratio (cs iw ih)
  "The width/height ratio a replaced element sizes by: its explicit CSS aspect-ratio
if set (CSS Sizing 4), else the intrinsic ratio IW/IH, else NIL."
  (or (css:cstyle-aspect-ratio cs)
      (and iw ih (plusp iw) (plusp ih) (/ (float iw 1d0) ih))))

(defun replaced-size (cs cw chh iw ih)
  "Concrete (values W H) of a replaced element from its definite CSS width CW /
height CHH (px, or NIL=auto) and intrinsic IW×IH: a definite dimension derives the
other through the aspect-ratio (explicit, else intrinsic); both auto -> intrinsic."
  (let ((ratio (replaced-ratio cs iw ih)))
    (cond ((and cw chh) (values cw chh))
          ((and cw ratio) (values cw (/ cw ratio)))
          ((and chh ratio) (values (* chh ratio) chh))
          (cw (values cw (or ih chh 0)))
          (chh (values (or iw cw 0) chh))
          (t (values (or iw 0) (or ih 0))))))

(defun img-box (node cs &optional avail-w avail-h)
  "An atomic replaced box for <img>, sized to its BORDER box: the layout footprint
is the content WxH plus its border (and padding) widths, with the decoded image (or
an alt-text placeholder) painted in the content area inset by that border/padding.
Modelled on OBJECT-BOX so a bordered <img> (e.g. HN's `border:1px white` logo)
reserves the browser's real box (20x20 for an 18x18 + 1px border) instead of just
its content.  The decoded/CSS/HTML intrinsic size is the CONTENT size; else an
alt-text placeholder."
  (let* ((src (img-source-url node))
         (decoded (cond
                    ((null src) nil)
                    ((and (>= (length src) 5) (string-equal (subseq src 0 5) "data:"))
                     (ignore-errors (decode-image src)))
                    ;; a network <img src> — fetched, decoded and cached through
                    ;; *IMAGE-LOADER* (NIL when running offline: stays a placeholder).
                    (t (fetch-image src))))
         (cw (let ((sw (css::resolve-size (css:cstyle-width cs) (or avail-w 300)))) (and (numberp sw) sw)))
         ;; A percentage height resolves against the containing-block height AVAIL-H
         ;; when definite (CSS 2.1 10.5) — e.g. a full-cover hero `height:100%` in a
         ;; fixed-height positioned parent.  When AVAIL-H is indefinite the legacy
         ;; 200px basis is kept (unchanged behaviour); only the definite case is new.
         (chh (let ((sh (if (numberp avail-h)
                            (css::resolve-height (css:cstyle-height cs) avail-h)
                            (css::resolve-size (css:cstyle-height cs) 200))))
                (and (numberp sh) sh)))
         ;; intrinsic dimensions: the width/height attributes, else the decoded image
         (iw (or (img-attr-num node "width") (and decoded (img-w decoded))))
         (ih (or (img-attr-num node "height") (and decoded (img-h decoded))))
         ;; When only one of width/height is given, derive the other from the intrinsic
         ;; aspect ratio (CSS 2.1 10.3.2 replaced-element sizing) rather than taking the
         ;; raw intrinsic value — so width:100% on a 3824x2640 image stays wide, not tall.
         (w0 (cond (cw cw)
                   ((and chh iw ih (plusp ih)) (* chh (/ iw ih)))
                   (iw iw) (t 120)))
         (hh0 (cond (chh chh)
                    ((and cw iw ih (plusp iw)) (* cw (/ ih iw)))
                    (ih ih) (t 90)))
         ;; max-width clamps the used width — `max-width:100%` is the near-universal
         ;; responsive-image rule, without which a large intrinsic image overflows its
         ;; container (a 1999px photo in a 760px column).  Scale the height with it so
         ;; the aspect ratio is preserved.
         (maxw (let ((mw (css:cstyle-max-width cs)))
                 (unless (eq mw :none) (css::resolve-size mw (or avail-w 300)))))
         (clamp (and (numberp maxw) (plusp w0) (> w0 maxw)))
         (w (if clamp maxw w0))
         (hh (if clamp (* hh0 (/ maxw w0)) hh0))
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
         (pl (max 0 (css::resolve-pad (css:cstyle-padding-left style) nil))) (pr (max 0 (css::resolve-pad (css:cstyle-padding-right style) nil)))
         (pt (max 0 (css::resolve-pad (css:cstyle-padding-top style) nil))) (pb (max 0 (css::resolve-pad (css:cstyle-padding-bottom style) nil)))
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

(defun object-fit-geom (fit iw ih dx dy dw dh)
  "Map CSS object-fit FIT for an IW×IH image into content box (DX DY DW DH):
return (values ox oy ow oh src) — the dest rectangle to paint and a source crop
SRC=(sx sy sw sh) in image pixels (NIL = whole image).  `cover` crops the image to
the box's aspect ratio and fills it (a wide relief image → a portrait hero, no
distortion); `contain` fits the whole image inside the box, centred; `none` paints
at intrinsic size, centred and cropped to the box; `scale-down` is none-or-contain,
whichever is smaller.  `fill` (and any unknown value) never reaches here."
  (if (or (null iw) (null ih) (<= iw 0) (<= ih 0) (<= dw 0) (<= dh 0))
      (values dx dy dw dh nil)
      (flet ((contain ()
               (let* ((s (min (/ dw iw) (/ dh ih)))
                      (w (max 1 (round (* iw s)))) (h (max 1 (round (* ih s)))))
                 (values (+ dx (round (- dw w) 2)) (+ dy (round (- dh h) 2)) w h nil)))
             (cover ()
               (let (sx sy sw sh)
                 (if (> (* iw dh) (* ih dw))       ; image wider than box → crop its width
                     (setf sh ih sw (max 1 (round (/ (* ih dw) dh))) sy 0 sx (round (- iw sw) 2))
                     (setf sw iw sh (max 1 (round (/ (* iw dh) dw))) sx 0 sy (round (- ih sh) 2)))
                 (values dx dy dw dh (list sx sy sw sh))))
             (none ()
               (let* ((sw (min iw dw)) (sh (min ih dh))
                      (sx (round (- iw sw) 2)) (sy (round (- ih sh) 2)))
                 (values (+ dx (round (- dw sw) 2)) (+ dy (round (- dh sh) 2)) sw sh
                         (list sx sy sw sh)))))
        (cond ((string= fit "cover") (cover))
              ((string= fit "contain") (contain))
              ((string= fit "none") (none))
              ((string= fit "scale-down") (if (or (> iw dw) (> ih dh)) (contain) (none)))
              (t (values dx dy dw dh nil))))))

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
         (pl (max 0 (css::resolve-pad (css:cstyle-padding-left cs) nil))) (pr (max 0 (css::resolve-pad (css:cstyle-padding-right cs) nil)))
         (pt (max 0 (css::resolve-pad (css:cstyle-padding-top cs) nil))) (pb (max 0 (css::resolve-pad (css:cstyle-padding-bottom cs) nil)))
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
             (chh (let ((s (css::resolve-size (css:cstyle-height cs) 150))) (and (numberp s) s))))
        (multiple-value-bind (rw rh) (replaced-size cs cw chh iw ih)
          (let ((w (max 1 (round rw))) (h (max 1 (round rh))))
            (make-lbox :x 0 :y 0 :w w :h h :style cs :node node :kind :block
                       :vpaint (lambda (cv x y bw bh) (declare (ignore bw bh))
                                 (paint-svg-box cv x y w h root)))))))))

(defun canvas-box (node cs)
  "An atomic replaced box for <canvas>: sized to its width/height attrs (default
300x150).  Its gesso-backed buffer, drawn by page script, is blitted during paint."
  (let* ((iw (or (img-attr-num node "width") 300))
         (ih (or (img-attr-num node "height") 150))
         (cw (let ((s (css::resolve-size (css:cstyle-width cs) nil))) (and (numberp s) s)))
         (chh (let ((s (css::resolve-size (css:cstyle-height cs) nil))) (and (numberp s) s))))
    (multiple-value-bind (rw rh) (replaced-size cs cw chh iw ih)
      (let ((w (max 1 (round rw))) (h (max 1 (round rh))))
        (make-lbox :x 0 :y 0 :w w :h h :style cs :node node :kind :block
                   :vpaint (lambda (cv x y bw bh) (declare (ignore bw bh))
                             (paint-canvas-box cv x y node)))))))

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

(defun contents-p (styles node)
  "True when NODE is a display:contents element (CSS Display 3 §3.2): it generates
no box of its own, but its children (and ::before/::after) take its place in flow."
  (and (eq (h:dnode-kind node) :element)
       (let ((cs (st styles node))) (and cs (string= (css:cstyle-display cs) "contents")))))

(defun flatten-contents (nodes styles)
  "Replace each display:contents element in NODES by its own in-flow children
(with its generated ::before/::after), recursively, so the box tree skips it while
the cascade still inherits its computed style down to those children."
  (loop for n in nodes
        if (contents-p styles n)
          append (flatten-contents
                  (multiple-value-bind (before after) (pseudo-kids n styles)
                    (append (when before (list before))
                            (coerce (h:dnode-children n) 'list)
                            (when after (list after))))
                  styles)
        else collect n))

(defun effective-child-elements (node styles)
  "NODE's in-flow child *elements* with display:contents wrappers flattened away —
the box-generating children a flex/grid/block container actually lays out."
  (remove-if-not (lambda (k) (eq (h:dnode-kind k) :element))
                 (flatten-contents (coerce (h:dnode-children node) 'list) styles)))

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

(defun font-ascent-px (cs)
  "Ascent of CS's font in px (baseline to top of the em box)."
  (* (css:cstyle-font-size cs) (face-ascent-ratio (style-face cs))))

(defun font-descent-px (cs)
  "Descent of CS's font in px (baseline to bottom of the em box)."
  (* (css:cstyle-font-size cs) (face-descent-ratio (style-face cs))))

(defun frag-ascent (fr)
  "The ascent a text run FR contributes ABOVE the line baseline (CSS 2.1 10.8):
its font ascent plus half its own leading (line-height − em-box, split evenly)."
  (let* ((cs (frag-style fr)) (face (style-face cs)) (fs (css:cstyle-font-size cs))
         (fh (* fs (+ (face-ascent-ratio face) (face-descent-ratio face))))
         (lh (max 1 (floor (used-line-height cs face)))))
    (+ (* fs (face-ascent-ratio face)) (/ (- lh fh) 2))))

(defun last-line-baseline (lb)
  "Offset from block box LB's top to the baseline of its LAST in-flow line box
(descending through block children in document order), or NIL if it has none.
CSS 2.1 §10.8.1: an inline-block's baseline is its last in-flow line box's."
  (let ((top (lbox-y lb)) (best nil))
    (labels ((rec (box)
               (dolist (c (lbox-children box))
                 (when (lbox-p c)
                   (case (lbox-kind c)
                     (:line (when (lbox-baseline c)
                              (setf best (- (+ (lbox-y c) (lbox-baseline c)) top))))
                     (:block (rec c)))))))
      (rec lb)
      best)))

(defun atomic-baseline-ascent (lb)
  "Ascent (top of margin box to baseline) of a baseline-aligned atomic inline LB.
CSS 2.1 §10.8.1: for an inline-block with in-flow content and overflow:visible the
baseline is its last in-flow line box's baseline; a replaced box, an empty box, or
one whose overflow is not visible uses its bottom margin edge (the full height)."
  (let ((cs (lbox-style lb)))
    (or (and cs (member (css:cstyle-overflow cs) '(nil "visible") :test #'equal)
             (last-line-baseline lb))
        (lbox-h lb))))

(defun va-atomic-extent (lb base-cs)
  "The ascent/descent an atomic inline box LB contributes ABOVE/BELOW the line's
baseline for its vertical-align (CSS 2.1 10.8), plus a placement KIND.  An inline
box's own baseline is its bottom margin edge.  Returns (values ASCENT DESCENT KIND)
where KIND is :normal (positioned by baseline), :top or :bottom (line-relative)."
  (let* ((cs (lbox-style lb)) (h (lbox-h lb))
         (va (and cs (css:cstyle-vertical-align cs)))
         (fs (css:cstyle-font-size (or cs base-cs))))
    (flet ((shift (raise) (values (+ h raise) (- raise)))    ; raise>0 lifts the box up
           (baseline ()                                       ; ascent to the box's own baseline
             (let ((asc (atomic-baseline-ascent lb))) (values asc (- h asc) :normal))))
      (cond
        ((null va) (baseline))                                       ; baseline
        ((and (consp va) (numberp (first va)))                       ; <length>/<percentage>
         (shift (let ((n (first va)) (u (second va)))
                  (cond ((string= u "px") n) ((string= u "em") (* n fs))
                        ((string= u "%") (* (/ n 100.0) (used-line-height (or cs base-cs)))) (t 0)))))
        ((equal va '("super")) (shift (* 0.3 (css:cstyle-font-size base-cs))))
        ((equal va '("sub"))   (shift (* -0.2 (css:cstyle-font-size base-cs))))
        ((equal va '("middle"))
         (let ((xh (* 0.25 (css:cstyle-font-size base-cs))))         ; ~half the x-height above baseline
           (values (+ (/ h 2.0) xh) (- (/ h 2.0) xh) :normal)))
        ((equal va '("top"))    (values h 0 :top))
        ((equal va '("bottom")) (values h 0 :bottom))
        (t (baseline))))))

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

(defparameter *hyphen-char* (string (code-char #x2010))
  "U+2010 HYPHEN — the visible glyph inserted at an automatic break (CSS Text 3 §6.1).")

(defun try-hyphenate (word style room)
  "When WORD hyphenates (English, hyphens:auto) so a leading fragment plus a hyphen
glyph fits in ROOM px, return (values PREFIX+hyphen REST) for the LARGEST such
fragment; else (values NIL NIL).  Keeps the head/tail minimums of HYPHENATE-WORD."
  (let ((offs (hyphenate-word word)))
    (if (null offs)
        (values nil nil)
        (let ((hw (word-w *hyphen-char* style)) (best nil))
          (dolist (q offs)                     ; OFFS ascending: last that fits is largest
            (when (<= (+ (word-w (subseq word 0 q) style) hw) room) (setf best q)))
          (if best
              (values (concatenate 'string (subseq word 0 best) *hyphen-char*)
                      (subseq word best))
              (values nil nil))))))

(defun word-min-width (word style node)
  "Min-content width of a single WORD: its full painted width, or — under
hyphens:auto (English) — the width of its widest hyphenation fragment, since the
word may break at any legal point (CSS Text 3 §6.1).  Interior fragments carry a
trailing hyphen, so they include the hyphen glyph's width."
  (let ((full (word-w word style)))
    (if (and node (string= (css:cstyle-hyphens style) "auto") (hyphenation-lang-ok-p node))
        (let ((offs (hyphenate-word word)))
          (if (null offs) full
              (let ((hw (word-w *hyphen-char* style)) (prev 0) (best 0))
                (dolist (q offs)
                  (setf best (max best (+ (word-w (subseq word prev q) style) hw)) prev q))
                (max best (word-w (subseq word prev) style)))))   ; last fragment: no hyphen
        full)))

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
         ;; Resolve the logical text-align keywords START/END to physical
         ;; LEFT/RIGHT against the inline base direction (CSS Text 3 §7.1):
         ;; start=left, end=right in LTR and the mirror in RTL.  Without this
         ;; they fell through to the left-align default.
         (align (let ((a (css:cstyle-text-align base-cs))
                      (rtl (string= (css:cstyle-direction base-cs) "rtl")))
                  (cond ((string= a "start") (if rtl "right" "left"))
                        ((string= a "end")   (if rtl "left" "right"))
                        (t a))))
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
                ;; before wrapping, try automatic hyphenation (CSS Text 3 §6.1): if
                ;; the word breaks so a leading fragment + hyphen still fits this
                ;; line, place it and send the remainder to the next line.
                (when (and (not atomic)
                           (string= (css:cstyle-hyphens (tok-meta wd)) "auto")
                           (hyphenation-lang-ok-p (tok-node wd)))
                  (multiple-value-bind (prefix rest)
                      (try-hyphenate (car wd) (tok-meta wd) (- avail (- cx lx) sw))
                    (when prefix
                      (when (and (> sw 0) (> (- cx lx) 0)) (incf cx sw))
                      (push (make-frag :x cx :w (word-w prefix (tok-meta wd)) :text prefix
                                       :style (tok-meta wd) :node (tok-node wd)) cur)
                      (setf (aref ws i) (list rest (tok-meta wd) nil 0 (tok-node wd))))))
                (return))
              ;; leading advance: collapsible whitespace is dropped at line start, but
              ;; a leading inline margin (TOK-GAP) still offsets the first item.
              (when (> sw 0)
                (if (> (- cx lx) 0) (incf cx sw) (incf cx (tok-gap wd))))
              (let ((room (- avail (- cx lx))))
                (cond
                  ;; a word too wide for this line that hyphenates (CSS Text 3 §6.1):
                  ;; place the largest leading fragment + hyphen that fits and send
                  ;; the remainder to the next line (where it may hyphenate again).
                  ((and (not atomic) (not nowrap) (> ww room) (>= room 1)
                        (string= (css:cstyle-hyphens (tok-meta wd)) "auto")
                        (hyphenation-lang-ok-p (tok-node wd))
                        (nth-value 0 (try-hyphenate (car wd) (tok-meta wd) room)))
                   (multiple-value-bind (prefix rest) (try-hyphenate (car wd) (tok-meta wd) room)
                     (push (make-frag :x cx :w (word-w prefix (tok-meta wd)) :text prefix
                                      :style (tok-meta wd) :node (tok-node wd)) cur)
                     (setf (aref ws i) (list rest (tok-meta wd) nil 0 (tok-node wd)))
                     (return)))
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
                              ((string= align "right") (max 0 (- avail used))) (t 0)))
                 ;; --- per-line baseline model (CSS 2.1 §10.8) --------------------
                 ;; The line box has a single baseline.  Every inline box contributes
                 ;; an ASCENT above it: the block's strut (its font ascent + half its
                 ;; leading), each text run at its own font, and each atomic per its
                 ;; vertical-align (an empty inline-block / replaced box by its bottom
                 ;; margin edge, an inline-block-with-content by its own last-line
                 ;; baseline).  The line's baseline sits MAX-ASCENT below the top; the
                 ;; painter drops every text run onto it (LBOX-BASELINE) instead of
                 ;; centering each run, so text sits ON the baseline next to a tall
                 ;; atomic rather than floating to the line's vertical middle.  With
                 ;; one font and no atomics this is just the strut, so a plain text
                 ;; line's baseline (and paint) is unchanged to the pixel.
                 (strut-fh (+ (font-ascent-px base-cs) (font-descent-px base-cs)))
                 (strut-asc (+ (font-ascent-px base-cs) (/ (- lh strut-fh) 2)))
                 (strut-desc (- lh strut-asc))
                 (metrics (loop for it in items unless (frag-p it)
                                collect (multiple-value-bind (a d k) (va-atomic-extent it base-cs)
                                          (list it a d (or k :normal)))))
                 ;; LINE-ASC / LINE-DESC drive line-box height + atomic placement and
                 ;; are kept as before (strut + atomics only) so element geometry is
                 ;; unchanged.  The paint BASELINE additionally maxes in each text
                 ;; run's own ascent, so mixed font sizes on a line share one baseline.
                 ;; CSS 2.1 §10.8: the line box height is first set by the
                 ;; baseline-relative content (the strut and every baseline-aligned
                 ;; atomic — :normal, incl. sub/super/middle/length).  A `top`- or
                 ;; `bottom`-aligned box is then placed against the line edge and
                 ;; grows the box ONLY when it is taller than that: a top box adds to
                 ;; the descent, a bottom box to the ascent.  (The old code added a
                 ;; top box's FULL height into the ascent and still kept the strut
                 ;; descent below it, so a top-aligned image exactly filling the line
                 ;; made the box ~font-descent too tall.)
                 (base-asc (reduce #'max metrics :initial-value strut-asc
                                   :key (lambda (m) (if (member (fourth m) '(:top :bottom)) 0.0 (second m)))))
                 (base-desc (reduce #'max metrics :initial-value strut-desc
                                    :key (lambda (m) (if (member (fourth m) '(:top :bottom)) 0.0 (third m)))))
                 (line-asc (reduce #'max metrics :initial-value base-asc
                                   :key (lambda (m) (if (eq (fourth m) :bottom)
                                                        (- (lbox-h (first m)) base-desc) 0.0))))
                 (line-desc (reduce #'max metrics :initial-value base-desc
                                    :key (lambda (m) (if (eq (fourth m) :top)
                                                         (- (lbox-h (first m)) base-asc) 0.0))))
                 (lh2 (max 1 (round (+ line-asc line-desc))))
                 ;; baseline offset (line top -> baseline) shared by all text runs.
                 (baseline (round (reduce #'max items :initial-value line-asc
                                          :key (lambda (it) (if (frag-p it) (frag-ascent it) 0.0))))))
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
            (if (loop for m in metrics
                      thereis (let ((va (css:cstyle-vertical-align (lbox-style (first m)))))
                                (and (consp va) (numberp (first va)))))   ; a <length>/<percentage> shift
                ;; a line carrying an explicit vertical-align uses the baseline model:
                ;; place each atomic at its aligned position; text frags paint from the
                ;; shared line baseline (LINE-ASC), so they line up on it.
                (progn
                  (dolist (m metrics)
                    (destructuring-bind (it a d k) m
                      (declare (ignore d))
                      (let ((top (case k
                                   (:top    (round y))
                                   (:bottom (round (+ y (- lh2 (lbox-h it)))))
                                   (t       (round (+ y (- line-asc a)))))))
                        (shift-box it 0 (- top (round (lbox-y it)))))))
                  (push (make-lbox :x lx :y y :w avail :h lh2 :kind :line
                                   :children items :baseline baseline) lines)
                  (incf y lh2) (incf h lh2))
                ;; the common case (no explicit vertical-align): baseline-align each
                ;; atomic.  An atomic inline's own baseline is its bottom margin edge
                ;; (CSS 2.1 10.8 / 10.8.1: empty inline-block, replaced content, or
                ;; overflow!=visible), so default vertical-align:baseline drops each
                ;; box's bottom onto the line's baseline — the bottom of the tallest
                ;; atomic (LINE-H is seeded from the text strut and grown to the
                ;; tallest atomic).  Bottom-aligning atomics of unequal height is the
                ;; primitive: previously they were all top-aligned, mis-placing a short
                ;; inline-block/img sharing a line with a taller one.
                ;; Atomics are bottom-aligned on LINE-H (their baseline = the line's,
                ;; Chrome-verified), but the LINE BOX height is LH2 = line-asc +
                ;; line-desc: when a tall atomic sets the ascent, the block's strut
                ;; still contributes its below-baseline DESCENT, so the line (and its
                ;; container) extend ~font-descent past the atomic's bottom exactly as
                ;; CSS 2.1 §10.8 requires (an img+text or tall-inline-block line is
                ;; font-descent taller than the atomic — matches Chrome).  For plain
                ;; text and sub-strut atomics LH2 == LINE-H, so nothing else moves.
                (progn
                  (dolist (it items)
                    (unless (frag-p it)
                      (shift-box it 0 (round (+ y (- line-h (lbox-h it)))))))
                  (push (make-lbox :x lx :y y :w avail :h lh2 :kind :line
                                   :children items :baseline baseline) lines)
                  (incf y lh2) (incf h lh2)))))))
    (values (nreverse lines) h)))

(defun collect-raw (node)
  "Raw text of NODE preserving whitespace (for <pre>)."
  (with-output-to-string (o)
    (labels ((rec (n) (case (h:dnode-kind n) (:text (write-string (h:dnode-data n) o))
                        (:element (loop for c across (h:dnode-children n) do (rec c))))))
      (rec node))))

(defun collect-styled (node styles default-style)
  "Walk NODE's subtree preserving whitespace, returning a list of (TEXT STYLE NODE)
runs — each text node tagged with its nearest element's computed style.  Lets the
<pre> fast path keep per-token colour (syntax-highlighted <span>s) instead of
flattening every token to the pre's own colour."
  (let ((out '()))
    (labels ((rec (n st onode)
               (case (h:dnode-kind n)
                 (:text (push (list (h:dnode-data n) st onode) out))
                 (:element (let ((cs (or (st styles n) st)))
                             (loop for c across (h:dnode-children n) do (rec c cs n)))))))
      (rec node default-style node))
    (nreverse out)))
(defun split-newlines (s)
  (loop with start = 0 for i from 0 to (length s)
        when (or (= i (length s)) (char= (char s i) #\Newline))
          collect (prog1 (subseq s start i) (setf start (1+ i)))))
(defun has-block-children (styles node)
  (some (lambda (c) (block-level-p styles c))
        (flatten-contents (coerce (h:dnode-children node) 'list) styles)))

;;; ---- block layout -------------------------------------------------------
(defvar *layout-debug* nil)
(defparameter *max-layout-depth* 400
  "Cap on layout recursion depth.  Real pages nest far shallower; capping keeps a
pathological or hostile document (thousands of nested boxes) from exhausting the
control stack — the outer levels render and the runaway interior is dropped, rather
than the whole render crashing.")
(defvar *layout-depth* 0)
(defvar *flex-main-size* nil
  "When laying out a flex item, its flex-resolved main size (px).  The item's own
`width` is overridden by this assigned size (CSS 9.9), like a table-cell in its column.
Reset to NIL inside the item so its descendants are laid out normally.")
(defvar *pos-border-w* nil
  "When re-laying out an out-of-flow box whose width is auto but both `left` and
`right` are definite, its used BORDER-box width = cb - left - right - margins
(CSS 2.1 10.3.7).  Consumed by the box; NIL for its descendants.")
(defvar *pos-border-h* nil
  "As *POS-BORDER-W* for an auto height with definite `top` and `bottom` (10.6.4).")
(defvar *flex-item-height* nil
  "When a flex item is stretched to a definite cross size (align stretch + auto
height in a row container of definite height, CSS 9.4), its resolved content-box
height (px).  This makes the item a definite-height containing block, so a
`height:100%` (or ratio-transferring) child resolves against it.  Consumed once
by the item box, then reset to NIL for its descendants.")
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

(defun resolve-inset (v extent)
  "Resolve an out-of-flow inset (top/bottom/left/right) V against the containing
block EXTENT: a length passes through, a percentage resolves against EXTENT, and
auto (NIL / :auto) returns NIL so the caller keeps the static position."
  (cond ((numberp v) v)
        ((and (consp v) (eq (first v) :percent) (numberp extent))
         (* extent (/ (second v) 100.0)))
        (t nil)))

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
             ;; CSS 2.1 §10.3.7: definite left AND right with an auto margin — the
             ;; auto margin(s) absorb the leftover space; two autos center the box
             ;; (the `position:fixed; inset:0; margin:auto` centering idiom).
             (h-auto (and (numberp left) (numberp right)
                          (or (css:cstyle-margin-left-auto cs) (css:cstyle-margin-right-auto cs))))
             ;; CSS 2.1 §10.6.4/§10.6.5: top and bottom position the *margin* edges,
             ;; resolved (percentages included) against the containing block's block
             ;; extent.  When both are definite and a block-axis margin is auto, the
             ;; auto margin(s) absorb the leftover space (two autos split it) — which
             ;; may be NEGATIVE (top:50%;bottom:0;margin:auto pulls the box up).
             (rtop (resolve-inset top ph)) (rbot (resolve-inset bottom ph))
             (v-auto (and rtop rbot
                          (or (css:cstyle-margin-top-auto cs) (css:cstyle-margin-bottom-auto cs))))
             (nx (cond (h-auto
                        (let ((space (- pw (lbox-w lb) left right))
                              (mla (css:cstyle-margin-left-auto cs))
                              (mra (css:cstyle-margin-right-auto cs)))
                          (+ px left (cond ((and mla mra) (/ (max 0 space) 2.0))
                                           (mla (max 0 space))
                                           (t 0)))))
                       ((numberp left)  (+ px left ml))
                       ((numberp right) (+ px (- pw (lbox-w lb) right mr)))
                       (t (lbox-x lb))))
             (ny (cond (v-auto
                        (let* ((mta (css:cstyle-margin-top-auto cs))
                               (mba (css:cstyle-margin-bottom-auto cs))
                               ;; leftover after the non-auto margins are honored
                               (space (- ph (lbox-h lb) rtop rbot (if mta 0 mt) (if mba 0 mb)))
                               (used-mt (cond ((and mta mba) (/ space 2.0))
                                              (mta space)
                                              (t mt))))
                          (+ py rtop used-mt)))
                       (rtop (+ py rtop mt))
                       (rbot (+ py (- ph (lbox-h lb) rbot mb)))
                       (t (lbox-y lb)))))
        (shift-box lb (round (- nx (lbox-x lb))) (round (- ny (lbox-y lb))))))))

(defun %percent-size-p (v)
  "True when a width/height spec V is a percentage — it must resolve against the
containing block, not the static-flow width the box was tentatively laid out in."
  (and (consp v) (eq (first v) :percent)))

(defun positioned-needs-cb-size-p (cs)
  "True when an out-of-flow box's used width or height depends on its containing
block: a percentage width/height (e.g. a full-cover hero `width:100%;height:100%`)."
  (or (%percent-size-p (css:cstyle-width cs))
      (%percent-size-p (css:cstyle-height cs))))

(defun copy-lbox-into (dst src)
  "Overwrite DST's geometry and content slots with SRC's, keeping DST's identity so
references held elsewhere (the paint tree's child list) observe the new geometry."
  (setf (lbox-x dst) (lbox-x src) (lbox-y dst) (lbox-y src)
        (lbox-w dst) (lbox-w src) (lbox-h dst) (lbox-h src)
        (lbox-style dst) (lbox-style src) (lbox-kind dst) (lbox-kind src)
        (lbox-children dst) (lbox-children src) (lbox-marker dst) (lbox-marker src)
        (lbox-img dst) (lbox-img src) (lbox-vpaint dst) (lbox-vpaint src))
  dst)

(defun finalize-positioned (entry cb styles)
  "Resolve an out-of-flow box (LB NODE CS) against its true containing block
CB=(px py pw ph).  A box tentatively laid out at the static-flow point sized its
percentage width/height against the wrong (flow content) width; when it needs the
containing block's dimensions (CSS 2.1 10.1/10.3.7 — e.g. `width:100%;height:100%`),
re-lay it out with CB's width and height as the available space and copy the fresh
geometry back into the collected box (the paint tree holds it by reference).  Then
shift it to its final top/left."
  (destructuring-bind (lb node cs) entry
    (when (and lb cb)
      (destructuring-bind (px py pw ph) cb
        (declare (ignore px py))
        ;; CSS 2.1 10.3.7 / 10.6.4: an out-of-flow box with an auto width (height)
        ;; but definite left+right (top+bottom) fills the gap between the offsets —
        ;; its used border-box size is cb minus those offsets and its own margins.
        (let* (;; a table box sizes to its own column model (shrink-to-fit), not the
               ;; left/right gap — so it is not an auto-width fill candidate (§17.5.2
               ;; overrides §10.3.7), letting auto margins then center it.
               (auto-w (and (member (css:cstyle-width cs) '(nil :auto))
                            (not (member (cdisplay cs) '("table" "inline-table") :test #'string=))))
               (auto-h (member (css:cstyle-height cs) '(nil :auto)))
               (l (css:cstyle-left cs)) (r (css:cstyle-right cs))
               (tp (css:cstyle-top cs)) (bt (css:cstyle-bottom cs))
               (pos-w (and auto-w (numberp l) (numberp r)
                           (max 0 (- pw l r (css:cstyle-margin-left cs) (css:cstyle-margin-right cs)))))
               (pos-h (and auto-h (numberp tp) (numberp bt)
                           (max 0 (- ph tp bt (css:cstyle-margin-top cs) (css:cstyle-margin-bottom cs))))))
          (when (or (positioned-needs-cb-size-p cs) pos-w pos-h)
            (handler-case
                (let* ((*pos-border-w* pos-w) (*pos-border-h* pos-h)
                       (nlb (layout-node node styles (lbox-x lb) (lbox-y lb) pw ph)))
                  (when nlb (copy-lbox-into lb nlb)))
              (error () nil))))
        (resolve-positioned lb cb cs)))))

(defun replaced-box (node cs &optional avail-w avail-h)
  "The replaced-content box for a leaf replaced element (img / svg / canvas / an
   <object> that decodes to an image), or NIL when NODE is not one.  These have no
   flow children — the box IS the content — so block-level and out-of-flow (absolute,
   e.g. the Next.js Image fill pattern) replaced elements render here, not only the
   inline ones handled in COLLECT-WORDS.  AVAIL-W is the containing width for a
   percentage-sized image."
  (when (eq (h:dnode-kind node) :element)
    (let ((name (h:dnode-name node)))
      (cond ((string-equal name "img") (img-box node cs avail-w avail-h))
            ((string-equal name "svg") (svg-box node cs))
            ((string-equal name "canvas") (canvas-box node cs))
            ((and (string-equal name "object") (object-data-image node))
             (object-box node cs (object-data-image node)))
            ;; <iframe> is replaced: its content is the nested document, never the
            ;; fallback children — an unsupported iframe renders as an empty box of
            ;; its own size (default 300x150), not the "FAIL" text between its tags.
            ((string-equal name "iframe")
             (let ((w (let ((s (css::resolve-size (css:cstyle-width cs) (or avail-w 300)))) (if (numberp s) (max 0 s) 300)))
                   (hh (let ((s (css::resolve-size (css:cstyle-height cs) avail-h))) (if (numberp s) (max 0 s) 150))))
               (make-lbox :x 0 :y 0 :w w :h hh :style cs :node node :kind :block)))))))

;;; ---- vertical writing modes (CSS Writing Modes 3) ----------------------
;;; A vertical box's subtree is laid out in a transposed "logical" frame — physical
;;; width/height, margins, padding and borders swapped — with the normal horizontal
;;; engine, then the resulting boxes are transposed back to physical coordinates
;;; (swap x<->y and w<->h; mirror the block axis for vertical-rl).  This gives the
;;; right box geometry for block children and text lines; orthogonal children
;;; (a horizontal box inside the vertical one) are left approximate.
(defun vertical-wm-p (cs)
  (and cs (member (css:cstyle-writing-mode cs) '("vertical-rl" "vertical-lr") :test #'string=)))
(defun has-abs-descendant-p (node styles)
  "True when NODE's subtree contains an absolutely/fixed-positioned box — a case the
transposed vertical flow does not yet place correctly, so it declines to transpose."
  (labels ((scan (n)
             (some (lambda (c)
                     (and (eq (h:dnode-kind c) :element)
                          (let ((cs (st styles c)))
                            (or (and cs (member (css:cstyle-position cs) '("absolute" "fixed") :test #'string=))
                                (scan c)))))
                   (h:dnode-children n))))
    (scan node)))
(defun swap-cstyle (cs)
  "A copy of CS with its physical box dimensions swapped, so the horizontal engine
lays it out in the vertical box's logical frame; writing-mode is reset so the swapped
subtree is not transposed again."
  (let ((c (css::copy-cstyle cs)))
    (rotatef (css:cstyle-width c) (css:cstyle-height c))
    (rotatef (css:cstyle-min-width c) (css:cstyle-min-height c))
    (rotatef (css:cstyle-max-width c) (css:cstyle-max-height c))
    (rotatef (css:cstyle-margin-left c) (css:cstyle-margin-top c))
    (rotatef (css:cstyle-margin-right c) (css:cstyle-margin-bottom c))
    (rotatef (css:cstyle-padding-left c) (css:cstyle-padding-top c))
    (rotatef (css:cstyle-padding-right c) (css:cstyle-padding-bottom c))
    (rotatef (css:cstyle-border-left-width c) (css:cstyle-border-top-width c))
    (rotatef (css:cstyle-border-right-width c) (css:cstyle-border-bottom-width c))
    ;; absolute insets are logical too: block-start/inline-start map across axes.
    (rotatef (css:cstyle-top c) (css:cstyle-left c))
    (rotatef (css:cstyle-bottom c) (css:cstyle-right c))
    (setf (css:cstyle-writing-mode c) "horizontal-tb")
    c))
(defun swap-all-styles (styles)
  "STYLES with every cstyle value replaced by its dimension-swapped copy."
  (let ((into (make-hash-table :test (hash-table-test styles))))
    (maphash (lambda (k v) (setf (gethash k into) (if (typep v 'css:cstyle) (swap-cstyle v) v))) styles)
    into))
(defun transpose-tree (box block-extent rl)
  "Transpose a logical box subtree to physical vertical coordinates in place: swap
x<->y and w<->h of every lbox; for vertical-rl mirror the block axis within
BLOCK-EXTENT (the physical width)."
  (when (typep box 'lbox)
    (let ((lx (lbox-x box)) (ly (lbox-y box)) (lw (lbox-w box)) (lh (lbox-h box)))
      (setf (lbox-x box) (if rl (- block-extent ly lh) ly)
            (lbox-y box) lx (lbox-w box) lh (lbox-h box) lw))
    (dolist (c (lbox-children box))
      (when (typep c 'lbox) (transpose-tree c block-extent rl)))))
(defun layout-vertical (node styles x y avail-w avail-h)
  "Lay out a vertical writing-mode NODE (its parent is horizontal).  Returns the
same (values lbox advance mt mb mneg) as %LAYOUT-NODE."
  (let* ((cs (st styles node))
         (rl (string= (css:cstyle-writing-mode cs) "vertical-rl"))
         (swapped (swap-all-styles styles))
         ;; logical inline available = the physical vertical space (so text wraps at
         ;; the box's height); fall back to the block-axis space.
         (logical-avail (or avail-h avail-w 0)))
    (multiple-value-bind (lb adv mt mb mneg)
        (%layout-node node swapped 0 0 logical-avail avail-w)
      (declare (ignore adv mt mb mneg))
      (if (null lb)
          (values nil 0 0 0 0)
          (let ((block-extent (lbox-h lb)))          ; logical height = physical width
            (transpose-tree lb block-extent rl)
            (shift-box lb (round x) (round y))
            (values lb (lbox-h lb) 0 (lbox-h lb) 0))))))

(defun block-align-content (cs)
  "For a block-level box, its align-content when set to an explicit *distributing*
value (CSS Box Alignment 3 §5.3) — center/end/space-*/baseline — which aligns the
box's content in the block axis and makes it establish an independent formatting
context.  Returns the value, else NIL (normal/start/stretch are indistinguishable
from weft's default here, so they do nothing)."
  (and cs
       (member (cdisplay cs) '("block" "flow-root" "list-item") :test #'string=)
       (let ((ac (css:cstyle-align-content cs)))
         (and (member ac '("center" "end" "flex-end" "space-between" "space-around"
                           "space-evenly" "baseline")
                      :test #'string=)
              ac))))
(defun establishes-bfc-p (cs)
  "True when a box with computed style CS establishes a block formatting context
(CSS 2.1 §9.4.1, CSS Display 3): display:flow-root, an inline-block, a table-cell/
caption, a non-visible overflow (the clearfix idiom), or a non-normal align-content
(CSS Box Alignment 3).  Such a box contains its own floats, keeps outside floats
from intruding, and does not collapse its margins with its children.  Floated and
absolutely-positioned boxes establish one too, but are handled where they are
placed / positioned."
  (and cs
       (or (member (cdisplay cs) '("flow-root" "inline-block" "table-cell"
                                   "inline-table" "table-caption")
                   :test #'string=)
           (member (css:cstyle-overflow cs) '("hidden" "scroll" "auto")
                   :test #'string=)
           (block-align-content cs))))

(defun %layout-node (node styles x y avail-w &optional avail-h)
  "Establish an absolute containing block for positioned elements, then lay the
node out.  A positioned element (relative/absolute/fixed) is the containing block
for its absolutely-positioned descendants; we collect those during subtree layout
and resolve them once this box's geometry is known (then a later unit-shift of
this box, if any, carries them along correctly).  A box establishing a BFC (see
ESTABLISHES-BFC-P) isolates and contains its floats the same way an abs/fixed box
does, though it is not a containing block for positioned descendants."
  (let* ((cs (st styles node))
         (pos (and cs (css:cstyle-position cs)))
         (positioned (and pos (member pos '("relative" "absolute" "fixed") :test #'string=)))
         ;; a box isolates floats when it establishes a BFC: abs/fixed always do,
         ;; and so do flow-root / overflow-clip / inline-block / table-cell boxes.
         (bfc (and cs (or (member pos '("absolute" "fixed") :test #'string=)
                          (establishes-bfc-p cs)))))
    (cond
     ;; a vertical writing-mode box lays its subtree out transposed (§ above),
     ;; unless it holds an out-of-flow descendant it cannot yet place — then it
     ;; falls back to normal flow rather than mis-transposing it.
     ((and cs (vertical-wm-p cs) (not (has-abs-descendant-p node styles)))
      (layout-vertical node styles x y avail-w avail-h))
     ((or positioned bfc)
        (let ((*abs-pending* (if positioned nil *abs-pending*))
              ;; Rebind *FLOATS* to NIL for a BFC so floats inside it do not
              ;; interact with the surrounding context (e.g. the float in Acid2's
              ;; absolute .eyes box must not shove the .nose float sideways).
              (*floats* (if bfc nil *floats*)))
          (multiple-value-bind (lb adv mt-eff mb-eff mneg) (%layout-core node styles x y avail-w avail-h)
            ;; A BFC box contains its floats: with auto height it grows so its
            ;; bottom border edge sits below the lowest float's bottom margin edge
            ;; (CSS 2.1 10.6.7).  Acid2's `.first.one` is an absolute auto-height
            ;; block whose only content is a float — its height is that float's
            ;; 12px, not 0.  Every *FLOATS* entry was generated inside this box.
            (when (and lb bfc *floats*
                       (null (css::resolve-height (css:cstyle-height cs) avail-h)))
              (let ((mfb (loop for f in *floats* maximize (fifth f)))
                    (bot (+ (lbox-y lb) (lbox-h lb))))
                (when (> mfb bot)
                  (setf (lbox-h lb) (- (+ mfb (used-border cs :b)) (lbox-y lb))))))
            (when (and lb positioned *abs-pending*)
              (let ((cb (pad-box lb cs)))
                (dolist (p *abs-pending*) (finalize-positioned p cb styles))))
            (values lb adv mt-eff mb-eff mneg))))
     (t (%layout-core node styles x y avail-w avail-h)))))

(defun collapse-margins (&rest ms)
  "CSS 2.1 8.3.1 collapsed margin of MS: sum of the largest positive and the
most-negative margin.  {20,30}->30  {20,-10}->10  {-5,-8}->-8  {-20,30}->10."
  (+ (reduce #'max ms :initial-value 0)
     (reduce #'min ms :initial-value 0)))

(defun %layout-core (node styles x y avail-w &optional avail-h)
  "Lay out block-level NODE at (X,Y); AVAIL-W is the containing content width and
AVAIL-H the containing-block height (px when definite, else NIL).
Returns (values lbox advance-height)."
  (let ((flex-item *flex-main-size*) (*flex-main-size* nil)   ; this box is a flex item iff set; its children are not
        (pos-bw *pos-border-w*) (*pos-border-w* nil)
        (pos-bh *pos-border-h*) (*pos-border-h* nil)
        (flex-ch *flex-item-height*) (*flex-item-height* nil))
   (let ((cs (st styles node)))
    (when (or (null cs) (string= (cdisplay cs) "none")) (return-from %layout-core (values nil 0 0 0)))
    ;; replaced elements (img/svg/canvas/object-image) reaching block layout — as a
    ;; block-level or out-of-flow box — are their own content; render and return.
    (let ((rb (replaced-box node cs avail-w avail-h)))
      (when rb
        ;; REPLACED-BOX builds the box at (0,0) with the bitmap on an inner content
        ;; child at coords relative to it; SHIFT-BOX moves the whole subtree so the
        ;; child ends up at absolute coords too (else the image blits at the origin).
        (shift-box rb x y)
        (return-from %layout-core (values rb (lbox-h rb) 0 0 0))))
    (let* ((mt (css:cstyle-margin-top cs)) (mb (css:cstyle-margin-bottom cs))
           (ml (css:cstyle-margin-left cs)) (mr (css:cstyle-margin-right cs))
           (pt (css::resolve-pad (css:cstyle-padding-top cs) avail-w)) (pb (css::resolve-pad (css:cstyle-padding-bottom cs) avail-w))
           (pl (css::resolve-pad (css:cstyle-padding-left cs) avail-w)) (pr (css::resolve-pad (css:cstyle-padding-right cs) avail-w))
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
           ;; a flex item stretched to a definite cross size adopts it as a definite
           ;; height when its own height is auto (FLEX-CH is the content-box height);
           ;; EXP-H is carried in the box-sizing sense so downstream math is uniform.
           (exp-h (cond ((and flex-ch (null (css:cstyle-height cs)))
                         (if border-box (+ flex-ch pt pb bt bb) flex-ch))
                        ;; auto height with definite top+bottom: fill the gap (10.6.4)
                        (pos-bh (if border-box pos-bh (max 0 (- pos-bh pt pb bt bb))))
                        (t (css::resolve-height (css:cstyle-height cs) avail-h))))  ; px or nil
           ;; content height handed to children as THEIR containing-block height:
           ;; this box's content-box height when its height is explicit, else NIL
           ;; (auto height is indefinite, so child percentage heights -> auto).
           (child-avail-h (when (numberp exp-h)
                            (max 0 (if (string= (css:cstyle-box-sizing cs) "border-box")
                                       (- exp-h (+ pt pb (used-border cs :t) (used-border cs :b)))
                                       exp-h))))
           ;; explicit aspect-ratio (CSS Sizing 4) for a non-replaced box: a positive
           ;; ratio derives the auto axis from the definite one (width<->height).
           (ar (let ((r (css:cstyle-aspect-ratio cs))) (and (numberp r) (plusp r) r)))
           (min-h (css::resolve-min-height (css:cstyle-min-height cs) avail-h))
           (max-h (css::resolve-max-height (css:cstyle-max-height cs) avail-h))
           ;; the USED content-box height (min/max-clamped) when height is definite,
           ;; else NIL — aspect-ratio derives the width from this, so `height:300px;
           ;; max-height:25px` gives a box 25 tall and 25*ratio wide.
           (used-h (when (numberp exp-h)
                     (let ((h (if border-box (- exp-h pt pb bt bb) exp-h)))
                       (when (numberp max-h) (setf h (min h max-h)))
                       (when (and (numberp min-h) (> min-h 0)) (setf h (max h min-h)))
                       (max 0 h))))
           ;; a flex item takes the main size its parent assigned (AVAIL-W = the
           ;; flex-resolved width), ignoring its own `width` — set it explicitly so an
           ;; inline-block item fills that width instead of shrinking to content.
           ;; intrinsic-sizing keywords (CSS Sizing 3): min-content / max-content /
           ;; fit-content resolve to the box's own content measure, carried in the
           ;; box-sizing sense so the width math below treats it like any length.
           (spec-w (cond (flex-item avail-w)
                         (table-cell nil)
                         ;; auto width with definite left+right: fill the gap (10.3.7)
                         (pos-bw (if border-box pos-bw (max 0 (- pos-bw pad-bord))))
                         ((member (css:cstyle-width cs) '(:min-content :max-content :fit-content))
                          (let* ((av (or avail-w 0))
                                 (c (case (css:cstyle-width cs)
                                      (:min-content (min-content-width node styles av))
                                      (:max-content (pref-content-width node styles av))
                                      (:fit-content (max (min-content-width node styles av)
                                                         (min (pref-content-width node styles av) av))))))
                            ;; with a definite (clamped) block size and an aspect-ratio, the
                            ;; intrinsic inline size is that block size transferred through the
                            ;; ratio, floored by the content measure (CSS Sizing 4 §4).
                            (when (and ar used-h) (setf c (max c (* used-h ar))))
                            (if border-box (+ c pad-bord) c)))
                         (t (css::resolve-size (css:cstyle-width cs) avail-w)))) ; px or nil
           ;; min-width/max-width may name an intrinsic-sizing keyword (CSS Sizing 3):
           ;; resolve it from the box's own content measure (a CONTENT-box width, so
           ;; border-box adds padding+border to make the clamp a border-box value).
           ;; Boxes with an aspect-ratio are skipped — their intrinsic minimum is the
           ;; transferred size (CSS Sizing 4 §aspect-ratio-minimum), not the content.
           (max-w (let ((k (and (not ar) (intrinsic-keyword-width (css:cstyle-max-width cs) node styles avail-w))))
                    (cond ((null k) (css::resolve-size (css:cstyle-max-width cs) avail-w))
                          (border-box (+ k pad-bord))
                          (t k))))
           (min-w (let ((k (and (not ar) (intrinsic-keyword-width (css:cstyle-min-width cs) node styles avail-w))))
                    (cond ((null k) (css::resolve-size (css:cstyle-min-width cs) avail-w))
                          (border-box (+ k pad-bord))
                          (t k))))
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
                                  ;; aspect-ratio with a definite height and auto width:
                                  ;; width comes from the (min/max-clamped) height * ratio
                                  ;; — for a plain block too, not only shrink boxes
                                  ;; (border-box = border-height*ratio; content-box adds
                                  ;; the horizontal padding+border).
                                  ((and ar used-h (null spec-w))
                                   (if border-box (* (+ used-h pt pb bt bb) ar) (+ (* used-h ar) pad-bord)))
                                  (shrink (min (- avail-w ml mr)
                                               (+ (pref-content-width node styles (- avail-w ml mr))
                                                  pad-bord)))
                                  (table-shrink
                                   ;; auto table width = max(min-content, min(available,
                                   ;; max-content)) — it shrinks to fit but never below its
                                   ;; min-content, overflowing the container instead (CSS 2.1
                                   ;; §17.5.2): four 50px cells in a 100px scroller stay 200.
                                   ;; an empty table (no cells, NAT 0) shrinks to its
                                   ;; own padding+border box, not the full available
                                   ;; width (css-tables-3 §computing-the-table-width):
                                   ;; a display:table with padding:155px is 310 wide.
                                   (let ((nat (table-natural-width node styles (- avail-w ml mr))))
                                     (max (+ (table-min-width node styles (- avail-w ml mr)) pad-bord)
                                          (min (- avail-w ml mr) (+ nat pad-bord)))))
                                  (t (- avail-w ml mr)))))
                    (when (numberp max-w) (setf bw (min bw (if border-box max-w (+ max-w pad-bord)))))
                    (when (numberp min-w) (setf bw (max bw (if border-box min-w (+ min-w pad-bord)))))
                    ;; a table box is never narrower than its min-content inline size,
                    ;; even under an explicit or max- constrained width (CSS 2.1 §17.5.2 /
                    ;; css-tables-3): display:table with inline-size:30px but a 60px cell
                    ;; is widened to fit.  This floor overrides width AND max-width.
                    (when (string= (cdisplay cs) "table")
                      (setf bw (max bw (+ (table-min-width node styles (- avail-w ml mr)) pad-bord))))
                    (max 0 bw)))
           ;; margin:auto centering when width is constrained — a definite width, a
           ;; definite max-width, or a width transferred from a definite height through
           ;; an aspect-ratio (all leave leftover inline space for the auto margins).
           (ml (if (and (css:cstyle-margin-left-auto cs) (css:cstyle-margin-right-auto cs)
                        (or (numberp spec-w) (numberp max-w) (and ar used-h (null spec-w)))
                        (< width avail-w))
                   (max 0 (floor (- avail-w width) 2)) ml))
           (content-w (max 0 (- width pad-bord)))
           ;; aspect-ratio-derived content-box height: when the box has a ratio
           ;; and an auto height, its content-box height is width/ratio (min/max
           ;; clamped).  This is a *definite* height, so a flex/grid container
           ;; hands it to children as their containing block and column-wrapping
           ;; sees it (CSS Sizing 4 §4).  NIL when height is definite or no ratio.
           (ar-h (when (and ar (null used-h))
                   (let ((rh (if border-box (max 0 (- (/ width ar) pt pb bt bb))
                                 (/ content-w ar))))
                     (when (numberp max-h) (setf rh (min rh max-h)))
                     (when (and (numberp min-h) (> min-h 0)) (setf rh (max rh min-h)))
                     (max 0 rh))))
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
        ;; Build styled line boxes: split each text run on newlines, and keep a
        ;; frag per run so syntax-highlight colours survive (a plain <pre> has one
        ;; run per line, identical to before).
        (let* ((segs (collect-styled node styles cs)) (yy cy)
               (lh (max *font-h* (round (used-line-height cs))))
               (frags '()) (lx cx))
          (labels ((emit-line ()
                     (push (make-lbox :x cx :y yy :w content-w :h lh :kind :line
                                      :children (nreverse frags))
                           children)
                     (setf frags '() lx cx) (incf yy lh) (incf content-h lh)))
            (dolist (seg segs)
              (destructuring-bind (txt st snode) seg
                (loop for part in (split-newlines txt) for i from 0 do
                  (when (plusp i) (emit-line))                 ; newline -> next line
                  (when (plusp (length part))
                    (let ((ww (word-w part st)))
                      (push (make-frag :x lx :w ww :text part :style st :node snode) frags)
                      (incf lx ww))))))
            (emit-line)))
        (let* ((box-h (+ content-h pt pb bt bb))
               (lb (make-lbox :x box-x :y box-y :w width :h box-h :style cs :node node
                              :kind :block :children (nreverse children))))
          (return-from %layout-core (values lb (+ mt box-h mb) mt mb))))
      ;; flex / table containers
      ;; the atomic-inline variants (inline-flex/-grid/-table) lay their contents
      ;; out with the same internal algorithm as the block-level form; only their
      ;; outer participation (a shrink-to-fit box on a line) differs, handled above.
      (when (member (cdisplay cs) '("flex" "inline-flex" "table" "inline-table"
                                    "grid" "inline-grid") :test #'string=)
        (multiple-value-bind (boxes ch)
            (cond ((member (cdisplay cs) '("flex" "inline-flex") :test #'string=)
                   (layout-flex node styles cx cy content-w cs (or child-avail-h ar-h)))
                  ((member (cdisplay cs) '("grid" "inline-grid") :test #'string=)
                   (layout-grid node styles cx cy content-w cs (or child-avail-h ar-h)))
                  (t (layout-table node styles cx cy content-w cs
                                   ;; grid-box height to grow rows into: a definite/min
                                   ;; height less the wrapper padding+border (table
                                   ;; heights size the border box), §17.5.3.
                                   (let ((th (cond ((and (numberp min-h) (> min-h 0)) min-h)
                                                   ((numberp exp-h) exp-h) (t nil))))
                                     (and th (max 0 (- th pt pb bt bb)))))))
          (let* ((box-h (let ((bh (+ (cond
                                       ;; a flex/grid container with a definite height USES
                                       ;; it — its items may overflow (an 80+100px column in
                                       ;; a 100px box stays 100, clipping) rather than
                                       ;; stretching to content.  A table's height is only a
                                       ;; minimum, so it keeps growing to its content.
                                       ((and child-avail-h
                                             (member (cdisplay cs) '("flex" "inline-flex" "grid" "inline-grid")
                                                     :test #'string=))
                                        child-avail-h)
                                       (ar-h (max ar-h ch))
                                       (t ch))
                                     pt pb bt bb)))
                          ;; min/max-height clamp the box (CSS 2.1 §10.7): max first then
                          ;; min, so min-height wins — a table with min-height:312px and a
                          ;; 5px cell is still 312px.  For a table these size the BORDER
                          ;; box (the padding + border are on the table wrapper box, so
                          ;; min-height:312 targets 312 total); flex/grid size the content
                          ;; box like a block, so add the padding + border on.
                          (let ((tbl (member (cdisplay cs) '("table" "inline-table") :test #'string=)))
                            ;; a table's explicit height is a *minimum* on its border box
                            ;; (css-tables-3 §computing-the-table-height): an empty table
                            ;; with height:100px is 100 tall even with no rows to grow.
                            (when (and tbl (numberp exp-h)) (setf bh (max bh exp-h)))
                            (when (numberp max-h) (setf bh (min bh (if tbl max-h (+ max-h pt pb bt bb)))))
                            (when (and (numberp min-h) (> min-h 0))
                              (setf bh (max bh (if tbl min-h (+ min-h pt pb bt bb))))))
                          bh))
                 (lb (make-lbox :x box-x :y box-y :w width :h box-h :style cs :node node
                                :kind :block :children boxes)))
            (return-from %layout-core (values lb (+ mt box-h mb) mt mb)))))
      ;; multi-column container: fragment the in-flow content into columns
      ;; (css3-multicol).  Gated on *MULTICOL-MEASURING* so the single-column
      ;; measuring pass inside LAYOUT-MULTICOL lays the content out normally.
      (when (and (not *multicol-measuring*) (multicol-p cs)
                 (not (multicol-l2-p cs))
                 (not (multicol-ancestor-p node styles))
                 (not (multicol-unsupported-descendant-p node styles)))
        (multiple-value-bind (boxes ch)
            (layout-multicol node styles box-x box-y cx cy content-w cs (or child-avail-h ar-h))
          (let* ((box-h (+ (if (numberp exp-h) exp-h (if ar-h (max ar-h ch) ch)) pt pb bt bb))
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
      (let ((kids (flatten-contents
                   (multiple-value-bind (before after) (pseudo-kids node styles)
                     (append (when before (list before))
                             (coerce (h:dnode-children node) 'list)
                             (when after (list after))))
                   styles))
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
                   (multiple-value-bind (words hoisted-floats hoisted-blocks)
                       (collect-words (nreverse group) styles cs content-w)
                     ;; place floats hoisted out of the inline run first, at the flow
                     ;; position the run starts from (after the pending margin), so the
                     ;; lines flow around them (CSS 2.1 §9.5) — and an all-float run
                     ;; leaves no phantom line box behind.
                     (dolist (fn hoisted-floats)
                       (let* ((pend (if prev-mb (+ (max 0 (car prev-mb)) (min 0 (cdr prev-mb))) 0))
                              (lb (place-float fn styles cx (+ cx content-w) (+ yy pend) content-w)))
                         (when lb (push lb children))))
                     ;; block-level descendants hoisted out of the run break the inline
                     ;; formatting context (block-in-inline, CSS 2.1 §9.2.1.1) and lay
                     ;; out as normal blocks at the current flow position.
                     (dolist (bn hoisted-blocks)
                       (when prev-mb
                         (let ((m (+ (max 0 (car prev-mb)) (min 0 (cdr prev-mb)))))
                           (incf yy m) (incf content-h m)))
                       (setf prev-mb nil)
                       (multiple-value-bind (lb adv) (layout-node bn styles cx yy content-w child-avail-h)
                         (when lb
                           (push lb children)
                           (incf yy adv) (incf content-h adv)
                           (setf content-started t))))
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
                 ;; The static point is where the next in-flow box would go, so it
                 ;; sits AFTER the preceding block's pending (collapsed) bottom
                 ;; margin — held in PREV-MB, exactly as a float is placed above.
                 ;; PREV-MB is preserved (an out-of-flow box does not consume it).
                 (multiple-value-bind (lb adv)
                     (layout-node k styles cx
                                  (+ yy (if prev-mb (+ (max 0 (car prev-mb)) (min 0 (cdr prev-mb))) 0))
                                  content-w child-avail-h)
                   (declare (ignore adv))
                   (when lb
                     (if (string= pos "fixed")
                         (push (list lb k kcs) *fixed-pending*)
                         (push (list lb k kcs) *abs-pending*))
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
                                ;; The root element (<html>) is a barrier: its first
                                ;; child's (<body>'s) top margin applies inside it and
                                ;; is not bubbled to the viewport, so <body> lands at
                                ;; the root's content top plus its own margin (how
                                ;; browsers place the body — Acid3's body{margin:-0.2em}).
                                (top-collapse (and (not content-started)
                                                   (zerop bt) (zerop pt)
                                                   (not (establishes-bfc-p cs))  ; a BFC seals its top margin
                                                   (let ((p (h:dnode-parent node)))
                                                     (and p (eq (h:dnode-kind p) :element)))))
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
              (if (and height-auto (zerop bb) (zerop pb) (not (establishes-bfc-p cs)))
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
      (let* ((content-final (cond ((numberp exp-h)
                                   (let ((eh (if border-box (- exp-h pad-bord) exp-h)))
                                     ;; a table cell's `height` is only a MINIMUM (CSS 2.1
                                     ;; 17.5.3): it always grows to fit its content, so a
                                     ;; short height:10px cell (HN's top-bar nav) still
                                     ;; expands when its links wrap to two lines.
                                     (if (string= (cdisplay cs) "table-cell") (max eh content-h) eh)))
                                  ;; aspect-ratio with an auto height: derive the content
                                  ;; height from the (definite) width via the ratio, when
                                  ;; the box has no in-flow content taller than that
                                  ;; (content still wins if it would overflow — the ratio
                                  ;; is a preferred, not maximum, size).
                                  ((and ar (null used-h)) (max ar-h content-h))
                                  (t (max 0 content-h))))
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
             ;; align-content on a block distributes block-axis free space to its
             ;; content as one alignment subject (CSS Box Alignment 3 §5.3): shift
             ;; the whole content down by the aligned offset.
             (%ac (let ((ac (block-align-content cs)) (free (- content-final content-h)))
                    (when (and ac (> free 0.5))
                      (let ((dy (cond ((member ac '("center" "space-around" "space-evenly") :test #'string=) (/ free 2))
                                      ((member ac '("end" "flex-end") :test #'string=) free)
                                      (t 0))))
                        (when (> dy 0.5)
                          (dolist (c children) (when (typep c 'lbox) (shift-box c 0 (round dy)))))))))
             (lb (progn %ac (make-lbox :x box-x :y box-y :w width :h box-h
                            :style cs :node node :kind :block :children (nreverse children)
                            :marker (when list-item (css:cstyle-list-style cs))))))
        (values lb (+ mt (lbox-h lb) mb) mt-eff mb-eff box-mneg))))))

(defun shift-box (lb dx dy)
  "Recursively offset LB and its descendants by (DX,DY)."
  (when lb
    (incf (lbox-x lb) dx) (incf (lbox-y lb) dy)
    (if (eq (lbox-kind lb) :line)
        (dolist (it (lbox-children lb))
          (if (frag-p it) (incf (frag-x it) dx) (shift-box it dx dy)))
        (dolist (c (lbox-children lb)) (shift-box c dx dy)))))


;;; ---- CSS transforms ----------------------------------------------------
;;; A transform is applied after layout as a visual/geometry effect (it does not
;;; change the flow of siblings, CSS Transforms 1 §3).  Pure translations shift the
;;; box and its whole subtree (text frags included) exactly; scale/rotate/matrix
;;; replace the box with the axis-aligned bounds of its transformed corners — the
;;; rectangle getBoundingClientRect reports, which the audit harness measures.
(defun tf-num (s) (or (ignore-errors (float (read-from-string (string-trim '(#\Space) s)) 1.0)) 0.0))
(defun tf-len (s fs ref)
  "A transform length argument -> px; a percentage resolves against REF (a box side)."
  (let* ((s (string-trim '(#\Space) s)) (n (length s)))
    (flet ((pre (k) (or (ignore-errors (float (read-from-string (subseq s 0 (- n k))) 1.0)) 0.0)))
      (cond ((zerop n) 0.0)
            ((char= (char s (1- n)) #\%) (* (/ (pre 1) 100.0) ref))
            ((and (>= n 3) (string-equal (subseq s (- n 2)) "px")) (pre 2))
            ((and (>= n 3) (string-equal (subseq s (- n 2)) "em")) (* (pre 2) fs))
            ((and (>= n 4) (string-equal (subseq s (- n 3)) "rem")) (* (pre 3) 16.0))
            (t (tf-num s))))))
(defun tf-angle (s)
  "A transform angle argument -> radians."
  (let* ((s (string-downcase (string-trim '(#\Space) s))) (n (length s)))
    (flet ((pre (k) (or (ignore-errors (float (read-from-string (subseq s 0 (- n k))) 1.0)) 0.0)))
      (cond ((zerop n) 0.0)
            ((and (>= n 4) (string-equal (subseq s (- n 3)) "deg")) (* (pre 3) (/ pi 180.0)))
            ((and (>= n 4) (string-equal (subseq s (- n 3)) "rad")) (pre 3))
            ((and (>= n 5) (string-equal (subseq s (- n 4)) "grad")) (* (pre 4) (/ pi 200.0)))
            ((and (>= n 5) (string-equal (subseq s (- n 4)) "turn")) (* (pre 4) (* 2 pi)))
            (t (* (tf-num s) (/ pi 180.0)))))))
(defun mat* (m1 m2)
  "Compose two affine matrices (a b c d e f): (MAT* M1 M2) applies M2 then M1."
  (destructuring-bind (a1 b1 c1 d1 e1 f1) m1
    (destructuring-bind (a2 b2 c2 d2 e2 f2) m2
      (list (+ (* a1 a2) (* c1 b2)) (+ (* b1 a2) (* d1 b2))
            (+ (* a1 c2) (* c1 d2)) (+ (* b1 c2) (* d1 d2))
            (+ (* a1 e2) (* c1 f2) e1) (+ (* b1 e2) (* d1 f2) f1)))))
(defun mat-pt (m x y)
  (destructuring-bind (a b c d e f) m
    (values (+ (* a x) (* c y) e) (+ (* b x) (* d y) f))))
(defun tf-rotate3d (args)
  "The orthographic 2D projection of rotate3d(x,y,z,angle): the top-left 2x2 of the
3x3 Rodrigues rotation matrix (a point in the z=0 plane, projected by dropping z)."
  (if (< (length args) 4)
      '(1.0 0.0 0.0 1.0 0.0 0.0)
      (let* ((ax (tf-num (first args))) (ay (tf-num (second args))) (az (tf-num (third args)))
             (th (tf-angle (fourth args)))
             (len (sqrt (+ (* ax ax) (* ay ay) (* az az)))))
        (if (zerop len)
            '(1.0 0.0 0.0 1.0 0.0 0.0)
            (let* ((x (/ ax len)) (y (/ ay len)) (z (/ az len))
                   (c (cos th)) (s (sin th)) (c1 (- 1.0 c))
                   (r00 (+ c (* x x c1)))        (r01 (- (* x y c1) (* z s)))
                   (r10 (+ (* y x c1) (* z s)))  (r11 (+ c (* y y c1))))
              ;; 2D affine (a b c d e f): a=r00 b=r10 c=r01 d=r11
              (list r00 r10 r01 r11 0.0 0.0))))))
(defun tf-fn-matrix (fn args fs bw bh)
  "The affine matrix for one transform function around the local origin."
  (flet ((len (s ref) (tf-len s fs ref)) (num (s) (tf-num s)) (ang (s) (tf-angle s)))
    (cond
      ((string= fn "translate") (list 1.0 0.0 0.0 1.0 (len (first args) bw) (if (second args) (len (second args) bh) 0.0)))
      ((string= fn "translatex") (list 1.0 0.0 0.0 1.0 (len (first args) bw) 0.0))
      ((string= fn "translatey") (list 1.0 0.0 0.0 1.0 0.0 (len (first args) bh)))
      ((string= fn "scale") (let ((sx (num (first args)))) (list sx 0.0 0.0 (if (second args) (num (second args)) sx) 0.0 0.0)))
      ((string= fn "scalex") (list (num (first args)) 0.0 0.0 1.0 0.0 0.0))
      ((string= fn "scaley") (list 1.0 0.0 0.0 (num (first args)) 0.0 0.0))
      ((string= fn "rotate") (let* ((r (ang (first args))) (c (cos r)) (s (sin r))) (list c s (- s) c 0.0 0.0)))
      ((string= fn "skewx") (list 1.0 0.0 (tan (ang (first args))) 1.0 0.0 0.0))
      ((string= fn "skewy") (list 1.0 (tan (ang (first args))) 0.0 1.0 0.0 0.0))
      ((string= fn "skew") (list 1.0 (if (second args) (tan (ang (second args))) 0.0)
                                 (tan (ang (first args))) 1.0 0.0 0.0))
      ((string= fn "matrix") (let ((a (mapcar #'num args))) (if (= (length a) 6) a '(1.0 0.0 0.0 1.0 0.0 0.0))))
      ;; 3D transforms projected orthographically (no perspective): rotateX/Y
      ;; foreshorten one axis by cos θ, rotateZ is the in-plane 2D rotation, and a
      ;; z-translation / scale has no 2D projection.  This matches getBoundingClientRect
      ;; whenever no perspective is in effect (CSS Transforms 2 §orthographic).
      ((string= fn "rotatez") (let* ((r (ang (first args))) (c (cos r)) (s (sin r))) (list c s (- s) c 0.0 0.0)))
      ((string= fn "rotatex") (list 1.0 0.0 0.0 (cos (ang (first args))) 0.0 0.0))
      ((string= fn "rotatey") (list (cos (ang (first args))) 0.0 0.0 1.0 0.0 0.0))
      ((string= fn "rotate3d") (tf-rotate3d args))
      ((string= fn "translatez") '(1.0 0.0 0.0 1.0 0.0 0.0))
      ((string= fn "translate3d") (list 1.0 0.0 0.0 1.0 (len (first args) bw) (if (second args) (len (second args) bh) 0.0)))
      ((string= fn "scale3d") (list (num (first args)) 0.0 0.0 (if (second args) (num (second args)) 1.0) 0.0 0.0))
      ((string= fn "perspective") '(1.0 0.0 0.0 1.0 0.0 0.0))   ; orthographic approximation
      ((string= fn "matrix3d")                                   ; column-major 4x4 -> 2D projection
       (let ((a (mapcar #'num args)))
         (if (= (length a) 16)
             (list (nth 0 a) (nth 1 a) (nth 4 a) (nth 5 a) (nth 12 a) (nth 13 a))
             '(1.0 0.0 0.0 1.0 0.0 0.0))))
      (t '(1.0 0.0 0.0 1.0 0.0 0.0)))))          ; unknown function: identity
(defun tf-origin-xy (origin fs bw bh)
  "Resolve TRANSFORM-ORIGIN tokens to (values ox oy) offsets within a BW x BH box
(default 50% 50%)."
  (flet ((axis (tok ref)
           (cond ((null tok) (* 0.5 ref))
                 ((member tok '("left" "top") :test #'string=) 0.0)
                 ((string= tok "center") (* 0.5 ref))
                 ((member tok '("right" "bottom") :test #'string=) (float ref 1.0))
                 (t (tf-len tok fs ref)))))
    (values (axis (first origin) bw) (axis (second origin) bh))))
(defun compound-3d-p (tl)
  "True when TL composes two or more out-of-plane 3D rotations (rotateX/Y, or a
rotate3d off the Z axis).  Projecting each orthographically and multiplying the 2D
results is only valid for a single such rotation; a compound one needs a full 3D
matrix, so the flow declines to project it (falls back to the untransformed box)."
  (>= (count-if (lambda (f)
                  (let ((fn (first f)))
                    (or (member fn '("rotatex" "rotatey") :test #'string=)
                        (and (string= fn "rotate3d")
                             ;; off-Z axis (some x or y component)
                             (or (/= 0 (tf-num (or (first (rest f)) "0")))
                                 (/= 0 (tf-num (or (second (rest f)) "0"))))))))
                tl)
      2))
(defun box-transform-matrix (cs box-x box-y bw bh)
  "The absolute affine matrix for CS's transform on a border box at (BOX-X,BOX-Y) of
size BW x BH, taken around its transform-origin — or NIL when there is no transform."
  (let ((tl (css:cstyle-transform cs)))
    (when (and tl (not (equal tl '("none"))) (not (compound-3d-p tl)))
      (let ((fs (css:cstyle-font-size cs)))
        (multiple-value-bind (ox oy) (tf-origin-xy (css:cstyle-transform-origin cs) fs bw bh)
          (let* ((oax (+ box-x ox)) (oay (+ box-y oy))
                 (mc (reduce (lambda (acc f) (mat* acc (tf-fn-matrix (first f) (rest f) fs bw bh)))
                             tl :initial-value '(1.0 0.0 0.0 1.0 0.0 0.0))))
            (mat* (mat* (list 1.0 0.0 0.0 1.0 oax oay) mc)
                  (list 1.0 0.0 0.0 1.0 (- oax) (- oay)))))))))
(defun tf-aabb (m x y w h)
  "Axis-aligned bounds (values x y w h) of the box (X,Y,W,H) transformed by M."
  (let ((xs '()) (ys '()))
    (dolist (p (list (cons x y) (cons (+ x w) y) (cons x (+ y h)) (cons (+ x w) (+ y h))))
      (multiple-value-bind (px py) (mat-pt m (car p) (cdr p)) (push px xs) (push py ys)))
    (let ((minx (reduce #'min xs)) (maxx (reduce #'max xs))
          (miny (reduce #'min ys)) (maxy (reduce #'max ys)))
      (values minx miny (- maxx minx) (- maxy miny)))))
(defun tf-pure-translate (tl fs bw bh)
  "If every function in TL is a translation, return (values tx ty); else NIL."
  (let ((tx 0.0) (ty 0.0))
    (dolist (f tl (values tx ty))
      (let ((fn (first f)) (a (rest f)))
        (cond ((string= fn "translate") (incf tx (tf-len (first a) fs bw))
                                        (when (second a) (incf ty (tf-len (second a) fs bh))))
              ((string= fn "translatex") (incf tx (tf-len (first a) fs bw)))
              ((string= fn "translatey") (incf ty (tf-len (first a) fs bh)))
              ((string= fn "translate3d") (incf tx (tf-len (first a) fs bw))    ; z has no 2D effect
                                          (when (second a) (incf ty (tf-len (second a) fs bh))))
              ((string= fn "translatez"))                                        ; no 2D effect
              (t (return nil)))))))
(defun apply-transforms (box)
  "Post-layout pass: apply each element's CSS transform to its box.  Children are
processed first so an ancestor's translation shifts an already-transformed subtree
(effective = ancestor . descendant, CSS Transforms 1 §3)."
  (when (typep box 'lbox)
    (dolist (c (lbox-children box)) (when (typep c 'lbox) (apply-transforms c)))
    (let ((cs (lbox-style box)))
      (when (and cs (css:cstyle-transform cs) (not (equal (css:cstyle-transform cs) '("none"))))
        (let* ((tl (css:cstyle-transform cs)) (fs (css:cstyle-font-size cs))
               (bw (lbox-w box)) (bh (lbox-h box)))
          (multiple-value-bind (tx ty) (tf-pure-translate tl fs bw bh)
            (if tx
                (shift-box box (round tx) (round ty))       ; exact; moves text frags too
                (let ((m (box-transform-matrix cs (lbox-x box) (lbox-y box) bw bh)))
                  (when m
                    (multiple-value-bind (nx ny nw nh) (tf-aabb m (lbox-x box) (lbox-y box) bw bh)
                      (setf (lbox-x box) (round nx) (lbox-y box) (round ny)
                            (lbox-w box) (round nw) (lbox-h box) (round nh))))))))))))

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

(defun multicol-used-count (base-cs content-w gap)
  "Used number of columns (css3-multicol §3.4, simplified) for content width
CONTENT-W: from column-count, or derived from column-width, or their combination."
  (let ((cc (css:cstyle-column-count base-cs))
        (cw (and (css:cstyle-column-width base-cs) (float (css:cstyle-column-width base-cs) 1.0))))
    (cond ((and cc cw) (max 1 (min cc (floor (+ content-w gap) (+ (max 1.0 cw) gap)))))
          (cc (max 1 cc))
          (cw (max 1 (floor (+ content-w gap) (+ (max 1.0 cw) gap))))
          (t 1))))

(defun layout-multicol (node styles box-x box-y cx cy content-w base-cs avail-h)
  "Fragment NODE's in-flow content into equal-width columns (css3-multicol).  The
content is first laid out in one column of the used column width — reusing the
normal block flow, so text breaks into line boxes and blocks keep their margins —
then those boxes are distributed left-to-right across the used number of columns,
balanced to roughly equal height (column-fill: balance; auto fills each column to
the content before moving on).  Returns (values column-boxes content-height)."
  (let* ((gap (max 0.0 (float (css:cstyle-column-gap base-cs) 1.0)))
         (k (multicol-used-count base-cs content-w gap))
         (colw (max 0.0 (/ (- content-w (* (1- k) gap)) k))))
    (if (<= k 1)
        (multiple-value-bind (lb ch) (let ((*multicol-measuring* t))
                                       (%layout-core node styles box-x box-y content-w avail-h))
          (values (and lb (copy-list (lbox-children lb))) (max 0.0 (float ch 1.0))))
        (multiple-value-bind (lb ch)
            (let ((*multicol-measuring* t)) (%layout-core node styles box-x box-y colw avail-h))
          (declare (ignore ch))
          (let ((kids (and lb (remove-if-not (lambda (c) (typep c 'lbox)) (lbox-children lb)))))
            (if (null kids)
                (values nil 0.0)
                (let* ((total (- (reduce #'max kids :key (lambda (c) (+ (lbox-y c) (lbox-h c))))
                                 cy))
                       (balance (string= (css:cstyle-column-fill base-cs) "balance"))
                       (target (if (and balance (> total 0)) (/ total k) most-positive-single-float))
                       (col 0) (col-top (lbox-y (first kids))) (maxh 0.0))
                  (dolist (c kids)
                    (let ((c-bot (+ (lbox-y c) (lbox-h c))))
                      (when (and (< col (1- k))
                                 (> (lbox-y c) col-top)
                                 (> (- c-bot col-top) target))
                        (incf col) (setf col-top (lbox-y c)))
                      (shift-box c (* col (+ colw gap)) (- cy col-top))
                      (setf maxh (max maxh (- (+ (lbox-y c) (lbox-h c)) cy)))))
                  (values kids maxh))))))))

(defun anon-flex-item (kids ref styles)
  "A synthetic block-level flex item wrapping a contiguous run of text/inline content
KIDS that sits directly inside a flex container (CSS Flexbox §4).  Inheritable style
is taken from the container REF so the text renders in the right font/colour; the box
itself generates no margin/border/padding/background and its inner display is block, so
the run lays out in a normal inline formatting context.  Flex properties are reset to
their initial values (grow 0, shrink 1, basis auto, order 0, align-self auto)."
  (let ((cs (let ((c (css::copy-cstyle (or (st styles ref) (css::make-cstyle)))))
              (setf (css:cstyle-display c) "block"
                    (css:cstyle-width c) :auto (css:cstyle-height c) :auto
                    (css:cstyle-float c) "none" (css:cstyle-position c) "static"
                    (css:cstyle-flex-grow c) 0.0 (css:cstyle-flex-shrink c) 1.0
                    (css:cstyle-flex-basis c) "auto" (css:cstyle-order c) 0
                    (css:cstyle-align-self c) "auto" (css:cstyle-content c) nil
                    (css:cstyle-background c) nil (css:cstyle-bg-image c) nil (css:cstyle-bg-gradient c) nil
                    (css:cstyle-margin-top c) 0.0 (css:cstyle-margin-right c) 0.0
                    (css:cstyle-margin-bottom c) 0.0 (css:cstyle-margin-left c) 0.0
                    (css:cstyle-padding-top c) 0.0 (css:cstyle-padding-right c) 0.0
                    (css:cstyle-padding-bottom c) 0.0 (css:cstyle-padding-left c) 0.0
                    (css:cstyle-border-top-width c) 0.0 (css:cstyle-border-right-width c) 0.0
                    (css:cstyle-border-bottom-width c) 0.0 (css:cstyle-border-left-width c) 0.0)
              c))
        (v (make-array (length kids) :adjustable t :fill-pointer 0)))
    (dolist (k kids) (vector-push-extend k v))
    (let ((el (h::%dnode :kind :element :name "flexitem" :children v)))
      (setf (gethash el styles) cs)
      el)))

(defun flex-item-nodes (node styles)
  "The flex items NODE lays out (CSS Flexbox §4): each in-flow child element, plus an
anonymous flex item wrapping each contiguous run of text directly contained in the
container.  A run that is entirely collapsible white space generates no item."
  (let ((kids (flatten-contents (coerce (h:dnode-children node) 'list) styles))
        (items '()) (run '()))
    (flet ((flush ()
             (when run
               (let ((r (nreverse run)))
                 (unless (every #'ws-only-text-p r)
                   (push (anon-flex-item r node styles) items)))
               (setf run '()))))
      (dolist (k kids)
        (cond ((eq (h:dnode-kind k) :element)
               ;; a display:none element generates no box and is not laid out; it must
               ;; NOT break a contiguous text run (CSS Flexbox §4), else the space around
               ;; it would be lost — "a <span style=display:none></span>b" is one item.
               (let ((cs (st styles k)))
                 (if (and cs (string= (css:cstyle-display cs) "none"))
                     nil
                     (progn (flush) (push k items)))))
              ((eq (h:dnode-kind k) :text) (push k run))))
      (flush))
    (nreverse items)))

(defun layout-flex (node styles cx cy content-w base-cs &optional avail-h)
  "Single-line flexbox layout.  Returns (values child-lboxes content-height).  AVAIL-H
is the container's definite content height (px) when known — the main-axis size a
column distributes grow/shrink into; NIL means auto (size to content)."
  (let* ((dir (css:cstyle-flex-direction base-cs))
         (row (not (or (string= dir "column") (string= dir "column-reverse"))))
         (justify (css:cstyle-justify-content base-cs))
         (align (css:cstyle-align-items base-cs))
         (gap (css:cstyle-gap base-cs))
         (items (remove-if-not (lambda (k) (let ((c (st styles k))) (and c (not (string= (css:cstyle-display c) "none"))))) (flex-item-nodes node styles)))
         ;; `order` reorders items (CSS 5.4); a stable sort keeps DOM order among ties.
         (items (stable-sort (copy-list items) #'<
                             :key (lambda (it) (let ((c (st styles it))) (if c (css:cstyle-order c) 0)))))
         ;; *-reverse lays the items out in reverse order along the main axis.
         (items (if (member dir '("row-reverse" "column-reverse") :test #'string=) (reverse items) items))
         (nitems (length items)))
    (when (zerop nitems) (return-from layout-flex (values nil 0)))
    (let* ((bases (mapcar (lambda (it) (item-base it styles content-w)) items))
           (total-gap (* gap (1- nitems)))
           (grows (mapcar (lambda (it) (css:cstyle-flex-grow (st styles it))) items))
           (sum-grow (reduce #'+ grows))
           ;; flex-shrink is weighted by shrink-factor * base-size (CSS 9.7): an item
           ;; with a larger base gives up proportionally more of the negative free space.
           (shrinks (mapcar (lambda (it) (css:cstyle-flex-shrink (st styles it))) items)))
      (if row
          ;; ---- ROW ----
          ;; Lay out one flex line of items (LITEMS with parallel base/grow/shrink
          ;; lists) at cross-offset LY: distribute the line's free space by grow (if
          ;; positive) or shrink*base (if negative, CSS 9.7), position left-to-right
          ;; per justify-content, then cross-align to the line's own max height.
          ;; Returns (values line-boxes line-height).
          (flet ((layout-line (litems lbases lgrows lshrinks ly &optional forced-cross)
                   (let* ((n (length litems))
                          (lgap (* gap (max 0 (1- n))))
                          ;; Each item's non-auto main-axis (left+right) margins count
                          ;; toward the hypothetical main size, so free space and
                          ;; justify-content spacing are computed on the item OUTER
                          ;; sizes (CSS Flexbox §9.2/§9.7).  Auto margins are absorbed
                          ;; separately below and contribute 0 here.
                          (lmargins (mapcar (lambda (it)
                                              (let ((is (st styles it)))
                                                (if (null is) 0
                                                    (+ (if (css:cstyle-margin-left-auto is) 0 (css:cstyle-margin-left is))
                                                       (if (css:cstyle-margin-right-auto is) 0 (css:cstyle-margin-right is))))))
                                            litems))
                          (lmargin-sum (reduce #'+ lmargins))
                          ;; LBASES are content-box (flex base) sizes; an item's OUTER
                          ;; main size also includes its main-axis padding+border (CSS
                          ;; Flexbox §9.2/§9.5).  Reserve that here so free space and
                          ;; justify-content spacing don't overrun by each item's padding.
                          (lpbm (mapcar (lambda (it)
                                          (let ((is (st styles it)))
                                            (if (null is) 0
                                                (+ (css::resolve-pad (css:cstyle-padding-left is) content-w)
                                                   (css::resolve-pad (css:cstyle-padding-right is) content-w)
                                                   (used-border is :l) (used-border is :r)))))
                                        litems))
                          (lpbm-sum (reduce #'+ lpbm))
                          (lfree (- content-w (+ (reduce #'+ lbases) lpbm-sum lgap lmargin-sum)))
                          (lsum-grow (reduce #'+ lgrows))
                          (scaled (mapcar #'* lshrinks lbases))
                          (sum-scaled (reduce #'+ scaled))
                          (sizes0 (cond ((and (> lfree 0) (> lsum-grow 0))
                                         (mapcar (lambda (b g) (+ b (* lfree (/ g lsum-grow)))) lbases lgrows))
                                        ((and (< lfree 0) (> sum-scaled 0))
                                         (mapcar (lambda (b sc) (max 0 (+ b (* lfree (/ sc sum-scaled))))) lbases scaled))
                                        (t lbases)))
                          ;; clamp each flexed size to the item's min/max-width (CSS
                          ;; Flexbox §9.7 step 4): freed/absorbed space then falls to
                          ;; justify-content.  Only explicit lengths/percentages clamp;
                          ;; auto/none/min-content are left to the base pass.
                          (sizes (mapcar (lambda (sz it)
                                           (let* ((is (st styles it))
                                                  (mx (and is (css::resolve-size (css:cstyle-max-width is) content-w)))
                                                  (mn (and is (css::resolve-size (css:cstyle-min-width is) content-w)))
                                                  (v sz))
                                             (when (and (numberp mx) (> v mx)) (setf v mx))
                                             (when (and (numberp mn) (< v mn)) (setf v mn))
                                             v))
                                         sizes0 litems))
                          (used (+ (reduce #'+ sizes) lpbm-sum lgap lmargin-sum))
                          (extra (max 0 (- content-w used)))
                          ;; auto margins on the main axis absorb positive free space
                          ;; BEFORE justify-content (CSS Flexbox §8.1): each takes an
                          ;; equal share, and justify-content then has nothing to do.
                          (n-mauto (loop for it in litems
                                         for is = (st styles it)
                                         sum (+ (if (and is (css:cstyle-margin-left-auto is)) 1 0)
                                                (if (and is (css:cstyle-margin-right-auto is)) 1 0))))
                          (mauto (and (> n-mauto 0) (> extra 0)))
                          (auto-unit (if mauto (/ extra n-mauto) 0))
                          (start (cond (mauto cx)
                                       ((string= justify "center") (+ cx (/ extra 2)))
                                       ((string= justify "flex-end") (+ cx extra)) (t cx)))
                          (between (cond (mauto 0)
                                         ((and (string= justify "space-between") (> n 1)) (/ extra (1- n)))
                                         ((string= justify "space-around") (/ extra n)) (t 0)))
                          (x (if (and (not mauto) (string= justify "space-around")) (+ start (/ between 2)) start))
                          (boxes '()) (max-h 0))
                     (loop for it in litems for w in sizes for pbm in lpbm
                           for is = (st styles it)
                           ;; fixed leading/trailing main-axis margins (0 when auto)
                           for ml = (if (and is (not (css:cstyle-margin-left-auto is))) (css:cstyle-margin-left is) 0)
                           for mr = (if (and is (not (css:cstyle-margin-right-auto is))) (css:cstyle-margin-right is) 0)
                           do
                       (when (and mauto is (css:cstyle-margin-left-auto is)) (incf x auto-unit))
                       (multiple-value-bind (lb adv)
                           (let* ((is (st styles it))
                                  ;; a definite-height row container (FORCED-CROSS) stretches an
                                  ;; auto-height item to fill it; give the item that definite
                                  ;; content-box height up front so its `height:100%` / ratio
                                  ;; children resolve (CSS 9.4 stretch then re-resolve).
                                  (a (let ((as (and is (css:cstyle-align-self is))))
                                       (if (and as (not (string= as "auto"))) as align)))
                                  (*flex-item-height*
                                   (when (and (numberp forced-cross) is (string= a "stretch")
                                              (null (css:cstyle-height is)))
                                     (max 0 (- forced-cross
                                               (max 0 (css:cstyle-margin-top is)) (max 0 (css:cstyle-margin-bottom is))
                                               (css::resolve-pad (css:cstyle-padding-top is) nil) (css::resolve-pad (css:cstyle-padding-bottom is) nil)
                                               (used-border is :t) (used-border is :b)))))
                                  (*flex-main-size* (round w)))   ; the item uses this width, not its own
                             (layout-node it styles (round x) ly (round w)))
                         (declare (ignore adv))
                         ;; the line's cross size is the largest item OUTER cross size
                         ;; (CSS Flexbox §9.4): include each item's cross-axis (top/bottom)
                         ;; margins so wrapped lines stack clear of them.  The border box
                         ;; already sits margin-top below the line top (block flow), so its
                         ;; outer extent is margin-top + border height + margin-bottom.
                         (when lb
                           (push lb boxes)
                           (let ((cmt (if (css:cstyle-margin-top-auto is) 0 (max 0 (css:cstyle-margin-top is))))
                                 (cmb (if (css:cstyle-margin-bottom-auto is) 0 (max 0 (css:cstyle-margin-bottom is)))))
                             (setf max-h (max max-h (+ cmt (lbox-h lb) cmb)))))
                         ;; layout-node already positioned the border box ML past X;
                         ;; advance past the whole margin box so the next item clears
                         ;; both this item's trailing margin and its own leading one.
                         ;; W is the content-box width, so add the item's main-axis
                         ;; padding+border (PBM) to clear its full border box.
                         (incf x (+ ml w pbm mr))
                         (when (and mauto is (css:cstyle-margin-right-auto is)) (incf x auto-unit))
                         (incf x (+ gap between))))
                     (let ((boxes (nreverse boxes))
                           ;; the line's cross size: a single-line container with a
                           ;; definite height fills it (items stretch to the container,
                           ;; not just to the tallest item); a wrapped line uses its own
                           ;; content height (FORCED-CROSS nil).  Per-item ALIGN-SELF
                           ;; overrides the container's ALIGN-ITEMS.
                           (cross (if (numberp forced-cross) (max max-h forced-cross) max-h)))
                       (dolist (lb boxes)                 ; cross-axis align within the line
                         ;; The item box already sits MARGIN-TOP below the line top (block
                         ;; flow applied it), so align/stretch within CROSS minus the item's
                         ;; own cross margins (CSS 9.4/9.6): a stretched item fills the line
                         ;; less its margins rather than overflowing by them.
                         (let* ((s (lbox-style lb))
                                (a (let ((as (css:cstyle-align-self s)))
                                     (if (and as (not (string= as "auto"))) as align)))
                                (mta (css:cstyle-margin-top-auto s))
                                (mba (css:cstyle-margin-bottom-auto s))
                                (mt (max 0 (css:cstyle-margin-top s)))
                                (mb (max 0 (css:cstyle-margin-bottom s)))
                                (space (- cross mt mb)))
                           ;; cross-axis auto margins take the free space before align
                           ;; (CSS Flexbox §8.1): two autos center, one pushes to a side.
                           (cond ((and mta mba) (shift-box lb 0 (round (/ (- space (lbox-h lb)) 2))))
                                 (mta (shift-box lb 0 (round (- space (lbox-h lb)))))
                                 (mba)                                   ; margin-bottom auto: stay at top
                                 ((string= a "stretch") (setf (lbox-h lb) (max (lbox-h lb) space)))
                                 ((string= a "center") (shift-box lb 0 (round (/ (- space (lbox-h lb)) 2))))
                                 ((member a '("flex-end" "end") :test #'string=) (shift-box lb 0 (round (- space (lbox-h lb))))))))
                       (values boxes cross)))))
            (let ((wrap (css:cstyle-flex-wrap base-cs)))
              (if (and (stringp wrap) (member wrap '("wrap" "wrap-reverse") :test #'string=))
                  ;; ---- multi-line row (flex-wrap, CSS 9.3) ----
                  ;; Break items into lines greedily: keep adding while the running
                  ;; base+gaps fit CONTENT-W; an item that overflows starts a new line
                  ;; (a lone item wider than the container gets its own line).  Each
                  ;; line is then laid out and sized independently, and lines stack
                  ;; along the cross axis separated by the cross (row) gap.
                  ;; wrap-reverse is treated as wrap here (lines not reversed).
                  (let ((cross-gap (css:cstyle-row-gap base-cs))
                        (lines '()) (ci '()) (cb '()) (cg '()) (cs '()) (cw 0))
                    (loop for it in items for b in bases for g in grows for s in shrinks do
                      (let ((add (if ci (+ cw gap b) b)))
                        (when (and ci (> add content-w))
                          (push (list (nreverse ci) (nreverse cb) (nreverse cg) (nreverse cs)) lines)
                          (setf ci nil cb nil cg nil cs nil add b))
                        (push it ci) (push b cb) (push g cg) (push s cs) (setf cw add)))
                    (when ci (push (list (nreverse ci) (nreverse cb) (nreverse cg) (nreverse cs)) lines))
                    (setf lines (nreverse lines))
                    ;; row-reverse reverses only the MAIN axis: items were reversed for
                    ;; line-breaking, so the lines emerge in reverse cross order too.
                    ;; Flip the line list back so lines stack in flow order (first line
                    ;; on top), while each line keeps its reversed main-axis placement.
                    (when (string= dir "row-reverse")
                      (setf lines (nreverse lines)))
                    ;; lay each line out at CY, then position the lines along the cross
                    ;; axis per align-content: when the container is taller than the lines
                    ;; the surplus is distributed as leading/between per the keyword.
                    (let ((laid '()))
                      (dolist (line lines)
                        (destructuring-bind (litems lbases lgrows lshrinks) line
                          (multiple-value-bind (boxes line-h) (layout-line litems lbases lgrows lshrinks (round cy))
                            (push (cons boxes line-h) laid))))
                      (setf laid (nreverse laid))
                      ;; flex-wrap:wrap-reverse swaps the cross-start and cross-end
                      ;; directions (CSS Flexbox §5.3): the first flex line is placed at
                      ;; the cross-END, so stack the lines in reverse cross order.
                      (when (string= wrap "wrap-reverse") (setf laid (nreverse laid)))
                      (let* ((nlines (length laid))
                             (total (+ (reduce #'+ (mapcar #'cdr laid)) (* cross-gap (max 0 (1- nlines)))))
                             (ac (css:cstyle-align-content base-cs))
                             (extra (if (numberp avail-h) (max 0 (- avail-h total)) 0))
                             (lead (cond ((<= extra 0) 0)
                                         ((member ac '("flex-end" "end") :test #'string=) extra)
                                         ((string= ac "center") (/ extra 2))
                                         ((string= ac "space-around") (/ extra (* 2 nlines)))
                                         ((string= ac "space-evenly") (/ extra (1+ nlines)))
                                         (t 0)))
                             (between (cond ((<= extra 0) 0)
                                            ((and (string= ac "space-between") (> nlines 1)) (/ extra (1- nlines)))
                                            ((string= ac "space-around") (/ extra nlines))
                                            ((string= ac "space-evenly") (/ extra (1+ nlines)))
                                            (t 0)))
                             (all '()) (y (+ cy lead)))
                        (dolist (pair laid)
                          (destructuring-bind (boxes . line-h) pair
                            (let ((dy (round (- y cy))))
                              (unless (zerop dy) (dolist (b boxes) (shift-box b 0 dy))))
                            (setf all (nconc all boxes))
                            (incf y (+ line-h cross-gap between))))
                        (values all (max total (if (numberp avail-h) avail-h 0))))))
                  ;; ---- single-line row (nowrap) ----
                  ;; pass AVAIL-H so a definite-height container stretches its items to fill it
                  (layout-line items bases grows shrinks cy avail-h))))
          ;; ---- COLUMN ----
          ;; Lay each item out at its natural size to get its main-axis (height) base;
          ;; then, if the container has a definite height, distribute the free space by
          ;; flex-grow / flex-shrink (weighted by shrink*height) along the vertical axis.
          (let ((boxes '()) (heights '()) (vmargins '()) (max-w 0))
            (dolist (it items)
              ;; cross size (width): a non-stretch item with an auto cross size
              ;; shrink-wraps to fit-content (CSS Flexbox §7.5); stretch or a definite
              ;; width fills / uses its own.
              (let* ((is (st styles it))
                     (a (let ((as (and is (css:cstyle-align-self is))))
                          (if (and as (not (string= as "auto"))) as align)))
                     (cw (if (and is (member (css:cstyle-width is) '(nil :auto)) (not (string= a "stretch")))
                             (min content-w (max 0 (pref-border-width it styles content-w 0)))
                             content-w))
                     ;; a definite flex-basis is the item's main (height) base — it is
                     ;; NOT applied to the item's box by layout-node (which sees only
                     ;; flex-basis, not height), so an empty flex:0 0 80px item would
                     ;; otherwise report its 0 natural content height (CSS Flexbox §9.2).
                     (basis (and is (css:cstyle-flex-basis is)))
                     (def-basis (and (stringp basis)
                                     (not (member basis '("auto" "content") :test #'string=))
                                     (let ((v (css::resolve-len basis (css:cstyle-font-size is))))
                                       (and (numberp v) v))))
                     ;; fixed main-axis (top/bottom) margins (0 when auto — those are
                     ;; absorbed as free space below).
                     (mt (if (and is (not (css:cstyle-margin-top-auto is))) (css:cstyle-margin-top is) 0))
                     (mb (if (and is (not (css:cstyle-margin-bottom-auto is))) (css:cstyle-margin-bottom is) 0)))
                (multiple-value-bind (lb adv) (layout-node it styles cx cy (round cw))
                  (push lb boxes)
                  ;; ADV is the item's outer (margin-box) advance; its main base size is
                  ;; the border box, so strip the fixed margins back off (CSS Flexbox §9.2).
                  (push (if lb (or def-basis (max 0 (- adv mt mb))) 0) heights)
                  (push (+ mt mb) vmargins)
                  (when lb (setf max-w (max max-w (lbox-w lb)))))))
            (setf boxes (nreverse boxes) heights (nreverse heights) vmargins (nreverse vmargins))
            (let* ((vmargin-sum (reduce #'+ vmargins))
                   (base-sum (+ (reduce #'+ heights) total-gap vmargin-sum))
                   (hfree (if (numberp avail-h) (- avail-h base-sum) 0))
                   (hscaled (mapcar #'* shrinks heights))
                   (sum-hscaled (reduce #'+ hscaled))
                   (tgt (cond ((and (numberp avail-h) (> hfree 0) (> sum-grow 0))
                               (mapcar (lambda (h g) (+ h (* hfree (/ g sum-grow)))) heights grows))
                              ((and (numberp avail-h) (< hfree 0) (> sum-hscaled 0))
                               (mapcar (lambda (h sc) (max 0 (+ h (* hfree (/ sc sum-hscaled))))) heights hscaled))
                              (t heights)))
                   ;; main-axis (vertical) auto margins absorb positive free space
                   ;; before justify-content (CSS Flexbox §8.1): margin-top:auto pushes
                   ;; an item down, margin:auto centers it in the column.
                   (used (+ (reduce #'+ tgt) total-gap vmargin-sum))
                   (free (if (numberp avail-h) (max 0 (- avail-h used)) 0))
                   (n-mauto (loop for it in items for is = (st styles it)
                                  sum (+ (if (and is (css:cstyle-margin-top-auto is)) 1 0)
                                         (if (and is (css:cstyle-margin-bottom-auto is)) 1 0))))
                   (auto-unit (if (and (> n-mauto 0) (> free 0)) (/ free n-mauto) 0))
                   (y cy))
              (loop for lb in boxes for h in tgt for it in items
                    for is = (st styles it)
                    for mt = (if (and is (not (css:cstyle-margin-top-auto is))) (css:cstyle-margin-top is) 0)
                    for mb = (if (and is (not (css:cstyle-margin-bottom-auto is))) (css:cstyle-margin-bottom is) 0)
                    do
                (when (and (> auto-unit 0) is (css:cstyle-margin-top-auto is)) (incf y auto-unit))
                (when lb
                  (shift-box lb 0 (round (- (+ y mt) (lbox-y lb))))   ; border box sits MT below the pen
                  ;; reset to the content-left: the item was laid out across the full
                  ;; container width, so block-level margin:auto centering may already
                  ;; have shifted it — flex cross-alignment below is the authority.
                  (shift-box lb (round (- cx (lbox-x lb))) 0)
                  (setf (lbox-h lb) (round h))
                  ;; cross-axis (horizontal) alignment: auto margins first (CSS §8.1),
                  ;; then align-self / align-items.  stretch only grows an auto-width item.
                  (let* ((s (lbox-style lb))
                         (mla (css:cstyle-margin-left-auto s)) (mra (css:cstyle-margin-right-auto s))
                         (a (let ((as (css:cstyle-align-self s)))
                              (if (and as (not (string= as "auto"))) as align))))
                    (cond ((and mla mra) (shift-box lb (round (/ (- content-w (lbox-w lb)) 2)) 0))
                          (mla (shift-box lb (round (- content-w (lbox-w lb))) 0))
                          (mra)                                  ; margin-right auto: stay at start
                          ((string= a "center") (shift-box lb (round (/ (- content-w (lbox-w lb)) 2)) 0))
                          ((member a '("flex-end" "end") :test #'string=)
                           (shift-box lb (round (- content-w (lbox-w lb))) 0))
                          ((string= a "stretch")
                           (when (null (css:cstyle-width s)) (setf (lbox-w lb) content-w))))))
                (incf y (+ mt h mb gap))
                (when (and (> auto-unit 0) is (css:cstyle-margin-bottom-auto is)) (incf y auto-unit)))
              (values (remove nil boxes) (max 0 (- y cy gap)))))))))

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
(defun ws-only-text-p (n)
  "True when N is a text node of only white space — ignored by the anonymous-cell
fixup, so it never forces an empty anonymous cell between real cells."
  (and (eq (h:dnode-kind n) :text)
       (every (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return #\Page)))
              (h:dnode-data n))))

(defun anon-table-cell (kids ref styles)
  "A synthetic table-cell wrapping a run of promoted non-cell content KIDS — the
anonymous cell a table row's fixup requires (CSS 2.1 17.2.1) when display:contents
cells (or stray content) leave inline/block runs directly in a row.  It generates
no box of its own (no margin/border/padding/background); inheritable style is taken
from REF's box."
  (let ((cs (let ((c (css::copy-cstyle (or (st styles ref) (css::make-cstyle)))))
              (setf (css:cstyle-display c) "table-cell"
                    (css:cstyle-width c) :auto (css:cstyle-height c) :auto
                    (css:cstyle-float c) "none" (css:cstyle-position c) "static"
                    (css:cstyle-background c) nil (css:cstyle-bg-image c) nil (css:cstyle-bg-gradient c) nil
                    (css:cstyle-margin-top c) 0.0 (css:cstyle-margin-right c) 0.0
                    (css:cstyle-margin-bottom c) 0.0 (css:cstyle-margin-left c) 0.0
                    (css:cstyle-padding-top c) 0.0 (css:cstyle-padding-right c) 0.0
                    (css:cstyle-padding-bottom c) 0.0 (css:cstyle-padding-left c) 0.0
                    (css:cstyle-border-top-width c) 0.0 (css:cstyle-border-right-width c) 0.0
                    (css:cstyle-border-bottom-width c) 0.0 (css:cstyle-border-left-width c) 0.0)
              c))
        (v (make-array 4 :adjustable t :fill-pointer 0)))
    (dolist (k kids) (vector-push-extend k v))
    (let ((el (h::%dnode :kind :element :name "td" :children v)))
      (setf (gethash el styles) cs)
      el)))

(defun cell-like-node-p (c styles)
  "As CELL-LIKE-P, but also treats a non-white-space text node (promoted out of a
display:contents cell) as cell-like content the row must wrap."
  (cond ((ws-only-text-p c) nil)
        ((eq (h:dnode-kind c) :text) t)
        (t (cell-like-p c styles))))

(defun flat-children (node styles)
  "NODE's in-flow children with display:contents wrappers flattened away, keeping
both elements and text — the sequence the table fixups actually consume."
  (flatten-contents (coerce (h:dnode-children node) 'list) styles))

(defun table-rows (node styles)
  "Collect <tr> rows directly under NODE or within row-groups, flattening any
display:contents wrapper (CSS Display 3) so a contents row / row-group still yields
its rows.  A container (the table, or a row-group) that ends up holding bare
cell-like content but no row box acts as an anonymous table-row (CSS 2.1 17.2.1) —
represented here by that container node itself."
  (let ((rows '()))
    (dolist (c (effective-child-elements node styles))
      (let ((d (cdisplay (st styles c))))
        (cond ((string= d "table-row") (push c rows))
              ((member d '("table-row-group" "table-header-group" "table-footer-group")
                       :test #'string=)
               (let ((grouprows '()) (bare nil))
                 (dolist (r (flat-children c styles))
                   (cond ((and (eq (h:dnode-kind r) :element)
                               (string= (cdisplay (st styles r)) "table-row"))
                          (push r grouprows))
                         ((cell-like-node-p r styles) (setf bare t))))
                 (cond (grouprows (dolist (r (nreverse grouprows)) (push r rows)))
                       (bare (push c rows))))))))          ; group of bare cells = anon row
    (or (nreverse rows)
        (when (some (lambda (c) (cell-like-node-p c styles)) (flat-children node styles))
          (list node)))))
(defun anon-row-p (row styles)
  "True when ROW is not a real table-row box but stands in for one (the table or a
row-group holding bare cell-like content)."
  (let ((cs (st styles row))) (not (and cs (string= (css:cstyle-display cs) "table-row")))))
(defun row-cells (row styles &optional table)
  "Cells of ROW.  A real <tr> contributes its table-cell children directly; a run of
promoted non-cell content — text/inline left behind by display:contents cells (CSS
Display 3), or a stray block — is gathered into one anonymous cell (CSS 2.1 17.2.1).
An anonymous row (the table or a row-group standing in for a row) keeps each cell-like
element as its own cell, but still wraps promoted text in an anonymous cell.  Both
paths flatten display:contents wrappers first."
  (declare (ignore table))
  (let ((anon (anon-row-p row styles)) (out '()) (run '()))
    (flet ((flush ()
             (when run
               (push (anon-table-cell (nreverse run) row styles) out)
               (setf run '()))))
      (dolist (c (flat-children row styles))
        (cond ((ws-only-text-p c))                              ; inter-cell white space
              ((and (eq (h:dnode-kind c) :element)
                    (string= (cdisplay (st styles c)) "table-cell"))
               (flush) (push c out))                            ; a real cell
              ((and anon (eq (h:dnode-kind c) :element) (cell-like-p c styles))
               (flush) (push c out))                            ; anon row: each cell-like box its own cell
              ((and (eq (h:dnode-kind c) :element)
                    (member (cdisplay (st styles c))
                            '("table-row" "table-row-group" "table-header-group"
                              "table-footer-group" "table-column" "table-column-group"
                              "table-caption" "none")
                            :test #'string=)))                   ; internal-table / none: skip
              (t (push c run))))                                 ; inline/block/text: gather
      (flush))
    (nreverse out)))

(defun cell-pad-bord (cs)
  "Left+right padding + border of a cell's box."
  (if cs (+ (css::resolve-pad (css:cstyle-padding-left cs) nil) (css::resolve-pad (css:cstyle-padding-right cs) nil)
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

(defun cell-line-count (lb)
  "Number of text-bearing line boxes anywhere under cell box LB.  Recurses through
block wrappers — HN's nav cell nests its lines inside a block <span>/<b>, not directly
under the cell — so a wrapped cell counts >1 and a single-line cell counts exactly 1."
  (labels ((scan (box)
             (loop for c in (and (typep box 'lbox) (lbox-children box))
                   sum (cond ((and (typep c 'lbox) (eq (lbox-kind c) :line) (some #'frag-p (lbox-children c))) 1)
                             ((typep c 'lbox) (scan c))
                             (t 0)))))
    (scan lb)))

(defun cell-has-text-p (lb)
  "True when cell box LB contains a text fragment anywhere — a real baseline.  A cell
whose only content is a block/replaced element (HN's display:block top-bar logo, the
votearrow div) has no frag, so it must not join the baseline group."
  (plusp (cell-line-count lb)))

(defun cell-inline-content-height (lb)
  "Height of LB's inline content — its line boxes' extent measured from the cell top."
  (- (loop for c in (lbox-children lb) maximize (+ (lbox-y c) (lbox-h c))) (lbox-y lb)))

(defun cell-single-line-text-p (lb)
  "True when cell LB holds exactly one text-bearing line box — one clean baseline.  A
cell that wrapped to several lines (HN's nav at a narrow width) has no single baseline
the other cells align to."
  (= 1 (cell-line-count lb)))

(defun place-cell-content (lb rowh baseline-ref center-mode)
  "Vertically place a table cell's content within its row.

Default vertical-align is baseline, so a single line of text drops to the row baseline
near the bottom (HN's story title beside the votearrow): a text cell shifts down to
align its baseline with BASELINE-REF, the tallest single-line text cell.

CENTER-MODE holds when the row is taller than that single-line baseline group because a
NON-baseline cell set the height — a block/replaced element (HN's display:block top-bar
logo) OR a cell that wrapped to several lines (the nav at mobile width).  With no single
baseline to drop onto, the browser centers every cell's content in the row.  A block
cell (no text baseline of its own) is likewise centered."
  (when (and (not (cell-lbox-valign-top-p lb)) (lbox-children lb))
    (let* ((content-h (cell-inline-content-height lb))
           ;; round the shift to a whole pixel: a fractional centering offset lands the
           ;; cell content on a half-pixel and its 1px borders round unevenly (HN's
           ;; logo showed 2 white pixels above, 1 below).
           (shift (cond (center-mode (round (- rowh content-h) 2))          ; center everything
                        ((cell-has-text-p lb) (max 0 (round (- (or baseline-ref rowh) content-h))))
                        (t (round (- rowh content-h) 2)))))                 ; block cell: center
      (when (> shift 0)
        (dolist (c (lbox-children lb)) (shift-box c 0 shift))))))

(defun min-inline-width (node styles cs content-w)
  "Min-content width of NODE's inline content: the widest single unbreakable
token (word or atomic box)."
  (let ((words (collect-words (coerce (h:dnode-children node) 'list) styles cs content-w)) (w 0))
    (dolist (wd words)
      (unless (eq (car wd) :break)          ; a forced break has no width
        (setf w (max w (if (eq (car wd) :atomic) (lbox-w (tok-meta wd))
                           (word-min-width (car wd) (tok-meta wd) (tok-node wd)))))))
    w))

(defun intrinsic-margin (m)
  "The inline-axis margin contribution of M when sizing a container to its
content (CSS Sizing 3 §intrinsic-contribution): a fixed length counts, a bare
percentage resolves to zero, and a percentage-bearing calc() contributes only its
fixed length part (its percentage resolves to zero).  Negative contributions are
clamped to zero, matching the numeric path."
  (cond ((numberp m) (max 0 m))
        ((and (consp m) (eq (first m) :calc)) (max 0 (second m)))
        (t 0)))

(defun min-content-width (node styles content-w &optional (depth 0) skip-own-width)
  "Min-content CONTENT width of element NODE (widest unbreakable run).  With
SKIP-OWN-WIDTH, NODE's own definite width does NOT fix its min-content — used when a
caller (a table cell) maxes the width in separately and needs the content's
intrinsic min, which overflowing content can exceed (a width:100px cell with a
150px word is 150 wide, not 100)."
  (with-intrinsic-memo (list :min node (round content-w) skip-own-width)
    (let ((cs (st styles node)))
      (cond
        ((or (not (eq (h:dnode-kind node) :element)) (null cs)) 0)
        ;; A table's min-content is the sum of its column MIN widths, measured by
        ;; the (memoised) column model — NOT its subtree flattened onto one inline
        ;; line, which would re-lay-out every nested table and blow up.
        ((table-box-p styles node) (min content-w (table-min-width node styles content-w)))
        ;; A definite width fixes the box's min-content contribution: the box neither
        ;; shrinks below nor grows past its used width, so its content overflowing (or
        ;; being empty) does not change it — an empty width:50px div contributes 50,
        ;; not 0 (CSS Sizing 3 §5.1).  This function returns the CONTENT-box width, so
        ;; a border-box width has its padding + border stripped off.  (A table cell is
        ;; the exception: its width is only a floor, content can grow it — SKIP here.)
        ((and (not skip-own-width) (numberp (css:cstyle-width cs)))
         (let ((w (css:cstyle-width cs)))
           (when (equal (css:cstyle-box-sizing cs) "border-box")
             (decf w (+ (css::resolve-pad (css:cstyle-padding-left cs) content-w)
                        (css::resolve-pad (css:cstyle-padding-right cs) content-w)
                        (used-border cs :l) (used-border cs :r))))
           (min content-w (max 0 w))))
        (t
         (let ((block-kids (remove-if-not (lambda (k) (or (block-level-p styles k) (float-p styles k)))
                                          (child-elements node))))
           (min content-w
                (if block-kids
                     (loop for k in block-kids
                           for kcs = (st styles k)
                           maximize (+ (intrinsic-margin (css:cstyle-margin-left kcs))
                                       (intrinsic-margin (css:cstyle-margin-right kcs))
                                       (css::resolve-pad (css:cstyle-padding-left kcs) content-w) (css::resolve-pad (css:cstyle-padding-right kcs) content-w)
                                       (used-border kcs :l) (used-border kcs :r)
                                       (min-content-width k styles content-w (1+ depth))))
                     (min-inline-width node styles cs content-w)))))))))

(defun cell-max-content-width (cell styles avail)
  "Max-content border-box width of a table CELL.  An explicit width is the target,
but never below the cell's unshrinkable min-content.  A box-sizing:border-box
width already includes the cell's padding+border, so it is not added again."
  (let* ((cs (st styles cell)) (w (and cs (css:cstyle-width cs)))
         (bb (and cs (string= (css:cstyle-box-sizing cs) "border-box"))))
    (max 0 (if (numberp w)
               (let ((cmin (min-content-width cell styles avail 0 t)))
                 (if bb (max w (+ cmin (cell-pad-bord cs)))
                     (+ (cell-pad-bord cs) (max w cmin))))
               (+ (cell-pad-bord cs) (pref-content-width cell styles avail))))))

(defun cell-min-content-width (cell styles avail)
  "Min-content border-box width of a table CELL.  An explicit width is honored as
a floor, but a cell can never be narrower than its unshrinkable content — e.g.
HN's logo cell is width:18px yet holds a 20px (bordered) <img>, so the column
must be 20, not 18 (matching how the browser widens the column to fit it).  A
box-sizing:border-box width already includes padding+border."
  (let* ((cs (st styles cell)) (w (and cs (css:cstyle-width cs)))
         (bb (and cs (string= (css:cstyle-box-sizing cs) "border-box")))
         ;; a scroll container (overflow:auto/scroll) has an automatic minimum
         ;; size of zero: its overflowing content scrolls and does not widen the
         ;; column (CSS Sizing 3 §5.1).
         (scroll (and cs (member (css:cstyle-overflow cs) '("auto" "scroll") :test #'equal)))
         (cmin (if scroll 0 (min-content-width cell styles avail 0 t))))
    (max 0 (if (numberp w)
               (if bb (max w (+ cmin (cell-pad-bord cs)))
                   (+ (cell-pad-bord cs) (max w cmin)))
               (+ (cell-pad-bord cs) cmin)))))

(defun cell-spec-width (cell styles)
  "Specified column width contributed by CELL: NIL, a border-box px number, or
a (:percent P) form.  A box-sizing:border-box width is already border-box."
  (let* ((cs (st styles cell)) (w (and cs (css:cstyle-width cs)))
         (bb (and cs (string= (css:cstyle-box-sizing cs) "border-box"))))
    (cond ((numberp w) (if bb w (+ w (cell-pad-bord cs))))
          ((and (consp w) (eq (car w) :percent)) (list :percent (second w)))
          ;; calc(% + px) mixing a percentage and a length: a deferred (:calc px
          ;; pct) form. calc(50% + 0px) must act as 50% here (csswg-drafts #3482),
          ;; so it becomes a column spec resolved against the table width like a
          ;; percentage, plus the fixed px part.
          ((and (consp w) (eq (car w) :calc)) (list :calc (second w) (third w)))
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
        ((and (consp sp) (eq (car sp) :calc))
         (+ (second sp) (* table-w (/ (third sp) 100.0))))
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
max-content (or fixed) widths.  0 when it has no cells.  A percentage column
width refers to the table's own width (CSS 2.1 §17.5.2): with no explicit table
width the table grows so the non-percentage columns fit in the remaining
(100 - Σpct)% — e.g. a 50% column beside a 100px column yields a 200px table."
  (multiple-value-bind (maxs mins specs ncols) (table-column-model node styles avail)
    (declare (ignore mins))
    (let ((fixed-sum 0.0) (tot-pct 0.0) (pct-need 0.0))
      (dotimes (i ncols)
        (let ((sp (aref specs i)) (mx (aref maxs i)))
          (cond
            ((numberp sp) (incf fixed-sum (max sp mx)))
            ((and (consp sp) (eq (car sp) :percent))
             (incf tot-pct (second sp))
             (when (plusp (second sp))
               (setf pct-need (max pct-need (* mx (/ 100.0 (second sp)))))))
            ((and (consp sp) (eq (car sp) :calc))
             (incf fixed-sum (max 0.0 (float (second sp))))
             (if (plusp (third sp))
                 (progn (incf tot-pct (third sp))
                        (setf pct-need (max pct-need
                                            (* (max 0.0 (- mx (float (second sp))))
                                               (/ 100.0 (third sp))))))
                 (incf fixed-sum (max 0.0 (- mx (float (second sp)))))))
            (t (incf fixed-sum mx)))))
      (if (plusp tot-pct)
          (let ((fixed-need (/ fixed-sum (- 1.0 (/ (min tot-pct 99.9) 100.0)))))
            (max fixed-need pct-need (loop for i below ncols sum (aref maxs i))))
          fixed-sum))))

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
    ;; A column is never squeezed below its min-content: when the target is smaller
    ;; than the sum of column min-contents (no shrink room, or a deficit exceeding
    ;; it) the table takes its min-content width and OVERFLOWS its container rather
    ;; than crushing cells below their content (CSS 2.1 §17.5.2) — e.g. four 50px
    ;; cells in a 100px overflow-x:auto scroller stay 50 (table 200), not 25.
    (loop for i below ncols collect (max (float (or (nth i mins) 1.0)) (aref w i) 1.0))))

(defun rel-offset (cs)
  "Visual (dx dy) shift for a position:relative CS (CSS 2.1 9.4.3), else (0 0)."
  (if (and cs (string= (css:cstyle-position cs) "relative"))
      (values (round (cond ((numberp (css:cstyle-left cs)) (css:cstyle-left cs))
                           ((numberp (css:cstyle-right cs)) (- (css:cstyle-right cs))) (t 0)))
              (round (cond ((numberp (css:cstyle-top cs)) (css:cstyle-top cs))
                           ((numberp (css:cstyle-bottom cs)) (- (css:cstyle-bottom cs))) (t 0))))
      (values 0 0)))

(defun layout-table (node styles cx cy content-w base-cs &optional target-h)
  "Automatic table layout (CSS 2.1 17.5.2): columns sized to their content (or a
specified width), rows stacked, cells stretched to row height.  TARGET-H, when a
number greater than the natural row total, is the grid-box height the rows are
grown to fill (from an explicit / min height, §17.5.3).  Returns (values
cell-lboxes content-height)."
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
        ;; PASS 1: lay each row's cells at the content origin CY and record its
        ;; natural height; placement is deferred so an over-tall table can grow the
        ;; rows first (§17.5.3) without re-laying their content.
        (let ((rowinfo '()) (natural 0))
          (dolist (row rows)
            (let ((cells (row-cells row styles node)) (rowh 0) (rowboxes '()) (col 0))
              (dolist (cell cells)
                (let* ((span (cell-colspan cell))
                       (x0 (aref colx (min ncols col)))
                       (x1 (aref colx (min ncols (+ col span))))
                       (cw (max 1 (round (- x1 x0)))))
                  (multiple-value-bind (lb adv) (layout-node cell styles (round (+ cx x0)) cy cw)
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
              (push (list row (nreverse rowboxes) rowh) rowinfo)
              (incf natural rowh)))
          (setf rowinfo (nreverse rowinfo))
          ;; PASS 2: distribute a table taller than its content across rows in
          ;; proportion to their heights (§17.5.3); with no surplus SHARE is 0 and
          ;; every row keeps its natural height, so ordinary tables are unchanged.
          (let ((surplus (if (and (numberp target-h) (> target-h natural)) (- target-h natural) 0)))
            (dolist (ri rowinfo)
              (destructuring-bind (row rowboxes rowh) ri
                (let* ((share (cond ((<= surplus 0) 0)
                                    ((> natural 0) (* surplus (/ rowh natural)))
                                    (t (/ surplus (length rowinfo)))))
                       (rh2 (round (+ rowh share))))
                  (dolist (lb rowboxes) (shift-box lb 0 (round (- y (lbox-y lb)))))
                  ;; the baseline group = the single-line text cells; shorter cells align
                  ;; to its tallest.  A row taller than that group (a block/wrapped cell,
                  ;; or a distributed surplus) has no single baseline, so cells center.
                  (let* ((bref (loop for lb in rowboxes
                                     when (and (not (cell-lbox-valign-top-p lb)) (cell-single-line-text-p lb))
                                     maximize (cell-inline-content-height lb)))
                         (center-mode (> rh2 (+ bref 1))))
                    (dolist (lb rowboxes)
                      (place-cell-content lb rh2 (and (plusp bref) bref) center-mode)
                      (setf (lbox-h lb) rh2)))                          ; stretch box to row height
                  ;; position:relative on the row, its row-group, or a cell shifts the
                  ;; box(es) visually (the table box model doesn't otherwise honor
                  ;; relative offsets on table parts).  Row + group move the whole row.
                  (multiple-value-bind (rdx rdy) (rel-offset (unless (eq row node) (st styles row)))
                    (let ((grp (h:dnode-parent row)))
                      (when (and grp (eq (h:dnode-kind grp) :element)
                                 (member (string-downcase (h:dnode-name grp)) '("tbody" "thead" "tfoot") :test #'string=))
                        (multiple-value-bind (gdx gdy) (rel-offset (st styles grp)) (incf rdx gdx) (incf rdy gdy))))
                    (dolist (lb rowboxes)
                      (multiple-value-bind (cdx cdy) (rel-offset (lbox-style lb))
                        (shift-box lb (+ rdx cdx) (+ rdy cdy)))))
                  ;; A row with no cell boxes but a positive height (an empty spacer row)
                  ;; still occupies its band; give it a box so it advances the flow and is
                  ;; recorded/painted like the browser's tr box.
                  (when (and (null rowboxes) (plusp rh2) (not (eq row node)))
                    (setf rowboxes (list (make-lbox :x (round cx) :y y :w (round content-w) :h rh2
                                                    :style (st styles row) :node row :kind :block))))
                  (setf boxes (nconc boxes rowboxes))
                  (incf y rh2)))))
          (values boxes (- y cy)))))))

(defun pref-inline-width (node styles cs content-w)
  "Max-content width of NODE's inline content: word + atomic-box widths summed on
a single (unwrapped) line.  Measures NODE's CHILDREN — collecting NODE itself would
re-wrap an inline-block/atomic node as one atomic box laid out at the full available
width (an empty icon then reports content-w instead of ~0)."
  (let ((words (collect-words (coerce (h:dnode-children node) 'list) styles cs content-w))
        (w 0) (seg 0) (first t))
    ;; Max-content is the widest run between forced breaks (CSS Sizing 3 §5.1):
    ;; a <br>/newline (:break) resets the running segment width rather than being
    ;; summed into one line, so `AAA<br>BBBBB` measures BBBBB, not AAABBBBB.
    ;; TOK-SPACE marks collapsible white space that PRECEDED a token; for the FIRST
    ;; token of a segment that leading space sits at the line start and collapses
    ;; away (CSS 2.1 16.6.1), so it must not widen the measure — only inter-word
    ;; spaces (a non-first token's leading space) count.
    (dolist (wd words)
      (if (eq (car wd) :break)
          (setf seg 0 first t)
          (progn
            (incf seg (+ (if (eq (car wd) :atomic) (lbox-w (tok-meta wd)) (word-w (car wd) (tok-meta wd)))
                         (tok-gap wd)
                         (if (and (tok-space wd) (not first)) (space-w (if (eq (car wd) :atomic) cs (tok-meta wd))) 0)))
            (setf first nil)))
      (setf w (max w seg)))
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
         (let ((items (remove-if (lambda (k) (out-of-flow-p styles k)) (effective-child-elements node styles))))
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

(defun intrinsic-keyword-width (kw node styles avail-w)
  "Resolve an intrinsic-sizing keyword (CSS Sizing 3) used as a min-width/max-width
value to a CONTENT-box px width from NODE's own content measures, or NIL when KW is
not such a keyword.  NODE's own definite width does not fix the measure (SKIP-OWN-
WIDTH) — the keyword asks for the intrinsic size of the content, so a box with both
`width:50px` and `min-width:min-content` still measures its children."
  (let ((av (or avail-w 0)))
    (case kw
      (:min-content (min-content-width node styles av 0 t))
      (:max-content (pref-content-width node styles av))
      (:fit-content (max (min-content-width node styles av 0 t)
                         (min (pref-content-width node styles av) av)))
      (t nil))))

(defun pref-border-width (node styles content-w depth)
  "Preferred BORDER-box width (incl. margins) of NODE for shrink-to-fit sizing."
  (let* ((cs (st styles node))
         (w (and cs (css:cstyle-width cs)))
         ;; A definite `box-sizing:border-box` width already includes the padding
         ;; + border, so they must not be added on top again (CSS Sizing 3); only
         ;; an auto or content-box width adds them.
         (border-box-w (and (numberp w) (equal (css:cstyle-box-sizing cs) "border-box"))))
    (if (null cs) 0
        (+ (css:cstyle-margin-left cs) (css:cstyle-margin-right cs)
           (if border-box-w 0
               (+ (used-border cs :l) (used-border cs :r)
                  (css::resolve-pad (css:cstyle-padding-left cs) content-w)
                  (css::resolve-pad (css:cstyle-padding-right cs) content-w)))
           (if (numberp w) w (pref-content-width node styles content-w depth))))))

(defun place-float (node styles cleft cright top content-w)
  "Position a floated NODE at the left/right edge within [CLEFT,CRIGHT], dropping
below existing floats if it does not fit.  Records it in *FLOATS*; returns its lbox."
  (let* ((cs (st styles node))
         (side (if (string= (css:cstyle-float cs) "left") :left :right))
         (ml (css:cstyle-margin-left cs)) (mr (css:cstyle-margin-right cs))
         (mb (css:cstyle-margin-bottom cs))
         (extra (+ (css::resolve-pad (css:cstyle-padding-left cs) content-w) (css::resolve-pad (css:cstyle-padding-right cs) content-w)
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

;;; ---- position: sticky (CSS Position 3 §3.3) -------------------------------
;;; A sticky box is laid out in normal flow, then shifted the minimum amount to
;;; keep it within its nearest scroll container's scrollport per its inset edges,
;;; but never outside its containing block.  weft renders at scroll offset 0 (no
;;; script), so the shift is non-zero only where the box's flow position already
;;; sits past an inset edge — e.g. a bottom:0 box below the scrollport fold sticks
;;; up to the bottom edge, clamped so it cannot leave its containing block.
(defun sticky-box-p (lb)
  (let ((cs (and (eq (lbox-kind lb) :block) (lbox-style lb))))
    (and cs (equal (css:cstyle-position cs) "sticky"))))

(defun scroll-container-box-p (lb)
  "A box whose overflow is not visible establishes a scrollport for descendant
sticky boxes (CSS Overflow 3: scroll/auto/hidden/clip all scroll-clip)."
  (let ((cs (and (eq (lbox-kind lb) :block) (lbox-style lb))))
    (and cs (member (css:cstyle-overflow cs) '("scroll" "auto" "hidden" "clip")
                    :test #'string=))))

(defun sticky-inset (v cb-size)
  "Resolve an inset value V (px number, (:percent N), or :auto) to px, or NIL for
:auto.  Percentages resolve against the containing-block extent CB-SIZE."
  (cond ((numberp v) v)
        ((and (consp v) (eq (car v) :percent)) (* (/ (second v) 100.0) cb-size))
        (t nil)))

(defun apply-sticky (lb scroll-rect cb-rect)
  "Shift sticky box LB within SCROLL-RECT (its scrollport at offset 0), clamped to
CB-RECT (its containing block).  Rects are (x y w h) in tree coordinates."
  (let* ((cs (lbox-style lb))
         (spt (second scroll-rect)) (spb (+ (second scroll-rect) (fourth scroll-rect)))
         (spl (first scroll-rect))  (spr (+ (first scroll-rect) (third scroll-rect)))
         (bt (lbox-y lb)) (bb (+ (lbox-y lb) (lbox-h lb)))
         (bl (lbox-x lb)) (br (+ (lbox-x lb) (lbox-w lb)))
         ;; The containing block always contains the box's own static position (the
         ;; box was laid out inside it, possibly in scrollable overflow the clipped
         ;; border-box excludes).  Expanding the clamp rect to include the box means
         ;; the CB confinement never pulls a box AWAY from a static position that is
         ;; already outside the visible scrollport — CSS Position 3: a sticky box
         ;; whose static position is outside its scrollport is not forced into it.
         (cbt (min (second cb-rect) bt)) (cbb (max (+ (second cb-rect) (fourth cb-rect)) bb))
         (cbl (min (first cb-rect) bl))  (cbr (max (+ (first cb-rect) (third cb-rect)) br))
         ;; a <percentage> inset resolves against the containing block (CB height for
         ;; top/bottom, CB width for left/right), as for relative positioning.
         (top (sticky-inset (css:cstyle-top cs) (fourth cb-rect)))
         (bottom (sticky-inset (css:cstyle-bottom cs) (fourth cb-rect)))
         (left (sticky-inset (css:cstyle-left cs) (third cb-rect)))
         (right (sticky-inset (css:cstyle-right cs) (third cb-rect)))
         (dx 0) (dy 0))
    (when (numberp top)    (let ((a (+ spt top)))    (when (< bt a) (setf dy (- a bt)))))
    (when (numberp bottom) (let ((a (- spb bottom))) (when (> (+ bb dy) a) (decf dy (- (+ bb dy) a)))))
    ;; confine to the containing block (lo may exceed hi if the box is taller than
    ;; its CB; then MAX wins and pins the box's start edge to the CB start).
    (setf dy (max (- cbt bt) (min dy (- cbb bb))))
    (when (numberp left)  (let ((a (+ spl left)))  (when (< bl a) (setf dx (- a bl)))))
    (when (numberp right) (let ((a (- spr right))) (when (> (+ br dx) a) (decf dx (- (+ br dx) a)))))
    (setf dx (max (- cbl bl) (min dx (- cbr br))))
    (when (or (/= dx 0) (/= dy 0)) (shift-box lb (round dx) (round dy)))))

(defun resolve-sticky (root vp-rect)
  "Walk the laid-out tree and settle every position:sticky box against the nearest
enclosing scroll container (else the viewport VP-RECT) and its containing block."
  (labels ((rect (lb) (list (lbox-x lb) (lbox-y lb) (lbox-w lb) (lbox-h lb)))
           (walk (lb scroll-rect cb-rect)
             (when (typep lb 'lbox)
               (when (sticky-box-p lb) (apply-sticky lb scroll-rect cb-rect))
               (let ((sr (if (scroll-container-box-p lb) (rect lb) scroll-rect))
                     (cr (if (eq (lbox-kind lb) :block) (rect lb) cb-rect)))
                 (dolist (c (lbox-children lb))
                   (when (typep c 'lbox) (walk c sr cr)))))))
    (walk root vp-rect vp-rect)))

(defun layout-tree (document styles width &optional viewport-height scroll-to abs-vh)
  (let* ((*floats* nil) (*abs-pending* nil) (*fixed-pending* nil)
         (*intrinsic-cache* (make-hash-table :test 'equal))
         ;; Lay out the ROOT element (<html>), not <body>: the root's own width /
         ;; border / margin / padding form the containing block for <body> (Acid3's
         ;; `html { width: 32em; border: 2cm solid gray; margin: 1em }`).  A normal
         ;; page's html is auto-width with no box, so <body> still fills the viewport.
         (root-el (or (css:query-select document "html") (css:query-select document "body"))))
    (when root-el
      ;; The initial containing block has the viewport height when the viewport
      ;; model is active (definite) — so a percentage height resolves against it
      ;; (CSS 2.1 10.5); otherwise the page height is indefinite.
      (multiple-value-bind (root adv) (layout-node root-el styles 0 0 width viewport-height)
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
               ;; initial containing block: the viewport-sized rectangle at the
               ;; document origin (CSS 2.1 §10.1) — it scrolls with the page, but
               ;; its height is the viewport's, not the content's, so `bottom` and
               ;; auto block-axis margins measure the visible box.  Fixed boxes
               ;; resolve against the viewport (offset by the current scroll so
               ;; they stay pinned to the visible rectangle).  ABS-VH carries the
               ;; true viewport height even in reader mode (where the layout height
               ;; is left indefinite so the page grows to content).
               (icb (list 0 0 width (or abs-vh viewport-height ph)))
               (vp  (list 0 scroll-y width (or abs-vh vph))))
          (dolist (p *abs-pending*)   (finalize-positioned p icb styles))
          (dolist (p *fixed-pending*) (finalize-positioned p vp styles))
          ;; Apply the scroll: shift the whole painted tree up so the anchor
          ;; (and fixed boxes, already placed at scroll-y+offset) land in view.
          (when (and root (plusp scroll-y)) (shift-box root 0 (- scroll-y)))
          ;; position:sticky settles after scroll/positioning, over final geometry:
          ;; each sticky box is confined to its scroll container's scrollport (at
          ;; offset 0) and its containing block (CSS Position 3 §3.3).
          (when root (resolve-sticky root (list 0 0 width vph)))
          ;; CSS transforms are a post-layout visual/geometry effect — applied last,
          ;; over the final (scrolled, positioned) tree, matching viewport-space
          ;; getBoundingClientRect (CSS Transforms 1 §3).
          (when root (apply-transforms root)))
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

(defun bg-tile-size (bs img bw bh)
  "Concrete background tile (width height) in px for IMG in a BW×BH positioning area
under background-size BS (NIL=auto | :contain | :cover | (w-spec h-spec)), via the
CSS Images §5.3 default sizing algorithm.  An SVG carries its intrinsic width/height/
ratio (any absent) which drives the auto cases; a raster's ratio is its pixel w/h, so
raster backgrounds size exactly as before.  A dimension may round to 0 (an empty tile
for a degenerate ratio, e.g. a zero-height viewBox); the painter skips those."
  (let* ((rw (img-w img)) (rh (img-h img))
         ;; intrinsic dimensions: an SVG's own (may be NIL); a raster's pixel size.
         (iw (if (img-sr img) (img-sw img) (and (plusp rw) rw)))
         (ih (if (img-sr img) (img-sh img) (and (plusp rh) rh)))
         (ir (cond ((img-sr img))                                    ; SVG ratio: number | 0 | :infinite
                   ((and (plusp rw) (plusp rh)) (/ (float rw 1d0) rh))
                   (t nil)))
         (ratio (and (numberp ir) (plusp ir) ir)))                   ; a usable numeric ratio (>0)
    ;; a single tile is bounded: cover/contain with a degenerate ratio can compute a
    ;; tile far larger than the paint area (only a slice of which is ever visible), so
    ;; cap each dimension — enough to exceed any real box, small enough to blit fast.
    (labels ((r (x) (max 0 (min 16384 (round (max 0 x)))))
             (defc (spec avail) (cond ((numberp spec) (float spec 1d0))
                                      ((and (consp spec) (eq (first spec) :percent))
                                       (* avail (/ (second spec) 100.0)))
                                      (t nil)))            ; :auto -> NIL
             (both-auto ()
               (cond ((and iw ih) (values iw ih))                    ; both intrinsic (rasters land here)
                     (ratio (cond (iw (values iw (/ iw ratio)))
                                  (ih (values (* ih ratio) ih))
                                  (t (let ((w (min bw (* bh ratio)))) (values w (/ w ratio)))))) ; contain the ratio
                     (iw (values iw bh))
                     (ih (values bw ih))
                     ((eql ir 0) (values 0 bh))            ; zero-width ratio -> empty width
                     ((eq ir :infinite) (values bw 0))     ; zero-height ratio -> empty height
                     (t (values bw bh)))))                 ; no intrinsic info -> the whole area
      (multiple-value-bind (w h)
          (cond
            ((eq bs :contain) (if ratio (let ((w (min bw (* bh ratio)))) (values w (/ w ratio))) (both-auto)))
            ((eq bs :cover)   (if ratio (let ((w (max bw (* bh ratio)))) (values w (/ w ratio))) (both-auto)))
            ((consp bs)
             (let ((sw (defc (first bs) bw)) (sh (defc (second bs) bh)))
               (cond ((and sw sh) (values sw sh))
                     (sw (cond (ratio (values sw (/ sw ratio)))
                               (ih (values sw ih))
                               ((or (eq ir :infinite) (eql ir 0)) (values sw 0)) ; degenerate ratio -> empty
                               (t (values sw bh))))
                     (sh (cond (ratio (values (* sh ratio) sh))
                               (iw (values iw sh))
                               ((or (eql ir 0) (eq ir :infinite)) (values 0 sh)) ; degenerate ratio -> empty
                               (t (values bw sh))))
                     (t (both-auto)))))
            (t (both-auto)))
        (values (r w) (r h))))))

(defun paint-bg-image (cv lb cs url)
  "Decode URL (data: URI or network image) and tile it across LB's padding box,
honoring background-repeat, -position and -size.  Best-effort: an undecodable image
paints nothing (the bg color shows through)."
  (let* ((duri (if (find #\% url) (percent-decode url) url))
         (img (if (and (>= (length duri) 5) (string-equal (subseq duri 0 5) "data:"))
                  (ignore-errors (decode-image duri))
                  ;; a network background image (icon/sprite, e.g. HN's votearrow
                  ;; triangle.svg) — fetched + decoded + cached through *IMAGE-LOADER*.
                  (ignore-errors (fetch-image url)))))
    (when (and img (plusp (img-w img)) (plusp (img-h img)))
      (let* ((iw0 (img-w img)) (ih0 (img-h img))
             (rep (css:cstyle-bg-repeat cs))
             (repx (member rep '("repeat" "repeat-x") :test #'string=))
             (repy (member rep '("repeat" "repeat-y") :test #'string=))
             ;; padding box (inside the borders)
             (px0 (round (+ (lbox-x lb) (used-border cs :l))))
             (py0 (round (+ (lbox-y lb) (used-border cs :t))))
             (px1 (round (- (+ (lbox-x lb) (lbox-w lb)) (used-border cs :r))))
             (py1 (round (- (+ (lbox-y lb) (lbox-h lb)) (used-border cs :b)))))
        (multiple-value-bind (iw ih)   ; effective tile size after background-size
            (bg-tile-size (css:cstyle-bg-size cs) img (- px1 px0) (- py1 py0))
          (let* ((pos (css:cstyle-bg-position cs))
                 (offx (if pos (bg-pos-offset (first pos) (- (- px1 px0) iw)) 0))
                 (offy (if pos (bg-pos-offset (second pos) (- (- py1 py0) ih)) 0))
                 (ox (+ px0 offx)) (oy (+ py0 offy)))
            ;; a degenerate tile (0 wide or tall — e.g. a zero-height-ratio SVG under
            ;; `background-size` with an auto axis) paints nothing.
            (when (and (> px1 px0) (> py1 py0) (> iw 0) (> ih 0))
              ;; clip tiles to the padding box so they never bleed out
              (let ((*clip* (clip-intersect px0 py0 px1 py1)))
                ;; common case: an intrinsic 1x1 image filling the box — solid rect.
                (if (and (= iw0 1) (= ih0 1) (null (css:cstyle-bg-size cs)) (>= (aref (img-rgba img) 3) 255))
                    (let ((r (aref (img-rgba img) 0)) (g (aref (img-rgba img) 1)) (b (aref (img-rgba img) 2)))
                      (fill-rect cv (if repx px0 ox) (if repy py0 oy)
                                 (if repx (- px1 px0) 1) (if repy (- py1 py0) 1) (list r g b)))
                    ;; general tiling, each tile scaled to the effective (IW IH).
                    ;; A tile-count cap bounds a pathological fine tiling (a tiny tile
                    ;; over a large area) so a degenerate background can't stall paint.
                    (let ((startx (if repx (- ox (* iw (ceiling (- ox px0) iw))) ox))
                          (starty (if repy (- oy (* ih (ceiling (- oy py0) ih))) oy))
                          (budget 200000))
                      (block tiles
                        (loop for ty = starty then (+ ty ih)
                              while (and (< ty py1) (or repy (= ty starty))) do
                          (loop for tx = startx then (+ tx iw)
                                while (and (< tx px1) (or repx (= tx startx))) do
                            (when (and (> (+ tx iw) px0) (> (+ ty ih) py0))
                              (blit-img cv img tx ty iw ih)
                              (when (<= (decf budget) 0) (return-from tiles))))))))))))))))

(defun paint-bg-image-fixed (cv lb cs url)
  "Tile URL's image as a background-attachment:fixed background: the tile grid is
anchored to the VIEWPORT origin (canvas 0,0) plus the background-position offset,
NOT to LB's box — so overlapping fixed-bg elements share one continuous tiling
(this is what fuses Acid2's two offset 2x2 images into a solid yellow fill).  The
painting is clipped to LB's border box (default background-clip)."
  (let* ((duri (if (find #\% url) (percent-decode url) url))
         (img (if (and (>= (length duri) 5) (string-equal (subseq duri 0 5) "data:"))
                  (ignore-errors (decode-image duri))
                  ;; a network background image (icon/sprite, e.g. HN's votearrow
                  ;; triangle.svg) — fetched + decoded + cached through *IMAGE-LOADER*.
                  (ignore-errors (fetch-image url)))))
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

(defun gradient-visible-p (grad)
  "NIL when both stops of GRAD (dir from-rgba to-rgba) are fully transparent — such
   a gradient (e.g. HN's `linear-gradient(transparent,transparent)` fallback layered
   under url(triangle.svg)) must paint nothing, not opaque black (FILL-GRADIENT drops
   the stop alpha).  A stop with no alpha component is treated as opaque."
  (destructuring-bind (dir from to) grad
    (declare (ignore dir))
    (flet ((opaque-ish (c) (or (< (length c) 4) (plusp (fourth c)))))
      (or (opaque-ish from) (opaque-ish to)))))

(defun paint-box (cv lb)
  (handler-case (%paint-box cv lb) (error () nil)))
(defun %paint-box (cv lb)
  (when lb
    (case (lbox-kind lb)
      (:block
       (let ((cs (lbox-style lb)))
        (when (box-visible-p cs)
         ;; An anonymous box (e.g. the wrapper generated for a block nested inside
         ;; an inline — HN's <a><div class=votearrow>) can carry a NIL style: it
         ;; paints no background/border of its own, but its children MUST still
         ;; paint, so every style-dependent step is guarded by (when cs ...).
         (when cs
           (cond ((and (css:cstyle-bg-gradient cs) (gradient-visible-p (css:cstyle-bg-gradient cs)))
                  (destructuring-bind (dir from to) (css:cstyle-bg-gradient cs)
                    ;; pass the raw rgba stops (keep the 4th/alpha element) so a
                    ;; translucent gradient composites over the box instead of
                    ;; painting opaque black — RGB is just the first three elements.
                    (fill-gradient cv (lbox-x lb) (lbox-y lb) (lbox-w lb) (lbox-h lb) dir from to)))
                 ((css:cstyle-background cs)
                  (fill-rect cv (lbox-x lb) (lbox-y lb) (lbox-w lb) (lbox-h lb) (rgb (css:cstyle-background cs)))))
           ;; CSS background image: over the bg color, under the borders, tiled and
           ;; clipped to this box's padding box.  Fixed-attachment images are not
           ;; painted (out of scope) — for Acid2 that is the correct result (their
           ;; images are positioned to the viewport, off this element).
           (when (css:cstyle-bg-image cs)
             (if (string-equal (css:cstyle-bg-attachment cs) "fixed")
                 (paint-bg-image-fixed cv lb cs (css:cstyle-bg-image cs))
                 (paint-bg-image cv lb cs (css:cstyle-bg-image cs)))))
         (when (lbox-img lb)
           (let ((fit (and cs (css:cstyle-object-fit cs))))
             (if (and fit (not (string= fit "fill")))
                 (multiple-value-bind (ox oy ow oh src)
                     (object-fit-geom fit (img-w (lbox-img lb)) (img-h (lbox-img lb))
                                      (round (lbox-x lb)) (round (lbox-y lb))
                                      (round (lbox-w lb)) (round (lbox-h lb)))
                   (blit-img cv (lbox-img lb) ox oy ow oh src))
                 (blit-img cv (lbox-img lb) (round (lbox-x lb)) (round (lbox-y lb))
                           (round (lbox-w lb)) (round (lbox-h lb))))))
         ;; Replaced vector content (inline <svg>, <canvas>): composite over the
         ;; box's background, under its borders.
         (when (lbox-vpaint lb)
           (funcall (lbox-vpaint lb) cv (round (lbox-x lb)) (round (lbox-y lb))
                    (round (lbox-w lb)) (round (lbox-h lb))))
         (when cs
           (paint-borders cv lb cs)
           (when (and (lbox-marker lb) (plusp (length (marker-glyph (lbox-marker lb)))))
             ;; the list marker (•, disc/circle/square) is painted via scribe so the
             ;; real bullet glyph renders (the 7x13 bitmap has none); it sits ~1.3em
             ;; left of the content, in the list's padding.
             (let ((fs (css:cstyle-font-size cs)))
               (draw-text-scribe cv (marker-glyph (lbox-marker lb))
                                 (round (- (+ (lbox-x lb) (css::resolve-pad (css:cstyle-padding-left cs) nil)) (* 1.3 fs)))
                                 (round (+ (lbox-y lb) (css::resolve-pad (css:cstyle-padding-top cs) nil)))
                                 (round (used-line-height cs))
                                 (css:cstyle-color cs) fs))))
         ;; overflow:hidden/clip/scroll clips descendants to this box's padding box.
         (if (and cs (member (css:cstyle-overflow cs) '("hidden" "clip" "scroll") :test #'string=))
             (let ((*clip* (clip-intersect
                            (round (+ (lbox-x lb) (used-border cs :l)))
                            (round (+ (lbox-y lb) (used-border cs :t)))
                            (round (- (+ (lbox-x lb) (lbox-w lb)) (used-border cs :r)))
                            (round (- (+ (lbox-y lb) (lbox-h lb)) (used-border cs :b))))))
               (paint-children cv (lbox-children lb)))
             (paint-children cv (lbox-children lb))))))
      (:line
       (loop for cell on (lbox-children lb)
             for it = (car cell)
             do (if (frag-p it)
                    (let ((cs (frag-style it)))
                      ;; pass the line box geometry so scribe centers the real font
                      ;; em-box (ascent+descent at font-size) within it.  A
                      ;; visibility:hidden run occupies its space but paints no glyphs.
                      (when (box-visible-p cs)
                        ;; inline background (e.g. <mark>, a highlighted <span>): paint
                        ;; the run's box behind its glyphs.  background-color is not
                        ;; inherited, so a non-nil value here is the run element's own.
                        (let ((bg (css:cstyle-background cs)))
                          (when (and bg (or (< (length bg) 4) (plusp (fourth bg))))
                            ;; extend across the inter-word gap when the next run is
                            ;; the SAME element (a multi-word highlight), so its spaces
                            ;; are covered too — but not into a separate adjacent chip.
                            (let* ((nx (cadr cell))
                                   (right (if (and (frag-p nx) (eq (frag-node nx) (frag-node it)))
                                              (frag-x nx)
                                              (+ (frag-x it) (frag-w it)))))
                              (fill-rect cv (round (frag-x it)) (lbox-y lb)
                                         (max 1 (round (- right (frag-x it)))) (lbox-h lb) bg))))
                        (let* ((ul (member "underline" (css:cstyle-text-decoration cs) :test #'string=))
                               (nxt (cadr cell))
                               ;; run the underline across the space into the next
                               ;; fragment when it is also underlined, so a multi-word
                               ;; link underlines continuously instead of per word.
                               (uend (when ul
                                       (if (and (frag-p nxt)
                                                (member "underline" (css:cstyle-text-decoration (frag-style nxt))
                                                        :test #'string=))
                                           (frag-x nxt)
                                           (+ (frag-x it) (frag-w it))))))
                          (draw-text-scribe cv (frag-text it) (round (frag-x it))
                                   (lbox-y lb) (lbox-h lb)
                                   (css:cstyle-color cs)
                                   (css:cstyle-font-size cs)
                                   :face (style-face cs)
                                   :bold (>= (css:cstyle-font-weight cs) 600)
                                   :letter-spacing (css:cstyle-letter-spacing cs)
                                   :underline ul
                                   :underline-end-x uend
                                   :baseline-off (lbox-baseline lb)))))
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

(defun %strip-cdata (s)
  "Strip an XHTML `<![CDATA[ … ]]>` wrapper from <style> text: parsed as RAWTEXT
the markers survive as literal characters and would corrupt the first selector."
  (let ((s (or s "")))
    (flet ((del (marker str)
             (loop for p = (search marker str) while p do
               (setf str (concatenate 'string (subseq str 0 p) (subseq str (+ p (length marker))))))
             str))
      (del "]]>" (del "<![CDATA[" s)))))

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
                          do (write-string (%strip-cdata (h:dnode-data c)) o) (terpri o)))
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
                                                 (and viewport-p scroll-to)
                                                 (and viewport-height (round viewport-height)))
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
