;;;; src/render/layout.lisp — block + inline-formatting layout, paint, render.
;;;;
;;;; Normal-flow layout: block-level boxes stacked vertically (margin/border/
;;;; padding), with INLINE formatting contexts that lay styled text runs into
;;;; line boxes — each fragment keeps its own color/weight/decoration, so bold,
;;;; links, and colored spans render correctly.  Mixed block+inline children are
;;;; grouped into anonymous inline runs.  List items get markers.  Painted to a
;;;; canvas and saved as PNG.
(in-package #:weft.render)

(defstruct lbox x y w h style node kind children marker img vpaint baseline rel)   ; kind :block | :line; img = decoded IMG; vpaint = (cv x y w h) replaced-content painter; baseline = px offset from a :line box's top to its shared baseline (CSS 2.1 10.8); rel = (dx . dy) inline relative-positioning visual shift for an atomic (§9.4.3)
(defstruct frag x w text style node (dx 0) (dy 0))         ; a positioned styled text run on a line; node = source DOM element (for hit-testing); dx/dy = inline relative-positioning visual shift (§9.4.3)

;;; Cumulative inline relative-positioning offset (CSS 2.1 §9.4.3) in effect while
;;; COLLECT-WORDS walks an inline run: a (dx . dy) visual shift compounded from the
;;; position:relative inline ancestors (and the atomic's own offset).  NIL = no shift.
(defvar *inline-rel* nil)
(defun rel-cons (cs)
  "The (dx . dy) relative-position shift of CS as a cons, or NIL when zero/none."
  (multiple-value-bind (dx dy) (rel-offset cs)
    (unless (and (zerop dx) (zerop dy)) (cons dx dy))))
(defun compound-rel (a b)
  "Sum two inline relative offsets (each NIL or (dx . dy)); NIL when the result is zero."
  (cond ((null a) b) ((null b) a)
        (t (let ((dx (+ (car a) (car b))) (dy (+ (cdr a) (cdr b))))
             (unless (and (zerop dx) (zerop dy)) (cons dx dy))))))

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
               (push (list payload meta pend pend-px node *inline-rel*) words) (setf pend nil pend-px 0))
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
                                                                 (and style (css:cstyle-text-transform style))
                                                                 node)
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
                         ((format-control-p c) nil)   ; invisible bidi/format control: no glyph
                         (t (write-char c b) (setf any t)))))
                   (flush))))
             (rec (n owner onode)
               (case (h:dnode-kind n)
                 (:text (emit-text (h:dnode-data n) owner onode))
                 (:element
                  (let* ((cs (or (st styles n) owner))
                         ;; compound this element's own inline relative offset (CSS 2.1
                         ;; §9.4.3) onto the ancestors' — every token emitted for it (its
                         ;; own atomic box, or the text/atomics of a relatively positioned
                         ;; inline span) carries the shift and is painted displaced, with
                         ;; flow position untouched (space is reserved where it would be).
                         (*inline-rel* (compound-rel *inline-rel* (rel-cons (st styles n)))))
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
                      ((string= (local-name (h:dnode-name n)) "svg")
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
                      ;; an inline-table is an atomic inline (CSS 2.1 §9.2.1 / §17.2):
                      ;; it lays out with the full table algorithm and participates in
                      ;; the line as a shrink-to-fit box, like an inline-block.
                      ((and cs (string= (cdisplay cs) "inline-table"))
                       (multiple-value-bind (lb adv) (layout-node n styles 0 0 content-w)
                         (declare (ignore adv))
                         (when lb (atom! lb n))))
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
                      ;; An auto-size block-level element in an inline run breaks the
                      ;; inline formatting context and lays out as a normal block
                      ;; (block-in-inline, CSS 2.1 §9.2.1.1): hoist it like flex/table
                      ;; above so the enclosing block places it on its own line.
                      ((and cs (member (cdisplay cs) '("block" "list-item") :test #'string=))
                       (push n blocks))
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

(defun combining-mark-p (ch)
  "Approximate Unicode combining-mark test (categories Mn/Mc/Me) via the common
blocks, so ::first-letter keeps a base letter and its accents together."
  (let ((c (char-code ch)))
    (or (<= #x300 c #x36F) (<= #x483 c #x489) (<= #x591 c #x5BD) (<= #x610 c #x61A)
        (<= #x64B c #x65F) (<= #x6D6 c #x6DC) (<= #x6DF c #x6E4) (<= #x900 c #x903)
        (<= #x93A c #x94F) (<= #x1AB0 c #x1AFF) (<= #x1DC0 c #x1DFF) (<= #x20D0 c #x20FF)
        (<= #xFE20 c #xFE2F))))

(defun first-letter-slice (word)
  "The ::first-letter typographic unit of WORD (CSS 2.1 §5.12.2): any leading
punctuation (Ps/Pe/Pi/Pf/Po), the first letter itself, and any punctuation that
immediately follows it.  Return the character count of that slice, or 0 when WORD
has no letter (all punctuation / empty) so the caller leaves it whole."
  (let ((n (length word)) (i 0))
    (declare (type fixnum n i))
    (loop while (and (< i n) (first-letter-punct-p (char-code (char word i)))) do (incf i))
    (if (>= i n)
        0                                   ; no base letter to anchor on
        (progn
          (incf i)                          ; the first letter (one typographic unit)
          ;; absorb combining marks that attach to the letter
          (loop while (and (< i n) (combining-mark-p (char word i))) do (incf i))
          (loop while (and (< i n) (first-letter-punct-p (char-code (char word i)))) do (incf i))
          i))))

(defun apply-first-letter (words fl-cs)
  "Restyle the ::first-letter slice of the first text token in WORDS with FL-CS,
splitting that token into a first-letter fragment (styled FL-CS) followed by its
remainder (CSS 2.1 §5.12.2).  Returns the possibly-modified WORDS list."
  (loop for cell on words
        for tok = (car cell)
        when (stringp (car tok)) do
          (let* ((word (car tok)) (k (first-letter-slice word)))
            (when (plusp k)
              (let* ((head (subseq word 0 k)) (tail (subseq word k))
                     ;; head token: inherits SPACE/GAP/NODE/REL of the original,
                     ;; carries FL-CS as its style.
                     (head-tok (list head fl-cs (tok-space tok) (tok-gap tok)
                                      (tok-node tok) (tok-rel tok))))
                (if (zerop (length tail))
                    (setf (car cell) head-tok)
                    ;; splice: head then tail (tail keeps original style; no leading
                    ;; space/gap between the two halves of one word).
                    (let ((tail-tok (list tail (tok-meta tok) nil 0
                                          (tok-node tok) (tok-rel tok))))
                      (setf (car cell) head-tok (cdr cell) (cons tail-tok (cdr cell)))))))
            (return))
        ;; a leading atomic/break before any text: first-letter does not apply
        when (not (stringp (car tok))) do (return))
  words)

;;; Inline-token accessors: (PAYLOAD META SPACE GAP) — PAYLOAD is (CAR tok) (a word
;;; string or :ATOMIC), META its style or lbox, SPACE whether whitespace preceded it,
;;; GAP extra leading px from the enclosing element's inline horizontal margins.
(declaim (inline tok-meta tok-space tok-gap tok-node tok-rel))
(defun tok-meta (tok) (cadr tok))
(defun tok-space (tok) (caddr tok))
(defun tok-gap (tok) (cadddr tok))
(defun tok-node (tok) (fifth tok))      ; source DOM element node (for hit-testing)
(defun tok-rel (tok) (sixth tok))       ; inline relative-position (dx . dy) shift, or NIL (§9.4.3)
(defun tok-dx (tok) (let ((r (tok-rel tok))) (if r (car r) 0)))
(defun tok-dy (tok) (let ((r (tok-rel tok))) (if r (cdr r) 0)))

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

(defun %noscript-fallback-src (node)
  "The real image URL for a lazy <img> whose own src/srcset are populated only by a
   scripted IntersectionObserver — never fired by a static, non-scrolling render.
   The author's <noscript> fallback (a sibling within the enclosing <picture>, or a
   following sibling in the wrapper) carries a plain <img src>; HTML 4.12.2 defines a
   <noscript>'s children as the content the document offers when the script can't act,
   so a viewportless render treats that fallback as the source.  Returns the first
   descendant <img src> of a sibling <noscript>, else NIL."
  (labels ((elem-p (n) (eq (h:dnode-kind n) :element))
           (img-src (n)
             (and (elem-p n) (string-equal (h:dnode-name n) "img")
                  (let ((v (cdr (assoc "src" (h:dnode-attrs n) :test #'string-equal))))
                    (and v (plusp (length (string-trim '(#\Space) v))) v))))
           (find-img (n)
             (when (elem-p n)
               (or (img-src n)
                   (loop for c across (h:dnode-children n) thereis (find-img c))))))
    (let ((parent (h:dnode-parent node)))
      (when (and parent (elem-p parent))
        (loop for c across (h:dnode-children parent)
              when (and (elem-p c) (string-equal (h:dnode-name c) "noscript"))
                do (let ((r (find-img c))) (when r (return r))))))))

(defun img-source-url (node)
  "The image URL for an <img>: src, else the first srcset candidate, else the
   lazy-load data-src / data-srcset — many sites defer the real URL into a data-
   attribute until a script swaps it into src on scroll, which a static render never
   triggers — else the <noscript> fallback for the script-populated lazy <img>."
  (flet ((a (name) (let ((v (cdr (assoc name (h:dnode-attrs node) :test #'string-equal))))
                     (and v (plusp (length (string-trim '(#\Space) v))) v))))
    (or (a "src") (%srcset-url (a "srcset")) (a "data-src") (%srcset-url (a "data-srcset"))
        (%noscript-fallback-src node))))

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

(defun obj-pos-off (spec avail)
  "CSS Images 3 §object-position: placement offset along one axis for content that
leaves AVAIL free space (box-size - content-size).  SPEC = (val unit) as parsed by
background-position: `%` -> that fraction of AVAIL; a length -> the offset directly;
NIL/other -> 50% (centred)."
  (if (and (consp spec) (>= (length spec) 2))
      (let ((val (first spec)) (unit (second spec)))
        (if (string= unit "%") (round (* avail (/ val 100.0))) (round val)))
      (round avail 2)))

(defun obj-pos-frac (spec)
  "Fraction 0..1 from an object-position component SPEC (`%` value), else 0.5 — used to
place the `cover` source crop when the length form has no direct pixel mapping."
  (if (and (consp spec) (>= (length spec) 2) (string= (second spec) "%"))
      (max 0.0 (min 1.0 (/ (first spec) 100.0)))
      0.5))

(defun object-fit-geom (fit iw ih dx dy dw dh &optional opos)
  "Map CSS object-fit FIT for an IW×IH image into content box (DX DY DW DH):
return (values ox oy ow oh src) — the dest rectangle to paint and a source crop
SRC=(sx sy sw sh) in image pixels (NIL = whole image).  `cover` crops the image to
the box's aspect ratio and fills it (a wide relief image → a portrait hero, no
distortion); `contain` fits the whole image inside the box; `none` paints at
intrinsic size, cropped to the box; `scale-down` is none-or-contain, whichever is
smaller.  OPOS ((xspec)(yspec)) is object-position (NIL = centred, 50% 50%); it
places the sized content within the box (or shifts the `cover` source crop).
`fill` (and any unknown value) never reaches here."
  (if (or (null iw) (null ih) (<= iw 0) (<= ih 0) (<= dw 0) (<= dh 0))
      (values dx dy dw dh nil)
      (let ((px (and (consp opos) (first opos))) (py (and (consp opos) (second opos))))
        (flet ((contain ()
                 (let* ((s (min (/ dw iw) (/ dh ih)))
                        (w (max 1 (round (* iw s)))) (h (max 1 (round (* ih s)))))
                   (values (+ dx (obj-pos-off px (- dw w))) (+ dy (obj-pos-off py (- dh h))) w h nil)))
               (cover ()
                 (let (sx sy sw sh)
                   (if (> (* iw dh) (* ih dw))       ; image wider than box → crop its width
                       (setf sh ih sw (max 1 (round (/ (* ih dw) dh))) sy 0
                             sx (round (* (- iw sw) (obj-pos-frac px))))
                       (setf sw iw sh (max 1 (round (/ (* iw dh) dw))) sx 0
                             sy (round (* (- ih sh) (obj-pos-frac py)))))
                   (values dx dy dw dh (list sx sy sw sh))))
               (none ()
                 ;; content at intrinsic size iw×ih, placed at offset (ox,oy) within
                 ;; the box, then clipped to it: visible dest rect + source crop.
                 (let* ((ox (obj-pos-off px (- dw iw))) (oy (obj-pos-off py (- dh ih)))
                        (vis-x0 (max 0 ox)) (vis-y0 (max 0 oy))
                        (vis-x1 (min dw (+ ox iw))) (vis-y1 (min dh (+ oy ih)))
                        (vw (- vis-x1 vis-x0)) (vh (- vis-y1 vis-y0)))
                   (if (or (<= vw 0) (<= vh 0))
                       (values dx dy 0 0 (list 0 0 0 0))    ; entirely outside the box
                       (values (+ dx vis-x0) (+ dy vis-y0) vw vh
                               (list (max 0 (- ox)) (max 0 (- oy)) vw vh))))))
          (cond ((string= fit "cover") (cover))
                ((string= fit "contain") (contain))
                ((string= fit "none") (none))
                ((string= fit "scale-down") (if (or (> iw dw) (> ih dh)) (contain) (none)))
                (t (values dx dy dw dh nil)))))))

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

(defun format-control-p (c)
  "T if C is a zero-width, invisible bidi/format/join control (Unicode
Default_Ignorable): it produces no glyph and no advance when rendered
(CSS Text 3 / Unicode Bidi).  Soft hyphen is excluded — it carries line-break
semantics handled elsewhere."
  (let ((u (char-code c)))
    (or (<= #x200B u #x200D)            ; ZWSP ZWNJ ZWJ
        (= u #x200E) (= u #x200F)       ; LRM RLM
        (= u #x061C)                    ; ALM
        (<= #x202A u #x202E)            ; LRE RLE PDF LRO RLO
        (<= #x2066 u #x2069)            ; LRI RLI FSI PDI
        (= u #x2060) (= u #xFEFF))))    ; WORD JOINER, ZWNBSP/BOM

(defun strip-format-controls (s)
  "Remove invisible bidi/format controls from S (see FORMAT-CONTROL-P)."
  (if (find-if #'format-control-p s) (remove-if #'format-control-p s) s))

(defun lang-prefix-p (lang code)
  "T if the (already lowercased) LANG is CODE or a CODE-prefixed subtag (e.g.
\"tr\" matches \"tr\" and \"tr-tr\")."
  (and lang (let ((n (length code)))
              (and (>= (length lang) n) (string= lang code :end1 n)
                   (or (= (length lang) n) (char= (char lang n) #\-))))))
(defun lang-script-subtag (lang)
  "The BCP47 script subtag of (lowercased) LANG — the first 4-alpha subtag — or NIL."
  (and lang (loop with start = 0
                  for pos = (position #\- lang :start start)
                  for sub = (subseq lang start (or pos (length lang)))
                  when (and (= (length sub) 4) (every #'alpha-char-p sub)) return sub
                  while pos do (setf start (1+ pos)))))
(defun latin-casing-lang-p (lang code)
  "T if LANG selects CODE's Latin-script casing tailoring: LANG is CODE (or a
CODE- subtag) whose script subtag, if present, is Latn.  An explicit non-Latin
script (e.g. tr-Cyrl) suppresses the tailoring (CSS Text 3 §2.1 languages)."
  (and (lang-prefix-p lang code)
       (let ((s (lang-script-subtag lang))) (or (null s) (string= s "latn")))))
(defun turkic-lang-p (lang) (or (latin-casing-lang-p lang "tr") (latin-casing-lang-p lang "az")))
(defun lithuanian-lang-p (lang) (latin-casing-lang-p lang "lt"))

(defun case-ignorable-char-p (c)
  "Approx Unicode Case_Ignorable: combining marks + a few format/punct chars.
Enough context for the Final_Sigma rule."
  (let ((u (char-code c)))
    (or (<= #x0300 u #x036F) (<= #x1AB0 u #x1AFF) (<= #x1DC0 u #x1DFF)
        (<= #x20D0 u #x20FF) (<= #xFE20 u #xFE2F)
        (member u '(#x0027 #x00AD #x00B7 #x02B0 #x02B1 #x2019 #x2027)))))
(defun cased-char-p (c)
  "Approx Unicode Cased: a character with a distinct upper/lower form."
  (let ((u (char-code c)))
    (or (/= (char-code (char-upcase c)) (char-code (char-downcase c)))
        (gethash u *full-upcase-map*) (gethash u *full-downcase-map*) nil)))
(defun above-mark-p (c)
  "Approx combining class 230 (Above) — the More_Above condition for Lithuanian."
  (let ((u (char-code c)))
    (or (<= #x0300 u #x0314) (<= #x033D u #x0344) (= u #x0346)
        (<= #x034A u #x034C) (<= #x0350 u #x0352) (= u #x0357)
        (= u #x035B) (<= #x0363 u #x036F))))
(defun soft-dotted-char-p (c)
  (member (char-code c) '(#x0069 #x006A #x012F #x0249 #x0268 #x029D #x02B2
                          #x03F3 #x0456 #x0458 #x1D62 #x1D96 #x1DA4 #x1DA8
                          #x1E2D #x1ECB #x2071 #x2C7C)))
(defun final-sigma-p (word i)
  "The Unicode Final_Sigma condition at index I of WORD: a cased letter precedes
\(skipping case-ignorables) and no cased letter follows."
  (and (loop for j from (1- i) downto 0 for cj = (char word j)
             do (cond ((case-ignorable-char-p cj)) ((cased-char-p cj) (return t))
                      (t (return nil)))
             finally (return nil))
       (loop for j from (1+ i) below (length word) for cj = (char word j)
             do (cond ((case-ignorable-char-p cj)) ((cased-char-p cj) (return nil))
                      (t (return t)))
             finally (return t))))

(defun full-upcase-char (out c)
  (let ((m (gethash (char-code c) *full-upcase-map*)))
    (if m (write-string m out) (write-char (char-upcase c) out))))
(defun full-downcase-char (out c)
  (let ((m (gethash (char-code c) *full-downcase-map*)))
    (if m (write-string m out) (write-char (char-downcase c) out))))

(defun transform-lower (word lang)
  "Full Unicode lowercase of WORD with Greek Final_Sigma and Turkic/Lithuanian
language tailoring (CSS Text 3 §2.1, Unicode SpecialCasing)."
  (let* ((out (make-string-output-stream)) (n (length word))
         (tr (turkic-lang-p lang)) (lt (lithuanian-lang-p lang)))
    (do ((i 0 (1+ i))) ((>= i n))
      (let* ((c (char word i)) (u (char-code c))
             (next (and (< (1+ i) n) (char word (1+ i)))))
        (cond
          ((and tr (= u #x0130)) (write-char #\i out))            ; İ -> i
          ((and tr (= u #x0049))                                  ; I
           (if (and next (= (char-code next) #x0307))
               (progn (write-char #\i out) (incf i))              ; I◌̇ -> i (drop dot)
               (write-char (code-char #x0131) out)))              ; I -> ı
          ((and lt (= u #x00CC)) (write-char (code-char #x69) out)
           (write-char (code-char #x307) out) (write-char (code-char #x300) out))
          ((and lt (= u #x00CD)) (write-char (code-char #x69) out)
           (write-char (code-char #x307) out) (write-char (code-char #x301) out))
          ((and lt (= u #x0128)) (write-char (code-char #x69) out)
           (write-char (code-char #x307) out) (write-char (code-char #x303) out))
          ((and lt (member u '(#x0049 #x004A #x012E)) next (above-mark-p next))
           (write-char (code-char (ecase u (#x0049 #x69) (#x004A #x6A) (#x012E #x12F))) out)
           (write-char (code-char #x0307) out))                   ; More_Above: insert dot
          ((= u #x03A3)                                           ; Σ final vs medial
           (write-char (code-char (if (final-sigma-p word i) #x03C2 #x03C3)) out))
          (t (full-downcase-char out c)))))
    (get-output-stream-string out)))

(defun transform-upper (word lang)
  "Full Unicode uppercase of WORD with Turkic/Lithuanian language tailoring."
  (let* ((out (make-string-output-stream)) (n (length word))
         (tr (turkic-lang-p lang)) (lt (lithuanian-lang-p lang)))
    (do ((i 0 (1+ i))) ((>= i n))
      (let* ((c (char word i)) (u (char-code c))
             (next (and (< (1+ i) n) (char word (1+ i)))))
        (cond
          ((and tr (= u #x0069)) (write-char (code-char #x0130) out)) ; i -> İ
          ((and lt (soft-dotted-char-p c) next (= (char-code next) #x0307))
           (full-upcase-char out c) (incf i))                     ; drop dot after soft-dotted
          (t (full-upcase-char out c)))))
    (get-output-stream-string out)))

(defun apply-text-transform (word transform &optional node)
  "Apply CSS text-transform to WORD (a whitespace-delimited token, so `capitalize`
   upper-cases the token's first character).  Uppercase/lowercase use the full
   Unicode case mapping (SBCL's CHAR-UPCASE/DOWNCASE are bijective-only and miss
   1->N and non-reversible cases such as ss/i-dotless/sharp-s), plus Greek
   Final_Sigma and Turkic/Lithuanian tailoring keyed on NODE's effective lang."
  (cond ((or (null transform) (string= transform "none")) word)
        ((string= transform "uppercase") (transform-upper word (and node (node-lang node))))
        ((string= transform "lowercase") (transform-lower word (and node (node-lang node))))
        ((and (string= transform "capitalize") (plusp (length word)))
         ;; capitalize titlecases only the first *letter* unit; non-letters
         ;; (e.g. enclosed alphanumerics, category So) are left untouched.
         (let ((c (char word 0)))
           (concatenate 'string
                        (if (alpha-char-p c)
                            (or (gethash (char-code c) *full-titlecase-map*)
                                (string (char-upcase c)))
                            (string c))
                        (subseq word 1))))
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
         (rtl (string= (css:cstyle-direction base-cs) "rtl"))
         (align (let ((a (css:cstyle-text-align base-cs)))
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
                                       :style (tok-meta wd) :node (tok-node wd)
                                       :dx (tok-dx wd) :dy (tok-dy wd)) cur)
                      (setf (aref ws i) (list rest (tok-meta wd) nil 0 (tok-node wd) (tok-rel wd))))))
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
                                      :style (tok-meta wd) :node (tok-node wd)
                                      :dx (tok-dx wd) :dy (tok-dy wd)) cur)
                     (setf (aref ws i) (list rest (tok-meta wd) nil 0 (tok-node wd) (tok-rel wd)))
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
                            (progn (push (make-frag :x cx :w ww :text (car wd) :style (tok-meta wd) :node (tok-node wd)
                                                    :dx (tok-dx wd) :dy (tok-dy wd)) cur)
                                   (incf cx ww) (incf i))))
                       (t (push (make-frag :x cx :w (word-w prefix (tok-meta wd)) :text prefix :style (tok-meta wd) :node (tok-node wd)
                                           :dx (tok-dx wd) :dy (tok-dy wd)) cur)
                          (cond ((plusp (length rest))
                                 (setf (aref ws i) (list rest (tok-meta wd) nil 0 (tok-node wd) (tok-rel wd)))
                                 (return))               ; remainder -> next line
                                (t (incf cx (word-w prefix (tok-meta wd))) (incf i)))))))
                  (atomic
                   (let ((lb (tok-meta wd)))
                     (shift-box lb (round (- cx (lbox-x lb))) 0)
                     (setf (lbox-rel lb) (tok-rel wd))
                     (setf line-h (max line-h (lbox-h lb))) (push lb cur))
                   (incf cx ww) (incf i))
                  (t (push (make-frag :x cx :w ww :text (car wd) :style (tok-meta wd) :node (tok-node wd)
                                      :dx (tok-dx wd) :dy (tok-dy wd)) cur)
                     (incf cx ww) (incf i))))))))
          (when (and (null cur) (not broke))     ; one item too wide for the band: force it
            (let* ((wd (aref ws i)))
              (if (eq (car wd) :atomic) (let ((lb (tok-meta wd))) (shift-box lb (round (- lx (lbox-x lb))) 0)
                                          (setf (lbox-rel lb) (tok-rel wd))
                                          (setf line-h (max line-h (lbox-h lb))) (push lb cur))
                  (push (make-frag :x lx :w (word-w (car wd) (tok-meta wd)) :text (car wd) :style (tok-meta wd) :node (tok-node wd)
                                   :dx (tok-dx wd) :dy (tok-dy wd)) cur))
              (incf i)))
          (let* ((items (nreverse cur))
                 (lastx (if items
                            (let ((it (car (last items)))) (if (frag-p it) (+ (frag-x it) (frag-w it)) (+ (lbox-x it) (lbox-w it))))
                            lx))    ; blank line (pre-line/pre-wrap forced break): empty line box
                 (used (- lastx lx))
                 ;; RTL base direction: inline-level boxes order against the base
                 ;; direction (CSS Writing Modes 4 / bidi) — reflect each item's
                 ;; position within the line band so the first logical box sits at the
                 ;; right, packed to the right edge.  (Full bidi of mixed-direction
                 ;; text is out of scope; this handles the common inline-block run.)
                 (reflect (when (and rtl items)
                            (dolist (it items)
                              (if (frag-p it)
                                  (setf (frag-x it) (- (+ lx avail) (- (frag-x it) lx) (frag-w it)))
                                  (shift-box it (round (- (- (+ lx avail) (- (lbox-x it) lx) (lbox-w it))
                                                          (lbox-x it))) 0)))
                            t))
                 (shift (if rtl
                            ;; reflected group is already right-packed (start = right in
                            ;; RTL).  weft's text-align default is "left" (CSS initial is
                            ;; the direction-relative "start"), so only an explicit center
                            ;; re-aligns; left/right/default keep the right-packed run.
                            (if (string= align "center") (- (floor (- avail used) 2)) 0)
                            (cond ((string= align "center") (max 0 (floor (- avail used) 2)))
                                  ((string= align "right") (max 0 (- avail used))) (t 0))))
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
            (progn reflect)   ; force the RTL reflection side effect (let* binding)
            (unless (zerop shift)
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
                      thereis (or (member (fourth m) '(:top :bottom))     ; line-relative box
                                  (let ((va (css:cstyle-vertical-align (lbox-style (first m)))))
                                    (and va (not (equal va '("baseline")))))))  ; any non-baseline vertical-align
                ;; a line carrying an explicit vertical-align (length/%, top,
                ;; bottom, middle, sub, super) uses the baseline model:
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
                  ;; inline relative-positioning (§9.4.3): a visual shift applied AFTER
                  ;; flow + baseline placement, so the atomic's reserved space is unmoved.
                  (dolist (it items)
                    (when (and (not (frag-p it)) (lbox-rel it))
                      (shift-box it (car (lbox-rel it)) (cdr (lbox-rel it)))))
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
                  ;; inline relative-positioning (§9.4.3): visual-only shift after
                  ;; flow + baseline placement (reserved space is unmoved).
                  (dolist (it items)
                    (when (and (not (frag-p it)) (lbox-rel it))
                      (shift-box it (car (lbox-rel it)) (cdr (lbox-rel it)))))
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
      (let* (;; left/right position the *inline* margin edges and, like top/bottom,
             ;; resolve percentages against the containing block's WIDTH (CSS 2.1
             ;; §10.3.7 / §9.3.2) — a `left:50%` was previously left unresolved (only
             ;; top/bottom were), so it fell through to the static position.
             (left (resolve-inset (css:cstyle-left cs) pw))
             (right (resolve-inset (css:cstyle-right cs) pw))
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
               ;; likewise a table's height is its own content/row model (a minimum grown
               ;; to fit, css-tables-3), never the top/bottom gap — so it is not an
               ;; auto-height fill candidate either, letting auto block-margins center it
               ;; (position-absolute-center: table with top:0;bottom:0;margin:auto).
               (auto-h (and (member (css:cstyle-height cs) '(nil :auto))
                            (not (member (cdisplay cs) '("table" "inline-table") :test #'string=))))
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
            ((string-equal (local-name name) "svg") (svg-box node cs))
            ((string-equal name "canvas") (canvas-box node cs))
            ((and (string-equal name "object") (object-data-image node))
             (object-box node cs (object-data-image node)))
            ;; <iframe> is replaced: its content is the nested document, never the
            ;; fallback children — an unsupported iframe renders as an empty box of
            ;; its own size (default 300x150), not the "FAIL" text between its tags.
            ((string-equal name "iframe")
             (let* ((w (let ((s (css::resolve-size (css:cstyle-width cs) (or avail-w 300)))) (if (numberp s) (max 0 s) 300)))
                    (hh (let ((s (css::resolve-size (css:cstyle-height cs) avail-h))) (if (numberp s) (max 0 s) 150)))
                    ;; W/HH are the CONTENT box (the used width/height, default 300x150);
                    ;; the box's border-box adds its own padding and borders (CSS 2.1
                    ;; §10.3 — box-sizing:content-box), so a bordered iframe is sized past
                    ;; its content the same as any replaced element (cf. OBJECT-BOX).
                    (bl (used-border cs :l)) (br (used-border cs :r))
                    (bt (used-border cs :t)) (bb (used-border cs :b))
                    (pl (max 0 (css::resolve-pad (css:cstyle-padding-left cs) nil)))
                    (pr (max 0 (css::resolve-pad (css:cstyle-padding-right cs) nil)))
                    (pt (max 0 (css::resolve-pad (css:cstyle-padding-top cs) nil)))
                    (pb (max 0 (css::resolve-pad (css:cstyle-padding-bottom cs) nil))))
               (make-lbox :x 0 :y 0 :w (+ bl pl w pr br) :h (+ bt pt hh pb bb)
                          :style cs :node node :kind :block)))))))

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

(defun seals-own-margins-p (cs)
  "True when a box with style CS does NOT collapse its top/bottom margins with its
in-flow children (CSS 2.1 §8.3.1): any BFC-establishing box, plus floated and
absolutely/fixed-positioned boxes (which establish a BFC too).  ESTABLISHES-BFC-P
alone omits the latter two — they must still seal a first/last child's margin so it
stays inside the box (e.g. an abspos box's margin never collapses with its child)."
  (and cs (or (establishes-bfc-p cs)
              (member (css:cstyle-position cs) '("absolute" "fixed") :test #'string=)
              (member (css:cstyle-float cs) '("left" "right") :test #'string=))))

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
    ;; Resolve deferred percentage margins against the containing block's inline
    ;; size AVAIL-W (CSS 2.1 §8.3 — all four edges resolve against width) before any
    ;; margin slot is read, so `margin-left:25%` positions the box instead of
    ;; collapsing to 0.  Idempotent (the -pct forms are kept), so a grid/flex parent
    ;; that already resolved them re-resolves to the same value.
    (css::resolve-pct-margins cs avail-w)
    ;; replaced elements (img/svg/canvas/object-image) reaching block layout — as a
    ;; block-level or out-of-flow box — are their own content; render and return.
    (let ((rb (replaced-box node cs avail-w avail-h)))
      (when rb
        ;; REPLACED-BOX builds the box at (0,0) with the bitmap on an inner content
        ;; child at coords relative to it; SHIFT-BOX moves the whole subtree so the
        ;; child ends up at absolute coords too (else the image blits at the origin).
        (if (member (css:cstyle-position cs) '("absolute" "fixed") :test #'string=)
            ;; out-of-flow: the static-flow point is (x,y); RESOLVE-POSITIONED later
            ;; applies its margins and offsets, so leave margins out here.
            (progn (shift-box rb x y)
                   (return-from %layout-core (values rb (lbox-h rb) 0 0 0)))
            ;; in-flow block-level replaced element (e.g. `img{display:block}`): honor
            ;; its margins (CSS 2.1 §10.3.3 / §10.6.2) — the border box stays at (x,y)
            ;; for the caller to place, horizontal auto margins with a definite width
            ;; center it, and the vertical margins are reported so they collapse with
            ;; siblings (a block img's margin-bottom was previously dropped).
            (let* ((mt (let ((m (css:cstyle-margin-top cs)))    (if (numberp m) m 0)))
                   (mb (let ((m (css:cstyle-margin-bottom cs))) (if (numberp m) m 0)))
                   (ml (css:cstyle-margin-left cs))
                   (mla (css:cstyle-margin-left-auto cs)) (mra (css:cstyle-margin-right-auto cs))
                   (free (max 0 (- avail-w (lbox-w rb))))
                   (offx (cond ((and mla mra) (round (/ free 2.0)))
                               (mla free)
                               ((numberp ml) ml)
                               (t 0))))
              (shift-box rb (+ x offx) y)
              (return-from %layout-core (values rb (lbox-h rb) mt mb 0))))))
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
           ;; `aspect-ratio: auto <ratio>` applies the ratio to the CONTENT box even
           ;; under box-sizing:border-box (CSS Sizing 4 §aspect-ratio); an explicit
           ;; ratio applies to the box-sizing box.  AR-BORDER-BOX = the box the ratio
           ;; transfer uses (false when the `auto` keyword forces the content box).
           (ar-border-box (and border-box (not (css:cstyle-aspect-ratio-auto cs))))
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
                                   (if ar-border-box (* (+ used-h pt pb bt bb) ar) (+ (* used-h ar) pad-bord)))
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
           (ml (cond ((and (css:cstyle-margin-left-auto cs) (css:cstyle-margin-right-auto cs)
                           (or (numberp spec-w) (numberp max-w) (and ar used-h (null spec-w)))
                           (< width avail-w))
                      (max 0 (floor (- avail-w width) 2)))
                     ;; a single auto left margin absorbs all leftover inline space
                     ;; (CSS 2.1 §10.3.3): margin-left = cb - border-box-width -
                     ;; margin-right.  WIDTH is already the border-box width, so this
                     ;; correctly uses the used content width even under border-box.
                     ((and (css:cstyle-margin-left-auto cs)
                           (or (numberp spec-w) (numberp max-w) (and ar used-h (null spec-w)))
                           (< (+ width (if (numberp mr) mr 0)) avail-w))
                      (max 0 (- avail-w width (if (numberp mr) mr 0))))
                     (t ml)))
           (content-w (max 0 (- width pad-bord)))
           ;; aspect-ratio-derived content-box height: when the box has a ratio
           ;; and an auto height, its content-box height is width/ratio (min/max
           ;; clamped).  This is a *definite* height, so a flex/grid container
           ;; hands it to children as their containing block and column-wrapping
           ;; sees it (CSS Sizing 4 §4).  NIL when height is definite or no ratio.
           (ar-h (when (and ar (null used-h))
                   (let ((rh (if ar-border-box (max 0 (- (/ width ar) pt pb bt bb))
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
                (loop for raw in (split-newlines txt) for i from 0
                      for part = (strip-format-controls raw) do
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
                            ;; a table's explicit height is a *minimum* on its grid box
                            ;; (css-tables-3 §computing-the-table-height): an empty table
                            ;; with height:100px is 100 tall even with no rows to grow.
                            ;; Under content-box the table's own padding + border sit
                            ;; OUTSIDE that grid, so height:50px + border:25px is a 100px
                            ;; border box; a border-box height already spans them.
                            (when (and tbl (numberp exp-h))
                              (setf bh (max bh (if border-box exp-h (+ exp-h pt pb bt bb)))))
                            (when (numberp max-h)
                              (setf bh (min bh (if (and tbl border-box) max-h (+ max-h pt pb bt bb)))))
                            (when (and (numberp min-h) (> min-h 0))
                              (setf bh (max bh (if (and tbl border-box) min-h (+ min-h pt pb bt bb))))))
                          bh))
                 (lb (make-lbox :x box-x :y box-y :w width :h box-h :style cs :node node
                                :kind :block :children boxes)))
            ;; A table box (display:table/-inline-table) may carry table-caption
            ;; children, which sit above/below the grid inside an anonymous table
            ;; wrapper box (CSS 2.1 §17.4).  For a caption-less table (and for flex/
            ;; grid) this returns LB unchanged.
            (if (member (cdisplay cs) '("table" "inline-table") :test #'string=)
                (multiple-value-bind (wbox adv)
                    (wrap-table-captions node styles lb box-x box-y width mt mb)
                  (return-from %layout-core (values wbox adv mt mb)))
                (return-from %layout-core (values lb (+ mt box-h mb) mt mb))))))
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
      (let ((kids (fixup-anon-tables
                   (flatten-contents
                    (multiple-value-bind (before after) (pseudo-kids node styles)
                      (append (when before (list before))
                              (coerce (h:dnode-children node) 'list)
                              (when after (list after))))
                    styles)
                   node styles))
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
                       ;; ::first-letter (CSS 2.1 §5.12.2): on the block's first
                       ;; formatted line only, restyle the leading typographic unit.
                       (let ((fl-cs (and (not content-started)
                                         (gethash (cons node :first-letter) styles))))
                         (when fl-cs (setf words (apply-first-letter words fl-cs))))
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
                                                   (not (seals-own-margins-p cs))  ; a BFC / abspos / float seals its top margin
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
              (if (and height-auto (zerop bb) (zerop pb) (not (seals-own-margins-p cs)))
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
                                   ;; border-box height -> content-box strips the
                                   ;; VERTICAL padding+border (pt pb bt bb), not the
                                   ;; horizontal PAD-BORD (a box with padding-left/right
                                   ;; only must keep its full height — box-sizing-001).
                                   (let ((eh (if border-box (- exp-h pt pb bt bb) exp-h)))
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
(defun tf-axis-aligned-p (m)
  "True when affine M (a b c d e f) is a pure positive scale/translate — off-diagonal
terms vanish AND both diagonal terms are positive — so the transform keeps the box
axis-aligned with the SAME orientation and the geometry/AABB path (which paints the
subtree upright) suffices.  A rotate/skew/general matrix — OR a reflection/180°
rotation (negative diagonal, e.g. rotate(180deg) = (-1 0 0 -1), scale(-1,1)) — flips
content and must be rasterised instead so a non-uniform fill maps correctly."
  (destructuring-bind (a b c d e f) m
    (declare (ignore e f))
    (and (< (abs b) 1.0d-4) (< (abs c) 1.0d-4)
         (> a 1.0d-4) (> d 1.0d-4))))
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
                  ;; Only an axis-aligned map (scale, projected 3D) collapses to a box
                  ;; resize — adjust it to the transformed AABB here.  A rotate/skew/
                  ;; general matrix can't: it is rasterised at paint time (see
                  ;; PAINT-TRANSFORMED), so its geometry is left untouched for the
                  ;; painter to render the subtree upright and inverse-map it.
                  (when (and m (tf-axis-aligned-p m))
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
      ;; flex-basis:N% resolves against the flex container's inner main size
      ;; (CSS Flexbox 1 §9.2.3.A); resolve-len returns NIL for percentages, which
      ;; would otherwise collapse the item to 0.
      ((and (stringp basis) (plusp (length basis)) (char= (char basis (1- (length basis))) #\%))
       (let ((p (css::parse-value "percentage" basis)))
         (if (numberp p) (* content-w (/ p 100.0)) 0)))
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
  (let* ((gap (max 0.0 (css::resolve-gap (css:cstyle-column-gap base-cs) content-w)))
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
         (gap (css::resolve-gap (css:cstyle-gap base-cs) (if row content-w (and (numberp avail-h) avail-h))))
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
                          ;; *-reverse flips the main axis: main-start is the far
                          ;; (right/bottom) edge, so flex-start packs there and
                          ;; flex-end at CX (CSS Flexbox §5.1 / §8.2).  Items are
                          ;; already reversed for traversal, so packing the whole
                          ;; run at CX+EXTRA lands its main-start edge flush right.
                          (rev (member dir '("row-reverse" "column-reverse") :test #'string=))
                          (start (cond (mauto cx)
                                       ((string= justify "center") (+ cx (/ extra 2)))
                                       ((member justify '("flex-end" "end" "right") :test #'string=)
                                        (if rev cx (+ cx extra)))
                                       ((member justify '("flex-start" "start" "left") :test #'string=)
                                        (if rev (+ cx extra) cx))
                                       ((member justify '("space-between" "space-around" "space-evenly") :test #'string=) cx)
                                       (t (if rev (+ cx extra) cx))))   ; default = flex-start
                          (between (cond (mauto 0)
                                         ((and (string= justify "space-between") (> n 1)) (/ extra (1- n)))
                                         ((string= justify "space-around") (/ extra n))
                                         ((string= justify "space-evenly") (/ extra (1+ n))) (t 0)))
                          ;; space-around insets by half a gap; space-evenly by a full
                          ;; gap (equal space before, between and after every item).
                          (x (cond ((and (not mauto) (string= justify "space-around")) (+ start (/ between 2)))
                                   ((and (not mauto) (string= justify "space-evenly")) (+ start between))
                                   (t start)))
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
                  (let ((cross-gap (css::resolve-gap (css:cstyle-row-gap base-cs) (if row (and (numberp avail-h) avail-h) content-w)))
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

(defun internal-table-display-p (d)
  "True for the internal table-box display values (CSS 2.1 §17.2): the boxes that,
when misparented (a parent that is not a table / table part), are gathered into an
anonymous table."
  (member d '("table-cell" "table-row" "table-row-group" "table-header-group"
              "table-footer-group" "table-column" "table-column-group" "table-caption")
          :test #'string=))

(defun anon-table-wrap (kids ref styles)
  "A synthetic block-level display:table wrapping a run of misparented internal
table boxes KIDS (CSS 2.1 §17.2.1 — a stray table-cell/-row/-row-group in normal
flow generates the missing anonymous table around it).  It generates no box of its
own; inheritable style comes from REF's box."
  (let ((cs (let ((c (css::copy-cstyle (or (st styles ref) (css::make-cstyle)))))
              (setf (css:cstyle-display c) "table"
                    (css:cstyle-width c) :auto (css:cstyle-height c) :auto
                    (css:cstyle-min-width c) 0.0 (css:cstyle-max-width c) :none
                    (css:cstyle-min-height c) 0.0 (css:cstyle-max-height c) :none
                    (css:cstyle-float c) "none" (css:cstyle-position c) "static"
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
    (let ((el (h::%dnode :kind :element :name "table" :children v)))
      (setf (gethash el styles) cs)
      el)))

(defun fixup-anon-tables (kids ref styles)
  "Wrap each maximal run of consecutive misparented internal table boxes among KIDS
in an anonymous block-level table (CSS 2.1 §17.2.1).  White space adjacent to a run
is dropped (white space between table boxes is not rendered).  Returns KIDS
unchanged when it holds no such boxes, so ordinary content is untouched."
  ;; Only intervene when internal table boxes are MIXED with real non-table flow
  ;; content (text or a block/inline sibling): that is when the anonymous table
  ;; must break the flow (e.g. a stray table-cell after a text run drops to its
  ;; own line).  A block whose only in-flow content is internal table boxes is
  ;; left alone — its flattened inline rendering already matches the references
  ;; these WPT anonymous-table tests use, and a real anon table would only differ
  ;; by cell-packing antialiasing.
  (flet ((internal (k) (and (eq (h:dnode-kind k) :element)
                            (let ((cs (st styles k)))
                              (and cs (internal-table-display-p (cdisplay cs))))))
         (flow-content (k)
           (or (and (eq (h:dnode-kind k) :text) (not (ws-only-text-p k)))
               (and (eq (h:dnode-kind k) :element)
                    (let ((cs (st styles k)))
                      (and cs (not (member (cdisplay cs) '("none") :test #'string=))
                           (not (member (css:cstyle-position cs) '("absolute" "fixed") :test #'string=))
                           (not (float-p styles k))))))))
  (if (not (and (some #'internal kids)
                (some (lambda (k) (and (flow-content k) (not (internal k)))) kids)))
      kids
      (let ((out '()) (run '()))
        (flet ((flush () (when run (push (anon-table-wrap (nreverse run) ref styles) out) (setf run '()))))
          (dolist (k kids)
            (cond ((and (eq (h:dnode-kind k) :element)
                        (let ((cs (st styles k))) (and cs (internal-table-display-p (cdisplay cs)))))
                   (push k run))
                  ((and run (ws-only-text-p k)))          ; drop ws inside/adjacent a run
                  (t (flush) (push k out))))
          (flush))
        (nreverse out)))))

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
  (let ((headers '()) (bodies '()) (footers '()))
    ;; Visual row order (CSS 2.1 §17.2.1): all table-header-group rows come first,
    ;; then table-row-groups / rows in document order, then table-footer-group rows
    ;; last — regardless of source position of the thead/tfoot.
    (flet ((emit (row where)
             (case where (:header (push row headers)) (:footer (push row footers))
                   (t (push row bodies)))))
      (dolist (c (effective-child-elements node styles))
        (let ((d (cdisplay (st styles c))))
          (cond ((string= d "table-row") (emit c :body))
                ((member d '("table-row-group" "table-header-group" "table-footer-group")
                         :test #'string=)
                 (let ((where (cond ((string= d "table-header-group") :header)
                                    ((string= d "table-footer-group") :footer)
                                    (t :body)))
                       (grouprows '()) (bare nil))
                   (dolist (r (flat-children c styles))
                     (cond ((and (eq (h:dnode-kind r) :element)
                                 (string= (cdisplay (st styles r)) "table-row"))
                            (push r grouprows))
                           ((cell-like-node-p r styles) (setf bare t))))
                   (cond (grouprows (dolist (r (nreverse grouprows)) (emit r where)))
                         (bare (emit c where)))))))))       ; group of bare cells = anon row
    (or (append (nreverse headers) (nreverse bodies) (nreverse footers))
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

(defun cell-rowspan (cell)
  "The td/th rowspan (>=1)."
  (let ((v (cdr (assoc "rowspan" (h:dnode-attrs cell) :test #'string-equal))))
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

(defun table-spacing (cs)
  "(values HS VS) border-spacing in px for a table with style CS (CSS 2.1 §17.6.1);
0 when border-collapse:collapse (spacing does not apply to the collapsed model)."
  (if (and cs (string= (css:cstyle-border-collapse cs) "collapse"))
      (values 0 0)
      (let ((bs (and cs (css:cstyle-border-spacing cs))))
        (if (consp bs) (values (car bs) (cdr bs)) (values 0 0)))))

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
      ;; separated-borders spacing widens the table by (ncols+1) horizontal gaps.
      (let ((hspace (* (1+ ncols) (table-spacing (st styles node)))))
        (+ hspace
           (if (plusp tot-pct)
               (let ((fixed-need (/ fixed-sum (- 1.0 (/ (min tot-pct 99.9) 100.0)))))
                 (max fixed-need pct-need (loop for i below ncols sum (aref maxs i))))
               fixed-sum))))))

(defun table-min-width (node styles avail)
  "Min-content CONTENT width of a display:table NODE: the sum of its per-column
min-content (or fixed floor) widths.  0 when it has no cells."
  (multiple-value-bind (maxs mins specs ncols) (table-column-model node styles avail)
    (declare (ignore maxs))
    (+ (* (1+ ncols) (table-spacing (st styles node)))
       (loop for i below ncols
             sum (let ((sp (aref specs i)))
                   (if (numberp sp) (max sp (aref mins i)) (aref mins i)))))))

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

(defun table-caption-boxes (node styles)
  "Direct display:table-caption element children of table NODE (CSS 2.1 §17.4)."
  (loop for c in (effective-child-elements node styles)
        when (let ((cs (st styles c))) (and cs (string= (cdisplay cs) "table-caption")))
          collect c))

(defun wrap-table-captions (node styles lb box-x box-y width mt mb)
  "Lay out NODE's table-caption children above / below its already-built grid box
LB (border box at BOX-X/BOX-Y, border-box WIDTH) per each caption's inherited
caption-side, and return (values box outer-advance).  A table with no captions is
returned unchanged (LB, its own advance) so ordinary tables keep their exact box
structure (CSS 2.1 §17.4.1).  The wrapper is an anonymous box (NIL style) so the
table element's own background/border stays on the grid box only — the caption
sits outside the table box but inside the table wrapper box."
  (let ((caps (table-caption-boxes node styles)))
    (if (null caps)
        (values lb (+ mt (lbox-h lb) mb))
        (let ((tops '()) (bots '()))
          (dolist (cap caps)
            (let* ((ccs (st styles cap))
                   (bottom (and ccs (string= (css:cstyle-caption-side ccs) "bottom"))))
              (if bottom (push cap bots) (push cap tops))))
          (setf tops (nreverse tops) bots (nreverse bots))
          (let ((y box-y) (kids '()) (top-h 0))
            (dolist (cap tops)
              (multiple-value-bind (clb adv) (layout-node cap styles box-x y width)
                (when clb (push clb kids) (incf y adv) (incf top-h adv))))
            ;; drop the grid down past the top captions, then stack bottom ones below
            (shift-box lb 0 (round (- y (lbox-y lb))))
            (push lb kids)
            (incf y (lbox-h lb))
            (let ((bot-h 0))
              (dolist (cap bots)
                (multiple-value-bind (clb adv) (layout-node cap styles box-x y width)
                  (when clb (push clb kids) (incf y adv) (incf bot-h adv))))
              (let* ((total-h (+ top-h (lbox-h lb) bot-h))
                     ;; anonymous table-wrapper box: a fresh style with no background
                     ;; or border, so the table element's own painting stays on the
                     ;; grid box LB while an inline-table's atomic placement (which
                     ;; dereferences the box style) still finds a valid CSTYLE.
                     (wrapper (make-lbox :x box-x :y box-y :w width :h total-h
                                         :style (css::make-cstyle) :node node :kind :block
                                         :children (nreverse kids))))
                (values wrapper (+ mt total-h mb)))))))))

(defun layout-table (node styles cx cy content-w base-cs &optional target-h)
  "Automatic table layout (CSS 2.1 17.5.2): columns sized to their content (or a
specified width), rows stacked, cells stretched to row height.  TARGET-H, when a
number greater than the natural row total, is the grid-box height the rows are
grown to fill (from an explicit / min height, §17.5.3).  Returns (values
cell-lboxes content-height)."
  (let ((rows (table-rows node styles)))
    (when (null rows) (return-from layout-table (values nil 0)))
    (multiple-value-bind (maxs mins specs ncols) (table-column-model node styles content-w)
      (let* ((sp (multiple-value-list (table-spacing base-cs)))
             (hs (first sp)) (vs (second sp))  ; horizontal / vertical border-spacing
             (rspecs (loop for i below ncols collect (resolve-spec (aref specs i) content-w)))
             ;; columns fit within the content width minus the (ncols+1) horizontal gaps.
             (colw (fit-columns rspecs
                                (loop for i below ncols collect (aref maxs i))
                                (loop for i below ncols collect (aref mins i))
                                (max 0 (- content-w (* (1+ ncols) hs))) ncols))
             (colx (make-array (1+ ncols) :initial-element 0.0))
             (y (+ cy vs)) (boxes '()))
        ;; col i's left edge = hs + Σ(prior colw + hs); colx[ncols] = right table edge.
        (setf (aref colx 0) (float hs))
        (loop for i below ncols do (setf (aref colx (1+ i)) (+ (aref colx i) (nth i colw) hs)))
        ;; PASS 1: lay each row's cells at the content origin CY and record its
        ;; natural height; placement is deferred so an over-tall table can grow the
        ;; rows first (§17.5.3) without re-laying their content.
        (let ((rowinfo '()) (natural 0)
              ;; rowspan occupancy: OCC[col] = rows still blocked below by a
              ;; rowspanning cell from a previous row (CSS 2.1 §17.5.1 cell grid).
              (occ (make-array (max 1 ncols) :initial-element 0))
              (spans '())    ; (lb startidx nrows natural-h) — cells to grow in pass 2
              (ridx 0))
          (dolist (row rows)
            (let ((cells (row-cells row styles node)) (rowh 0) (rowboxes '()) (col 0))
              (dolist (cell cells)
                ;; skip columns still occupied by a rowspanning cell above
                (loop while (and (< col ncols) (plusp (aref occ col))) do (incf col))
                (let* ((span (cell-colspan cell))
                       (rspan (cell-rowspan cell))
                       (x0 (aref colx (min ncols col)))
                       ;; drop the trailing gap so a cell spans its columns + the
                       ;; internal spacing only, not the gap after its last column.
                       (x1 (- (aref colx (min ncols (+ col span))) hs))
                       (cw (max 1 (round (- x1 x0)))))
                  (multiple-value-bind (lb adv) (layout-node cell styles (round (+ cx x0)) cy cw)
                    (declare (ignore adv))
                    (when lb
                      (push lb rowboxes)
                      (if (> rspan 1)
                          ;; a rowspanning cell's height spans several rows: defer its
                          ;; sizing to pass 2 and block its columns for the rows below.
                          (progn (push (list lb ridx rspan (lbox-h lb)) spans)
                                 (loop for c from col below (min ncols (+ col span))
                                       do (setf (aref occ c) rspan)))
                          (setf rowh (max rowh (lbox-h lb))))))
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
              (incf natural rowh)
              (incf ridx)
              ;; consume one row of every active rowspan occupancy
              (dotimes (c ncols) (when (plusp (aref occ c)) (decf (aref occ c))))))
          (setf rowinfo (nreverse rowinfo))
          ;; A rowspanning cell taller than the rows it covers grows the last row it
          ;; spans (CSS 2.1 §17.5.3); the common case (spanned rows sized by other
          ;; cells) leaves the row heights untouched.
          (let* ((nrows-total (length rowinfo))
                 (rh-vec (map 'vector #'third rowinfo)))
            (dolist (sp spans)
              (destructuring-bind (lb startidx nrows nat-h) sp
                (declare (ignore lb))
                (let* ((endidx (min (1- nrows-total) (+ startidx nrows -1)))
                       (sum (loop for i from startidx to endidx sum (aref rh-vec i))))
                  (when (> nat-h sum) (incf (aref rh-vec endidx) (- nat-h sum))))))
            (setf natural (loop for i below nrows-total sum (aref rh-vec i)))
          ;; PASS 2: distribute a table taller than its content across rows in
          ;; proportion to their heights (§17.5.3); with no surplus SHARE is 0 and
          ;; every row keeps its natural height, so ordinary tables are unchanged.
          (let* ((vspace (* (1+ nrows-total) vs))   ; (nrows+1) vertical border-spacing gaps
                 (eff-target (and (numberp target-h) (- target-h vspace)))
                 (surplus (if (and eff-target (> eff-target natural)) (- eff-target natural) 0))
                ;; row-group node -> (top . bottom) px band, so a table-row-group /
                ;; -header-group / -footer-group with its own background or border
                ;; paints a box behind its rows (CSS 2.1 §17.5.1 layer 2).
                (group-band (make-hash-table :test 'eq))
                ;; rowspanning cells: sized after all row tops/heights are known.
                (span-set (let ((h (make-hash-table :test 'eq)))
                            (dolist (sp spans) (setf (gethash (first sp) h) t)) h))
                (row-y (make-array nrows-total)) (row-h2 (make-array nrows-total)))
            (loop for ri in rowinfo for ridx2 from 0 do
              (destructuring-bind (row rowboxes rowh-nat) ri
                (declare (ignore rowh-nat))
                (let* ((rowh (aref rh-vec ridx2))
                       (share (cond ((<= surplus 0) 0)
                                    ((> natural 0) (* surplus (/ rowh natural)))
                                    (t (/ surplus (length rowinfo)))))
                       (rh2 (round (+ rowh share))))
                  (setf (aref row-y ridx2) y (aref row-h2 ridx2) rh2)
                  (dolist (lb rowboxes) (shift-box lb 0 (round (- y (lbox-y lb)))))
                  ;; the baseline group = the single-line text cells; shorter cells align
                  ;; to its tallest.  A row taller than that group (a block/wrapped cell,
                  ;; or a distributed surplus) has no single baseline, so cells center.
                  (let* ((bref (loop for lb in rowboxes
                                     when (and (not (gethash lb span-set))
                                               (not (cell-lbox-valign-top-p lb)) (cell-single-line-text-p lb))
                                     maximize (cell-inline-content-height lb)))
                         (center-mode (> rh2 (+ bref 1))))
                    (dolist (lb rowboxes)
                      (unless (gethash lb span-set)     ; rowspanning cells sized after the loop
                        (place-cell-content lb rh2 (and (plusp bref) bref) center-mode)
                        (setf (lbox-h lb) rh2))))                       ; stretch box to row height
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
                  ;; record this row's band under its row-group (if any) so the group
                  ;; background can span its full row set.
                  (let ((grp (and (not (eq row node)) (h:dnode-parent row))))
                    (when (and grp (eq (h:dnode-kind grp) :element)
                               (let ((gcs (st styles grp)))
                                 (and gcs (member (cdisplay gcs)
                                                  '("table-row-group" "table-header-group" "table-footer-group")
                                                  :test #'string=))))
                      (let ((cur (gethash grp group-band)))
                        (setf (gethash grp group-band)
                              (cons (if cur (min (car cur) y) y)
                                    (if cur (max (cdr cur) (+ y rh2)) (+ y rh2)))))))
                  (setf boxes (nconc boxes rowboxes))
                  (incf y (+ rh2 vs)))))
            ;; grow each rowspanning cell to cover the rows it spans, now that every
            ;; row top/height is known, and re-place its content within that band.
            (dolist (sp spans)
              (destructuring-bind (lb startidx nrows nat-h) sp
                (declare (ignore nat-h))
                (let* ((endidx (min (1- nrows-total) (+ startidx nrows -1)))
                       (h (round (- (+ (aref row-y endidx) (aref row-h2 endidx))
                                    (aref row-y startidx)))))
                  (place-cell-content lb h nil t)
                  (setf (lbox-h lb) h))))
            ;; prepend group-background boxes so they paint behind the rows/cells.
            (let ((gboxes '()) (gw (round (aref colx ncols))))
              (maphash (lambda (grp band)
                         (let ((gcs (st styles grp)))
                           (when (and gcs
                                      (or (css:cstyle-background gcs) (css:cstyle-bg-image gcs)
                                          (css:cstyle-bg-gradient gcs)
                                          (plusp (+ (used-border gcs :t) (used-border gcs :r)
                                                    (used-border gcs :b) (used-border gcs :l)))))
                             (multiple-value-bind (gdx gdy) (rel-offset gcs)
                               (push (make-lbox :x (+ (round cx) gdx) :y (+ (car band) gdy)
                                                :w gw :h (round (- (cdr band) (car band)))
                                                :style gcs :node grp :kind :block)
                                     gboxes)))))
                       group-band)
              (when gboxes (setf boxes (nconc gboxes boxes)))))
          (values boxes (- y cy))))))))

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
                   (* (css::resolve-gap (css:cstyle-gap cs) content-w) (max 0 (1- (length items))))))))
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

;;; ---- @container query resolution (CSS Containment 3) --------------------
(defun cq-container-size (lb cs)
  "Logical content-box size of a laid-out query container LB with style CS.
Returns (values PW PH IW BH) — physical width/height and inline/block sizes px."
  (let* ((bl (used-border cs :l)) (br (used-border cs :r))
         (bt (used-border cs :t)) (bb (used-border cs :b))
         (pl (css:cstyle-padding-left cs)) (pr (css:cstyle-padding-right cs))
         (pt (css:cstyle-padding-top cs)) (pb (css:cstyle-padding-bottom cs))
         (pw (max 0.0 (- (lbox-w lb) bl br pl pr)))
         (ph (max 0.0 (- (lbox-h lb) bt bb pt pb)))
         (horiz (string= (css:cstyle-writing-mode cs) "horizontal-tb")))
    (values pw ph (if horiz pw ph) (if horiz ph pw))))

(defun measure-containers (root sizes)
  "Populate SIZES (a node->size-plist hash) from the laid-out tree ROOT for every
query container (container-type size/inline-size).  Returns T if any size moved
from its previous value — the signal to iterate the container-query pass."
  (let ((changed nil))
    (labels ((walk (lb)
               (when (typep lb 'lbox)
                 (let ((node (lbox-node lb)) (cs (lbox-style lb)))
                   (when (and node cs
                              (member (css:cstyle-container-type cs) '("size" "inline-size")
                                      :test #'string=))
                     (multiple-value-bind (pw ph iw bh) (cq-container-size lb cs)
                       ;; An inline-size container contains (and can be queried on)
                       ;; only its inline axis; the block axis is unavailable (NIL).
                       (let* ((inline-only (string= (css:cstyle-container-type cs) "inline-size"))
                              (horiz (string= (css:cstyle-writing-mode cs) "horizontal-tb"))
                              (new (list :pw (if (and inline-only (not horiz)) nil pw)
                                         :ph (if (and inline-only horiz) nil ph)
                                         :iw iw :bh (if inline-only nil bh)
                                         :fs (css:cstyle-font-size cs)
                                         :wm (css:cstyle-writing-mode cs)))
                              (old (gethash node sizes)))
                         (unless (equal old new) (setf changed t))
                         (setf (gethash node sizes) new))))
                   (dolist (c (lbox-children lb)) (walk c))))))
      (walk root))
    changed))

(defun layout-with-container-queries (document styles sheet width
                                      &optional viewport-height scroll-to abs-vh)
  "Cascade + lay out, resolving @container queries via a bounded post-layout
re-cascade.  DOCUMENT is already cascaded into STYLES (with no @container
declarations applied).  When SHEET holds @container rules, measure each query
container from the laid-out tree, re-cascade with those sizes so matching
@container declarations apply, and re-lay-out — iterating up to 3 passes until
container sizes stabilise.  Returns (values ROOT ADV STYLES)."
  (multiple-value-bind (root adv) (layout-tree document styles width viewport-height scroll-to abs-vh)
    (if (css:sheet-has-container-queries-p sheet)
        (let ((sizes (make-hash-table :test 'eq)))
          (loop for iter from 0 below 3 do
            (let ((changed (measure-containers root sizes)))
              (when (and (plusp iter) (not changed)) (return))
              (let ((ns (css:compute-styles document sheet sizes)))
                (multiple-value-bind (nr na)
                    (layout-tree document ns width viewport-height scroll-to abs-vh)
                  (setf root nr adv na styles ns)))))
          (values root adv styles))
        (values root adv styles))))

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

(defun lbox-stacking-context-p (lb cs)
  "True when LB establishes a stacking context (CSS 2.1 §9.9.1 / css-position-3):
a positioned box with a numeric z-index, opacity < 1, or a rasterised transform.
A NON-context box does not isolate its positioned descendants' z-order — their
negative z-index refers to the ancestor context, so they paint behind LB itself."
  (and cs
       ;; z-index defaults to 0 in weft (CSS `auto`), so only a NONZERO z-index on a
       ;; positioned box counts as an explicit stacking context (auto/0 do not).
       (or (and (numberp (css:cstyle-z-index cs)) (not (zerop (css:cstyle-z-index cs)))
                (member (css:cstyle-position cs) '("relative" "absolute" "fixed") :test #'string=))
           (let ((o (css:cstyle-opacity cs))) (and (numberp o) (< o 1.0)))
           (box-raster-transform-matrix lb))))

(defun lbox-hoist-neg (lb cs)
  "Direct children of LB that must paint BEHIND LB's own background: negative
z-index positioned boxes, when LB does not itself establish a stacking context
(so they belong to the ancestor context — e.g. an abs z-index:-1 child sits behind
its position:relative z-index:auto parent, CSS 2.1 §9.9.1)."
  (unless (lbox-stacking-context-p lb cs)
    (remove-if-not (lambda (c) (and (lbox-positioned-p c) (minusp (lbox-z c))))
                   (lbox-children lb))))

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
        ;; CSS 2.1 §14.2.1: position % = percentage * (area - image), which is
        ;; NEGATIVE when the image is larger than the positioning area (the image
        ;; is pulled partly off the near edge).  Do NOT clamp AVAIL to 0.
        (cond ((string= unit "%") (round (* (/ val 100.0) avail)))
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

(defun effective-bg-clip (cs)
  "The background-clip keyword for the bottom-most background layer (CSS Backgrounds
3 §2.11.2 — background-color uses that layer's clip).  With a comma-valued
background-clip, layer L uses the (L mod count)-th value."
  (let ((lst (css:cstyle-bg-clip-list cs)))
    (if lst
        (nth (mod (1- (css:cstyle-bg-layers cs)) (length lst)) lst)
        (css:cstyle-bg-clip cs))))

(defun bg-box-edges (lb cs box)
  "Edges (values x0 y0 x1 y1) of LB's BOX area (a background-origin/-clip keyword):
border-box is the full box; padding-box insets by the borders; content-box also by
the padding (CSS Backgrounds 3 §2.1)."
  (let* ((bl (used-border cs :l)) (bt (used-border cs :t))
         (br (used-border cs :r)) (bb (used-border cs :b))
         (pl (if (string= box "content-box") (max 0 (css::resolve-pad (css:cstyle-padding-left cs) nil)) 0))
         (pt (if (string= box "content-box") (max 0 (css::resolve-pad (css:cstyle-padding-top cs) nil)) 0))
         (pr (if (string= box "content-box") (max 0 (css::resolve-pad (css:cstyle-padding-right cs) nil)) 0))
         (pb (if (string= box "content-box") (max 0 (css::resolve-pad (css:cstyle-padding-bottom cs) nil)) 0))
         (inl (if (string= box "border-box") 0 (+ bl pl)))
         (int (if (string= box "border-box") 0 (+ bt pt)))
         (inr (if (string= box "border-box") 0 (+ br pr)))
         (inb (if (string= box "border-box") 0 (+ bb pb))))
    (values (round (+ (lbox-x lb) inl)) (round (+ (lbox-y lb) int))
            (round (- (+ (lbox-x lb) (lbox-w lb)) inr))
            (round (- (+ (lbox-y lb) (lbox-h lb)) inb)))))

(defun grad-tile-size (bs pw ph)
  "Concrete gradient tile (w h) in a PW×PH positioning area under background-size BS.
A gradient has no intrinsic size/ratio, so auto/contain/cover collapse to the area;
explicit lengths/percentages size the tile (CSS Images 3 §4 default sizing)."
  (flet ((c (spec avail) (cond ((numberp spec) (float spec 1.0))
                               ((and (consp spec) (eq (first spec) :percent)) (* avail (/ (second spec) 100.0)))
                               (t nil))))          ; :auto / unknown -> area dim
    (cond ((consp bs) (values (or (c (first bs) pw) pw) (or (c (second bs) ph) ph)))
          (t (values pw ph)))))

(defun bg-repeat-axis-mode (rep axis)
  "Per-AXIS (:x/:y) repeat mode :repeat | :norepeat | :space from a background-repeat
keyword (CSS Backgrounds 3 §3.4)."
  (cond ((string= rep "no-repeat") :norepeat)
        ((string= rep "space") :space)
        ((string= rep "repeat-x") (if (eq axis :x) :repeat :norepeat))
        ((string= rep "repeat-y") (if (eq axis :y) :repeat :norepeat))
        (t :repeat)))                        ; repeat / round (approx) / unknown

(defun bg-axis-origins (mode astart alen tile off clip0 clip1)
  "Tile origin coordinates along one axis: repeated across [CLIP0,CLIP1], a single
positioned tile, or `space`-distributed whole tiles within the ALEN positioning area
at ASTART (edge-to-edge, equal gaps; CSS Backgrounds 3 §3.4 <repeat-style> space)."
  (ecase mode
    (:norepeat (list (+ astart off)))
    (:space (let ((n (if (> tile 0) (floor alen tile) 0)))
              ;; gap is derived from the positioning area, but the spaced pattern then
              ;; fills the whole painting/clip area (tiles repeat behind the border).
              (if (<= n 1) (list (+ astart off))
                  (let* ((gap (/ (- alen (* n tile)) (1- n)))
                         (period (+ tile gap))
                         (start (- astart (* period (ceiling (- astart clip0) period)))))
                    (loop for t0 = start then (+ t0 period)
                          while (< t0 clip1) when (> (+ t0 tile) clip0) collect t0)))))
    (:repeat (let* ((o (+ astart off))
                    (start (- o (* tile (ceiling (- o clip0) tile)))))
               (loop for t0 = start then (+ t0 tile)
                     while (< t0 clip1) when (> (+ t0 tile) clip0) collect t0)))))

(defun tile-gradient (cv grad curcolor ax0 ay0 aw ah cx0 cy0 cx1 cy1 rep bs pos)
  "Rasterise GRAD tiled: tile sized by background-size BS in the AW×AH positioning
area anchored at (AX0,AY0), offset by background-position POS, repeated per REP,
clipped to (CX0 CY0 CX1 CY1).  A sub-pixel-but-positive tile clamps to 1px (so a
green→green gradient at background-size:0.2px still fills, not vanishes)."
  (multiple-value-bind (iw ih) (grad-tile-size bs aw ah)
    (setf iw (if (> iw 0) (max 1 (round iw)) 0)
          ih (if (> ih 0) (max 1 (round ih)) 0))
    (when (and (> cx1 cx0) (> cy1 cy0) (> iw 0) (> ih 0))
      (let* ((offx (if pos (bg-pos-offset (first pos) (- aw iw)) 0))
             (offy (if pos (bg-pos-offset (second pos) (- ah ih)) 0))
             (xs (bg-axis-origins (bg-repeat-axis-mode rep :x) ax0 aw iw offx cx0 cx1))
             (ys (bg-axis-origins (bg-repeat-axis-mode rep :y) ay0 ah ih offy cy0 cy1))
             (*clip* (clip-intersect cx0 cy0 cx1 cy1))
             (budget 200000))
        (block tiles
          (dolist (ty ys)
            (when (and (< ty cy1) (> (+ ty ih) cy0))
              (dolist (tx xs)
                (when (and (< tx cx1) (> (+ tx iw) cx0))
                  (fill-css-gradient cv tx ty iw ih grad curcolor)
                  (when (<= (decf budget) 0) (return-from tiles)))))))))))

(defun paint-bg-gradient-core (cv lb cs grad origin clip rep size pos)
  "Paint GRAD as one background-image layer: rasterise into its tile (background-size
SIZE), positioned by POS within the background-origin box ORIGIN, repeated per REP,
clipped to the background-clip box CLIP (CSS Backgrounds 3 §3).  The per-layer
properties are passed explicitly so a comma-separated multi-layer background paints
each layer with its own values."
  (multiple-value-bind (px0 py0 px1 py1) (bg-box-edges lb cs origin)
    (multiple-value-bind (cx0 cy0 cx1 cy1) (bg-box-edges lb cs clip)
      (tile-gradient cv grad (css:cstyle-color cs) px0 py0 (- px1 px0) (- py1 py0)
                     cx0 cy0 cx1 cy1 rep size pos))))

(defun paint-bg-gradient (cv lb cs grad)
  "Single-layer wrapper: paint GRAD using CS's background-* slots (byte-identical
to the pre-multilayer path)."
  (paint-bg-gradient-core cv lb cs grad (css:cstyle-bg-origin cs) (css:cstyle-bg-clip cs)
                          (css:cstyle-bg-repeat cs) (css:cstyle-bg-size cs) (css:cstyle-bg-position cs)))

(defun paint-bg-image-core (cv lb cs url origin clip rep size pos)
  "Decode URL (data: URI or network image) and tile it across the background
positioning area (background-origin), clipped to the painting area (background-clip),
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
             (repx (member rep '("repeat" "repeat-x") :test #'string=))
             (repy (member rep '("repeat" "repeat-y") :test #'string=)))
        ;; positioning area = background-origin box (default padding-box)
        (multiple-value-bind (px0 py0 px1 py1) (bg-box-edges lb cs origin)
        ;; painting/clip area = background-clip box (default border-box)
        (multiple-value-bind (cx0 cy0 cx1 cy1) (bg-box-edges lb cs clip)
        (multiple-value-bind (iw ih)   ; effective tile size after background-size
            (bg-tile-size size img (- px1 px0) (- py1 py0))
          (let* ((offx (if pos (bg-pos-offset (first pos) (- (- px1 px0) iw)) 0))
                 (offy (if pos (bg-pos-offset (second pos) (- (- py1 py0) ih)) 0))
                 (ox (+ px0 offx)) (oy (+ py0 offy)))
            ;; a degenerate tile (0 wide or tall — e.g. a zero-height-ratio SVG under
            ;; `background-size` with an auto axis) paints nothing.
            (when (and (> cx1 cx0) (> cy1 cy0) (> iw 0) (> ih 0))
              ;; clip to the background-clip (painting) box; the tile grid is
              ;; positioned against the origin box but repeats to fill the clip box.
              (let ((*clip* (clip-intersect cx0 cy0 cx1 cy1)))
                ;; common case: an intrinsic 1x1 image filling the box — solid rect.
                (if (and (= iw0 1) (= ih0 1) (null size) (>= (aref (img-rgba img) 3) 255))
                    (let ((r (aref (img-rgba img) 0)) (g (aref (img-rgba img) 1)) (b (aref (img-rgba img) 2)))
                      (fill-rect cv (if repx cx0 ox) (if repy cy0 oy)
                                 (if repx (- cx1 cx0) 1) (if repy (- cy1 cy0) 1) (list r g b)))
                    ;; general tiling, each tile scaled to the effective (IW IH).
                    ;; A tile-count cap bounds a pathological fine tiling (a tiny tile
                    ;; over a large area) so a degenerate background can't stall paint.
                    (let ((startx (if repx (- ox (* iw (ceiling (- ox cx0) iw))) ox))
                          (starty (if repy (- oy (* ih (ceiling (- oy cy0) ih))) oy))
                          (budget 200000))
                      (block tiles
                        (loop for ty = starty then (+ ty ih)
                              while (and (< ty cy1) (or repy (= ty starty))) do
                          (loop for tx = startx then (+ tx iw)
                                while (and (< tx cx1) (or repx (= tx startx))) do
                            (when (and (> (+ tx iw) cx0) (> (+ ty ih) cy0))
                              (blit-img cv img tx ty iw ih)
                              (when (<= (decf budget) 0) (return-from tiles))))))))))))))))))

(defun paint-bg-image (cv lb cs url)
  "Single-layer wrapper: tile URL using CS's background-* slots (byte-identical
to the pre-multilayer path)."
  (paint-bg-image-core cv lb cs url (css:cstyle-bg-origin cs) (css:cstyle-bg-clip cs)
                       (css:cstyle-bg-repeat cs) (css:cstyle-bg-size cs) (css:cstyle-bg-position cs)))

(defun paint-bg-image-fixed-core (cv lb cs url rep pos)
  "Tile URL's image as a background-attachment:fixed background: the tile grid is
anchored to the VIEWPORT origin (canvas 0,0) plus the background-position offset POS,
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
             (repx (member rep '("repeat" "repeat-x") :test #'string=))
             (repy (member rep '("repeat" "repeat-y") :test #'string=))
             ;; border box of the element (the clip region)
             (bx0 (round (lbox-x lb))) (by0 (round (lbox-y lb)))
             (bx1 (round (+ (lbox-x lb) (lbox-w lb))))
             (by1 (round (+ (lbox-y lb) (lbox-h lb))))
             ;; tile origin: anchored to the viewport (canvas 0,0) plus the
             ;; background-position offset, computed against the viewport (canvas).
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

(defun paint-bg-image-fixed (cv lb cs url)
  "Single-layer wrapper: fixed-attachment tile of URL using CS's slots."
  (paint-bg-image-fixed-core cv lb cs url (css:cstyle-bg-repeat cs) (css:cstyle-bg-position cs)))

(defun paint-one-bg-layer (cv lb cs layer radii)
  "Paint one background-image LAYER (a CSS:BG-LAYER) over the already-painted backdrop,
clipped to the layer's background-clip box (rounded per RADII, NIL = rectangular fast
path).  Source-over compositing — BLEND modes are handled by PAINT-BG-LAYERS.  Routes
gradients and url() images through the shared per-layer cores, so a single layer paints
byte-identically to the pre-multilayer path."
  (let* ((clip (css:bg-layer-clip layer))
         (rg (and radii (bg-round-region lb cs clip radii)))
         (*round-clip* (if rg (cons rg *round-clip*) *round-clip*))
         (img (css:bg-layer-image layer)))
    (cond
      ((null img) nil)
      ((eq (car img) :gradient)
       (when (gradient-visible-p (cdr img))
         (paint-bg-gradient-core cv lb cs (cdr img) (css:bg-layer-origin layer) clip
                                 (css:bg-layer-repeat layer) (css:bg-layer-size layer)
                                 (css:bg-layer-position layer))))
      ((eq (car img) :url)
       (if (string-equal (css:bg-layer-attachment layer) "fixed")
           (paint-bg-image-fixed-core cv lb cs (cdr img) (css:bg-layer-repeat layer)
                                      (css:bg-layer-position layer))
           (paint-bg-image-core cv lb cs (cdr img) (css:bg-layer-origin layer) clip
                                (css:bg-layer-repeat layer) (css:bg-layer-size layer)
                                (css:bg-layer-position layer)))))))

(defun primary-bg-layers (cs)
  "The layer-1 (topmost) background-image layer(s) built from CS's single-value bg-*
slots, in bottom→top paint order.  A gradient and a url image can both be present
(gradient below the image, matching the pre-multilayer paint order); each becomes a
BG-LAYER carrying CS's shared position/size/repeat/origin/clip/attachment."
  (let ((out '()))
    (when (css:cstyle-bg-gradient cs)
      (push (css:make-bg-layer :image (cons :gradient (css:cstyle-bg-gradient cs))
                               :position (css:cstyle-bg-position cs) :size (css:cstyle-bg-size cs)
                               :repeat (css:cstyle-bg-repeat cs) :origin (css:cstyle-bg-origin cs)
                               :clip (css:cstyle-bg-clip cs) :attachment (css:cstyle-bg-attachment cs))
            out))
    (when (css:cstyle-bg-image cs)
      (push (css:make-bg-layer :image (cons :url (css:cstyle-bg-image cs))
                               :position (css:cstyle-bg-position cs) :size (css:cstyle-bg-size cs)
                               :repeat (css:cstyle-bg-repeat cs) :origin (css:cstyle-bg-origin cs)
                               :clip (css:cstyle-bg-clip cs) :attachment (css:cstyle-bg-attachment cs))
            out))
    (nreverse out)))

(defun effective-bg-layers (cs)
  "The full ordered background-image layer list in BOTTOM→TOP paint order: the extra
layers 2..N (BG-EXTRA-LAYERS is listed top→bottom, so bottom-most last → reverse it
first) below the layer-1 primary layer(s) from the single-value slots on top.  The
blend keyword for each layer is threaded from BG-BLEND-LIST by its LISTED index."
  (let* ((extras (css:cstyle-bg-extra-layers cs))          ; listed order: layer 2 .. N
         (prim (primary-bg-layers cs))                     ; layer 1 (bottom→top for a grad+img pair)
         ;; bottom→top = reverse(extras) then primary
         (ordered (append (reverse extras) prim))
         (blends (css:cstyle-bg-blend-list cs)))
    ;; attach per-layer blend by LISTED index (layer 1 = listed 0, extras = 1..)
    (when blends
      (let ((nb (length blends))
            ;; listed order top→bottom = primary(reversed to top-first) then extras
            (listed (append (reverse prim) extras)))
        (loop for lyr in listed for i from 0
              do (setf (css:bg-layer-blend lyr) (nth (mod i nb) blends)))))
    ordered))

(defun paint-bg-layers (cv lb cs radii)
  "Paint the background-image layers over the already-painted background colour, from
bottom-most to top-most (CSS Backgrounds 3 §3.11.2).  A layer with a non-normal
background-blend-mode blends against the accumulated backdrop; a normal layer composites
source-over.  Single-layer, all-normal backgrounds paint byte-identically to the
pre-multilayer path."
  (dolist (layer (effective-bg-layers cs))
    (let ((mode (css:bg-layer-blend layer)))
      (if (and mode (not (eq mode :normal)))
          (paint-bg-layer-blended cv lb cs layer radii)
          (paint-one-bg-layer cv lb cs layer radii)))))

(defun paint-bg-layer-blended (cv lb cs layer radii)
  "CSS Compositing 1 §background-blend-mode: paint one background LAYER blended against
the backdrop already on CV (the background colour + lower layers).  The layer is
rendered alone over black + white to recover its straight colour + coverage (the same
trick as PAINT-BLENDED), then each covered pixel is blended with the CV backdrop via the
layer's blend mode.  Clipped to the layer's background-clip box (rounded per RADII)."
  (let ((mode (css:bg-layer-blend layer)))
    (multiple-value-bind (bx0 by0 bx1 by1) (bg-box-edges lb cs (css:bg-layer-clip layer))
      (let ((sw (- bx1 bx0)) (sh (- by1 by0)))
        (when (and (plusp sw) (plusp sh) (<= (* sw sh) 16000000))
          (let ((offa (make-canvas sw sh '(0 0 0)))
                (offb (make-canvas sw sh '(255 255 255))))
            (shift-box lb (- bx0) (- by0))
            (let ((*clip* nil) (*round-clip* nil))
              (paint-one-bg-layer offa lb cs layer radii)
              (paint-one-bg-layer offb lb cs layer radii))
            (shift-box lb bx0 by0)
            (let ((pa (canvas-pixels offa)) (pb (canvas-pixels offb))
                  (dpx (canvas-pixels cv)) (cw (canvas-width cv)) (chh (canvas-height cv)))
              (dotimes (sy sh)
                (let ((dy (+ by0 sy)))
                  (when (and (>= dy 0) (< dy chh))
                    (dotimes (sx sw)
                      (let ((dx (+ bx0 sx)))
                        (when (and (>= dx 0) (< dx cw))
                          (let* ((i (* 3 (+ (* sy sw) sx)))
                                 (ar (aref pa i)) (ag (aref pa (+ i 1))) (ab (aref pa (+ i 2)))
                                 (br (aref pb i)) (bg (aref pb (+ i 1))) (bb (aref pb (+ i 2)))
                                 (cov (- 1.0 (/ (+ (- br ar) (- bg ag) (- bb ab)) 765.0))))
                            (when (> cov 0.004)
                              (let* ((di (* 3 (+ (* dy cw) dx)))
                                     (kr (aref dpx di)) (kg (aref dpx (+ di 1))) (kb (aref dpx (+ di 2))))
                                (flet ((cc (v) (min 255 (max 0 (round (/ v cov))))))
                                  (blend-put cv dx dy
                                             (blend-channel mode kr (cc ar))
                                             (blend-channel mode kg (cc ag))
                                             (blend-channel mode kb (cc ab))
                                             (min 255 (max 0 (round (* 255.0 cov))))))))))))))))))))))

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

;;;; ---- border-radius geometry (CSS Backgrounds 3 §5.5) ---------------------
(defun box-has-radius-p (cs)
  "True when CS declares any corner radius.  A fast gate: rounded-corner paint is
skipped entirely for every box without it, so unrounded boxes render byte-identically.
A collapsed-border table (or its internal table elements) ignores border-radius
(CSS Backgrounds 3 §5.5), so radius is suppressed there."
  (and cs (or (css:cstyle-border-tl-radius cs) (css:cstyle-border-tr-radius cs)
              (css:cstyle-border-br-radius cs) (css:cstyle-border-bl-radius cs))
       (not (and (equal (css:cstyle-border-collapse cs) "collapse")
                 (member (css:cstyle-display cs)
                         '("table" "inline-table" "table-cell" "table-row" "table-row-group"
                           "table-header-group" "table-footer-group" "table-column"
                           "table-column-group" "table-caption")
                         :test #'equal)))))

(defun resolve-radius-comp (comp ref)
  "px value of a radius component (px float | (:percent N) | NIL) against REF length."
  (cond ((null comp) 0.0)
        ((numberp comp) (float comp 1.0))
        ((and (consp comp) (eq (first comp) :percent)) (* (/ (second comp) 100.0) (float ref 1.0)))
        (t 0.0)))

(defun corner-radii-px (cs w h)
  "The four corner radii (px) for CS over a W×H border box, with the §5.5 overlap
clamp applied: if adjacent radii exceed a side, all radii scale by f=min(side/sum).
Returns a simple-vector #(tlx tly trx try brx bry blx bly)."
  (let* ((tl (css:cstyle-border-tl-radius cs)) (tr (css:cstyle-border-tr-radius cs))
         (br (css:cstyle-border-br-radius cs)) (bl (css:cstyle-border-bl-radius cs))
         (tlx (resolve-radius-comp (car tl) w)) (tly (resolve-radius-comp (cdr tl) h))
         (trx (resolve-radius-comp (car tr) w)) (tryy (resolve-radius-comp (cdr tr) h))
         (brx (resolve-radius-comp (car br) w)) (bry (resolve-radius-comp (cdr br) h))
         (blx (resolve-radius-comp (car bl) w)) (bly (resolve-radius-comp (cdr bl) h))
         (f 1.0))
    (flet ((cap (sum len) (when (and (> sum 0.0) (> sum (float len 1.0)))
                            (setf f (min f (/ (float len 1.0) sum))))))
      (cap (+ tlx trx) w) (cap (+ blx brx) w)     ; top / bottom sides
      (cap (+ tly bly) h) (cap (+ tryy bry) h))   ; left / right sides
    (if (< f 1.0)
        (vector (* tlx f) (* tly f) (* trx f) (* tryy f) (* brx f) (* bry f) (* blx f) (* bly f))
        (vector tlx tly trx tryy brx bry blx bly))))

(defun inset-radii (radii il it ir ib)
  "Reduce border-box RADII to a box inset by IL/IT/IR/IB (the border[+padding] on
each side), flooring each at 0 — the inner border/padding corner curve (§5.5)."
  (flet ((n (x) (max 0.0 (float x 1.0))))
    (vector (n (- (aref radii 0) il)) (n (- (aref radii 1) it))    ; TL
            (n (- (aref radii 2) ir)) (n (- (aref radii 3) it))    ; TR
            (n (- (aref radii 4) ir)) (n (- (aref radii 5) ib))    ; BR
            (n (- (aref radii 6) il)) (n (- (aref radii 7) ib)))))  ; BL

(defun make-round-region (x0 y0 x1 y1 radii)
  "A rounded-clip region vector for rect (X0 Y0 X1 Y1) with an 8-element RADII vector."
  (vector (float x0 1.0) (float y0 1.0) (float x1 1.0) (float y1 1.0)
          (aref radii 0) (aref radii 1) (aref radii 2) (aref radii 3)
          (aref radii 4) (aref radii 5) (aref radii 6) (aref radii 7)))

(defun round-region (x0 y0 x1 y1 radii)
  "MAKE-ROUND-REGION, or NIL when RADII are all zero (a plain rect needs no clip)."
  (when (some #'plusp radii) (make-round-region x0 y0 x1 y1 radii)))

(defun bg-round-region (lb cs box radii)
  "Rounded-clip region for LB's background-clip BOX (border-/padding-/content-box)
given the border-box RADII, or NIL if no rounding remains after insetting."
  (multiple-value-bind (x0 y0 x1 y1) (bg-box-edges lb cs box)
    (if (string= box "border-box")
        (round-region x0 y0 x1 y1 radii)
        (let* ((bl (used-border cs :l)) (bt (used-border cs :t))
               (br (used-border cs :r)) (bb (used-border cs :b))
               (content (string= box "content-box"))
               (pl (if content (max 0 (css::resolve-pad (css:cstyle-padding-left cs) nil)) 0))
               (pt (if content (max 0 (css::resolve-pad (css:cstyle-padding-top cs) nil)) 0))
               (pr (if content (max 0 (css::resolve-pad (css:cstyle-padding-right cs) nil)) 0))
               (pb (if content (max 0 (css::resolve-pad (css:cstyle-padding-bottom cs) nil)) 0)))
          (round-region x0 y0 x1 y1 (inset-radii radii (+ bl pl) (+ bt pt) (+ br pr) (+ bb pb)))))))

(defun clip-lp-px (v ref &optional (origin 0.0))
  "Resolve a clip-path length-percentage V (px float | (:pct . N)) to a device px
coordinate: ORIGIN + px, or ORIGIN + N% of REF."
  (+ origin (cond ((numberp v) (float v 1.0))
                  ((and (consp v) (eq (car v) :pct)) (* (/ (cdr v) 100.0) (float ref 1.0)))
                  (t 0.0))))

(defun clip-shape-radius (spec ref-w ref-h cx cy x0 y0 x1 y1)
  "Resolve a circle/ellipse <shape-radius> SPEC on the axis whose REF is REF-W (px |
\(:pct . N) | :closest-side | :farthest-side).  CX/CY the centre, (X0 Y0 X1 Y1) the box."
  (cond ((numberp spec) (float spec 1.0))
        ((and (consp spec) (eq (car spec) :pct)) (* (/ (cdr spec) 100.0) (float ref-w 1.0)))
        ((eq spec :closest-side) (min (abs (- cx x0)) (abs (- x1 cx))
                                      (abs (- cy y0)) (abs (- y1 cy))))
        ((eq spec :farthest-side) (max (abs (- cx x0)) (abs (- x1 cx))
                                       (abs (- cy y0)) (abs (- y1 cy))))
        (t 0.0)))

(defun clip-path-region (lb spec)
  "Build a *ROUND-CLIP* region for CS's clip-path SPEC over LB's border box (the
default reference box), or NIL if it degenerates.  Lengths/percentages resolve here
because the box geometry is only known at paint (CSS Masking 1 §clip-path)."
  (let* ((x0 (float (lbox-x lb) 1.0)) (y0 (float (lbox-y lb) 1.0))
         (w (float (lbox-w lb) 1.0)) (h (float (lbox-h lb) 1.0))
         (x1 (+ x0 w)) (y1 (+ y0 h)))
    (case (car spec)
      (:inset
       (destructuring-bind (top right bottom left) (cdr spec)
         (let ((ix0 (clip-lp-px left w x0)) (iy0 (clip-lp-px top h y0))
               (ix1 (- x1 (clip-lp-px right w))) (iy1 (- y1 (clip-lp-px bottom h))))
           (when (and (< ix0 ix1) (< iy0 iy1))
             (make-round-region ix0 iy0 ix1 iy1 #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0))))))
      (:circle
       (destructuring-bind (rad cx cy) (cdr spec)
         (let* ((pcx (clip-lp-px cx w x0)) (pcy (clip-lp-px cy h y0))
                ;; circle radius resolves against sqrt(w²+h²)/√2 for percentages
                (r (cond ((and (consp rad) (eq (car rad) :pct))
                          (* (/ (cdr rad) 100.0) (/ (sqrt (+ (* w w) (* h h))) (sqrt 2.0))))
                         (t (clip-shape-radius rad w h pcx pcy x0 y0 x1 y1)))))
           (when (> r 0.0) (list :circle pcx pcy (float r 1.0) (float r 1.0))))))
      (:ellipse
       (destructuring-bind (rx ry cx cy) (cdr spec)
         (let* ((pcx (clip-lp-px cx w x0)) (pcy (clip-lp-px cy h y0))
                (prx (clip-shape-radius rx w h pcx pcy x0 y0 x1 y1))
                (pry (clip-shape-radius ry h w pcy pcx y0 x0 y1 x1)))
           (when (and (> prx 0.0) (> pry 0.0))
             (list :circle pcx pcy (float prx 1.0) (float pry 1.0))))))
      (:polygon
       (let* ((pts (cdr spec))
              (v (map 'simple-vector
                      (lambda (p) (cons (clip-lp-px (car p) w x0) (clip-lp-px (cdr p) h y0)))
                      pts))
              (bx0 x1) (by0 y1) (bx1 x0) (by1 y0))
         (when (>= (length v) 3)
           (loop for p across v do
             (setf bx0 (min bx0 (car p)) by0 (min by0 (cdr p))
                   bx1 (max bx1 (car p)) by1 (max by1 (cdr p))))
           (list :polygon bx0 by0 (+ bx1 1.0) (+ by1 1.0) v))))
      (t nil))))

(defun fill-round-ring (cv outer inner color)
  "Fill the ring OUTER minus INNER (both rounded regions) with the uniform COLOR — a
solid rounded border (§5.5).  INNER may be NIL (a filled rounded rect).  Hard-edged
\(binary centre test), matching weft's hard-edged rectangular border fill so a rounded
border aligns with an identically-constructed reference and does not leak the layers
beneath it at the corners.  Ancestor rounded clip / rect *CLIP* are honoured via PUT."
  (let ((x0 (max 0 (floor (svref outer 0)))) (y0 (max 0 (floor (svref outer 1))))
        (x1 (min (canvas-width cv) (ceiling (svref outer 2))))
        (y1 (min (canvas-height cv) (ceiling (svref outer 3))))
        (r (first color)) (g (second color)) (b (third color))
        (*round-clip* (cons outer *round-clip*)))
    (loop for yy from y0 below y1 do
      (loop for xx from x0 below x1 do
        (unless (and inner (region-contains-f inner (+ xx 0.5) (+ yy 0.5)))
          (put cv xx yy r g b))))))

;;;; ---- box-shadow (CSS Backgrounds 3 §7) ------------------------------------
(defun blur-box-radius (blur)
  "Integer box-blur radius R for a CSS blur-radius BLUR (px).  Three box-blur passes of
radius R approximate a Gaussian of std-dev sigma=BLUR/2 (their combined variance
R^2+R = sigma^2), so R ~= BLUR/2 for large blurs — matching the reference rasteriser
closely enough for the fuzzy grader."
  (if (<= blur 0.5)
      0
      (let ((sigma (/ blur 2.0)))
        (max 0 (round (/ (- (sqrt (+ 1.0 (* 4.0 sigma sigma))) 1.0) 2.0))))))

(defun %hbox-blur (src dst w h r)
  "One horizontal box-blur of radius R (window 2R+1, out-of-range = 0) over the W×H
alpha buffer SRC into DST, using a running sum (O(w*h))."
  (declare (type (simple-array (unsigned-byte 8) (*)) src dst) (type fixnum w h r)
           (optimize (speed 3) (safety 0)))
  (let ((win (+ (* 2 r) 1)))
    (declare (type fixnum win))
    (dotimes (y h)
      (let ((base (the fixnum (* y w))) (sum 0))
        (declare (type fixnum base sum))
        (loop for i fixnum from 0 to (min r (1- w)) do (incf sum (aref src (+ base i))))
        (dotimes (x w)
          (setf (aref dst (+ base x)) (the (unsigned-byte 8) (truncate sum win)))
          (let ((add (+ x r 1)) (rem (- x r)))
            (declare (type fixnum add rem))
            (when (< add w) (incf sum (aref src (+ base add))))
            (when (>= rem 0) (decf sum (aref src (+ base rem))))))))))

(defun %vbox-blur (src dst w h r)
  "One vertical box-blur of radius R over the W×H alpha buffer SRC into DST."
  (declare (type (simple-array (unsigned-byte 8) (*)) src dst) (type fixnum w h r)
           (optimize (speed 3) (safety 0)))
  (let ((win (+ (* 2 r) 1)))
    (declare (type fixnum win))
    (dotimes (x w)
      (let ((sum 0))
        (declare (type fixnum sum))
        (loop for i fixnum from 0 to (min r (1- h)) do (incf sum (aref src (+ (* i w) x))))
        (dotimes (y h)
          (setf (aref dst (+ (the fixnum (* y w)) x)) (the (unsigned-byte 8) (truncate sum win)))
          (let ((add (+ y r 1)) (rem (- y r)))
            (declare (type fixnum add rem))
            (when (< add h) (incf sum (aref src (+ (the fixnum (* add w)) x))))
            (when (>= rem 0) (decf sum (aref src (+ (the fixnum (* rem w)) x))))))))))

(defun blur-alpha (buf w h r)
  "Blur the W×H alpha BUF in place by box-radius R, 3 separable passes (~Gaussian)."
  (when (plusp r)
    (let ((tmp (make-array (* w h) :element-type '(unsigned-byte 8))))
      (dotimes (i 3)
        (%hbox-blur buf tmp w h r)
        (%vbox-blur tmp buf w h r)))))

(defun spread-radii (radii spread)
  "Outer-shadow corner radii: a positive border radius grows by SPREAD (floored at 0);
a sharp (0) corner stays sharp (CSS Backgrounds 3 §7.1.1)."
  (let ((v (make-array 8)))
    (dotimes (i 8 v)
      (let ((rr (aref radii i)))
        (setf (aref v i) (if (> rr 0.0) (max 0.0 (+ rr spread)) 0.0))))))

(defun shrink-radii (radii spread)
  "Inner-hole corner radii: each padding-box inner radius shrinks by SPREAD (floored 0)."
  (let ((v (make-array 8)))
    (dotimes (i 8 v)
      (setf (aref v i) (max 0.0 (- (aref radii i) spread))))))

(defparameter *zero-radii* (vector 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0))

(defun shadow-rgba (color cs)
  "Resolve a shadow COLOR spec (:currentcolor or (r g b [a])) -> (values r g b a255)."
  (let ((c (if (eq color :currentcolor) (css:cstyle-color cs) color)))
    (values (first c) (second c) (third c)
            (if (and (>= (length c) 4) (fourth c)) (round (* 255 (fourth c))) 255))))

(defun %composite-shadow (cv cr cg cb a0 ox oy bw bh cover-fn r knockout-fn)
  "Build a BW×BH alpha buffer at device origin (OX OY): 255 where COVER-FN(fx fy) (pixel
centres), blur it by box-radius R, then composite (CR CG CB) at straight alpha A0/255
through the blurred alpha — except where KNOCKOUT-FN(fx fy) (the shadow-casting box's
own region is knocked out).  Honours *clip*/*round-clip* via BLEND-PUT."
  (when (and (plusp bw) (plusp bh) (< (* bw bh) 40000000))
    (let ((buf (make-array (* bw bh) :element-type '(unsigned-byte 8) :initial-element 0)))
      (dotimes (yy bh)
        (let ((bas (* yy bw)) (fy (+ oy yy 0.5)))
          (dotimes (xx bw)
            (when (funcall cover-fn (+ ox xx 0.5) fy)
              (setf (aref buf (+ bas xx)) 255)))))
      (blur-alpha buf bw bh r)
      (dotimes (yy bh)
        (let ((bas (* yy bw)) (fy (+ oy yy 0.5)) (dy (+ oy yy)))
          (dotimes (xx bw)
            (let ((av (aref buf (+ bas xx))))
              (when (plusp av)
                (let ((fx (+ ox xx 0.5)))
                  (unless (and knockout-fn (funcall knockout-fn fx fy))
                    (blend-put cv (+ ox xx) dy cr cg cb (floor (* av a0) 255))))))))))))

(defun paint-outset-shadow (cv lb cs offx offy blur spread color)
  "Paint one outset shadow behind LB: the border box, translated by (OFFX OFFY),
expanded by SPREAD, blurred by BLUR, knocked out under the box's own border box."
  (multiple-value-bind (cr cg cb a0) (shadow-rgba color cs)
    (when (plusp a0)
      (let* ((bx0 (lbox-x lb)) (by0 (lbox-y lb)) (bw (lbox-w lb)) (bh (lbox-h lb))
             (bx1 (+ bx0 bw)) (by1 (+ by0 bh))
             (sx0 (- (+ bx0 offx) spread)) (sy0 (- (+ by0 offy) spread))
             (sx1 (+ bx1 offx spread)) (sy1 (+ by1 offy spread))
             (r (blur-box-radius blur)) (pad (+ (* 3 r) 2))
             (brad (and (box-has-radius-p cs) (corner-radii-px cs bw bh))))
        (when (and (> sx1 sx0) (> sy1 sy0))
          (let* ((shape (make-round-region sx0 sy0 sx1 sy1
                                           (if brad (spread-radii brad spread) *zero-radii*)))
                 (knock (make-round-region bx0 by0 bx1 by1 (or brad *zero-radii*)))
                 (ox (floor (- sx0 pad))) (oy (floor (- sy0 pad)))
                 (ex (ceiling (+ sx1 pad))) (ey (ceiling (+ sy1 pad))))
            (%composite-shadow cv cr cg cb a0 ox oy (- ex ox) (- ey oy)
                               (lambda (fx fy) (region-contains-f shape fx fy))
                               r
                               (lambda (fx fy) (region-contains-f knock fx fy)))))))))

(defun paint-inset-shadow (cv lb cs offx offy blur spread color)
  "Paint one inset shadow over LB's background, clipped to the padding box: the padding
box filled with the shadow color, with an inner hole (padding box translated by
\(OFFX OFFY), contracted by SPREAD) blurred by BLUR (CSS Backgrounds 3 §7.1.1)."
  (multiple-value-bind (cr cg cb a0) (shadow-rgba color cs)
    (when (plusp a0)
      (let* ((bx0 (lbox-x lb)) (by0 (lbox-y lb)) (bw (lbox-w lb)) (bh (lbox-h lb))
             (bl (used-border cs :l)) (bt (used-border cs :t))
             (brr (used-border cs :r)) (bb (used-border cs :b))
             (px0 (+ bx0 bl)) (py0 (+ by0 bt))
             (px1 (- (+ bx0 bw) brr)) (py1 (- (+ by0 bh) bb))
             (r (blur-box-radius blur)) (pad (+ (* 3 r) 2))
             (prad (if (box-has-radius-p cs)
                       (inset-radii (corner-radii-px cs bw bh) bl bt brr bb)
                       *zero-radii*))
             (hx0 (+ px0 offx spread)) (hy0 (+ py0 offy spread))
             (hx1 (- (+ px1 offx) spread)) (hy1 (- (+ py1 offy) spread)))
        (when (and (> px1 px0) (> py1 py0))
          (let* ((pad-region (make-round-region px0 py0 px1 py1 prad))
                 (hole (and (> hx1 hx0) (> hy1 hy0)
                            (make-round-region hx0 hy0 hx1 hy1 (shrink-radii prad spread))))
                 (ox (floor (- px0 pad))) (oy (floor (- py0 pad)))
                 (ex (ceiling (+ px1 pad))) (ey (ceiling (+ py1 pad))))
            (%composite-shadow cv cr cg cb a0 ox oy (- ex ox) (- ey oy)
                               ;; shadow color fills everywhere except inside the hole
                               (if hole
                                   (lambda (fx fy) (not (region-contains-f hole fx fy)))
                                   (lambda (fx fy) (declare (ignore fx fy)) t))
                               r
                               ;; composite only inside the padding box (knock out outside)
                               (lambda (fx fy) (not (region-contains-f pad-region fx fy))))))))))

(defun paint-box-shadows (cv lb cs inset-p)
  "Paint CS's box-shadow list on LB.  INSET-P selects the inset (T) or outset (NIL)
shadows.  First-listed shadow is topmost, so paint the list in reverse."
  (let ((shadows (css:cstyle-box-shadow cs)))
    (when shadows
      (dolist (sh (reverse shadows))
        (destructuring-bind (inset offx offy blur spread color) sh
          (when (eq (and inset t) inset-p)
            (if inset-p
                (paint-inset-shadow cv lb cs offx offy blur spread color)
                (paint-outset-shadow cv lb cs offx offy blur spread color))))))))

(defun bevel-darken (c)
  "Darken a border color to 9/16 per channel — the darker tone used by the
inset/outset/ridge/groove 3D border styles (CSS 2.1 §8.5.3); the lighter tone is
the border color itself.  Matches the reference UA's shading of these styles."
  (if (>= (length c) 4)
      (list (floor (* (first c) 9) 16) (floor (* (second c) 9) 16) (floor (* (third c) 9) 16) (fourth c))
      (list (floor (* (first c) 9) 16) (floor (* (second c) 9) 16) (floor (* (third c) 9) 16))))

(defun paint-beveled-borders (cv style x0 y0 x1 y1 bt br bb bl base)
  "Paint the four border edges with the 3D bevel of inset/outset/ridge/groove
(CSS 2.1 §8.5.3).  Top/left and bottom/right edges get opposite tones (light = the
border color, dark = bevel-darken) so the box looks embedded (inset), raised
(outset), carved (groove) or ridged (ridge).  groove/ridge split each edge into an
outer and inner half with opposite tones.  Corners are mitered on the 45-deg
diagonal (fill-poly), matching the per-side border painter."
  (let* ((light base) (dark (bevel-darken base))
         (ix0 (+ x0 bl)) (iy0 (+ y0 bt)) (ix1 (- x1 br)) (iy1 (- y1 bb))
         (mx0 (+ x0 (/ bl 2))) (my0 (+ y0 (/ bt 2)))
         (mx1 (- x1 (/ br 2))) (my1 (- y1 (/ bb 2))))
    (labels ((p (pts col) (fill-poly cv pts col))
             ;; full mitered trapezoid for each side
             (top-full (c)    (p (list (cons x0 y0) (cons x1 y0) (cons ix1 iy0) (cons ix0 iy0)) c))
             (bottom-full (c) (p (list (cons x0 y1) (cons ix0 iy1) (cons ix1 iy1) (cons x1 y1)) c))
             (left-full (c)   (p (list (cons x0 y0) (cons ix0 iy0) (cons ix0 iy1) (cons x0 y1)) c))
             (right-full (c)  (p (list (cons x1 y0) (cons x1 y1) (cons ix1 iy1) (cons ix1 iy0)) c))
             ;; outer/inner halves (outer = nearer the outer box edge)
             (top-o (c)  (p (list (cons x0 y0) (cons x1 y0) (cons mx1 my0) (cons mx0 my0)) c))
             (top-i (c)  (p (list (cons mx0 my0) (cons mx1 my0) (cons ix1 iy0) (cons ix0 iy0)) c))
             (bot-o (c)  (p (list (cons x0 y1) (cons mx0 my1) (cons mx1 my1) (cons x1 y1)) c))
             (bot-i (c)  (p (list (cons mx0 my1) (cons ix0 iy1) (cons ix1 iy1) (cons mx1 my1)) c))
             (left-o (c) (p (list (cons x0 y0) (cons mx0 my0) (cons mx0 my1) (cons x0 y1)) c))
             (left-i (c) (p (list (cons mx0 my0) (cons ix0 iy0) (cons ix0 iy1) (cons mx0 my1)) c))
             (right-o (c)(p (list (cons x1 y0) (cons x1 y1) (cons mx1 my1) (cons mx1 my0)) c))
             (right-i (c)(p (list (cons mx1 my0) (cons mx1 my1) (cons ix1 iy1) (cons ix1 iy0)) c)))
      (cond
        ((string= style "inset")   ; top/left dark, bottom/right light
         (when (plusp bt) (top-full dark))   (when (plusp bl) (left-full dark))
         (when (plusp bb) (bottom-full light))(when (plusp br) (right-full light)))
        ((string= style "outset")  ; top/left light, bottom/right dark
         (when (plusp bt) (top-full light))  (when (plusp bl) (left-full light))
         (when (plusp bb) (bottom-full dark))(when (plusp br) (right-full dark)))
        ((string= style "ridge")   ; outer half = outset tone, inner half = inset tone
         (when (plusp bt) (top-o light)  (top-i dark))
         (when (plusp bl) (left-o light) (left-i dark))
         (when (plusp bb) (bot-o dark)   (bot-i light))
         (when (plusp br) (right-o dark) (right-i light)))
        ((string= style "groove")  ; outer half = inset tone, inner half = outset tone
         (when (plusp bt) (top-o dark)   (top-i light))
         (when (plusp bl) (left-o dark)  (left-i light))
         (when (plusp bb) (bot-o light)  (bot-i dark))
         (when (plusp br) (right-o light)(right-i dark)))))))

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
    (let* ((radii (and (box-has-radius-p cs) (corner-radii-px cs w h)))
           (ts (css:cstyle-border-top-style cs))
           (bevel (and ts (equal ct crr) (equal ct cb) (equal ct cl)
                       (member ts '("inset" "outset" "ridge" "groove") :test #'string=)
                       (equal ts (css:cstyle-border-right-style cs))
                       (equal ts (css:cstyle-border-bottom-style cs))
                       (equal ts (css:cstyle-border-left-style cs))
                       ts)))
     (cond
      ((and radii (some #'plusp radii) (equal ct crr) (equal ct cb) (equal ct cl))
       ;; uniform color + border-radius: fill the rounded ring (outer border-box
       ;; rounded rect minus the inner padding-box rounded rect) — §5.5.
       (let ((outer (make-round-region x0 y0 x1 y1 radii))
             (inner (make-round-region (+ x0 bl) (+ y0 bt) (- x1 br) (- y1 bb)
                                       (inset-radii radii bl bt br bb))))
         (fill-round-ring cv outer inner ct)))
      ((and (equal ct crr) (equal ct cb) (equal ct cl)
            (flet ((dbl (s) (and s (string= s "double"))))
              (and (dbl (css:cstyle-border-top-style cs)) (dbl (css:cstyle-border-right-style cs))
                   (dbl (css:cstyle-border-bottom-style cs)) (dbl (css:cstyle-border-left-style cs))
                   (>= (min bt br bb bl) 3))))
       ;; uniform double (CSS 2.1 §border-style): two lines with a gap, each a third
       ;; of the border width — paint the outer third and inner third as nested rings
       ;; and leave the middle third for the box background to show through.
       (flet ((edge3 (n) (max 1.0 (ffloor (/ n 3.0)))))
         (let ((tt (edge3 bt)) (tr (edge3 br)) (tb (edge3 bb)) (tl (edge3 bl)))
           ;; outer ring
           (fill-rect cv x0 y0 w tt ct) (fill-rect cv x0 (- y1 tb) w tb cb)
           (fill-rect cv x0 y0 tl h cl) (fill-rect cv (- x1 tr) y0 tr h crr)
           ;; inner ring (at the inner edge of each border)
           (fill-rect cv x0 (- (+ y0 bt) tt) w tt ct) (fill-rect cv x0 (- y1 bb) w tb cb)
           (fill-rect cv (- (+ x0 bl) tl) y0 tl h cl) (fill-rect cv (- x1 br) y0 tr h crr))))
      (bevel
       ;; uniform inset/outset/ridge/groove: 3D bevel shading (CSS 2.1 §8.5.3).
       (paint-beveled-borders cv bevel x0 y0 x1 y1 bt br bb bl ct))
      ((and (equal ct crr) (equal ct cb) (equal ct cl))
        ;; uniform color: overlapping rectangles, exactly as the original code.
          (when (plusp bt) (fill-rect cv x0 y0 w bt ct))
          (when (plusp bb) (fill-rect cv x0 (- y1 bb) w bb cb))
          (when (plusp bl) (fill-rect cv x0 y0 bl h cl))
          (when (plusp br) (fill-rect cv (- x1 br) y0 br h crr)))
      (t
        ;; differing colors: mitered trapezoids (degenerate to triangles).
        (let ((ix0 (+ x0 bl)) (iy0 (+ y0 bt)) (ix1 (- x1 br)) (iy1 (- y1 bb)))
          (when (plusp bt) (fill-poly cv (list (cons x0 y0) (cons x1 y0) (cons ix1 iy0) (cons ix0 iy0)) ct))
          (when (plusp bb) (fill-poly cv (list (cons x0 y1) (cons ix0 iy1) (cons ix1 iy1) (cons x1 y1)) cb))
          (when (plusp bl) (fill-poly cv (list (cons x0 y0) (cons ix0 iy0) (cons ix0 iy1) (cons x0 y1)) cl))
          (when (plusp br) (fill-poly cv (list (cons x1 y0) (cons x1 y1) (cons ix1 iy1) (cons ix1 iy0)) crr))))))))

;;; ---- border-image (CSS Backgrounds & Borders 3 §6) ----------------------

(defun bi-source-img (src nw nh curcolor)
  "An IMG (rgba) for a border-image-source SRC over an NW×NH natural area: a url/data
string is decoded at its intrinsic size; a parsed gradient is rendered into a fresh
NW×NH opaque RGBA buffer (its natural size = the border-image area)."
  (cond
    ((stringp src)
     (let* ((duri (if (find #\% src) (percent-decode src) src))
            (img (if (and (>= (length duri) 5) (string-equal (subseq duri 0 5) "data:"))
                     (ignore-errors (decode-image duri))
                     (ignore-errors (fetch-image src)))))
       (and img (plusp (img-w img)) (plusp (img-h img)) (img-rgba img) img)))
    ((consp src)                        ; a parsed gradient
     (let* ((tw (max 1 (round nw))) (th (max 1 (round nh)))
            (tmp (make-canvas tw th '(0 0 0)))
            (rgba (make-array (* 4 tw th) :element-type '(unsigned-byte 8))))
       (let ((*clip* nil) (*round-clip* nil))
         (fill-css-gradient tmp 0 0 tw th src curcolor))
       (let ((px (canvas-pixels tmp)))
         (dotimes (i (* tw th))
           (setf (aref rgba (* 4 i))       (aref px (* 3 i))
                 (aref rgba (+ 1 (* 4 i))) (aref px (+ 1 (* 3 i)))
                 (aref rgba (+ 2 (* 4 i))) (aref px (+ 2 (* 3 i)))
                 (aref rgba (+ 3 (* 4 i))) 255)))
       (make-img :w tw :h th :rgba rgba)))
    (t nil)))

(defun bi-slice-px (spec dim)
  "Resolve a border-image-slice offset SPEC ((VAL . :px|:pct)) to source pixels over DIM."
  (cond ((null spec) 0.0)
        ((eq (cdr spec) :pct) (* (/ (car spec) 100.0) (float dim 1.0)))
        (t (min (float (car spec) 1.0) (float dim 1.0)))))

(defun bi-width-px (spec border slice-px area-dim)
  "Resolve one border-image-width component SPEC (NIL default = 1×BORDER; :auto = the
slice size; (VAL . :num) = ×border; :px = px; :pct = % of the border-image area dim)."
  (cond ((null spec) (float border 1.0))
        ((eq spec :auto) (float slice-px 1.0))
        ((and (consp spec) (eq (cdr spec) :num)) (* (car spec) (float border 1.0)))
        ((and (consp spec) (eq (cdr spec) :px)) (float (car spec) 1.0))
        ((and (consp spec) (eq (cdr spec) :pct)) (* (/ (car spec) 100.0) (float area-dim 1.0)))
        (t (float border 1.0))))

(defun bi-blit-region (cv img sx sy sw sh dx dy dw dh mode axis)
  "Paint source rect (SX SY SW SH) of IMG into dest (DX DY DW DH).  AXIS :none is a
corner (stretch); :h/:v tiles the slice along x/y for :repeat/:round (else stretches)."
  (when (and (> sw 0) (> sh 0) (> dw 0) (> dh 0))
    (let ((sx (round sx)) (sy (round sy)) (sw (round sw)) (sh (round sh)))
      (if (or (member mode '(:stretch :space)) (eq axis :none))
          (blit-img cv img (round dx) (round dy) (round dw) (round dh) (list sx sy sw sh))
          (ecase axis
            (:h (let* ((tw0 (* sw (/ dh sh)))      ; scale slice so its height fills DH
                       (tw (max 1.0 (if (eq mode :round) (/ dw (max 1 (round (/ dw tw0)))) tw0)))
                       (*clip* (clip-intersect (round dx) (round dy) (round (+ dx dw)) (round (+ dy dh)))))
                  (loop for x = dx then (+ x tw) while (< x (+ dx dw))
                        do (blit-img cv img (round x) (round dy) (round tw) (round dh) (list sx sy sw sh)))))
            (:v (let* ((th0 (* sh (/ dw sw)))
                       (th (max 1.0 (if (eq mode :round) (/ dh (max 1 (round (/ dh th0)))) th0)))
                       (*clip* (clip-intersect (round dx) (round dy) (round (+ dx dw)) (round (+ dy dh)))))
                  (loop for y = dy then (+ y th) while (< y (+ dy dh))
                        do (blit-img cv img (round dx) (round y) (round dw) (round th) (list sx sy sw sh))))))))))

(defun paint-border-image (cv lb cs)
  "Paint CS's border-image over LB's border box, 9-sliced into the 4 corners (stretched)
and 4 edges (stretched/repeated/rounded), plus the middle when `fill` is set (CSS
Backgrounds & Borders 3 §6).  Returns T when a decodable source painted, else NIL."
  (let ((src (css:cstyle-border-image-source cs)))
    (when src
      (let* ((x0 (float (lbox-x lb) 1.0)) (y0 (float (lbox-y lb) 1.0))
             (bw (float (lbox-w lb) 1.0)) (bh (float (lbox-h lb) 1.0))
             (bt (used-border cs :t)) (br (used-border cs :r))
             (bb (used-border cs :b)) (bl (used-border cs :l))
             (img (bi-source-img src bw bh (css:cstyle-color cs))))
        (when (and img (plusp (img-w img)) (plusp (img-h img)))
          (let* ((nw (img-w img)) (nh (img-h img))
                 (slice (or (css:cstyle-border-image-slice cs)
                            '((100 . :pct) (100 . :pct) (100 . :pct) (100 . :pct) nil)))
                 (st (bi-slice-px (first slice) nh)) (sr (bi-slice-px (second slice) nw))
                 (sb (bi-slice-px (third slice) nh)) (sl (bi-slice-px (fourth slice) nw))
                 (fillp (fifth slice))
                 (ws (css:cstyle-border-image-width cs))
                 (wt (bi-width-px (and ws (first ws)) bt st bh))
                 (wr (bi-width-px (and ws (second ws)) br sr bw))
                 (wb (bi-width-px (and ws (third ws)) bb sb bh))
                 (wl (bi-width-px (and ws (fourth ws)) bl sl bw))
                 (rep (or (css:cstyle-border-image-repeat cs) '(:stretch . :stretch)))
                 (rh (car rep)) (rv (cdr rep))
                 (msw (max 0 (- nw sl sr))) (msh (max 0 (- nh st sb)))
                 (dmw (max 0.0 (- bw wl wr))) (dmh (max 0.0 (- bh wt wb)))
                 (rx (- (+ x0 bw) wr)) (by (- (+ y0 bh) wb)))
            ;; corners (always stretched)
            (when (and (> sl 0) (> st 0)) (bi-blit-region cv img 0 0 sl st x0 y0 wl wt :stretch :none))
            (when (and (> sr 0) (> st 0)) (bi-blit-region cv img (- nw sr) 0 sr st rx y0 wr wt :stretch :none))
            (when (and (> sr 0) (> sb 0)) (bi-blit-region cv img (- nw sr) (- nh sb) sr sb rx by wr wb :stretch :none))
            (when (and (> sl 0) (> sb 0)) (bi-blit-region cv img 0 (- nh sb) sl sb x0 by wl wb :stretch :none))
            ;; edges
            (when (and (> msw 0) (> st 0)) (bi-blit-region cv img sl 0 msw st (+ x0 wl) y0 dmw wt rh :h))
            (when (and (> msw 0) (> sb 0)) (bi-blit-region cv img sl (- nh sb) msw sb (+ x0 wl) by dmw wb rh :h))
            (when (and (> msh 0) (> sl 0)) (bi-blit-region cv img 0 st sl msh x0 (+ y0 wt) wl dmh rv :v))
            (when (and (> msh 0) (> sr 0)) (bi-blit-region cv img (- nw sr) st sr msh rx (+ y0 wt) wr dmh rv :v))
            ;; the middle is painted only with the `fill` keyword
            (when (and fillp (> msw 0) (> msh 0) (> dmw 0) (> dmh 0))
              (bi-blit-region cv img sl st msw msh (+ x0 wl) (+ y0 wt) dmw dmh :stretch :none))
            t))))))

(defun paint-outline (cv lb cs)
  "Paint the CSS-UI outline: a uniform ring just outside the border edge, offset
outward by outline-offset (may be negative).  Outlines do not affect layout and
are not clipped by the element's own overflow.  Any non-none/hidden style
(including `auto`) paints as a solid ring (weft has no dotted/dashed rasteriser;
this matches how weft paints those border styles too)."
  (let ((sty (css:cstyle-outline-style cs))
        ;; used outline-width rounds a sub-1px width UP to 1px (an outline must be
        ;; visible if it has any width) and a >=1px width DOWN to a whole pixel
        ;; (CSS-UI computed outline-width; subpixel-outline-width test).
        (w (let ((rw (css:cstyle-outline-width cs)))
             (cond ((<= rw 0) 0) ((< rw 1) 1) (t (ffloor rw)))))
        (off (css:cstyle-outline-offset cs)))
    (when (and sty (not (member sty '("none" "hidden") :test #'string=)) (plusp w))
      (let* ((raw (css:cstyle-outline-color cs))
             ;; resolve the used outline color: :auto -> accent-color for an
             ;; auto-style outline (CSS-UI-4), else currentColor; NIL -> currentColor.
             (rc (cond ((eq raw :auto)
                        (or (and (string= sty "auto") (css:cstyle-accent-color cs))
                            (css:cstyle-color cs)))
                       ((null raw) (css:cstyle-color cs))
                       (t raw))))
       ;; a fully transparent outline (alpha 0) paints nothing — e.g. an ancestor's
       ;; `outline-color: transparent` must not paint over a descendant's outline
       ;; (outlines paint after children).
       (when (or (< (length rc) 4) (plusp (fourth rc)))
        (let* ((col (rgb rc))
             (x0 (lbox-x lb)) (y0 (lbox-y lb))
             (x1 (+ x0 (lbox-w lb))) (y1 (+ y0 (lbox-h lb)))
             (ix0 (- x0 off)) (iy0 (- y0 off)) (ix1 (+ x1 off)) (iy1 (+ y1 off)))
        ;; a large negative outline-offset can invert the inner rectangle; the
        ;; outline collapses to a zero-size line/point at the box center on the
        ;; inverted axis (matches Chrome's negative-outline-offset rendering).
        (when (< ix1 ix0) (let ((c (/ (+ x0 x1) 2))) (setf ix0 c ix1 c)))
        (when (< iy1 iy0) (let ((c (/ (+ y0 y1) 2))) (setf iy0 c iy1 c)))
        (let* ((ox0 (- ix0 w)) (ow (max 0 (- (+ ix1 w) ox0)))
             (ih (max 0 (- iy1 iy0))))
        (fill-rect cv ox0 (- iy0 w) ow w col)   ; top
        (fill-rect cv ox0 iy1 ow w col)          ; bottom
        (fill-rect cv ox0 iy0 w ih col)          ; left
        (fill-rect cv ix1 iy0 w ih col))))))))    ; right

(defun box-visible-p (cs)
  "NIL when CS is visibility:hidden/collapse — such a box paints nothing of its
   own (background, border, image, text) but keeps its layout space, and visible
   descendants (visibility:visible) still paint (visibility is inherited)."
  (not (and cs (member (css:cstyle-visibility cs) '("hidden" "collapse") :test #'string=))))

(defun gradient-visible-p (grad)
  "NIL when every color stop of GRAD is fully transparent — such a gradient (e.g.
   HN's `linear-gradient(transparent,transparent)` fallback layered under
   url(triangle.svg)) must paint nothing, not opaque black.  A stop with no alpha
   component, or currentcolor, is treated as opaque."
  (let ((stops (ecase (first grad)
                 (:linear (third grad))
                 (:radial (fifth grad))
                 (:conic (fourth grad)))))
    (some (lambda (st)
            (and (eq (first st) :c)
                 (let ((c (second st)))
                   (or (eq c :currentcolor) (< (length c) 4) (plusp (fourth c))))))
          stops)))

;;; ---- 2D transform rasterisation (CSS Transforms 1 §6) --------------------
;;; A rotate/skew/general-matrix transform cannot be reduced to a box move, so the
;;; element and its descendants are painted UPRIGHT into an offscreen buffer and
;;; then inverse-mapped through the transform matrix onto the main canvas.  Because
;;; the RGB canvas has no alpha, coverage is recovered by painting the subtree twice
;;; — once over black, once over white: out = a*c over bg gives black->a*c and
;;; white->a*c+(1-a)*255, so a = 1-(white-black)/255 and c = black/a.  This yields
;;; correct fractional coverage along antialiased/rotated edges.
(defun box-raster-transform-matrix (lb)
  "The absolute affine matrix for LB's transform when it must be rasterised (a
rotate/skew/general non-axis-aligned map), else NIL.  Pure translations (already
geometry-shifted) and axis-aligned scales (AABB-adjusted) return NIL so they keep
their byte-identical fast paths."
  (let ((cs (lbox-style lb)))
    (when (and cs (css:cstyle-transform cs) (not (equal (css:cstyle-transform cs) '("none"))))
      (let ((tl (css:cstyle-transform cs)) (fs (css:cstyle-font-size cs))
            (bw (lbox-w lb)) (bh (lbox-h lb)))
        (unless (tf-pure-translate tl fs bw bh)
          (let ((m (box-transform-matrix cs (lbox-x lb) (lbox-y lb) bw bh)))
            (when (and m (not (tf-axis-aligned-p m))) m)))))))

(defun subtree-paint-bounds (lb)
  "Device-space integer bounds (values x0 y0 x1 y1) enclosing LB and every descendant
box/fragment — the region the subtree paints into when rendered upright."
  (let ((x0 (lbox-x lb)) (y0 (lbox-y lb))
        (x1 (+ (lbox-x lb) (lbox-w lb))) (y1 (+ (lbox-y lb) (lbox-h lb))))
    (labels ((rec (b)
               (setf x0 (min x0 (lbox-x b)) y0 (min y0 (lbox-y b))
                     x1 (max x1 (+ (lbox-x b) (lbox-w b)))
                     y1 (max y1 (+ (lbox-y b) (lbox-h b))))
               (if (eq (lbox-kind b) :line)
                   (dolist (it (lbox-children b))
                     (if (frag-p it)
                         (setf x0 (min x0 (frag-x it))
                               x1 (max x1 (+ (frag-x it) (frag-w it))))
                         (when (lbox-p it) (rec it))))
                   (dolist (c (lbox-children b)) (when (lbox-p c) (rec c))))))
      (rec lb))
    (values (floor x0) (floor y0) (ceiling x1) (ceiling y1))))

(declaim (inline %bilerp))
(defun %bilerp (px w h fx fy)
  "Bilinear-sample RGB buffer PX (W×H, pixel centres at integer+0.5) at continuous
point (FX,FY); returns (values r g b) as floats, edge-clamped."
  (let* ((gx (- fx 0.5)) (gy (- fy 0.5))
         (x0 (floor gx)) (y0 (floor gy))
         (tx (- gx x0)) (ty (- gy y0))
         (x1 (1+ x0)) (y1 (1+ y0)))
    (setf x0 (max 0 (min (1- w) x0)) x1 (max 0 (min (1- w) x1))
          y0 (max 0 (min (1- h) y0)) y1 (max 0 (min (1- h) y1)))
    (flet ((at (x y ch) (aref px (+ (* 3 (+ (* y w) x)) ch))))
      (flet ((ch (k)
               (let ((top (+ (* (at x0 y0 k) (- 1.0 tx)) (* (at x1 y0 k) tx)))
                     (bot (+ (* (at x0 y1 k) (- 1.0 tx)) (* (at x1 y1 k) tx))))
                 (+ (* top (- 1.0 ty)) (* bot ty)))))
        (values (ch 0) (ch 1) (ch 2))))))

(defun composite-transformed (cv m offA offB sx0 sy0 sw sh &optional (alpha 1.0))
  "Inverse-map the offscreen buffers (subtree painted over black=OFFA and white=OFFB)
through M onto CV: for each destination pixel in the transformed AABB, find the source
point, sample both buffers, recover colour + coverage, and alpha-composite (honouring
the ancestor's *CLIP*/*ROUND-CLIP* via BLEND-PUT).  ALPHA (<1 for group opacity) scales
the recovered coverage, so the whole subtree blends as one group (CSS Color 4 §opacity)."
  (destructuring-bind (a b c d e f) m
    (let ((det (- (* a d) (* b c))))
      (when (> (abs det) 1.0d-9)
        (let* ((iaa (/ d det)) (ibb (/ (- b) det)) (icc (/ (- c) det)) (idd (/ a det))
               (iee (- (+ (* iaa e) (* icc f)))) (iff (- (+ (* ibb e) (* idd f))))
               (pa (canvas-pixels offA)) (pb (canvas-pixels offB))
               (xs '()) (ys '()))
          (dolist (p (list (cons sx0 sy0) (cons (+ sx0 sw) sy0)
                           (cons sx0 (+ sy0 sh)) (cons (+ sx0 sw) (+ sy0 sh))))
            (multiple-value-bind (px py) (mat-pt m (car p) (cdr p))
              (push px xs) (push py ys)))
          (let ((dx0 (max 0 (floor (reduce #'min xs))))
                (dy0 (max 0 (floor (reduce #'min ys))))
                (dx1 (min (canvas-width cv) (ceiling (reduce #'max xs))))
                (dy1 (min (canvas-height cv) (ceiling (reduce #'max ys)))))
            (loop for dy from dy0 below dy1 do
              (loop for dx from dx0 below dx1 do
                (let* ((fx (+ dx 0.5)) (fy (+ dy 0.5))
                       (u (+ (* iaa fx) (* icc fy) iee))
                       (v (+ (* ibb fx) (* idd fy) iff))
                       (lx (- u sx0)) (ly (- v sy0)))
                  (when (and (>= lx -0.5) (>= ly -0.5) (< lx (+ sw 0.5)) (< ly (+ sh 0.5)))
                    (multiple-value-bind (ar ag ab) (%bilerp pa sw sh lx ly)
                      (multiple-value-bind (br bg bb) (%bilerp pb sw sh lx ly)
                        (let ((cov (- 1.0 (/ (+ (- br ar) (- bg ag) (- bb ab)) 765.0))))
                          (when (> cov 0.004)
                            (flet ((cc (v) (min 255 (max 0 (round (/ v cov))))))
                              (blend-put cv dx dy (cc ar) (cc ag) (cc ab)
                                         (min 255 (max 0 (round (* 255.0 cov alpha))))))))))))))))))))

(defun paint-transformed (cv lb m &optional (alpha 1.0))
  "Render LB's subtree upright into two offscreen buffers and composite it through the
transform matrix M (rotate/skew/general-matrix, CSS Transforms 1 §6).  ALPHA (<1)
applies group opacity to the transformed result."
  (multiple-value-bind (sx0 sy0 sx1 sy1) (subtree-paint-bounds lb)
    (decf sx0) (decf sy0) (incf sx1) (incf sy1)          ; 1px pad for edge AA
    (let ((sw (- sx1 sx0)) (sh (- sy1 sy0)))
      (when (and (plusp sw) (plusp sh) (<= (* sw sh) 16000000))
        (let ((offa (make-canvas sw sh '(0 0 0)))
              (offb (make-canvas sw sh '(255 255 255))))
          ;; paint the subtree upright at offset (-sx0,-sy0) into each buffer
          (shift-box lb (- sx0) (- sy0))
          (let ((*clip* nil) (*round-clip* nil))
            (%paint-box-content offa lb)
            (%paint-box-content offb lb))
          (shift-box lb sx0 sy0)
          (composite-transformed cv m offa offb sx0 sy0 sw sh alpha))))))

(defun paint-opacity (cv lb alpha)
  "Group opacity (CSS Color 4 §opacity): render LB's subtree upright into two offscreen
buffers (over black and over white), recover per-pixel coverage + colour, and composite
1:1 onto CV at global ALPHA — the element's whole subtree blends against the backdrop as
one group (self-overlap composites once, unlike per-operation alpha).  Honours the
ancestor's *CLIP*/*ROUND-CLIP* via BLEND-PUT."
  (multiple-value-bind (sx0 sy0 sx1 sy1) (subtree-paint-bounds lb)
    (decf sx0) (decf sy0) (incf sx1) (incf sy1)          ; 1px pad for edge AA
    (let ((sw (- sx1 sx0)) (sh (- sy1 sy0)))
      (when (and (plusp sw) (plusp sh) (<= (* sw sh) 16000000))
        (let ((offa (make-canvas sw sh '(0 0 0)))
              (offb (make-canvas sw sh '(255 255 255))))
          (shift-box lb (- sx0) (- sy0))
          (let ((*clip* nil) (*round-clip* nil))
            (%paint-box-content offa lb)
            (%paint-box-content offb lb))
          (shift-box lb sx0 sy0)
          (let ((pa (canvas-pixels offa)) (pb (canvas-pixels offb)))
            (dotimes (sy sh)
              (let ((dy (+ sy0 sy)))
                (when (and (>= dy 0) (< dy (canvas-height cv)))
                  (dotimes (sx sw)
                    (let ((dx (+ sx0 sx)))
                      (when (and (>= dx 0) (< dx (canvas-width cv)))
                        (let* ((i (* 3 (+ (* sy sw) sx)))
                               (ar (aref pa i)) (ag (aref pa (+ i 1))) (ab (aref pa (+ i 2)))
                               (br (aref pb i)) (bg (aref pb (+ i 1))) (bb (aref pb (+ i 2)))
                               (cov (- 1.0 (/ (+ (- br ar) (- bg ag) (- bb ab)) 765.0))))
                          (when (> cov 0.004)
                            (flet ((cc (v) (min 255 (max 0 (round (/ v cov))))))
                              (blend-put cv dx dy (cc ar) (cc ag) (cc ab)
                                         (min 255 (max 0 (round (* 255.0 cov alpha))))))))))))))))))))

;;; ---- filter (CSS Filter Effects 1 §filter) ------------------------------

(defun %filter-matrix (op amt)
  "3x3 straight-RGB colour matrix (9 flonums, row-major) for the linear filter OP at
amount AMT (:saturate/:grayscale/:sepia), using the spec luma coefficients."
  (flet ((sat (s)   ; SVG saturate matrix; grayscale(a) == saturate(1-a)
           (list (+ 0.2126 (* 0.7874 s)) (- 0.7152 (* 0.7152 s)) (- 0.0722 (* 0.0722 s))
                 (- 0.2126 (* 0.2126 s)) (+ 0.7152 (* 0.2848 s)) (- 0.0722 (* 0.0722 s))
                 (- 0.2126 (* 0.2126 s)) (- 0.7152 (* 0.7152 s)) (+ 0.0722 (* 0.9278 s)))))
    (ecase op
      (:saturate  (sat (float amt 1.0)))
      (:grayscale (sat (- 1.0 (float amt 1.0))))
      (:sepia (let ((tt (- 1.0 (float amt 1.0))))
                (list (+ 0.393 (* 0.607 tt)) (- 0.769 (* 0.769 tt)) (- 0.189 (* 0.189 tt))
                      (- 0.349 (* 0.349 tt)) (+ 0.686 (* 0.314 tt)) (- 0.168 (* 0.168 tt))
                      (- 0.272 (* 0.272 tt)) (- 0.534 (* 0.534 tt)) (+ 0.131 (* 0.869 tt))))))))

(defun %apply-matrix (m cr cg cb n)
  "Apply the 3x3 colour matrix M over the N straight-RGB pixels in CR/CG/CB in place."
  (destructuring-bind (m00 m01 m02 m10 m11 m12 m20 m21 m22) m
    (dotimes (i n)
      (let ((r (aref cr i)) (g (aref cg i)) (b (aref cb i)))
        (flet ((cl (v) (the (unsigned-byte 8) (min 255 (max 0 (round v))))))
          (setf (aref cr i) (cl (+ (* m00 r) (* m01 g) (* m02 b)))
                (aref cg i) (cl (+ (* m10 r) (* m11 g) (* m12 b)))
                (aref cb i) (cl (+ (* m20 r) (* m21 g) (* m22 b)))))))))

(defun %blur-rgba (cr cg cb ca sw sh px)
  "filter:blur(PXpx): premultiply the straight colour by coverage, box-blur each of
the four channels (reusing the shadow box-blur), then un-premultiply.  For filter:blur the CSS length is the
Gaussian std-dev directly (sigma=PX), whereas BLUR-BOX-RADIUS takes sigma=arg/2, so
pass 2*PX (CSS Filter Effects 1 §blur)."
  (let ((r (blur-box-radius (* 2.0 px))) (n (* sw sh)))
    (when (plusp r)
      (dotimes (i n)
        (let ((a (aref ca i)))
          (setf (aref cr i) (truncate (* (aref cr i) a) 255)
                (aref cg i) (truncate (* (aref cg i) a) 255)
                (aref cb i) (truncate (* (aref cb i) a) 255))))
      (blur-alpha cr sw sh r) (blur-alpha cg sw sh r)
      (blur-alpha cb sw sh r) (blur-alpha ca sw sh r)
      (dotimes (i n)
        (let ((a (aref ca i)))
          (if (plusp a)
              (setf (aref cr i) (min 255 (truncate (* (aref cr i) 255) a))
                    (aref cg i) (min 255 (truncate (* (aref cg i) 255) a))
                    (aref cb i) (min 255 (truncate (* (aref cb i) 255) a)))
              (setf (aref cr i) 0 (aref cg i) 0 (aref cb i) 0)))))))

(defun %apply-filters (filters cr cg cb ca sw sh)
  "Apply the FILTERS chain left-to-right over the straight-RGBA buffers CR/CG/CB/CA."
  (let ((n (* sw sh)))
    (flet ((cl (v) (the (unsigned-byte 8) (min 255 (max 0 (round v))))))
      (dolist (f filters)
        (let ((op (car f)) (amt (float (cdr f) 1.0)))
          (case op
            (:blur (%blur-rgba cr cg cb ca sw sh amt))
            (:opacity (dotimes (i n) (setf (aref ca i) (cl (* (aref ca i) amt)))))
            (:brightness
             (dotimes (i n)
               (setf (aref cr i) (cl (* (aref cr i) amt))
                     (aref cg i) (cl (* (aref cg i) amt))
                     (aref cb i) (cl (* (aref cb i) amt)))))
            (:contrast
             (let ((off (* 127.5 (- 1.0 amt))))
               (dotimes (i n)
                 (setf (aref cr i) (cl (+ (* (aref cr i) amt) off))
                       (aref cg i) (cl (+ (* (aref cg i) amt) off))
                       (aref cb i) (cl (+ (* (aref cb i) amt) off))))))
            (:invert
             (dotimes (i n)
               (flet ((iv (c) (cl (+ c (* amt (- 255 (* 2 c)))))))
                 (setf (aref cr i) (iv (aref cr i))
                       (aref cg i) (iv (aref cg i))
                       (aref cb i) (iv (aref cb i))))))
            ((:grayscale :sepia :saturate)
             (%apply-matrix (%filter-matrix op amt) cr cg cb n))))))))

(defun paint-filtered (cv lb filters &optional (alpha 1.0))
  "CSS Filter Effects 1 §filter: render LB's subtree upright into two offscreen buffers
(over black + white), recover per-pixel straight colour + coverage, apply the FILTERS
chain (per-pixel colour LUTs/matrices; blur spatially), then composite over CV at group
ALPHA.  Honours the ancestor's *CLIP*/*ROUND-CLIP* via BLEND-PUT."
  (multiple-value-bind (sx0 sy0 sx1 sy1) (subtree-paint-bounds lb)
    (let ((pad (1+ (reduce #'max (mapcar (lambda (f) (if (eq (car f) :blur)
                                                         (ceiling (* 3 (cdr f))) 0))
                                         filters) :initial-value 0))))
      (decf sx0 pad) (decf sy0 pad) (incf sx1 pad) (incf sy1 pad)
      (let ((sw (- sx1 sx0)) (sh (- sy1 sy0)))
        (when (and (plusp sw) (plusp sh) (<= (* sw sh) 16000000))
          (let ((offa (make-canvas sw sh '(0 0 0)))
                (offb (make-canvas sw sh '(255 255 255))))
            (shift-box lb (- sx0) (- sy0))
            (let ((*clip* nil) (*round-clip* nil))
              (%paint-box-content offa lb)
              (%paint-box-content offb lb))
            (shift-box lb sx0 sy0)
            (let* ((n (* sw sh))
                   (pa (canvas-pixels offa)) (pb (canvas-pixels offb))
                   (cr (make-array n :element-type '(unsigned-byte 8)))
                   (cg (make-array n :element-type '(unsigned-byte 8)))
                   (cb (make-array n :element-type '(unsigned-byte 8)))
                   (ca (make-array n :element-type '(unsigned-byte 8))))
              (dotimes (i n)
                (let* ((j (* 3 i))
                       (ar (aref pa j)) (ag (aref pa (+ j 1))) (ab (aref pa (+ j 2)))
                       (br (aref pb j)) (bg (aref pb (+ j 1))) (bb (aref pb (+ j 2)))
                       (cov (- 1.0 (/ (+ (- br ar) (- bg ag) (- bb ab)) 765.0))))
                  (if (> cov 0.004)
                      (flet ((cc (v) (min 255 (max 0 (round (/ v cov))))))
                        (setf (aref cr i) (cc ar) (aref cg i) (cc ag) (aref cb i) (cc ab)
                              (aref ca i) (min 255 (max 0 (round (* 255.0 cov))))))
                      (setf (aref cr i) 0 (aref cg i) 0 (aref cb i) 0 (aref ca i) 0))))
              (%apply-filters filters cr cg cb ca sw sh)
              (dotimes (sy sh)
                (let ((dy (+ sy0 sy)))
                  (when (and (>= dy 0) (< dy (canvas-height cv)))
                    (dotimes (sx sw)
                      (let ((dx (+ sx0 sx)))
                        (when (and (>= dx 0) (< dx (canvas-width cv)))
                          (let* ((i (+ (* sy sw) sx)) (a (aref ca i)))
                            (when (plusp a)
                              (blend-put cv dx dy (aref cr i) (aref cg i) (aref cb i)
                                         (min 255 (max 0 (round (* a alpha))))))))))))))))))))

;;; ---- mix-blend-mode (CSS Compositing 1 §mix-blend-mode) ------------------

(declaim (inline blend-channel))
(defun blend-channel (mode cb cs)
  "Separable blend of one backdrop channel CB with source channel CS (0-255)."
  (declare (type (unsigned-byte 8) cb cs))
  (ecase mode
    (:multiply   (truncate (* cb cs) 255))
    (:screen     (- 255 (truncate (* (- 255 cb) (- 255 cs)) 255)))
    (:overlay    (if (< cb 128) (truncate (* 2 cb cs) 255)
                     (- 255 (truncate (* 2 (- 255 cb) (- 255 cs)) 255))))
    (:hard-light (if (< cs 128) (truncate (* 2 cs cb) 255)
                     (- 255 (truncate (* 2 (- 255 cs) (- 255 cb)) 255))))
    (:darken     (min cb cs))
    (:lighten    (max cb cs))
    (:difference (abs (- cb cs)))
    (:exclusion  (- (+ cb cs) (truncate (* 2 cb cs) 255)))
    (:soft-light                        ; W3C soft-light, integer approximation
     (let ((b (/ cb 255.0)) (s (/ cs 255.0)))
       (let ((res (if (<= s 0.5)
                      (- b (* (- 1.0 (* 2.0 s)) b (- 1.0 b)))
                      (let ((d (if (<= b 0.25) (* (- (* 16.0 b) 12.0) (+ (* b b) b))
                                   (sqrt b))))
                        (+ b (* (- (* 2.0 s) 1.0) (- d b)))))))
         (min 255 (max 0 (round (* 255.0 res)))))))))

(defun paint-blended (cv lb mode &optional (alpha 1.0))
  "CSS Compositing 1 §mix-blend-mode: render LB's subtree upright into two offscreen
buffers (over black + white), recover per-pixel straight colour + coverage, then
composite over CV blending each pixel's source colour with the CV backdrop already
painted behind it via MODE.  The canvas is opaque, so the composite reduces to
mix(backdrop, blend(backdrop, source), coverage*ALPHA)."
  (multiple-value-bind (sx0 sy0 sx1 sy1) (subtree-paint-bounds lb)
    (decf sx0) (decf sy0) (incf sx1) (incf sy1)
    (let ((sw (- sx1 sx0)) (sh (- sy1 sy0)))
      (when (and (plusp sw) (plusp sh) (<= (* sw sh) 16000000))
        (let ((offa (make-canvas sw sh '(0 0 0)))
              (offb (make-canvas sw sh '(255 255 255))))
          (shift-box lb (- sx0) (- sy0))
          (let ((*clip* nil) (*round-clip* nil))
            (%paint-box-content offa lb)
            (%paint-box-content offb lb))
          (shift-box lb sx0 sy0)
          (let ((pa (canvas-pixels offa)) (pb (canvas-pixels offb))
                (dpx (canvas-pixels cv)) (cw (canvas-width cv)) (chh (canvas-height cv)))
            (dotimes (sy sh)
              (let ((dy (+ sy0 sy)))
                (when (and (>= dy 0) (< dy chh))
                  (dotimes (sx sw)
                    (let ((dx (+ sx0 sx)))
                      (when (and (>= dx 0) (< dx cw))
                        (let* ((i (* 3 (+ (* sy sw) sx)))
                               (ar (aref pa i)) (ag (aref pa (+ i 1))) (ab (aref pa (+ i 2)))
                               (br (aref pb i)) (bg (aref pb (+ i 1))) (bb (aref pb (+ i 2)))
                               (cov (- 1.0 (/ (+ (- br ar) (- bg ag) (- bb ab)) 765.0))))
                          (when (> cov 0.004)
                            (let* ((di (* 3 (+ (* dy cw) dx)))
                                   (kr (aref dpx di)) (kg (aref dpx (+ di 1))) (kb (aref dpx (+ di 2))))
                              (flet ((cc (v) (min 255 (max 0 (round (/ v cov))))))
                                (blend-put cv dx dy
                                           (blend-channel mode kr (cc ar))
                                           (blend-channel mode kg (cc ag))
                                           (blend-channel mode kb (cc ab))
                                           (min 255 (max 0 (round (* 255.0 cov alpha)))))))))))))))))))))

(defun paint-box (cv lb)
  (handler-case (%paint-box cv lb) (error () nil)))
(defun %paint-box (cv lb)
  ;; A rotate/skew/general-matrix transform is rasterised (subtree painted upright,
  ;; then inverse-mapped); everything else — untransformed, pure-translate, axis
  ;; scale — paints byte-identically through %PAINT-BOX-CONTENT.  An opacity < 1
  ;; renders the subtree to offscreen buffers and composites it as one group.
  (when lb
    (let* ((m (box-raster-transform-matrix lb))
           (cs (lbox-style lb))
           (op (and cs (let ((o (css:cstyle-opacity cs)))
                         (and (numberp o) (>= o 0.0) (< o 1.0) (float o 1.0)))))
           (fil (and cs (css:cstyle-filter cs)))
           (blend (and cs (css:cstyle-mix-blend-mode cs)))
           ;; clip-path (CSS Masking 1): a per-pixel clip region on this box + subtree,
           ;; layered onto *ROUND-CLIP* around the whole paint (own box AND descendants).
           (clip (and cs (css:cstyle-clip-path cs)))
           (crg (and clip (clip-path-region lb clip))))
      (flet ((body ()
               (cond (fil (paint-filtered cv lb fil (or op 1.0)))
                     (blend (paint-blended cv lb blend (or op 1.0)))
                     (op (if m (paint-transformed cv lb m op) (paint-opacity cv lb op)))
                     (m (paint-transformed cv lb m))
                     (t (%paint-box-content cv lb)))))
        (if crg
            (let ((*round-clip* (cons crg *round-clip*))) (body))
            (body))))))
(defun %paint-box-content (cv lb)
  (when lb
    (case (lbox-kind lb)
      (:block
       (let* ((cs (lbox-style lb))
              (hoist (and cs (lbox-hoist-neg lb cs)))
              (kids (if hoist (remove-if (lambda (c) (member c hoist)) (lbox-children lb))
                        (lbox-children lb))))
        (let ((vis (box-visible-p cs)))
         ;; visibility:hidden/collapse suppresses only THIS box's own rendering
         ;; (background, border, image, marker, outline); its layout space and its
         ;; descendants are unaffected, and a descendant may re-assert
         ;; visibility:visible to paint (CSS 2.1 §11.2 — visibility is inherited but
         ;; overridable).  So VIS gates the own-paint steps below while children
         ;; (hoisted neg-z + the normal child pass) always recurse.
         ;; A negative z-index positioned child of a non-stacking-context box belongs
         ;; to the ancestor stacking context: paint it BEHIND this box's own
         ;; background/border (CSS 2.1 §9.9.1), then omit it from the normal child
         ;; pass so it is not repainted on top.
         (dolist (c hoist) (paint-box cv c))
         ;; An anonymous box (e.g. the wrapper generated for a block nested inside
         ;; an inline — HN's <a><div class=votearrow>) can carry a NIL style: it
         ;; paints no background/border of its own, but its children MUST still
         ;; paint, so every style-dependent step is guarded by (when cs ...).
         (when (and cs vis)
           ;; box-shadow (CSS Backgrounds 3 §7): outset shadows paint BEHIND the box,
           ;; before its own background/border.  Gated on a non-NIL box-shadow slot, so
           ;; a box without box-shadow paints byte-identically.
           (when (css:cstyle-box-shadow cs) (paint-box-shadows cv lb cs nil))
           ;; border-radius: the background/gradient/image of a rounded box is
           ;; clipped to the rounded background-clip box (CSS Backgrounds 3 §5.5).
           ;; RADII is NIL for the common unrounded box, so *ROUND-CLIP* stays NIL
           ;; and every fill takes its byte-identical rectangular fast path.
           (let ((radii (and (box-has-radius-p cs) (corner-radii-px cs (lbox-w lb) (lbox-h lb)))))
            (macrolet ((with-bg-round ((clip-box) &body body)
                         `(let ((*round-clip*
                                  (let ((rg (and radii (bg-round-region lb cs ,clip-box radii))))
                                    (if rg (cons rg *round-clip*) *round-clip*))))
                            ,@body)))
             ;; background-color is the BOTTOM-most background layer (CSS Backgrounds 3
             ;; §3.10): always paint it first, then the gradient/image layers over it —
             ;; so a sized/positioned gradient tile that does not cover the box lets the
             ;; base colour (e.g. a `linear-gradient(blue,blue)` bottom layer folded into
             ;; background-color) show through.  For an opaque full-box gradient the
             ;; colour is fully covered, so this is byte-identical to painting only it.
             (when (css:cstyle-background cs)
               ;; background-clip: fill only the painting area (default border-box ==
               ;; the whole box; padding/content-box inset by border[+padding]).
               (multiple-value-bind (bx0 by0 bx1 by1) (bg-box-edges lb cs (effective-bg-clip cs))
                 (with-bg-round ((effective-bg-clip cs))
                   (fill-rect cv bx0 by0 (- bx1 bx0) (- by1 by0) (css:cstyle-background cs)))))
             ;; background-image LAYERS (CSS Backgrounds 3 §3.11.2): a comma-separated
             ;; background paints its image layers from the BOTTOM-most (last listed) to
             ;; the TOP-most (first listed), over the background colour.  A CSS gradient
             ;; is a background image: rasterised (honouring background-position/-size/
             ;; -repeat) as a tiled layer.  Fixed-attachment images are viewport-anchored.
             ;; A single layer routes through the same per-layer core → byte-identical.
             (paint-bg-layers cv lb cs radii)))
           ;; box-shadow (CSS Backgrounds 3 §7): inset shadows paint OVER the background,
           ;; clipped to the padding box, under the box's border and content.
           (when (css:cstyle-box-shadow cs) (paint-box-shadows cv lb cs t)))
         (when (and vis (lbox-img lb))
           (let ((fit (and cs (css:cstyle-object-fit cs))))
             (if (and fit (not (string= fit "fill")))
                 (multiple-value-bind (ox oy ow oh src)
                     (object-fit-geom fit (img-w (lbox-img lb)) (img-h (lbox-img lb))
                                      (round (lbox-x lb)) (round (lbox-y lb))
                                      (round (lbox-w lb)) (round (lbox-h lb))
                                      (css:cstyle-object-position cs))
                   (when (and (plusp ow) (plusp oh))
                     (blit-img cv (lbox-img lb) ox oy ow oh src)))
                 (blit-img cv (lbox-img lb) (round (lbox-x lb)) (round (lbox-y lb))
                           (round (lbox-w lb)) (round (lbox-h lb))))))
         ;; Replaced vector content (inline <svg>, <canvas>): composite over the
         ;; box's background, under its borders.
         (when (and vis (lbox-vpaint lb))
           (funcall (lbox-vpaint lb) cv (round (lbox-x lb)) (round (lbox-y lb))
                    (round (lbox-w lb)) (round (lbox-h lb))))
         (when (and cs vis)
           ;; border-image (§6): when a decodable source is set, the 9-sliced image
           ;; replaces the normal border paint; otherwise fall back to the border.
           (unless (and (css:cstyle-border-image-source cs) (paint-border-image cv lb cs))
             (paint-borders cv lb cs))
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
         ;; Clipping is per-axis (css-overflow-3): overflow-x:clip / overflow-y:visible
         ;; clips horizontally while letting content overflow vertically.  A visible
         ;; axis uses an effectively unbounded edge so only the clipped axis constrains.
         (if (and cs (member (css:cstyle-overflow cs) '("hidden" "clip" "scroll") :test #'string=))
             (let* ((cx (member (css:cstyle-overflow-x cs) '("hidden" "clip" "scroll") :test #'string=))
                    (cy (member (css:cstyle-overflow-y cs) '("hidden" "clip" "scroll") :test #'string=))
                    ;; overflow-clip-margin (css-overflow-3): for overflow:clip, the clip
                    ;; region is taken from the named reference box (default padding box)
                    ;; and grown outward by the margin length.  hidden/scroll ignore it.
                    (ocm (and (string= (css:cstyle-overflow cs) "clip")
                              (css:cstyle-overflow-clip-margin cs)))
                    (rbox (if ocm (car ocm) :padding)) (rmar (if ocm (cdr ocm) 0.0))
                    (padl (css::resolve-pad (css:cstyle-padding-left cs) (lbox-w lb)))
                    (padt (css::resolve-pad (css:cstyle-padding-top cs) (lbox-w lb)))
                    (padr (css::resolve-pad (css:cstyle-padding-right cs) (lbox-w lb)))
                    (padb (css::resolve-pad (css:cstyle-padding-bottom cs) (lbox-w lb)))
                    (il (ecase rbox (:border 0.0) (:padding (used-border cs :l)) (:content (+ (used-border cs :l) padl))))
                    (it (ecase rbox (:border 0.0) (:padding (used-border cs :t)) (:content (+ (used-border cs :t) padt))))
                    (ir (ecase rbox (:border 0.0) (:padding (used-border cs :r)) (:content (+ (used-border cs :r) padr))))
                    (ib (ecase rbox (:border 0.0) (:padding (used-border cs :b)) (:content (+ (used-border cs :b) padb))))
                    (bl (round (- (+ (lbox-x lb) il) rmar)))
                    (bt (round (- (+ (lbox-y lb) it) rmar)))
                    (brr (round (+ (- (+ (lbox-x lb) (lbox-w lb)) ir) rmar)))
                    (bb (round (+ (- (+ (lbox-y lb) (lbox-h lb)) ib) rmar)))
                    (*clip* (clip-intersect (if cx bl -1000000) (if cy bt -1000000)
                                            (if cx brr 1000000) (if cy bb 1000000)))
                    ;; border-radius + overflow clip: also clip descendants to the
                    ;; rounded padding box (§5.5 / css-overflow-3).  Only for the plain
                    ;; both-axes padding-box case: a rounded region has no meaning when
                    ;; only one axis is clipped, and `overflow: clip` with an
                    ;; overflow-clip-margin uses an expanded-corner geometry we do not
                    ;; model, so restrict to hidden/scroll/auto (the common case).
                    (rr (and cx cy (not ocm)
                             (not (string= (css:cstyle-overflow cs) "clip"))
                             (box-has-radius-p cs)
                             (let ((radii (corner-radii-px cs (lbox-w lb) (lbox-h lb))))
                               (when (some #'plusp radii)
                                 (make-round-region
                                  bl bt brr bb
                                  (inset-radii radii (used-border cs :l) (used-border cs :t)
                                               (used-border cs :r) (used-border cs :b)))))))
                    (*round-clip* (if rr (cons rr *round-clip*) *round-clip*)))
               (paint-children cv kids))
             (paint-children cv kids))
         ;; outline paints on top of the box + descendants, and is NOT clipped by
         ;; this box's own overflow (CSS-UI §outline; CSS 2.1 appendix E step 10).
         (when (and cs vis) (paint-outline cv lb cs)))))
      (:line
       (loop for cell on (lbox-children lb)
             for it = (car cell)
             do (if (frag-p it)
                    (let ((cs (frag-style it))
                          ;; inline relative-positioning visual shift (§9.4.3): displace
                          ;; this run's paint x and baseline; flow geometry stays put.
                          (fdx (frag-dx it)) (fdy (frag-dy it)))
                      ;; pass the line box geometry so scribe centers the real font
                      ;; em-box (ascent+descent at font-size) within it.  A
                      ;; visibility:hidden run occupies its space but paints no glyphs.
                      (when (box-visible-p cs)
                        ;; inline background (e.g. <mark>, a highlighted <span>): paint
                        ;; the run's box behind its glyphs.  background-color is not
                        ;; inherited, so a non-nil value here is the run element's own.
                        ;; Only an actual inline-level run (a <span>/<mark>/<a> etc.)
                        ;; paints its own background across the line fragments it
                        ;; spans.  Anonymous text directly in a block carries the
                        ;; BLOCK's style, whose background is painted once by the block
                        ;; box — repainting it per line box (at the line's height) is
                        ;; both redundant and wrong for a `height:0`/overflowing block,
                        ;; which would bleed the background over its overflow lines.
                        (let ((bg (and (string= (cdisplay cs) "inline") (css:cstyle-background cs))))
                          (when (and bg (or (< (length bg) 4) (plusp (fourth bg))))
                            ;; extend across the inter-word gap when the next run is
                            ;; the SAME element (a multi-word highlight), so its spaces
                            ;; are covered too — but not into a separate adjacent chip.
                            (let* ((nx (cadr cell))
                                   (right (if (and (frag-p nx) (eq (frag-node nx) (frag-node it)))
                                              (frag-x nx)
                                              (+ (frag-x it) (frag-w it)))))
                              (fill-rect cv (round (+ (frag-x it) fdx)) (+ (lbox-y lb) fdy)
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
                          ;; CSS Text Decoration 3 §text-shadow: paint each shadow
                          ;; BEHIND the glyphs, bottom-most first (the first-listed
                          ;; shadow ends up on top).  Sharp offset copy in the shadow
                          ;; colour (currentColor -> the run's own colour); blur is
                          ;; not rasterised.  A transparent shadow colour (alpha 0)
                          ;; paints nothing, so opacity-zero shadows are free.
                          (dolist (sh (reverse (css:cstyle-text-shadow cs)))
                            (destructuring-bind (offx offy blur scolor) sh
                              (declare (ignore blur))
                              (draw-text-scribe cv (frag-text it)
                                   (round (+ (frag-x it) fdx offx))
                                   (+ (lbox-y lb) fdy offy) (lbox-h lb)
                                   (if (eq scolor :currentcolor) (css:cstyle-color cs) scolor)
                                   (css:cstyle-font-size cs)
                                   :face (style-face cs)
                                   :bold (>= (css:cstyle-font-weight cs) 600)
                                   :letter-spacing (css:cstyle-letter-spacing cs)
                                   :baseline-off (lbox-baseline lb))))
                          (draw-text-scribe cv (frag-text it) (round (+ (frag-x it) fdx))
                                   (+ (lbox-y lb) fdy) (lbox-h lb)
                                   (css:cstyle-color cs)
                                   (css:cstyle-font-size cs)
                                   :face (style-face cs)
                                   :bold (>= (css:cstyle-font-weight cs) 600)
                                   :letter-spacing (css:cstyle-letter-spacing cs)
                                   :underline ul
                                   :underline-end-x (and uend (+ uend fdx))
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

(defun canvas-bg-style (cs)
  "True when CS carries a background that participates in canvas propagation
(CSS 2.1 §14.2 / css-backgrounds §3.11.3): a non-transparent colour, a bg image,
or a bg gradient."
  (and cs (or (css:cstyle-background cs) (css:cstyle-bg-image cs) (css:cstyle-bg-gradient cs))))

(defun propagate-canvas-bg (cv pe-cs anchor-lb anchor-cs width height)
  "Paint the propagation element PE-CS's background IMAGE and GRADIENT onto the
canvas (CSS 2.1 §14.2).  The root element's — else the body's — background is
transferred to the canvas: the PAINTING area is the whole canvas, but the
POSITIONING area is the ROOT element's background positioning area (its padding
box, ANCHOR-LB inset by ANCHOR-CS's borders) — NOT the canvas origin and NOT the
supplying element's own box.  So the html's margin offsets the anchor, and a body
background (used because the root is transparent) is positioned as if it were the
root's.  The caller strips PE-CS's own image/gradient so paint-box does not repaint
them at the element box.  Non-fixed image only; the colour is the canvas fill."
  (when (and (css:cstyle-bg-gradient pe-cs) (gradient-visible-p (css:cstyle-bg-gradient pe-cs)))
    (let ((grad (css:cstyle-bg-gradient pe-cs)) (curcolor (css:cstyle-color pe-cs)))
      (if (or (null anchor-lb) (string-equal (css:cstyle-bg-attachment pe-cs) "fixed"))
          ;; background-attachment:fixed anchors the tile to the viewport origin (0,0),
          ;; not the root's padding box (CSS Backgrounds 3 §3.10).
          (if (string-equal (css:cstyle-bg-attachment pe-cs) "fixed")
              (tile-gradient cv grad curcolor 0 0 width height 0 0 width height
                             (css:cstyle-bg-repeat pe-cs) (css:cstyle-bg-size pe-cs)
                             (css:cstyle-bg-position pe-cs))
              (fill-css-gradient cv 0 0 width height grad curcolor))
          ;; positioning area = root element's padding box; painting area = whole canvas
          (let ((ax0 (round (+ (lbox-x anchor-lb) (used-border anchor-cs :l))))
                (ay0 (round (+ (lbox-y anchor-lb) (used-border anchor-cs :t))))
                (aw (round (- (lbox-w anchor-lb) (used-border anchor-cs :l) (used-border anchor-cs :r))))
                (ah (round (- (lbox-h anchor-lb) (used-border anchor-cs :t) (used-border anchor-cs :b)))))
            (tile-gradient cv grad curcolor ax0 ay0 aw ah 0 0 width height
                           (css:cstyle-bg-repeat pe-cs) (css:cstyle-bg-size pe-cs)
                           (css:cstyle-bg-position pe-cs))))))
  (when (and anchor-lb (css:cstyle-bg-image pe-cs)
             (not (string-equal (css:cstyle-bg-attachment pe-cs) "fixed")))
    (let* ((url (css:cstyle-bg-image pe-cs))
           (duri (if (find #\% url) (percent-decode url) url))
           (img (if (and (>= (length duri) 5) (string-equal (subseq duri 0 5) "data:"))
                    (ignore-errors (decode-image duri))
                    (ignore-errors (fetch-image url)))))
      (when (and img (plusp (img-w img)) (plusp (img-h img)))
        (let* ((rep (css:cstyle-bg-repeat pe-cs))
               (repx (member rep '("repeat" "repeat-x") :test #'string=))
               (repy (member rep '("repeat" "repeat-y") :test #'string=))
               ;; anchor = root element's background positioning area (padding box)
               (ax0 (round (+ (lbox-x anchor-lb) (used-border anchor-cs :l))))
               (ay0 (round (+ (lbox-y anchor-lb) (used-border anchor-cs :t))))
               (aw (round (- (lbox-w anchor-lb) (used-border anchor-cs :l) (used-border anchor-cs :r))))
               (ah (round (- (lbox-h anchor-lb) (used-border anchor-cs :t) (used-border anchor-cs :b)))))
          (multiple-value-bind (iw ih) (bg-tile-size (css:cstyle-bg-size pe-cs) img aw ah)
            (let* ((pos (css:cstyle-bg-position pe-cs))
                   (offx (if pos (bg-pos-offset (first pos) (- aw iw)) 0))
                   (offy (if pos (bg-pos-offset (second pos) (- ah ih)) 0))
                   (ox (+ ax0 offx)) (oy (+ ay0 offy)))
              (when (and (> iw 0) (> ih 0))
                ;; painting area = whole canvas; tiles anchored at OX,OY
                (let ((*clip* (clip-intersect 0 0 width height)))
                  (let ((startx (if repx (- ox (* iw (ceiling ox iw))) ox))
                        (starty (if repy (- oy (* ih (ceiling oy ih))) oy))
                        (budget 200000))
                    (block tiles
                      (loop for ty = starty then (+ ty ih)
                            while (and (< ty height) (or repy (= ty starty))) do
                        (loop for tx = startx then (+ tx iw)
                              while (and (< tx width) (or repx (= tx startx))) do
                          (when (and (> (+ tx iw) 0) (> (+ ty ih) 0))
                            (blit-img cv img tx ty iw ih)
                            (when (<= (decf budget) 0) (return-from tiles))))))))))))))))

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
    (multiple-value-bind (root adv styles)
        (layout-with-container-queries doc styles sheet width vph
                                       (and viewport-p scroll-to)
                                       (and viewport-height (round viewport-height)))
      (declare (ignore adv))
      (let* ((content-h (if root (round (+ (lbox-y root) (lbox-h root) 8)) min-height))
             (height (if vph vph (min max-height (max min-height content-h))))
             (body (css:query-select doc "body"))
             (root-el (css:query-select doc "html"))
             (body-cs (and body (gethash body styles)))
             (root-cs (and root-el (gethash root-el styles)))
             ;; CSS 2.1 §14.2: the ROOT element's background propagates to the
             ;; canvas; only when the root has NO background of its own does the
             ;; BODY's background propagate instead.
             (pe-cs (cond ((canvas-bg-style root-cs) root-cs)
                          ((canvas-bg-style body-cs) body-cs)
                          (t nil)))
             (bg (and pe-cs (css:cstyle-background pe-cs)))
             (cv (make-canvas width height (if bg (rgb bg) '(255 255 255)))))
        ;; propagate the element's background IMAGE/GRADIENT onto the canvas at the
        ;; ICB origin, then strip them so paint-box does not repaint at the box.
        (when (and pe-cs (or (css:cstyle-bg-image pe-cs) (css:cstyle-bg-gradient pe-cs)))
          (propagate-canvas-bg cv pe-cs root root-cs width height)
          (setf (css:cstyle-bg-image pe-cs) nil (css:cstyle-bg-gradient pe-cs) nil))
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
