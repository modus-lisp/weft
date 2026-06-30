;;;; src/render/layout.lisp — block + inline-formatting layout, paint, render.
;;;;
;;;; Normal-flow layout: block-level boxes stacked vertically (margin/border/
;;;; padding), with INLINE formatting contexts that lay styled text runs into
;;;; line boxes — each fragment keeps its own color/weight/decoration, so bold,
;;;; links, and colored spans render correctly.  Mixed block+inline children are
;;;; grouped into anonymous inline runs.  List items get markers.  Painted to a
;;;; canvas and saved as PNG.
(in-package #:weft.render)

(defstruct lbox x y w h style node kind children marker img)   ; kind :block | :line; img = decoded IMG
(defstruct frag x w text style)                            ; a positioned styled text run on a line

(defvar *floats* nil "Page-scoped float list: each (side left right top bottom).")
(defvar *abs-pending* nil
  "Out-of-flow absolutely-positioned boxes awaiting resolution against the
nearest positioned ancestor: a list of (lbox . cstyle).  Rebound fresh by every
positioned element; the top-level binding (layout-tree) is the initial CB.")
(defvar *fixed-pending* nil
  "Out-of-flow fixed boxes awaiting resolution against the viewport: (lbox . cstyle).")

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
(defparameter *block-displays* '("block" "list-item" "flex" "table"))
(defun inline-level-p (styles node)
  (case (h:dnode-kind node)
    (:text t)
    (:element (let ((cs (st styles node))) (and cs (not (member (cdisplay cs) (cons "none" *block-displays*) :test #'string=)))))
    (t nil)))
(defun block-level-p (styles node)
  (and (eq (h:dnode-kind node) :element)
       (let ((cs (st styles node))) (and cs (member (cdisplay cs) *block-displays* :test #'string=)))))

;;; ---- inline content: styled word runs -> line boxes --------------------
(defun collect-words (node styles default-style content-w)
  "Walk inline content of NODE; return a list of inline tokens: (word . style)
text runs and (:atomic . lbox) for inline-block / replaced boxes."
  (let ((words '()))
    (labels ((emit-text (s style)
               (let ((b (make-string-output-stream)) (any nil))
                 (loop for c across s do
                   (if (member c '(#\Space #\Tab #\Newline #\Return))
                       (when any (push (cons (get-output-stream-string b) style) words) (setf any nil b (make-string-output-stream)))
                       (progn (write-char c b) (setf any t))))
                 (when any (push (cons (get-output-stream-string b) style) words))))
             (rec (n owner)
               (case (h:dnode-kind n)
                 (:text (emit-text (h:dnode-data n) owner))
                 (:element
                  (let ((cs (or (st styles n) owner)))
                    (cond
                      ((and cs (string= (cdisplay cs) "none")))
                      ((member (h:dnode-name n) '("script" "style") :test #'string=))
                      ((and cs (member (cdisplay cs) '("inline-block" "flex" "table") :test #'string=))
                       (multiple-value-bind (lb adv) (layout-node n styles 0 0 content-w)
                         (declare (ignore adv))
                         (when lb (push (cons :atomic lb) words))))
                      ((string= (h:dnode-name n) "img")    ; replaced element placeholder
                       (let ((lb (img-box n cs))) (when lb (push (cons :atomic lb) words))))
                      (t (loop for c across (h:dnode-children n) do (rec c cs)))))))))
      (rec node (or (st styles node) default-style)))
    (nreverse words)))

(defun img-attr-num (node name)
  (let ((v (cdr (assoc name (h:dnode-attrs node) :test #'string-equal))))
    (when v (ignore-errors (parse-integer (string-trim '(#\Space #\p #\x) v) :junk-allowed t)))))

(defun img-box (node cs)
  "A box for <img>: if src is a decodable data: URI, paint real pixels at its
intrinsic size (or CSS/HTML override); else an alt-text placeholder."
  (let* ((src (cdr (assoc "src" (h:dnode-attrs node) :test #'string-equal)))
         (decoded (and src (>= (length src) 5) (string-equal (subseq src 0 5) "data:")
                       (ignore-errors (decode-image src))))
         (cw (let ((sw (css::resolve-size (css:cstyle-width cs) 300))) (and (numberp sw) sw)))
         (chh (let ((sh (css::resolve-size (css:cstyle-height cs) 200))) (and (numberp sh) sh)))
         (w (or cw (img-attr-num node "width") (and decoded (img-w decoded)) 120))
         (hh (or chh (img-attr-num node "height") (and decoded (img-h decoded)) 90))
         (alt (cdr (assoc "alt" (h:dnode-attrs node) :test #'string-equal)))
         (lb (make-lbox :x 0 :y 0 :w w :h hh :style cs :kind :block :img decoded)))
    (unless decoded
      (setf (lbox-children lb)
            (when (and alt (plusp (length alt)))
              (list (make-lbox :x 2 :y (max 0 (floor (- hh *font-h*) 2)) :w (- w 4) :h *font-h* :kind :line
                               :children (list (make-frag :x 2 :w (word-w alt cs) :text alt :style cs)))))))
    lb))

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

(defun word-w (word &optional style)
  "Reserved width for WORD at its style's font-size — the width scribe will paint
\(falls back to the bitmap metric inside MEASURE-TEXT-WIDTH on font failure)."
  (round (measure-text-width word (style-size style))))

(defun space-w (&optional style)
  "Reserved inter-word space width at STYLE's font-size (the font's space-glyph
advance), defaulting to the bitmap metric."
  (round (measure-text-width " " (style-size style))))

(defun next-float-bottom (y)
  "Smallest float bottom strictly greater than Y, or NIL."
  (let ((best nil))
    (dolist (f *floats*)
      (let ((fb (fifth f))) (when (> fb y) (setf best (if best (min best fb) fb)))))
    best))

(defun layout-inline (words content-x start-y content-w base-cs)
  "Greedy-wrap WORDS into line boxes, flowing around active floats and honoring
text-align.  Returns (values line-boxes total-height)."
  (let* ((lh (max *font-h* (round (* (css:cstyle-font-size base-cs) (css:cstyle-line-height base-cs)))))
         (align (css:cstyle-text-align base-cs))
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
        (let* ((avail (- rx lx)) (cur '()) (cx lx) (line-h lh))
          (loop while (< i n) do
            (let* ((wd (aref ws i)) (atomic (eq (car wd) :atomic))
                   (sw (space-w (if atomic base-cs (cdr wd))))
                   (ww (if atomic (lbox-w (cdr wd)) (word-w (car wd) (cdr wd))))
                   (need (if cur (+ sw ww) ww)))
              (when (and cur (not nowrap) (> (+ (- cx lx) need) avail)) (return))
              (when (> (- cx lx) 0) (incf cx sw))
              (if atomic
                  (let ((lb (cdr wd)))
                    (shift-box lb (round (- cx (lbox-x lb))) 0)
                    (setf line-h (max line-h (lbox-h lb))) (push lb cur))
                  (push (make-frag :x cx :w ww :text (car wd) :style (cdr wd)) cur))
              (incf cx ww) (incf i)))
          (when (null cur)                       ; one item too wide for the band: force it
            (let* ((wd (aref ws i)))
              (if (eq (car wd) :atomic) (let ((lb (cdr wd))) (shift-box lb (round (- lx (lbox-x lb))) 0)
                                          (setf line-h (max line-h (lbox-h lb))) (push lb cur))
                  (push (make-frag :x lx :w (word-w (car wd) (cdr wd)) :text (car wd) :style (cdr wd)) cur))
              (incf i)))
          (let* ((items (nreverse cur))
                 (lastx (let ((it (car (last items)))) (if (frag-p it) (+ (frag-x it) (frag-w it)) (+ (lbox-x it) (lbox-w it)))))
                 (used (- lastx lx))
                 (shift (cond ((string= align "center") (max 0 (floor (- avail used) 2)))
                              ((string= align "right") (max 0 (- avail used))) (t 0))))
            (when (plusp shift)
              (dolist (it items) (if (frag-p it) (incf (frag-x it) shift) (shift-box it shift 0))))
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
(defun layout-node (node styles x y avail-w &optional avail-h)
  "Resilient wrapper: a failing subtree degrades to an empty box, not a crash.
AVAIL-H is the containing-block height in px when definite, else NIL (CSS 2.1
10.5: a percentage height resolves against it only when definite)."
  (handler-case (%layout-node node styles x y avail-w avail-h)
    (error (e) (if *layout-debug* (error e) (values nil 0 0 0)))))

(defun pad-box (lb cs)
  "Padding box (px py pw ph) of LB — the border-box minus borders.  This is the
containing block that absolutely-positioned descendants resolve against."
  (let ((bl (css:cstyle-border-left-width cs)) (br (css:cstyle-border-right-width cs))
        (bt (css:cstyle-border-top-width cs)) (bb (css:cstyle-border-bottom-width cs)))
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
             (nx (cond ((numberp left)  (+ px left))
                       ((numberp right) (+ px (- pw (lbox-w lb) right)))
                       (t (lbox-x lb))))
             (ny (cond ((numberp top)    (+ py top))
                       ((numberp bottom) (+ py (- ph (lbox-h lb) bottom)))
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
        (let ((*abs-pending* nil))
          (multiple-value-bind (lb adv mt-eff mb-eff) (%layout-core node styles x y avail-w avail-h)
            (when (and lb *abs-pending*)
              (let ((cb (pad-box lb cs)))
                (dolist (p *abs-pending*) (resolve-positioned (car p) cb (cdr p)))))
            (values lb adv mt-eff mb-eff)))
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
           (bt (css:cstyle-border-top-width cs)) (bb (css:cstyle-border-bottom-width cs))
           (bl (css:cstyle-border-left-width cs)) (br (css:cstyle-border-right-width cs))
           (border-box (string= (css:cstyle-box-sizing cs) "border-box"))
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
                                                   (css:cstyle-border-top-width cs) (css:cstyle-border-bottom-width cs)))
                                       exp-h))))
           (spec-w (css::resolve-size (css:cstyle-width cs) avail-w))      ; px or nil
           (max-w (css::resolve-size (css:cstyle-max-width cs) avail-w))
           (min-w (css::resolve-size (css:cstyle-min-width cs) avail-w))
           ;; an absolutely-positioned / fixed box with width:auto is sized
           ;; shrink-to-fit (CSS 10.3.7): min(available, preferred max-content).
           (shrink (and (null spec-w)
                        (member (css:cstyle-position cs) '("absolute" "fixed") :test #'string=)))
           ;; border-box width of this element
           (width (let ((bw (cond ((numberp spec-w) (if border-box spec-w (+ spec-w pad-bord)))
                                  (shrink (min (- avail-w ml mr)
                                               (+ (pref-content-width node styles (- avail-w ml mr))
                                                  pad-bord)))
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
           (list-item (string= (cdisplay cs) "list-item"))
           ;; effective (collapsed) outer margins reported up to the parent:
           ;; default to this box's own margins, raised by parent/child collapse.
           (mt-eff mt) (mb-eff mb)
           (children '()) (content-h 0))
      ;; <pre>/white-space:pre — preserve newlines, no wrapping
      (when (and (string= (css:cstyle-white-space cs) "pre") (not (has-block-children styles node)))
        (let* ((text (collect-raw node)) (yy cy)
               (lh (max *font-h* (round (* (css:cstyle-font-size cs) (css:cstyle-line-height cs))))))
          (dolist (ln (split-newlines text))
            (push (make-lbox :x cx :y yy :w content-w :h lh :kind :line
                             :children (when (plusp (length ln))
                                         (list (make-frag :x cx :w (word-w ln cs) :text ln :style cs))))
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
            (group '()) (yy cy) (prev-mb nil) (content-started nil) (first-child-mt nil))
        (flet ((flush-inline ()
                 (when group
                   (let ((words (loop for g in (nreverse group) append (collect-words g styles cs content-w))))
                     (when words
                       (multiple-value-bind (lines lh-total) (layout-inline words cx yy content-w cs)
                         (dolist (l lines) (push l children))
                         (incf yy lh-total) (incf content-h lh-total)
                         ;; inline content separates block margins: no collapse,
                         ;; and the box is no longer empty / a fresh first child.
                         (setf prev-mb nil content-started t))))
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
                 (let ((lb (place-float k styles cx (+ cx content-w) yy content-w)))
                   (when lb (push lb children))))
                ((block-level-p styles k)
                 (flush-inline)
                 (let ((cleared nil))
                   (when (and kcs (member (css:cstyle-clear kcs) '("left" "right" "both") :test #'string=))
                     (let ((ny (clear-y yy cx (+ cx content-w)
                                       (case (intern (string-upcase (css:cstyle-clear kcs)) :keyword)
                                         (:left '(:left)) (:right '(:right)) (t '(:left :right))))))
                       (when (> ny yy)                                          ; clearance: margins do not collapse across it
                         (incf content-h (- ny yy)) (setf yy ny)
                         (setf prev-mb nil cleared t))))
                   (let ((own-mt (if kcs (css:cstyle-margin-top kcs) 0)))
                     ;; lay the child out at YY, then SHIFT it so its border-top
                     ;; lands at YY+GAP (GAP = the collapsed adjoining margin).
                     ;; CMT/CMB are the child's *effective* margins (already
                     ;; collapsed with its own first/last child).
                     (multiple-value-bind (lb adv cmt cmb) (layout-node k styles cx yy content-w child-avail-h)
                       (declare (ignore adv))
                       (when lb
                         (let* ((cmt (or cmt own-mt))
                                (cmb (or cmb (if kcs (css:cstyle-margin-bottom kcs) 0)))
                                ;; parent/first in-flow child top-margin collapse:
                                ;; only with zero top border AND padding, at the
                                ;; very start of flow (nothing precedes the child).
                                (top-collapse (and (not content-started) (not cleared)
                                                   (zerop bt) (zerop pt)))
                                (gap (cond (prev-mb (collapse-margins prev-mb cmt))
                                           (top-collapse 0)   ; margin bubbles to parent's mt-eff
                                           (t cmt))))
                           (when top-collapse (setf first-child-mt cmt))
                           (shift-box lb 0 (round (- (+ yy gap) (lbox-y lb))))   ; flow placement
                           (when (and kcs (string= pos "relative"))             ; visual shift, flow unchanged
                             (shift-box lb (round (cond ((numberp (css:cstyle-left kcs)) (css:cstyle-left kcs))
                                                        ((numberp (css:cstyle-right kcs)) (- (css:cstyle-right kcs))) (t 0)))
                                        (round (cond ((numberp (css:cstyle-top kcs)) (css:cstyle-top kcs))
                                                     ((numberp (css:cstyle-bottom kcs)) (- (css:cstyle-bottom kcs))) (t 0)))))
                           (push lb children)
                           (let ((new-yy (+ (lbox-y lb) (lbox-h lb))))          ; border-bottom edge
                             (incf content-h (- new-yy yy))
                             (setf yy new-yy)
                             (setf prev-mb cmb)                                  ; held to collapse with next sibling
                             (setf content-started t))))))))
                ((or (eq (h:dnode-kind k) :text) (inline-level-p styles k)) (push k group)))))
          (flush-inline)
          ;; The last in-flow block's bottom margin (PREV-MB) was held back from
          ;; YY.  Parent/last-child collapse (CSS 2.1 8.3.1): when this box has
          ;; auto height and zero bottom border AND padding, that margin sticks
          ;; out below and collapses into MB-EFF; otherwise it is contained and
          ;; adds to the content height.
          (when prev-mb
            (let ((height-auto (not (numberp exp-h))))
              (if (and height-auto (zerop bb) (zerop pb))
                  (setf mb-eff (collapse-margins mb prev-mb))
                  (incf content-h prev-mb))))
          ;; parent/first-child collapse: first child's top margin bubbled up.
          (when first-child-mt
            (setf mt-eff (collapse-margins mt first-child-mt)))))
      (let* ((content-final (cond ((numberp exp-h) (if border-box (- exp-h pad-bord) exp-h)) (t content-h)))
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
        (values lb (+ mt (lbox-h lb) mb) mt-eff mb-eff)))))

(defun shift-box (lb dx dy)
  "Recursively offset LB and its descendants by (DX,DY)."
  (when lb
    (incf (lbox-x lb) dx) (incf (lbox-y lb) dy)
    (if (eq (lbox-kind lb) :line)
        (dolist (it (lbox-children lb))
          (if (frag-p it) (incf (frag-x it) dx) (shift-box it dx dy)))
        (dolist (c (lbox-children lb)) (shift-box c dx dy)))))

(defun est-content-width (node styles)
  "Rough shrink-to-fit width estimate for a flex item."
  (let* ((base (st styles node)) (words (collect-words node styles base 600)) (w 0))
    (dolist (wd words)
      (if (eq (car wd) :atomic)
          (incf w (+ (lbox-w (cdr wd)) (space-w base)))
          (incf w (+ (word-w (car wd) (cdr wd)) (space-w (cdr wd))))))
    (min w 600)))

(defun item-base (item styles content-w)
  (let* ((cs (st styles item)) (basis (css:cstyle-flex-basis cs)))
    (cond
      ((and (stringp basis) (not (member basis '("auto" "content") :test #'string=)))
       (let ((v (css::resolve-len basis (css:cstyle-font-size cs)))) (if (numberp v) v 0)))
      ((numberp (css:cstyle-width cs)) (css:cstyle-width cs))
      ((> (css:cstyle-flex-grow cs) 0) 0)
      (t (min content-w (est-content-width item styles))))))

(defun layout-flex (node styles cx cy content-w base-cs)
  "Single-line flexbox layout.  Returns (values child-lboxes content-height)."
  (let* ((dir (css:cstyle-flex-direction base-cs))
         (row (not (or (string= dir "column") (string= dir "column-reverse"))))
         (justify (css:cstyle-justify-content base-cs))
         (align (css:cstyle-align-items base-cs))
         (gap (css:cstyle-gap base-cs))
         (items (remove-if-not (lambda (k) (let ((c (st styles k))) (and c (not (string= (css:cstyle-display c) "none"))))) (child-elements node)))
         (nitems (length items)))
    (when (zerop nitems) (return-from layout-flex (values nil 0)))
    (let* ((main-avail (if row content-w content-w))   ; column main size is intrinsic; treat width as cross
           (bases (mapcar (lambda (it) (if row (item-base it styles content-w) (item-base it styles content-w))) items))
           (total-gap (* gap (1- nitems)))
           (sum-base (+ (reduce #'+ bases) total-gap))
           (free (- main-avail sum-base))
           (grows (mapcar (lambda (it) (css:cstyle-flex-grow (st styles it))) items))
           (sum-grow (reduce #'+ grows))
           (sizes (if (and row (> free 0) (> sum-grow 0))
                      (mapcar (lambda (b g) (+ b (* free (/ g sum-grow)))) bases grows)
                      bases)))
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
            (values (nreverse boxes) (- y cy gap)))))))

(defun table-rows (node styles)
  "Collect <tr> rows directly under NODE or within row-groups."
  (let ((rows '()))
    (dolist (c (child-elements node))
      (let ((d (cdisplay (st styles c))))
        (cond ((string= d "table-row") (push c rows))
              ((string= d "table-row-group")
               (dolist (r (child-elements c)) (when (string= (cdisplay (st styles r)) "table-row") (push r rows)))))))
    (nreverse rows)))
(defun row-cells (row styles)
  (remove-if-not (lambda (c) (string= (cdisplay (st styles c)) "table-cell")) (child-elements row)))

(defun layout-table (node styles cx cy content-w base-cs)
  "Fixed table layout: equal columns, rows stacked, cells stretched to row height.
Returns (values cell-lboxes content-height)."
  (declare (ignore base-cs))
  (let* ((rows (table-rows node styles)))
    (when (null rows) (return-from layout-table (values nil 0)))
    (let* ((ncols (reduce #'max (mapcar (lambda (r) (length (row-cells r styles))) rows) :initial-value 1))
           (colw (max 1 (floor content-w ncols))) (y cy) (boxes '()))
      (dolist (row rows)
        (let ((cells (row-cells row styles)) (rowh 0) (rowboxes '()) (col 0))
          (dolist (cell cells)
            (multiple-value-bind (lb adv) (layout-node cell styles (+ cx (* col colw)) y colw)
              (declare (ignore adv))
              (when lb (push lb rowboxes) (setf rowh (max rowh (lbox-h lb))))
              (incf col)))
          (dolist (lb rowboxes) (setf (lbox-h lb) rowh))   ; stretch to row height
          (setf boxes (nconc boxes (nreverse rowboxes)))
          (incf y rowh)))
      (values boxes (- y cy)))))

(defun pref-inline-width (node styles cs content-w)
  "Max-content width of NODE's inline content: word + atomic-box widths summed
on a single (unwrapped) line."
  (let ((words (collect-words node styles cs content-w)) (w 0))
    (dolist (wd words)
      (incf w (+ (if (eq (car wd) :atomic) (lbox-w (cdr wd)) (word-w (car wd) (cdr wd)))
                 (space-w (if (eq (car wd) :atomic) cs (cdr wd))))))
    w))

(defun pref-content-width (node styles content-w &optional (depth 0))
  "Shrink-to-fit preferred (max-content) CONTENT width of element NODE: the
widest of its block/float children's border-box widths, else its inline
max-content width.  Bounded by DEPTH and CONTENT-W so it stays cheap/resilient."
  (let ((cs (st styles node)))
    (if (or (> depth 6) (not (eq (h:dnode-kind node) :element)) (null cs))
        0
        (let ((block-kids (remove-if-not (lambda (k) (or (block-level-p styles k) (float-p styles k)))
                                         (child-elements node))))
          (min content-w
               (if block-kids
                   (loop for k in block-kids
                         maximize (pref-border-width k styles content-w (1+ depth)))
                   (pref-inline-width node styles cs content-w)))))))

(defun pref-border-width (node styles content-w depth)
  "Preferred BORDER-box width (incl. margins) of NODE for shrink-to-fit sizing."
  (let* ((cs (st styles node))
         (w (and cs (css:cstyle-width cs))))
    (if (null cs) 0
        (+ (css:cstyle-margin-left cs) (css:cstyle-margin-right cs)
           (css:cstyle-border-left-width cs) (css:cstyle-border-right-width cs)
           (css:cstyle-padding-left cs) (css:cstyle-padding-right cs)
           (if (numberp w) w (pref-content-width node styles content-w depth))))))

(defun place-float (node styles cleft cright top content-w)
  "Position a floated NODE at the left/right edge within [CLEFT,CRIGHT], dropping
below existing floats if it does not fit.  Records it in *FLOATS*; returns its lbox."
  (let* ((cs (st styles node))
         (side (if (string= (css:cstyle-float cs) "left") :left :right))
         (ml (css:cstyle-margin-left cs)) (mr (css:cstyle-margin-right cs))
         (extra (+ (css:cstyle-padding-left cs) (css:cstyle-padding-right cs)
                   (css:cstyle-border-left-width cs) (css:cstyle-border-right-width cs)))
         ;; AVAIL-W is the available content width handed to LAYOUT-NODE; for an
         ;; auto-width float the box fills (avail - margins), so to shrink-wrap
         ;; we size it to the float's preferred content width + its own
         ;; padding/border/margins (CSS 10.3.5 shrink-to-fit), capped at content.
         (avail-w (let ((w (css:cstyle-width cs)))
                    (if (numberp w) (+ w ml mr extra)
                        (min content-w
                             (max 0 (+ (pref-content-width node styles content-w) extra ml mr))))))
         (y top))
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
            (push (list side fx (+ fx avail-w) y (+ y (lbox-h lb))) *floats*))
          lb)))))

(defun find-lbox-for-node (lb node)
  "Locate the block lbox whose source NODE is NODE, NIL if none."
  (when (and lb node)
    (if (eq (lbox-node lb) node) lb
        (when (eq (lbox-kind lb) :block)
          (some (lambda (c) (find-lbox-for-node c node)) (lbox-children lb))))))

(defun layout-tree (document styles width &optional viewport-height scroll-to)
  (let ((*floats* nil) (*abs-pending* nil) (*fixed-pending* nil)
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
               (scroll-y (if anchor
                             (max 0 (min (round (lbox-y anchor)) (max 0 (- ph vph))))
                             0))
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
             (px0 (round (+ (lbox-x lb) (css:cstyle-border-left-width cs))))
             (py0 (round (+ (lbox-y lb) (css:cstyle-border-top-width cs))))
             (px1 (round (- (+ (lbox-x lb) (lbox-w lb)) (css:cstyle-border-right-width cs))))
             (py1 (round (- (+ (lbox-y lb) (lbox-h lb)) (css:cstyle-border-bottom-width cs))))
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

(defun border-edge-color (cs edge)
  "RGB list for EDGE (:t :r :b :l): the per-edge color, falling back to BORDER-COLOR."
  (rgb (or (case edge
             (:t (css:cstyle-border-top-color cs)) (:r (css:cstyle-border-right-color cs))
             (:b (css:cstyle-border-bottom-color cs)) (:l (css:cstyle-border-left-color cs)))
           (css:cstyle-border-color cs))))

(defun paint-borders (cv lb cs)
  "Paint the four border edges, each with its own color (overlapping rectangles)."
  (let* ((bt (css:cstyle-border-top-width cs)) (br (css:cstyle-border-right-width cs))
         (bb (css:cstyle-border-bottom-width cs)) (bl (css:cstyle-border-left-width cs))
         (x0 (lbox-x lb)) (y0 (lbox-y lb)) (w (lbox-w lb)) (h (lbox-h lb))
         (x1 (+ x0 w)) (y1 (+ y0 h)))
    (when (plusp bt) (fill-rect cv x0 y0 w bt (border-edge-color cs :t)))
    (when (plusp bb) (fill-rect cv x0 (- y1 bb) w bb (border-edge-color cs :b)))
    (when (plusp bl) (fill-rect cv x0 y0 bl h (border-edge-color cs :l)))
    (when (plusp br) (fill-rect cv (- x1 br) y0 br h (border-edge-color cs :r)))))

(defun paint-box (cv lb)
  (handler-case (%paint-box cv lb) (error () nil)))
(defun %paint-box (cv lb)
  (when lb
    (case (lbox-kind lb)
      (:block
       (let ((cs (lbox-style lb)))
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
         (paint-borders cv lb cs)
         (when (and (lbox-marker lb) (plusp (length (marker-glyph (lbox-marker lb)))))
           (draw-text cv (marker-glyph (lbox-marker lb))
                      (round (- (+ (lbox-x lb) (css:cstyle-padding-left cs)) (* 2 *font-w*)))
                      (round (+ (lbox-y lb) (css:cstyle-padding-top cs))) (rgb (css:cstyle-color cs))))
         ;; overflow:hidden/clip/scroll clips descendants to this box's padding box.
         (if (member (css:cstyle-overflow cs) '("hidden" "clip" "scroll") :test #'string=)
             (let ((*clip* (clip-intersect
                            (round (+ (lbox-x lb) (css:cstyle-border-left-width cs)))
                            (round (+ (lbox-y lb) (css:cstyle-border-top-width cs)))
                            (round (- (+ (lbox-x lb) (lbox-w lb)) (css:cstyle-border-right-width cs)))
                            (round (- (+ (lbox-y lb) (lbox-h lb)) (css:cstyle-border-bottom-width cs))))))
               (paint-children cv (lbox-children lb)))
             (paint-children cv (lbox-children lb)))))
      (:line
       (dolist (it (lbox-children lb))
         (if (frag-p it)
             (let ((cs (frag-style it)))
               ;; pass the line box geometry so scribe centers the real font
               ;; em-box (ascent+descent at font-size) within it.
               (draw-text-scribe cv (frag-text it) (round (frag-x it))
                          (lbox-y lb) (lbox-h lb)
                          (rgb (css:cstyle-color cs))
                          (css:cstyle-font-size cs)
                          :bold (>= (css:cstyle-font-weight cs) 600)
                          :underline (member "underline" (css:cstyle-text-decoration cs) :test #'string=)))
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
                                              (viewport-height 600) scroll-to)
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
  (let* ((doc (h:parse-html html))
         (sheet (css:parse-stylesheet (concatenate 'string (or css "") (string #\Newline)
                                                   (collect-stylesheets doc))))
         (styles (css:compute-styles doc sheet))
         (viewport-p (and viewport-height (root-clips-p doc styles)))
         (vph (and viewport-p (round viewport-height))))
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
