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
                               :children (list (make-frag :x 2 :w (* (length alt) *font-w*) :text alt :style cs)))))))
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

(defun word-w (word) (* (length word) *font-w*))
(defun space-w () *font-w*)

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
                   (ww (if atomic (lbox-w (cdr wd)) (word-w (car wd))))
                   (need (if cur (+ (space-w) ww) ww)))
              (when (and cur (not nowrap) (> (+ (- cx lx) need) avail)) (return))
              (when (> (- cx lx) 0) (incf cx (space-w)))
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
                  (push (make-frag :x lx :w (word-w (car wd)) :text (car wd) :style (cdr wd)) cur))
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
(defun layout-node (node styles x y avail-w)
  "Resilient wrapper: a failing subtree degrades to an empty box, not a crash."
  (handler-case (%layout-node node styles x y avail-w)
    (error (e) (if *layout-debug* (error e) (values nil 0)))))

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

(defun %layout-node (node styles x y avail-w)
  "Establish an absolute containing block for positioned elements, then lay the
node out.  A positioned element (relative/absolute/fixed) is the containing block
for its absolutely-positioned descendants; we collect those during subtree layout
and resolve them once this box's geometry is known (then a later unit-shift of
this box, if any, carries them along correctly)."
  (let ((cs (st styles node)))
    (if (and cs (member (css:cstyle-position cs) '("relative" "absolute" "fixed") :test #'string=))
        (let ((*abs-pending* nil))
          (multiple-value-bind (lb adv) (%layout-core node styles x y avail-w)
            (when (and lb *abs-pending*)
              (let ((cb (pad-box lb cs)))
                (dolist (p *abs-pending*) (resolve-positioned (car p) cb (cdr p)))))
            (values lb adv)))
        (%layout-core node styles x y avail-w))))

(defun %layout-core (node styles x y avail-w)
  "Lay out block-level NODE at (X,Y); AVAIL-W is the containing content width.
Returns (values lbox advance-height)."
  (let ((cs (st styles node)))
    (when (or (null cs) (string= (cdisplay cs) "none")) (return-from %layout-core (values nil 0)))
    (let* ((mt (css:cstyle-margin-top cs)) (mb (css:cstyle-margin-bottom cs))
           (ml (css:cstyle-margin-left cs)) (mr (css:cstyle-margin-right cs))
           (pt (css:cstyle-padding-top cs)) (pb (css:cstyle-padding-bottom cs))
           (pl (css:cstyle-padding-left cs)) (pr (css:cstyle-padding-right cs))
           (bt (css:cstyle-border-top-width cs)) (bb (css:cstyle-border-bottom-width cs))
           (bl (css:cstyle-border-left-width cs)) (br (css:cstyle-border-right-width cs))
           (border-box (string= (css:cstyle-box-sizing cs) "border-box"))
           (pad-bord (+ pl pr bl br))
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
           (children '()) (content-h 0))
      ;; <pre>/white-space:pre — preserve newlines, no wrapping
      (when (and (string= (css:cstyle-white-space cs) "pre") (not (has-block-children styles node)))
        (let* ((text (collect-raw node)) (yy cy)
               (lh (max *font-h* (round (* (css:cstyle-font-size cs) (css:cstyle-line-height cs))))))
          (dolist (ln (split-newlines text))
            (push (make-lbox :x cx :y yy :w content-w :h lh :kind :line
                             :children (when (plusp (length ln))
                                         (list (make-frag :x cx :w (word-w ln) :text ln :style cs))))
                  children)
            (incf yy lh) (incf content-h lh)))
        (let* ((box-h (+ content-h pt pb bt bb))
               (lb (make-lbox :x box-x :y box-y :w width :h box-h :style cs :node node
                              :kind :block :children (nreverse children))))
          (return-from %layout-core (values lb (+ mt box-h mb)))))
      ;; flex / table containers
      (when (member (cdisplay cs) '("flex" "table") :test #'string=)
        (multiple-value-bind (boxes ch)
            (if (string= (cdisplay cs) "flex")
                (layout-flex node styles cx cy content-w cs)
                (layout-table node styles cx cy content-w cs))
          (let* ((box-h (+ ch pt pb bt bb))
                 (lb (make-lbox :x box-x :y box-y :w width :h box-h :style cs :node node
                                :kind :block :children boxes)))
            (return-from %layout-core (values lb (+ mt box-h mb))))))
      ;; classify children: anonymous-group consecutive inline-level nodes.
      ;; ::before / ::after generated boxes bracket the real children.
      (let ((kids (multiple-value-bind (before after) (pseudo-kids node styles)
                    (append (when before (list before))
                            (coerce (h:dnode-children node) 'list)
                            (when after (list after)))))
            (group '()) (yy cy))
        (flet ((flush-inline ()
                 (when group
                   (let ((words (loop for g in (nreverse group) append (collect-words g styles cs content-w))))
                     (when words
                       (multiple-value-bind (lines lh-total) (layout-inline words cx yy content-w cs)
                         (dolist (l lines) (push l children))
                         (incf yy lh-total) (incf content-h lh-total))))
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
                 (multiple-value-bind (lb adv) (layout-node k styles cx yy content-w)
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
                 (when (and kcs (member (css:cstyle-clear kcs) '("left" "right" "both") :test #'string=))
                   (let ((ny (clear-y yy cx (+ cx content-w)
                                     (case (intern (string-upcase (css:cstyle-clear kcs)) :keyword)
                                       (:left '(:left)) (:right '(:right)) (t '(:left :right))))))
                     (when (> ny yy) (incf content-h (- ny yy)) (setf yy ny))))
                 (multiple-value-bind (lb adv) (layout-node k styles cx yy content-w)
                   (when lb
                     (when (and kcs (string= pos "relative"))                    ; visual shift, flow unchanged
                       (shift-box lb (round (cond ((numberp (css:cstyle-left kcs)) (css:cstyle-left kcs))
                                                  ((numberp (css:cstyle-right kcs)) (- (css:cstyle-right kcs))) (t 0)))
                                  (round (cond ((numberp (css:cstyle-top kcs)) (css:cstyle-top kcs))
                                               ((numberp (css:cstyle-bottom kcs)) (- (css:cstyle-bottom kcs))) (t 0)))))
                     (push lb children))
                   (incf yy adv) (incf content-h adv)))
                ((or (eq (h:dnode-kind k) :text) (inline-level-p styles k)) (push k group)))))
          (flush-inline)))
      (let* ((exp-h (css::resolve-size (css:cstyle-height cs) avail-w))   ; explicit height (px) or nil
             (content-final (cond ((numberp exp-h) (if border-box (- exp-h pad-bord) exp-h)) (t content-h)))
             (box-h0 (+ content-final pt pb bt bb))
             ;; min/max-height as box-height floor/ceiling
             (box-h (let ((bh box-h0) (mn (css:cstyle-min-height cs)) (mx (css:cstyle-max-height cs)))
                      (when (and (numberp mn) (> mn 0)) (setf bh (max bh (+ mn pt pb bt bb))))
                      (when (numberp mx) (setf bh (min bh (+ mx pt pb bt bb))))
                      (max bh (if list-item *font-h* 0))))
             (lb (make-lbox :x box-x :y box-y :w width :h box-h
                            :style cs :node node :kind :block :children (nreverse children)
                            :marker (when list-item (css:cstyle-list-style cs)))))
        (values lb (+ mt (lbox-h lb) mb))))))

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
  (let ((words (collect-words node styles (st styles node) 600)) (w 0))
    (dolist (wd words)
      (if (eq (car wd) :atomic)
          (incf w (+ (lbox-w (cdr wd)) (space-w)))
          (incf w (+ (word-w (car wd)) (space-w)))))
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
      (incf w (+ (if (eq (car wd) :atomic) (lbox-w (cdr wd)) (word-w (car wd))) (space-w))))
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
      (multiple-value-bind (root adv) (layout-node body styles 0 0 width)
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

(defun paint-children (cv children)
  "Paint CHILDREN in a simplified CSS stacking order (appendix E): negative
z-index positioned boxes, then in-flow content in tree order, then >=0
positioned boxes ordered by z-index.  Equal z-index keeps tree order (stable)."
  (let ((flow '()) (pos '()))
    (dolist (c children) (if (lbox-positioned-p c) (push c pos) (push c flow)))
    (setf flow (nreverse flow) pos (nreverse pos))
    (let ((neg    (stable-sort (remove-if-not (lambda (c) (minusp (lbox-z c))) (copy-list pos))
                               #'< :key #'lbox-z))
          (nonneg (stable-sort (remove-if     (lambda (c) (minusp (lbox-z c))) (copy-list pos))
                               #'< :key #'lbox-z)))
      (dolist (c neg)    (paint-box cv c))
      (dolist (c flow)   (paint-box cv c))
      (dolist (c nonneg) (paint-box cv c)))))

(defun marker-glyph (kind) (cond ((string= kind "circle") "o") ((string= kind "square") "#")
                                 ((string= kind "none") "") (t "•")))

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
         (when (lbox-img lb)
           (blit-img cv (lbox-img lb) (round (lbox-x lb)) (round (lbox-y lb))
                     (round (lbox-w lb)) (round (lbox-h lb))))
         (let ((bc (rgb (css:cstyle-border-color cs))))
           (when (plusp (css:cstyle-border-top-width cs)) (fill-rect cv (lbox-x lb) (lbox-y lb) (lbox-w lb) (css:cstyle-border-top-width cs) bc))
           (when (plusp (css:cstyle-border-bottom-width cs)) (fill-rect cv (lbox-x lb) (- (+ (lbox-y lb) (lbox-h lb)) (css:cstyle-border-bottom-width cs)) (lbox-w lb) (css:cstyle-border-bottom-width cs) bc))
           (when (plusp (css:cstyle-border-left-width cs)) (fill-rect cv (lbox-x lb) (lbox-y lb) (css:cstyle-border-left-width cs) (lbox-h lb) bc))
           (when (plusp (css:cstyle-border-right-width cs)) (fill-rect cv (- (+ (lbox-x lb) (lbox-w lb)) (css:cstyle-border-right-width cs)) (lbox-y lb) (css:cstyle-border-right-width cs) (lbox-h lb) bc)))
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
       (let ((yoff (max 0 (floor (- (lbox-h lb) *font-h*) 2))))
         (dolist (it (lbox-children lb))
           (if (frag-p it)
               (let ((cs (frag-style it)))
                 (draw-text cv (frag-text it) (round (frag-x it)) (round (+ (lbox-y lb) yoff))
                            (rgb (css:cstyle-color cs))
                            :bold (>= (css:cstyle-font-weight cs) 600)
                            :underline (member "underline" (css:cstyle-text-decoration cs) :test #'string=)))
               (paint-box cv it))))))))   ; atomic inline-block / img box

(defun collect-stylesheets (doc)
  (with-output-to-string (o)
    (labels ((rec (n)
               (when (eq (h:dnode-kind n) :element)
                 (when (string= (h:dnode-name n) "style")
                   (loop for c across (h:dnode-children n) when (eq (h:dnode-kind c) :text)
                         do (write-string (h:dnode-data c) o) (terpri o)))
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
