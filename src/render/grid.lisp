;;;; src/render/grid.lisp — CSS Grid layout (a practical subset of CSS Grid L1).
;;;;
;;;; Supported: explicit column/row templates (px, %, fr, auto, min/max-content,
;;;; minmax(), repeat()), row/column gaps, item placement by `span N` / line
;;;; numbers, row-major auto-placement (grid-auto-flow:row), grid-auto-rows, and
;;;; the box-alignment set (justify/align items + self, start/center/end/stretch).
;;;; Deferred: template areas, named lines, subgrid, dense packing, auto-flow column.
;;;;
;;;; The flow mirrors CSS Grid §7-11: build the explicit track lists, place items
;;;; into the implicit grid (§8), resolve column then row track sizes (§11), then
;;;; position each item in its cell honoring self-alignment.
(in-package #:weft.render)

;;; CSS Grid 2 §subgrid: when a nested grid declares grid-template-columns:subgrid,
;;; it adopts its parent's column tracks over the area it spans instead of defining
;;; its own.  The parent binds this to (WIDTHS . GAP) — the resolved px widths of the
;;; spanned columns and the parent's column gap — around laying the subgrid item out;
;;; the subgrid's own LAYOUT-GRID consumes it as fixed tracks.  NIL = ordinary grid.
(defvar *subgrid-cols* nil)

(defun subgrid-axis-p (str)
  "True when a grid-template-columns/rows value is the `subgrid` keyword."
  (and str (string-equal (string-trim '(#\Space #\Tab #\Newline #\Return) str) "subgrid")))

;;; ---- track-list parsing -------------------------------------------------
;;; A track spec is one of:
;;;   (:fixed px) (:percent n) (:fr n) (:auto) (:min-content) (:max-content)
;;;   (:minmax <min-spec> <max-spec>)

(defun grid-split-top-level (str)
  "Split STR on whitespace, but keep parenthesised groups (repeat()/minmax())
whole so their inner commas/spaces survive."
  (let ((toks '()) (buf (make-string-output-stream)) (depth 0))
    (flet ((flush () (let ((s (get-output-stream-string buf)))
                       (when (plusp (length s)) (push s toks)))))
      (loop for ch across str do
        (cond ((member ch '(#\( #\[)) (incf depth) (write-char ch buf))
              ((member ch '(#\) #\])) (when (plusp depth) (decf depth)) (write-char ch buf))
              ((and (zerop depth) (member ch '(#\Space #\Tab #\Newline #\Return))) (flush))
              (t (write-char ch buf))))
      (flush))
    (nreverse toks)))

(defun grid-line-name-map (str fs)
  "Map each [line-name] in a grid-template-* track list STR to its 1-based grid line
number (CSS Grid §7.1).  Line 1 is before the first track; a bracket token attaches
its names to the current line, a track token advances the line by the number of
tracks it produces.  Returns a hash-table name(string)->line(integer)."
  (let ((map (make-hash-table :test 'equal)) (line 1))
    (when (and str (plusp (length str)))
      (dolist (tok (grid-split-top-level str))
        (if (and (plusp (length tok)) (char= (char tok 0) #\[))
            (dolist (name (grid-split-top-level
                           (string-trim "[]" tok)))
              (unless (gethash name map) (setf (gethash name map) line)))
            (incf line (max 1 (length (grid-parse-track-list tok fs)))))))
    map))

(defun grid-parse-single (tok fs)
  "Parse one track keyword/length TOK into a track spec."
  (let* ((tok (string-trim '(#\Space #\Tab #\Newline #\Return) tok))
         (n (length tok)))
    (cond
      ((string= tok "auto") '(:auto))
      ((string= tok "min-content") '(:min-content))
      ((string= tok "max-content") '(:max-content))
      ((and (> n 2) (string= (subseq tok (- n 2)) "fr"))
       (list :fr (let ((v (ignore-errors (read-from-string (subseq tok 0 (- n 2))))))
                   (if (realp v) (float v) 1.0))))
      ((and (> n 0) (char= (char tok (1- n)) #\%))
       (list :percent (let ((v (ignore-errors (read-from-string (subseq tok 0 (1- n))))))
                        (if (realp v) v 0))))
      (t (let ((px (css::resolve-len tok fs))) (if (numberp px) (list :fixed px) '(:auto)))))))

(defun grid-auto-repeat-count (sub avail gap)
  "Repetition count for repeat(auto-fill|auto-fit, SUB) that fits in AVAIL px with
GAP between tracks (CSS Grid §7.2.3).  SUB must be fixed/percent tracks; falls back
to 1 when the axis size is indefinite or a sub track is not fixed-size."
  (let ((sizes (mapcar (lambda (s) (case (car s)
                                     (:fixed (float (second s)))
                                     (:percent (and (numberp avail) (* avail (/ (second s) 100.0))))
                                     (:minmax (case (car (second s))   ; auto-fill uses the min sizing fn
                                                (:fixed (float (second (second s))))
                                                (:percent (and (numberp avail) (* avail (/ (second (second s)) 100.0))))
                                                (t nil)))
                                     (t nil)))
                       sub)))
    (if (and (numberp avail) sub (every #'identity sizes))
        (let* ((rep-size (+ (reduce #'+ sizes) (* gap (max 0 (1- (length sub))))))
               (per (+ rep-size gap)))
          (if (plusp per) (max 1 (floor (+ avail gap) per)) 1))
        1)))

(defun grid-parse-track-list (str fs &optional avail (gap 0.0))
  "Expand a grid-template-* string into a flat list of track specs.  AVAIL (the
axis's definite content size) and GAP resolve repeat(auto-fill|auto-fit)."
  (when (and str (plusp (length str)))
    (let ((out '()))
      (dolist (tok (grid-split-top-level str))
        (let ((low (string-downcase tok)))
          (cond
            ;; [line-name] tokens carry no track — collected separately (grid-line-name-map).
            ((and (plusp (length tok)) (char= (char tok 0) #\[)) nil)
            ((and (>= (length low) 7) (string= (subseq low 0 7) "repeat("))
             ;; repeat(N, <track-list>) — count then the sub-list, expanded N times.
             ;; N may be auto-fill / auto-fit (resolved from AVAIL/GAP).
             (let* ((inner (subseq tok 7 (max 7 (1- (length tok)))))
                    (comma (position #\, inner))
                    (count-str (and comma (string-downcase (string-trim '(#\Space) (subseq inner 0 comma)))))
                    (sub (and comma (grid-parse-track-list (subseq inner (1+ comma)) fs)))
                    (count (cond ((and count-str (member count-str '("auto-fill" "auto-fit") :test #'string=))
                                  (grid-auto-repeat-count sub avail gap))
                                 (t (max 1 (or (and comma (ignore-errors (parse-integer count-str :junk-allowed t))) 1))))))
               (dotimes (i count) (dolist (s sub) (push s out)))))
            ((and (>= (length low) 12) (string= (subseq low 0 12) "fit-content("))
             ;; fit-content(L): a content track clamped at L (CSS Grid §7.2.2).
             (let* ((inner (subseq tok 12 (max 12 (1- (length tok)))))
                    (spec (grid-parse-single inner fs)))
               (push (list :fit-content (case (car spec)
                                          (:fixed (float (second spec)))
                                          (:percent (list :percent (second spec)))
                                          (t 0.0)))
                     out)))
            ((and (>= (length low) 7) (string= (subseq low 0 7) "minmax("))
             (let* ((inner (subseq tok 7 (max 7 (1- (length tok)))))
                    (comma (position #\, inner)))
               (if comma
                   (push (list :minmax (grid-parse-single (subseq inner 0 comma) fs)
                               (grid-parse-single (subseq inner (1+ comma)) fs))
                         out)
                   (push (grid-parse-single inner fs) out))))
            (t (push (grid-parse-single tok fs) out)))))
      (nreverse out))))

;;; ---- item measurement ---------------------------------------------------

(defun grid-item-max-width (item styles content-w)
  "Max-content outer (border+padding+margin) width of a grid ITEM.  An auto,
content-box width floors its content contribution at the item's specified min-width
(CSS Grid §11.5 / Sizing 3): the item's used width never drops below min-width, so
the track it max-content-sizes must be wide enough to hold it."
  (let ((base (pref-border-width item styles content-w 0))
        (cs (st styles item)))
    (if (and cs (numberp (css:cstyle-min-width cs)) (plusp (css:cstyle-min-width cs))
             (not (numberp (css:cstyle-width cs)))
             (not (equal (css:cstyle-box-sizing cs) "border-box")))
        (let ((content (pref-content-width item styles content-w 0))
              (minw (css:cstyle-min-width cs)))
          (max base (+ base (max 0.0 (- minw content)))))
        base)))

(defun grid-item-min-width (item styles content-w)
  "Min-content outer width of a grid ITEM."
  (let ((cs (st styles item)))
    (if (null cs) 0
        (+ (css:cstyle-margin-left cs) (css:cstyle-margin-right cs)
           (used-border cs :l) (used-border cs :r)
           (css::resolve-pad (css:cstyle-padding-left cs) nil) (css::resolve-pad (css:cstyle-padding-right cs) nil)
           (min-content-width item styles content-w)))))

(defun grid-track-content-w (spec items styles content-w)
  "Content-based size (px) of a content track: the max intrinsic width over its
single-column ITEMS.  min-content uses min-content widths; auto/max-content use
max-content widths (CSS Grid §11.5)."
  (let ((min-p (eq (car spec) :min-content)))
    (reduce #'max
            (mapcar (lambda (it) (if min-p (grid-item-min-width it styles content-w)
                                     (grid-item-max-width it styles content-w)))
                    items)
            :initial-value 0.0)))

;;; ---- track sizing -------------------------------------------------------

(defun grid-fixed-size (spec content-w items styles)
  "Resolve a non-flexible track SPEC to a px base size."
  (case (car spec)
    (:fixed (float (second spec)))
    (:percent (* content-w (/ (second spec) 100.0)))
    ((:auto :min-content :max-content) (float (grid-track-content-w spec items styles content-w)))
    ;; fit-content(L): min(max-content, max(min-content, L)) — a content-sized track
    ;; capped at L, never stretched (CSS Grid §7.2.2).
    (:fit-content
     (let* ((lim (let ((l (second spec)))
                   (if (and (consp l) (eq (car l) :percent)) (* content-w (/ (second l) 100.0)) (float l))))
            (mx (grid-track-content-w '(:max-content) items styles content-w))
            (mn (grid-track-content-w '(:min-content) items styles content-w)))
       (min mx (max mn lim))))
    (t 0.0)))

(defun grid-auto-max-p (spec)
  "True when track SPEC has an `auto` MAX sizing function (CSS Grid §11.8) — `auto`
itself, or minmax(_, auto).  Such tracks absorb leftover free space when the grid's
content-distribution is normal/stretch (max-content/min-content maxima do not)."
  (case (car spec)
    (:auto t)
    (:minmax (eq (car (third spec)) :auto))
    (t nil)))

(defun grid-stretch-dist-p (dist)
  "True when a content-distribution value DIST (justify-content/align-content) leaves
auto tracks free to stretch (CSS Grid §11.8): the `normal`/`stretch` default, plus
weft's flex-shared `flex-start` sentinel (grids spell explicit start as `start`)."
  (or (null dist)
      (member dist '("normal" "stretch" "flex-start") :test #'string=)))

(defun grid-size-columns (specs content-w cgap items-by-col styles &optional (dist "flex-start"))
  "Resolve column track SPECS to an array of px widths.  Fixed/percent tracks take
their size, content tracks take their items' intrinsic width, then leftover space
is shared among fr tracks in proportion to their flex factor (CSS Grid §11)."
  (let* ((n (length specs))
         (total-gap (* cgap (max 0 (1- n))))
         (avail (max 0.0 (- content-w total-gap)))
         (base (make-array n :initial-element 0.0))
         (flex (make-array n :initial-element nil)))
    (loop for i from 0
          for spec in specs
          for items = (if (< i (length items-by-col)) (aref items-by-col i) nil) do
      (case (car spec)
        (:fr (setf (aref flex i) (float (second spec))))
        (:minmax
         (destructuring-bind (mn mx) (cdr spec)
           (let ((mnsz (grid-fixed-size mn content-w items styles)))
             (if (eq (car mx) :fr)
                 (setf (aref base i) mnsz (aref flex i) (float (second mx)))
                 (let ((mxsz (grid-fixed-size mx content-w items styles)))
                   (setf (aref base i) (max mnsz (min mxsz (grid-track-content-w '(:max-content) items styles content-w)))))))))
        (t (setf (aref base i) (grid-fixed-size spec content-w items styles)))))
    ;; distribute free space over fr tracks (CSS Grid §11.7.1 "find the size of an
    ;; fr"): a flex track's used size is max(base, fr*factor).  The fr unit is found
    ;; over the leftover space with any track whose base exceeds fr*factor frozen at
    ;; its base and removed from the fr pool, then recomputed — so minmax(50px,1fr)
    ;; alongside 1fr in 200px sizes 100/100, not 125/75 (fr added atop the base).
    (let* ((sum-fr (loop for i below n when (aref flex i) sum (aref flex i)))
           (frozen (make-array n :initial-element nil)))
      (when (plusp sum-fr)
        (loop
          (let* ((nonflex (loop for i below n
                                when (or (not (aref flex i)) (aref frozen i)) sum (aref base i)))
                 (leftover (max 0.0 (- avail nonflex)))
                 (active (loop for i below n when (and (aref flex i) (not (aref frozen i)))
                               sum (aref flex i))))
            (if (<= active 0.0)
                (return)
                (let ((hyp (/ leftover active)) (froze nil))
                  (loop for i below n
                        when (and (aref flex i) (not (aref frozen i))
                                  (> (aref base i) (* hyp (aref flex i))))
                        do (setf (aref frozen i) t froze t))
                  (unless froze
                    (loop for i below n when (and (aref flex i) (not (aref frozen i)))
                          do (setf (aref base i) (* hyp (aref flex i))))
                    (return)))))))
      ;; Stretch auto tracks (CSS Grid §11.8): with no flexible (fr) track to
      ;; absorb it and a normal/stretch content-distribution, share the leftover
      ;; free space equally among tracks whose MAX is `auto`, so a single implicit
      ;; auto column grows to the grid's width (empty items then fill it) rather
      ;; than collapsing to their 0 content size.
      (when (and (not (plusp sum-fr)) (grid-stretch-dist-p dist))
        (let* ((used (loop for i below n sum (aref base i)))
               (free2 (max 0.0 (- avail used)))
               (autos (loop for i below n for spec in specs
                            when (grid-auto-max-p spec) collect i)))
          (when (and (plusp free2) autos)
            (let ((unit (/ free2 (length autos))))
              (dolist (i autos) (incf (aref base i) unit)))))))
    base))

;;; ---- placement ----------------------------------------------------------

(defun grid-placement (str &optional ntracks names)
  "Parse a grid-column/grid-row value into (values line-start span): LINE-START is
a 1-based grid line or NIL (auto placement), SPAN is a positive track count.  When
NTRACKS (the axis's track count) is given, a negative line number counts back from
the end — line -1 is the last line NTRACKS+1 (CSS Grid §8.3), so `1 / -1` spans the
whole axis (Tailwind's col-span-full) and `5 / -1` spans column 5 to the end.
NAMES (a hash name->line from grid-line-name-map) resolves named line references."
  (if (or (null str) (string= str "") (string= str "auto"))
      (values nil 1)
      (let* ((slash (position #\/ str))
             (left (string-trim '(#\Space) (if slash (subseq str 0 slash) str)))
             (right (and slash (string-trim '(#\Space) (subseq str (1+ slash))))))
        (labels ((num (s) (and s (or (ignore-errors (parse-integer s :junk-allowed t))
                                     (and names (gethash (string-trim '(#\Space) s) names)))))
                 (line (s) (let ((v (num s))) (if (and v ntracks (< v 0)) (+ ntracks 2 v) v)))
                 (spanp (s) (and s (search "span" s)))
                 (spanval (s) (let ((v (num (subseq s (+ 4 (search "span" s)))))) (if (and v (> v 0)) v 1))))
          (cond
            ((and right (not (spanp left)) (not (spanp right)) (num left) (num right))
             (let ((a (line left)) (b (line right))) (values (min a b) (max 1 (abs (- b a))))))
            ((and right (num left) (spanp right)) (values (line left) (spanval right)))
            ((and right (spanp left) (num right))
             (let ((b (line right)) (sp (spanval left))) (values (max 1 (- b sp)) sp)))
            ((spanp left) (values nil (spanval left)))
            ((num left) (values (line left) 1))
            (t (values nil 1)))))))

(defun grid-place-items (items styles ncols &optional areas col-flow (nrows 1) col-names row-names)
  "Assign each item a cell region.  Returns a list of (item row col rspan cspan),
0-based.  Auto-placement is row-major (grid-auto-flow:row, CSS Grid §8) by default;
when COL-FLOW is true it is column-major (grid-auto-flow:column), filling down each
column through NROWS rows before advancing to the next (implicit) column.  AREAS,
when given, is the container's grid-template-areas map NAME->(r0 c0 rspan cspan)
used to place items that name an area with `grid-area`."
  (let ((occ (make-hash-table :test 'equal))
        (out '()) (cur-r 0) (cur-c 0))
    (labels ((freep (r c cs rs)
               (loop for rr from r below (+ r rs) always
                 (loop for cc from c below (+ c cs) always (not (gethash (cons rr cc) occ)))))
             (mark (r c cs rs)
               (loop for rr from r below (+ r rs) do
                 (loop for cc from c below (+ c cs) do (setf (gethash (cons rr cc) occ) t)))))
      (dolist (it items)
        (let* ((cs (st styles it))
               ;; a named grid-area resolves to explicit line placement
               (area (and cs areas (css:cstyle-grid-area cs) (gethash (css:cstyle-grid-area cs) areas))))
          (multiple-value-bind (cline cspan)
              (if area (values (1+ (second area)) (fourth area))
                  (grid-placement (and cs (css:cstyle-grid-column cs)) ncols col-names))
            (multiple-value-bind (rline rspan)
                (if area (values (1+ (first area)) (third area))
                    (grid-placement (and cs (css:cstyle-grid-row cs)) nil row-names))
              (setf cspan (max 1 (min cspan ncols)))
              (let* ((col-def (and cline (>= cline 1)))
                     (row-def (and rline (>= rline 1)))
                     (c0 (and col-def (max 0 (min (1- cline) (- ncols cspan)))))
                     (r0 (and row-def (max 0 (1- rline)))))
                (multiple-value-bind (r c)
                    (cond
                      ((and row-def col-def) (values r0 c0))
                      (col-def (loop for r from 0 when (freep r c0 cspan rspan) return (values r c0)))
                      ;; NB: keep the column search's result as a SINGLE value — an
                      ;; (or (loop ... return (values r0 c)) ...) would drop c (OR only
                      ;; forwards multiple values from its last form), mis-placing any
                      ;; item with an explicit grid-row but auto column.
                      (row-def (let ((fc (loop for c from 0 to (- ncols cspan)
                                               when (freep r0 c cspan rspan) return c)))
                                 (values r0 (or fc 0))))
                      ;; grid-auto-flow:column — fill down each column (through NROWS)
                      ;; then advance to the next, growing implicit columns.
                      ((and col-flow (not row-def) (not col-def))
                       (loop named scan for c from cur-c do
                         (loop for r from (if (= c cur-c) cur-r 0) to (- nrows rspan) do
                           (when (freep r c cspan rspan) (return-from scan (values r c))))))
                      (t (loop named scan for r from cur-r do
                           (loop for c from (if (= r cur-r) cur-c 0) to (- ncols cspan) do
                             (when (freep r c cspan rspan) (return-from scan (values r c)))))))
                  (mark r c cspan rspan)
                  (push (list it r c rspan cspan) out)
                  (unless (or row-def col-def)
                    (if col-flow (setf cur-c c cur-r (+ r rspan))
                        (setf cur-r r cur-c (+ c cspan))))))))))
      (nreverse out))))

;;; ---- self-alignment -----------------------------------------------------

(defun grid-norm-align (kw)
  "Fold a box-alignment keyword to start | center | end | stretch."
  (cond ((member kw '("start" "flex-start" "self-start") :test #'string=) "start")
        ((member kw '("end" "flex-end" "self-end") :test #'string=) "end")
        ((string= kw "center") "center")
        (t "stretch")))

(defun grid-justify (cs container-cs)
  "Resolved inline-axis self-alignment for an item (justify-self, else the
container's justify-items)."
  (let ((j (and cs (css:cstyle-justify-self cs))))
    (grid-norm-align (if (or (null j) (string= j "auto")) (css:cstyle-justify-items container-cs) j))))

(defun grid-align (cs container-cs)
  "Resolved block-axis self-alignment for an item (align-self, else the
container's align-items)."
  (let ((a (and cs (css:cstyle-align-self cs))))
    (grid-norm-align (if (or (null a) (string= a "auto")) (css:cstyle-align-items container-cs) a))))

(defun grid-content-offsets (n bases gap free dist origin)
  "Vector of N track start positions measured from ORIGIN, applying the grid
content-distribution DIST (justify-content / align-content) over FREE leftover px
(CSS Align 3 §content-distribution).  normal / stretch / start / flex-start / left
and unknown values pack tightly from ORIGIN (weft does not grow tracks for stretch)."
  (let ((offs (make-array n :initial-element (float origin)))
        (free (max 0.0 free)))
    (multiple-value-bind (lead extra)
        (cond ((zerop free) (values 0.0 0.0))
              ((string= dist "center") (values (/ free 2) 0.0))
              ((member dist '("end" "flex-end" "right") :test #'string=) (values free 0.0))
              ((string= dist "space-between") (if (> n 1) (values 0.0 (/ free (1- n))) (values 0.0 0.0)))
              ((string= dist "space-around") (if (> n 0) (values (/ free (* 2 n)) (/ free n)) (values 0.0 0.0)))
              ((string= dist "space-evenly") (values (/ free (1+ n)) (/ free (1+ n))))
              (t (values 0.0 0.0)))
      (loop with pos = (+ origin lead)
            for i from 0 below n do
        (setf (aref offs i) pos)
        (incf pos (+ (aref bases i) gap extra))))
    offs))

;;; ---- the container layout ----------------------------------------------

(defun layout-grid (node styles cx cy content-w base-cs &optional avail-h)
  "Lay out a CSS Grid container.  Same contract as LAYOUT-FLEX: returns
(values child-lboxes content-height); CX/CY are the content-box origin, CONTENT-W
its width, AVAIL-H its definite content height (px) when known else NIL."
  (let ((%subgrid-cols *subgrid-cols*)   ; tracks this grid adopts from a subgrid parent
        (*subgrid-cols* nil))            ; isolate: descendant/measured grids don't inherit
  (let* ((fs (css:cstyle-font-size base-cs))
         ;; percentage gaps resolve against the container content size in that axis
         ;; (CSS Box Alignment §8.3); an indefinite row basis yields 0.
         (cgap (css::resolve-gap (css:cstyle-column-gap base-cs) content-w))
         (rgap (css::resolve-gap (css:cstyle-row-gap base-cs) (and (numberp avail-h) avail-h)))
         (auto-row (first (grid-parse-track-list (css:cstyle-grid-auto-rows base-cs) fs)))
         (auto-col (first (grid-parse-track-list (css:cstyle-grid-auto-columns base-cs) fs)))
         ;; With NO explicit grid-template-columns every column is implicit and is
         ;; sized by grid-auto-columns (CSS Grid §7.5); fall back to that single track
         ;; rather than a bare (:auto) so grid-auto-flow:column honours auto-columns.
         ;; grid-template-columns:subgrid — adopt the parent grid's spanned column
         ;; tracks (fixed px) and its column gap (CSS Grid 2 §subgrid).
         (subgrid-cols-p (and %subgrid-cols (subgrid-axis-p (css:cstyle-grid-template-columns base-cs))))
         (cgap (if subgrid-cols-p (cdr %subgrid-cols) cgap))
         (col-specs (cond (subgrid-cols-p (mapcar (lambda (w) (list :fixed (float w))) (car %subgrid-cols)))
                          ((grid-parse-track-list (css:cstyle-grid-template-columns base-cs) fs content-w cgap))
                          (t (list (or auto-col '(:auto))))))
         (row-specs (grid-parse-track-list (css:cstyle-grid-template-rows base-cs) fs
                                           (and (numberp avail-h) avail-h) rgap))
         (col-flow (let ((f (css:cstyle-grid-auto-flow base-cs))) (and f (string= f "column"))))
         (col-names (grid-line-name-map (css:cstyle-grid-template-columns base-cs) fs))
         (row-names (grid-line-name-map (css:cstyle-grid-template-rows base-cs) fs))
         (nrows-tpl (max 1 (length row-specs)))
         (items (remove-if-not
                 (lambda (k) (let ((c (st styles k))) (and c (not (string= (css:cstyle-display c) "none")))))
                 (child-elements node))))
    (when (null items) (return-from layout-grid (values nil 0)))
    ;; Implicit columns (CSS Grid §7.5): grow the column count to cover any item
    ;; explicitly placed past the template's last column line; the extra tracks are
    ;; sized by grid-auto-columns (default auto).
    (let ((explicit-ncols (length col-specs)))
      (let ((need explicit-ncols) (n-auto 0))
        (dolist (it items)
          (let ((cs (st styles it)))
            (when cs
              (multiple-value-bind (cline cspan) (grid-placement (css:cstyle-grid-column cs) nil col-names)
                (if (and cline (>= cline 1))
                    (setf need (max need (+ (1- cline) (max 1 cspan))))
                    (incf n-auto))))))
        ;; grid-auto-flow:column packs auto items down NROWS-TPL rows then into the
        ;; next (implicit) column, so the column count grows to hold them (CSS Grid §8).
        (when col-flow
          (setf need (max need (ceiling n-auto nrows-tpl))))
        (when (> need explicit-ncols)
          (setf col-specs (append col-specs
                                   (make-list (- need explicit-ncols)
                                              :initial-element (or auto-col '(:auto))))))))
    (let ((ncols (length col-specs)))
    ;; Resolve grid items' percentage margins against the grid's inline content size
    ;; (CSS 2.1 §8.3 / CSS Grid §item-margins) before track sizing and item layout,
    ;; so both intrinsic-width contributions and final placement see the used px.
    (dolist (it items)
      (let ((c (st styles it))) (when c (css::resolve-pct-margins c content-w))))
    (let* ((placed (grid-place-items items styles ncols (css:cstyle-grid-template-areas base-cs) col-flow nrows-tpl col-names row-names))
           ;; single-column items feed content-track sizing
           (items-by-col (make-array ncols :initial-element nil)))
      (dolist (p placed)
        (destructuring-bind (it r c rspan cspan) p
          (declare (ignore r rspan))
          (when (= cspan 1) (push it (aref items-by-col c)))))
      (let* ((colw (grid-size-columns col-specs content-w cgap items-by-col styles
                                      (css:cstyle-justify-content base-cs)))
             ;; column left offsets (content-box relative), applying justify-content
             ;; distribution over the leftover inline space (CSS Align 3).
             (col-used (+ (loop for i below ncols sum (aref colw i)) (* cgap (max 0 (1- ncols)))))
             (colx (grid-content-offsets ncols colw cgap (- content-w col-used)
                                         (css:cstyle-justify-content base-cs) 0.0)))
        ;; lay each item out at its final width (justify decides fill vs shrink).
        ;; Item margins offset the border box inside its grid area and shrink the
        ;; space its width fills (CSS Grid §11.8 / CSS 2.1 §8.3): alignment positions
        ;; the MARGIN box, so free space is measured against the outer margin extent.
        (let ((cells '()))   ; (lb nh r c rspan cspan align cellw mt mb)
          (dolist (p placed)
            (destructuring-bind (it r c rspan cspan) p
              (let* ((cs (st styles it))
                     (ml (if (css:cstyle-margin-left-auto cs) 0 (css:cstyle-margin-left cs)))
                     (mr (if (css:cstyle-margin-right-auto cs) 0 (css:cstyle-margin-right cs)))
                     (mt (if (css:cstyle-margin-top-auto cs) 0 (css:cstyle-margin-top cs)))
                     (mb (if (css:cstyle-margin-bottom-auto cs) 0 (css:cstyle-margin-bottom cs)))
                     (span-w (+ (loop for k from c below (min ncols (+ c cspan)) sum (aref colw k))
                                (* cgap (1- cspan))))
                     (just (grid-justify cs base-cs))
                     ;; margin box the item's alignment positions within the area;
                     ;; LAYOUT-NODE subtracts the item's own margins from AVAIL and
                     ;; offsets the border box by margin-left, so pass the outer
                     ;; margin-box width as AVAIL and the area origin (sans margin) as x.
                     (mbox-w (if (string= just "stretch") span-w
                                 (min span-w (grid-item-max-width it styles span-w))))
                     (hoff (cond ((string= just "center") (/ (- span-w mbox-w) 2))
                                 ((string= just "end") (- span-w mbox-w))
                                 (t 0)))
                     (x (+ cx (aref colx c) hoff))
                     ;; a subgrid child adopts THIS grid's spanned column tracks + gap
                     (*subgrid-cols* (if (subgrid-axis-p (css:cstyle-grid-template-columns cs))
                                         (cons (loop for k from c below (min ncols (+ c cspan))
                                                     collect (aref colw k))
                                               cgap)
                                         nil)))
                (multiple-value-bind (lb adv) (layout-node it styles (round x) (round cy) (round mbox-w))
                  (declare (ignore adv))
                  (push (list lb (if lb (lbox-h lb) 0) r c rspan cspan (grid-align cs base-cs) span-w mt mb)
                        cells)))))
          (setf cells (nreverse cells))
          ;; ---- row sizing ----
          (let* ((nrows (max 1 (loop for cell in cells maximize (+ (third cell) (fifth cell)))))
                 (rbase (make-array nrows :initial-element 0.0))
                 (rflex (make-array nrows :initial-element nil))
                 (rfixed (make-array nrows :initial-element nil)))
            (loop for r below nrows
                  for spec = (or (nth r row-specs) auto-row '(:auto)) do
              (case (car spec)
                (:fixed (setf (aref rbase r) (float (second spec)) (aref rfixed r) t))
                (:percent (setf (aref rbase r) (if (numberp avail-h) (* avail-h (/ (second spec) 100.0)) 0.0)
                                (aref rfixed r) t))
                (:fr (when (numberp avail-h) (setf (aref rflex r) (float (second spec)))))
                (t nil)))   ; auto/content — filled below
            ;; content pass: single-row items set their row's auto height (outer,
            ;; margin box included — CSS 2.1 §8.3)
            (dolist (cell cells)
              (destructuring-bind (lb nh r c rspan cspan align cellw mt mb) cell
                (declare (ignore lb c cspan align cellw))
                (when (and (= rspan 1) (not (aref rfixed r)))
                  (setf (aref rbase r) (max (aref rbase r) (+ nh mt mb))))))
            ;; multi-row items: grow the last non-fixed spanned row to fit
            (dolist (cell cells)
              (destructuring-bind (lb nh r c rspan cspan align cellw mt mb) cell
                (declare (ignore lb c cspan align cellw))
                (when (> rspan 1)
                  (let* ((span-h (+ (loop for rr from r below (+ r rspan) sum (aref rbase rr))
                                    (* rgap (1- rspan))))
                         (deficit (- (+ nh mt mb) span-h)))
                    (when (> deficit 0)
                      (let ((target (1- (+ r rspan))))
                        (loop for rr from (1- (+ r rspan)) downto r
                              unless (aref rfixed rr) do (setf target rr) (return))
                        (incf (aref rbase target) deficit)))))))
            ;; fr rows: share leftover definite height
            (when (and (numberp avail-h) (some #'identity (coerce rflex 'list)))
              (let* ((used (+ (loop for r below nrows sum (aref rbase r)) (* rgap (max 0 (1- nrows)))))
                     (free (max 0.0 (- avail-h used)))
                     (sum-fr (loop for r below nrows when (aref rflex r) sum (aref rflex r))))
                (when (plusp sum-fr)
                  (let ((unit (/ free sum-fr)))
                    (loop for r below nrows when (aref rflex r) do
                      (incf (aref rbase r) (* unit (aref rflex r))))))))
            ;; Stretch auto rows to a definite height (CSS Grid §11.8), mirroring the
            ;; column auto-track stretch: with no fr row and a normal/stretch
            ;; align-content, share the leftover block space equally among auto-max
            ;; rows so a single implicit/auto row grows to the grid's height instead
            ;; of collapsing to its (often zero) content height.
            (when (and (numberp avail-h)
                       (grid-stretch-dist-p (css:cstyle-align-content base-cs))
                       (notany #'identity (coerce rflex 'list)))
              (let* ((used (+ (loop for r below nrows sum (aref rbase r)) (* rgap (max 0 (1- nrows)))))
                     (free2 (max 0.0 (- avail-h used)))
                     (autos (loop for r below nrows
                                  for spec = (or (nth r row-specs) auto-row '(:auto))
                                  when (grid-auto-max-p spec) collect r)))
                (when (and (plusp free2) autos)
                  (let ((unit (/ free2 (length autos))))
                    (dolist (r autos) (incf (aref rbase r) unit))))))
            ;; row top offsets, applying align-content distribution over the
            ;; leftover block space when the grid has a definite height (CSS Align 3).
            (let* ((row-used (+ (loop for r below nrows sum (aref rbase r)) (* rgap (max 0 (1- nrows)))))
                   (row-free (if (numberp avail-h) (- avail-h row-used) 0.0))
                   (roff (grid-content-offsets nrows rbase rgap row-free
                                               (css:cstyle-align-content base-cs) cy)))
              ;; ---- position each item in its cell ----
              (let ((boxes '()))
                (dolist (cell cells)
                  (destructuring-bind (lb nh r c rspan cspan align cellw mt mb) cell
                    (declare (ignore c cspan cellw))
                    (when lb
                      (let* ((cellh (+ (loop for rr from r below (+ r rspan) sum (aref rbase rr))
                                       (* rgap (1- rspan))))
                             (celly (aref roff r))
                             (mbox-h (+ nh mt mb))
                             (voff (cond ((string= align "center") (/ (- cellh mbox-h) 2))
                                         ((string= align "end") (- cellh mbox-h))
                                         (t 0)))
                             (fy (+ celly voff mt)))
                        (shift-box lb 0 (round (- fy (lbox-y lb))))
                        (when (string= align "stretch") (setf (lbox-h lb) (max nh (round (- cellh mt mb)))))
                        (push lb boxes)))))
                (values (nreverse boxes)
                        (+ (loop for r below nrows sum (aref rbase r))
                           (* rgap (max 0 (1- nrows)))))))))))))))
