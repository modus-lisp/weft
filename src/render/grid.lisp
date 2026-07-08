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
        (cond ((char= ch #\() (incf depth) (write-char ch buf))
              ((char= ch #\)) (when (plusp depth) (decf depth)) (write-char ch buf))
              ((and (zerop depth) (member ch '(#\Space #\Tab #\Newline #\Return))) (flush))
              (t (write-char ch buf))))
      (flush))
    (nreverse toks)))

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

(defun grid-parse-track-list (str fs)
  "Expand a grid-template-* string into a flat list of track specs."
  (when (and str (plusp (length str)))
    (let ((out '()))
      (dolist (tok (grid-split-top-level str))
        (let ((low (string-downcase tok)))
          (cond
            ((and (>= (length low) 7) (string= (subseq low 0 7) "repeat("))
             ;; repeat(N, <track-list>) — count then the sub-list, expanded N times.
             (let* ((inner (subseq tok 7 (max 7 (1- (length tok)))))
                    (comma (position #\, inner))
                    (count (max 1 (or (and comma (ignore-errors (parse-integer (subseq inner 0 comma) :junk-allowed t))) 1)))
                    (sub (and comma (grid-parse-track-list (subseq inner (1+ comma)) fs))))
               (dotimes (i count) (dolist (s sub) (push s out)))))
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
  "Max-content outer (border+padding+margin) width of a grid ITEM."
  (pref-border-width item styles content-w 0))

(defun grid-item-min-width (item styles content-w)
  "Min-content outer width of a grid ITEM."
  (let ((cs (st styles item)))
    (if (null cs) 0
        (+ (css:cstyle-margin-left cs) (css:cstyle-margin-right cs)
           (used-border cs :l) (used-border cs :r)
           (css:cstyle-padding-left cs) (css:cstyle-padding-right cs)
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
    (t 0.0)))

(defun grid-size-columns (specs content-w cgap items-by-col styles)
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
    ;; distribute free space over fr tracks
    (let* ((fixed-sum (loop for i below n sum (aref base i)))
           (free (max 0.0 (- avail fixed-sum)))
           (sum-fr (loop for i below n when (aref flex i) sum (aref flex i))))
      (when (plusp sum-fr)
        (let ((unit (/ free sum-fr)))
          (loop for i below n when (aref flex i) do
            (incf (aref base i) (* unit (aref flex i)))))))
    base))

;;; ---- placement ----------------------------------------------------------

(defun grid-placement (str &optional ntracks)
  "Parse a grid-column/grid-row value into (values line-start span): LINE-START is
a 1-based grid line or NIL (auto placement), SPAN is a positive track count.  When
NTRACKS (the axis's track count) is given, a negative line number counts back from
the end — line -1 is the last line NTRACKS+1 (CSS Grid §8.3), so `1 / -1` spans the
whole axis (Tailwind's col-span-full) and `5 / -1` spans column 5 to the end."
  (if (or (null str) (string= str "") (string= str "auto"))
      (values nil 1)
      (let* ((slash (position #\/ str))
             (left (string-trim '(#\Space) (if slash (subseq str 0 slash) str)))
             (right (and slash (string-trim '(#\Space) (subseq str (1+ slash))))))
        (labels ((num (s) (and s (ignore-errors (parse-integer s :junk-allowed t))))
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

(defun grid-place-items (items styles ncols)
  "Assign each item a cell region.  Returns a list of (item row col rspan cspan),
0-based, using row-major auto-placement (CSS Grid §8, grid-auto-flow:row)."
  (let ((occ (make-hash-table :test 'equal))
        (out '()) (cur-r 0) (cur-c 0))
    (labels ((freep (r c cs rs)
               (loop for rr from r below (+ r rs) always
                 (loop for cc from c below (+ c cs) always (not (gethash (cons rr cc) occ)))))
             (mark (r c cs rs)
               (loop for rr from r below (+ r rs) do
                 (loop for cc from c below (+ c cs) do (setf (gethash (cons rr cc) occ) t)))))
      (dolist (it items)
        (let ((cs (st styles it)))
          (multiple-value-bind (cline cspan) (grid-placement (and cs (css:cstyle-grid-column cs)) ncols)
            (multiple-value-bind (rline rspan) (grid-placement (and cs (css:cstyle-grid-row cs)))
              (setf cspan (max 1 (min cspan ncols)))
              (let* ((col-def (and cline (>= cline 1)))
                     (row-def (and rline (>= rline 1)))
                     (c0 (and col-def (max 0 (min (1- cline) (- ncols cspan)))))
                     (r0 (and row-def (max 0 (1- rline)))))
                (multiple-value-bind (r c)
                    (cond
                      ((and row-def col-def) (values r0 c0))
                      (col-def (loop for r from 0 when (freep r c0 cspan rspan) return (values r c0)))
                      (row-def (or (loop for c from 0 to (- ncols cspan)
                                         when (freep r0 c cspan rspan) return (values r0 c))
                                   (values r0 0)))
                      (t (loop named scan for r from cur-r do
                           (loop for c from (if (= r cur-r) cur-c 0) to (- ncols cspan) do
                             (when (freep r c cspan rspan) (return-from scan (values r c)))))))
                  (mark r c cspan rspan)
                  (push (list it r c rspan cspan) out)
                  (unless (or row-def col-def) (setf cur-r r cur-c (+ c cspan)))))))))
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

;;; ---- the container layout ----------------------------------------------

(defun layout-grid (node styles cx cy content-w base-cs &optional avail-h)
  "Lay out a CSS Grid container.  Same contract as LAYOUT-FLEX: returns
(values child-lboxes content-height); CX/CY are the content-box origin, CONTENT-W
its width, AVAIL-H its definite content height (px) when known else NIL."
  (let* ((fs (css:cstyle-font-size base-cs))
         (cgap (css:cstyle-column-gap base-cs))
         (rgap (css:cstyle-row-gap base-cs))
         (col-specs (or (grid-parse-track-list (css:cstyle-grid-template-columns base-cs) fs)
                        '((:auto))))
         (row-specs (grid-parse-track-list (css:cstyle-grid-template-rows base-cs) fs))
         (auto-row (first (grid-parse-track-list (css:cstyle-grid-auto-rows base-cs) fs)))
         (ncols (length col-specs))
         (items (remove-if-not
                 (lambda (k) (let ((c (st styles k))) (and c (not (string= (css:cstyle-display c) "none")))))
                 (child-elements node))))
    (when (null items) (return-from layout-grid (values nil 0)))
    (let* ((placed (grid-place-items items styles ncols))
           ;; single-column items feed content-track sizing
           (items-by-col (make-array ncols :initial-element nil)))
      (dolist (p placed)
        (destructuring-bind (it r c rspan cspan) p
          (declare (ignore r rspan))
          (when (= cspan 1) (push it (aref items-by-col c)))))
      (let* ((colw (grid-size-columns col-specs content-w cgap items-by-col styles))
             ;; column left offsets (content-box relative)
             (colx (make-array ncols :initial-element 0.0)))
        (loop for i from 1 below ncols do
          (setf (aref colx i) (+ (aref colx (1- i)) (aref colw (1- i)) cgap)))
        ;; lay each item out at its final width (justify decides fill vs shrink)
        (let ((cells '()))   ; (lb nh r c rspan cspan align cellw x-offset)
          (dolist (p placed)
            (destructuring-bind (it r c rspan cspan) p
              (let* ((cs (st styles it))
                     (span-w (+ (loop for k from c below (min ncols (+ c cspan)) sum (aref colw k))
                                (* cgap (1- cspan))))
                     (just (grid-justify cs base-cs))
                     (itemw (if (string= just "stretch") span-w
                                (min span-w (grid-item-max-width it styles span-w))))
                     (hoff (cond ((string= just "center") (/ (- span-w itemw) 2))
                                 ((string= just "end") (- span-w itemw))
                                 (t 0)))
                     (x (+ cx (aref colx c) hoff)))
                (multiple-value-bind (lb adv) (layout-node it styles (round x) (round cy) (round itemw))
                  (declare (ignore adv))
                  (push (list lb (if lb (lbox-h lb) 0) r c rspan cspan (grid-align cs base-cs) span-w)
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
            ;; content pass: single-row items set their row's auto height
            (dolist (cell cells)
              (destructuring-bind (lb nh r c rspan cspan align cellw) cell
                (declare (ignore lb c cspan align cellw))
                (when (and (= rspan 1) (not (aref rfixed r)))
                  (setf (aref rbase r) (max (aref rbase r) nh)))))
            ;; multi-row items: grow the last non-fixed spanned row to fit
            (dolist (cell cells)
              (destructuring-bind (lb nh r c rspan cspan align cellw) cell
                (declare (ignore lb c cspan align cellw))
                (when (> rspan 1)
                  (let* ((span-h (+ (loop for rr from r below (+ r rspan) sum (aref rbase rr))
                                    (* rgap (1- rspan))))
                         (deficit (- nh span-h)))
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
            ;; row top offsets
            (let ((roff (make-array nrows :initial-element 0.0)))
              (setf (aref roff 0) cy)
              (loop for r from 1 below nrows do
                (setf (aref roff r) (+ (aref roff (1- r)) (aref rbase (1- r)) rgap)))
              ;; ---- position each item in its cell ----
              (let ((boxes '()))
                (dolist (cell cells)
                  (destructuring-bind (lb nh r c rspan cspan align cellw) cell
                    (declare (ignore c cspan cellw))
                    (when lb
                      (let* ((cellh (+ (loop for rr from r below (+ r rspan) sum (aref rbase rr))
                                       (* rgap (1- rspan))))
                             (celly (aref roff r))
                             (voff (cond ((string= align "center") (/ (- cellh nh) 2))
                                         ((string= align "end") (- cellh nh))
                                         (t 0)))
                             (fy (+ celly voff)))
                        (shift-box lb 0 (round (- fy (lbox-y lb))))
                        (when (string= align "stretch") (setf (lbox-h lb) (max nh (round cellh))))
                        (push lb boxes)))))
                (values (nreverse boxes)
                        (+ (loop for r below nrows sum (aref rbase r))
                           (* rgap (max 0 (1- nrows)))))))))))))
