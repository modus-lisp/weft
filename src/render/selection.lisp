;;;; src/render/selection.lisp — selecting document text: hit-testing a point to a
;;;; character, holding the range, painting the highlight, and extracting the string.
;;;;
;;;; This is the producer side of a clipboard.  A shell (loom) turns a press-drag-
;;;; release into a TEXT-SELECTION, paints it by handing it back to RENDER-DOCUMENT,
;;;; and copies SELECTION-TEXT.  Everything here is additive: with no selection the
;;;; painter's highlight hook is NIL and a page renders byte-identically.
;;;;
;;;; COORDINATES.  A selection endpoint is a DOM text node plus a character offset
;;;; into that node's data — not a box, not a fragment index.  It has to be, because
;;;; the box tree is rebuilt on every relayout and line breaking moves with the
;;;; viewport width, while source offsets do not.  Selecting a sentence and then
;;;; resizing the window keeps the same sentence selected.
;;;;
;;;; WHAT LAYOUT HAD TO START KEEPING.  A FRAG used to say only "this string, this
;;;; wide, at this x, from this element".  That is enough to paint and enough to
;;;; answer "which link did I click", but not enough to answer "which character".
;;;; So a frag now also carries SNODE/SOFF/SLEN — the text node it came from and the
;;;; half-open source range it shows — threaded from the tokenizer through
;;;; hyphenation, break-all and ::first-letter splits (TOK-FRAG / TOK-REST in
;;;; layout.lisp).  Three slots per text run, and one text-node reference that the
;;;; document already holds anyway.
;;;;
;;;; WHAT IT STILL DOES NOT KEEP: per-glyph advances.  Shaping measures them and
;;;; throws them away, and storing an advance vector for every run on every line
;;;; would cost more than the rest of the box tree.  They are re-measured here, for
;;;; the one frag a hit-test lands in and the two frags a highlight ends in, with
;;;; the same WORD-W that laid the line out — so the caret and the highlight edge
;;;; land exactly where the glyphs do, by construction rather than by agreement.

(in-package #:weft.render)

;;; ---------------------------------------------------------------------------
;;; The model
;;; ---------------------------------------------------------------------------
;;; Anchor and focus, in the DOM's own words, exactly as a user drag produces them:
;;; the anchor is where the button went down and does not move, the focus follows
;;; the pointer and may end up before the anchor.  Document order is imposed later
;;; (SELECTION-SPAN), not stored, so extending a selection backwards needs no
;;; special case.  This is deliberately NOT a DOM Range: no live mutation tracking,
;;; no element/offset endpoints, no boundary-point comparison against nodes that
;;; were never laid out.  A start/end pair over the laid-out text is all a
;;; clipboard or a screen reader needs.

(defstruct (text-selection (:constructor make-text-selection
                               (anchor-node anchor-offset
                                &optional (focus-node anchor-node)
                                          (focus-offset anchor-offset))))
  anchor-node anchor-offset focus-node focus-offset)

(defun selection-collapsed-p (sel)
  "True when SEL selects nothing — no endpoints, or both at the same character.
   A collapsed selection is a caret: it highlights nothing and copies nothing, and
   callers must not let it clobber a clipboard."
  (or (null sel)
      (null (text-selection-anchor-node sel))
      (null (text-selection-focus-node sel))
      (and (eq (text-selection-anchor-node sel) (text-selection-focus-node sel))
           (eql (text-selection-anchor-offset sel) (text-selection-focus-offset sel)))))

(defun selection-extend (sel node offset)
  "SEL with its focus moved to (NODE,OFFSET), the anchor untouched — one drag step.
   With no SEL yet, a collapsed selection anchored there."
  (if sel
      (make-text-selection (text-selection-anchor-node sel) (text-selection-anchor-offset sel)
                           node offset)
      (make-text-selection node offset)))

;;; ---------------------------------------------------------------------------
;;; The laid-out text, in document order
;;; ---------------------------------------------------------------------------

(defstruct (trun (:constructor %trun (line frag block)))
  line          ; the :LINE lbox the run sits on (what a highlight rectangle spans)
  frag          ; the FRAG itself
  block)        ; the paragraph the run belongs to — where a line break comes from

(defun text-runs (root)
  "Every selectable text run under ROOT, in paint order, tagged with its line box
   and the paragraph that owns it.  Runs with no source text node are skipped,
   which is what keeps replaced content and generated content out of a selection:
   an <img>'s alt frag, a ::before's content and a form widget's painted value
   carry no SNODE, so dragging across them yields no offsets rather than garbage
   ones.

   The paragraph is the nearest BLOCK-LEVEL ancestor box, and an atomic inline is
   not one: `Hover <span style=display:inline-block>this chip</span> now' is one
   sentence and must copy as one line, even though the chip is a block container
   with a line box inside it.  Blocks nested deeper inside that chip do start
   paragraphs again — ATOMIC only suppresses the atomic's own box, not its
   descendants'."
  (let ((out (make-array 32 :adjustable t :fill-pointer 0)))
    (labels ((walk (b para atomic)
               (when (lbox-p b)
                 (if (eq (lbox-kind b) :line)
                     (dolist (c (lbox-children b))
                       (cond ((frag-p c)
                              (when (and (frag-snode c) (frag-soff c)
                                         (eq (h:dnode-kind (frag-snode c)) :text))
                                (vector-push-extend (%trun b c para) out)))
                             ;; an atomic inline (inline-block / replaced) can hold
                             ;; lines of its own; they stay in THIS paragraph
                             ((lbox-p c) (walk c para t))))
                     (let ((p (if atomic para b)))
                       (dolist (c (lbox-children b)) (walk c p nil)))))))
      (walk root nil nil))
    out))

(defun run-soff (r) (frag-soff (trun-frag r)))
(defun run-slen (r) (or (frag-slen (trun-frag r)) 0))

(defun locate-position (runs node offset)
  "Where (NODE,OFFSET) sits in RUNS: (values INDEX WITHIN), WITHIN counted from the
   run's own source start.  NIL when NODE contributed no laid-out text at all
   (display:none, or a node from a stale document).

   An offset that falls in the gap BETWEEN two runs of the same node — the
   collapsed whitespace between two words — belongs to neither, so it snaps to the
   end of the run before it.  That is the reading a drag wants: releasing over a
   space selects the word you dragged past, not the one you have not reached."
  (let ((first nil) (last nil))
    (loop for i from 0 below (length runs)
          for r = (aref runs i)
          when (eq (frag-snode (trun-frag r)) node) do
            (unless first (setf first i))
            (setf last i)
            (let ((a (run-soff r)))
              (when (and (<= a offset) (<= offset (+ a (run-slen r))))
                (return-from locate-position (values i (- offset a))))
              ;; past this run but before the next one of the same node: clamp back
              (when (< offset a) (return-from locate-position (values i 0)))))
    (cond ((null first) nil)
          (t (values last (run-slen (aref runs last)))))))

;;; ---------------------------------------------------------------------------
;;; Geometry: character <-> x, re-measured on demand
;;; ---------------------------------------------------------------------------

(defun frag-x-at (fr k)
  "Document X of the boundary before the Kth painted character of FR.  The prefix
   width is re-measured with WORD-W — the same call that sized the run during
   layout — rather than read from a stored advance table, which layout does not
   build.  Both ends are answered from FRAG-X and FRAG-W directly so a full-run
   highlight is exact regardless of what re-measurement rounds to."
  (let* ((txt (frag-text fr))
         (n (length txt))
         (i (min (max 0 k) n))
         (x0 (+ (frag-x fr) (frag-dx fr))))
    (cond ((zerop i) x0)
          ((>= i n) (+ x0 (frag-w fr)))
          (t (+ x0 (word-w (subseq txt 0 i) (frag-style fr)))))))

(defun frag-char-index (fr x)
  "The painted-character boundary of FR nearest document X — so a click on the left
   half of a glyph lands before it and on the right half after it, and a click past
   either end saturates.  Linear in the run's length, which is a word."
  (let* ((txt (frag-text fr)) (n (length txt))
         (best 0) (bd (abs (- x (frag-x-at fr 0)))))
    (loop for k from 1 to n
          for d = (abs (- x (frag-x-at fr k)))
          when (< d bd) do (setf bd d best k))
    best))

(defun run-source-offset (r k)
  "Painted index K within run R as an absolute offset into its text node's data.
   The two indices differ only where text-transform changed a character's length
   (uppercasing ß paints two glyphs for one source character), so K is clamped into
   the run's source range instead of being trusted past its end."
  (+ (run-soff r) (min k (run-slen r))))

;;; ---------------------------------------------------------------------------
;;; Hit-testing
;;; ---------------------------------------------------------------------------

(defun text-position-at (root x y)
  "Document point (X,Y) as (values TEXT-NODE OFFSET DIRECT-P), or NIL when ROOT
   holds no selectable text.  DIRECT-P is true when the point is actually inside a
   run; otherwise the nearest run answered, which is what makes a drag into the
   margin below a paragraph keep selecting it instead of stopping dead.

   Nearness is lexicographic — vertical distance first, then horizontal — because
   lines are the unit a reader thinks in: a point far to the right of a short line
   belongs to that line's end, not to a longer line above it."
  (let ((runs (text-runs root)) (best nil) (bd nil) (direct nil))
    (loop for k from 0 below (length runs)
          for r = (aref runs k)
          for fr = (trun-frag r)
          for line = (trun-line r)
          for ly = (+ (lbox-y line) (frag-dy fr))
          for lh = (lbox-h line)
          for fx = (+ (frag-x fr) (frag-dx fr))
          for dv = (cond ((< y ly) (- ly y)) ((> y (+ ly lh)) (- y ly lh)) (t 0))
          for dh = (cond ((< x fx) (- fx x))
                         ((> x (+ fx (frag-w fr))) (- x fx (frag-w fr)))
                         (t 0))
          for d = (+ (* 100000 dv) dh)
          do (when (or (null bd) (< d bd))
               (setf bd d best r direct (and (zerop dv) (zerop dh)))))
    (when best
      (values (frag-snode (trun-frag best))
              (run-source-offset best (frag-char-index (trun-frag best) x))
              direct))))

;;; ---------------------------------------------------------------------------
;;; Document order
;;; ---------------------------------------------------------------------------

(defun selection-span (runs sel)
  "SEL normalised to document order over RUNS: (values I A J B) — from character A
   of run I through character B of run J, offsets relative to each run's source
   start.  NIL when the selection is collapsed, empty, or names nodes that were
   never laid out.

   Order comes from the run vector, not from a DOM tree walk: the runs are already
   in paint order, so comparing two positions is comparing two indices.  That also
   means a position in a node the layout dropped simply has no place in the order
   and the whole selection declines, rather than half-resolving."
  (unless (selection-collapsed-p sel)
    (multiple-value-bind (i a)
        (locate-position runs (text-selection-anchor-node sel) (text-selection-anchor-offset sel))
      (multiple-value-bind (j b)
          (locate-position runs (text-selection-focus-node sel) (text-selection-focus-offset sel))
        (when (and i j)
          (cond ((or (< i j) (and (= i j) (< a b))) (values i a j b))
                ((or (> i j) (and (= i j) (> a b))) (values j b i a))
                (t nil)))))))

(defmacro do-selected-runs ((r lo hi runs i a j b) &body body)
  "Walk the selected part of each run from (I,A) to (J,B): R is the run, LO/HI the
   source-relative half-open range of it that is selected."
  (let ((k (gensym)) (ri (gensym)) (ra (gensym)) (rj (gensym)) (rb (gensym)))
    `(let ((,ri ,i) (,ra ,a) (,rj ,j) (,rb ,b))
       (loop for ,k from ,ri to ,rj
             for ,r = (aref ,runs ,k)
             for ,lo = (if (= ,k ,ri) ,ra 0)
             for ,hi = (if (= ,k ,rj) ,rb (run-slen ,r))
             when (< ,lo ,hi) do (progn ,@body)))))

;;; ---------------------------------------------------------------------------
;;; The highlight
;;; ---------------------------------------------------------------------------

(defun %line-space-tolerance (line)
  "How wide a gap on LINE still counts as inside the selection: the widest
   inter-word space any run on it uses.  Selecting `quick brown' must tint the
   space between the words — the runs are separate but the selection is not — while
   a genuinely distant run stays its own rectangle."
  (let ((tol 0))
    (dolist (c (lbox-children line) (+ tol 1))
      (when (frag-p c) (setf tol (max tol (space-w (frag-style c))))))))

(defun %merge-intervals (ivs tol)
  "Sort (X0 . X1) intervals left to right and fuse any two separated by at most TOL,
   so one line yields one rectangle per contiguous stretch instead of one per word."
  (let ((sorted (sort (copy-list ivs) #'< :key #'car)) (out '()))
    (dolist (iv sorted (nreverse out))
      (if (and out (<= (- (car iv) (cdr (first out))) tol))
          (setf (cdr (first out)) (max (cdr (first out)) (cdr iv)))
          (push (cons (car iv) (cdr iv)) out)))))

(defun selection-highlight-map (root sel)
  "SEL as a line-box -> list of (X0 . X1) document-space intervals, ready to bind to
   *SELECTION-HIGHLIGHT* around a paint; NIL when nothing is selected.  One entry
   per line, because a selection is a run of characters and not a rectangle — the
   last line of a wrapped paragraph stops where the text does."
  (let ((runs (text-runs root)))
    (multiple-value-bind (i a j b) (selection-span runs sel)
      (when i
        (let ((map (make-hash-table :test 'eq)))
          (do-selected-runs (r lo hi runs i a j b)
            (let ((fr (trun-frag r)))
              (push (cons (frag-x-at fr lo) (frag-x-at fr hi))
                    (gethash (trun-line r) map))))
          (maphash (lambda (line ivs)
                     (setf (gethash line map) (%merge-intervals ivs (%line-space-tolerance line))))
                   map)
          map)))))

;;; ---------------------------------------------------------------------------
;;; Extraction
;;; ---------------------------------------------------------------------------

(defun %node-data (n) (or (and n (h:dnode-data n)) ""))

(defun %normalize-gap (gap style)
  "The source characters that fall BETWEEN two runs, rendered the way the runs
   themselves were.  Layout dropped them — a word token never includes the
   whitespace around it — so extraction has to put back exactly what white-space
   says was there: everything under `pre', newlines only under `pre-line', and a
   single space under the normal collapsing rules."
  (let ((ws (if style (css:cstyle-white-space style) "normal")))
    (cond ((zerop (length gap)) "")
          ((member ws '("pre" "pre-wrap" "break-spaces") :test #'string=) gap)
          ((string= ws "pre-line")
           (let ((nl (count #\Newline gap)))
             (if (plusp nl) (make-string nl :initial-element #\Newline) " ")))
          ;; collapsing: any whitespace at all becomes one space; a gap of nothing
          ;; but invisible format controls becomes nothing
          ((find-if (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return))) gap) " ")
          (t ""))))

(defun %gap-source (prev-run prev-end run)
  "The source text skipped between the end of PREV-RUN and the start of RUN: the
   tail of the previous text node plus the head of this one.  Within one node that
   is the whitespace between two words — or, in a <pre>, the newline that ended a
   line.  Across nodes it is the space that keeps `see <a>x</a>' from copying as
   `seex'."
  (let ((pn (frag-snode (trun-frag prev-run)))
        (n (frag-snode (trun-frag run)))
        (start (frag-soff (trun-frag run))))
    (if (eq pn n)
        (let* ((d (%node-data n)) (s (min (length d) (max 0 prev-end))) (e (min (length d) (max s start))))
          (subseq d s e))
        (let* ((pd (%node-data pn)) (d (%node-data n))
               (ps (min (length pd) (max 0 prev-end))))
          (concatenate 'string (subseq pd ps) (subseq d 0 (min (length d) (max 0 start))))))))

(defun selection-text (root sel)
  "The selected text, or NIL when nothing is selected.

   Characters come from the SOURCE — a slice of each text node's data — not from
   the painted strings, so what is copied is what the document says: a
   text-transform:uppercase heading copies in its own case, and an automatic
   hyphen inserted at a line break is not in the source and does not come along.
   That is also why a word split across two lines rejoins with nothing between it.

   The joins between runs are the whole problem, and they resolve in this order:
   * a different owning block is a paragraph boundary -> one newline.  Two <p>s,
     two <li>s, two table cells, a heading and its body all come out on their own
     lines instead of running together.
   * otherwise, whatever SOURCE text the layout skipped between the two runs,
     normalised by white-space (%NORMALIZE-GAP).  This is what makes a <pre> keep
     its newlines and indentation, a soft wrap inside a paragraph copy as a single
     space, and the two halves of a word broken by overflow-wrap copy with nothing
     between them — one rule, because they are one question.
   * otherwise, a run the tokenizer saw whitespace before -> one space.  This is
     the case where the whitespace lived in a THIRD text node that produced no run
     of its own, as in `a <em>b</em> <b>c</b>'.
   * otherwise nothing."
  (let ((runs (text-runs root)))
    (multiple-value-bind (i a j b) (selection-span runs sel)
      (when i
        (let ((prev nil) (prev-end 0))
          (with-output-to-string (o)
            (do-selected-runs (r lo hi runs i a j b)
              (let* ((fr (trun-frag r))
                     (data (%node-data (frag-snode fr)))
                     (s (max 0 (min (length data) (+ (frag-soff fr) lo))))
                     (e (max s (min (length data) (+ (frag-soff fr) hi)))))
                (when (< s e)
                  (when prev
                    (if (not (eq (trun-block r) (trun-block prev)))
                        (write-char #\Newline o)
                        (let ((gap (%normalize-gap (%gap-source prev prev-end r) (frag-style fr))))
                          (if (plusp (length gap))
                              (write-string gap o)
                              (when (frag-space fr) (write-char #\Space o))))))
                  (write-string data o :start s :end e)
                  (setf prev r prev-end e))))))))))

(defun select-all-position (root which)
  "The (values NODE OFFSET) of the very :START or :END of ROOT's text, so a shell can
   offer select-all without knowing anything about the box tree."
  (let ((runs (text-runs root)))
    (when (plusp (length runs))
      (let ((r (aref runs (if (eq which :start) 0 (1- (length runs))))))
        (values (frag-snode (trun-frag r))
                (if (eq which :start) (run-soff r) (+ (run-soff r) (run-slen r))))))))
