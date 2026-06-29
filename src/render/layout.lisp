;;;; src/render/layout.lisp — block + inline-formatting layout, paint, render.
;;;;
;;;; Normal-flow layout: block-level boxes stacked vertically (margin/border/
;;;; padding), with INLINE formatting contexts that lay styled text runs into
;;;; line boxes — each fragment keeps its own color/weight/decoration, so bold,
;;;; links, and colored spans render correctly.  Mixed block+inline children are
;;;; grouped into anonymous inline runs.  List items get markers.  Painted to a
;;;; canvas and saved as PNG.
(in-package #:weft.render)

(defstruct lbox x y w h style node kind children marker)   ; kind :block | :line
(defstruct frag x w text style)                            ; a positioned styled text run on a line

(defun st (styles node) (gethash node styles))
(defun cdisplay (cs) (if cs (css:cstyle-display cs) "inline"))
(defun inline-level-p (styles node)
  (case (h:dnode-kind node)
    (:text t)
    (:element (let ((cs (st styles node))) (and cs (not (member (cdisplay cs) '("block" "list-item" "none") :test #'string=)))))
    (t nil)))
(defun block-level-p (styles node)
  (and (eq (h:dnode-kind node) :element)
       (let ((cs (st styles node))) (and cs (member (cdisplay cs) '("block" "list-item") :test #'string=)))))

;;; ---- inline content: styled word runs -> line boxes --------------------
(defun collect-words (node styles default-style)
  "Walk inline content of NODE; return a list of (word . style) tokens
(whitespace becomes token separators).  DEFAULT-STYLE owns bare text."
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
                 (:element (let ((cs (or (st styles n) owner)))
                             (unless (and cs (string= (cdisplay cs) "none"))
                               (unless (member (h:dnode-name n) '("script" "style") :test #'string=)
                                 (loop for c across (h:dnode-children n) do (rec c cs)))))))))
      (rec node (or (st styles node) default-style)))
    (nreverse words)))

(defun word-w (word) (* (length word) *font-w*))
(defun space-w () *font-w*)

(defun layout-inline (words content-x start-y content-w base-cs)
  "Greedy-wrap WORDS (list of (word . style)) into line boxes.  Returns
(values line-boxes total-height).  Honors text-align (center/right)."
  (let* ((lh (max *font-h* (round (* (css:cstyle-font-size base-cs) (css:cstyle-line-height base-cs)))))
         (align (css:cstyle-text-align base-cs))
         (lines '()) (cur '()) (cx content-x) (y start-y) (h 0))
    (flet ((flush ()
             (when cur
               (let* ((frags (nreverse cur))
                      (used (- (+ (frag-x (car (last frags))) (frag-w (car (last frags)))) content-x))
                      (shift (cond ((string= align "center") (max 0 (floor (- content-w used) 2)))
                                   ((string= align "right") (max 0 (- content-w used)))
                                   (t 0))))
                 (when (plusp shift) (dolist (fr frags) (incf (frag-x fr) shift)))
                 (push (make-lbox :x content-x :y y :w content-w :h lh :kind :line :children frags) lines))
               (incf y lh) (incf h lh) (setf cur '() cx content-x))))
      (dolist (wd words)
        (let* ((word (car wd)) (style (cdr wd)) (ww (word-w word))
               (need (if cur (+ (space-w) ww) ww)))
          (when (and cur (> (+ (- cx content-x) need) content-w)) (flush))
          (when (> (- cx content-x) 0) (incf cx (space-w)))
          (push (make-frag :x cx :w ww :text word :style style) cur)
          (incf cx ww)))
      (flush))
    (values (nreverse lines) (if (zerop h) 0 h))))

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
(defun layout-node (node styles x y avail-w)
  "Lay out block-level NODE at (X,Y); AVAIL-W is the containing content width.
Returns (values lbox advance-height)."
  (let ((cs (st styles node)))
    (when (or (null cs) (string= (cdisplay cs) "none")) (return-from layout-node (values nil 0)))
    (let* ((mt (css:cstyle-margin-top cs)) (mb (css:cstyle-margin-bottom cs))
           (ml (css:cstyle-margin-left cs)) (mr (css:cstyle-margin-right cs))
           (pt (css:cstyle-padding-top cs)) (pb (css:cstyle-padding-bottom cs))
           (pl (css:cstyle-padding-left cs)) (pr (css:cstyle-padding-right cs))
           (bt (css:cstyle-border-top-width cs)) (bb (css:cstyle-border-bottom-width cs))
           (bl (css:cstyle-border-left-width cs)) (br (css:cstyle-border-right-width cs))
           (width (let ((w (css:cstyle-width cs))) (if (numberp w) w (- avail-w ml mr))))
           (content-w (max 0 (- width bl br pl pr)))
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
          (return-from layout-node (values lb (+ mt box-h mb)))))
      ;; classify children: anonymous-group consecutive inline-level nodes
      (let ((kids (coerce (h:dnode-children node) 'list)) (group '()) (yy cy))
        (flet ((flush-inline ()
                 (when group
                   (let ((words (loop for g in (nreverse group) append (collect-words g styles cs))))
                     (when words
                       (multiple-value-bind (lines lh-total) (layout-inline words cx yy content-w cs)
                         (dolist (l lines) (push l children))
                         (incf yy lh-total) (incf content-h lh-total))))
                   (setf group '()))))
          (dolist (k kids)
            (cond
              ((block-level-p styles k)
               (flush-inline)
               (multiple-value-bind (lb adv) (layout-node k styles cx yy content-w)
                 (when lb (push lb children)) (incf yy adv) (incf content-h adv)))
              ((or (eq (h:dnode-kind k) :text) (inline-level-p styles k)) (push k group))))
          (flush-inline)))
      (let* ((box-h (+ content-h pt pb bt bb))
             (lb (make-lbox :x box-x :y box-y :w width :h (max box-h (if list-item *font-h* 0))
                            :style cs :node node :kind :block :children (nreverse children)
                            :marker (when list-item (css:cstyle-list-style cs)))))
        (values lb (+ mt (lbox-h lb) mb))))))

(defun layout-tree (document styles width)
  (let ((body (css:query-select document "body")))
    (when body (layout-node body styles 0 0 width))))

;;; ---- paint --------------------------------------------------------------
(defun rgb (color) (list (first color) (second color) (third color)))

(defun marker-glyph (kind) (cond ((string= kind "circle") "o") ((string= kind "square") "#")
                                 ((string= kind "none") "") (t "•")))

(defun paint-box (cv lb)
  (when lb
    (case (lbox-kind lb)
      (:block
       (let ((cs (lbox-style lb)))
         (when (css:cstyle-background cs)
           (fill-rect cv (lbox-x lb) (lbox-y lb) (lbox-w lb) (lbox-h lb) (rgb (css:cstyle-background cs))))
         (let ((bc (rgb (css:cstyle-border-color cs))))
           (when (plusp (css:cstyle-border-top-width cs)) (fill-rect cv (lbox-x lb) (lbox-y lb) (lbox-w lb) (css:cstyle-border-top-width cs) bc))
           (when (plusp (css:cstyle-border-bottom-width cs)) (fill-rect cv (lbox-x lb) (- (+ (lbox-y lb) (lbox-h lb)) (css:cstyle-border-bottom-width cs)) (lbox-w lb) (css:cstyle-border-bottom-width cs) bc))
           (when (plusp (css:cstyle-border-left-width cs)) (fill-rect cv (lbox-x lb) (lbox-y lb) (css:cstyle-border-left-width cs) (lbox-h lb) bc))
           (when (plusp (css:cstyle-border-right-width cs)) (fill-rect cv (- (+ (lbox-x lb) (lbox-w lb)) (css:cstyle-border-right-width cs)) (lbox-y lb) (css:cstyle-border-right-width cs) (lbox-h lb) bc)))
         (when (and (lbox-marker lb) (plusp (length (marker-glyph (lbox-marker lb)))))
           (draw-text cv (marker-glyph (lbox-marker lb))
                      (round (- (+ (lbox-x lb) (css:cstyle-padding-left cs)) (* 2 *font-w*)))
                      (round (+ (lbox-y lb) (css:cstyle-padding-top cs))) (rgb (css:cstyle-color cs))))
         (dolist (c (lbox-children lb)) (paint-box cv c))))
      (:line
       (let ((yoff (max 0 (floor (- (lbox-h lb) *font-h*) 2))))
         (dolist (fr (lbox-children lb))
           (let ((cs (frag-style fr)))
             (draw-text cv (frag-text fr) (round (frag-x fr)) (round (+ (lbox-y lb) yoff))
                        (rgb (css:cstyle-color cs))
                        :bold (>= (css:cstyle-font-weight cs) 600)
                        :underline (member "underline" (css:cstyle-text-decoration cs) :test #'string=)))))))))

(defun collect-stylesheets (doc)
  (with-output-to-string (o)
    (labels ((rec (n)
               (when (eq (h:dnode-kind n) :element)
                 (when (string= (h:dnode-name n) "style")
                   (loop for c across (h:dnode-children n) when (eq (h:dnode-kind c) :text)
                         do (write-string (h:dnode-data c) o) (terpri o)))
                 (loop for c across (h:dnode-children n) do (rec c)))))
      (loop for c across (h:dnode-children doc) do (rec c)))))

(defun render-to-png (html css width path &key (min-height 200))
  "Parse HTML, gather CSS (explicit + page <style> tags), cascade, lay out at
WIDTH px, paint, save PNG.  Returns (values path width height)."
  (let* ((doc (h:parse-html html))
         (sheet (css:parse-stylesheet (concatenate 'string (or css "") (string #\Newline)
                                                   (collect-stylesheets doc))))
         (styles (css:compute-styles doc sheet)))
    (multiple-value-bind (root adv) (layout-tree doc styles width)
      (declare (ignore adv))
      (let* ((height (max min-height (if root (round (+ (lbox-y root) (lbox-h root) 8)) min-height)))
             ;; canvas background = body's background (propagated), else white
             (body (css:query-select doc "body"))
             (bg (let ((cs (and body (gethash body styles)))) (and cs (css:cstyle-background cs))))
             (cv (make-canvas width height (if bg (rgb bg) '(255 255 255)))))
        (paint-box cv root)
        (write-png cv path)
        (values path width height)))))
