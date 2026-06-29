;;;; src/render/layout.lisp — block + inline-text layout, paint, and render entry.
;;;;
;;;; A pragmatic CSS layout: the normal flow with block-level boxes stacked
;;;; vertically (margins/border/padding honored) and inline content (text)
;;;; word-wrapped into lines.  Produces a box tree (LBOX), painted onto a canvas
;;;; and saved to PNG.  Not pixel-perfect — a real, recognizable rendering.
(in-package #:weft.render)

(defstruct lbox x y w h style node kind text children)   ; kind :block | :line

(defun st (styles node) (gethash node styles))
(defun child-elements (node) (loop for c across (h:dnode-children node) when (eq (h:dnode-kind c) :element) collect c))
(defun block-child-p (styles node)
  (let ((cs (st styles node))) (and cs (string= (css:cstyle-display cs) "block"))))
(defun has-block-children (styles node)
  (some (lambda (c) (block-child-p styles c)) (child-elements node)))

(defun collect-text (node)
  "Concatenate descendant text (inline content), whitespace-collapsed."
  (let ((out (make-string-output-stream)) (sp nil) (started nil))
    (labels ((rec (n)
               (case (h:dnode-kind n)
                 (:text (loop for c across (h:dnode-data n) do
                          (if (member c '(#\Space #\Tab #\Newline #\Return)) (setf sp t)
                              (progn (when (and sp started) (write-char #\Space out))
                                     (setf sp nil started t) (write-char c out)))))
                 (:element (unless (member (h:dnode-name n) '("script" "style") :test #'string=)
                             (loop for c across (h:dnode-children n) do (rec c)))))))
      (rec node))
    (get-output-stream-string out)))

(defun wrap-words (text content-w)
  "Greedy word-wrap TEXT to lines fitting CONTENT-W px (font is fixed-width)."
  (let ((cpl (max 1 (floor content-w *font-w*))) (lines '()) (cur ""))
    (dolist (word (split-spaces text))
      (let ((cand (if (string= cur "") word (concatenate 'string cur " " word))))
        (if (<= (length cand) cpl) (setf cur cand)
            (progn (when (plusp (length cur)) (push cur lines))
                   (setf cur (if (> (length word) cpl) (subseq word 0 cpl) word))))))
    (when (plusp (length cur)) (push cur lines))
    (nreverse lines)))

(defun split-spaces (s)
  (let ((out '()) (b (make-string-output-stream)) (any nil))
    (loop for c across s do
      (if (char= c #\Space) (when any (push (get-output-stream-string b) out) (setf any nil b (make-string-output-stream)))
          (progn (write-char c b) (setf any t))))
    (when any (push (get-output-stream-string b) out))
    (nreverse out)))

(defun line-px (cs) (max *font-h* (round (* (css:cstyle-font-size cs) (css:cstyle-line-height cs)))))

(defun layout-node (node styles x y avail-w)
  "Lay out NODE (a block element) at (X,Y) with AVAIL-W content width available.
Returns (values lbox advance-height)."
  (let* ((cs (st styles node)))
    (when (or (null cs) (string= (css:cstyle-display cs) "none")) (return-from layout-node (values nil 0)))
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
           (children '()) (content-h 0))
      (cond
        ((has-block-children styles node)
         (let ((yy cy))
           (dolist (c (child-elements node))
             (multiple-value-bind (lb adv) (layout-node c styles cx yy content-w)
               (when lb (push lb children)) (incf yy adv) (incf content-h adv)))))
        (t ;; inline content -> wrapped text lines
         (let* ((text (collect-text node)) (lines (wrap-words text content-w))
                (lh (line-px cs)) (yy cy))
           (when (string= (css:cstyle-white-space cs) "pre")
             (setf lines (split-newlines (collect-pre node))))
           (dolist (ln lines)
             (push (make-lbox :x cx :y yy :w content-w :h lh :style cs :kind :line :text ln) children)
             (incf yy lh) (incf content-h lh)))))
      (let* ((box-h (+ content-h pt pb bt bb))
             (lb (make-lbox :x box-x :y box-y :w width :h box-h :style cs :node node :kind :block
                            :children (nreverse children))))
        (values lb (+ mt box-h mb))))))

(defun collect-pre (node)
  (with-output-to-string (o)
    (labels ((rec (n) (case (h:dnode-kind n) (:text (write-string (h:dnode-data n) o))
                        (:element (loop for c across (h:dnode-children n) do (rec c)))))) (rec node))))
(defun split-newlines (s) (loop with start = 0 for i from 0 to (length s)
                                 when (or (= i (length s)) (char= (char s i) #\Newline))
                                   collect (prog1 (subseq s start i) (setf start (1+ i)))))

(defun layout-tree (document styles width)
  "Lay out the document body into a box tree within WIDTH px."
  (let ((body (css:query-select document "body")))
    (when body (layout-node body styles 0 0 width))))

;;; ---- paint --------------------------------------------------------------
(defun rgb (color) (list (first color) (second color) (third color)))

(defun paint-box (cv lb)
  (when lb
    (case (lbox-kind lb)
      (:block
       (let ((cs (lbox-style lb)))
         (when (css:cstyle-background cs)
           (fill-rect cv (lbox-x lb) (lbox-y lb) (lbox-w lb) (lbox-h lb) (rgb (css:cstyle-background cs))))
         ;; borders (simple: top/bottom/left/right bars)
         (let ((bc (rgb (css:cstyle-border-color cs))))
           (when (plusp (css:cstyle-border-top-width cs)) (fill-rect cv (lbox-x lb) (lbox-y lb) (lbox-w lb) (css:cstyle-border-top-width cs) bc))
           (when (plusp (css:cstyle-border-bottom-width cs)) (fill-rect cv (lbox-x lb) (- (+ (lbox-y lb) (lbox-h lb)) (css:cstyle-border-bottom-width cs)) (lbox-w lb) (css:cstyle-border-bottom-width cs) bc))
           (when (plusp (css:cstyle-border-left-width cs)) (fill-rect cv (lbox-x lb) (lbox-y lb) (css:cstyle-border-left-width cs) (lbox-h lb) bc))
           (when (plusp (css:cstyle-border-right-width cs)) (fill-rect cv (- (+ (lbox-x lb) (lbox-w lb)) (css:cstyle-border-right-width cs)) (lbox-y lb) (css:cstyle-border-right-width cs) (lbox-h lb) bc)))
         (dolist (c (lbox-children lb)) (paint-box cv c))))
      (:line
       (let ((cs (lbox-style lb)))
         (draw-text cv (lbox-text lb) (round (lbox-x lb))
                    (round (+ (lbox-y lb) (max 0 (floor (- (lbox-h lb) *font-h*) 2))))
                    (rgb (css:cstyle-color cs))))))))

(defun collect-stylesheets (doc)
  "Concatenate the text of all <style> elements in DOC."
  (with-output-to-string (o)
    (labels ((rec (n)
               (when (eq (h:dnode-kind n) :element)
                 (when (string= (h:dnode-name n) "style")
                   (loop for c across (h:dnode-children n)
                         when (eq (h:dnode-kind c) :text) do (write-string (h:dnode-data c) o)
                         do (terpri o)))
                 (loop for c across (h:dnode-children n) do (rec c)))))
      (loop for c across (h:dnode-children doc) do (rec c)))))

(defun render-to-png (html css width path &key (min-height 200))
  "Full pipeline: parse HTML, gather CSS (explicit + the page's <style> tags),
cascade, lay out at WIDTH px, paint, save PNG."
  (let* ((doc (h:parse-html html))
         (sheet (css:parse-stylesheet (concatenate 'string (or css "") (string #\Newline)
                                                   (collect-stylesheets doc))))
         (styles (css:compute-styles doc sheet)))
    (multiple-value-bind (root adv) (layout-tree doc styles width)
      (declare (ignore adv))
      (let* ((height (max min-height (if root (round (+ (lbox-y root) (lbox-h root) 8)) min-height)))
             (cv (make-canvas width height)))
        (paint-box cv root)
        (write-png cv path)
        (values path width height)))))
