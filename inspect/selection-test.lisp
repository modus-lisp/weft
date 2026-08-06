;;;; inspect/selection-test.lisp — the text selection: what is highlighted, what is
;;;; copied, and whether the two are the same characters.
;;;;
;;;; Two independent readings of one selection have to agree, and the interesting
;;;; failures are the ones where only ONE of them is wrong:
;;;;
;;;;   * the MAP against the EXTRACTION — SELECTION-HIGHLIGHT-MAP names the glyphs a
;;;;     reader sees tinted, SELECTION-TEXT names the characters that reach the
;;;;     clipboard.  A provenance bug (a token re-cut without its SNODE/SOFF/SLEN)
;;;;     shows up as the two naming different characters.
;;;;
;;;;   * the MAP against the CANVAS — the check the first one structurally cannot
;;;;     make, because both of its sides are model-side.  A page can agree with
;;;;     itself perfectly and still paint the rectangle somewhere else, and then the
;;;;     highlight simply does not sit under the words.  So this reads PIXELS: render
;;;;     the same box tree twice through the repaint closure, once plain and once
;;;;     selected, and every column that differs is, by construction, where the
;;;;     highlight actually landed.
;;;;
;;;; The pixel side is the reason the layer cases below exist.  Group opacity, a
;;;; filter, a blend mode, a mask and a rasterised transform all paint their subtree
;;;; by SHIFT-BOXing it into an offscreen buffer's frame and back, so a line box's
;;;; absolute x is only true until one of them starts painting.  A highlight map that
;;;; remembered absolute positions was correct on a plain page and displaced by the
;;;; layer's origin on any page wrapping its text in one — which is most of the real
;;;; web, and was `opacity' on a page as ordinary as example.com.
(defpackage #:weft.render.selection-test
  (:use #:cl) (:local-nicknames (#:r #:weft.render) (#:h #:weft.html)) (:export #:run))
(in-package #:weft.render.selection-test)

(defvar *pass* 0) (defvar *fail* 0) (defvar *notes* '())
(defun ck (name ok &optional detail)
  (if ok (incf *pass*)
      (progn (incf *fail*)
             (when (< (length *notes*) 20)
               (push (format nil "~a~@[ — ~a~]" name detail) *notes*)))))

(defun render (html &key (width 900))
  (multiple-value-bind (cv root styles repaint) (r:render-document (h:parse-html html) :width width)
    (declare (ignore styles))
    (values cv root repaint)))

(defun whole (root)
  (multiple-value-bind (sn so) (r:select-all-position root :start)
    (multiple-value-bind (en eo) (r:select-all-position root :end)
      (and sn (r:make-text-selection sn so en eo)))))

(defun no-ws (s) (remove-if (lambda (c) (member c '(#\Space #\Newline #\Tab))) (or s "")))

;;; ---------------------------------------------------------------------------
;;; Reading the highlight back off the box tree (which glyphs sit under a rectangle)
;;; ---------------------------------------------------------------------------
(defun visually-selected (root sel)
  "The characters whose midpoint falls inside one of their line's highlight
   intervals — the highlight read as text, independently of SELECTION-TEXT."
  (let ((map (r:selection-highlight-map root sel)) (out (make-string-output-stream)))
    (when map
      (labels ((walk (b)
                 (when (r::lbox-p b)
                   (if (eq (r::lbox-kind b) :line)
                       (let ((ivs (gethash b map)) (ox (r::lbox-x b)))
                         (dolist (c (r::lbox-children b))
                           (cond ((and (r::frag-p c) ivs)
                                  (let ((txt (r::frag-text c)))
                                    (dotimes (k (length txt))
                                      ;; intervals are line-relative; put the line's x back
                                      (let ((mid (- (/ (+ (r::frag-x-at c k) (r::frag-x-at c (1+ k))) 2) ox)))
                                        (when (some (lambda (iv) (and (<= (car iv) mid) (< mid (cdr iv)))) ivs)
                                          (write-char (char txt k) out))))))
                                 ((r::lbox-p c) (walk c)))))
                       (dolist (c (r::lbox-children b)) (walk c))))))
        (walk root)))
    (get-output-stream-string out)))

;;; ---------------------------------------------------------------------------
;;; Reading the highlight back off the CANVAS (where the rectangle really landed)
;;; ---------------------------------------------------------------------------
(defun differing-extent (plain sel y)
  "(values LO HI) — the first and last+1 column of row Y where the two canvases
   differ.  With one selection painted and nothing else changed, that IS the
   highlight's extent.  NIL NIL when the row is identical in both."
  (let* ((pa (r:canvas-pixels plain)) (pb (r:canvas-pixels sel))
         (w (r:canvas-width plain)) (lo nil) (hi nil))
    (when (< y (r:canvas-height plain))
      (dotimes (x w)
        (let ((o (* 3 (+ (* y w) x))))
          (unless (and (= (aref pa o) (aref pb o))
                       (= (aref pa (+ o 1)) (aref pb (+ o 1)))
                       (= (aref pa (+ o 2)) (aref pb (+ o 2))))
            (unless lo (setf lo x)) (setf hi (1+ x))))))
    (values lo hi)))

(defun highlight-lines (root sel)
  "Every highlighted line as (MID-ROW TOP BOTTOM WANT-X0 WANT-X1) in document space —
   the line-relative intervals with the line box's own x added back, which is exactly
   what the painter is supposed to fill."
  (let ((map (r:selection-highlight-map root sel)) (out '()))
    (when map
      (labels ((walk (b)
                 (when (r::lbox-p b)
                   (let ((ivs (gethash b map)))
                     (when ivs
                       (let ((ox (r::lbox-x b))
                             (y (round (r::lbox-y b))) (hh (round (r::lbox-h b))))
                         (push (list (+ y (floor hh 2)) y (+ y hh)
                                     (round (+ ox (reduce #'min ivs :key #'car)))
                                     (round (+ ox (reduce #'max ivs :key #'cdr))))
                               out))))
                   (dolist (c (r::lbox-children b)) (walk c)))))
        (walk root)))
    (nreverse out)))

(defun own-row-p (line lines)
  "Is LINE's sample row inside no OTHER highlighted line?  An inline-block puts a
   line box inside a line box, and the inner one's rows carry the outer one's
   highlight too — so a single row cannot be attributed to either.  The outer line
   is checked on its own rows; the nested one is skipped rather than mismeasured."
  (let ((row (first line)))
    (notany (lambda (o) (and (not (eq o line)) (<= (second o) row) (< row (third o))))
            lines)))

(defun check-painted (label html &key (width 900))
  "The highlight must be painted where the map says, in document space."
  (multiple-value-bind (plain root repaint) (render html :width width)
    (let ((sel (whole root)))
      (if (null sel)
          (ck (format nil "painted: ~a" label) nil "no selectable text")
          (let ((tinted (funcall repaint sel)) (lines (highlight-lines root sel)) (bad '()))
            (ck (format nil "painted: ~a has highlighted lines" label) (plusp (length lines)))
            (dolist (ln lines)
              (destructuring-bind (row top bot want0 want1) ln
                (declare (ignore top bot))
                (when (and (> (- want1 want0) 2) (own-row-p ln lines))
                  (multiple-value-bind (lo hi) (differing-extent plain tinted row)
                    ;; 2px of slack: the rectangle is rounded to whole pixels and an
                    ;; antialiased glyph edge can tint the column beside it.
                    (unless (and lo (<= (abs (- lo want0)) 2) (<= (abs (- hi want1)) 2))
                      (when (< (length bad) 3)
                        (push (format nil "row ~a want [~a,~a) painted ~@[[~a,~a)~]~:[ NOTHING~;~]"
                                      row want0 want1 lo hi lo)
                              bad)))))))
            (ck (format nil "painted: ~a rectangle sits under its glyphs" label)
                (null bad) (format nil "~{~a; ~}" (reverse bad))))))))

(defun check-agrees (label html &key (width 900))
  "The highlight and the extraction must name the same characters."
  (multiple-value-bind (plain root) (render html :width width)
    (declare (ignore plain))
    (let ((sel (whole root)))
      (ck (format nil "agrees: ~a" label)
          (and sel (string= (no-ws (visually-selected root sel))
                            (no-ws (r:selection-text root sel))))))))

(defun check-drag-pairs (label html &key (width 420))
  "Every drag on a grid of points: the highlighted glyphs and the copied string
   have to be the same characters, whichever way the pointer went."
  (multiple-value-bind (plain root) (render html :width width)
    (declare (ignore plain))
    (let ((bad 0) (tot 0))
      (loop for y1 from 4 below 140 by 9 do
        (loop for x1 from 2 below width by 37 do
          (loop for y2 from 4 below 140 by 13 do
            (loop for x2 from 2 below width by 53 do
              (multiple-value-bind (n1 o1) (r:text-position-at root x1 y1)
                (multiple-value-bind (n2 o2) (r:text-position-at root x2 y2)
                  (when (and n1 n2)
                    (let ((sel (r:make-text-selection n1 o1 n2 o2)))
                      (incf tot)
                      (unless (string= (no-ws (visually-selected root sel))
                                       (no-ws (or (r:selection-text root sel) "")))
                        (incf bad))))))))))
      (ck (format nil "drag pairs: ~a (~a pairs)" label tot) (zerop bad)
          (format nil "~a disagreed" bad)))))

;;; ---------------------------------------------------------------------------
;;; The pages
;;; ---------------------------------------------------------------------------
(defparameter +plain+
  "<html><body><p>The quick brown fox jumps over the lazy dog and keeps on
   running well past the edge of the box.</p><p>A second paragraph.</p></body></html>")

;;; Each of these wraps the SAME text in a construct that makes the painter render
;;; the subtree into an offscreen buffer at its own origin.  Before the highlight map
;;; became line-relative every one of them painted the rectangle displaced by that
;;; origin — the glyphs in one place and the tint in another.
(defparameter +layers+
  '(("group opacity"     "opacity:0.8")
    ("near-opaque group" "opacity:0.99")
    ("filter"            "filter:grayscale(1)")
    ("blend mode"        "mix-blend-mode:multiply")
    ("opacity + margin"  "opacity:0.6;margin-left:140px")))

(defun layer-page (decl)
  (format nil "<html><body><div style=\"~a\"><p>The quick brown fox jumps over the
   lazy dog and keeps on running past the edge.</p><p>A second line of words.</p></div></body></html>"
          decl))

;;; example.com, the page this was actually found on: a viewport-sized centred body
;;; (so the text does not start at the page's left edge) inside an opacity group (so
;;; the painter works in a shifted frame).  Together those made the highlight land a
;;; whole container-width to the right of the words it was supposed to be under.
(defparameter +example-com+
  "<!doctype html><html lang=\"en\"><head><title>Example Domain</title><style>
   body{background:#eee;width:60vw;margin:15vh auto;font-family:system-ui,sans-serif}
   h1{font-size:1.5em}div{opacity:0.8}a:link,a:visited{color:#348}</style></head>
   <body><div><h1>Example Domain</h1><p>This domain is for use in documentation
   examples without needing permission. Avoid use in operations.</p>
   <p><a href=\"https://iana.org/domains/example\">Learn more</a></p></div></body></html>")

;;; loom's own start page, in miniature: a coloured header, cards in a width-capped
;;; main, and an inline-block chip in the middle of a sentence — which puts a line
;;; box inside a line box, the one shape where a row of pixels belongs to two lines
;;; at once.  The browser opens on this, so a selection bug here is the first one
;;; anybody meets.
(defparameter +loom-home+
  "<html><head><style>
   body{font-family:sans-serif;margin:0;background:#f6f7f9;color:#1a1a1a}
   header{background:#2b3a55;color:#fff;padding:20px 28px}
   main{padding:24px 28px;max-width:760px}
   .card{background:#fff;border:1px solid #dde1e7;border-radius:8px;padding:18px 20px;margin:0 0 18px}
   #hot{display:inline-block;padding:6px 10px;border-radius:6px;background:#eef2f8}
   </style></head><body>
   <header><h1>loom</h1><p>weft, in a window you can browse.</p></header>
   <main><div class=\"card\"><h2>a live render</h2>
   <p>This page was fetched, parsed, cascaded, laid out and painted by weft to an
   RGB8 canvas.</p>
   <p>Hover <span id=\"hot\">this chip</span> — its <code>mouseover</code> handler
   reacts.</p></div></main>
   <footer>loom &middot; MIT</footer></body></html>")

(defun run ()
  (let ((*pass* 0) (*fail* 0) (*notes* '()))
    (format t "~&=== weft text selection gate ===~%")

    ;; --- the highlight is painted where the map says -------------------------
    (check-painted "plain page" +plain+)
    (check-painted "indented block" "<html><body><div style='margin-left:120px'><p>Indented words here.</p></div></body></html>")
    (check-painted "centred column" "<html><body><div style='max-width:600px;margin:0 auto'><p>Centred words here.</p></div></body></html>")
    (dolist (l +layers+) (check-painted (first l) (layer-page (second l))))
    (check-painted "example.com" +example-com+)
    (check-painted "loom home page" +loom-home+)
    ;; nested layers: the inner frame's origin is relative to the outer one
    (check-painted "opacity inside opacity"
                   "<html><body><div style='opacity:0.9;margin-left:80px'><div style='opacity:0.8'><p>Nested layer words here.</p></div></div></body></html>")

    ;; --- an unselected page is untouched by any of this ----------------------
    (multiple-value-bind (cv root repaint) (render +example-com+)
      (declare (ignore root))
      (let ((again (funcall repaint nil)))
        (ck "no selection renders byte-identically"
            (equalp (r:canvas-pixels cv) (r:canvas-pixels again)))))

    ;; --- highlight and extraction name the same characters -------------------
    (check-agrees "plain page" +plain+)
    (check-agrees "example.com" +example-com+)
    (check-agrees "loom home page" +loom-home+)
    (dolist (l +layers+) (check-agrees (first l) (layer-page (second l))))
    (check-agrees "table cells" "<html><body><table><tr><td>a1</td><td>b1</td></tr><tr><td>a2</td><td>b2</td></tr></table></body></html>")
    (check-agrees "inline-block chip"
                  "<html><body><p>Hover <span style='display:inline-block;padding:6px 10px'>this chip</span> mid sentence.</p></body></html>")
    (check-agrees "first-letter" "<html><body><style>p::first-letter{font-size:2em;float:left}</style><p>Dropped capital opening a paragraph of text.</p></body></html>")
    (check-agrees "pre keeps its spaces" "<html><body><pre>  indented
  lines  here</pre></body></html>")

    ;; --- and they keep agreeing across a grid of drags -----------------------
    (check-drag-pairs "wrapped paragraph" +plain+)
    (check-drag-pairs "inside an opacity group" (layer-page "opacity:0.8"))

    (format t "~%~d passed, ~d failed~%" *pass* *fail*)
    (when *notes* (format t "~%failures:~%~{  ~a~%~}" (reverse *notes*)))
    (values *pass* *fail*)))
