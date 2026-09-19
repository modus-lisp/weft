;;;; geometry.lisp — the layout tree, read from script.
;;;;
;;;; getBoundingClientRect is the most-called layout API on the web: sticky
;;;; headers, tooltips, virtualised lists, lazy-loading, charts.  It returns a
;;;; VALUE, and dom.lisp has refused for several waves to invent one --
;;;; "a fabricated rect is worse than a missing method because no caller can tell
;;;; it is a guess.  When this arrives it must come from the layout tree."  This
;;;; is that: every number here is read out of the boxes the renderer actually
;;;; produced, and nothing is synthesised.
;;;;
;;;; THE LAYOUT IS RUN ON DEMAND.  Script runs before (and between) paints, so
;;;; there may be no current layout when a script asks -- and after a DOM change
;;;; the old one is wrong.  ENSURE-LAYOUT lays out when asked and caches until the
;;;; document changes again.  That means a script that mutates and then measures
;;;; pays for a fresh layout on every measurement, which is not a shortcoming: it
;;;; is exactly what a browser does, and exactly why "layout thrashing" is a
;;;; documented performance footgun rather than a bug.
;;;;
;;;; WHAT A NODE'S RECTANGLE IS depends on what kind of box it got:
;;;;   * a block-level element, and an atomic one (inline-block, <img>, a form
;;;;     control), owns an LBOX -- its rectangle is that box, exactly;
;;;;   * a non-replaced inline element (<span>, <a>) owns NO box.  Its geometry is
;;;;     the text fragments laid out for it, whose rectangle weft already defines
;;;;     as (frag-x, line-y, frag-w, line-h) -- that is how INTERACT resolves a
;;;;     click on inline text to the <a> it landed in.  Using the same convention
;;;;     here keeps one answer to "where is this element", rather than inventing a
;;;;     second one that disagrees with where clicks land.
;;;;   * an element with neither (display:none, or never laid out) has no
;;;;     rectangle at all, and the spec's answer for that is a zero rect.
(in-package #:weft.script)

(defun ensure-layout (ctx)
  "The layout tree for CTX's document, laid out on demand and cached until the
   DOM changes.  Returns the root LBOX, or NIL when the document lays out to
   nothing at all."
  (let ((cached (context-layout ctx)))
    (if (and cached (not (context-dirty ctx)))
        cached
        ;; No usable box tree: cascade and lay out one.  A shell that has already
        ;; laid the document out publishes its root here (loom does, after every
        ;; render), and then this branch never runs -- which matters for more than
        ;; speed, since the rectangle a script reads should be the one on screen.
        (setf (context-layout ctx)
              (r:layout-document (context-document ctx)
                                 :width (context-width ctx)
                                 :css (context-css ctx))))))

(defun %frag-belongs-to-p (frag node)
  "True when FRAG was laid out for NODE or for something inside it.  An inline
   element's text may sit in fragments attributed to a deeper inline (<a><b>x</b></a>
   gives <b>'s fragment), so the <a>'s rectangle has to include its descendants'."
  (let ((fn (r:frag-node frag)))
    (loop for n = fn then (h:dnode-parent n)
          while n
          when (eq n node) do (return t))))

(defun node-document-rects (ctx node)
  "Every rectangle NODE occupies, in DOCUMENT coordinates, as a list of
   (x y w h).  Empty when the node was not laid out."
  (let ((root (ensure-layout ctx))
        (rects '()))
    (labels ((walk (b)
               (when (r:lbox-p b)
                 (when (eq (r:lbox-node b) node)
                   (push (list (r:lbox-x b) (r:lbox-y b) (r:lbox-w b) (r:lbox-h b))
                         rects))
                 (if (eq (r:lbox-kind b) :line)
                     ;; A LINE BOX'S CHILDREN ARE MIXED: text fragments, and the
                     ;; LBOXes of atomic inlines (inline-block, <img>, a form
                     ;; control), which sit on the line as boxes in their own
                     ;; right.  Walking only the fragments reported a zero rect for
                     ;; every inline-block on the page.
                     (dolist (c (r:lbox-children b))
                       (cond ((r:lbox-p c) (walk c))
                             ((and (r:frag-p c) (r:frag-node c)
                                   (%frag-belongs-to-p c node))
                              (push (list (+ (r:frag-x c) (r:frag-dx c))
                                          (+ (r:lbox-y b) (r:frag-dy c))
                                          (r:frag-w c)
                                          (r:lbox-h b))
                                    rects))))
                     (dolist (c (r:lbox-children b)) (walk c))))))
      (when root (walk root)))
    (nreverse rects)))

(defun node-bounding-rect (ctx node)
  "NODE's border-box union in VIEWPORT coordinates as (values x y w h), or
   (values 0 0 0 0) when it has no box -- which is the spec's answer for a node
   that is not rendered, and the only zero rect this file will produce."
  (let ((rects (node-document-rects ctx node)))
    (if (null rects)
        (values 0 0 0 0)
        (let ((x0 (reduce #'min rects :key #'first))
              (y0 (reduce #'min rects :key #'second))
              (x1 (reduce #'max rects :key (lambda (r) (+ (first r) (third r)))))
              (y1 (reduce #'max rects :key (lambda (r) (+ (second r) (fourth r))))))
          (values x0 (- y0 (context-scroll-y ctx)) (- x1 x0) (- y1 y0))))))

(defun make-dom-rect (ctx x y w h)
  "A DOMRect: x/y/width/height plus the four derived edges, as
   getBoundingClientRect/getClientRects return them."
  (let* ((realm (context-realm ctx))
         (o (js:make-object :proto (js:eval-script realm "Object.prototype"))))
    (flet ((n (k v) (js:put o k (num v))))
      (n "x" x) (n "y" y) (n "width" w) (n "height" h)
      (n "top" y) (n "left" x)
      (n "right" (+ x w)) (n "bottom" (+ y h)))
    o))

(defun make-rect-list (ctx rects)
  "The DOMRectList getClientRects returns: one DOMRect per box, in an array whose
   `length' follows from the index writes (shuttle's arrays maintain it)."
  (let ((arr (js:eval-script (context-realm ctx) "[]")))
    (loop for r in rects for i from 0
          do (js:put arr (princ-to-string i)
                     (make-dom-rect ctx (first r) (second r) (third r) (fourth r))))
    ;; JS:PUT is the raw property setter -- it does NOT run the array exotic
    ;; [[Set]], so `length` does not follow the index writes and has to be set.
    (js:put arr "length" (num (length rects)))
    arr))
