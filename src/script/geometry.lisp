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

;;; ===========================================================================
;;; The offset* / client* family (CSSOM View)
;;; ===========================================================================
;;; These are the older, integer-rounded way to ask the same layout questions, and
;;; they are still what a great deal of real code uses -- jQuery's .width(), any
;;; hand-rolled positioning.  Each is a different BOX of the same element, so the
;;; distinctions matter and are not interchangeable:
;;;
;;;   offsetWidth/Height   the BORDER box, rounded.  An LBOX already is the border
;;;                        box (a 200px-wide div with 5px padding and a 3px border
;;;                        lays out 216 wide), so these read straight off it.
;;;   clientWidth/Height   the PADDING box: border box less the two border widths.
;;;                        Zero for an element with no CSS box.
;;;   clientTop/Left       the top and left BORDER widths themselves.
;;;   offsetTop/Left       the element's border-box corner measured from the
;;;                        offsetParent's PADDING edge -- so the parent's border is
;;;                        subtracted, which is what makes it different from simply
;;;                        differencing two getBoundingClientRect calls.

(defun %node-lbox (ctx node)
  "NODE's principal layout box, or NIL when it has none.  Descends through line
   boxes too, since an atomic inline (inline-block, <img>, a form control) is a box
   sitting on a line rather than a child of a block."
  (let ((root (ensure-layout ctx)) (found nil))
    (labels ((walk (b)
               (when (and (null found) (r:lbox-p b))
                 (if (eq (r:lbox-node b) node)
                     (setf found b)
                     (dolist (c (r:lbox-children b)) (walk c))))))
      (when root (walk root)))
    found))

(defun %border-widths (box)
  "(values top right bottom left) border widths of BOX, as integers."
  (let ((cs (and box (r:lbox-style box))))
    (if (null cs)
        (values 0 0 0 0)
        (values (round (css:cstyle-border-top-width cs))
                (round (css:cstyle-border-right-width cs))
                (round (css:cstyle-border-bottom-width cs))
                (round (css:cstyle-border-left-width cs))))))

(defun %offset-parent-node (ctx node)
  "The element NODE's offsets are measured against: the nearest ANCESTOR that is
   positioned, or a table cell or table, else the body.  NIL when NODE is the root,
   is not rendered, or is itself fixed -- in which case offsets are measured from
   the document origin."
  (when (member (%tag-name node) '("html" "body") :test #'string=)
    (return-from %offset-parent-node nil))
  (let ((own (%node-lbox ctx node)))
    (when (and own (r:lbox-style own)
               (string-equal (or (css:cstyle-position (r:lbox-style own)) "static") "fixed"))
      (return-from %offset-parent-node nil))
    (loop for n = (h:dnode-parent node) then (h:dnode-parent n)
          while n
          when (eq (h:dnode-kind n) :element)
            do (let* ((tag (string-downcase (or (h:dnode-name n) "")))
                      (box (%node-lbox ctx n))
                      (pos (and box (r:lbox-style box)
                                (or (css:cstyle-position (r:lbox-style box)) "static"))))
                 (when (and box
                            (or (and pos (not (string-equal pos "static")))
                                (member tag '("td" "th" "table" "body") :test #'string=)))
                   (return n))))))

(defun %tag-name (node)
  (string-downcase (or (and (eq (h:dnode-kind node) :element) (h:dnode-name node)) "")))

(defun %static-p (box)
  (let ((cs (and box (r:lbox-style box))))
    (or (null cs) (string-equal (or (css:cstyle-position cs) "static") "static"))))

(defun node-offset-metrics (ctx node)
  "(values offset-left offset-top offset-width offset-height) for NODE, all
   integers, and all zero when NODE has no layout box.

   THE ORIGIN IS NOT ALWAYS THE OFFSETPARENT, which is the part that surprises:
     * the root element and the body report 0, whatever their boxes say -- body's
       border box sits at y=10 on the corpus's first page and its offsetTop is
       still 0;
     * a STATICALLY positioned element whose offsetParent is the BODY is measured
       from the initial containing block, i.e. in document coordinates, NOT from
       the body's padding edge (CSSOM View).  Measuring from the body instead was
       wrong for every ordinary element on an ordinary page -- 14 of 45 in the
       corpus -- and wrong by exactly the body's own offset, which is the kind of
       error that looks like a rounding problem until it is written down;
     * otherwise the origin IS the offsetParent's padding edge: its border box
       corner plus its own top/left border widths."
  (let ((box (%node-lbox ctx node)))
    (cond
      ((null box) (values 0 0 0 0))
      ((member (%tag-name node) '("html" "body") :test #'string=)
       (values 0 0 (round (r:lbox-w box)) (round (r:lbox-h box))))
      (t
       (let* ((parent (%offset-parent-node ctx node))
              (pbox (and parent (%node-lbox ctx parent)))
              (from-icb (or (null pbox)
                            (and (string= (%tag-name parent) "body") (%static-p box)))))
         (multiple-value-bind (pt pr pb pl) (%border-widths pbox)
           (declare (ignore pr pb))
           (let ((ox (if from-icb 0 (+ (r:lbox-x pbox) pl)))
                 (oy (if from-icb 0 (+ (r:lbox-y pbox) pt))))
             (values (round (- (r:lbox-x box) ox))
                     (round (- (r:lbox-y box) oy))
                     (round (r:lbox-w box))
                     (round (r:lbox-h box))))))))))

(defun node-client-metrics (ctx node)
  "(values client-left client-top client-width client-height) for NODE: the border
   widths, and the padding box's size.

   THE ROOT ELEMENT ANSWERS WITH THE VIEWPORT, not with its own box -- that is what
   document.documentElement.clientHeight means, and it is how a page asks how tall
   the window is.  We answer it only when the shell has told us the viewport
   height; with no shell there is no window to report, and the element's own
   padding box is the honest fallback rather than a made-up number."
  (let ((box (%node-lbox ctx node)))
    (cond
      ((null box) (values 0 0 0 0))
      ((and (string= (%tag-name node) "html") (context-viewport-height ctx))
       (values 0 0 (round (context-width ctx)) (round (context-viewport-height ctx))))
      (t
       (multiple-value-bind (bt br bb bl) (%border-widths box)
         (values bl bt
                 (max 0 (round (- (r:lbox-w box) bl br)))
                 (max 0 (round (- (r:lbox-h box) bt bb)))))))))

;;; ---- the scrolling area ----------------------------------------------------
;;; scrollWidth/scrollHeight are the size of what an element COULD scroll over:
;;; its padding box, enlarged to contain every descendant box that overflows it.
;;; So they equal clientWidth/clientHeight exactly when nothing overflows, which is
;;; the common case and the one that must not drift from the client* answer.
;;;
;;; scrollTop/scrollLeft are a POSITION, not a size, and weft has no per-element
;;; scroll containers -- it lays a page out whole and the shell blits a slice.  So
;;; an ordinary element is never scrolled and answers 0, while the ROOT element
;;; reports the shell's own scroll, which is the document scroll a page reads as
;;; document.documentElement.scrollTop.

(defun %subtree-extent (box right bottom)
  "(values right bottom) extended to cover everything laid out inside BOX, in
   document coordinates -- the far corner of its scrolling area.

   THE SEED IS THE PADDING BOX, not the border box, and the caller supplies it: the
   scrolling area is the union of an element's PADDING box with its descendants'
   border boxes, so seeding with the border box made every bordered element's
   scrollWidth/scrollHeight too large by exactly one border width -- 213 against
   Chrome's 210 on a 3px border, 322 against 320 on a 2px one.  Off by a border is
   the kind of wrong that looks like rounding."
  (let ((right right) (bottom bottom))
    (labels ((walk (b)
               (when (r:lbox-p b)
                 (setf right (max right (+ (r:lbox-x b) (r:lbox-w b)))
                       bottom (max bottom (+ (r:lbox-y b) (r:lbox-h b))))
                 (dolist (c (r:lbox-children b)) (walk c)))))
      (dolist (c (r:lbox-children box)) (walk c)))
    (values right bottom)))

(defun node-scroll-metrics (ctx node)
  "(values scroll-left scroll-top scroll-width scroll-height) for NODE."
  (let ((box (%node-lbox ctx node)))
    (if (null box)
        (values 0 0 0 0)
        (multiple-value-bind (bt br bb bl) (%border-widths box)
          (declare (ignorable bb))
          (multiple-value-bind (right bottom)
              (%subtree-extent box
                               (- (+ (r:lbox-x box) (r:lbox-w box)) br)
                               (- (+ (r:lbox-y box) (r:lbox-h box))
                                  (nth-value 2 (%border-widths box))))
            (let* ((root-p (string= (%tag-name node) "html"))
                   ;; the padding edge is where a scrolling area starts
                   (px (+ (r:lbox-x box) bl))
                   (py (+ (r:lbox-y box) bt))
                   (cw (max 0 (round (- (r:lbox-w box) bl br))))
                   (ch (nth-value 3 (node-client-metrics ctx node))))
              (values 0
                      (if root-p (round (context-scroll-y ctx)) 0)
                      (max cw (round (- right px)))
                      (max ch (round (- bottom py))))))))))

;;; ===========================================================================
;;; Asking the shell to scroll
;;; ===========================================================================
;;; scrollIntoView and window.scrollTo are the one direction the script layer has
;;; to push rather than read: everything else here answers a question about the
;;; layout, while these ask for the viewport to MOVE.  Only the shell can do that
;;; -- it owns the scroll position and the painting -- so the context carries a
;;; hook it installs, and the fallback when there is no shell is to move our own
;;; recorded scroll so that every rectangle read afterwards is consistent with the
;;; scroll that was requested.  A scrollIntoView that silently did nothing would
;;; leave a page measuring against a viewport it believes it moved.

(defun request-scroll (ctx y)
  "Ask for the viewport's top to be at document Y.  Returns the Y adopted."
  (let ((y (max 0 (round y))))
    (if (context-scroll-fn ctx)
        (funcall (context-scroll-fn ctx) y)
        (setf (context-scroll-y ctx) y))))

(defun scroll-node-into-view (ctx node block)
  "Scroll so NODE is visible.  BLOCK is :start, :end, :center or :nearest, the
   CSSOM View alignments -- :nearest moves the minimum needed and is the only one
   that may decide to do nothing at all."
  (let ((box (%node-lbox ctx node)))
    (when box
      (let* ((top (r:lbox-y box))
             (h (r:lbox-h box))
             (bottom (+ top h))
             (vh (or (context-viewport-height ctx)
                     (let ((root (ensure-layout ctx))) (if root (r:lbox-h root) 0))))
             (cur (context-scroll-y ctx)))
        (request-scroll
         ctx
         (ecase block
           (:start top)
           (:end (- bottom vh))
           (:center (- top (/ (- vh h) 2)))
           (:nearest (cond ((< top cur) top)                    ; above the fold
                           ((> bottom (+ cur vh)) (- bottom vh)) ; below it
                           (t cur)))))))))                       ; already visible
