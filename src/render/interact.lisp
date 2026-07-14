;;;; src/render/interact.lisp — interactive-shell support: hit-testing and a
;;;; box-tree-exposing render entry point, plus a numeric viewport scroll offset.
;;;;
;;;; These are real browser features (a page needs to know which element is under
;;;; the pointer, and a viewport scrolls), so they live in weft and stay pure CL.
;;;; They are additive: RENDER-TO-CANVAS is untouched, so the Acid gates that
;;;; pin its byte output do not move.  A shell (loom) drives these to route
;;;; pointer input into the DOM and to scroll the painted page.
(in-package #:weft.render)

(defvar *progress* nil
  "When bound to a function of (PHASE &optional DETAIL), RENDER-DOCUMENT reports
   its sub-phases — :cascade :layout :painting — so a host can surface live
   progress.  NIL (the default) disables reporting.")

(defun note-progress (phase &optional detail)
  "Call the progress hook if one is bound; never signals."
  (when *progress* (ignore-errors (funcall *progress* phase detail))))

;;; ---------------------------------------------------------------------------
;;; Hit-testing — document point (x,y) -> box -> DOM node
;;; ---------------------------------------------------------------------------
;;; Every lbox carries ABSOLUTE document coordinates (paint reads lbox-x/lbox-y
;;; directly and shift-box translates whole subtrees), so a point test is a
;;; direct rectangle containment in document space.  A caller in a scrolled
;;; viewport passes (mouse-x, mouse-y + scroll-y).

(defun point-in-box-p (b x y)
  "True when document point (X,Y) lies within box B's border box."
  (and (lbox-p b)
       (<= (lbox-x b) x (+ (lbox-x b) (lbox-w b)))
       (<= (lbox-y b) y (+ (lbox-y b) (lbox-h b)))))

(defun box-at (root x y)
  "The deepest lbox in the tree at ROOT whose border box contains document point
   (X,Y), or NIL.  Children are tested front-to-back (a later-painted child sits
   on top), so the returned box is the topmost at that point."
  (when (point-in-box-p root x y)
    (or (dolist (c (reverse (lbox-children root)))
          (when (lbox-p c)
            (let ((hit (box-at c x y))) (when hit (return hit)))))
        root)))

(defun node-at (root x y)
  "The DOM node (an h:dnode) of the deepest boxed element at document point
   (X,Y), or NIL.  Line boxes carry no node, so the point falls through to the
   enclosing block element — the element a browser reports as the event target."
  (labels ((frag-hit (b)
             ;; A line box holds positioned text frags; each frag carries the
             ;; inline element it came from (an <a>, <button>, …), so a click on
             ;; inline text resolves to that element, not the enclosing block.
             (when (eq (lbox-kind b) :line)
               (dolist (f (reverse (lbox-children b)))
                 (when (and (frag-p f) (frag-node f)
                            (<= (frag-x f) x (+ (frag-x f) (frag-w f)))
                            (<= (lbox-y b) y (+ (lbox-y b) (lbox-h b))))
                   (return (frag-node f))))))
           (rec (b)
             (when (point-in-box-p b x y)
               (or (dolist (c (reverse (lbox-children b)))
                     (when (lbox-p c)
                       (let ((hit (rec c))) (when hit (return hit)))))
                   (frag-hit b)
                   (lbox-node b)))))
    (rec root)))

;;; ---------------------------------------------------------------------------
;;; Render a parsed document, exposing the box tree
;;; ---------------------------------------------------------------------------
;;; RENDER-TO-CANVAS parses HTML from a string and throws the box tree away.  A
;;; shell holds a live (script-mutated) document across frames and needs both the
;;; canvas AND the box tree (to hit-test) plus the computed styles.  This mirrors
;;; RENDER-TO-CANVAS's layout+paint but takes an already-parsed DOC and returns
;;; (values canvas root-box styles).

(defun render-document (doc &key (width 1024) (css "") (min-height 200)
                                 (max-height 20000) viewport-height (scroll-y 0) scroll-to)
  "Cascade + lay out + paint an already-parsed DOC at WIDTH px.  Returns
   (values CANVAS ROOT-BOX STYLES).

   Two height models, chosen the same way as RENDER-TO-CANVAS so the reader-view
   service and the conformance harness agree:
   * Viewport model — when VIEWPORT-HEIGHT is set AND the root establishes overflow
     clipping (html/body {overflow:hidden|clip|scroll}), the canvas is a fixed
     VIEWPORT-HEIGHT rectangle, painting is clipped to it, position:fixed boxes land
     at viewport coordinates, and the page is scrolled so SCROLL-TO's fragment anchor
     sits at the top (how a browser tames Acid2's giant margins and composes the face).
   * Reader view — otherwise the canvas grows to content height (a shell scrolls by
     blitting a sub-rectangle); SCROLL-Y offsets it within MAX-HEIGHT."
  (let* ((css::*viewport-w* (float width))
         (css::*viewport-h* (float (or viewport-height 600)))
         (*element-canvas* (make-hash-table :test 'eq))
         (sheet (css:parse-stylesheet
                 (concatenate 'string (or css "") (string #\Newline)
                              (collect-stylesheets doc))))
         ;; download+register any @font-face web fonts before the cascade so a
         ;; page's own font resolves over the bundled fallback (no-op offline).
         (styles (progn (load-font-faces sheet)
                        (note-progress :cascade) (css:compute-styles doc sheet)))
         ;; a fixed-height viewport only when the page itself clips at the root —
         ;; a normal page keeps growing to content even with a viewport height given.
         (viewport-p (and viewport-height (root-clips-p doc styles)))
         (vph (and viewport-p (round viewport-height))))
    (note-progress :layout)
    (multiple-value-bind (root adv) (layout-tree doc styles width vph (and viewport-p scroll-to))
      (declare (ignore adv))
      (let* ((content-h (if root (round (+ (lbox-y root) (lbox-h root) 8)) min-height))
             (height (if vph vph (min max-height (max min-height content-h))))
             ;; reader-view only: clamp an extra scroll offset to the scrollable range
             (sy (if (and (not vph) (plusp scroll-y))
                     (max 0 (min (round scroll-y) (max 0 (- content-h height)))) 0))
             (body (css:query-select doc "body"))
             (bg (let ((cs (and body (gethash body styles))))
                   (and cs (css:cstyle-background cs))))
             (cv (make-canvas width height (if bg (rgb bg) '(255 255 255)))))
        (when (and root (plusp sy)) (shift-box root 0 (- sy)))
        (note-progress :painting)
        (if vph
            (let ((*clip* (clip-intersect 0 0 width vph))) (paint-box cv root))
            (paint-box cv root))
        (values cv root styles)))))
