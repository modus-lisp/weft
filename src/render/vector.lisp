;;;; src/render/vector.lisp — compositing SVG and <canvas> into the page.
;;;;
;;;; Replaced vector content is drawn through gesso onto a scribe canvas and
;;;; composited into the page bitmap at the element's box:
;;;;   * inline <svg> is converted to a stencil SVG DOM and rendered at paint
;;;;     time over a copy of the backdrop, so anti-aliased edges blend against
;;;;     whatever sits behind the element;
;;;;   * <canvas> owns a persistent gesso-backed scribe buffer that page script
;;;;     draws into (via getContext('2d')); the buffer is blitted at paint.
(in-package #:weft.render)

;;; ---- <canvas> element buffers ---------------------------------------------
(defvar *element-canvas* (make-hash-table :test 'eq)
  "Maps a <canvas> weft dnode to its scribe canvas buffer.  The buffer is created
by the scripting seam (getContext('2d')) and drawn into by page JS; layout blits
it into the page.  Bound fresh per render so buffers never leak across pages.")

(defun element-canvas (node) (gethash node *element-canvas*))
(defun (setf element-canvas) (v node) (setf (gethash node *element-canvas*) v))

;;; ---- weft DOM subtree -> stencil SVG DOM ----------------------------------
(defun dnode->svg-node (dn)
  "Convert a weft <svg> element subtree DN into a stencil SVG DOM node, so the
stencil renderer can paint it.  Element children recurse; character-data children
accumulate into the node's text (for <text>/<tspan>)."
  (let ((sn (st:make-svg-node (h:dnode-name dn)
                              (mapcar (lambda (a) (cons (string-downcase (car a)) (cdr a)))
                                      (h:dnode-attrs dn))))
        (txt nil))
    (loop for c across (h:dnode-children dn) do
      (case (h:dnode-kind c)
        (:element (st:append-child sn (dnode->svg-node c)))
        (:text (setf txt (concatenate 'string (or txt "") (or (h:dnode-data c) ""))))))
    (when txt (setf (st:svg-node-text sn) txt))
    sn))

;;; ---- scribe <-> page-canvas pixel bridging --------------------------------
(defun %copy-region-to-scribe (cv x y w h)
  "A fresh scribe canvas W x H holding CV's current pixels at (X,Y) — the backdrop
a replaced element composites over.  Out-of-bounds samples read white."
  (let* ((sc (sc:make-canvas (max 1 w) (max 1 h) '(255 255 255)))
         (sp (sc:canvas-pixels sc)) (dp (canvas-pixels cv))
         (dw (canvas-width cv)) (dh (canvas-height cv)))
    (dotimes (row h)
      (let ((sy (+ y row)))
        (when (and (>= sy 0) (< sy dh))
          (dotimes (col w)
            (let ((sx (+ x col)))
              (when (and (>= sx 0) (< sx dw))
                (let ((si (* 3 (+ (* row w) col))) (di (* 3 (+ (* sy dw) sx))))
                  (setf (aref sp si)       (aref dp di)
                        (aref sp (+ si 1)) (aref dp (+ di 1))
                        (aref sp (+ si 2)) (aref dp (+ di 2))))))))))
    sc))

(defun %blit-scribe (cv sc x y)
  "Copy scribe canvas SC opaquely onto page canvas CV at (X,Y), honoring *CLIP*."
  (let ((sp (sc:canvas-pixels sc)) (sw (sc:canvas-width sc)) (sh (sc:canvas-height sc)))
    (dotimes (row sh)
      (dotimes (col sw)
        (let ((si (* 3 (+ (* row sw) col))))
          (put cv (+ x col) (+ y row) (aref sp si) (aref sp (+ si 1)) (aref sp (+ si 2))))))))

;;; ---- paint entry points (called from %PAINT-BOX via LBOX-VPAINT) ----------
(defun paint-svg-box (cv x y w h root)
  "Render stencil SVG DOM ROOT at box (X,Y,W,H) over a copy of the backdrop and
composite it back — so transparent regions and anti-aliased edges blend with the
page underneath."
  (when (and root (plusp w) (plusp h))
    (let ((sc (%copy-region-to-scribe cv x y w h)))
      (ignore-errors (st:render-svg-to-canvas root :width w :height h :canvas sc))
      (%blit-scribe cv sc x y))))

(defun paint-canvas-box (cv x y node)
  "Composite a <canvas> element's scribe buffer onto the page at (X,Y).  An RGBA
buffer composites by alpha (so transparent/cleared areas show the page through); an
opaque buffer is copied."
  (let ((sc (element-canvas node)))
    (when sc
      (if (sc:canvas-alpha sc)
          (blit-img cv (rgba-canvas->img sc) x y)
          (%blit-scribe cv sc x y)))))
