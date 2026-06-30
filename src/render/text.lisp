;;;; src/render/text.lisp — real shaped, anti-aliased glyphs via scribe.
;;;;
;;;; Phase A: paint quality only.  Layout still computes all glyph positions with
;;;; the old fixed-metric math; here we just draw prettier glyphs at those spots
;;;; using scribe's analytic rasterizer + gamma-correct compositing.
;;;;
;;;; Everything here is paint code: a font-load failure, a missing glyph, or any
;;;; error degrades to the bitmap DRAW-TEXT path — it must never crash a render.
(in-package #:weft.render)

(defvar *scribe-font* :unset
  "Cached scribe font object, NIL if loading failed, :UNSET before first try.")
(defvar *scribe-ascent-ratio* 0.8d0
  "Cached ascender / units-per-em — baseline = text-top + ratio*ppem.")
(defvar *scribe-descent-ratio* 0.2d0)

(defun scribe-font ()
  "Lazily load and cache the vendored DejaVu Sans font.  Returns the scribe font,
or NIL if it could not be loaded (callers then fall back to the bitmap path)."
  (when (eq *scribe-font* :unset)
    (setf *scribe-font*
          (handler-case
              (let* ((path (asdf:system-relative-pathname
                            "weft" "src/render/fonts/DejaVuSans.ttf"))
                     (bytes (with-open-file (s path :element-type '(unsigned-byte 8))
                              (let ((v (make-array (file-length s)
                                                   :element-type '(unsigned-byte 8))))
                                (read-sequence v s) v)))
                     (font (scribe:open-font bytes))
                     (upem (float (scribe:font-units-per-em font) 1d0))
                     (hhea (scribe::parse-hhea font))
                     (asc (cdr (assoc "ascent" hhea :test #'string=)))
                     (desc (cdr (assoc "descent" hhea :test #'string=))))
                (when (and asc (plusp upem))
                  (setf *scribe-ascent-ratio* (/ asc upem)))
                (when (and desc (plusp upem))
                  (setf *scribe-descent-ratio* (/ (abs desc) upem)))
                font)
            (error () nil))))
  *scribe-font*)

(defun draw-text-scribe (cv text x y color size &key bold underline)
  "Paint TEXT with scribe glyphs.  X/Y is the top-left of the text within its line
box (same contract as the bitmap DRAW-TEXT); SIZE is the px font-size used as the
rasterization ppem.  Returns the advance (pen x at end).

Degrades to the bitmap DRAW-TEXT for the whole string if the font is unavailable
or anything goes wrong; respects weft's *CLIP* rect per pixel."
  (let ((font (scribe-font)))
    (if (or (null font) (zerop (length text)))
        (draw-text cv text x y color :bold bold :underline underline)
        (handler-case
            (let* ((ppem (float (max 1 size) 1d0))
                   (baseline (+ y (round (* *scribe-ascent-ratio* ppem))))
                   ;; zero-copy view of weft's pixel buffer as a scribe canvas
                   (scv (scribe::%make-canvas :width (canvas-width cv)
                                              :height (canvas-height cv)
                                              :pixels (canvas-pixels cv)))
                   (scribe::*stem-darkening* (if bold 0.6d0 scribe::*stem-darkening*))
                   ;; clip bounds (or full canvas)
                   (cx0 (if *clip* (the fixnum (first *clip*)) 0))
                   (cy0 (if *clip* (the fixnum (second *clip*)) 0))
                   (cx1 (if *clip* (the fixnum (third *clip*)) (canvas-width cv)))
                   (cy1 (if *clip* (the fixnum (fourth *clip*)) (canvas-height cv)))
                   (penx (float x 1d0)))
              (loop for ch across text do
                (let* ((gid (scribe:font-glyph-index font (char-code ch)))
                       (sub (- penx (ffloor penx))))
                  ;; gid 0 = .notdef: don't draw a tofu box, but still advance by a
                  ;; space so the rest of the run keeps roughly its layout position.
                  (if (zerop gid)
                      (incf penx (* 0.5d0 ppem))
                      (multiple-value-bind (cov w h left top adv)
                          (scribe:rasterize-glyph font gid ppem :subpixel sub)
                        (when cov
                          (let ((ox (+ (floor penx) left)) (oy (+ baseline top)))
                            (dotimes (yy h)
                              (let ((py (+ oy yy)))
                                (when (and (>= py cy0) (< py cy1))
                                  (dotimes (xx w)
                                    (let ((px (+ ox xx)))
                                      (when (and (>= px cx0) (< px cx1))
                                        (let ((c (aref cov (+ (* yy w) xx))))
                                          (when (> c 0d0)
                                            (scribe:blend-coverage scv px py c color)))))))))))
                        (incf penx adv)))))
              (when underline
                (fill-rect cv x (+ y *font-h* -1) (- (round penx) x) 1 color))
              (round penx))
          (error ()
            ;; partial paint may have happened; finish the string with the bitmap
            ;; path so the user at least sees the text.
            (draw-text cv text x y color :bold bold :underline underline))))))
