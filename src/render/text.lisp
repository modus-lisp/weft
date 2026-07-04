;;;; src/render/text.lisp — real shaped, anti-aliased glyphs via scribe.
;;;;
;;;; Text is measured and painted with the concrete face scribe's MATCH-FONT
;;;; resolves for the fragment's (font-family, weight, style): a page that asks
;;;; for Arial/Helvetica/Verdana gets LiberationSans (the browser's Linux
;;;; metric-compatible substitute), Times -> LiberationSerif, Courier/monospace
;;;; -> LiberationMono.  This makes weft's text widths match a real browser.
;;;;
;;;; Everything here is resilient: a font-load failure, a missing glyph, or any
;;;; error degrades to the bitmap DRAW-TEXT path or the bitmap metric — it must
;;;; never crash a render or a layout.
(in-package #:weft.render)

;;; ---- resolved faces -------------------------------------------------------
;;; A FACE bundles the open scribe font with its ascent/descent ratios (a
;;; fraction of the em), so the painter can position the baseline and layout can
;;; derive line metrics.  Faces are resolved once per (family weight style) key
;;; and cached; MATCH-FONT ships the Liberation faces so resolution effectively
;;; never fails, but every entry point still tolerates a NIL face.

(defstruct (face (:constructor %make-face))
  font                     ; scribe open-font
  (ascent-ratio 0.8d0)     ; hhea ascent / units-per-em
  (descent-ratio 0.2d0)    ; |hhea descent| / units-per-em
  (line-gap-ratio 0.0d0)   ; hhea line-gap / units-per-em
  (upem 1000d0))           ; units-per-em

(defun face-normal-lh-factor (face)
  "The `line-height: normal` multiplier for FACE: the font's own line spacing,
(ascent + |descent| + line-gap) / units-per-em (hhea metrics, what a browser uses
for `normal`).  ~1.15 for Liberation Sans, vs the flat 1.2 fallback for no face."
  (if face
      (let ((f (+ (face-ascent-ratio face) (face-descent-ratio face)
                  (face-line-gap-ratio face))))
        (if (plusp f) f 1.2d0))
      1.2d0))

(defvar *face-cache* (make-hash-table :test 'equal)
  "Cache of (family-list weight style-kw) -> FACE (or :FAILED).")

(defun %build-face (family-list weight style-kw)
  "Resolve a FACE via scribe:match-font, or NIL on any failure."
  (handler-case
      (let* ((font (scribe:match-font family-list :weight weight :style style-kw))
             (upem (float (scribe:font-units-per-em font) 1d0))
             (asc (scribe:font-ascent font))
             (desc (scribe:font-descent font))
             (gap (scribe:font-line-gap font))
             (f (%make-face :font font :upem (if (plusp upem) upem 1000d0))))
        (when (and asc (plusp upem)) (setf (face-ascent-ratio f) (/ asc upem)))
        (when (and desc (plusp upem)) (setf (face-descent-ratio f) (/ (abs desc) upem)))
        (when (and gap (plusp upem)) (setf (face-line-gap-ratio f) (/ (abs gap) upem)))
        f)
    (error () nil)))

(defun resolve-face (family-list weight style-kw)
  "Return the cached FACE for (FAMILY-LIST WEIGHT STYLE-KW), resolving+caching on
first use.  Returns NIL if the face could not be resolved (callers fall back to
the bitmap path)."
  (let ((key (list family-list weight style-kw)))
    (multiple-value-bind (v present) (gethash key *face-cache*)
      (if present
          (unless (eq v :failed) v)
          (let ((f (%build-face family-list weight style-kw)))
            (setf (gethash key *face-cache*) (or f :failed))
            f)))))

(defun style-face (style)
  "Resolve the FACE for a computed STYLE (its font-family/weight/style).  A NIL
style (or one with no font-family) resolves the generic sans-serif face."
  (let* ((family (and style (css:cstyle-font-family style)))
         (weight (if style (css:cstyle-font-weight style) 400))
         (sstr (and style (css:cstyle-font-style style)))
         (style-kw (if (and sstr (member sstr '("italic" "oblique") :test #'string=))
                       :italic :normal)))
    (resolve-face family (if (integerp weight) weight 400) style-kw)))

;;; The generic default face (sans-serif) — used when no face/style is supplied.
(defun default-face () (resolve-face nil 400 :normal))

;;; ---- hinting --------------------------------------------------------------
(defparameter *hint-glyphs* t
  "Grid-fit glyphs via scribe's TrueType bytecode hinting before rasterizing, so
small body text (stems, baseline, x-height) lands on pixel boundaries instead of
smearing across fractional rows.  Applies to both painting and measurement (they
share scribe's grid-fit integer advance, so widths stay consistent).")
(defparameter *hint-max-ppem* 24
  "Only hint at rounded ppem <= this — large text is already crisp geometrically
and skips the interpreter cost.  Covers body copy and small headings.")
(defparameter *hint-light* t
  "Light (Y-only) hinting: grid-fit vertically (crisp baseline/x-height/stem heights)
but keep the fractional X, so horizontal stem rhythm stays smooth instead of full
hinting's crisp-but-choppy X grid-fit.  Matches the browser's default Linux rendering
and halves the per-pair width error vs Chromium.  NIL = full (crispest, choppier).")

;;; ---- measurement ----------------------------------------------------------
(defun glyph-advance-px (font gid ppem upem)
  "Advance in px for GID at PPEM — the SAME value the painter advances the pen
by, so measured widths match painted widths to the pixel.  .notdef (gid 0)
advances half an em, matching the painter's tofu-suppression.  Uses the geometric
(fractional) advance whether or not the glyph is hinted: hinting grid-fits the
outline but keeps sub-pixel horizontal positioning, so spacing stays smooth."
  (if (zerop gid)
      (* 0.5d0 ppem)
      (* (scribe::glyph-advance font gid) (/ ppem upem))))

(defvar *shape-cache* (make-hash-table :test 'equal)
  "Memo of (font text ppem) -> shaped px glyph list; a page repeats many words
(\"points\", \"by\", \"hours\", \"comments\"), so shape each once.")
(defparameter *shape-cache-limit* 50000
  "Bound on *SHAPE-CACHE* — keyed by every unique word×size, it would otherwise grow
without limit across a long browsing session.  When exceeded the cache is dropped
(entries are pure memoization: cheaply recomputed on next use).")

(defun shape-px (font text ppem upem)
  "Shape TEXT with scribe (GPOS kerning + GSUB ligatures) into a list of
(GID XADV XOFF YOFF), metrics in px (memoized).  Shared by MEASURE-TEXT-WIDTH and
DRAW-TEXT-SCRIBE so the kerned advances are identical (measure = paint).  A .notdef
(gid 0) advances half an em (tofu suppression)."
  (let ((key (list font text ppem)))
    (or (gethash key *shape-cache*)
        (progn
          (when (> (hash-table-count *shape-cache*) *shape-cache-limit*)
            (clrhash *shape-cache*))
        (setf (gethash key *shape-cache*)
              (let ((s (/ ppem upem)))
                (loop for g across (scribe:shape-run font text)
                      for gid = (scribe::glyph-pos-gid g)
                      collect (list gid
                                    (if (zerop gid) (* 0.5d0 ppem) (* (scribe::glyph-pos-x-advance g) s))
                                    (* (scribe::glyph-pos-x-offset g) s)
                                    (* (scribe::glyph-pos-y-offset g) s)))))))))

(defun measure-text-width (text size &optional face)
  "Width of the shaped (kerned) TEXT at px SIZE in FACE — the width scribe will
actually paint.  FACE defaults to the generic sans-serif face.  Falls back to
the bitmap metric (LEN*7) if no face is available or anything errors, so layout
never depends on the font succeeding."
  (let ((face (or face (default-face))))
    (if (or (null face) (zerop (length text)))
        (* (length text) *font-w*)
        (handler-case
            (let* ((font (face-font face))
                   (ppem (float (max 1 size) 1d0))
                   (upem (face-upem face))
                   (w 0d0))
              (dolist (g (shape-px font text ppem upem)) (incf w (second g)))
              w)
          (error () (* (length text) *font-w*))))))

(defun bitmap-top (line-top line-h)
  "Where the bitmap DRAW-TEXT path centers its fixed *FONT-H* slot in a line box."
  (+ line-top (max 0 (floor (- line-h *font-h*) 2))))

(defparameter *blend-gamma* 0.95d0
  "Gamma space scribe composites glyphs in (bound to scribe's *BLEND-GAMMA*).
Linear-light AA (scribe's image default, ~2.2) renders dark-on-light text
washed-out; a lower gamma is heavier.  Calibrated to Chromium's text weight by a
total-ink match on an Arial/Liberation sample (both resolve to Liberation Sans, so
only the AA/weight differs): 0.95 lands within ~2% of Chromium's ink.  This is much
heavier than the old 1.5 — that value had to compensate for the stem hinting weft
*couldn't* do; now that hinting grid-fits stems solid like the browser (see
*HINT-GLYPHS*), the honest match is ~0.95.")
(defparameter *stem-darkening* 1.0d0
  "Extra coverage-exponent darkening on top of *BLEND-GAMMA* (1.0 = none).  The
gamma carries the weight match; kept as a separate knob.")

(defun draw-text-scribe (cv text x line-top line-h color size
                         &key bold underline face)
  "Paint TEXT with scribe glyphs from FACE (defaulting to the generic sans-serif
face).  X is the left edge; LINE-TOP/LINE-H are the line box's top and height —
the real font em-box (ascent+descent at SIZE px) is centered within it, so large
text lands correctly instead of being pinned to a 13px slot.  SIZE is the px
font-size used as the rasterization ppem.  Returns the pen x at end.

Degrades to the bitmap DRAW-TEXT for the whole string if no face is available or
anything goes wrong; respects weft's *CLIP* rect per pixel."
  (let ((face (or face (default-face))))
    (if (or (null face) (zerop (length text)))
        (draw-text cv text x (bitmap-top line-top line-h) color :bold bold :underline underline)
        (handler-case
            (let* ((font (face-font face))
                   (ppem (float (max 1 size) 1d0))
                   (asc-ratio (face-ascent-ratio face))
                   (desc-ratio (face-descent-ratio face))
                   (upem (face-upem face))
                   ;; center the font's em-box (ascent+descent) in the line box,
                   ;; then the baseline sits ascent px below the box's top.
                   (text-h (* (+ asc-ratio desc-ratio) ppem))
                   (baseline (round (+ line-top (/ (- line-h text-h) 2)
                                       (* asc-ratio ppem))))
                   ;; zero-copy view of weft's pixel buffer as a scribe canvas
                   (scv (scribe::%make-canvas :width (canvas-width cv)
                                              :height (canvas-height cv)
                                              :pixels (canvas-pixels cv)))
                   ;; Blend glyphs in an intermediate gamma space, not linear
                   ;; light — browsers blend text this way; linear-light AA leaves
                   ;; edges washed-out.  (BOLD is only consulted by the bitmap
                   ;; fallback below.)
                   (scribe::*blend-gamma* *blend-gamma*)
                   (scribe::*stem-darkening* *stem-darkening*)
                   ;; grid-fit small glyphs so stems/baseline are crisp; light (Y-only)
                   ;; keeps horizontal spacing smooth (see *HINT-LIGHT*).
                   (scribe::*hinting* (and *hint-glyphs* *hint-max-ppem*))
                   (scribe::*hint-light* *hint-light*)
                   ;; clip bounds (or full canvas)
                   (cx0 (if *clip* (the fixnum (first *clip*)) 0))
                   (cy0 (if *clip* (the fixnum (second *clip*)) 0))
                   (cx1 (if *clip* (the fixnum (third *clip*)) (canvas-width cv)))
                   (cy1 (if *clip* (the fixnum (fourth *clip*)) (canvas-height cv)))
                   (penx (float x 1d0)))
              (loop for (gid xadv xoff yoff) in (shape-px font text ppem upem) do
                (let ((sub (- (+ penx xoff) (ffloor (+ penx xoff)))))
                  ;; gid 0 = .notdef: don't draw a tofu box, but still advance so the
                  ;; run keeps its layout position.  Advance by the SHAPED (kerned)
                  ;; x-advance so painting matches MEASURE-TEXT-WIDTH to the pixel.
                  (if (zerop gid)
                      (incf penx xadv)
                      (multiple-value-bind (cov w h left top adv)
                          (scribe:rasterize-glyph font gid ppem :subpixel sub)
                        (declare (ignore adv))
                        (when cov
                          (let ((ox (+ (floor (+ penx xoff)) left)) (oy (+ baseline top (- (round yoff)))))
                            (dotimes (yy h)
                              (let ((py (+ oy yy)))
                                (when (and (>= py cy0) (< py cy1))
                                  (dotimes (xx w)
                                    (let ((px (+ ox xx)))
                                      (when (and (>= px cx0) (< px cx1))
                                        (let ((c (aref cov (+ (* yy w) xx))))
                                          (when (> c 0d0)
                                            (scribe:blend-coverage scv px py c color)))))))))))
                        (incf penx xadv)))))
              (when underline
                (let ((uy (min (1- cy1) (+ baseline (max 1 (round (* 0.12d0 ppem)))))))
                  (fill-rect cv x uy (- (round penx) x) (max 1 (round (* 0.06d0 ppem))) color)))
              (round penx))
          (error ()
            ;; partial paint may have happened; finish the string with the bitmap
            ;; path so the user at least sees the text.
            (draw-text cv text x (bitmap-top line-top line-h) color :bold bold :underline underline))))))
