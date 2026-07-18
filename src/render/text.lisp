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

(defun %face-from-font (font)
  "Wrap a scribe FONT in a weft FACE (upem + ascent/descent/line-gap ratios)."
  (let* ((upem (float (scribe:font-units-per-em font) 1d0))
         (asc (scribe:font-ascent font)) (desc (scribe:font-descent font)) (gap (scribe:font-line-gap font))
         (f (%make-face :font font :upem (if (plusp upem) upem 1000d0))))
    (when (and asc (plusp upem)) (setf (face-ascent-ratio f) (/ asc upem)))
    (when (and desc (plusp upem)) (setf (face-descent-ratio f) (/ (abs desc) upem)))
    (when (and gap (plusp upem)) (setf (face-line-gap-ratio f) (/ (abs gap) upem)))
    f))

(defvar *registered-faces* (make-hash-table :test 'equal)
  "(family-lc weight-class slant) -> FACE, from REGISTER-FONT: @font-face web fonts
   and the Ahem test font.  weight-class is :regular|:bold, slant :roman|:italic.
   Consulted before the generic Liberation substitutes, so a page's own web font
   wins over the metric-compatible fallback.")

(defun %weight-class (w) (if (and (integerp w) (>= w 600)) :bold :regular))
(defun %slant-kw (s) (if (member s '(:italic :oblique)) :italic :roman))

(defun register-font (family bytes &key (weight 400) (style :normal))
  "Load a font from BYTES (a TTF/OTF/WOFF/WOFF2 octet vector) and register it under
   FAMILY for the given WEIGHT/STYLE, so `font-family:FAMILY` resolves to it.  Returns
   the FACE, or NIL on failure."
  (handler-case
      (let ((f (%face-from-font (scribe:open-font bytes)))
            (key (list (string-downcase family) (%weight-class weight) (%slant-kw style))))
        (setf (gethash key *registered-faces*) f)
        (clrhash *face-cache*)          ; a newly-registered family may satisfy cached misses
        f)
    (error () nil)))

(defun %registered-face (family weight style-kw)
  "The registered FACE for FAMILY nearest the requested WEIGHT/STYLE-KW: exact
(weight-class, slant) first, then relaxing slant, then weight, then any registered
variant of the family.  NIL if the family was never registered."
  (let ((fam (string-downcase family)) (wc (%weight-class weight)) (sl (%slant-kw style-kw)))
    (or (gethash (list fam wc sl) *registered-faces*)
        (gethash (list fam wc :roman) *registered-faces*)
        (gethash (list fam :regular sl) *registered-faces*)
        (gethash (list fam :regular :roman) *registered-faces*)
        (loop for k being the hash-keys of *registered-faces* using (hash-value v)
              when (string= (first k) fam) return v))))

(defun %build-face (family-list weight style-kw)
  "Resolve a FACE: a registered (@font-face / Ahem) family first, else
   scribe:match-font.  NIL on any failure."
  (or (loop for fam in family-list
            for r = (%registered-face fam weight style-kw)
            when r return r)
      (handler-case (%face-from-font (scribe:match-font family-list :weight weight :style style-kw))
        (error () nil))))

;;; ---- @font-face web-font loading ------------------------------------------
(defvar *font-loader* nil
  "When bound by the embedder, (funcall *FONT-LOADER* url) returns the font-file
bytes for an absolute @font-face `src` URL (or NIL).  NIL disables web-font
downloading — text falls back to the bundled faces, as before.")

(defvar *font-face-cache* (make-hash-table :test 'equal)
  "src-url -> :loaded | :failed, so a re-render (or a repeated @font-face across
sheets) fetches each web font at most once.")

(defvar *font-load-budget* nil
  "Seconds LOAD-FONT-FACES may spend fetching @font-face fonts before it stops and
   leaves the rest on the bundled fallback.  @font-face fetches are serial, and a
   page can inject a stylesheet declaring hundreds of faces (nytimes.com ships a
   1.4 MB script-built sheet), which would otherwise stall the render ~75s on fonts
   the viewport never uses.  NIL (the default) is unbounded — the Acid gates and
   any single-font page are unaffected.")

(defun load-font-faces (sheet)
  "Fetch and register the @font-face web fonts declared in SHEET through
*FONT-LOADER*.  A no-op when no loader is bound.  For each face the src URLs are
tried best-format-first; the first that downloads and decodes is registered for
that (family, weight, style) and the rest are skipped.  Every fetch is guarded and
memoized, so a failed or absent font never breaks a render.  Stops early once
*FONT-LOAD-BUDGET* seconds have elapsed (a page can declare far more faces than it
uses)."
  (when *font-loader*
    (let ((deadline (and *font-load-budget*
                         (+ (get-internal-real-time)
                            (round (* *font-load-budget* internal-time-units-per-second))))))
      (dolist (face (css:collect-font-faces sheet))
        (when (and deadline (> (get-internal-real-time) deadline)) (return))
        (let ((fam (getf face :family)) (weight (getf face :weight)) (style (getf face :style)))
          (block one
            (dolist (url (getf face :urls))
              (case (gethash url *font-face-cache*)
                (:loaded (return-from one))          ; this face already came from this url
                (:failed nil)                        ; known-bad url: try the next
                (t (let ((bytes (ignore-errors (funcall *font-loader* url))))
                     (if (and bytes (register-font fam bytes :weight weight :style style))
                         (progn (setf (gethash url *font-face-cache*) :loaded) (return-from one))
                         (setf (gethash url *font-face-cache*) :failed))))))))))))

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

(defvar *glyph-cache* (make-hash-table :test 'equal)
  "Memo of (font gid ppem subpixel) -> (cov w h left top): the rasterized glyph
coverage.  Rasterizing outlines is the hottest paint cost on a text page, where
the same glyph recurs thousands of times at one size; cache the bitmap.")
(defparameter *glyph-cache-limit* 30000
  "Bound on *GLYPH-CACHE*.  Dropped wholesale when exceeded (pure memoization).")

(defun glyph-raster (font gid ppem sub)
  "Rasterize GID at PPEM (cached).  SUB, the sub-pixel x offset in [0,1), is
quantized to 4 levels so the cache hits across a line's many fractional pen
positions; the resulting <=0.25px change in anti-aliasing is imperceptible.
Returns (cov w h left top)."
  (let* ((subq (* (ffloor (* sub 4d0)) 0.25d0))
         (key (list font gid ppem subq)))
    (or (gethash key *glyph-cache*)
        (progn
          (when (> (hash-table-count *glyph-cache*) *glyph-cache-limit*) (clrhash *glyph-cache*))
          (multiple-value-bind (cov w h left top adv)
              (scribe:rasterize-glyph font gid ppem :subpixel subq)
            (declare (ignore adv))
            (setf (gethash key *glyph-cache*) (list cov w h left top)))))))

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

;;; ---- script itemization (font fallback) -----------------------------------
;;; The primary face (Liberation) covers Latin/Greek/Cyrillic; a page with other
;;; scripts (Arabic, CJK, emoji, ...) is split into runs, each shaped and painted
;;; with the bundled Noto face that covers it — resolved per codepoint through
;;; scribe.  An all-primary string (the common case) is a single run: no change.

(defun %font-upem (font)
  "units-per-em of a scribe FONT as a double (default 1000)."
  (let ((u (ignore-errors (float (scribe:font-units-per-em font) 1d0))))
    (if (and u (plusp u)) u 1000d0)))

(defun %codepoint-font (primary cp)
  "Font to render codepoint CP: PRIMARY when it has the glyph, else a scribe
script-fallback face, else PRIMARY (renders .notdef, so the run still advances)."
  (cond ((scribe:font-covers-p primary cp) primary)
        ((scribe:fallback-font-for-codepoint cp))
        (t primary)))

(defvar *segment-cache* (make-hash-table :test 'equal)
  "Memo of (primary-font . text) -> list of (font upem string) script runs.")
(defparameter *segment-cache-limit* 50000)

(defun text-segments (primary text)
  "Split TEXT into maximal (font upem string) runs, each covered by a single face:
PRIMARY where it can, a bundled Noto fallback face elsewhere.  Memoized; an
all-primary string returns one run (font=PRIMARY) so the Latin fast path is a
single cache hit and shapes exactly as before."
  (let ((key (cons primary text)))
    (or (gethash key *segment-cache*)
        (progn
          (when (> (hash-table-count *segment-cache*) *segment-cache-limit*) (clrhash *segment-cache*))
          (setf (gethash key *segment-cache*)
                (let ((n (length text)))
                  (if (zerop n) '()
                      (let* ((runs '()) (start 0)
                             (cur (%codepoint-font primary (char-code (char text 0)))))
                        (loop for i from 1 below n
                              for f = (%codepoint-font primary (char-code (char text i)))
                              unless (eq f cur) do
                                (push (list cur (%font-upem cur) (subseq text start i)) runs)
                                (setf start i cur f))
                        (push (list cur (%font-upem cur) (subseq text start n)) runs)
                        (nreverse runs)))))))))

(defun measure-text-width (text size &optional face (letter-spacing 0))
  "Width of the shaped (kerned) TEXT at px SIZE in FACE — the width scribe will
actually paint.  FACE defaults to the generic sans-serif face.  LETTER-SPACING px
is added after each glyph (CSS letter-spacing).  Falls back to the bitmap metric
(LEN*7) if no face is available or anything errors, so layout never depends on the
font succeeding."
  (let ((face (or face (default-face))))
    (if (or (null face) (zerop (length text)))
        (* (length text) *font-w*)
        (handler-case
            (let* ((font (face-font face))
                   (ppem (float (min 2000 (max 1 size)) 1d0))   ; cap ppem: no real glyph exceeds this, and it bounds rasterization memory
                   (w 0d0))
              ;; itemize into script runs; each run shapes with its own face/upem
              ;; (fallback faces have their own em size) so advances stay correct.
              (dolist (seg (text-segments font text))
                (destructuring-bind (segfont segupem segstr) seg
                  (dolist (g (shape-px segfont segstr ppem segupem))
                    (incf w (+ (second g) letter-spacing)))))
              w)
          (error () (+ (* (length text) *font-w*) (* (length text) letter-spacing)))))))

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
                         &key bold underline face (letter-spacing 0) underline-end-x baseline-off)
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
                   (ppem (float (min 2000 (max 1 size)) 1d0))   ; cap ppem: no real glyph exceeds this, and it bounds rasterization memory
                   (asc-ratio (face-ascent-ratio face))
                   (desc-ratio (face-descent-ratio face))
                   ;; The run's baseline: when the line supplies one (the per-line
                   ;; baseline model, CSS 2.1 §10.8), every run on the line shares it,
                   ;; so mixed sizes and text beside a tall atomic sit on one baseline.
                   ;; Otherwise fall back to centering the font's em-box (ascent+
                   ;; descent) in the line box — a single-font line lands identically.
                   (text-h (* (+ asc-ratio desc-ratio) ppem))
                   (baseline (round (if baseline-off
                                        (+ line-top baseline-off)
                                        (+ line-top (/ (- line-h text-h) 2)
                                           (* asc-ratio ppem)))))
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
                   ;; the colour's alpha modulates glyph coverage: `color: transparent`
                   ;; (alpha 0) paints nothing, a translucent colour paints faded.
                   (alpha (let ((a (fourth color))) (if a (float a 1d0) 1d0)))
                   (penx (float x 1d0)))
              ;; itemize into script runs; paint each with the face that covers it
              ;; (fallback faces for non-Latin scripts / emoji), all on the primary
              ;; face's baseline so mixed-script text sits on one line.
              (dolist (seg (text-segments font text))
               (destructuring-bind (segfont segupem segstr) seg
                (loop for (gid xadv xoff yoff) in (shape-px segfont segstr ppem segupem) do
                  (let ((sub (- (+ penx xoff) (ffloor (+ penx xoff)))))
                    ;; gid 0 = .notdef: don't draw a tofu box, but still advance so the
                    ;; run keeps its layout position.  Advance by the SHAPED (kerned)
                    ;; x-advance so painting matches MEASURE-TEXT-WIDTH to the pixel.
                    (if (zerop gid)
                        (incf penx (+ xadv letter-spacing))
                        (destructuring-bind (cov w h left top)
                            (glyph-raster segfont gid ppem sub)
                          (when cov
                            (let ((ox (+ (floor (+ penx xoff)) left)) (oy (+ baseline top (- (round yoff)))))
                              (dotimes (yy h)
                                (let ((py (+ oy yy)))
                                  (when (and (>= py cy0) (< py cy1))
                                    (dotimes (xx w)
                                      (let ((px (+ ox xx)))
                                        (when (and (>= px cx0) (< px cx1))
                                          (let ((c (* alpha (aref cov (+ (* yy w) xx)))))
                                            (when (> c 0d0)
                                              (scribe:blend-coverage scv px py c color)))))))))))
                          (incf penx (+ xadv letter-spacing))))))))
              (when (and underline (plusp alpha))
                ;; underline to UNDERLINE-END-X when given (so a multi-word link's
                ;; underline runs continuously across the spaces), else to the pen.
                ;; ALPHA gate: a transparent text colour (currentColor) paints no
                ;; decoration — fill-rect is opaque, so skip it entirely at alpha 0.
                (let ((uy (min (1- cy1) (+ baseline (max 1 (round (* 0.12d0 ppem))))))
                      (uend (if underline-end-x (round underline-end-x) (round penx))))
                  (fill-rect cv x uy (max 0 (- uend x)) (max 1 (round (* 0.06d0 ppem))) color)))
              (round penx))
          (error ()
            ;; partial paint may have happened; finish the string with the bitmap
            ;; path so the user at least sees the text.
            (draw-text cv text x (bitmap-top line-top line-h) color :bold bold :underline underline))))))
