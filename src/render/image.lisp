;;;; src/render/image.lisp — weft's glue over the pigment image codecs.
;;;;
;;;; The actual decoders (PNG/GIF/JPEG/WebP/SVG + data: URIs -> an RGBA IMG) live
;;;; in the standalone `pigment` library; weft imports its IMG struct + decode
;;;; entry points (see src/render/packages.lisp).  What stays here is the part
;;;; that is specific to weft's own renderer: painting an IMG onto weft's canvas
;;;; (BLIT-IMG), and the network image-loading + decode cache that feeds real
;;;; <img>/background fetches through pigment.
(in-package #:weft.render)

;;; ---- network image loading + cache -------------------------------------
(defvar *image-loader* nil
  "When bound to a function (URL) -> (values BYTES MIME), FETCH-IMAGE pulls network
   <img> bitmaps through it (backed by weft.fetch over seal in the browsing paths).
   NIL disables network images — the render stays offline/deterministic.")
(defparameter *image-store* (make-hash-table :test 'equal :synchronized t)
  "Persistent URL -> IMG (or :FAILED) cache, so re-layout and re-visits don't
   refetch or re-decode.  Bounded by *IMAGE-STORE-CAP*.")
(defparameter *image-store-cap* 256)

(defun clear-image-cache () (clrhash *image-store*))

(defun image-size-bytes (bytes)
  "Read an image's intrinsic (values WIDTH HEIGHT) from just its header (PNG IHDR,
   GIF screen descriptor, JPEG SOF) — no full decode.  So a box can be reserved at
   the right size even when the pixels can't (yet) be decoded."
  (when (and bytes (>= (length bytes) 24))
    (cond
      ((and (= (aref bytes 0) 137) (= (aref bytes 1) 80))          ; PNG: IHDR at 16/20, big-endian
       (values (logior (ash (aref bytes 16) 24) (ash (aref bytes 17) 16) (ash (aref bytes 18) 8) (aref bytes 19))
               (logior (ash (aref bytes 20) 24) (ash (aref bytes 21) 16) (ash (aref bytes 22) 8) (aref bytes 23))))
      ((and (= (aref bytes 0) 71) (= (aref bytes 1) 73))           ; GIF: screen w/h at 6/8, little-endian
       (values (logior (aref bytes 6) (ash (aref bytes 7) 8)) (logior (aref bytes 8) (ash (aref bytes 9) 8))))
      ((and (= (aref bytes 0) #xff) (= (aref bytes 1) #xd8))       ; JPEG: SOF
       (pigment:jpeg-size bytes))
      (t nil))))

(defvar *image-fetch-deadline* nil
  "When set (an internal-real-time), FETCH-IMAGE serves only already-cached images
   past it — a NEW network fetch is skipped and NIL returned.  A host bounds the
   time a render spends on images this way, so a page with slow/hung image URLs
   still paints (the misses simply aren't in this frame) instead of blocking on the
   slowest fetch.  NIL (the default) never bounds fetching.")

(defun fetch-image (url)
  "Fetch, decode, and cache the network image at URL — returns an IMG or NIL.
   If the pixels can't be decoded (unsupported feature, truncation) but the header
   gives a size, returns a dimensions-only IMG (RGBA NIL) so the box is still
   reserved at the correct intrinsic size and does not reflow later.  Memoizes
   successes and failures in the bounded *IMAGE-STORE*."
  (when (and *image-loader* url (plusp (length url)))
    (multiple-value-bind (hit found) (gethash url *image-store*)
      (cond
        (found (and (not (eq hit :failed)) hit))
        ;; past the render's image budget: don't start a new (possibly slow) fetch;
        ;; leave it uncached so a later render can still try.
        ((and *image-fetch-deadline* (> (get-internal-real-time) *image-fetch-deadline*)) nil)
        (t
          (let ((img (handler-case
                         (multiple-value-bind (bytes mime) (funcall *image-loader* url)
                           (and bytes
                                (or (decode-image-bytes bytes mime)
                                    (multiple-value-bind (w h) (image-size-bytes bytes)
                                      (and w h (plusp w) (plusp h) (make-img :w w :h h :rgba nil))))))
                       (error () nil))))
            (when (>= (hash-table-count *image-store*) *image-store-cap*) (clrhash *image-store*))
            (setf (gethash url *image-store*) (or img :failed))
            img))))))

;;; ---- painting an IMG onto weft's canvas --------------------------------
(defun blit-img (cv img x y &optional dw dh src)
  "Paint IMG onto CV at (X,Y), straight-alpha over existing pixels, optionally
scaled to (DW,DH).  SRC=(sx sy sw sh) is a source crop rectangle in image pixels
(default the whole image) — the CSS object-fit:cover/none path samples only that
sub-rectangle, scaled to fill (DW,DH), so a wide relief image crops to a portrait
hero box without distortion.  Downscaling area-averages each output pixel's source
footprint (a box filter, alpha-weighted) so a high-resolution image resolves
smoothly instead of aliasing to noise; upscaling / 1:1 is nearest-neighbour."
  (let* ((iw (img-w img)) (ih (img-h img)) (rgba (img-rgba img))
         ;; source rectangle: whole image unless a crop is given.  IW stays the row
         ;; stride into RGBA; SX0/SY0 offset into it, SW/SH bound the sampled region.
         (sx0 (if src (max 0 (first src)) 0)) (sy0 (if src (max 0 (second src)) 0))
         (sw (if src (min (third src) (- iw sx0)) iw))
         (sh (if src (min (fourth src) (- ih sy0)) ih))
         (ow (or dw sw)) (oh (or dh sh))
         (down (or (< ow sw) (< oh sh))))
    (when (or (<= sw 0) (<= sh 0) (<= ow 0) (<= oh 0)) (return-from blit-img))
    (flet ((blend (ox oy r g b a)
             (when (plusp a)
               (let ((px (+ x ox)) (py (+ y oy)))
                 (if (>= a 255)
                     (put cv px py r g b)   ; opaque: PUT honours the rounded clip
                     (when (and (>= px 0) (>= py 0) (< px (canvas-width cv)) (< py (canvas-height cv))
                                (rclip-ok px py))
                       (let* ((di (* 3 (+ (* py (canvas-width cv)) px))) (pb (canvas-pixels cv)) (ia (- 255 a)))
                         (setf (aref pb di)       (floor (+ (* r a) (* (aref pb di) ia)) 255)
                               (aref pb (+ di 1)) (floor (+ (* g a) (* (aref pb (+ di 1)) ia)) 255)
                               (aref pb (+ di 2)) (floor (+ (* b a) (* (aref pb (+ di 2)) ia)) 255)))))))))
      (if (not down)
          (dotimes (oy oh)                       ; nearest-neighbour (no shrink)
            (dotimes (ox ow)
              (let* ((sx (min (1- iw) (+ sx0 (floor (* ox sw) ow)))) (sy (min (1- ih) (+ sy0 (floor (* oy sh) oh))))
                     (si (* 4 (+ (* sy iw) sx))))
                (blend ox oy (aref rgba si) (aref rgba (+ si 1)) (aref rgba (+ si 2)) (aref rgba (+ si 3))))))
          (dotimes (oy oh)                       ; box filter: average the source footprint
            (let* ((ry0 (+ sy0 (floor (* oy sh) oh))) (ry1 (+ sy0 (max (1+ (floor (* oy sh) oh)) (floor (* (1+ oy) sh) oh)))))
              (dotimes (ox ow)
                (let* ((rx0 (+ sx0 (floor (* ox sw) ow))) (rx1 (+ sx0 (max (1+ (floor (* ox sw) ow)) (floor (* (1+ ox) sw) ow))))
                       (sr 0) (sg 0) (sb 0) (sa 0) (n 0))
                  (loop for sy from ry0 below (min ih ry1) do
                    (loop for sx from rx0 below (min iw rx1) do
                      (let* ((si (* 4 (+ (* sy iw) sx))) (a (aref rgba (+ si 3))))
                        (incf sr (* (aref rgba si) a)) (incf sg (* (aref rgba (+ si 1)) a))
                        (incf sb (* (aref rgba (+ si 2)) a)) (incf sa a) (incf n))))
                  (when (plusp n)
                    (if (plusp sa)
                        (blend ox oy (floor sr sa) (floor sg sa) (floor sb sa) (floor sa n))
                        (blend ox oy 0 0 0 0)))))))))))
