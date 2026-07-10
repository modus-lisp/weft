;;;; src/render/image.lisp — data-URI + base64 + PNG/GIF decode to RGBA pixels.
;;;;
;;;; Enough to paint real <img src="data:..."> and CSS data-URI backgrounds:
;;;; a base64 decoder, a data-URI splitter, and a PNG decoder (IHDR/PLTE/tRNS/
;;;; IDAT via chipz inflate, scanline unfiltering, color types 0/2/3/4/6) plus a
;;;; minimal GIF87a/89a decoder.  Returns an IMG struct (w h rgba) where rgba is
;;;; a (w*h*4) octet vector, straight-alpha.
(in-package #:weft.render)

(defstruct img w h rgba)

;;; ---- base64 ------------------------------------------------------------
(defparameter +b64+
  (let ((tbl (make-array 256 :initial-element -1)))
    (loop for c across "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
          for i from 0 do (setf (aref tbl (char-code c)) i))
    tbl))

(defun base64-decode (string)
  "Decode a base64 STRING to an (unsigned-byte 8) vector."
  (let ((out (make-array (* 3 (ceiling (length string) 4)) :element-type '(unsigned-byte 8) :fill-pointer 0))
        (acc 0) (bits 0))
    (loop for ch across string
          for v = (if (< (char-code ch) 256) (aref +b64+ (char-code ch)) -1)
          when (>= v 0) do
            (setf acc (logior (ash acc 6) v) bits (+ bits 6))
            (when (>= bits 8) (decf bits 8) (vector-push-extend (logand (ash acc (- bits)) #xff) out)))
    out))

(defun parse-data-uri (uri)
  "Parse data:[<mime>][;base64],<data>.  Returns (values mime bytes) or NIL."
  (when (and (>= (length uri) 5) (string-equal (subseq uri 0 5) "data:"))
    (let* ((comma (position #\, uri)))
      (when comma
        (let* ((meta (subseq uri 5 comma)) (payload (subseq uri (1+ comma)))
               (b64 (search ";base64" meta :test #'char-equal))
               (mime (if b64 (subseq meta 0 b64) meta)))
          (values mime (if b64 (base64-decode payload)
                           (map '(vector (unsigned-byte 8)) #'char-code payload))))))))

;;; ---- PNG ---------------------------------------------------------------
(defun be32 (v i) (logior (ash (aref v i) 24) (ash (aref v (+ i 1)) 16) (ash (aref v (+ i 2)) 8) (aref v (+ i 3))))

(defun png-decode (bytes)
  "Decode a PNG byte vector to an IMG (RGBA), or NIL."
  (when (and (>= (length bytes) 8) (= (aref bytes 0) 137) (= (aref bytes 1) 80))
    (let ((i 8) (n (length bytes)) w h depth ctype (interlace 0)
          (idat (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
          (plte nil) (trns nil))
      (loop while (< (+ i 8) n) do
        (let* ((len (be32 bytes i)) (type (map 'string #'code-char (subseq bytes (+ i 4) (+ i 8))))
               (ds (+ i 8)))
          (cond
            ((string= type "IHDR")
             (setf w (be32 bytes ds) h (be32 bytes (+ ds 4)) depth (aref bytes (+ ds 8)) ctype (aref bytes (+ ds 9))
                   interlace (aref bytes (+ ds 12))))
            ((string= type "PLTE") (setf plte (subseq bytes ds (+ ds len))))
            ((string= type "tRNS") (setf trns (subseq bytes ds (+ ds len))))
            ((string= type "IDAT") (loop for k from ds below (+ ds len) do (vector-push-extend (aref bytes k) idat)))
            ((string= type "IEND") (return)))
          (setf i (+ ds len 4))))   ; skip CRC
      (when (and w h depth ctype (only-supported-p depth ctype))
        ;; chipz needs a SIMPLE array; COERCE leaves an adjustable/fill-pointer
        ;; vector unchanged, so build a fresh simple-array of the bytes.
        (png-finish w h depth ctype
                    (replace (make-array (length idat) :element-type '(unsigned-byte 8)) idat)
                    plte trns interlace)))))

(defun only-supported-p (depth ctype) (and (= depth 8) (member ctype '(0 2 3 4 6))))

(defun channels (ctype) (ecase ctype (0 1) (2 3) (3 1) (4 2) (6 4)))

(defun png-unfilter (raw ri w h ch)
  "Unfilter H scanlines of W*CH bytes from RAW starting at index RI (each scanline
prefixed by a PNG filter-type byte).  Returns (values rows next-ri); ROWS is a
fresh (W*H*CH) octet vector of reconstructed samples."
  (let* ((stride (* w ch)) (rows (make-array (* h stride) :element-type '(unsigned-byte 8))) (rp 0))
    (dotimes (y h)
      (let ((ft (aref raw ri))) (incf ri)
        (dotimes (xb stride)
          (let* ((rawb (aref raw ri))
                 (a (if (>= xb ch) (aref rows (- rp ch)) 0))
                 (b (if (> y 0) (aref rows (- rp stride)) 0))
                 (c (if (and (> y 0) (>= xb ch)) (aref rows (- rp stride ch)) 0))
                 (val (ecase ft
                        (0 rawb) (1 (+ rawb a)) (2 (+ rawb b)) (3 (+ rawb (floor (+ a b) 2)))
                        (4 (+ rawb (paeth a b c))))))
            (setf (aref rows rp) (logand val #xff)) (incf rp) (incf ri)))))
    (values rows ri)))

;; Adam7 interlace passes: (x-start y-start x-step y-step).
(defparameter +adam7+ '((0 0 8 8) (4 0 8 8) (0 4 4 8) (2 0 4 4) (0 2 2 4) (1 0 2 2) (0 1 1 2)))

(defun png-deinterlace (raw w h ch)
  "Reconstruct a non-interlaced W*H*CH sample buffer from Adam7-interlaced RAW."
  (let ((rows (make-array (* w h ch) :element-type '(unsigned-byte 8))) (ri 0) (stride (* w ch)))
    (dolist (pass +adam7+ rows)
      (destructuring-bind (sx sy dx dy) pass
        (let ((pw (ceiling (max 0 (- w sx)) dx)) (ph (ceiling (max 0 (- h sy)) dy)))
          (when (and (plusp pw) (plusp ph))
            (multiple-value-bind (prows nri) (png-unfilter raw ri pw ph ch)
              (setf ri nri)
              (let ((pstride (* pw ch)))
                (dotimes (py ph)
                  (dotimes (px pw)
                    (let ((srcp (+ (* py pstride) (* px ch)))
                          (dstp (+ (* (+ sy (* py dy)) stride) (* (+ sx (* px dx)) ch))))
                      (dotimes (k ch) (setf (aref rows (+ dstp k)) (aref prows (+ srcp k)))))))))))))))

(defun png-finish (w h depth ctype idat plte trns interlace)
  (declare (ignore depth))
  (let* ((ch (channels ctype)) (stride (* w ch))
         (raw (handler-case (chipz:decompress nil 'chipz:zlib idat) (error () nil))))
    (when (and raw (if (= interlace 1)
                       (plusp (length raw))
                       (>= (length raw) (* h (1+ stride)))))
      (let ((rows (if (= interlace 1)
                      (png-deinterlace raw w h ch)
                      (png-unfilter raw 0 w h ch))))
        ;; expand to RGBA
        (let ((rgba (make-array (* w h 4) :element-type '(unsigned-byte 8))))
          (dotimes (p (* w h))
            (let ((s (* p ch)) (d (* p 4)))
              (ecase ctype
                ;; tRNS for grayscale (ctype 0) is a single 16-bit key sample;
                ;; a pixel equal to it is fully transparent (depth 8 -> low byte).
                (0 (let ((g (aref rows s)))
                     (setf (aref rgba d) g (aref rgba (+ d 1)) g (aref rgba (+ d 2)) g
                           (aref rgba (+ d 3))
                           (if (and trns (>= (length trns) 2) (= g (aref trns 1))) 0 255))))
                ;; tRNS for truecolor (ctype 2) is an R,G,B key (16-bit each);
                ;; a pixel matching it is fully transparent (Acid2's eye tiles use
                ;; a black key so their two opposite pixels read through).
                (2 (let ((r (aref rows s)) (g (aref rows (+ s 1))) (bl (aref rows (+ s 2))))
                     (setf (aref rgba d) r (aref rgba (+ d 1)) g (aref rgba (+ d 2)) bl
                           (aref rgba (+ d 3))
                           (if (and trns (>= (length trns) 6)
                                    (= r (aref trns 1)) (= g (aref trns 3)) (= bl (aref trns 5)))
                               0 255))))
                (3 (let* ((idx (aref rows s)) (pi3 (* idx 3)))
                     (setf (aref rgba d) (if plte (aref plte pi3) 0)
                           (aref rgba (+ d 1)) (if plte (aref plte (+ pi3 1)) 0)
                           (aref rgba (+ d 2)) (if plte (aref plte (+ pi3 2)) 0)
                           (aref rgba (+ d 3)) (if (and trns (< idx (length trns))) (aref trns idx) 255))))
                (4 (let ((g (aref rows s))) (setf (aref rgba d) g (aref rgba (+ d 1)) g (aref rgba (+ d 2)) g (aref rgba (+ d 3)) (aref rows (+ s 1)))))
                (6 (setf (aref rgba d) (aref rows s) (aref rgba (+ d 1)) (aref rows (+ s 1)) (aref rgba (+ d 2)) (aref rows (+ s 2)) (aref rgba (+ d 3)) (aref rows (+ s 3)))))))
          (make-img :w w :h h :rgba rgba))))))

(defun paeth (a b c)
  (let* ((p (- (+ a b) c)) (pa (abs (- p a))) (pb (abs (- p b))) (pc (abs (- p c))))
    (cond ((and (<= pa pb) (<= pa pc)) a) ((<= pb pc) b) (t c))))

;;; ---- GIF (minimal, first frame, global palette) ------------------------
(defun gif-decode (bytes)
  "Minimal GIF decode -> IMG, or NIL (uncompressed/LZW omitted; returns the
background-filled canvas size so layout at least gets dimensions)."
  (when (and (>= (length bytes) 10) (= (aref bytes 0) 71) (= (aref bytes 1) 73))   ; "GI"
    (let ((w (logior (aref bytes 6) (ash (aref bytes 7) 8)))
          (h (logior (aref bytes 8) (ash (aref bytes 9) 8))))
      (when (and (plusp w) (plusp h))
        (make-img :w w :h h :rgba (make-array (* w h 4) :element-type '(unsigned-byte 8) :initial-element 0))))))

;;; ---- SVG (rendered through stencil to a straight-alpha bitmap) ----------
(defun rgba-canvas->img (cv)
  "Convert a premultiplied RGBA scribe canvas to a straight-alpha weft IMG."
  (let* ((w (sc:canvas-width cv)) (h (sc:canvas-height cv))
         (px (sc:canvas-pixels cv)) (ap (sc:canvas-alpha cv))
         (rgba (make-array (* w h 4) :element-type '(unsigned-byte 8))))
    (dotimes (j (* w h))
      (let ((a (aref ap j)) (i3 (* 3 j)) (i4 (* 4 j)))
        (if (zerop a)
            (setf (aref rgba i4) 0 (aref rgba (+ i4 1)) 0 (aref rgba (+ i4 2)) 0 (aref rgba (+ i4 3)) 0)
            (setf (aref rgba i4)       (min 255 (floor (* (aref px i3) 255) a))
                  (aref rgba (+ i4 1)) (min 255 (floor (* (aref px (+ i3 1)) 255) a))
                  (aref rgba (+ i4 2)) (min 255 (floor (* (aref px (+ i3 2)) 255) a))
                  (aref rgba (+ i4 3)) a))))
    (make-img :w w :h h :rgba rgba)))

(defun svg-data-uri-source (uri)
  "The SVG source string carried by a data:image/svg+xml URI, or NIL."
  (let ((comma (position #\, uri)))
    (when comma
      (let* ((meta (subseq uri 5 comma)) (payload (subseq uri (1+ comma)))
             (b64 (search ";base64" meta :test #'char-equal)))
        (if b64
            (map 'string #'code-char (base64-decode payload))
            (percent-decode payload))))))

(defun decode-svg-source (src)
  "Render an SVG source string through stencil to a straight-alpha IMG at the SVG's
   intrinsic size, or NIL."
  (when (and src (plusp (length src)))
    (let ((root (ignore-errors (st:parse-svg src))))
      (when root
        (multiple-value-bind (iw ih) (st:svg-intrinsic-size root)
          (let* ((w (max 1 (round iw))) (h (max 1 (round ih)))
                 (cv (sc:make-rgba-canvas w h)))
            (ignore-errors (st:render-svg-to-canvas root :width w :height h :canvas cv))
            (rgba-canvas->img cv)))))))

(defun decode-svg-image (uri)
  "Render a data:image/svg+xml URI through stencil to a straight-alpha IMG, or NIL."
  (decode-svg-source (svg-data-uri-source uri)))

;;; ---- entry -------------------------------------------------------------
(defun decode-image (uri)
  "Decode a data: URI (PNG, GIF or SVG) to an IMG, or NIL."
  (multiple-value-bind (mime bytes) (parse-data-uri uri)
    (cond ((and mime (search "svg" mime :test #'char-equal)) (decode-svg-image uri))
          ((null bytes) nil)
          ((search "png" (or mime "") :test #'char-equal) (png-decode bytes))
          ((or (search "jpeg" (or mime "") :test #'char-equal) (search "jpg" (or mime "") :test #'char-equal))
           (jpeg-decode bytes))
          ((search "gif" (or mime "") :test #'char-equal) (gif-decode bytes))
          ((and (>= (length bytes) 2) (= (aref bytes 0) 137)) (png-decode bytes))
          ((and (>= (length bytes) 2) (= (aref bytes 0) #xff) (= (aref bytes 1) #xd8)) (jpeg-decode bytes))
          (t nil))))

(defun %svg-bytes-p (bytes)
  "True when BYTES look like SVG/XML text (starts with '<' after optional BOM/space)."
  (loop for i from 0 below (min 64 (length bytes))
        for c = (aref bytes i)
        do (cond ((member c '(9 10 13 32 #xef #xbb #xbf)))   ; whitespace / UTF-8 BOM
                 ((= c (char-code #\<))
                  (return (let ((s (map 'string #'code-char (subseq bytes i (min (length bytes) (+ i 256))))))
                            (or (search "<svg" s :test #'char-equal)
                                (search "<?xml" s :test #'char-equal)))))
                 (t (return nil)))))

(defun webp-to-img (bytes)
  "Decode a WebP (VP8) via webp-pure into an opaque RGBA IMG, or NIL."
  (multiple-value-bind (w h rgb) (webp-pure:decode bytes)
    (when (and w h rgb (plusp w) (plusp h))
      (let ((rgba (make-array (* w h 4) :element-type '(unsigned-byte 8))))
        (dotimes (i (* w h))
          (setf (aref rgba (* i 4))       (aref rgb (* i 3))
                (aref rgba (+ (* i 4) 1)) (aref rgb (+ (* i 3) 1))
                (aref rgba (+ (* i 4) 2)) (aref rgb (+ (* i 3) 2))
                (aref rgba (+ (* i 4) 3)) 255))
        (make-img :w w :h h :rgba rgba)))))

(defun decode-image-bytes (bytes &optional mime)
  "Decode raw image BYTES (from a network fetch, not a data: URI) to an IMG, or NIL.
   Dispatches on MIME when given, else on the leading magic bytes."
  (when (and bytes (plusp (length bytes)))
    (cond
      ((and mime (search "svg" mime :test #'char-equal))
       (decode-svg-source (map 'string #'code-char bytes)))
      ((and (>= (length bytes) 8) (= (aref bytes 0) 137) (= (aref bytes 1) 80)) (png-decode bytes))
      ((and (>= (length bytes) 2) (= (aref bytes 0) #xff) (= (aref bytes 1) #xd8)) (jpeg-decode bytes))
      ((webp-pure:webp-p bytes) (ignore-errors (webp-to-img bytes)))
      ((and (>= (length bytes) 3) (= (aref bytes 0) 71) (= (aref bytes 1) 73) (= (aref bytes 2) 70))
       (gif-decode bytes))               ; "GIF"
      ((and mime (search "png" mime :test #'char-equal)) (png-decode bytes))
      ((and mime (or (search "jpeg" mime :test #'char-equal) (search "jpg" mime :test #'char-equal))) (jpeg-decode bytes))
      ((and mime (search "gif" mime :test #'char-equal)) (gif-decode bytes))
      ((%svg-bytes-p bytes) (decode-svg-source (map 'string #'code-char bytes)))
      (t nil))))

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
       (jpeg-size bytes))
      (t nil))))

(defun fetch-image (url)
  "Fetch, decode, and cache the network image at URL — returns an IMG or NIL.
   If the pixels can't be decoded (unsupported feature, truncation) but the header
   gives a size, returns a dimensions-only IMG (RGBA NIL) so the box is still
   reserved at the correct intrinsic size and does not reflow later.  Memoizes
   successes and failures in the bounded *IMAGE-STORE*."
  (when (and *image-loader* url (plusp (length url)))
    (multiple-value-bind (hit found) (gethash url *image-store*)
      (if found
          (and (not (eq hit :failed)) hit)
          (let ((img (handler-case
                         (multiple-value-bind (bytes mime) (funcall *image-loader* url)
                           (and bytes
                                (or (decode-image-bytes bytes mime)
                                    (multiple-value-bind (w h) (image-size-bytes bytes)
                                      (and w h (plusp w) (plusp h) (make-img :w w :h h :rgba nil))))))
                       (error () nil))))
            (when (>= (hash-table-count *image-store*) *image-store-cap*) (clrhash *image-store*))
            (setf (gethash url *image-store*) (or img :failed))
            img)))))

(defun blit-img (cv img x y &optional dw dh)
  "Paint IMG onto CV at (X,Y), straight-alpha over existing pixels, optionally
scaled to (DW,DH).  Downscaling area-averages each output pixel's source
footprint (a box filter, alpha-weighted) so a high-resolution image resolves
smoothly instead of aliasing to noise; upscaling / 1:1 is nearest-neighbour."
  (let* ((iw (img-w img)) (ih (img-h img)) (rgba (img-rgba img))
         (ow (or dw iw)) (oh (or dh ih))
         (down (or (< ow iw) (< oh ih))))
    (flet ((blend (ox oy r g b a)
             (when (plusp a)
               (let ((px (+ x ox)) (py (+ y oy)))
                 (if (>= a 255)
                     (put cv px py r g b)
                     (when (and (>= px 0) (>= py 0) (< px (canvas-width cv)) (< py (canvas-height cv)))
                       (let* ((di (* 3 (+ (* py (canvas-width cv)) px))) (pb (canvas-pixels cv)) (ia (- 255 a)))
                         (setf (aref pb di)       (floor (+ (* r a) (* (aref pb di) ia)) 255)
                               (aref pb (+ di 1)) (floor (+ (* g a) (* (aref pb (+ di 1)) ia)) 255)
                               (aref pb (+ di 2)) (floor (+ (* b a) (* (aref pb (+ di 2)) ia)) 255)))))))))
      (if (not down)
          (dotimes (oy oh)                       ; nearest-neighbour (no shrink)
            (dotimes (ox ow)
              (let* ((sx (min (1- iw) (floor (* ox iw) ow))) (sy (min (1- ih) (floor (* oy ih) oh)))
                     (si (* 4 (+ (* sy iw) sx))))
                (blend ox oy (aref rgba si) (aref rgba (+ si 1)) (aref rgba (+ si 2)) (aref rgba (+ si 3))))))
          (dotimes (oy oh)                       ; box filter: average the source footprint
            (let* ((sy0 (floor (* oy ih) oh)) (sy1 (max (1+ sy0) (floor (* (1+ oy) ih) oh))))
              (dotimes (ox ow)
                (let* ((sx0 (floor (* ox iw) ow)) (sx1 (max (1+ sx0) (floor (* (1+ ox) iw) ow)))
                       (sr 0) (sg 0) (sb 0) (sa 0) (n 0))
                  (loop for sy from sy0 below (min ih sy1) do
                    (loop for sx from sx0 below (min iw sx1) do
                      (let* ((si (* 4 (+ (* sy iw) sx))) (a (aref rgba (+ si 3))))
                        (incf sr (* (aref rgba si) a)) (incf sg (* (aref rgba (+ si 1)) a))
                        (incf sb (* (aref rgba (+ si 2)) a)) (incf sa a) (incf n))))
                  (when (plusp n)
                    (if (plusp sa)
                        (blend ox oy (floor sr sa) (floor sg sa) (floor sb sa) (floor sa n))
                        (blend ox oy 0 0 0 0)))))))))))
