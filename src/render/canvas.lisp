;;;; src/render/canvas.lisp — RGB pixel canvas, bitmap text, and a PNG encoder.
;;;;
;;;; The PNG encoder writes a truecolor (RGB8) PNG using zlib *stored* (BTYPE=00)
;;;; deflate blocks — no compression library needed — with our own CRC32 +
;;;; Adler32.  Enough to save a rendered page to a real .png.
(in-package #:weft.render)

(defstruct (canvas (:constructor %make-canvas))
  width height pixels)   ; pixels: (unsigned-byte 8) vector, w*h*3, row-major RGB

(defun make-canvas (w h &optional (bg '(255 255 255)))
  (let* ((px (make-array (* w h 3) :element-type '(unsigned-byte 8))))
    (loop for i from 0 below (length px) by 3 do
      (setf (aref px i) (first bg) (aref px (+ i 1)) (second bg) (aref px (+ i 2)) (third bg)))
    (%make-canvas :width w :height h :pixels px)))

(defvar *clip* nil
  "Active clip rectangle (x0 y0 x1 y1) in device pixels, or NIL for none.
Bound by paint when entering an overflow:hidden/clip/scroll box.")

(declaim (inline put))
(defun put (cv x y r g b)
  (when (and (>= x 0) (>= y 0) (< x (canvas-width cv)) (< y (canvas-height cv))
             (or (null *clip*)
                 (and (>= x (the fixnum (first *clip*))) (>= y (the fixnum (second *clip*)))
                      (< x (the fixnum (third *clip*))) (< y (the fixnum (fourth *clip*))))))
    (let ((i (* 3 (+ (* y (canvas-width cv)) x))) (px (canvas-pixels cv)))
      (setf (aref px i) r (aref px (+ i 1)) g (aref px (+ i 2)) b))))

(defun clip-intersect (x0 y0 x1 y1)
  "Intersect (X0 Y0 X1 Y1) with the current *CLIP*, returning a new clip rect."
  (if *clip*
      (list (max x0 (first *clip*)) (max y0 (second *clip*))
            (min x1 (third *clip*)) (min y1 (fourth *clip*)))
      (list x0 y0 x1 y1)))

(defun fill-rect (cv x y w h color)
  (let ((r (first color)) (g (second color)) (b (third color))
        (x0 (max 0 (round x))) (y0 (max 0 (round y)))
        (x1 (min (canvas-width cv) (round (+ x w)))) (y1 (min (canvas-height cv) (round (+ y h)))))
    (loop for yy from y0 below y1 do
      (loop for xx from x0 below x1 do (put cv xx yy r g b)))))

(defun fill-poly (cv points color)
  "Scanline-fill the polygon POINTS (a list of (x . y) float conses) with COLOR
\(an (r g b ...) list).  Handles convex and degenerate (collapsed-edge) polygons;
respects *CLIP*.  Used for mitered border trapezoids/triangles."
  (when (and points (>= (length points) 3))
    (let* ((r (first color)) (g (second color)) (b (third color))
           (pts (coerce points 'vector)) (n (length pts))
           (ys (map 'list #'cdr pts))
           (y0 (max 0 (floor (reduce #'min ys))))
           (y1 (min (canvas-height cv) (ceiling (reduce #'max ys)))))
      ;; Sample each scanline at the pixel's TOP edge (yc = yy), not its centre
      ;; (yy+0.5).  For two triangles that meet on a shared horizontal base at an
      ;; integer y (e.g. Acid2's nose diamond: an up-triangle whose base is the
      ;; down-triangle's top, both at y=192), centre sampling straddles the base
      ;; row — 191.5 and 192.5 both reach full width — so the diamond renders
      ;; symmetric about y+0.5 (two equal widest rows) and half a pixel too high.
      ;; Top-edge sampling lands one scanline exactly on the base, giving a single
      ;; widest row and a diamond symmetric about the integer centre — matching
      ;; the reference rasteriser pixel-for-pixel.
      (loop for yy from y0 below y1
            for yc = yy
            do (let ((xs '()))
                 (dotimes (i n)
                   (let* ((p1 (aref pts i)) (p2 (aref pts (mod (1+ i) n)))
                          (ya (cdr p1)) (yb (cdr p2)))
                     (when (or (and (<= ya yc) (< yc yb)) (and (<= yb yc) (< yc ya)))
                       (let ((xa (car p1)) (xb (car p2)))
                         (push (+ xa (* (- xb xa) (/ (- yc ya) (- yb ya)))) xs)))))
                 (setf xs (sort xs #'<))
                 (loop for (xl xr) on xs by #'cddr
                       when xr do
                         (loop for xx from (max 0 (round xl))
                                 below (min (canvas-width cv) (round xr))
                               do (put cv xx yy r g b))))))))

(defun lerp (a b tt) (round (+ a (* (- b a) tt))))

(defun fill-gradient (cv x y w h dir from to)
  "Fill a rect with a 2-stop linear gradient (DIR :vertical|:horizontal)."
  (let ((x0 (max 0 (round x))) (y0 (max 0 (round y)))
        (x1 (min (canvas-width cv) (round (+ x w)))) (y1 (min (canvas-height cv) (round (+ y h)))))
    (loop for yy from y0 below y1 do
      (loop for xx from x0 below x1 do
        (let ((tt (if (eq dir :horizontal)
                      (if (> w 1) (/ (- xx x) (float w)) 0)
                      (if (> h 1) (/ (- yy y) (float h)) 0))))
          (setf tt (max 0.0 (min 1.0 tt)))
          (put cv xx yy (lerp (first from) (first to) tt)
               (lerp (second from) (second to) tt) (lerp (third from) (third to) tt)))))))

(defun draw-char (cv ch x y color &optional bold)
  "Draw one ASCII char at (x,y) top-left.  Returns the advance width."
  (let ((code (char-code ch)))
    (when (and (>= code 32) (<= code 126))
      (let ((glyph (aref (if bold *font-bold* *font*) (- code 32)))
            (r (first color)) (g (second color)) (b (third color)))
        (dotimes (row *font-h*)
          (let ((bits (aref glyph row)))
            (dotimes (col *font-w*)
              (when (logbitp col bits) (put cv (+ x col) (+ y row) r g b)))))))
    *font-w*))

(defun draw-text (cv text x y color &key bold underline)
  (let ((cx x))
    (loop for ch across text do (incf cx (draw-char cv ch cx y color bold)))
    (when underline
      (fill-rect cv x (+ y *font-h* -1) (- cx x) 1 color))
    cx))

;;; ---- CRC32 / Adler32 ---------------------------------------------------
(defparameter *crc-table*
  (let ((tbl (make-array 256 :element-type '(unsigned-byte 32))))
    (dotimes (n 256 tbl)
      (let ((c n))
        (dotimes (k 8) (setf c (if (logbitp 0 c) (logxor #xedb88320 (ash c -1)) (ash c -1))))
        (setf (aref tbl n) c)))))

(defun crc32 (bytes &optional (start 0) (end (length bytes)))
  (let ((c #xffffffff))
    (loop for i from start below end do
      (setf c (logxor (aref *crc-table* (logand (logxor c (aref bytes i)) #xff)) (ash c -8))))
    (logxor c #xffffffff)))

(defun adler32 (bytes)
  (let ((a 1) (b 0))
    (loop for x across bytes do (setf a (mod (+ a x) 65521) b (mod (+ b a) 65521)))
    (logior (ash b 16) a)))

;;; ---- PNG ----------------------------------------------------------------
(defun u32be (v) (vector (ldb (byte 8 24) v) (ldb (byte 8 16) v) (ldb (byte 8 8) v) (ldb (byte 8 0) v)))

;;; ---- DEFLATE (fixed-Huffman + greedy LZ77) -------------------------------
;;; A real compressor so the raster PNG is small at the source (a text page's IDAT
;;; goes from ~raw-RGB to a fraction), rather than shipping tens of MB and leaning on
;;; HTTP zstd — which a non-zstd client never gets, leaving an empty image.

(defparameter *len-base*
  #(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99 115 131 163 195 227 258))
(defparameter *len-extra*
  #(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0))
(defparameter *dist-base*
  #(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769 1025 1537 2049 3073 4097 6145 8193 12289 16385 24577))
(defparameter *dist-extra*
  #(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13))

(defstruct (bitw (:constructor make-bitw ()))
  (buf (make-array 4096 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
  (acc 0 :type (unsigned-byte 32)) (nbits 0 :type fixnum))

(declaim (inline bw-bits bw-code))
(defun bw-bits (bw val n)
  "Write the low N bits of VAL to BW, least-significant bit first (DEFLATE stream order)."
  (setf (bitw-acc bw) (logior (bitw-acc bw) (ash (logand val (1- (ash 1 n))) (bitw-nbits bw))))
  (incf (bitw-nbits bw) n)
  (loop while (>= (bitw-nbits bw) 8) do
    (vector-push-extend (logand (bitw-acc bw) #xff) (bitw-buf bw))
    (setf (bitw-acc bw) (ash (bitw-acc bw) -8))
    (decf (bitw-nbits bw) 8)))

(defun bw-code (bw code len)
  "Write a Huffman CODE of LEN bits, most-significant bit first (Huffman packing order)."
  (let ((r 0)) (dotimes (i len) (setf r (logior (ash r 1) (logand (ash code (- i)) 1))))
    (bw-bits bw r len)))

(defun bw-flush (bw)
  (when (plusp (bitw-nbits bw))
    (vector-push-extend (logand (bitw-acc bw) #xff) (bitw-buf bw))
    (setf (bitw-acc bw) 0 (bitw-nbits bw) 0)))

(defun fixed-lit (bw sym)
  "Emit literal/length symbol SYM (0-287) with the fixed Huffman table (RFC 1951 3.2.6)."
  (cond ((<= sym 143) (bw-code bw (+ #x30 sym) 8))
        ((<= sym 255) (bw-code bw (+ #x190 (- sym 144)) 9))
        ((<= sym 279) (bw-code bw (- sym 256) 7))
        (t (bw-code bw (+ #xc0 (- sym 280)) 8))))

(defun emit-match (bw len dist)
  (let ((li (loop for i from 28 downto 0 when (>= len (aref *len-base* i)) return i)))
    (fixed-lit bw (+ 257 li))
    (when (plusp (aref *len-extra* li)) (bw-bits bw (- len (aref *len-base* li)) (aref *len-extra* li))))
  (let ((di (loop for i from 29 downto 0 when (>= dist (aref *dist-base* i)) return i)))
    (bw-code bw di 5)
    (when (plusp (aref *dist-extra* di)) (bw-bits bw (- dist (aref *dist-base* di)) (aref *dist-extra* di)))))

(defun deflate-fixed (raw)
  "Deflate RAW into a single fixed-Huffman block with greedy LZ77 (hash-chain matcher)."
  (declare (type (simple-array (unsigned-byte 8) (*)) raw))
  (let* ((n (length raw)) (bw (make-bitw))
         (hbits 15) (hsize (ash 1 hbits)) (hmask (1- hsize))
         (head (make-array hsize :element-type 'fixnum :initial-element -1))
         (prev (make-array (max 1 n) :element-type 'fixnum :initial-element -1)))
    (bw-bits bw 1 1) (bw-bits bw 1 2)   ; BFINAL=1, BTYPE=01 (fixed Huffman)
    (labels ((h3 (i) (logand (logxor (ash (aref raw i) 5) (ash (aref raw (+ i 1)) 2) (aref raw (+ i 2))) hmask))
             (mlen (a b) (let ((l 0) (mx (min 258 (- n b))))
                           (loop while (and (< l mx) (= (aref raw (+ a l)) (aref raw (+ b l)))) do (incf l))
                           l))
             (insert (i) (when (< (+ i 2) n) (let ((h (h3 i))) (setf (aref prev i) (aref head h) (aref head h) i)))))
      (let ((i 0))
        (loop while (< i n) do
          (let ((best 0) (bdist 0))
            (when (< (+ i 2) n)
              (let ((cand (aref head (h3 i))) (chain 0))
                (loop while (and (>= cand 0) (< chain 32) (<= (- i cand) 32768)) do
                  (when (and (< best 258) (< (+ i best) n)                    ; cheap reject:
                             (= (aref raw (+ cand best)) (aref raw (+ i best)))) ; extend only if it could beat BEST
                    (let ((l (mlen cand i))) (when (> l best) (setf best l bdist (- i cand)))))
                  (setf cand (aref prev cand)) (incf chain))))
            (cond ((>= best 3)
                   (emit-match bw best bdist)
                   (loop for k from i below (min n (+ i best)) do (insert k))
                   (incf i best))
                  (t (insert i) (fixed-lit bw (aref raw i)) (incf i)))))))
    (fixed-lit bw 256)   ; end of block
    (bw-flush bw)
    (bitw-buf bw)))

(defun zlib-compress (raw)
  "Wrap RAW in a zlib stream, DEFLATE-compressed (fixed Huffman)."
  (let ((out (make-array 4096 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (rr (coerce raw '(simple-array (unsigned-byte 8) (*)))))
    (vector-push-extend #x78 out) (vector-push-extend #x9c out)   ; zlib: deflate, default window
    (loop for b across (deflate-fixed rr) do (vector-push-extend b out))
    (let ((ad (adler32 rr))) (loop for s in '(24 16 8 0) do (vector-push-extend (ldb (byte 8 s) ad) out)))
    out))

(defun zlib-store (raw)
  "Wrap RAW bytes in a zlib stream using deflate stored blocks."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (n (length raw)))
    (flet ((push8 (b) (vector-push-extend b out)))
      (push8 #x78) (push8 #x01)                    ; zlib header (no compression)
      (let ((i 0))
        (loop while (< i n) do
          (let* ((blk (min 65535 (- n i))) (final (if (>= (+ i blk) n) 1 0)))
            (push8 final)                          ; BFINAL, BTYPE=00
            (push8 (ldb (byte 8 0) blk)) (push8 (ldb (byte 8 8) blk))
            (push8 (ldb (byte 8 0) (lognot blk))) (push8 (ldb (byte 8 8) (lognot blk)))
            (loop for k from i below (+ i blk) do (push8 (aref raw k)))
            (incf i blk))))
      (let ((ad (adler32 raw))) (loop for s in '(24 16 8 0) do (push8 (ldb (byte 8 s) ad)))))
    out))

(defun png-chunk (out type data)
  "Append a PNG chunk (type is a 4-char string) to adjustable vector OUT."
  (let* ((tbytes (map '(vector (unsigned-byte 8)) #'char-code type))
         (payload (concatenate '(vector (unsigned-byte 8)) tbytes data)))
    (loop for b across (u32be (length data)) do (vector-push-extend b out))
    (loop for b across payload do (vector-push-extend b out))
    (loop for b across (u32be (crc32 payload)) do (vector-push-extend b out))))

(defun canvas->png (cv)
  "Encode CANVAS CV to a truecolor PNG as an in-memory octet vector."
  (let* ((w (canvas-width cv)) (hh (canvas-height cv)) (px (canvas-pixels cv))
         ;; scanlines: filter byte 0 + RGB row
         (raw (make-array (* hh (1+ (* w 3))) :element-type '(unsigned-byte 8)))
         (ri 0))
    (dotimes (y hh)
      (setf (aref raw ri) 0) (incf ri)
      (let ((base (* y w 3)))
        (dotimes (k (* w 3)) (setf (aref raw ri) (aref px (+ base k))) (incf ri))))
    (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
      (loop for b in '(137 80 78 71 13 10 26 10) do (vector-push-extend b out))   ; signature
      (png-chunk out "IHDR" (concatenate '(vector (unsigned-byte 8))
                                         (u32be w) (u32be hh) (vector 8 2 0 0 0)))
      (png-chunk out "IDAT" (coerce (zlib-compress raw) '(vector (unsigned-byte 8))))
      (png-chunk out "IEND" (make-array 0 :element-type '(unsigned-byte 8)))
      out)))

(defun write-png (cv path)
  "Write CANVAS CV to PATH as a truecolor PNG."
  (with-open-file (s path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
    (write-sequence (canvas->png cv) s))
  path)
