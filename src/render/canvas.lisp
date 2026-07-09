;;;; src/render/canvas.lisp — RGB pixel canvas, bitmap text, and a PNG encoder.
;;;;
;;;; The PNG encoder writes a truecolor (RGB8) PNG using our own fixed-Huffman
;;;; DEFLATE (greedy LZ77) + CRC32 + Adler32 — no compression library needed.
;;;; For tall page canvases the image data is compressed on several threads
;;;; (pigz-style contiguous chunks), so saving a rendered page stays cheap.
(in-package #:weft.render)

(defstruct (canvas (:constructor %make-canvas))
  width height pixels)   ; pixels: (unsigned-byte 8) vector, w*h*3, row-major RGB

(defun make-canvas (w h &optional (bg '(255 255 255)))
  (let* ((n (* w h 3))
         (px (make-array n :element-type '(unsigned-byte 8)))
         (r (first bg)) (g (second bg)) (b (third bg)))
    (if (and (= r g) (= g b))
        (fill px r)                     ; uniform background (e.g. white) — one bulk fill
        (locally (declare (type (simple-array (unsigned-byte 8) (*)) px)
                          (type fixnum n) (type (unsigned-byte 8) r g b)
                          (optimize (speed 3) (safety 0)))
          (do ((i 0 (+ i 3))) ((>= i n))
            (setf (aref px i) r (aref px (+ i 1)) g (aref px (+ i 2)) b))))
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
  ;; Clip once up front and write the buffer directly, rather than a per-pixel
  ;; PUT (whose bounds + clip checks and index math dominate large background
  ;; and border fills on a tall page).
  (let ((r (first color)) (g (second color)) (b (third color))
        (cw (canvas-width cv)) (ch (canvas-height cv))
        (x0 (max 0 (round x))) (y0 (max 0 (round y)))
        (x1 (min (canvas-width cv) (round (+ x w)))) (y1 (min (canvas-height cv) (round (+ y h)))))
    (declare (ignore ch))
    (when *clip*
      (setf x0 (max x0 (the fixnum (first *clip*)))
            y0 (max y0 (the fixnum (second *clip*)))
            x1 (min x1 (the fixnum (third *clip*)))
            y1 (min y1 (the fixnum (fourth *clip*)))))
    (when (and (< x0 x1) (< y0 y1))
      (let ((px (canvas-pixels cv)))
        (declare (type (simple-array (unsigned-byte 8) (*)) px)
                 (type fixnum x0 y0 x1 y1 cw r g b)
                 (optimize (speed 3) (safety 0)))
        (loop for yy fixnum from y0 below y1 do
          (let ((i (the fixnum (* 3 (+ (the fixnum (* yy cw)) x0)))))
            (declare (type fixnum i))
            (loop for xx fixnum from x0 below x1 do
              (setf (aref px i) r (aref px (the fixnum (+ i 1)) ) g (aref px (the fixnum (+ i 2))) b)
              (incf i 3))))))))

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
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes) (type fixnum start end)
           (optimize (speed 3) (safety 0)))
  (let ((c #xffffffff) (tbl *crc-table*))
    (declare (type (unsigned-byte 32) c) (type (simple-array (unsigned-byte 32) (*)) tbl))
    (loop for i fixnum from start below end do
      (setf c (logxor (aref tbl (logand (logxor c (aref bytes i)) #xff)) (ash c -8))))
    (logxor c #xffffffff)))

(defun adler32 (bytes &optional (start 0) (end (length bytes)))
  ;; Deferred-modulo Adler-32 (zlib's NMAX chunking): accumulate up to 5552
  ;; bytes between reductions — the largest count that keeps both sums inside a
  ;; 32-bit word — instead of a modulo per byte.
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes)
           (type fixnum start end)
           (optimize (speed 3) (safety 0)))
  (let ((a 1) (b 0) (i start))
    (declare (type (unsigned-byte 32) a b) (type fixnum i))
    (loop while (< i end) do
      (let ((k (min 5552 (the fixnum (- end i)))))
        (declare (type fixnum k))
        (dotimes (j k) (incf a (aref bytes i)) (incf b a) (incf i))
        (setf a (mod a 65521) b (mod b 65521))))
    (logior (ash b 16) a)))

;;; ---- PNG ----------------------------------------------------------------
(defun u32be (v) (vector (ldb (byte 8 24) v) (ldb (byte 8 16) v) (ldb (byte 8 8) v) (ldb (byte 8 0) v)))

;;; ---- DEFLATE (fixed-Huffman + greedy LZ77) -------------------------------
;;; A real compressor so the raster PNG is small at the source (a text page's IDAT
;;; goes from ~raw-RGB to a fraction), rather than shipping tens of MB and leaning on
;;; HTTP zstd — which a non-zstd client never gets, leaving an empty image.

(defmacro %u16vec (&rest xs) `(coerce #(,@xs) '(simple-array (unsigned-byte 16) (*))))
(defparameter *len-base*
  (%u16vec 3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99 115 131 163 195 227 258))
(defparameter *len-extra*
  (%u16vec 0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0))
(defparameter *dist-base*
  (%u16vec 1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769 1025 1537 2049 3073 4097 6145 8193 12289 16385 24577))
(defparameter *dist-extra*
  (%u16vec 0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13))

;; Reverse-lookup tables so the length/distance symbol for a match is an O(1)
;; index instead of a linear scan over the base tables, and the fixed-Huffman
;; literal/length codes are pre-reversed into DEFLATE bit order (LSB-first) so
;; the inner loop writes them with a single BW-BITS.
(defun %bit-reverse (code len)
  (let ((r 0)) (dotimes (i len r) (setf r (logior (ash r 1) (logand (ash code (- i)) 1))))))

(defparameter *len-sym*
  ;; length (3..258) -> length-code index (0..28)
  (let ((tbl (make-array 259 :element-type '(unsigned-byte 8))))
    (loop for len from 3 to 258 do
      (setf (aref tbl len)
            (loop for i from 28 downto 0 when (>= len (aref *len-base* i)) return i)))
    tbl))

(defparameter *dist-sym*
  ;; distance (1..32768) -> distance-code index (0..29)
  (let ((tbl (make-array 32769 :element-type '(unsigned-byte 8))))
    (loop for d from 1 to 32768 do
      (setf (aref tbl d)
            (loop for i from 29 downto 0 when (>= d (aref *dist-base* i)) return i)))
    tbl))

(defparameter *dist-code*
  ;; distance-code index (0..29) -> pre-reversed 5-bit code
  (let ((tbl (make-array 30 :element-type '(unsigned-byte 8))))
    (dotimes (i 30 tbl) (setf (aref tbl i) (%bit-reverse i 5)))))

(defparameter *fixed-lit-code*
  ;; literal/length symbol (0..287) -> pre-reversed fixed-Huffman code (RFC 1951 3.2.6)
  (make-array 288 :element-type '(unsigned-byte 16)))
(defparameter *fixed-lit-len*
  ;; literal/length symbol (0..287) -> fixed-Huffman code length in bits
  (make-array 288 :element-type '(unsigned-byte 8)))
(dotimes (sym 288)
  (multiple-value-bind (code len)
      (cond ((<= sym 143) (values (+ #x30 sym) 8))
            ((<= sym 255) (values (+ #x190 (- sym 144)) 9))
            ((<= sym 279) (values (- sym 256) 7))
            (t (values (+ #xc0 (- sym 280)) 8)))
    (setf (aref *fixed-lit-code* sym) (%bit-reverse code len)
          (aref *fixed-lit-len* sym) len)))

(defstruct (bitw (:constructor make-bitw ()))
  (buf (make-array 4096 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
  (acc 0 :type (unsigned-byte 32)) (nbits 0 :type fixnum))

(declaim (inline bw-bits bw-code))
(defun bw-bits (bw val n)
  "Write the low N bits of VAL to BW, least-significant bit first (DEFLATE stream order)."
  (declare (type fixnum val n) (optimize (speed 3) (safety 0)))
  (setf (bitw-acc bw) (logand (logior (bitw-acc bw) (ash (logand val (1- (ash 1 n))) (bitw-nbits bw)))
                              #xffffffff))
  (incf (bitw-nbits bw) n)
  (loop while (>= (bitw-nbits bw) 8) do
    (vector-push-extend (logand (bitw-acc bw) #xff) (bitw-buf bw))
    (setf (bitw-acc bw) (ash (bitw-acc bw) -8))
    (decf (bitw-nbits bw) 8)))

(defun bw-code (bw code len)
  "Write a Huffman CODE of LEN bits, most-significant bit first (Huffman packing order)."
  (declare (type fixnum code len) (optimize (speed 3) (safety 0)))
  (let ((r 0)) (declare (type fixnum r))
    (dotimes (i len) (setf r (logior (ash r 1) (logand (ash code (- i)) 1))))
    (bw-bits bw r len)))

(defun bw-flush (bw)
  (when (plusp (bitw-nbits bw))
    (vector-push-extend (logand (bitw-acc bw) #xff) (bitw-buf bw))
    (setf (bitw-acc bw) 0 (bitw-nbits bw) 0)))

(declaim (inline fixed-lit))
(defun fixed-lit (bw sym)
  "Emit literal/length symbol SYM (0-287) with the fixed Huffman table (RFC 1951 3.2.6)."
  (declare (type (integer 0 287) sym) (optimize (speed 3) (safety 0)))
  (bw-bits bw (aref *fixed-lit-code* sym) (aref *fixed-lit-len* sym)))

(defun emit-match (bw len dist)
  (declare (type (integer 3 258) len) (type (integer 1 32768) dist)
           (optimize (speed 3) (safety 0)))
  (let ((li (aref *len-sym* len)))
    (fixed-lit bw (+ 257 li))
    (let ((ex (aref *len-extra* li)))
      (when (plusp ex) (bw-bits bw (- len (aref *len-base* li)) ex))))
  (let ((di (aref *dist-sym* dist)))
    (bw-bits bw (aref *dist-code* di) 5)
    (let ((ex (aref *dist-extra* di)))
      (when (plusp ex) (bw-bits bw (- dist (aref *dist-base* di)) ex)))))

(defun deflate-fixed (raw &key (start 0) (end (length raw)) (finalp t) (syncp nil))
  "Deflate RAW[START,END) into fixed-Huffman DEFLATE with greedy LZ77 (hash-chain
matcher), returning a byte-aligned octet vector.  FINALP sets BFINAL on the block.
When SYNCP, append an empty stored block (a zlib sync flush) so the result ends on
a byte boundary and can be concatenated with the next chunk's stream — the standard
way to stitch independently-compressed pieces (pigz)."
  (declare (type (simple-array (unsigned-byte 8) (*)) raw)
           (type fixnum start end)
           (optimize (speed 3) (safety 0)))
  (let* ((bw (make-bitw))
         (hbits 15) (hsize (ash 1 hbits)) (hmask (1- hsize))
         (head (make-array hsize :element-type 'fixnum :initial-element -1))
         ;; PREV is indexed by position-within-chunk and never read before it is
         ;; written (chains only visit positions that were previously INSERTed),
         ;; so it needs no -1 fill — skipping it avoids an n-word bash per call.
         (prev (make-array (max 1 (- end start)) :element-type 'fixnum)))
    (declare (type fixnum hmask start end)
             (type (simple-array fixnum (*)) head prev))
    (bw-bits bw (if finalp 1 0) 1) (bw-bits bw 1 2)   ; BFINAL, BTYPE=01 (fixed Huffman)
    (labels ((h3 (i) (declare (type fixnum i))
               (logand (logxor (ash (aref raw i) 5) (ash (aref raw (+ i 1)) 2) (aref raw (+ i 2))) hmask))
             (mlen (a b) (declare (type fixnum a b))
               (let ((l 0) (mx (min 258 (the fixnum (- end b))))) (declare (type fixnum l mx))
                 (loop while (and (< l mx) (= (aref raw (the fixnum (+ a l))) (aref raw (the fixnum (+ b l))))) do (incf l))
                 l))
             (insert (i) (declare (type fixnum i))
               (when (< (+ i 2) end) (let ((h (h3 i))) (setf (aref prev (- i start)) (aref head h) (aref head h) i)))))
      (declare (inline h3 insert))
      (let ((i start)) (declare (type fixnum i))
        (loop while (< i end) do
          (let ((best 0) (bdist 0)) (declare (type fixnum best bdist))
            (when (< (+ i 2) end)
              (let ((cand (aref head (h3 i))) (chain 0)) (declare (type fixnum cand chain))
                (loop while (and (>= cand 0) (< chain 32) (<= (- i cand) 32768)) do
                  (when (and (< best 258) (< (+ i best) end)                  ; cheap reject:
                             (= (aref raw (+ cand best)) (aref raw (+ i best)))) ; extend only if it could beat BEST
                    (let ((l (mlen cand i))) (declare (type fixnum l)) (when (> l best) (setf best l bdist (- i cand)))))
                  (setf cand (aref prev (- cand start))) (incf chain))))
            (cond ((>= best 3)
                   (emit-match bw best bdist)
                   (loop for k fixnum from i below (min end (+ i best)) do (insert k))
                   (incf i best))
                  (t (insert i) (fixed-lit bw (aref raw i)) (incf i)))))))
    (fixed-lit bw 256)   ; end of block
    (cond (syncp
           ;; Empty stored block: BFINAL=0, BTYPE=00, align to byte, LEN=0 NLEN=0xffff.
           (bw-bits bw 0 1) (bw-bits bw 0 2) (bw-flush bw)
           (let ((buf (bitw-buf bw)))
             (vector-push-extend 0 buf) (vector-push-extend 0 buf)
             (vector-push-extend #xff buf) (vector-push-extend #xff buf)))
          (t (bw-flush bw)))
    (bitw-buf bw)))

(defun %cpu-count ()
  "Online processor count (Linux /proc/cpuinfo), or 4 if it can't be determined."
  (or (ignore-errors
        (with-open-file (s "/proc/cpuinfo" :if-does-not-exist nil)
          (when s
            (loop for line = (read-line s nil nil) while line
                  count (and (>= (length line) 9) (string= "processor" line :end2 9))))))
      4))

(defparameter *png-deflate-threads*
  (max 1 (min 16 (- (%cpu-count) 2)))
  "Worker threads for parallel DEFLATE of the PNG image data.  A tall page's IDAT
splits into this many contiguous chunks, each compressed independently (pigz-style)
and stitched together byte-aligned.  Cross-chunk back-references are lost at the
seams, a negligible ratio cost against a near-linear speedup on large canvases.")

(defun deflate-chunks (raw nthreads)
  "Compress simple octet vector RAW into concatenated fixed-Huffman DEFLATE, split
into NTHREADS contiguous chunks compressed on worker threads (pigz-style).  Returns
one octet vector holding the whole DEFLATE stream (no zlib header/trailer)."
  (declare (type (simple-array (unsigned-byte 8) (*)) raw))
  (let* ((n (length raw))
         (nt (max 1 (min nthreads n)))
         (bounds (make-array (1+ nt) :element-type 'fixnum))
         (parts (make-array nt :initial-element nil)))
    ;; Contiguous, roughly equal chunks; boundaries anywhere are valid since the
    ;; decompressed pieces simply concatenate back to RAW.
    (dotimes (k (1+ nt)) (setf (aref bounds k) (floor (* n k) nt)))
    (let ((threads
            (loop for k below nt
                  for s = (aref bounds k) for e = (aref bounds (1+ k))
                  for last = (= k (1- nt))
                  collect (let ((kk k) (ss s) (ee e) (ll last))
                            (sb-thread:make-thread
                             (lambda ()
                               (setf (aref parts kk)
                                     (deflate-fixed raw :start ss :end ee
                                                        :finalp ll :syncp (not ll))))
                             :name "png-deflate")))))
      (dolist (th threads) (sb-thread:join-thread th)))
    (let ((total (loop for p across parts sum (length p))))
      (let ((out (make-array total :element-type '(unsigned-byte 8))) (o 0))
        (declare (type fixnum o))
        (loop for p across parts do (replace out p :start1 o) (incf o (length p)))
        out))))

(defun zlib-compress (raw)
  "Wrap RAW in a zlib stream, DEFLATE-compressed (fixed Huffman)."
  (let* ((rr (coerce raw '(simple-array (unsigned-byte 8) (*))))
         (nt *png-deflate-threads*)
         (body (if (and (> nt 1) (> (length rr) (* 2 nt)))
                   (deflate-chunks rr nt)
                   (deflate-fixed rr)))
         (blen (length body))
         (out (make-array (+ 6 blen) :element-type '(unsigned-byte 8)))
         (ad (adler32 rr)))
    (setf (aref out 0) #x78 (aref out 1) #x9c)   ; zlib: deflate, default window
    (replace out body :start1 2)
    (setf (aref out (+ 2 blen)) (ldb (byte 8 24) ad)
          (aref out (+ 3 blen)) (ldb (byte 8 16) ad)
          (aref out (+ 4 blen)) (ldb (byte 8 8) ad)
          (aref out (+ 5 blen)) (ldb (byte 8 0) ad))
    out))

(defun zlib-store (raw)
  "Wrap RAW bytes in a zlib stream using deflate stored blocks."
  (let* ((raw (coerce raw '(simple-array (unsigned-byte 8) (*))))
         (out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
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
         (rowb (* w 3)) (stride (1+ rowb))          ; scanline: filter byte 0 + RGB row
         (raw (make-array (* hh stride) :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 8) (*)) px raw)
             (type fixnum w hh rowb stride)
             (optimize (speed 3) (safety 0)))
    (dotimes (y hh)
      ;; Filter type 0 (None); bulk-copy the row rather than a per-byte loop.
      (let ((ri (the fixnum (* y stride))) (base (the fixnum (* y rowb))))
        (setf (aref raw ri) 0)
        (replace raw px :start1 (the fixnum (1+ ri)) :start2 base :end2 (the fixnum (+ base rowb)))))
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
