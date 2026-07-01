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

(defun write-png (cv path)
  "Write CANVAS CV to PATH as a truecolor PNG."
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
      (png-chunk out "IDAT" (coerce (zlib-store raw) '(vector (unsigned-byte 8))))
      (png-chunk out "IEND" (make-array 0 :element-type '(unsigned-byte 8)))
      (with-open-file (s path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
        (write-sequence out s)))
    path))
