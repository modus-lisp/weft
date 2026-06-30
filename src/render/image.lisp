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
    (let ((i 8) (n (length bytes)) w h depth ctype
          (idat (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
          (plte nil) (trns nil))
      (loop while (< (+ i 8) n) do
        (let* ((len (be32 bytes i)) (type (map 'string #'code-char (subseq bytes (+ i 4) (+ i 8))))
               (ds (+ i 8)))
          (cond
            ((string= type "IHDR")
             (setf w (be32 bytes ds) h (be32 bytes (+ ds 4)) depth (aref bytes (+ ds 8)) ctype (aref bytes (+ ds 9))))
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
                    plte trns)))))

(defun only-supported-p (depth ctype) (and (= depth 8) (member ctype '(0 2 3 4 6))))

(defun channels (ctype) (ecase ctype (0 1) (2 3) (3 1) (4 2) (6 4)))

(defun png-finish (w h depth ctype idat plte trns)
  (declare (ignore depth))
  (let* ((ch (channels ctype)) (stride (* w ch))
         (raw (handler-case (chipz:decompress nil 'chipz:zlib idat) (error () nil))))
    (when (and raw (>= (length raw) (* h (1+ stride))))
      (let ((rows (make-array (* h stride) :element-type '(unsigned-byte 8))) (rp 0) (ri 0))
        ;; unfilter
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
        ;; expand to RGBA
        (let ((rgba (make-array (* w h 4) :element-type '(unsigned-byte 8))))
          (dotimes (p (* w h))
            (let ((s (* p ch)) (d (* p 4)))
              (ecase ctype
                (0 (let ((g (aref rows s))) (setf (aref rgba d) g (aref rgba (+ d 1)) g (aref rgba (+ d 2)) g (aref rgba (+ d 3)) 255)))
                (2 (setf (aref rgba d) (aref rows s) (aref rgba (+ d 1)) (aref rows (+ s 1)) (aref rgba (+ d 2)) (aref rows (+ s 2)) (aref rgba (+ d 3)) 255))
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

;;; ---- entry -------------------------------------------------------------
(defun decode-image (uri)
  "Decode a data: URI (PNG or GIF) to an IMG, or NIL."
  (multiple-value-bind (mime bytes) (parse-data-uri uri)
    (when bytes
      (cond ((search "png" (or mime "") :test #'char-equal) (png-decode bytes))
            ((search "gif" (or mime "") :test #'char-equal) (gif-decode bytes))
            ((and (>= (length bytes) 2) (= (aref bytes 0) 137)) (png-decode bytes))
            (t nil)))))

(defun blit-img (cv img x y &optional dw dh)
  "Paint IMG onto CV at (X,Y), straight-alpha over existing pixels, optionally
scaled to (DW,DH) by nearest-neighbour."
  (let* ((iw (img-w img)) (ih (img-h img)) (rgba (img-rgba img))
         (ow (or dw iw)) (oh (or dh ih)))
    (dotimes (oy oh)
      (dotimes (ox ow)
        (let* ((sx (min (1- iw) (floor (* ox iw) ow))) (sy (min (1- ih) (floor (* oy ih) oh)))
               (si (* 4 (+ (* sy iw) sx))) (a (aref rgba (+ si 3))))
          (when (plusp a)
            (let ((px (+ x ox)) (py (+ y oy)))
              (if (>= a 255)
                  (put cv px py (aref rgba si) (aref rgba (+ si 1)) (aref rgba (+ si 2)))
                  (when (and (>= px 0) (>= py 0) (< px (canvas-width cv)) (< py (canvas-height cv)))
                    (let* ((di (* 3 (+ (* py (canvas-width cv)) px))) (pb (canvas-pixels cv)) (ia (- 255 a)))
                      (setf (aref pb di) (floor (+ (* (aref rgba si) a) (* (aref pb di) ia)) 255)
                            (aref pb (+ di 1)) (floor (+ (* (aref rgba (+ si 1)) a) (* (aref pb (+ di 1)) ia)) 255)
                            (aref pb (+ di 2)) (floor (+ (* (aref rgba (+ si 2)) a) (* (aref pb (+ di 2)) ia)) 255))))))))))))
