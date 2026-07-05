;;;; src/render/jpeg.lisp — a baseline (sequential DCT) JPEG/JFIF decoder to RGBA.
;;;;
;;;; Enough to paint real <img src="…jpg">: marker parse (DQT/SOF0/DHT/DRI/SOS),
;;;; canonical-Huffman entropy decode with byte-stuffing and restart intervals,
;;;; dequantise + de-zigzag, a separable 8x8 inverse DCT, level shift, chroma
;;;; upsampling and YCbCr->RGB.  Progressive (SOF2) and arithmetic coding are not
;;;; handled (return NIL) — the vast majority of web JPEGs are baseline Huffman.
(in-package #:weft.render)

(defun j16 (v i) (logior (ash (aref v i) 8) (aref v (+ i 1))))

;; The k-th zig-zag coefficient maps to this index in the natural 8x8 block.
(defparameter +zigzag+
  #(0 1 8 16 9 2 3 10 17 24 32 25 18 11 4 5 12 19 26 33 40 48 41 34 27 20 13 6 7 14 21 28
    35 42 49 56 57 50 43 36 29 22 15 23 30 37 44 51 58 59 52 45 38 31 39 46 53 60 61 54 47 55 62 63))

;; idct-cos[x][u] = C(u) * cos((2x+1) u pi / 16), C(0)=1/sqrt2 else 1.
(defparameter +idct-cos+
  (let ((tbl (make-array '(8 8) :element-type 'double-float))
        (pd (coerce pi 'double-float)))
    (dotimes (x 8)
      (dotimes (u 8)
        (setf (aref tbl x u)
              (* (if (zerop u) (/ 1d0 (sqrt 2d0)) 1d0)
                 (cos (/ (* (+ (* 2 x) 1) u pd) 16d0))))))
    tbl))

;;; ---- canonical Huffman ---------------------------------------------------
(defstruct jhuff mincode maxcode valptr symbols)

(defun build-huff (counts symbols)
  "Build a decode table from the 16 per-length COUNTS and the flat SYMBOLS."
  (let ((mincode (make-array 17 :initial-element 0))
        (maxcode (make-array 17 :initial-element -1))
        (valptr (make-array 17 :initial-element 0))
        (code 0) (k 0))
    (loop for l from 1 to 16 for c = (aref counts (1- l)) do
      (when (plusp c)
        (setf (aref valptr l) k (aref mincode l) code)
        (incf code c) (incf k c)
        (setf (aref maxcode l) (1- code)))
      (setf code (ash code 1)))
    (make-jhuff :mincode mincode :maxcode maxcode :valptr valptr :symbols symbols)))

;;; ---- entropy bit reader (byte-stuffing + restart markers) ----------------
(defstruct jbr data (pos 0) (cnt 0) (buf 0) (eof nil))

(defun jbr-next-bit (br)
  "Next entropy bit; 0 once a marker is reached (marker left at POS)."
  (when (zerop (jbr-cnt br))
    (let ((data (jbr-data br)) (n (length (jbr-data br))))
      (when (>= (jbr-pos br) n) (setf (jbr-eof br) t) (return-from jbr-next-bit 0))
      (let ((b (aref data (jbr-pos br))))
        (incf (jbr-pos br))
        (when (= b #xff)
          (let ((b2 (if (< (jbr-pos br) n) (aref data (jbr-pos br)) #xd9)))
            (cond ((= b2 0) (incf (jbr-pos br)))        ; stuffed FF00 -> data byte FF
                  (t (decf (jbr-pos br))                ; a marker: leave FF.. for the caller
                     (setf (jbr-eof br) t)
                     (return-from jbr-next-bit 0)))))
        (setf (jbr-buf br) b (jbr-cnt br) 8))))
  (decf (jbr-cnt br))
  (logand (ash (jbr-buf br) (- (jbr-cnt br))) 1))

(defun jbr-restart (br)
  "Byte-align and consume the next RSTn (FF D0-D7) marker at a restart boundary."
  (setf (jbr-cnt br) 0 (jbr-eof br) nil)
  (let ((data (jbr-data br)) (n (length (jbr-data br))))
    (loop while (< (jbr-pos br) n) do
      (if (and (= (aref data (jbr-pos br)) #xff)
               (< (1+ (jbr-pos br)) n)
               (<= #xd0 (aref data (1+ (jbr-pos br))) #xd7))
          (progn (incf (jbr-pos br) 2) (return))
          (incf (jbr-pos br))))))

(defun jbr-receive (br n)
  (let ((v 0)) (dotimes (i n v) (setf v (logior (ash v 1) (jbr-next-bit br))))))

(defun jhuff-extend (v n)
  "Sign-extend an N-bit magnitude V (JPEG EXTEND)."
  (if (and (plusp n) (< v (ash 1 (1- n)))) (+ v 1 (- (ash 1 n))) v))

(defun jhuff-decode (br huff)
  (let ((code 0))
    (loop for l from 1 to 16 do
      (setf code (logior (ash code 1) (jbr-next-bit br)))
      (when (and (>= (aref (jhuff-maxcode huff) l) 0) (<= code (aref (jhuff-maxcode huff) l)))
        (return-from jhuff-decode
          (aref (jhuff-symbols huff)
                (+ (aref (jhuff-valptr huff) l) (- code (aref (jhuff-mincode huff) l)))))))
    0))

;;; ---- block decode + IDCT -------------------------------------------------
(defun decode-block (br dc-huff ac-huff qtable pred block)
  "Decode one 8x8 block into BLOCK (64 doubles, natural order, dequantised).
   Returns the new DC predictor."
  (fill block 0d0)
  (let* ((s (jhuff-decode br dc-huff))
         (dc (+ pred (if (plusp s) (jhuff-extend (jbr-receive br s) s) 0))))
    (setf (aref block 0) (* (coerce dc 'double-float) (aref qtable 0)))
    (let ((k 1))
      (loop while (< k 64) do
        (let* ((rs (jhuff-decode br ac-huff)) (r (ash rs -4)) (sz (logand rs 15)))
          (cond ((zerop sz) (if (= r 15) (incf k 16) (return)))   ; ZRL (16 zeros) or EOB
                (t (incf k r)
                   (when (< k 64)
                     (setf (aref block (aref +zigzag+ k))
                           (* (coerce (jhuff-extend (jbr-receive br sz) sz) 'double-float)
                              (aref qtable k)))
                     (incf k)))))))
    dc))

(defun idct-8x8 (block out)
  "Separable inverse DCT of the 64-double coefficient BLOCK into OUT (spatial)."
  (let ((tmp (make-array 64 :element-type 'double-float)))
    (dotimes (r 8)                                   ; rows
      (dotimes (x 8)
        (let ((sum 0d0))
          (dotimes (u 8) (incf sum (* (aref block (+ (* r 8) u)) (aref +idct-cos+ x u))))
          (setf (aref tmp (+ (* r 8) x)) (* 0.5d0 sum)))))
    (dotimes (c 8)                                   ; columns
      (dotimes (y 8)
        (let ((sum 0d0))
          (dotimes (v 8) (incf sum (* (aref tmp (+ (* v 8) c)) (aref +idct-cos+ y v))))
          (setf (aref out (+ (* y 8) c)) (* 0.5d0 sum)))))))

(declaim (inline clamp8))
(defun clamp8 (x) (cond ((< x 0d0) 0) ((> x 255d0) 255) (t (round x))))

;;; ---- top level -----------------------------------------------------------
(defstruct jcomp id h v qsel plane cw ch (pred 0))

(defun jpeg-decode (bytes)
  "Decode a baseline JPEG byte vector to an IMG (RGBA), or NIL."
  (when (and (>= (length bytes) 3) (= (aref bytes 0) #xff) (= (aref bytes 1) #xd8))
    (let ((n (length bytes)) (i 2)
          (qt (make-array 4 :initial-element nil))     ; 4 quant tables, zig-zag order
          (dht (make-array 4 :initial-element nil)) (aht (make-array 4 :initial-element nil))
          (w 0) (h 0) (comps nil) (ri 0) (scan nil) (progressive nil))
      (loop
        (loop while (and (< i n) (/= (aref bytes i) #xff)) do (incf i))
        (loop while (and (< i n) (= (aref bytes i) #xff)) do (incf i))   ; fill FFs
        (when (>= i n) (return))
        (let ((m (aref bytes i))) (incf i)
          (cond
            ((= m #xd9) (return))                                        ; EOI
            ((<= #xd0 m #xd7))                                           ; stray RSTn
            ((= m #xdb)                                                  ; DQT
             (let ((len (j16 bytes i)) (p (+ i 2)))
               (loop while (< p (+ i len)) do
                 (let* ((pq/tq (aref bytes p)) (prec (ash pq/tq -4)) (id (logand pq/tq 15))
                        (tbl (make-array 64)))
                   (incf p)
                   (dotimes (k 64) (setf (aref tbl k) (if (zerop prec) (aref bytes (+ p k))
                                                          (j16 bytes (+ p (* k 2))))))
                   (incf p (if (zerop prec) 64 128))
                   (setf (aref qt id) tbl)))
               (incf i len)))
            ((or (= m #xc0) (= m #xc1))                                  ; SOF0/1 baseline
             (let ((nc (aref bytes (+ i 7))))
               (setf h (j16 bytes (+ i 3)) w (j16 bytes (+ i 5)))
               (dotimes (c nc)
                 (let ((o (+ i 8 (* c 3))))
                   (push (make-jcomp :id (aref bytes o)
                                     :h (ash (aref bytes (+ o 1)) -4)
                                     :v (logand (aref bytes (+ o 1)) 15)
                                     :qsel (aref bytes (+ o 2))) comps)))
               (setf comps (nreverse comps))
               (incf i (j16 bytes i))))
            ((or (= m #xc2) (= m #xc3) (<= #xc5 m #xcf))                 ; progressive/other SOF
             (setf progressive t) (incf i (j16 bytes i)))
            ((= m #xc4)                                                  ; DHT
             (let ((len (j16 bytes i)) (p (+ i 2)))
               (loop while (< p (+ i len)) do
                 (let* ((tc/th (aref bytes p)) (class (ash tc/th -4)) (id (logand tc/th 15))
                        (counts (make-array 16)) (total 0))
                   (incf p)
                   (dotimes (l 16) (setf (aref counts l) (aref bytes (+ p l))) (incf total (aref bytes (+ p l))))
                   (incf p 16)
                   (let ((syms (subseq bytes p (+ p total))))
                     (incf p total)
                     (if (zerop class) (setf (aref dht id) (build-huff counts syms))
                         (setf (aref aht id) (build-huff counts syms))))))
               (incf i len)))
            ((= m #xdd) (setf ri (j16 bytes (+ i 2))) (incf i (j16 bytes i)))   ; DRI
            ((= m #xda)                                                  ; SOS
             (let* ((len (j16 bytes i)) (ns (aref bytes (+ i 2))))
               (setf scan (loop for c below ns
                                for o = (+ i 3 (* c 2))
                                collect (cons (aref bytes o) (aref bytes (+ o 1)))))
               (incf i len)
               (return)))                                                ; entropy data at I
            (t (incf i (j16 bytes i))))))                                ; APPn/COM/…
      (when (or progressive (zerop w) (zerop h) (null comps) (null scan)) (return-from jpeg-decode nil))
      ;; ---- decode the single baseline scan ----
      (let* ((hmax (reduce #'max comps :key #'jcomp-h)) (vmax (reduce #'max comps :key #'jcomp-v))
             (mx (ceiling w (* 8 hmax))) (my (ceiling h (* 8 vmax)))
             (br (make-jbr :data bytes :pos i))
             (block (make-array 64 :element-type 'double-float))
             (spatial (make-array 64 :element-type 'double-float)))
        (dolist (c comps)
          (setf (jcomp-cw c) (* mx (jcomp-h c) 8) (jcomp-ch c) (* my (jcomp-v c) 8)
                (jcomp-plane c) (make-array (* (jcomp-cw c) (jcomp-ch c))
                                            :element-type '(unsigned-byte 8) :initial-element 0)))
        (let ((mcu 0))
          (dotimes (myi my)
            (dotimes (mxi mx)
              (dolist (c comps)
                (let* ((sc (find (jcomp-id c) scan :key #'car))
                       (dh (aref dht (ash (cdr sc) -4))) (ah (aref aht (logand (cdr sc) 15)))
                       (qtab (aref qt (jcomp-qsel c))) (cw (jcomp-cw c)) (plane (jcomp-plane c)))
                  (dotimes (byi (jcomp-v c))
                    (dotimes (bxi (jcomp-h c))
                      (setf (jcomp-pred c) (decode-block br dh ah qtab (jcomp-pred c) block))
                      (idct-8x8 block spatial)
                      (let ((col (* (+ (* mxi (jcomp-h c)) bxi) 8)) (row (* (+ (* myi (jcomp-v c)) byi) 8)))
                        (dotimes (yy 8)
                          (dotimes (xx 8)
                            (setf (aref plane (+ (* (+ row yy) cw) col xx))
                                  (clamp8 (+ (aref spatial (+ (* yy 8) xx)) 128d0))))))))))
              (incf mcu)
              (when (and (plusp ri) (zerop (mod mcu ri)) (not (and (= mxi (1- mx)) (= myi (1- my)))))
                (jbr-restart br) (dolist (c comps) (setf (jcomp-pred c) 0))))))
        ;; ---- compose RGBA (upsample chroma by replication) ----
        (let ((out (make-array (* w h 4) :element-type '(unsigned-byte 8)))
              (c0 (first comps)) (c1 (second comps)) (c2 (third comps)))
          (dotimes (py h)
            (dotimes (px w)
              (flet ((samp (c) (aref (jcomp-plane c)
                                     (+ (* (floor (* py (jcomp-v c)) vmax) (jcomp-cw c))
                                        (floor (* px (jcomp-h c)) hmax)))))
                (let ((o (* (+ (* py w) px) 4)))
                  (if (and c1 c2)
                      (let* ((yy (samp c0)) (cb (- (samp c1) 128)) (cr (- (samp c2) 128)))
                        (setf (aref out o)       (clamp8 (+ yy (* 1.402d0 cr)))
                              (aref out (+ o 1)) (clamp8 (- yy (* 0.344136d0 cb) (* 0.714136d0 cr)))
                              (aref out (+ o 2)) (clamp8 (+ yy (* 1.772d0 cb)))))
                      (let ((g (samp c0))) (setf (aref out o) g (aref out (+ o 1)) g (aref out (+ o 2)) g)))
                  (setf (aref out (+ o 3)) 255)))))
          (make-img :w w :h h :rgba out))))))
