;;;; src/render/jpeg.lisp — a pure-CL JPEG/JFIF decoder to RGBA (baseline + progressive).
;;;;
;;;; Decodes real <img src="…jpg">: marker parse (DQT/SOF0/SOF2/DHT/DRI/SOS),
;;;; canonical-Huffman entropy decode with byte-stuffing and restart intervals,
;;;; and — for both sequential (baseline) and PROGRESSIVE images — accumulates the
;;;; DCT coefficients across every scan (spectral selection + successive
;;;; approximation) before a single dequantise / 8x8 inverse DCT / level-shift /
;;;; chroma-upsample / YCbCr->RGB pass.  Because the frame's SOF dimensions are
;;;; fixed up front and every scan only refines coefficients, the result is always
;;;; the full intrinsic size — a partial/truncated progressive stream yields a
;;;; lower-quality image at the SAME size, so the layout box never resizes.
;;;; Arithmetic coding is not handled.
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

;;; ---- per-block coefficient decoders --------------------------------------
;;; COEF is a component's flat (signed-byte 16) coefficient buffer (natural order,
;;; one 64-slot block per grid cell); OFF is this block's base index.  EOB is a
;;; one-element list holding the shared end-of-band run counter for AC scans.

(defun decode-baseline-block (br dc-huff ac-huff comp coef off)
  "Sequential (baseline) block: DC diff + the full AC run, no bit shifting."
  (let* ((s (jhuff-decode br dc-huff))
         (diff (if (plusp s) (jhuff-extend (jbr-receive br s) s) 0)))
    (incf (jcomp-pred comp) diff)
    (setf (aref coef off) (jcomp-pred comp))
    (let ((k 1))
      (loop while (< k 64) do
        (let* ((rs (jhuff-decode br ac-huff)) (r (ash rs -4)) (sz (logand rs 15)))
          (cond ((zerop sz) (if (= r 15) (incf k 16) (return)))     ; ZRL / EOB
                (t (incf k r)
                   (when (< k 64)
                     (setf (aref coef (+ off (aref +zigzag+ k))) (jhuff-extend (jbr-receive br sz) sz))
                     (incf k)))))))))

(defun decode-dc-first (br dc-huff comp coef off al)
  (let* ((s (jhuff-decode br dc-huff))
         (diff (if (plusp s) (jhuff-extend (jbr-receive br s) s) 0)))
    (incf (jcomp-pred comp) diff)
    (setf (aref coef off) (ash (jcomp-pred comp) al))))

(defun decode-dc-refine (br coef off al)
  (when (= (jbr-next-bit br) 1)
    (setf (aref coef off) (logior (aref coef off) (ash 1 al)))))

(defun decode-ac-first (br ac-huff coef off ss se al eob)
  "Progressive first AC scan for one band [Ss,Se], approximation bit Al."
  (if (plusp (car eob))
      (decf (car eob))
      (let ((k ss))
        (loop while (<= k se) do
          (let* ((rs (jhuff-decode br ac-huff)) (r (ash rs -4)) (sz (logand rs 15)))
            (cond ((zerop sz)
                   (if (< r 15)
                       (progn (setf (car eob) (1- (ash 1 r)))
                              (when (plusp r) (incf (car eob) (jbr-receive br r)))
                              (return))
                       (incf k 16)))                       ; ZRL: 16 zeros
                  (t (incf k r)
                     (when (<= k se)
                       (setf (aref coef (+ off (aref +zigzag+ k)))
                             (ash (jhuff-extend (jbr-receive br sz) sz) al))
                       (incf k)))))))))

(defun decode-ac-refine (br ac-huff coef off ss se al eob)
  "Progressive AC refinement scan: correction bits for already-nonzero
   coefficients plus newly-significant ones (JPEG Annex G.1.2.3)."
  (let ((p1 (ash 1 al)) (m1 (- (ash 1 al))) (k ss))
    (when (zerop (car eob))
      (loop while (<= k se) do
        (let* ((rs (jhuff-decode br ac-huff)) (r (ash rs -4)) (sz (logand rs 15)) (val 0))
          (cond ((zerop sz)
                 (when (< r 15)
                   (setf (car eob) (ash 1 r))
                   (when (plusp r) (incf (car eob) (jbr-receive br r)))
                   (return)))
                (t (setf val (if (= (jbr-next-bit br) 1) p1 m1))))   ; sz is 1: a new coefficient
          ;; advance over R zero-history coefficients, refining nonzero ones met
          (loop while (<= k se) do
            (let ((idx (+ off (aref +zigzag+ k))))
              (if (/= (aref coef idx) 0)
                  (when (and (= (jbr-next-bit br) 1) (zerop (logand (aref coef idx) p1)))
                    (incf (aref coef idx) (if (plusp (aref coef idx)) p1 m1)))
                  (if (zerop r) (return) (decf r))))
            (incf k))
          (when (and (/= sz 0) (<= k se))
            (setf (aref coef (+ off (aref +zigzag+ k))) val))
          (incf k))))
    ;; within an EOB run: refine every remaining nonzero coefficient
    (when (plusp (car eob))
      (loop while (<= k se) do
        (let ((idx (+ off (aref +zigzag+ k))))
          (when (and (/= (aref coef idx) 0)
                     (= (jbr-next-bit br) 1) (zerop (logand (aref coef idx) p1)))
            (incf (aref coef idx) (if (plusp (aref coef idx)) p1 m1))))
        (incf k))
      (decf (car eob)))))

;;; ---- inverse DCT ---------------------------------------------------------
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
(defstruct jcomp id h v qsel coef bpl bh (pred 0))   ; coef: natural-order (s16) blocks

(defun jpeg-size (bytes)
  "Read a JPEG's intrinsic (values WIDTH HEIGHT) from its SOF marker without
   decoding the pixels — so a box can be reserved before/without a full decode."
  (when (and (>= (length bytes) 3) (= (aref bytes 0) #xff) (= (aref bytes 1) #xd8))
    (let ((n (length bytes)) (i 2))
      (loop
        (loop while (and (< i n) (/= (aref bytes i) #xff)) do (incf i))
        (loop while (and (< i n) (= (aref bytes i) #xff)) do (incf i))
        (when (>= i (- n 4)) (return))
        (let ((m (aref bytes i))) (incf i)
          (cond ((or (<= #xc0 m #xc3) (<= #xc5 m #xc7) (<= #xc9 m #xcb) (<= #xcd m #xcf))  ; any SOF
                 (return-from jpeg-size (values (j16 bytes (+ i 5)) (j16 bytes (+ i 3)))))
                ((or (= m #xd8) (= m #xd9) (<= #xd0 m #xd7)))       ; standalone markers
                (t (incf i (j16 bytes i)))))))
    nil))

(defun jpeg-decode (bytes)
  "Decode a baseline OR progressive JPEG byte vector to an IMG (RGBA), or NIL."
  (when (and (>= (length bytes) 3) (= (aref bytes 0) #xff) (= (aref bytes 1) #xd8))
    (let ((n (length bytes)) (i 2)
          (qt (make-array 4 :initial-element nil))     ; 4 quant tables, NATURAL order
          (dht (make-array 4 :initial-element nil)) (aht (make-array 4 :initial-element nil))
          (w 0) (h 0) (comps nil) (ri 0) (hmax 1) (vmax 1) (mx 0) (my 0)
          (br nil) (spatial (make-array 64 :element-type 'double-float))
          (fblock (make-array 64 :element-type 'double-float)))
      (labels
          ((setup-frame ()
             (setf hmax (reduce #'max comps :key #'jcomp-h) vmax (reduce #'max comps :key #'jcomp-v)
                   mx (ceiling w (* 8 hmax)) my (ceiling h (* 8 vmax)))
             (dolist (c comps)
               (setf (jcomp-bpl c) (* mx (jcomp-h c)) (jcomp-bh c) (* my (jcomp-v c))
                     (jcomp-coef c) (make-array (* (jcomp-bpl c) (jcomp-bh c) 64)
                                                :element-type '(signed-byte 16) :initial-element 0))))
           (decode-scan (sos-i)
             ;; parse the SOS header at SOS-I, then decode its entropy segment.
             (let* ((ns (aref bytes (+ sos-i 2)))
                    (sc (loop for k below ns for o = (+ sos-i 3 (* k 2))
                              for c = (find (aref bytes o) comps :key #'jcomp-id)
                              collect (list c (ash (aref bytes (+ o 1)) -4) (logand (aref bytes (+ o 1)) 15))))
                    (p (+ sos-i 3 (* ns 2)))
                    (ss (aref bytes p)) (se (aref bytes (+ p 1)))
                    (ah (ash (aref bytes (+ p 2)) -4)) (al (logand (aref bytes (+ p 2)) 15)))
               (setf (jbr-pos br) (+ p 3) (jbr-cnt br) 0 (jbr-eof br) nil)
               (dolist (e sc) (setf (jcomp-pred (first e)) 0))
               (if (plusp ss)
                   ;; ---- AC scan: single component, non-interleaved block order ----
                   (destructuring-bind (c dc-id ac-id) (first sc)
                     (declare (ignore dc-id))
                     (let* ((ach (aref aht ac-id)) (eob (list 0)) (cnt 0)
                            (bw (ceiling (ceiling (* w (jcomp-h c)) hmax) 8))
                            (bh (ceiling (ceiling (* h (jcomp-v c)) vmax) 8)))
                       (dotimes (by bh)
                         (dotimes (bx bw)
                           (let ((off (* (+ (* by (jcomp-bpl c)) bx) 64)))
                             (if (zerop ah) (decode-ac-first br ach (jcomp-coef c) off ss se al eob)
                                 (decode-ac-refine br ach (jcomp-coef c) off ss se al eob)))
                           (incf cnt)
                           (when (and (plusp ri) (zerop (mod cnt ri)) (< cnt (* bw bh)))
                             (jbr-restart br) (setf (car eob) 0))))))
                   ;; ---- DC / baseline scan: interleaved MCU order ----
                   (let ((mcu 0))
                     (dotimes (myi my)
                       (dotimes (mxi mx)
                         (dolist (e sc)
                           (destructuring-bind (c dc-id ac-id) e
                             (let ((dch (aref dht dc-id)) (ach (aref aht ac-id)) (coef (jcomp-coef c)))
                               (dotimes (byi (jcomp-v c))
                                 (dotimes (bxi (jcomp-h c))
                                   (let ((off (* (+ (* (+ (* myi (jcomp-v c)) byi) (jcomp-bpl c))
                                                    (+ (* mxi (jcomp-h c)) bxi)) 64)))
                                     (cond ((and (= se 63) (zerop ah)) (decode-baseline-block br dch ach c coef off))
                                           ((zerop ah) (decode-dc-first br dch c coef off al))
                                           (t (decode-dc-refine br coef off al)))))))))
                         (incf mcu)
                         (when (and (plusp ri) (zerop (mod mcu ri)) (< mcu (* mx my)))
                           (jbr-restart br) (dolist (e sc) (setf (jcomp-pred (first e)) 0))))))))))
        ;; ---- marker loop (parses tables + drives every scan) ----
        (block markers
          (loop
            (loop while (and (< i n) (/= (aref bytes i) #xff)) do (incf i))
            (loop while (and (< i n) (= (aref bytes i) #xff)) do (incf i))
            (when (>= i n) (return-from markers))
            (let ((m (aref bytes i))) (incf i)
              (cond
                ((= m #xd9) (return-from markers))                          ; EOI
                ((<= #xd0 m #xd7))                                          ; stray RSTn
                ((= m #xdb)                                                 ; DQT (store natural order)
                 (let ((len (j16 bytes i)) (p (+ i 2)))
                   (loop while (< p (+ i len)) do
                     (let* ((pq/tq (aref bytes p)) (prec (ash pq/tq -4)) (id (logand pq/tq 15))
                            (tbl (make-array 64)))
                       (incf p)
                       (dotimes (k 64)
                         (setf (aref tbl (aref +zigzag+ k))
                               (if (zerop prec) (aref bytes (+ p k)) (j16 bytes (+ p (* k 2))))))
                       (incf p (if (zerop prec) 64 128))
                       (setf (aref qt id) tbl)))
                   (incf i len)))
                ((or (= m #xc0) (= m #xc1) (= m #xc2))                      ; SOF0/1 (baseline) / SOF2 (progressive)
                 (let ((nc (aref bytes (+ i 7))))
                   (setf h (j16 bytes (+ i 3)) w (j16 bytes (+ i 5)) comps nil)
                   (dotimes (c nc)
                     (let ((o (+ i 8 (* c 3))))
                       (push (make-jcomp :id (aref bytes o) :h (ash (aref bytes (+ o 1)) -4)
                                         :v (logand (aref bytes (+ o 1)) 15) :qsel (aref bytes (+ o 2))) comps)))
                   (setf comps (nreverse comps))
                   (incf i (j16 bytes i))
                   (setup-frame)
                   (setf br (make-jbr :data bytes))))
                ((= m #xc4)                                                 ; DHT (in the C3-CF range, so BEFORE the decline)
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
                ((<= #xc3 m #xcf) (return-from markers))                    ; lossless / arithmetic / other SOF — decline
                ((= m #xdd) (setf ri (j16 bytes (+ i 2))) (incf i (j16 bytes i)))   ; DRI
                ((= m #xda)                                                 ; SOS — decode this scan
                 (when (null br) (return-from markers))
                 (let ((sos-i i))
                   ;; a truncated / malformed scan must not abort the whole image:
                   ;; keep whatever coefficients decoded so far (progressive-friendly).
                   (ignore-errors (decode-scan sos-i))
                   ;; resume marker parsing right after the entropy segment.
                   (setf i (if (and br (> (jbr-pos br) sos-i)) (jbr-pos br) (+ sos-i (j16 bytes i))))))
                (t (incf i (j16 bytes i))))))))                              ; APPn / COM / …
      (when (or (zerop w) (zerop h) (null comps)) (return-from jpeg-decode nil))
      ;; ---- dequantise + IDCT into per-component planes ----
      (dolist (c comps)
        (let* ((cw (* (jcomp-bpl c) 8)) (ch (* (jcomp-bh c) 8)) (coef (jcomp-coef c))
               (qtab (aref qt (jcomp-qsel c)))
               (plane (make-array (* cw ch) :element-type '(unsigned-byte 8) :initial-element 0)))
          (dotimes (by (jcomp-bh c))
            (dotimes (bx (jcomp-bpl c))
              (let ((off (* (+ (* by (jcomp-bpl c)) bx) 64)))
                (dotimes (k 64) (setf (aref fblock k) (* (coerce (aref coef (+ off k)) 'double-float)
                                                         (coerce (aref qtab k) 'double-float))))
                (idct-8x8 fblock spatial)
                (let ((col (* bx 8)) (row (* by 8)))
                  (dotimes (yy 8)
                    (dotimes (xx 8)
                      (setf (aref plane (+ (* (+ row yy) cw) col xx))
                            (clamp8 (+ (aref spatial (+ (* yy 8) xx)) 128d0)))))))))
          (setf (jcomp-coef c) plane (jcomp-bpl c) cw)))    ; reuse slots: coef->plane, bpl->plane width
      ;; ---- compose RGBA (upsample chroma by replication) ----
      (let ((out (make-array (* w h 4) :element-type '(unsigned-byte 8)))
            (c0 (first comps)) (c1 (second comps)) (c2 (third comps)))
        (dotimes (py h)
          (dotimes (px w)
            (flet ((samp (c) (aref (jcomp-coef c)
                                   (+ (* (floor (* py (jcomp-v c)) vmax) (jcomp-bpl c))
                                      (floor (* px (jcomp-h c)) hmax)))))
              (let ((o (* (+ (* py w) px) 4)))
                (if (and c1 c2)
                    (let* ((yy (samp c0)) (cb (- (samp c1) 128)) (cr (- (samp c2) 128)))
                      (setf (aref out o)       (clamp8 (+ yy (* 1.402d0 cr)))
                            (aref out (+ o 1)) (clamp8 (- yy (* 0.344136d0 cb) (* 0.714136d0 cr)))
                            (aref out (+ o 2)) (clamp8 (+ yy (* 1.772d0 cb)))))
                    (let ((g (samp c0))) (setf (aref out o) g (aref out (+ o 1)) g (aref out (+ o 2)) g)))
                (setf (aref out (+ o 3)) 255)))))
        (make-img :w w :h h :rgba out)))))
