;;;; inspect/acid2-conformance.lisp — Acid2 as a REGRESSION GATE (pure CL, offline).
;;;;
;;;; The pixel/geometry oracles (inspect/acid2-*.py) proved weft renders Acid2 at
;;;; 100% pixel-match vs a real browser. This makes that a permanent test: it
;;;; asserts the render stays within bounds of the vendored browser ground truth
;;;; (acid2-reference.png + acid2-browser-layout.json), with NO external tools —
;;;; weft decodes the reference with its own png-decode and parses the JSON here.
;;;;   (1) PIXEL  — colour-class agreement of the rendered face vs the reference
;;;;                smiley (auto-aligned over a bounded window); mismatched face
;;;;                pixels must stay <= *MAX-MISMATCH-PX*.
;;;;   (2) GEOMETRY — per-element box deltas vs Chromium ground truth; the visible
;;;;                face error and the total error must stay under their bounds.
;;;; Bounds are regression gates with modest headroom over the current values.
(defpackage #:weft.acid2.conformance
  (:use #:cl) (:local-nicknames (#:r #:weft.render) (#:h #:weft.html) (#:css #:weft.css))
  (:export #:run))
(in-package #:weft.acid2.conformance)

;;; ---- bounds (current actuals in comments; fail if exceeded) --------------
(defparameter *max-mismatch-px* 40)   ; face colour-class mismatches   (now 0)
(defparameter *max-face-geom*    220) ; visible-face box error         (now 166)
(defparameter *max-total-geom*   2100); total box error vs browser     (now 1886)

;;; ---- small helpers -------------------------------------------------------
(defun rel (p) (asdf:system-relative-pathname "weft" p))
(defun slurp (p) (with-open-file (s p :external-format :utf-8)
                   (let ((str (make-string (file-length s)))) (subseq str 0 (read-sequence str s)))))
(defun slurp-bytes (p) (with-open-file (s p :element-type '(unsigned-byte 8))
                         (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8)))) (read-sequence v s) v)))
(defun field (o k) (cdr (assoc k o :test #'string=)))

(defun json-parse (string)
  (let ((i 0) (n (length string)))
    (labels
        ((peek () (when (< i n) (char string i)))
         (next () (prog1 (char string i) (incf i)))
         (ws () (loop while (and (< i n) (member (char string i) '(#\Space #\Tab #\Newline #\Return))) do (incf i)))
         (value () (ws) (let ((ch (peek)))
                          (cond ((char= ch #\{) (object)) ((char= ch #\[) (array)) ((char= ch #\") (jstr))
                                ((or (digit-char-p ch) (member ch '(#\- #\+))) (jnum))
                                ((char= ch #\t) (incf i 4) :true) ((char= ch #\f) (incf i 5) :false)
                                ((char= ch #\n) (incf i 4) :null))))
         (object () (next) (ws) (let ((al '())) (when (char= (peek) #\}) (next) (return-from object '()))
                                  (loop (ws) (let ((k (jstr))) (ws) (next) (push (cons k (value)) al)) (ws)
                                        (if (char= (peek) #\,) (next) (progn (next) (return)))) (nreverse al)))
         (array () (next) (ws) (let ((items '())) (when (char= (peek) #\]) (next) (return-from array '()))
                                 (loop (push (value) items) (ws) (if (char= (peek) #\,) (next) (progn (next) (return)))) (nreverse items)))
         (jstr () (next) (with-output-to-string (o)
                           (loop for ch = (next) until (char= ch #\") do
                             (if (char= ch #\\)
                                 (let ((e (next))) (case e (#\n (write-char #\Newline o)) (#\t (write-char #\Tab o))
                                                     (#\/ (write-char #\/ o)) (#\\ (write-char #\\ o)) (#\" (write-char #\" o))
                                                     (t (write-char e o))))
                                 (write-char ch o)))))
         (jnum () (let ((st i)) (loop while (and (< i n) (or (digit-char-p (char string i)) (member (char string i) '(#\- #\+ #\. #\e #\E)))) do (incf i))
                    (read-from-string (subseq string st i)))))
      (value))))

(declaim (inline classify))
(defun classify (rr gg bb)
  (cond ((and (> rr 200) (> gg 200) (> bb 200)) 0)   ; white
        ((and (< rr 80)  (< gg 80)  (< bb 80))  1)   ; black
        ((and (> rr 200) (> gg 200) (< bb 100)) 2)   ; yellow
        ((and (> rr 180) (< gg 90)  (< bb 90))  3)   ; red
        (t 4)))                                       ; other

(defun render-canvas ()
  (r:render-to-canvas (slurp (rel "inspect/vectors/acid/acid2.html")) nil 700 :min-height 200 :scroll-to "top"))

;;; ---- (1) PIXEL: face colour-class mismatch vs the reference smiley --------
(defun pixel-mismatch (cv)
  "Best (min) count of non-white reference pixels whose colour class disagrees
with the rendered canvas, aligning over a bounded window around the face."
  (let* ((ref (r::png-decode (slurp-bytes (rel "inspect/vectors/acid/acid2-reference.png"))))
         (rw (r::img-w ref)) (rh (r::img-h ref)) (rgba (r::img-rgba ref))
         (cw (r:canvas-width cv)) (chh (r:canvas-height cv)) (px (r:canvas-pixels cv))
         (rn 0) (rxs (make-array (* rw rh))) (rys (make-array (* rw rh))) (rcs (make-array (* rw rh)))
         (ccls (make-array (* cw chh) :element-type '(unsigned-byte 8))))
    ;; reference: keep only non-white pixels (x y class)
    (dotimes (yy rh)
      (dotimes (xx rw)
        (let* ((k (+ (* yy rw) xx)) (c (classify (aref rgba (* k 4)) (aref rgba (+ (* k 4) 1)) (aref rgba (+ (* k 4) 2)))))
          (unless (= c 0) (setf (aref rxs rn) xx (aref rys rn) yy (aref rcs rn) c) (incf rn)))))
    ;; canvas class grid
    (dotimes (i (* cw chh))
      (setf (aref ccls i) (classify (aref px (* i 3)) (aref px (+ (* i 3) 1)) (aref px (+ (* i 3) 2)))))
    ;; bounded slide search around the known face origin (~72,108)
    (let ((best most-positive-fixnum))
      (loop for oy from (max 0 78) to (min (- chh rh) 138) do
        (loop for ox from (max 0 42) to (min (- cw rw) 102) do
          (let ((mis 0))
            (dotimes (j rn)
              (unless (= (aref ccls (+ (* (+ oy (aref rys j)) cw) (+ ox (aref rxs j)))) (aref rcs j))
                (incf mis)))
            (when (< mis best) (setf best mis)))))
      (values best rn))))

;;; ---- (2) GEOMETRY: per-element box deltas vs the vendored browser truth ---
;;; weft's boxes are matched to the browser's by document order (both walk the
;;; .picture subtree depth-first), so no per-element name matching is needed.
(defun weft-boxes ()
  "List of weft's .picture-descendant boxes (x y w h) or NIL, in document order,
each relative to .picture — the same walk the browser dump uses."
  (let* ((doc (h:parse-html (slurp (rel "inspect/vectors/acid/acid2.html"))))
         (styles (css:compute-styles doc (css:parse-stylesheet (r::collect-stylesheets doc))))
         (root (r::layout-tree doc styles 700))
         (n->b (make-hash-table :test 'eq)) (pic nil) (out '()))
    (labels ((index (lb)
               (when lb
                 (when (and (eq (r::lbox-kind lb) :block) (r::lbox-node lb) (not (gethash (r::lbox-node lb) n->b)))
                   (setf (gethash (r::lbox-node lb) n->b) lb))
                 (when (eq (r::lbox-kind lb) :line)
                   (dolist (it (r::lbox-children lb)) (unless (r::frag-p it) (index it))))
                 (when (eq (r::lbox-kind lb) :block) (dolist (c (r::lbox-children lb)) (index c)))))
             (find-pic (nn) (when (eq (h:dnode-kind nn) :element)
                              (let ((cls (cdr (assoc "class" (h:dnode-attrs nn) :test #'string-equal))))
                                (when (and cls (search "picture" cls)) (setf pic nn))))
               (loop for c across (h:dnode-children nn) do (find-pic c)))
             (walk (nn px py)
               (when (eq (h:dnode-kind nn) :element)
                 (let ((lb (gethash nn n->b)))
                   (push (when lb (list (round (- (r::lbox-x lb) px)) (round (- (r::lbox-y lb) py))
                                        (round (r::lbox-w lb)) (round (r::lbox-h lb)))) out))
                 (loop for c across (h:dnode-children nn) do (walk c px py)))))
      (index root) (find-pic doc)
      (let ((pb (gethash pic n->b)))
        (walk pic (if pb (r::lbox-x pb) 0) (if pb (r::lbox-y pb) 0)))
      (nreverse out))))

(defun geometry-error ()
  "Returns (values face-error total-error), summing |dx|+|dy|+|dw|+|dh| over
elements weft boxes, vs the vendored browser layout. FACE = browser-visible
(w>0,h>0) elements within the face (browser y < 190)."
  (let* ((brow (json-parse (slurp (rel "inspect/vectors/acid/acid2-browser-layout.json"))))
         (els (field brow "els")) (weft (weft-boxes))
         (face 0) (total 0))
    (loop for be in els for wb in weft
          for bx = (field be "x") for by = (field be "y") for bw = (field be "w") for bh = (field be "h")
          when (and wb bx) do
            (destructuring-bind (wx wy ww wh) wb
              (let ((err (+ (abs (- wx bx)) (abs (- wy by)) (abs (- ww bw)) (abs (- wh bh)))))
                (incf total err)
                (when (and (> bw 0) (> bh 0) (< by 190)) (incf face err)))))
    (values face total)))

;;; ---- gate ----------------------------------------------------------------
(defun run ()
  (format t "~&=== Acid2 conformance gate (pixel + geometry vs a real browser) ===~%")
  (let ((fails 0))
    (handler-case
        (multiple-value-bind (mis refn) (pixel-mismatch (render-canvas))
          (let ((ok (<= mis *max-mismatch-px*)))
            (unless ok (incf fails))
            (format t "  ~a pixel   : ~d/~d face px mismatch (~,2f% match)  [bound <= ~d]~%"
                    (if ok "ok  " "FAIL") mis refn (* 100 (- 1 (/ mis (float refn)))) *max-mismatch-px*)))
      (error (e) (incf fails) (format t "  FAIL pixel gate errored: ~a~%" e)))
    (handler-case
        (multiple-value-bind (face total) (geometry-error)
          (let ((fok (<= face *max-face-geom*)) (tok (<= total *max-total-geom*)))
            (unless (and fok tok) (incf fails))
            (format t "  ~a geometry: face error ~d [bound <= ~d], total ~d [bound <= ~d]~%"
                    (if (and fok tok) "ok  " "FAIL") face *max-face-geom* total *max-total-geom*)))
      (error (e) (incf fails) (format t "  FAIL geometry gate errored: ~a~%" e)))
    (format t "~a~%" (if (zerop fails) "Acid2 conformance: PASS" "Acid2 conformance: FAIL"))
    (values (if (zerop fails) 1 0) fails)))
