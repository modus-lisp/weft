;;;; inspect/svg-canvas.lisp — render proof for inline SVG and <canvas>.
;;;;
;;;; Renders two pages end to end and checks that the vector content actually
;;;; reached the page bitmap: an inline <svg> (rect/circle/path/text) drawn
;;;; through stencil+gesso, and a <canvas> drawn by page script through the
;;;; CanvasRenderingContext2D bridge.  Saves a PNG of each.
;;;;   sbcl --script inspect/svg-canvas.lisp
(require :asdf)
(push (truename (merge-pathnames "../" (directory-namestring *load-truename*)))
      asdf:*central-registry*)
(asdf:load-system "weft/script")

(defpackage #:weft.svg-canvas
  (:use #:cl) (:local-nicknames (#:s #:weft.script) (#:r #:weft.render))
  (:export #:run))
(in-package #:weft.svg-canvas)

(defvar *pass* 0) (defvar *fail* 0)
(defun ok (label test) (if test (incf *pass*)
                           (progn (incf *fail*) (format t "~&FAIL ~a~%" label))))

(defun out (name) (namestring (asdf:system-relative-pathname "weft" (format nil "inspect/~a" name))))

(defun count-color (cv r g b &optional (tol 24))
  "Number of pixels within TOL of (R,G,B)."
  (let ((px (r:canvas-pixels cv)) (n 0))
    (loop for i from 0 below (length px) by 3
          when (and (<= (abs (- (aref px i) r)) tol)
                    (<= (abs (- (aref px (+ i 1)) g)) tol)
                    (<= (abs (- (aref px (+ i 2)) b)) tol))
            do (incf n))
    n))

(defparameter *svg-html*
  "<!doctype html><html><body><p>before</p>
<svg width='200' height='140' viewBox='0 0 200 140'>
  <rect x='10' y='10' width='120' height='60' fill='#3060d0'/>
  <circle cx='60' cy='40' r='26' fill='#e03030'/>
  <path d='M10 130 L60 90 L110 130 Z' fill='#20a040'/>
  <text x='14' y='30' font-size='16' fill='#ffffff'>SVG</text>
</svg>
<p>after</p></body></html>")

(defparameter *canvas-html*
  "<!doctype html><html><body><p>before</p>
<canvas id='c' width='240' height='140'></canvas>
<script>
var ctx = document.getElementById('c').getContext('2d');
ctx.fillStyle = '#3060d0'; ctx.fillRect(10,10,120,60);
ctx.fillStyle = '#e03030';
ctx.beginPath(); ctx.arc(60,100,26,0,6.2832,false); ctx.fill();
ctx.fillStyle = '#20a040';
ctx.beginPath(); ctx.moveTo(150,20); ctx.lineTo(220,20); ctx.lineTo(185,80); ctx.closePath(); ctx.fill();
ctx.fillStyle = '#101010'; ctx.font = '18px sans-serif'; ctx.fillText('Canvas',140,120);
</script>
<p>after</p></body></html>")

(defun run ()
  ;; Inline SVG (no script needed): stencil + gesso onto the page.
  (let ((cv (r:render-to-canvas *svg-html* "" 400)))
    (r:write-png cv (out "svg-inline.png"))
    (ok "svg: blue rect present"   (> (count-color cv #x30 #x60 #xd0) 3000))
    (ok "svg: red circle present"  (> (count-color cv #xe0 #x30 #x30) 800))
    (ok "svg: green path present"  (> (count-color cv #x20 #xa0 #x40) 300))
    (format t "~&wrote ~a~%" (out "svg-inline.png")))
  ;; <canvas> driven by page script through the 2D bridge.
  (multiple-value-bind (cv ctx)
      (s:render-scripted-to-canvas *canvas-html* "" 400)
    (declare (ignore ctx))
    (r:write-png cv (out "canvas-draw.png"))
    (ok "canvas: blue rect present"  (> (count-color cv #x30 #x60 #xd0) 3000))
    (ok "canvas: red circle present" (> (count-color cv #xe0 #x30 #x30) 800))
    (ok "canvas: green tri present"  (> (count-color cv #x20 #xa0 #x40) 800))
    (format t "~&wrote ~a~%" (out "canvas-draw.png")))
  (format t "~&svg/canvas render check: ~a passed, ~a failed~%" *pass* *fail*)
  (values (zerop *fail*) *fail*))

(run)
