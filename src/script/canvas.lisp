;;;; src/script/canvas.lisp — canvas.getContext('2d') -> a CanvasRenderingContext2D
;;;; host object driving a gesso context, whose scribe buffer composites into the
;;;; page during paint.
(in-package #:weft.script)

(defun canvas-dim (node attr default)
  "The integer ATTR (width/height) on a <canvas> NODE, or DEFAULT."
  (let ((v (dom:get-attribute node attr)))
    (or (and v (ignore-errors (parse-integer (string-trim '(#\Space #\Tab) v) :junk-allowed t)))
        default)))

(defun c2d-color (s)
  "Parse a CSS color string S to an (r g b) list; black on failure."
  (let ((c (css:parse-value "color" (string (jstr s)))))
    (if (and (consp c) (numberp (first c))) (list (first c) (second c) (third c)) '(0 0 0))))

(defun c2d-num (args i &optional (default 0d0))
  "The Nth argument as a double float; NaN/non-number -> DEFAULT."
  (let ((v (js:to-number (arg args i))))
    (if (and (numberp v) (= v v)) (float v 1d0) default)))

(defun c2d-font-size (s)
  "The px font-size in a CSS font shorthand S (e.g. \"bold 16px sans-serif\"),
defaulting to 10."
  (let* ((str (jstr s)) (px (search "px" str)))
    (if px
        (let ((start px))
          (loop while (and (> start 0)
                           (let ((c (char str (1- start))))
                             (or (digit-char-p c) (char= c #\.))))
                do (decf start))
          (or (ignore-errors (float (read-from-string (subseq str start px)) 1d0)) 10d0))
        10d0)))

(defun make-2d-context (ctx node gctx)
  "Build the CanvasRenderingContext2D host object over gesso context GCTX."
  (let* ((realm (context-realm ctx))
         (op (js:eval-script realm "Object.prototype"))
         (obj (js:make-object :proto op))
         (fill-css "#000000") (stroke-css "#000000") (line-w 1d0) (alpha 1d0) (font-css "10px sans-serif"))
    (flet ((m (name len fn) (js:put obj name (js:native-function realm name fn len)
                                     :enumerable nil :writable t :configurable t))
           (acc (name getter setter)
             (js:put-accessor obj name
               :get (js:native-function realm (concatenate 'string "get " name)
                      (lambda (th ig) (declare (ignore th ig)) (funcall getter)) 0)
               :set (js:native-function realm (concatenate 'string "set " name)
                      (lambda (th a) (declare (ignore th)) (funcall setter (arg a 0)) js:*undefined*) 1)
               :enumerable t :configurable t)))
      ;; paint state
      (acc "fillStyle" (lambda () fill-css)
           (lambda (v) (setf fill-css (jstr v)) (g:set-fill gctx (c2d-color v))))
      (acc "strokeStyle" (lambda () stroke-css)
           (lambda (v) (setf stroke-css (jstr v)) (g:set-stroke gctx (c2d-color v))))
      (acc "lineWidth" (lambda () (num line-w))
           (lambda (v) (setf line-w (float (js:to-number v) 1d0)) (g:set-line-width gctx line-w)))
      (acc "globalAlpha" (lambda () (num alpha))
           (lambda (v) (setf alpha (float (js:to-number v) 1d0)) (g:set-global-alpha gctx alpha)))
      (acc "font" (lambda () font-css)
           (lambda (v) (setf font-css (jstr v)) (g:set-font gctx (c2d-font-size v))))
      ;; state stack + transforms
      (m "save" 0 (lambda (th a) (declare (ignore th a)) (g:save gctx) js:*undefined*))
      (m "restore" 0 (lambda (th a) (declare (ignore th a)) (g:restore gctx) js:*undefined*))
      (m "translate" 2 (lambda (th a) (declare (ignore th)) (g:translate gctx (c2d-num a 0) (c2d-num a 1)) js:*undefined*))
      (m "scale" 2 (lambda (th a) (declare (ignore th)) (g:scale gctx (c2d-num a 0 1d0) (c2d-num a 1 1d0)) js:*undefined*))
      (m "rotate" 1 (lambda (th a) (declare (ignore th)) (g:rotate gctx (c2d-num a 0)) js:*undefined*))
      (m "transform" 6 (lambda (th a) (declare (ignore th))
                         (g:transform gctx (c2d-num a 0 1d0) (c2d-num a 1) (c2d-num a 2)
                                      (c2d-num a 3 1d0) (c2d-num a 4) (c2d-num a 5)) js:*undefined*))
      (m "setTransform" 6 (lambda (th a) (declare (ignore th))
                            (g:set-transform gctx (c2d-num a 0 1d0) (c2d-num a 1) (c2d-num a 2)
                                             (c2d-num a 3 1d0) (c2d-num a 4) (c2d-num a 5)) js:*undefined*))
      (m "resetTransform" 0 (lambda (th a) (declare (ignore th a)) (g:reset-transform gctx) js:*undefined*))
      ;; path building
      (m "beginPath" 0 (lambda (th a) (declare (ignore th a)) (g:begin-path gctx) js:*undefined*))
      (m "closePath" 0 (lambda (th a) (declare (ignore th a)) (g:close-path gctx) js:*undefined*))
      (m "moveTo" 2 (lambda (th a) (declare (ignore th)) (g:move-to gctx (c2d-num a 0) (c2d-num a 1)) js:*undefined*))
      (m "lineTo" 2 (lambda (th a) (declare (ignore th)) (g:line-to gctx (c2d-num a 0) (c2d-num a 1)) js:*undefined*))
      (m "rect" 4 (lambda (th a) (declare (ignore th)) (g:rect gctx (c2d-num a 0) (c2d-num a 1) (c2d-num a 2) (c2d-num a 3)) js:*undefined*))
      (m "arc" 6 (lambda (th a) (declare (ignore th))
                   (g:arc gctx (c2d-num a 0) (c2d-num a 1) (c2d-num a 2) (c2d-num a 3) (c2d-num a 4) (truthy (arg a 5))) js:*undefined*))
      (m "ellipse" 8 (lambda (th a) (declare (ignore th))
                       (g:ellipse gctx (c2d-num a 0) (c2d-num a 1) (c2d-num a 2) (c2d-num a 3)
                                  (c2d-num a 4) (c2d-num a 5) (c2d-num a 6) (truthy (arg a 7))) js:*undefined*))
      (m "quadraticCurveTo" 4 (lambda (th a) (declare (ignore th))
                                (g:quadratic-curve-to gctx (c2d-num a 0) (c2d-num a 1) (c2d-num a 2) (c2d-num a 3)) js:*undefined*))
      (m "bezierCurveTo" 6 (lambda (th a) (declare (ignore th))
                             (g:bezier-curve-to gctx (c2d-num a 0) (c2d-num a 1) (c2d-num a 2) (c2d-num a 3) (c2d-num a 4) (c2d-num a 5)) js:*undefined*))
      ;; painting
      (m "fill" 0 (lambda (th a) (declare (ignore th a)) (g:fill-path gctx) js:*undefined*))
      (m "stroke" 0 (lambda (th a) (declare (ignore th a)) (g:stroke-path gctx) js:*undefined*))
      (m "fillRect" 4 (lambda (th a) (declare (ignore th)) (g:fill-rect gctx (c2d-num a 0) (c2d-num a 1) (c2d-num a 2) (c2d-num a 3)) js:*undefined*))
      (m "strokeRect" 4 (lambda (th a) (declare (ignore th)) (g:stroke-rect gctx (c2d-num a 0) (c2d-num a 1) (c2d-num a 2) (c2d-num a 3)) js:*undefined*))
      (m "clearRect" 4 (lambda (th a) (declare (ignore th)) (g:clear-rect gctx (c2d-num a 0) (c2d-num a 1) (c2d-num a 2) (c2d-num a 3)) js:*undefined*))
      (m "fillText" 4 (lambda (th a) (declare (ignore th)) (g:fill-text gctx (jstr (arg a 0)) (c2d-num a 1) (c2d-num a 2)) js:*undefined*))
      (m "strokeText" 4 (lambda (th a) (declare (ignore th)) (g:stroke-text gctx (jstr (arg a 0)) (c2d-num a 1) (c2d-num a 2)) js:*undefined*))
      (m "measureText" 1 (lambda (th a) (declare (ignore th))
                           (let ((o (js:make-object :proto op)))
                             (js:put o "width" (num (g:measure-text gctx (jstr (arg a 0))))) o)))
      ;; drawImage(source, dx, dy [, dw, dh]) for a <canvas> source
      (m "drawImage" 5 (lambda (th a) (declare (ignore th))
                         (let* ((srcnode (node-of ctx (arg a 0)))
                                (src (and srcnode (r:element-canvas srcnode))))
                           (when src
                             (if (>= (length a) 5)
                                 (g:draw-image gctx src (c2d-num a 1) (c2d-num a 2) (c2d-num a 3) (c2d-num a 4))
                                 (g:draw-image gctx src (c2d-num a 1) (c2d-num a 2)))))
                         js:*undefined*))
      ;; line-dash / cap / join surface (accepted, not yet honored by gesso)
      (m "setLineDash" 1 (lambda (th a) (declare (ignore th a)) js:*undefined*))
      (m "getLineDash" 0 (lambda (th a) (declare (ignore th a)) (js:eval-script realm "[]")))
      (js:put obj "lineCap" "butt") (js:put obj "lineJoin" "miter")
      (js:put obj "canvas" (wrap ctx node) :enumerable t))
    obj))

(defun canvas-rendering-context (ctx node kind)
  "The memoized CanvasRenderingContext2D for a <canvas> NODE (KIND \"2d\"), or null.
Registers the gesso context's scribe buffer so layout composites it at paint."
  (if (member (string-downcase (string (jstr kind))) '("2d") :test #'string=)
      (or (gethash node (context-canvas-ctxs ctx))
          (let* ((w (max 1 (canvas-dim node "width" 300)))
                 (h (max 1 (canvas-dim node "height" 150)))
                 (gctx (g:make-context w h :background '(255 255 255)))
                 (obj (make-2d-context ctx node gctx)))
            (setf (r:element-canvas node) (g:context-canvas gctx))
            (setf (gethash node (context-canvas-ctxs ctx)) obj)
            obj))
      js:*null*))
