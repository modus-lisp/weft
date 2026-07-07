;;;; test-grid.lisp — CSS grid layout geometry checks.
;;;;   run:  sbcl --non-interactive --load inspect/test-grid.lisp
(require :asdf)
(push (truename "./") asdf:*central-registry*)
(handler-case (asdf:load-system "weft/render")
  (error (e) (format t "~&LOAD-ERR ~a~%" e) (sb-ext:exit :code 1)))

(in-package :weft.render)

(defun grid-boxes (html width)
  "Lay out HTML and return an alist of (id . (x y w h)) for every element with an id."
  (let* ((doc (weft.html:parse-html html))
         (styles (weft.css:compute-styles doc nil))
         (root (layout-tree doc styles width))
         (out '()))
    (labels ((idof (n) (and n (eq (weft.html:dnode-kind n) :element)
                            (cdr (assoc "id" (weft.html:dnode-attrs n) :test #'string-equal))))
             (walk (lb)
               (when (typep lb 'lbox)
                 (let ((id (idof (lbox-node lb))))
                   (when id (push (list id (round (lbox-x lb)) (round (lbox-y lb))
                                        (round (lbox-w lb)) (round (lbox-h lb)))
                                  out)))
                 (dolist (c (lbox-children lb)) (walk c)))))
      (walk root))
    out))

(defparameter *tol* 2)
(defvar *fails* 0) (defvar *checks* 0)

(defun expect (boxes id &key x w y (ymin nil))
  "Assert element ID's box.  X/W/Y checked to +/-*TOL* when given; YMIN asserts y >= it."
  (incf *checks*)
  (let ((b (cdr (assoc id boxes :test #'string=))))
    (if (null b)
        (progn (incf *fails*) (format t "  FAIL ~a: no box~%" id))
        (destructuring-bind (bx by bw bh) b
          (declare (ignore bh))
          (let ((bad '()))
            (when (and x (> (abs (- bx x)) *tol*)) (push (format nil "x ~a!=~a" bx x) bad))
            (when (and w (> (abs (- bw w)) *tol*)) (push (format nil "w ~a!=~a" bw w) bad))
            (when (and y (> (abs (- by y)) *tol*)) (push (format nil "y ~a!=~a" by y) bad))
            (when (and ymin (< by ymin)) (push (format nil "y ~a<~a" by ymin) bad))
            (when bad (incf *fails*) (format t "  FAIL ~a: ~{~a ~}~%" id bad)))))))

(defmacro case- (name html width &body checks)
  `(let ((boxes (grid-boxes ,html ,width)))
     (format t "~&[~a]~%" ,name)
     ,@(loop for c in checks collect `(expect boxes ,@c))))

(let ((*fails* 0) (*checks* 0))
  ;; 1. fixed + fr: 250px | 1fr  in 1000  -> 250, 750  (same row)
  (case- "fixed+fr" "<html><body style='margin:0'><div style='display:grid;grid-template-columns:250px 1fr;width:1000px'><div id=a>A</div><div id=b>B</div></div>" 1000
    ("a" :x 0 :w 250 :y 0) ("b" :x 250 :w 750 :y 0))
  ;; 2. three equal fr with 20px gap in 1000 -> (1000-40)/3 = 320
  (case- "3fr+gap" "<html><body style='margin:0'><div style='display:grid;grid-template-columns:1fr 1fr 1fr;gap:20px;width:1000px'><div id=a>A</div><div id=b>B</div><div id=c>C</div></div>" 1000
    ("a" :x 0 :w 320 :y 0) ("b" :x 340 :w 320 :y 0) ("c" :x 680 :w 320 :y 0))
  ;; 3. repeat(2,1fr) auto-placement wraps: 4 items -> 2 rows
  (case- "repeat+wrap" "<html><body style='margin:0'><div style='display:grid;grid-template-columns:repeat(2,1fr);width:1000px'><div id=a>A</div><div id=b>B</div><div id=c>C</div><div id=d>D</div></div>" 1000
    ("a" :x 0 :w 500 :y 0) ("b" :x 500 :w 500 :y 0) ("c" :x 0 :w 500 :ymin 5) ("d" :x 500 :w 500 :ymin 5))
  ;; 4. column span: a spans 2 of repeat(4,1fr) in 800 (col=200)
  (case- "span" "<html><body style='margin:0'><div style='display:grid;grid-template-columns:repeat(4,1fr);width:800px'><div id=a style='grid-column:span 2'>A</div><div id=b>B</div><div id=c>C</div></div>" 800
    ("a" :x 0 :w 400 :y 0) ("b" :x 400 :w 200 :y 0) ("c" :x 600 :w 200 :y 0))
  (format t "~&grid: ~a checks, ~a failed~%" *checks* *fails*)
  (sb-ext:exit :code (if (zerop *fails*) 0 1)))
