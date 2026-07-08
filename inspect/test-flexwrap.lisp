;;;; test-flexwrap.lisp — multi-line flexbox (flex-wrap) geometry checks.
;;;;   run:  sbcl --non-interactive --load inspect/test-flexwrap.lisp
(require :asdf)
(push (truename "./") asdf:*central-registry*)
(handler-case (asdf:load-system "weft/render")
  (error (e) (format t "~&LOAD-ERR ~a~%" e) (sb-ext:exit :code 1)))
(in-package :weft.render)

(defun fw-boxes (html width)
  (let* ((doc (weft.html:parse-html html))
         (styles (weft.css:compute-styles doc nil))
         (root (layout-tree doc styles width))
         (out '()))
    (labels ((idof (n) (and n (eq (weft.html:dnode-kind n) :element)
                            (cdr (assoc "id" (weft.html:dnode-attrs n) :test #'string-equal))))
             (walk (lb) (when (typep lb 'lbox)
                          (let ((id (idof (lbox-node lb))))
                            (when id (push (list id (round (lbox-x lb)) (round (lbox-y lb))
                                                 (round (lbox-w lb)) (round (lbox-h lb))) out)))
                          (dolist (c (lbox-children lb)) (walk c)))))
      (walk root))
    out))

(defparameter *tol* 3)
(defvar *fails* 0) (defvar *checks* 0)
(defun expect (boxes id &key x w y ymin)
  (incf *checks*)
  (let ((b (cdr (assoc id boxes :test #'string=))))
    (if (null b) (progn (incf *fails*) (format t "  FAIL ~a: no box~%" id))
        (destructuring-bind (bx by bw bh) b (declare (ignore bh))
          (let ((bad '()))
            (when (and x (> (abs (- bx x)) *tol*)) (push (format nil "x ~a!=~a" bx x) bad))
            (when (and w (> (abs (- bw w)) *tol*)) (push (format nil "w ~a!=~a" bw w) bad))
            (when (and y (> (abs (- by y)) *tol*)) (push (format nil "y ~a!=~a" by y) bad))
            (when (and ymin (< by ymin)) (push (format nil "y ~a<~a" by ymin) bad))
            (when bad (incf *fails*) (format t "  FAIL ~a: ~{~a ~}~%" id bad)))))))

(defmacro case- (name html width &body checks)
  `(let ((boxes (fw-boxes ,html ,width))) (format t "~&[~a]~%" ,name) ,@(loop for c in checks collect `(expect boxes ,@c))))

(let ((*fails* 0) (*checks* 0))
  ;; 1. two full-width items in a wrap row -> each on its own line, full width (the figure/figcaption case)
  (case- "wrap-two-full" "<html><body style='margin:0'><div style='display:flex;flex-wrap:wrap;width:800px'><div id=a style='width:100%'>A</div><div id=b style='width:100%'>B</div></div>" 800
    ("a" :x 0 :w 800 :y 0) ("b" :x 0 :w 800 :ymin 5))
  ;; 2. three 250px items in 600 wrap -> 2 per line, third wraps
  (case- "wrap-three" "<html><body style='margin:0'><div style='display:flex;flex-wrap:wrap;width:600px'><div id=a style='width:250px'>A</div><div id=b style='width:250px'>B</div><div id=c style='width:250px'>C</div></div>" 600
    ("a" :x 0 :w 250 :y 0) ("b" :x 250 :w 250 :y 0) ("c" :x 0 :w 250 :ymin 5))
  ;; 3. nowrap control: two width:100% items shrink to ~half on one line (must not regress)
  (case- "nowrap-shrink" "<html><body style='margin:0'><div style='display:flex;width:800px'><div id=a style='width:100%'>A</div><div id=b style='width:100%'>B</div></div>" 800
    ("a" :x 0 :w 400 :y 0) ("b" :x 400 :w 400 :y 0))
  (format t "~&flexwrap: ~a checks, ~a failed~%" *checks* *fails*)
  (sb-ext:exit :code (if (zerop *fails*) 0 1)))
