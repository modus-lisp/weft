;;;; gbcr-audit.lisp — grade weft's getBoundingClientRect against Chrome's.
;;;;
;;;; The oracle here is the very API being implemented: inspect/gbcr-chrome.js
;;;; dumps every rendered element's path and rect from Chrome's
;;;; getBoundingClientRect, this dumps the same rows through weft's, and the two
;;;; are diffed row by row.  So a disagreement is a difference in ANSWER, not a
;;;; difference in method -- and where weft's layout itself is wrong, that shows up
;;;; here as a wrong rectangle rather than hiding behind a missing method.
;;;;
;;;; The dump runs INSIDE the page, through the JS binding, so what is graded is
;;;; the thing scripts actually call.  A <script> is display:none and is skipped by
;;;; both dumpers, so injecting one does not move the geometry it measures.
;;;;
;;;;   node inspect/gbcr-chrome.js 800 inspect/gbcr/*.html > /tmp/chrome.tsv
;;;;   sbcl --script inspect/gbcr-audit.lisp /tmp/chrome.tsv inspect/gbcr/*.html
(require :asdf)
(asdf:initialize-source-registry
 `(:source-registry (:tree #p"/home/claude/") (:exclude "vendor") (:exclude "deps")
   :inherit-configuration))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system "loom"))
(in-package :cl-user)

(defparameter *tolerance*
  (or (ignore-errors (parse-integer (uiop:getenv "GBCR_TOLERANCE"))) 1)
  "Pixels of slack per edge (GBCR_TOLERANCE overrides).  Chrome rounds a subpixel layout; weft lays out in
   whole pixels, so a 1px disagreement is a rounding difference rather than a
   different answer.  Anything larger is a real divergence and is reported.")

;;; The dump script: the same structural path as gbcr-chrome.js's pathOf, and the
;;; same skip rules, so a row names the same element in both.
(defparameter *dump-js* "
(function(){
  function pathOf(el){
    var parts=[];
    while(el && el.nodeType===1 && el.tagName!=='HTML'){
      var p=el.parentElement; if(!p) break;
      var same=[]; for(var i=0;i<p.children.length;i++){
        if(p.children[i].tagName===el.tagName) same.push(p.children[i]); }
      parts.unshift(el.tagName.toLowerCase()+':'+(same.indexOf(el)+1));
      el=p;
    }
    return parts.join('/');
  }
  var out=[], all=document.querySelectorAll('*');
  for(var i=0;i<all.length;i++){
    var el=all[i];
    if(el.tagName==='SCRIPT'||el.tagName==='STYLE') continue;
    var r=el.getBoundingClientRect();
    out.push([pathOf(el),Math.round(r.x),Math.round(r.y),
              Math.round(r.width),Math.round(r.height)].join('\\t'));
  }
  document.body.setAttribute('data-gbcr', out.join('\\n'));
})();
")

(defun slurp (path) (uiop:read-file-string path))

(defun split-on (ch s)
  (loop with start = 0 for i = (position ch s :start start)
        collect (subseq s start i) do (if i (setf start (1+ i)) (loop-finish))))

(defun weft-rows (file width)
  "FILE's rows as an alist of path -> (x y w h), measured through weft's own
   getBoundingClientRect."
  (let* ((html (slurp file))
         (marker "</body>")
         (pos (search marker html :test #'char-equal))
         (injected (if pos
                       (concatenate 'string (subseq html 0 pos)
                                    "<script>" *dump-js* "</script>" (subseq html pos))
                       (concatenate 'string html "<script>" *dump-js* "</script>")))
         (pg (loom:load-page injected :width width))
         (body (weft.css:query-select (loom:page-doc pg) "body"))
         (dump (and body (weft.dom:get-attribute body "data-gbcr"))))
    (when (loom:page-js-error pg)
      (format t "  !! script error: ~a~%" (loom:page-js-error pg)))
    (loop for line in (split-on #\Newline (or dump ""))
          for f = (split-on #\Tab line)
          when (= (length f) 5)
            collect (cons (first f) (mapcar #'parse-integer (rest f))))))

(defun chrome-rows (tsv)
  "The reference dump, as a hash of file -> (path -> (x y w h))."
  (let ((by-file (make-hash-table :test 'equal)) (current nil))
    (dolist (line (split-on #\Newline tsv) by-file)
      (cond ((and (> (length line) 6) (string= "#FILE" (subseq line 0 5)))
             (setf current (subseq line 6))
             (setf (gethash current by-file) (make-hash-table :test 'equal)))
            ((and current (find #\Tab line))
             (let ((f (split-on #\Tab line)))
               (when (= (length f) 5)
                 (setf (gethash (first f) (gethash current by-file))
                       (mapcar #'parse-integer (rest f))))))))))

(defun main ()
  (let* ((args (uiop:command-line-arguments))
         (tsv (first args))
         (files (rest args))
         (width 800)
         (ref (chrome-rows (slurp tsv)))
         (total 0) (agree 0) (missing 0) (extra 0))
    (dolist (file files)
      (let* ((base (file-namestring file))
             (want (gethash base ref))
             (got (weft-rows file width))
             (f-total 0) (f-agree 0))
        (unless want
          (format t "~&~a: NO REFERENCE ROWS — skipped~%" base)
          (return-from main))
        (dolist (row got)
          (let ((w (gethash (car row) want)))
            (cond
              ((null w) (incf extra))
              (t (incf total) (incf f-total)
                 (if (every (lambda (a b) (<= (abs (- a b)) *tolerance*)) (cdr row) w)
                     (progn (incf agree) (incf f-agree))
                     (format t "~&  ~a  ~a~%     weft   ~{~5d~}~%     chrome ~{~5d~}~%"
                             base (car row) (cdr row) w))))))
        (let ((seen (mapcar #'car got)))
          (maphash (lambda (k v) (declare (ignore v))
                     (unless (member k seen :test #'string=) (incf missing)))
                   want))
        (format t "~&~a: ~d/~d within ~dpx~%" base f-agree f-total *tolerance*)))
    (format t "~&~%TOTAL ~d/~d elements agree within ~dpx (~,1f%)~@[  [~d chrome rows weft did not report]~]~@[  [~d weft rows chrome did not]~]~%"
            agree total *tolerance* (if (plusp total) (* 100.0 (/ agree total)) 0)
            (and (plusp missing) missing) (and (plusp extra) extra))))

(main)
