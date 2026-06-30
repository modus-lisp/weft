;;;; inspect/acid2-layout-dump.lisp — dump weft's Acid2 face layout as JSON,
;;;; element boxes relative to .picture, for diffing against the real-browser
;;;; ground truth (acid2-browser-layout.json, captured via Playwright/Chromium).
;;;; Walks the DOM under .picture in document order (matching the browser walk),
;;;; emitting each element's (x y w h) when weft gives it a block/atomic box, or
;;;; null when weft doesn't box it (inline elements). Usage:
;;;;   sbcl --non-interactive --load inspect/acid2-layout-dump.lisp
;;;; -> writes inspect/vectors/acid/acid2-weft-layout.json
(asdf:load-system "weft/render")
(in-package :weft.render)

(defun el-desc (n)
  (let* ((tag (string-downcase (h:dnode-name n)))
         (id (cdr (assoc "id" (h:dnode-attrs n) :test #'string-equal)))
         (cls (cdr (assoc "class" (h:dnode-attrs n) :test #'string-equal))))
    (format nil "~a~@[#~a~]~@[.~a~]" tag id
            (when (and cls (plusp (length cls)))
              (substitute #\. #\Space (string-trim '(#\Space) cls))))))

(let* ((html (with-open-file (s (asdf:system-relative-pathname "weft" "inspect/vectors/acid/acid2.html")
                                :external-format :utf-8)
               (let ((str (make-string (file-length s)))) (subseq str 0 (read-sequence str s)))))
       (doc (h:parse-html html))
       (styles (weft.css:compute-styles doc (weft.css:parse-stylesheet (collect-stylesheets doc))))
       (root (layout-tree doc styles 700))
       (node->box (make-hash-table :test 'eq)))
  ;; map DOM node -> its block/atomic lbox
  (labels ((index (lb) (when lb
                         (when (and (eq (lbox-kind lb) :block) (lbox-node lb))
                           (unless (gethash (lbox-node lb) node->box) (setf (gethash (lbox-node lb) node->box) lb)))
                         (when (eq (lbox-kind lb) :line)
                           (dolist (it (lbox-children lb)) (unless (frag-p it) (index it))))
                         (when (eq (lbox-kind lb) :block) (dolist (c (lbox-children lb)) (index c))))))
    (index root))
  ;; find .picture node + box
  (let (pic pic-box)
    (labels ((find-pic (n)
               (when (eq (h:dnode-kind n) :element)
                 (let ((cls (cdr (assoc "class" (h:dnode-attrs n) :test #'string-equal))))
                   (when (and cls (search "picture" cls)) (setf pic n))))
               (loop for c across (h:dnode-children n) do (find-pic c))))
      (find-pic doc))
    (setf pic-box (gethash pic node->box))
    (let ((px (if pic-box (lbox-x pic-box) 0)) (py (if pic-box (lbox-y pic-box) 0))
          (rows '()))
      (labels ((walk (n d)
                 (when (eq (h:dnode-kind n) :element)
                   (let ((lb (gethash n node->box)))
                     (push (if lb
                               (format nil "{\"d\":~d,\"el\":~s,\"x\":~d,\"y\":~d,\"w\":~d,\"h\":~d}"
                                       d (el-desc n) (round (- (lbox-x lb) px)) (round (- (lbox-y lb) py))
                                       (round (lbox-w lb)) (round (lbox-h lb)))
                               (format nil "{\"d\":~d,\"el\":~s,\"box\":null}" d (el-desc n)))
                           rows))
                   (loop for c across (h:dnode-children n) do (walk c (1+ d))))))
        (walk pic 0))
      (with-open-file (o (asdf:system-relative-pathname "weft" "inspect/vectors/acid/acid2-weft-layout.json")
                         :direction :output :if-exists :supersede)
        (format o "[~%~{ ~a~^,~%~}~%]~%" (nreverse rows)))
      (format t "wrote acid2-weft-layout.json (~d elements); .picture box ~a~%"
              (length rows) (when pic-box (list (round px) (round py) (round (lbox-w pic-box)) (round (lbox-h pic-box))))))))
