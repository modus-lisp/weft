;;;; inspect/page-layout-dump.lisp — dump weft's element boxes for ANY page,
;;;; relative to <body>, in document order — the weft side of a real-page layout
;;;; reftest against a browser (Playwright) capture. Generalises acid2-layout-dump.
;;;;   sbcl --non-interactive --load inspect/page-layout-dump.lisp <html> <out.json>
(asdf:load-system "weft/render")
(in-package :weft.render)

(defun %pl-desc (n)
  (let ((tag (string-downcase (h:dnode-name n)))
        (id (cdr (assoc "id" (h:dnode-attrs n) :test #'string-equal)))
        (cls (cdr (assoc "class" (h:dnode-attrs n) :test #'string-equal))))
    (format nil "~a~@[#~a~]~@[.~a~]" tag id
            (when (and cls (plusp (length cls))) (substitute #\. #\Space (string-trim '(#\Space) cls))))))

(destructuring-bind (html-path out) (last sb-ext:*posix-argv* 2)
  (let* ((html (with-open-file (s html-path :external-format :utf-8)
                 (let ((str (make-string (file-length s)))) (subseq str 0 (read-sequence str s)))))
         (doc (h:parse-html html))
         (styles (weft.css:compute-styles doc (weft.css:parse-stylesheet
                                               (concatenate 'string (collect-stylesheets doc) (string #\Newline)))))
         (root (layout-tree doc styles 800))
         (n->b (make-hash-table :test 'eq)) (body nil) (rows '()))
    (labels ((index (lb)
               (when lb
                 (when (and (eq (lbox-kind lb) :block) (lbox-node lb) (not (gethash (lbox-node lb) n->b)))
                   (setf (gethash (lbox-node lb) n->b) lb))
                 (when (eq (lbox-kind lb) :line) (dolist (it (lbox-children lb)) (unless (frag-p it) (index it))))
                 (when (eq (lbox-kind lb) :block) (dolist (c (lbox-children lb)) (index c)))))
             (find-body (n) (when (and (eq (h:dnode-kind n) :element) (string-equal (h:dnode-name n) "body")) (setf body n))
               (loop for c across (h:dnode-children n) do (find-body c)))
             (walk (n d px py)
               (when (eq (h:dnode-kind n) :element)
                 (let ((lb (gethash n n->b)))
                   (push (if lb
                             (format nil "{\"d\":~d,\"el\":~s,\"x\":~d,\"y\":~d,\"w\":~d,\"h\":~d}"
                                     d (%pl-desc n) (round (- (lbox-x lb) px)) (round (- (lbox-y lb) py))
                                     (round (lbox-w lb)) (round (lbox-h lb)))
                             (format nil "{\"d\":~d,\"el\":~s,\"box\":null}" d (%pl-desc n)))
                         rows))
                 (loop for c across (h:dnode-children n) do (walk c (1+ d) px py)))))
      (index root) (find-body doc)
      (let ((bb (gethash body n->b)))
        (walk body 0 (if bb (lbox-x bb) 0) (if bb (lbox-y bb) 0)))
      (with-open-file (o out :direction :output :if-exists :supersede)
        (format o "{\"els\":[~%~{ ~a~^,~%~}~%]}~%" (nreverse rows)))
      (format t "wrote ~a (~d elements)~%" out (length rows)))))
