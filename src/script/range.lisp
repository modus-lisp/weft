;;;; src/script/range.lisp — DOM Range.
;;;;
;;;; A range is a pair of boundary points (node, offset) over the live tree.
;;;; This implements the boundary-point ordering the spec is built on, the
;;;; setStart/End family, selectNode(Contents), collapse, compareBoundaryPoints,
;;;; toString, insertNode, and clone/extract/deleteContents.
(in-package #:weft.script)

(defstruct rg sc (so 0) ec (eo 0))         ; start/end container + offset

(defun rg-of (ctx obj)
  (or (gethash obj (context-ranges ctx))
      (js:js-throw (js:make-native-error "TypeError" "not a Range"))))

;;; ---- node metrics ---------------------------------------------------------
(defun node-index (node)
  (let ((p (h:dnode-parent node))) (if p (or (position node (h:dnode-children p)) 0) 0)))

(defun node-len (node)
  (if (char-data-p node) (length (or (h:dnode-data node) ""))
      (length (h:dnode-children node))))

(defun ancestor-p (a b)
  "T if A is a proper ancestor of B."
  (loop for p = (h:dnode-parent b) then (h:dnode-parent p)
        while p thereis (eq p a)))

(defun ancestor-child (anc desc)
  "The child of ANC that is-or-contains DESC."
  (loop for n = desc then (h:dnode-parent n)
        when (eq (h:dnode-parent n) anc) return n))

(defun node-path (node)
  (let ((path '()))
    (loop for n = node then p for p = (h:dnode-parent n) while p
          do (push (node-index n) path))
    path))

(defun tree-order (a b)
  "-1 if A precedes B, 1 if follows, 0 if equal (document preorder)."
  (if (eq a b) 0
      (let ((pa (node-path a)) (pb (node-path b)))
        (loop for x in pa for y in pb
              do (cond ((< x y) (return-from tree-order -1))
                       ((> x y) (return-from tree-order 1))))
        (cond ((< (length pa) (length pb)) -1)   ; A is an ancestor -> precedes
              ((> (length pa) (length pb)) 1)
              (t 0)))))

(defun bp-pos (ca oa cb ob)
  "Boundary-point ordering: -1 (before), 0 (equal), 1 (after)."
  (cond
    ((eq ca cb) (let ((d (- oa ob))) (cond ((< d 0) -1) ((> d 0) 1) (t 0))))
    ((> (tree-order ca cb) 0) (- (bp-pos cb ob ca oa)))
    ((ancestor-p ca cb) (if (< (node-index (ancestor-child ca cb)) oa) 1 -1))
    (t -1)))

;;; ---- boundary mutation ----------------------------------------------------
(defun rg-collapsed-p (r) (and (eq (rg-sc r) (rg-ec r)) (= (rg-so r) (rg-eo r))))

(defun rg-set-start (r c o)
  (setf (rg-sc r) c (rg-so r) o)
  (when (or (not (eq (range-root c) (range-root (rg-ec r))))
            (= (bp-pos c o (rg-ec r) (rg-eo r)) 1))
    (setf (rg-ec r) c (rg-eo r) o)))

(defun rg-set-end (r c o)
  (setf (rg-ec r) c (rg-eo r) o)
  (when (or (not (eq (range-root c) (range-root (rg-sc r))))
            (= (bp-pos c o (rg-sc r) (rg-so r)) -1))
    (setf (rg-sc r) c (rg-so r) o)))

(defun range-root (node)
  (loop for n = node then (h:dnode-parent n)
        when (null (h:dnode-parent n)) return n))

(defun common-ancestor (r)
  (let ((a (rg-sc r)))
    (loop until (or (eq a (rg-ec r)) (ancestor-p a (rg-ec r)))
          do (setf a (h:dnode-parent a)) (when (null a) (return)))
    (or a (rg-sc r))))

;;; ---- toString -------------------------------------------------------------
(defun all-text-nodes (root)
  (let ((out '()))
    (labels ((walk (n) (when (eq (h:dnode-kind n) :text) (push n out))
               (loop for c across (h:dnode-children n) do (walk c))))
      (walk root))
    (nreverse out)))

(defun rg-string (r)
  (let ((sc (rg-sc r)) (ec (rg-ec r)))
    (cond
      ((and (eq sc ec) (char-data-p sc))
       (subseq (or (h:dnode-data sc) "") (rg-so r) (rg-eo r)))
      (t
       (with-output-to-string (s)
         (when (char-data-p sc) (write-string (subseq (h:dnode-data sc) (rg-so r)) s))
         (dolist (tn (all-text-nodes (common-ancestor r)))
           (when (and (not (eq tn sc)) (not (eq tn ec))
                      (= (bp-pos tn 0 (rg-sc r) (rg-so r)) 1)
                      (= (bp-pos tn (node-len tn) (rg-ec r) (rg-eo r)) -1))
             (write-string (h:dnode-data tn) s)))
         (when (char-data-p ec) (write-string (subseq (h:dnode-data ec) 0 (rg-eo r)) s)))))))

;;; ---- contained-node collection (for clone/extract/delete) -----------------
(defun rg-contained-nodes (r)
  "Top-level nodes fully contained by R, in tree order (a node is contained iff
   both its boundary points lie within the range and its parent is not)."
  (let ((ca (common-ancestor r)) (out '()))
    (labels ((walk (n)
               (when (and (not (eq n ca))
                          (= (bp-pos n 0 (rg-sc r) (rg-so r)) 1)
                          (= (bp-pos n (node-len n) (rg-ec r) (rg-eo r)) -1)
                          (let ((p (h:dnode-parent n)))
                            (or (null p)
                                (not (and (= (bp-pos p 0 (rg-sc r) (rg-so r)) 1)
                                          (= (bp-pos p (node-len p) (rg-ec r) (rg-eo r)) -1))))))
                 (push n out) (return-from walk))
               (loop for c across (copy-seq (h:dnode-children n)) do (walk c))))
      (walk ca))
    (nreverse out)))

(defun rg-clone-contents (ctx r)
  (let ((frag (h:make-fragment)))
    (cond
      ((rg-collapsed-p r))
      ((and (eq (rg-sc r) (rg-ec r)) (char-data-p (rg-sc r)))
       (h:dom-append frag (h:make-text (subseq (h:dnode-data (rg-sc r)) (rg-so r) (rg-eo r)))))
      (t (dolist (n (rg-contained-nodes r)) (h:dom-append frag (copy-dnode n t)))))
    (wrap ctx frag)))

(defun rg-extract-contents (ctx r)
  (let ((frag (h:make-fragment)))
    (cond
      ((rg-collapsed-p r))
      ((and (eq (rg-sc r) (rg-ec r)) (char-data-p (rg-sc r)))
       (let* ((d (h:dnode-data (rg-sc r)))
              (piece (subseq d (rg-so r) (rg-eo r))))
         (setf (h:dnode-data (rg-sc r)) (concatenate 'string (subseq d 0 (rg-so r)) (subseq d (rg-eo r))))
         (h:dom-append frag (h:make-text piece))))
      (t (dolist (n (rg-contained-nodes r)) (h:dom-remove n) (h:dom-append frag n))
         (rg-set-end r (rg-sc r) (rg-so r))))   ; collapse to start
    (setf (context-dirty ctx) t)
    (wrap ctx frag)))

(defun rg-delete-contents (ctx r)
  (unless (rg-collapsed-p r)
    (if (and (eq (rg-sc r) (rg-ec r)) (char-data-p (rg-sc r)))
        (let ((d (h:dnode-data (rg-sc r))))
          (setf (h:dnode-data (rg-sc r)) (concatenate 'string (subseq d 0 (rg-so r)) (subseq d (rg-eo r)))))
        (progn (dolist (n (rg-contained-nodes r)) (h:dom-remove n))
               (rg-set-end r (rg-sc r) (rg-so r))))
    (setf (context-dirty ctx) t)))

(defun split-text (ctx node offset)
  "Split Text NODE at OFFSET: NODE keeps [0,offset), a new Text node takes the
   rest and is inserted right after it. Live ranges are adjusted. Returns the new
   node (the second half)."
  (let* ((d (or (h:dnode-data node) "")) (new (h:make-text (subseq d offset)))
         (parent (h:dnode-parent node)) (idx (and parent (node-index node))))
    (setf (h:dnode-data node) (subseq d 0 offset))
    (when parent
      (let ((ref (n-next-sib node)))
        (if ref (h:dom-insert-before parent new ref) (h:dom-append parent new))))
    ;; adjust every range boundary that falls in NODE past OFFSET, or in PARENT
    ;; after NODE's index (a child was inserted).
    (maphash (lambda (obj rr) (declare (ignore obj))
               (flet ((fix-c (getc setc geto seto)
                        (let ((c (funcall getc rr)) (o (funcall geto rr)))
                          (cond ((and (eq c node) (> o offset))
                                 (funcall setc new) (funcall seto (- o offset)))
                                ((and parent (eq c parent) (> o idx))
                                 (funcall seto (1+ o)))))))
                 (fix-c #'rg-sc (lambda (v) (setf (rg-sc rr) v)) #'rg-so (lambda (v) (setf (rg-so rr) v)))
                 (fix-c #'rg-ec (lambda (v) (setf (rg-ec rr) v)) #'rg-eo (lambda (v) (setf (rg-eo rr) v)))))
             (context-ranges ctx))
    new))

(defun inclusive-descendant-p (c anc)
  (loop for n = c then (h:dnode-parent n) while n thereis (eq n anc)))

(defun adjust-ranges-for-removal (ctx node)
  "Live-range fixup for removing NODE (call while NODE is still attached): a
   boundary inside NODE moves to (oldParent, oldIndex); a boundary in the parent
   past NODE's index shifts left one."
  (let ((parent (h:dnode-parent node)))
    (when parent
      (let ((index (node-index node)))
        (maphash
         (lambda (obj rr) (declare (ignore obj))
           (flet ((fix (getc setc geto seto)
                    (let ((c (funcall getc rr)) (o (funcall geto rr)))
                      (cond ((inclusive-descendant-p c node)
                             (funcall setc parent) (funcall seto index))
                            ((and (eq c parent) (> o index)) (funcall seto (1- o)))))))
             (fix #'rg-sc (lambda (v) (setf (rg-sc rr) v)) #'rg-so (lambda (v) (setf (rg-so rr) v)))
             (fix #'rg-ec (lambda (v) (setf (rg-ec rr) v)) #'rg-eo (lambda (v) (setf (rg-eo rr) v)))))
         (context-ranges ctx))))))

;;; ---- installation ---------------------------------------------------------
(defun install-range-proto (ctx rp)
  (macrolet ((r (this) `(rg-of ctx ,this)))
    (defget ctx rp "startContainer" (this) (wrap ctx (rg-sc (r this))))
    (defget ctx rp "startOffset" (this) (num (rg-so (r this))))
    (defget ctx rp "endContainer" (this) (wrap ctx (rg-ec (r this))))
    (defget ctx rp "endOffset" (this) (num (rg-eo (r this))))
    (defget ctx rp "collapsed" (this) (jbool (rg-collapsed-p (r this))))
    (defget ctx rp "commonAncestorContainer" (this) (wrap ctx (common-ancestor (r this))))
    (dolist (pair '(("START_TO_START" . 0) ("START_TO_END" . 1) ("END_TO_END" . 2) ("END_TO_START" . 3)))
      (js:put rp (car pair) (num (cdr pair)) :enumerable nil :writable nil :configurable nil))
    (defmethod* ctx rp "setStart" 2 (this a)
      (rg-set-start (r this) (require-node ctx (arg a 0)) (int-arg a 1)) js:*undefined*)
    (defmethod* ctx rp "setEnd" 2 (this a)
      (rg-set-end (r this) (require-node ctx (arg a 0)) (int-arg a 1)) js:*undefined*)
    (flet ((bounded (node)
             (let ((p (h:dnode-parent node)))
               (unless p (throw-dom ctx "InvalidNodeTypeError" 24 "node has no parent"))
               p)))
      (defmethod* ctx rp "setStartBefore" 1 (this a)
        (let ((n (require-node ctx (arg a 0)))) (rg-set-start (r this) (bounded n) (node-index n))) js:*undefined*)
      (defmethod* ctx rp "setStartAfter" 1 (this a)
        (let ((n (require-node ctx (arg a 0)))) (rg-set-start (r this) (bounded n) (1+ (node-index n)))) js:*undefined*)
      (defmethod* ctx rp "setEndBefore" 1 (this a)
        (let ((n (require-node ctx (arg a 0)))) (rg-set-end (r this) (bounded n) (node-index n))) js:*undefined*)
      (defmethod* ctx rp "setEndAfter" 1 (this a)
        (let ((n (require-node ctx (arg a 0)))) (rg-set-end (r this) (bounded n) (1+ (node-index n)))) js:*undefined*))
    (defmethod* ctx rp "collapse" 1 (this a)
      (let ((rr (r this)))
        (if (js:js-truthy (arg a 0)) (setf (rg-ec rr) (rg-sc rr) (rg-eo rr) (rg-so rr))
            (setf (rg-sc rr) (rg-ec rr) (rg-so rr) (rg-eo rr))))
      js:*undefined*)
    (defmethod* ctx rp "selectNode" 1 (this a)
      (let* ((n (require-node ctx (arg a 0))) (p (h:dnode-parent n)) (i (node-index n)) (rr (r this)))
        (unless p (throw-dom ctx "InvalidNodeTypeError" 24 "node has no parent"))
        (setf (rg-sc rr) p (rg-so rr) i (rg-ec rr) p (rg-eo rr) (1+ i)))
      js:*undefined*)
    (defmethod* ctx rp "selectNodeContents" 1 (this a)
      (let* ((n (require-node ctx (arg a 0))) (rr (r this)))
        (setf (rg-sc rr) n (rg-so rr) 0 (rg-ec rr) n (rg-eo rr) (node-len n)))
      js:*undefined*)
    (defmethod* ctx rp "compareBoundaryPoints" 2 (this a)
      (let* ((how (int-arg a 0)) (rr (r this)) (other (rg-of ctx (arg a 1))))
        (multiple-value-bind (ca oa cb ob)
            (ecase how
              (0 (values (rg-sc rr) (rg-so rr) (rg-sc other) (rg-so other)))
              (1 (values (rg-ec rr) (rg-eo rr) (rg-sc other) (rg-so other)))
              (2 (values (rg-ec rr) (rg-eo rr) (rg-ec other) (rg-eo other)))
              (3 (values (rg-sc rr) (rg-so rr) (rg-ec other) (rg-eo other))))
          (num (bp-pos ca oa cb ob)))))
    (defmethod* ctx rp "cloneRange" 0 (this a)
      (let* ((rr (r this)) (obj (js:make-object :proto rp)))
        (setf (gethash obj (context-ranges ctx))
              (make-rg :sc (rg-sc rr) :so (rg-so rr) :ec (rg-ec rr) :eo (rg-eo rr)))
        obj))
    (defmethod* ctx rp "detach" 0 (this a) js:*undefined*)
    (defmethod* ctx rp "toString" 0 (this a) (rg-string (r this)))
    (defmethod* ctx rp "cloneContents" 0 (this a) (rg-clone-contents ctx (r this)))
    (defmethod* ctx rp "extractContents" 0 (this a) (rg-extract-contents ctx (r this)))
    (defmethod* ctx rp "deleteContents" 0 (this a) (rg-delete-contents ctx (r this)) js:*undefined*)
    (defmethod* ctx rp "insertNode" 1 (this a)
      (let* ((rr (r this)) (node (require-node ctx (arg a 0)))
             (c (rg-sc rr)) (o (rg-so rr))
             (parent (if (char-data-p c) (h:dnode-parent c) c))
             (ref (if (char-data-p c)
                      (split-text ctx c o)          ; ref = the second half
                      (let ((ch (h:dnode-children c))) (when (< o (length ch)) (aref ch o))))))
        (when (eq node ref) (setf ref (n-next-sib node)))
        (dom-detach node)
        (if ref (h:dom-insert-before parent node ref) (h:dom-append parent node))
        (setf (context-dirty ctx) t)
        js:*undefined*))))

(defun install-range (ctx)
  (let* ((realm (context-realm ctx))
         (op (js:eval-script realm "Object.prototype"))
         (rp (js:make-object :proto op))
         (doc (context-document ctx)))
    (setf (proto ctx :range) rp)
    (install-range-proto ctx rp)
    (let ((dp (proto ctx :document)))
      (defmethod* ctx dp "createRange" 0 (this a)
        (let ((obj (js:make-object :proto rp)) (root (require-node ctx this)))
          (setf (gethash obj (context-ranges ctx))
                (make-rg :sc root :so 0 :ec root :eo 0))
          obj)))
    ;; Range constructor: new Range() selects the document.
    (let ((ctor (js:native-function realm "Range"
                  (lambda (this args) (declare (ignore this args))
                    (let ((obj (js:make-object :proto rp)))
                      (setf (gethash obj (context-ranges ctx))
                            (make-rg :sc doc :so 0 :ec doc :eo 0))
                      obj)) 0)))
      (js:put ctor "prototype" rp :enumerable nil)
      (js:define-global realm "Range" ctor))))
