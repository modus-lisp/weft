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
  ;; Only Text nodes contribute (Comments/PIs do not).
  (let ((sc (rg-sc r)) (ec (rg-ec r)))
    (cond
      ((and (eq sc ec) (eq (h:dnode-kind sc) :text))
       (subseq (or (h:dnode-data sc) "") (rg-so r) (rg-eo r)))
      (t
       (with-output-to-string (s)
         (when (eq (h:dnode-kind sc) :text) (write-string (subseq (h:dnode-data sc) (rg-so r)) s))
         (dolist (tn (all-text-nodes (common-ancestor r)))
           (when (and (not (eq tn sc)) (not (eq tn ec))
                      (= (bp-pos tn 0 (rg-sc r) (rg-so r)) 1)
                      (= (bp-pos tn (node-len tn) (rg-ec r) (rg-eo r)) -1))
             (write-string (h:dnode-data tn) s)))
         (when (eq (h:dnode-kind ec) :text) (write-string (subseq (h:dnode-data ec) 0 (rg-eo r)) s)))))))

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
  (wrap ctx (%range-cut (rg-sc r) (rg-so r) (rg-ec r) (rg-eo r) nil)))

(defun incl-anc-p (a b) "A is B or an ancestor of B."
  (loop for n = b then (h:dnode-parent n) while n thereis (eq n a)))
(defun child-containing (common node)
  "The child of COMMON that is-or-contains NODE."
  (loop for n = node then (h:dnode-parent n) when (eq (h:dnode-parent n) common) return n))
(defun contained-children (common sc so ec eo)
  (loop for c across (h:dnode-children common)
        when (and (= (bp-pos c 0 sc so) 1) (= (bp-pos c (node-len c) ec eo) -1))
          collect c))
(defun append-frag-children (frag container)
  (loop for c across (copy-seq (h:dnode-children frag))
        do (h:dom-remove c) (h:dom-append container c)))

(defun %range-cut (sc so ec eo movep)
  "The recursive DOM extract/clone algorithm over the boundary points; MOVEP T
   mutates the source (extract), NIL leaves it (clone). Returns a fragment."
  (let ((frag (h:make-fragment)))
    (cond
      ((and (eq sc ec) (= so eo)) frag)                       ; collapsed
      ((and (eq sc ec) (char-data-p sc))                      ; within one text node
       (h:dom-append frag (h:make-text (subseq (h:dnode-data sc) so eo)))
       (when movep (setf (h:dnode-data sc)
                         (concatenate 'string (subseq (h:dnode-data sc) 0 so) (subseq (h:dnode-data sc) eo))))
       frag)
      (t
       (let* ((common (loop for a = sc then (h:dnode-parent a) when (incl-anc-p a ec) return a))
              (first-pc (unless (incl-anc-p sc ec) (child-containing common sc)))
              (last-pc  (unless (incl-anc-p ec sc) (child-containing common ec)))
              (contained (contained-children common sc so ec eo)))
         ;; first partially-contained node
         (cond
           ((and first-pc (char-data-p first-pc))
            (h:dom-append frag (h:make-text (subseq (h:dnode-data first-pc) so)))
            (when movep (setf (h:dnode-data first-pc) (subseq (h:dnode-data first-pc) 0 so))))
           (first-pc
            (let ((clone (copy-dnode first-pc nil)))
              (h:dom-append frag clone)
              (append-frag-children (%range-cut sc so first-pc (node-len first-pc) movep) clone))))
         ;; wholly-contained children
         (dolist (child contained)
           (if movep (progn (h:dom-remove child) (h:dom-append frag child))
               (h:dom-append frag (copy-dnode child t))))
         ;; last partially-contained node
         (cond
           ((and last-pc (char-data-p last-pc))
            (h:dom-append frag (h:make-text (subseq (h:dnode-data last-pc) 0 eo)))
            (when movep (setf (h:dnode-data last-pc) (subseq (h:dnode-data last-pc) eo))))
           (last-pc
            (let ((clone (copy-dnode last-pc nil)))
              (h:dom-append frag clone)
              (append-frag-children (%range-cut last-pc 0 ec eo movep) clone))))
         frag)))))

(defun %new-boundary (sc so ec)
  "The collapsed boundary a range takes after extract/delete."
  (if (incl-anc-p sc ec) (values sc so)
      (let ((ref (loop for r = sc then (h:dnode-parent r)
                       until (incl-anc-p (h:dnode-parent r) ec) finally (return r))))
        (values (h:dnode-parent ref) (1+ (node-index ref))))))

(defun rg-extract-contents (ctx r)
  (let ((frag (%range-cut (rg-sc r) (rg-so r) (rg-ec r) (rg-eo r) t)))
    (unless (rg-collapsed-p r)
      (multiple-value-bind (nc no) (%new-boundary (rg-sc r) (rg-so r) (rg-ec r))
        (setf (rg-sc r) nc (rg-so r) no (rg-ec r) nc (rg-eo r) no)))
    (setf (context-dirty ctx) t)
    (wrap ctx frag)))

(defun rg-delete-contents (ctx r)
  (unless (rg-collapsed-p r)
    (%range-cut (rg-sc r) (rg-so r) (rg-ec r) (rg-eo r) t)
    (multiple-value-bind (nc no) (%new-boundary (rg-sc r) (rg-so r) (rg-ec r))
      (setf (rg-sc r) nc (rg-so r) no (rg-ec r) nc (rg-eo r) no))
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
      (%range-insert ctx (r this) (require-node ctx (arg a 0)))
      js:*undefined*)
    (defmethod* ctx rp "surroundContents" 1 (this a)
      (let* ((rr (r this)) (np (require-node ctx (arg a 0))))
        ;; a partially-contained non-Text node (e.g. a Comment) is illegal
        (when (some (lambda (n) (not (eq (h:dnode-kind n) :text))) (partially-contained-nodes rr))
          (throw-dom ctx "InvalidStateError" 11 "range partially selects a non-Text node"))
        (let ((frag (%range-cut (rg-sc rr) (rg-so rr) (rg-ec rr) (rg-eo rr) t)))
          (loop for c across (copy-seq (h:dnode-children np)) do (h:dom-remove c))
          (%range-insert ctx rr np)                 ; may raise HierarchyRequestError
          (append-frag-children frag np)
          (setf (rg-sc rr) (h:dnode-parent np) (rg-so rr) (node-index np)
                (rg-ec rr) (h:dnode-parent np) (rg-eo rr) (1+ (node-index np)))))
      js:*undefined*)
    ;; DOM Parsing §Range.createContextualFragment: parse HTML in the context of
    ;; the range's start node's nearest element (default <body>).
    (defmethod* ctx rp "createContextualFragment" 1 (this a)
      (when (null a)
        (js:js-throw (js:make-native-error "TypeError" "createContextualFragment requires 1 argument")))
      (let* ((rr (r this)) (html (jstr (arg a 0)))
             (elt (loop for n = (rg-sc rr) then (h:dnode-parent n)
                        while n when (eq (h:dnode-kind n) :element) return n))
             (ctxname (if (and elt (not (string= (h:dnode-name elt) "html")))
                          (h:dnode-name elt) "body"))
             (frag (h:parse-fragment html ctxname)))
        (adopt-fragment-owners ctx frag (owner-doc-node ctx (rg-sc rr)))
        (wrap ctx frag)))))

(defun partially-contained-nodes (rr)
  "Nodes that are an inclusive ancestor of exactly one of the range's boundary
   containers (they hold one boundary point but not both)."
  (let ((sc (rg-sc rr)) (ec (rg-ec rr)) (out '()))
    (loop for n = sc then (h:dnode-parent n) while n
          when (not (incl-anc-p n ec)) do (pushnew n out))
    (loop for n = ec then (h:dnode-parent n) while n
          when (not (incl-anc-p n sc)) do (pushnew n out))
    out))

(defun %range-insert (ctx rr node)
  "Insert NODE at the range's start (splitting a Text start container), enforcing
   the document single-element-child constraint."
  (let* ((c (rg-sc rr)) (o (rg-so rr))
         (parent (if (char-data-p c) (h:dnode-parent c) c))
         (ref (if (char-data-p c) (split-text ctx c o)
                  (let ((ch (h:dnode-children c))) (when (< o (length ch)) (aref ch o))))))
    (when (eq node ref) (setf ref (n-next-sib node)))
    (when (and (eq (h:dnode-kind parent) :document) (eq (h:dnode-kind node) :element)
               (loop for ch across (h:dnode-children parent)
                     thereis (and (eq (h:dnode-kind ch) :element) (not (eq ch node)))))
      (throw-dom ctx "HierarchyRequestError" 3 "document can have only one element child"))
    (dom-detach node)
    (if ref (h:dom-insert-before parent node ref) (h:dom-append parent node))
    (setf (context-dirty ctx) t)))

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
