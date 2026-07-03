;;;; src/script/traversal.lisp — DOM Traversal: NodeIterator + TreeWalker.
;;;;
;;;; Document-order traversal of a subtree filtered by a whatToShow bitmask and
;;;; an optional NodeFilter (a function, or an object with acceptNode).  The
;;;; filter may throw; the exception propagates to the caller, as the spec (and
;;;; Acid3) require.
(in-package #:weft.script)

(defstruct ni root what filter ref (before t))     ; NodeIterator state
(defstruct tw root what filter current)            ; TreeWalker state

;;; ---- document-order stepping (restricted to a root subtree) ---------------
(defun n-children (node) (h:dnode-children node))
(defun n-first (node) (let ((c (n-children node))) (when (plusp (length c)) (aref c 0))))
(defun n-last (node) (h:dom-last-child node))
(defun n-next-sib (node)
  (let* ((p (h:dnode-parent node)) (ch (and p (n-children p))) (i (and ch (position node ch))))
    (when (and i (< (1+ i) (length ch))) (aref ch (1+ i)))))
(defun n-prev-sib (node)
  (let* ((p (h:dnode-parent node)) (ch (and p (n-children p))) (i (and ch (position node ch))))
    (when (and i (plusp i)) (aref ch (1- i)))))

(defun following (node root)
  "The next node after NODE in document order, staying within ROOT's subtree."
  (let ((f (n-first node)))
    (if f f
        (loop for n = node then (h:dnode-parent n)
              while (and n (not (eq n root)))
              do (let ((s (n-next-sib n))) (when s (return s)))
              finally (return nil)))))

(defun preceding (node root)
  "The previous node before NODE in document order, staying within ROOT."
  (when (eq node root) (return-from preceding nil))
  (let ((s (n-prev-sib node)))
    (if s (loop for n = s then (n-last n) while (n-last n) finally (return n))
        (let ((p (h:dnode-parent node))) (and p (not (eq node root)) p)))))

;;; ---- filtering ------------------------------------------------------------
(defun node-show-bit (node)
  (ash 1 (1- (truncate (node-type-of node)))))

(defun run-filter (ctx filter what node)
  "Return 1 (accept), 2 (reject) or 3 (skip) for NODE."
  (if (zerop (logand what (node-show-bit node))) 3
      (if (nullish filter) 1
          (let ((r (cond ((js:js-callable-p filter)
                          (js:js-call filter js:*undefined* (list (wrap ctx node))))
                         ((js:js-object-p filter)
                          (js:js-call (js:js-get filter "acceptNode") filter (list (wrap ctx node))))
                         (t js:*undefined*))))
            (js-int r)))))

;;; ---- NodeIterator ---------------------------------------------------------
(defun ni-traverse (ctx state next-p)
  (let ((node (ni-ref state)) (before (ni-before state)))
    (loop
      (if next-p
          (if (not before)
              (let ((f (following node (ni-root state))))
                (if f (setf node f) (return-from ni-traverse js:*null*)))
              (setf before nil))
          (if before
              (let ((p (preceding node (ni-root state))))
                (if p (setf node p) (return-from ni-traverse js:*null*)))
              (setf before t)))
      (when (= (run-filter ctx (ni-filter state) (ni-what state) node) 1)
        (setf (ni-ref state) node (ni-before state) before)
        (return-from ni-traverse (wrap ctx node))))))

(defun install-nodeiterator-proto (ctx nip)
  (macrolet ((st (this) `(or (gethash ,this (context-traversal ctx))
                             (js:js-throw (js:make-native-error "TypeError" "not a NodeIterator")))))
    (defget ctx nip "root" (this) (wrap ctx (ni-root (st this))))
    (defget ctx nip "referenceNode" (this) (wrap ctx (ni-ref (st this))))
    (defget ctx nip "pointerBeforeReferenceNode" (this) (jbool (ni-before (st this))))
    (defget ctx nip "whatToShow" (this) (num (ni-what (st this))))
    (defget ctx nip "filter" (this) (or (ni-filter (st this)) js:*null*))
    (defmethod* ctx nip "nextNode" 0 (this a) (ni-traverse ctx (st this) t))
    (defmethod* ctx nip "previousNode" 0 (this a) (ni-traverse ctx (st this) nil))
    (defmethod* ctx nip "detach" 0 (this a) js:*undefined*)))

;;; ---- TreeWalker -----------------------------------------------------------
(defun tw-child (ctx state first-p)
  "firstChild/lastChild per WHATWG traverse-children."
  (let ((node (if first-p (n-first (tw-current state)) (n-last (tw-current state)))))
    (loop while node do
      (let* ((r (run-filter ctx (tw-filter state) (tw-what state) node))
             (descend (and (= r 3) (if first-p (n-first node) (n-last node)))))
        (cond
          ((= r 1) (setf (tw-current state) node) (return-from tw-child (wrap ctx node)))
          (descend (setf node descend))     ; skip with children: descend
          (t                                ; reject, or skip w/o children: follow
           (loop
             (let ((sib (if first-p (n-next-sib node) (n-prev-sib node))))
               (when sib (setf node sib) (return))
               (setf node (h:dnode-parent node))
               (when (or (null node) (eq node (tw-current state)))
                 (return-from tw-child js:*null*))))))))
    js:*null*))

(defun tw-sibling (ctx state next-p)
  "nextSibling/previousSibling per WHATWG traverse-siblings."
  (let ((node (tw-current state)))
    (when (eq node (tw-root state)) (return-from tw-sibling js:*null*))
    (loop
      (let ((sib (if next-p (n-next-sib node) (n-prev-sib node))))
        (loop while sib do
          (setf node sib)
          (let ((r (run-filter ctx (tw-filter state) (tw-what state) node)))
            (when (= r 1) (setf (tw-current state) node) (return-from tw-sibling (wrap ctx node)))
            (setf sib (if next-p (n-first node) (n-last node)))
            (when (or (= r 2) (null sib))
              (setf sib (if next-p (n-next-sib node) (n-prev-sib node))))))
        (setf node (h:dnode-parent node))
        (when (or (null node) (eq node (tw-root state))) (return-from tw-sibling js:*null*))
        (when (= (run-filter ctx (tw-filter state) (tw-what state) node) 1)
          (return-from tw-sibling js:*null*))))))

(defun tw-parent (ctx state)
  "parentNode per WHATWG: climb to the nearest accepted ancestor within root."
  (let ((node (tw-current state)))
    (loop
      (when (or (null node) (eq node (tw-root state))) (return-from tw-parent js:*null*))
      (setf node (h:dnode-parent node))
      (when (null node) (return-from tw-parent js:*null*))
      (when (= (run-filter ctx (tw-filter state) (tw-what state) node) 1)
        (setf (tw-current state) node) (return-from tw-parent (wrap ctx node)))
      (when (eq node (tw-root state)) (return-from tw-parent js:*null*)))))

(defun tw-traverse (ctx state next-p)
  "nextNode/previousNode in document order, applying the filter."
  (let ((node (tw-current state)))
    (loop
      (let ((cand (if next-p (following node (tw-root state)) (preceding node (tw-root state)))))
        (when (null cand) (return-from tw-traverse js:*null*))
        (setf node cand)
        (let ((r (run-filter ctx (tw-filter state) (tw-what state) node)))
          (when (= r 1) (setf (tw-current state) node)
            (return-from tw-traverse (wrap ctx node))))))))

(defun install-treewalker-proto (ctx twp)
  (macrolet ((st (this) `(or (gethash ,this (context-traversal ctx))
                             (js:js-throw (js:make-native-error "TypeError" "not a TreeWalker")))))
    (defget ctx twp "root" (this) (wrap ctx (tw-root (st this))))
    (defget ctx twp "whatToShow" (this) (num (tw-what (st this))))
    (defget ctx twp "filter" (this) (or (tw-filter (st this)) js:*null*))
    (defgetset ctx twp "currentNode" (this) (wrap ctx (tw-current (st this)))
      (v) (let ((n (node-of ctx v))) (when n (setf (tw-current (st this)) n))))
    (defmethod* ctx twp "parentNode" 0 (this a) (tw-parent ctx (st this)))
    (defmethod* ctx twp "firstChild" 0 (this a) (tw-child ctx (st this) t))
    (defmethod* ctx twp "lastChild" 0 (this a) (tw-child ctx (st this) nil))
    (defmethod* ctx twp "nextSibling" 0 (this a) (tw-sibling ctx (st this) t))
    (defmethod* ctx twp "previousSibling" 0 (this a) (tw-sibling ctx (st this) nil))
    (defmethod* ctx twp "nextNode" 0 (this a) (tw-traverse ctx (st this) t))
    (defmethod* ctx twp "previousNode" 0 (this a) (tw-traverse ctx (st this) nil))))

;;; ---- factory + globals ----------------------------------------------------
(defun what-arg (args n)
  "whatToShow argument: default SHOW_ALL (0xFFFFFFFF)."
  (let ((v (arg args n)))
    (if (js:js-undefined-p v) #xFFFFFFFF (logand (js-int v) #xFFFFFFFF))))

(defun filter-arg (args n)
  (let ((v (arg args n))) (if (nullish v) nil v)))

(defun install-traversal (ctx)
  (let* ((realm (context-realm ctx))
         (op (js:eval-script realm "Object.prototype"))
         (nip (js:make-object :proto op))
         (twp (js:make-object :proto op)))
    (setf (proto ctx :node-iterator) nip (proto ctx :tree-walker) twp)
    (install-nodeiterator-proto ctx nip)
    (install-treewalker-proto ctx twp)
    (let ((dp (proto ctx :document)))
      (defmethod* ctx dp "createNodeIterator" 3 (this a)
        (let ((root (require-node ctx (arg a 0)))
              (obj (js:make-object :proto nip)))
          (setf (gethash obj (context-traversal ctx))
                (make-ni :root root :what (what-arg a 1) :filter (filter-arg a 2) :ref root :before t))
          obj))
      (defmethod* ctx dp "createTreeWalker" 3 (this a)
        (let ((root (require-node ctx (arg a 0)))
              (obj (js:make-object :proto twp)))
          (setf (gethash obj (context-traversal ctx))
                (make-tw :root root :what (what-arg a 1) :filter (filter-arg a 2) :current root))
          obj)))
    ;; NodeFilter global with SHOW_* and FILTER_* constants.
    (let ((nf (js:make-object :proto op)))
      (dolist (pair '(("FILTER_ACCEPT" . 1) ("FILTER_REJECT" . 2) ("FILTER_SKIP" . 3)
                      ("SHOW_ALL" . #xFFFFFFFF) ("SHOW_ELEMENT" . 1) ("SHOW_ATTRIBUTE" . 2)
                      ("SHOW_TEXT" . 4) ("SHOW_CDATA_SECTION" . 8) ("SHOW_ENTITY_REFERENCE" . 16)
                      ("SHOW_ENTITY" . 32) ("SHOW_PROCESSING_INSTRUCTION" . 64) ("SHOW_COMMENT" . 128)
                      ("SHOW_DOCUMENT" . 256) ("SHOW_DOCUMENT_TYPE" . 512)
                      ("SHOW_DOCUMENT_FRAGMENT" . 1024) ("SHOW_NOTATION" . 2048)))
        (js:put nf (car pair) (num (cdr pair)) :enumerable nil :writable nil :configurable nil))
      (js:define-global realm "NodeFilter" nf))))
