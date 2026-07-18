;;;; src/script/dom.lisp — the DOM core surface bound onto shuttle host objects.
;;;;
;;;; Node / Element / Document / CharacterData (Text, Comment) / DocumentFragment,
;;;; their traversal accessors, the mutation methods, attribute reflection, and
;;;; the document factory + lookups.  Methods live on shared per-context
;;;; prototypes and recover their backing weft node from `this` via NODE-OF.
(in-package #:weft.script)

;;; ---- node-kind reflection -------------------------------------------------
(defun node-type-of (node)
  (num (case (h:dnode-kind node)
         (:element 1) (:text 3) (:cdata 4) (:processing-instruction 7)
         (:comment 8) (:document 9)
         (:doctype 10) (:fragment 11) (t 1))))

(defvar *html-ns* "http://www.w3.org/1999/xhtml")

(defun ascii-upcase (s)
  "ASCII-only uppercasing (DOM tagName): bytes A-Z, leaving non-ASCII intact."
  (map 'string (lambda (c) (if (char<= #\a c #\z) (char-upcase c) c)) s))

(defun element-tag-name (node &optional ctx)
  "tagName/nodeName for an element (DOM §Element): the qualified name, ASCII
   upper-cased when the element is in the HTML namespace *and* its node document
   is an HTML document (so it re-lowercases after adoption into an XML document);
   case-preserved otherwise (XML/SVG names are case-sensitive)."
  (let* ((info (and ctx (gethash node (context-ns-info ctx))))
         (qname (if info (getf info :qname) (h:dnode-name node))))
    (if (and ctx (equal (element-real-ns ctx node) *html-ns*)
             (html-doc-p ctx (node-document ctx node)))
        (ascii-upcase qname)
        qname)))

(defun node-name-of (node &optional ctx)
  (case (h:dnode-kind node)
    (:element (element-tag-name node ctx))
    (:text "#text") (:comment "#comment") (:document "#document")
    (:cdata "#cdata-section")
    (:processing-instruction (or (h:dnode-name node) ""))
    (:fragment "#document-fragment")
    (:doctype (or (h:dnode-name node) "")) (t "")))

(defun char-data-p (node)
  (member (h:dnode-kind node) '(:text :comment :cdata :processing-instruction)))

(defun tag= (node name) (and (eq (h:dnode-kind node) :element)
                             (string= (h:dnode-name node) name)))

(defun internal-attr-p (name) (string-equal name "weft-checked"))

;;; ---- DOMException ---------------------------------------------------------
(defparameter +dom-codes+
  '(("INDEX_SIZE_ERR" . 1) ("DOMSTRING_SIZE_ERR" . 2) ("HIERARCHY_REQUEST_ERR" . 3)
    ("WRONG_DOCUMENT_ERR" . 4) ("INVALID_CHARACTER_ERR" . 5) ("NO_DATA_ALLOWED_ERR" . 6)
    ("NO_MODIFICATION_ALLOWED_ERR" . 7) ("NOT_FOUND_ERR" . 8) ("NOT_SUPPORTED_ERR" . 9)
    ("INUSE_ATTRIBUTE_ERR" . 10) ("INVALID_STATE_ERR" . 11) ("SYNTAX_ERR" . 12)
    ("INVALID_MODIFICATION_ERR" . 13) ("NAMESPACE_ERR" . 14) ("INVALID_ACCESS_ERR" . 15)
    ("VALIDATION_ERR" . 16) ("TYPE_MISMATCH_ERR" . 17) ("SECURITY_ERR" . 18)
    ("NETWORK_ERR" . 19) ("ABORT_ERR" . 20) ("URL_MISMATCH_ERR" . 21)
    ("QUOTA_EXCEEDED_ERR" . 22) ("TIMEOUT_ERR" . 23) ("INVALID_NODE_TYPE_ERR" . 24)
    ("DATA_CLONE_ERR" . 25)))

(defun make-dom-exception (ctx name code message)
  ;; Proto = the real DOMException.prototype (installed in bridge.lisp) so that
  ;; `e instanceof DOMException` and testharness's `e.constructor === DOMException`
  ;; hold; the legacy numeric code constants are inherited from that prototype.
  (let ((o (js:make-object :proto (js:eval-script (context-realm ctx) "DOMException.prototype"))))
    (js:put o "name" name :enumerable nil)
    (js:put o "message" message :enumerable nil)
    (js:put o "code" (num code) :enumerable nil)
    o))

(defun throw-dom (ctx name code message)
  (js:js-throw (make-dom-exception ctx name code message)))

(defun ancestor-or-self-p (a node)
  "T if A is NODE or an ancestor of NODE."
  (loop for p = node then (h:dnode-parent p) while p thereis (eq p a)))

(defun name-start-char-p (c)
  (or (alpha-char-p c) (member c '(#\_ #\:))))
(defun valid-name-p (name)
  "A conservative XML Name check: non-empty, a letter/_/: start, and no control
   char, space or markup delimiter (rejects null bytes and digit-led names, per
   Acid3 tests 20/22/23)."
  (and (plusp (length name))
       (name-start-char-p (char name 0))
       (every (lambda (c) (let ((cc (char-code c)))
                            (and (>= cc #x21)
                                 (not (member c '(#\< #\> #\& #\" #\' #\/ #\= #\Space))))))
              name)))

;;; ---- structural mutation (weft side) --------------------------------------
(defun set-text-content (node text)
  "The textContent setter: detach NODE's children and, if TEXT is non-empty,
   give it a single text child holding TEXT (DOM Node.textContent)."
  (if (char-data-p node)
      (setf (h:dnode-data node) text)
      (let ((children (h:dnode-children node)))
        (loop for c across children do (setf (h:dnode-parent c) nil))
        (setf (fill-pointer children) 0)
        (when (plusp (length text))
          (h:dom-append node (h:make-text text))))))

(defun nullable-string (v)
  "A nullable DOMString attribute setter value: null or undefined -> \"\" (DOM
   §nodeValue/§textContent setters), else the usual stringification."
  (if (nullish v) "" (jstr v)))

(defun null->empty (v)
  "WebIDL [LegacyNullToEmptyString]: a JS null becomes \"\" (used by the
innerHTML/outerHTML setters); everything else stringifies normally."
  (if (eq v js:*null*) "" (jstr v)))

(defun dom-detach (node) (when (h:dnode-parent node) (h:dom-remove node)))

(defun replace-all-children (node fragment)
  "Detach every child of NODE and append FRAGMENT's children in their place
(the innerHTML-setter 'replace all' step)."
  (let ((children (h:dnode-children node)))
    (loop for c across children do (setf (h:dnode-parent c) nil))
    (setf (fill-pointer children) 0))
  (loop for c across (copy-seq (h:dnode-children fragment))
        do (h:dom-remove c) (h:dom-append node c)))

(defun mark-fragment-scripts-started (ctx fragment)
  "Set the 'already started' flag on every <script> parsed as part of FRAGMENT
(HTML §fragment parsing / §innerHTML) so it never executes, even after a later
insertion into a connected tree."
  (labels ((walk (n)
             (when (eq (h:dnode-kind n) :element)
               (when (string-equal (h:dnode-name n) "script")
                 (setf (gethash n (context-ran-scripts ctx)) t))
               (loop for c across (h:dnode-children n) do (walk c)))))
    (loop for c across (h:dnode-children fragment) do (walk c))))

(defun adopt-fragment-owners (ctx fragment doc)
  "Record DOC as the owner document for FRAGMENT's top-level children (so a
detached subtree still answers ownerDocument correctly)."
  (loop for c across (h:dnode-children fragment)
        unless (gethash c (context-owner-docs ctx))
          do (setf (gethash c (context-owner-docs ctx)) doc)))

(defun adopt-parsed-fragment (ctx fragment doc)
  "Owner-adopt FRAGMENT and neuter its scripts — the innerHTML / outerHTML /
insertAdjacentHTML path, where parsed <script>s must never execute (unlike
Range.createContextualFragment, whose scripts run when later inserted)."
  (mark-fragment-scripts-started ctx fragment)
  (adopt-fragment-owners ctx fragment doc))

(defun new-node (ctx doc-obj node)
  "Wrap a freshly created NODE, recording its owner document (the document whose
   factory made it) so ownerDocument is correct even while the node is detached."
  (setf (gethash node (context-owner-docs ctx)) (require-node ctx doc-obj))
  (wrap ctx node))

(defun ensure-can-have-children (ctx parent)
  "DOM pre-insertion validity: only Document/DocumentFragment/Element may hold
children; a Text/Comment/CDATA/PI/DocumentType parent throws HierarchyRequestError."
  (unless (member (h:dnode-kind parent) '(:document :fragment :element))
    (throw-dom ctx "HierarchyRequestError" 3 "node cannot have children")))

(defun element-child-count (parent)
  (count :element (h:dnode-children parent) :key #'h:dnode-kind))
(defun doctype-child-p (parent)
  (find :doctype (h:dnode-children parent) :key #'h:dnode-kind))
(defun sibling-of-kind-after (parent child kind)
  (let* ((ch (h:dnode-children parent)) (i (and child (position child ch))))
    (and i (find kind ch :start (1+ i) :key #'h:dnode-kind))))
(defun sibling-of-kind-before (parent child kind)
  (let* ((ch (h:dnode-children parent)) (i (and child (position child ch))))
    (and i (plusp i) (find kind (subseq ch 0 i) :key #'h:dnode-kind))))

(defun element-child-count-excluding (parent except)
  (count-if (lambda (c) (and (eq (h:dnode-kind c) :element) (not (eq c except))))
            (h:dnode-children parent)))
(defun doctype-child-excluding (parent except)
  (find-if (lambda (c) (and (eq (h:dnode-kind c) :doctype) (not (eq c except))))
           (h:dnode-children parent)))

(defun ensure-replace-validity (ctx node parent child)
  "DOM §replaceChild pre-checks: like pre-insertion validity but CHILD (the node
   being replaced) is excluded from the document element/doctype counts."
  (unless (member (h:dnode-kind parent) '(:document :fragment :element))
    (throw-dom ctx "HierarchyRequestError" 3 "parent cannot have children"))
  (when (ancestor-or-self-p node parent)
    (throw-dom ctx "HierarchyRequestError" 3 "would create a cycle"))
  (unless (eq (h:dnode-parent child) parent)
    (throw-dom ctx "NotFoundError" 8 "node to replace is not a child"))
  (unless (member (h:dnode-kind node)
                  '(:fragment :doctype :element :text :comment :cdata :processing-instruction))
    (throw-dom ctx "HierarchyRequestError" 3 "node cannot be inserted here"))
  (when (or (and (member (h:dnode-kind node) '(:text :cdata)) (eq (h:dnode-kind parent) :document))
            (and (eq (h:dnode-kind node) :doctype) (not (eq (h:dnode-kind parent) :document))))
    (throw-dom ctx "HierarchyRequestError" 3 "invalid node for this parent"))
  (when (eq (h:dnode-kind parent) :document)
    (flet ((bad () (throw-dom ctx "HierarchyRequestError" 3 "invalid document child")))
      (case (h:dnode-kind node)
        (:fragment
         (let ((elems (element-child-count node))
               (text (find-if (lambda (c) (member (h:dnode-kind c) '(:text :cdata)))
                              (h:dnode-children node))))
           (when (or (> elems 1) text) (bad))
           (when (and (= elems 1)
                      (or (plusp (element-child-count-excluding parent child))
                          (sibling-of-kind-after parent child :doctype)))
             (bad))))
        (:element
         (when (or (plusp (element-child-count-excluding parent child))
                   (sibling-of-kind-after parent child :doctype))
           (bad)))
        (:doctype
         (when (or (doctype-child-excluding parent child)
                   (sibling-of-kind-before parent child :element))
           (bad)))))))

(defun ensure-pre-insertion-validity (ctx node parent child)
  "DOM §\"ensure pre-insertion validity\": validate inserting NODE into PARENT
   before CHILD (NIL = append), throwing HierarchyRequestError/NotFoundError."
  (unless (member (h:dnode-kind parent) '(:document :fragment :element))
    (throw-dom ctx "HierarchyRequestError" 3 "parent cannot have children"))
  (when (ancestor-or-self-p node parent)
    (throw-dom ctx "HierarchyRequestError" 3 "would create a cycle"))
  (when (and child (not (eq (h:dnode-parent child) parent)))
    (throw-dom ctx "NotFoundError" 8 "reference child is not a child of parent"))
  (unless (member (h:dnode-kind node)
                  '(:fragment :doctype :element :text :comment :cdata :processing-instruction))
    (throw-dom ctx "HierarchyRequestError" 3 "node cannot be inserted here"))
  (when (or (and (member (h:dnode-kind node) '(:text :cdata)) (eq (h:dnode-kind parent) :document))
            (and (eq (h:dnode-kind node) :doctype) (not (eq (h:dnode-kind parent) :document))))
    (throw-dom ctx "HierarchyRequestError" 3 "invalid node for this parent"))
  (when (eq (h:dnode-kind parent) :document)
    (flet ((bad () (throw-dom ctx "HierarchyRequestError" 3 "invalid document child")))
      (case (h:dnode-kind node)
        (:fragment
         (let ((elems (element-child-count node))
               (text (find-if (lambda (c) (member (h:dnode-kind c) '(:text :cdata)))
                              (h:dnode-children node))))
           (when (or (> elems 1) text) (bad))
           (when (and (= elems 1)
                      (or (plusp (element-child-count parent))
                          (and child (eq (h:dnode-kind child) :doctype))
                          (sibling-of-kind-after parent child :doctype)))
             (bad))))
        (:element
         (when (or (plusp (element-child-count parent))
                   (and child (eq (h:dnode-kind child) :doctype))
                   (sibling-of-kind-after parent child :doctype))
           (bad)))
        (:doctype
         (when (or (doctype-child-p parent)
                   (sibling-of-kind-before parent child :element)
                   (and (null child) (plusp (element-child-count parent))))
           (bad)))))))

(defun insert-into (parent node ref)
  "Insert NODE (or, if NODE is a fragment, each of its children) into PARENT
   before REF (NIL = append). Returns NODE."
  (if (eq (h:dnode-kind node) :fragment)
      (loop for c across (copy-seq (h:dnode-children node))
            do (h:dom-remove c) (if ref (h:dom-insert-before parent c ref)
                                     (h:dom-append parent c)))
      (progn (dom-detach node)
             (if ref (h:dom-insert-before parent node ref) (h:dom-append parent node))))
  node)

(defun node-next-sibling (node)
  (let ((p (h:dnode-parent node)))
    (when p (let* ((ch (h:dnode-children p)) (i (position node ch)))
              (when (and i (< (1+ i) (length ch))) (aref ch (1+ i)))))))

(defun owner-doc-node (ctx node)
  (or (gethash node (context-owner-docs ctx)) (context-document ctx)))

(defun adopt-subtree (ctx node doc)
  "Set NODE's (and every descendant's) recorded node document to DOC (DOM §adopt)."
  (setf (gethash node (context-owner-docs ctx)) doc)
  (loop for c across (h:dnode-children node) do (adopt-subtree ctx c doc)))

(defun insert-adjacent (ctx element where node)
  "DOM §\"insert adjacent\": place NODE relative to ELEMENT per WHERE (ASCII
   case-insensitive).  Returns NODE, or NIL for a before/after position when
   ELEMENT has no parent; an unknown WHERE throws SyntaxError."
  (let ((w (string-downcase where)))
    (cond
      ((string= w "beforebegin")
       (let ((parent (h:dnode-parent element)))
         (when parent
           (ensure-pre-insertion-validity ctx node parent element)
           (insert-into parent node element) node)))
      ((string= w "afterbegin")
       (let ((ref (and (plusp (length (h:dnode-children element)))
                       (aref (h:dnode-children element) 0))))
         (ensure-pre-insertion-validity ctx node element ref)
         (insert-into element node ref) node))
      ((string= w "beforeend")
       (ensure-pre-insertion-validity ctx node element nil)
       (insert-into element node nil) node)
      ((string= w "afterend")
       (let ((parent (h:dnode-parent element)))
         (when parent
           (let ((ref (node-next-sibling element)))
             (ensure-pre-insertion-validity ctx node parent ref)
             (insert-into parent node ref) node))))
      (t (throw-dom ctx "SyntaxError" 12 "invalid insertAdjacent position")))))

(defun args->insertion (ctx doc args)
  "DOM §\"converting nodes into a node\": ARGS (each a DOM node or a string, the
   latter becoming a Text node) collapse to a single node to insert — the lone
   node, or a DocumentFragment holding several.  NIL when ARGS is empty."
  (let ((items (mapcar (lambda (x) (or (node-of ctx x) (h:make-text (jstr x)))) args)))
    (dolist (it items)
      (unless (gethash it (context-owner-docs ctx))
        (setf (gethash it (context-owner-docs ctx)) doc)))
    (cond ((null items) nil)
          ((null (cdr items)) (first items))
          (t (let ((frag (h:make-fragment)))
               (dolist (it items) (h:dom-remove it) (h:dom-append frag it))
               frag)))))

(defun install-parent-node-methods (ctx proto)
  "ParentNode mixin (DOM §4.2.6): append / prepend / replaceChildren."
  (macrolet ((n (this) `(require-node ctx ,this)))
    (defmethod* ctx proto "append" 1 (this a)
      (let* ((parent (n this)) (ins (args->insertion ctx (owner-doc-node ctx parent) a)))
        (when ins
          (ensure-pre-insertion-validity ctx ins parent nil)
          (insert-into parent ins nil) (setf (context-dirty ctx) t)))
      js:*undefined*)
    (defmethod* ctx proto "prepend" 1 (this a)
      (let* ((parent (n this)) (ins (args->insertion ctx (owner-doc-node ctx parent) a))
             (ch (h:dnode-children parent)) (ref (and (plusp (length ch)) (aref ch 0))))
        (when ins
          (ensure-pre-insertion-validity ctx ins parent ref)
          (insert-into parent ins ref) (setf (context-dirty ctx) t)))
      js:*undefined*)
    (defmethod* ctx proto "replaceChildren" 1 (this a)
      (let* ((parent (n this)) (ins (args->insertion ctx (owner-doc-node ctx parent) a)))
        ;; Validate before removing existing children (DOM §replaceChildren).
        (when ins (ensure-pre-insertion-validity ctx ins parent nil))
        (loop for c across (copy-seq (h:dnode-children parent)) do (h:dom-remove c))
        (when ins (insert-into parent ins nil))
        (setf (context-dirty ctx) t))
      js:*undefined*)))

(defun install-parent-node-queries (ctx proto)
  "ParentNode query surface (DOM §4.2.6): children / first|lastElementChild /
childElementCount / querySelector / querySelectorAll.  Shared by Element,
Document and DocumentFragment prototypes."
  (macrolet ((n (this) `(require-node ctx ,this)))
    (defget ctx proto "children" (this)
      (let ((node (n this))) (make-collection ctx (lambda () (element-children-list node)))))
    (defget ctx proto "firstElementChild" (this) (wrap ctx (dom:first-element-child (n this))))
    (defget ctx proto "lastElementChild" (this) (wrap ctx (dom:last-element-child (n this))))
    (defget ctx proto "childElementCount" (this) (num (dom:child-element-count (n this))))
    (defmethod* ctx proto "querySelector" 1 (this a)
      (let ((sl (parse-selector-or-throw ctx (jstr (arg a 0)))))
        (let ((m (and sl (qs-first (n this) sl)))) (if m (wrap ctx m) js:*null*))))
    (defmethod* ctx proto "querySelectorAll" 1 (this a)
      (let ((sl (parse-selector-or-throw ctx (jstr (arg a 0)))))
        (make-collection ctx (lambda () (and sl (qs-all (n this) sl))) nil :nodelist)))))

(defun install-fragment-proto (ctx fp)
  "DocumentFragment.prototype: the ParentNode mixin (append/prepend/
replaceChildren + the query surface) plus getElementById (DOM §4.2.7)."
  (macrolet ((n (this) `(require-node ctx ,this)))
    (install-parent-node-methods ctx fp)
    (install-parent-node-queries ctx fp)
    (defmethod* ctx fp "getElementById" 1 (this a)
      (let* ((id (jstr (arg a 0)))
             (m (and (plusp (length id)) (dom:get-element-by-id (n this) id))))
        (if m (wrap ctx m) js:*null*)))))

(defun arg-node-set (ctx args)
  "The DOM nodes among ARGS (strings, which become Text nodes, are ignored)."
  (loop for x in args for nn = (node-of ctx x) when nn collect nn))
(defun viable-next-sibling (node argnodes)
  "NODE's first following sibling that is not in ARGNODES, or NIL."
  (let ((s (node-next-sibling node)))
    (loop while (and s (member s argnodes)) do (setf s (node-next-sibling s)))
    s))
(defun node-prev-sibling (node)
  (let ((p (h:dnode-parent node)))
    (when p (let* ((ch (h:dnode-children p)) (i (position node ch)))
              (when (and i (plusp i)) (aref ch (1- i)))))))
(defun viable-prev-sibling (node argnodes)
  "NODE's first preceding sibling that is not in ARGNODES, or NIL."
  (let ((s (node-prev-sibling node)))
    (loop while (and s (member s argnodes)) do (setf s (node-prev-sibling s)))
    s))

(defun normalize-node (ctx node)
  "DOM §Node.normalize: drop empty Text descendants and merge each run of
   contiguous Text siblings into the first."
  (loop for c across (copy-seq (h:dnode-children node))
        when (eq (h:dnode-kind c) :element) do (normalize-node ctx c))
  (let ((i 0))
    (loop while (< i (length (h:dnode-children node)))
          do (let ((c (aref (h:dnode-children node) i)))
               (cond
                 ((not (eq (h:dnode-kind c) :text)) (incf i))
                 ;; An empty Text node is dropped (so the first *non-empty* node of
                 ;; a run is the one that survives and absorbs the rest).
                 ((zerop (length (or (h:dnode-data c) "")))
                  (adjust-ranges-for-removal ctx c) (h:dom-remove c))
                 (t (loop while (and (< (1+ i) (length (h:dnode-children node)))
                                     (eq (h:dnode-kind (aref (h:dnode-children node) (1+ i))) :text))
                          do (let ((nx (aref (h:dnode-children node) (1+ i))))
                               (setf (h:dnode-data c)
                                     (concatenate 'string (or (h:dnode-data c) "")
                                                  (or (h:dnode-data nx) "")))
                               (adjust-ranges-for-removal ctx nx)
                               (h:dom-remove nx)))
                    (incf i)))))))

(defun install-child-node-methods (ctx proto)
  "ChildNode mixin (DOM §4.2.7): before / after / replaceWith / remove."
  (macrolet ((n (this) `(require-node ctx ,this)))
    (defmethod* ctx proto "remove" 0 (this a) (declare (ignore a))
      (let ((node (n this)))
        (when (h:dnode-parent node)
          (adjust-ranges-for-removal ctx node)
          (ni-pre-remove ctx node)
          (h:dom-remove node) (setf (context-dirty ctx) t)))
      js:*undefined*)
    ;; before/after/replaceWith must skip any argument nodes when choosing the
    ;; reference sibling (DOM §"viable previous/next sibling"), so that passing a
    ;; current sibling as an argument still lands the moved node in the right slot.
    (defmethod* ctx proto "before" 1 (this a)
      (let* ((node (n this)) (parent (h:dnode-parent node)))
        (when parent
          (let* ((argnodes (arg-node-set ctx a))
                 (viable (viable-prev-sibling node argnodes))
                 (ins (args->insertion ctx (owner-doc-node ctx node) a))
                 (ref (if viable (node-next-sibling viable)
                          (let ((ch (h:dnode-children parent)))
                            (and (plusp (length ch)) (aref ch 0))))))
            (when ins (insert-into parent ins ref) (setf (context-dirty ctx) t)))))
      js:*undefined*)
    (defmethod* ctx proto "after" 1 (this a)
      (let* ((node (n this)) (parent (h:dnode-parent node)))
        (when parent
          (let* ((argnodes (arg-node-set ctx a))
                 (ref (viable-next-sibling node argnodes))
                 (ins (args->insertion ctx (owner-doc-node ctx node) a)))
            (when ins (insert-into parent ins ref) (setf (context-dirty ctx) t)))))
      js:*undefined*)
    (defmethod* ctx proto "replaceWith" 1 (this a)
      (let* ((node (n this)) (parent (h:dnode-parent node)))
        (when parent
          (let* ((argnodes (arg-node-set ctx a))
                 (ref (viable-next-sibling node argnodes))
                 (ins (args->insertion ctx (owner-doc-node ctx node) a)))
            ;; If THIS is still a child, replace it in place; if converting the
            ;; arguments moved THIS into the fragment (it was itself an argument),
            ;; just pre-insert the node before the viable sibling (DOM §replaceWith).
            (if (eq (h:dnode-parent node) parent)
                (progn (adjust-ranges-for-removal ctx node)
                       (when ins (insert-into parent ins node))
                       (h:dom-remove node))
                (when ins (insert-into parent ins ref)))
            (setf (context-dirty ctx) t))))
      js:*undefined*)))

(defun copy-dnode (node deep)
  "A structural copy of NODE (deep when DEEP) without the per-context namespace
   metadata — used by Range extract/clone where ctx is not threaded."
  (let ((c (ecase (h:dnode-kind node)
             (:element (h:make-element (h:dnode-name node)
                                       (copy-alist (h:dnode-attrs node))
                                       (h:dnode-namespace node)))
             (:text (h:make-text (h:dnode-data node)))
             (:comment (h:make-comment (h:dnode-data node)))
             (:cdata (h:make-cdata (h:dnode-data node)))
             (:processing-instruction
              (h:make-processing-instruction (h:dnode-name node) (h:dnode-data node)))
             (:doctype (h:make-doctype (h:dnode-name node) (h:dnode-public node)
                                       (h:dnode-system node)))
             (:fragment (h:make-fragment))
             (:document (h:make-document)))))
    (when (and deep (not (char-data-p node)))
      (loop for ch across (h:dnode-children node) do (h:dom-append c (copy-dnode ch t))))
    c))

(defun clone-dnode (ctx node deep)
  "DOM §\"clone a node\": a copy of NODE (deep when DEEP), preserving the
   createElementNS namespace metadata, per-attribute namespace records, and a
   document's content type / about:blank flag."
  (let ((c (ecase (h:dnode-kind node)
             (:element (h:make-element (h:dnode-name node)
                                       (copy-alist (h:dnode-attrs node))
                                       (h:dnode-namespace node)))
             (:text (h:make-text (h:dnode-data node)))
             (:comment (h:make-comment (h:dnode-data node)))
             (:cdata (h:make-cdata (h:dnode-data node)))
             (:processing-instruction
              (h:make-processing-instruction (h:dnode-name node) (h:dnode-data node)))
             (:doctype (h:make-doctype (h:dnode-name node) (h:dnode-public node)
                                       (h:dnode-system node)))
             (:fragment (h:make-fragment))
             (:document (h:make-document)))))
    (when (eq (h:dnode-kind node) :element)
      (let ((info (gethash node (context-ns-info ctx))))
        (when info (setf (gethash c (context-ns-info ctx)) (copy-list info))))
      ;; Carry each attribute's namespace record onto the clone's fresh cons cell.
      (loop for orig in (h:dnode-attrs node)
            for clone in (h:dnode-attrs c)
            for rec = (gethash orig (context-attr-recs ctx))
            when rec do (setf (gethash clone (context-attr-recs ctx))
                              (make-attr-rec :cell clone :ns (attr-rec-ns rec)
                                             :prefix (attr-rec-prefix rec)
                                             :local (attr-rec-local rec) :owner c))))
    (when (eq (h:dnode-kind node) :document)
      (let ((ct (gethash node (context-doc-content-types ctx))))
        (when ct (setf (gethash c (context-doc-content-types ctx)) ct)))
      (when (gethash node (context-blank-url-docs ctx))
        (setf (gethash c (context-blank-url-docs ctx)) t))
      (setf (h:dnode-mode c) (h:dnode-mode node)))
    (when (and deep (not (char-data-p node)))
      (loop for ch across (h:dnode-children node)
            do (h:dom-append c (clone-dnode ctx ch t))))
    c))

(defun attrs-equal-p (ctx a b)
  "DOM §isEqualNode: attribute lists match iff same count and every attribute in
A has one in B with equal namespace, local name, and value (prefix ignored)."
  (and (= (length a) (length b))
       (every (lambda (cell)
                (multiple-value-bind (ns local) (cell-ns/local ctx cell)
                  (loop for other in b
                        thereis (multiple-value-bind (ons olocal) (cell-ns/local ctx other)
                                  (and (equal ns ons) (equal local olocal)
                                       (equal (cdr cell) (cdr other)))))))
              a)))

(defun nodes-equal-p (ctx a b)
  "DOM Node.isEqualNode: same type, type-specific data, attributes (as a set),
and pairwise-equal children."
  (and a b
       (eq (h:dnode-kind a) (h:dnode-kind b))
       (case (h:dnode-kind a)
         (:doctype (and (equal (h:dnode-name a) (h:dnode-name b))
                        (equal (h:dnode-public a) (h:dnode-public b))
                        (equal (h:dnode-system a) (h:dnode-system b))))
         (:element
          ;; Compare on real namespace, prefix, and local name (DOM §isEqualNode) —
          ;; element-real-ns folds the default HTML namespace in so an ns-info-less
          ;; HTML element equals a createDocument'd xhtml one.
          (and (equal (element-real-ns ctx a) (element-real-ns ctx b))
               (equal (element-prefix ctx a) (element-prefix ctx b))
               (equal (element-local ctx a) (element-local ctx b))
               (attrs-equal-p ctx (h:dnode-attrs a) (h:dnode-attrs b))))
         ((:text :cdata :comment) (equal (h:dnode-data a) (h:dnode-data b)))
         (:processing-instruction (and (equal (h:dnode-name a) (h:dnode-name b))
                                       (equal (h:dnode-data a) (h:dnode-data b))))
         (t t))
       (= (length (h:dnode-children a)) (length (h:dnode-children b)))
       (loop for ca across (h:dnode-children a)
             for cb across (h:dnode-children b)
             always (nodes-equal-p ctx ca cb))))

(defun node-root (n) (loop for p = n then (h:dnode-parent p)
                           when (null (h:dnode-parent p)) return p))
(defun proper-ancestor-p (a b) "T if A is a proper ancestor of B."
  (loop for p = (h:dnode-parent b) then (h:dnode-parent p)
        while p thereis (eq p a)))
(defun child-index-in (parent node)
  (position node (h:dnode-children parent)))
(defun tree-order-precedes-p (a b)
  "T if A precedes B in tree order (both in the same tree, neither an ancestor
of the other)."
  (let ((pa (nreverse (loop for p = a then (h:dnode-parent p) while p collect p)))
        (pb (nreverse (loop for p = b then (h:dnode-parent p) while p collect p))))
    ;; pa/pb are root..node; find the first divergence and compare child order.
    (loop for ta on pa for tb on pb
          for na = (car ta) for nb = (car tb)
          when (not (eq na nb))
            do (return (< (child-index-in (h:dnode-parent na) na)
                          (child-index-in (h:dnode-parent nb) nb)))
          finally (return nil))))
(defun compare-document-position (this other)
  "The bitmask for OTHER's position relative to THIS (DOM §Node)."
  (if (eq this other) 0
      (if (not (eq (node-root this) (node-root other)))
          (logior 1 32 (if (< (sxhash other) (sxhash this)) 2 4)) ; DISCONNECTED
          (cond ((proper-ancestor-p other this) (logior 8 2))  ; CONTAINS | PRECEDING
                ((proper-ancestor-p this other) (logior 16 4)) ; CONTAINED_BY | FOLLOWING
                ((tree-order-precedes-p other this) 2)         ; PRECEDING
                (t 4)))))                                      ; FOLLOWING

;;; ---- selector queries (querySelector / querySelectorAll) ------------------
(defun parse-selector-or-throw (ctx str)
  "Parse STR as a selector list for the JS Selectors API.  An invalid selector
must throw a SyntaxError DOMException (DOM §Element.matches / ParentNode query),
whereas the CSS cascade silently drops invalid rules — so this uses the strict
CSS:SELECTOR-LIST-VALID-P validator, distinct from the lenient cascade path."
  (unless (css:selector-list-valid-p str)
    (throw-dom ctx "SyntaxError" 12 (format nil "'~a' is not a valid selector" str)))
  (css:parse-selector-list str))

(defun qs-first (root selector-list)
  "First descendant element of ROOT (in tree order, excluding ROOT) matching
   SELECTOR-LIST; NIL if none.  Combinators are evaluated against the live tree."
  (labels ((walk (n)
             (loop for c across (h:dnode-children n)
                   when (eq (h:dnode-kind c) :element) do
                     (when (css:selector-matches-p selector-list c) (return-from qs-first c))
                     (walk c))))
    (walk root) nil))

(defun qs-all (root selector-list)
  "All descendant elements of ROOT (tree order, excluding ROOT) matching SELECTOR-LIST."
  (let ((out '()))
    (labels ((walk (n)
               (loop for c across (h:dnode-children n)
                     when (eq (h:dnode-kind c) :element) do
                       (when (css:selector-matches-p selector-list c) (push c out))
                       (walk c))))
      (walk root))
    (nreverse out)))

;;; ---- DOMTokenList (classList) ---------------------------------------------
(defun ascii-ws-p (c) (member c '(#\Space #\Tab #\Newline #\Return #\Page)))

(defun split-tokens (string)
  "Split STRING on ASCII whitespace into a list of non-empty tokens (order kept)."
  (when string
    (loop with n = (length string) with i = 0 with out = '()
          while (< i n)
          do (loop while (and (< i n) (ascii-ws-p (char string i))) do (incf i))
             (let ((start i))
               (loop while (and (< i n) (not (ascii-ws-p (char string i)))) do (incf i))
               (when (> i start) (push (subseq string start i) out)))
          finally (return (nreverse out)))))

(defun ordered-set (tokens)
  "The ordered set: TOKENS deduped, first occurrence kept (DOM §ordered sets)."
  (let ((seen (make-hash-table :test 'equal)) (out '()))
    (dolist (tk tokens (nreverse out))
      (unless (gethash tk seen) (setf (gethash tk seen) t) (push tk out)))))

(defun make-class-list (ctx node)
  "A live DOMTokenList over NODE's class attribute (DOM §DOMTokenList): reads and
   writes the attribute on every operation so it always reflects the current value.
   Integer indexing is served through a host-object [[Get]] trap."
  (let* ((realm (context-realm ctx))
         (methods (make-hash-table :test 'equal))
         tl)
    (labels ((toks () (ordered-set (split-tokens (get-attr node "class"))))
             (store (list)
               ;; DOM §DOMTokenList update steps: an absent attribute with an empty
               ;; token set stays absent (do not materialize class="").
               (if (and (null list) (not (dom:has-attribute node "class")))
                   nil
                   (progn (set-attr node "class" (format nil "~{~a~^ ~}" list))
                          (setf (context-dirty ctx) t))))
             (empty-check (tk) (when (zerop (length tk))
                                 (throw-dom ctx "SyntaxError" 12 "empty token")) tk)
             (ws-check (tk) (when (some #'ascii-ws-p tk)
                              (throw-dom ctx "InvalidCharacterError" 5 "token contains whitespace")) tk)
             (check (tk) (empty-check tk) (ws-check tk) tk)
             (meth (name arity fn)
               (setf (gethash name methods) (js:native-function realm name fn arity))))
      (meth "item" 1 (lambda (this a) (declare (ignore this))
                       (let* ((i (int-arg a 0)) (ts (toks)))
                         (if (and (>= i 0) (< i (length ts))) (nth i ts) js:*null*))))
      (meth "contains" 1 (lambda (this a) (declare (ignore this))
                           (jbool (member (jstr (arg a 0)) (toks) :test #'string=))))
      (meth "add" 1 (lambda (this a) (declare (ignore this))
                      (let ((ts (toks)))
                        (dolist (x a) (let ((tk (check (jstr x))))
                                        (unless (member tk ts :test #'string=)
                                          (setf ts (append ts (list tk))))))
                        (store ts) js:*undefined*)))
      (meth "remove" 1 (lambda (this a) (declare (ignore this))
                         (let ((ts (toks)))
                           (dolist (x a) (setf ts (remove (check (jstr x)) ts :test #'string=)))
                           (store ts) js:*undefined*)))
      (meth "toggle" 2 (lambda (this a) (declare (ignore this))
                         (let* ((tk (check (jstr (arg a 0)))) (ts (toks))
                                (present (member tk ts :test #'string=))
                                (force-given (> (length a) 1)) (force (js:js-truthy (arg a 1))))
                           (cond ((and present (or (not force-given) (not force)))
                                  (store (remove tk ts :test #'string=)) js:*false*)
                                 ((and (not present) (or (not force-given) force))
                                  (store (append ts (list tk))) js:*true*)
                                 (t (jbool present))))))
      (meth "replace" 2 (lambda (this a) (declare (ignore this))
                          ;; Empty checks (SyntaxError) precede whitespace checks
                          ;; (InvalidCharacterError) for BOTH tokens (DOM §replace).
                          (let ((old (jstr (arg a 0))) (new (jstr (arg a 1))))
                            (empty-check old) (empty-check new) (ws-check old) (ws-check new)
                            (let ((ts (toks)))
                              (if (member old ts :test #'string=)
                                  (progn (store (ordered-set (substitute new old ts :test #'string=)))
                                         js:*true*)
                                  js:*false*)))))
      ;; classList has no defined set of supported tokens, so .supports() must
      ;; throw TypeError (DOM §) — leaving it unimplemented gives exactly that.
      (meth "toString" 0 (lambda (this a) (declare (ignore this a)) (or (get-attr node "class") "")))
      (setf tl (js:make-host-object realm
        :proto (js:eval-script realm "Object.prototype")
        :get (lambda (o key rcv) (declare (ignore rcv))
               (let ((key (js:to-property-key key)))
                 (cond
                   ((and (stringp key) (string= key "length")) (num (length (toks))))
                   ((and (stringp key) (string= key "value")) (or (get-attr node "class") ""))
                   ((and (stringp key) (gethash key methods)) (gethash key methods))
                   ((index-string-p key)
                    (let ((i (parse-integer key)) (ts (toks)))
                      (if (< i (length ts)) (nth i ts) js:*undefined*)))
                   ;; Own props (Symbol.toStringTag + the installed iterator
                   ;; methods) then the prototype chain.
                   (t (js::ordinary-get o key o)))))
        :set (lambda (o key val rcv)
               (let ((k (js:to-property-key key)))
                 (if (and (stringp k) (string= k "value"))
                     (progn (set-attr node "class" (jstr val)) (setf (context-dirty ctx) t) t)
                     ;; Other writes (Symbol.iterator/keys/... from the installer)
                     ;; take the ordinary path so they land as own properties.
                     (js::ordinary-set o key val (or rcv o)))))
        :has (lambda (o key)
               (let ((k (js:to-property-key key)))
                 (or (and (stringp k)
                          (or (string= k "length") (string= k "value") (gethash k methods) nil))
                     (and (index-string-p k) (< (parse-integer k) (length (toks))))
                     (js::ordinary-has o key))))))
      ;; Per WebIDL a DOMTokenList's iterator surface is Array.prototype's own
      ;; functions (the tests assert `classList.keys === Array.prototype.keys`);
      ;; they are generic over the live length/index [[Get]] traps.
      (let ((installer (js:eval-script realm "(function(tl){
        tl[Symbol.toStringTag]='DOMTokenList';
        tl[Symbol.iterator]=Array.prototype[Symbol.iterator];
        tl.keys=Array.prototype.keys;
        tl.values=Array.prototype.values;
        tl.entries=Array.prototype.entries;
        tl.forEach=Array.prototype.forEach;
      })")))
        (js:js-call installer js:*undefined* (list tl)))
      tl)))

;;; ---- attributes -----------------------------------------------------------
(defun attr-name (name) (string-downcase (jstr name)))
(defun get-attr (node name) (dom:get-attribute node (attr-name name)))
(defun set-attr (node name value)
  (let* ((n (attr-name name)) (v (jstr value))
         (cell (assoc n (h:dnode-attrs node) :test #'string=)))
    (if cell (setf (cdr cell) v)
        (setf (h:dnode-attrs node)
              (append (h:dnode-attrs node) (list (cons n v)))))))
(defun remove-attr (node name)
  ;; removeAttribute removes the FIRST attribute whose qualified name matches
  ;; (DOM §remove-an-attribute-by-name); :count 1 keeps any same-qname twins.
  (setf (h:dnode-attrs node)
        (remove (attr-name name) (h:dnode-attrs node) :key #'car :test #'string= :count 1)))

;;; ---- namespaced attributes + Attr / NamedNodeMap (DOM §4.9/§Attr) ----------
;;; An attribute's value + qualified name live in a (qname . value) cons inside the
;;; owner element's H:DNODE-ATTRS list (the store the parser/CSS/render already
;;; read).  Namespace metadata (and standalone/owner-less Attr identity) hangs off
;;; that cons in the per-context ATTR-RECS side table — mirroring how CONTEXT-NS-INFO
;;; records createElementNS namespaces.  Duplicate qualified names (setAttributeNS
;;; with differing namespaces) are distinct conses, so nothing collides.
(defstruct attr-rec
  cell                                  ; the (qname . value) cons (in the owner's list, or free)
  (ns nil)                              ; namespaceURI, or NIL
  (prefix nil)                          ; prefix, or NIL
  local                                 ; localName
  (owner nil))                          ; owning element dnode, or NIL when detached

(defun ns-arg (v) "JS null/undefined/empty-string namespace -> NIL."
  (if (or (nullish v) (and (stringp v) (string= v ""))) nil (jstr v)))

(defun attr-rec-of (ctx cell &optional owner)
  "The ATTR-REC for attribute CELL, lazily synthesizing a null-namespace record
   (localName = qualified name) for a plain attribute first reflected as an Attr."
  (let ((rec (or (gethash cell (context-attr-recs ctx))
                 (setf (gethash cell (context-attr-recs ctx))
                       (make-attr-rec :cell cell :local (car cell) :owner owner)))))
    (when (and owner (null (attr-rec-owner rec))) (setf (attr-rec-owner rec) owner))
    rec))

(defun cell-ns/local (ctx cell)
  "(values namespace localName) for attribute CELL (defaults: null ns, qname)."
  (let ((rec (gethash cell (context-attr-recs ctx))))
    (if rec (values (attr-rec-ns rec) (attr-rec-local rec))
        (values nil (car cell)))))

(defun find-attr-ns (ctx el ns local)
  "First attribute cons of EL matching NS (may be NIL) and localName LOCAL."
  (loop for cell in (h:dnode-attrs el)
        do (multiple-value-bind (cns cl) (cell-ns/local ctx cell)
             (when (and (equal cns ns) (equal cl local)) (return cell)))))

(defun find-attr-qname (el qname)
  "First attribute cons of EL whose qualified name is QNAME (case-sensitive)."
  (assoc qname (h:dnode-attrs el) :test #'string=))

(defun html-doc-p (ctx doc)
  "T if DOC is an HTML document (contentType text/html) — governs createAttribute
   ASCII-lowercasing (DOM §)."
  (equal (or (gethash doc (context-doc-content-types ctx)) "text/html") "text/html"))

(defun node-document (ctx node)
  "The node document of NODE — the :document root of its tree if it is in one
   (adoption follows tree position), else its recorded owner document."
  (let ((root (loop for a = node then (h:dnode-parent a)
                    when (null (h:dnode-parent a)) return a)))
    (if (eq (h:dnode-kind root) :document) root
        (or (gethash node (context-owner-docs ctx)) (context-document ctx)))))

(defun element-real-ns (ctx el)
  "EL's real namespace URI (from createElementNS ns-info, else the coarse kind)."
  (let ((info (gethash el (context-ns-info ctx))))
    (cond (info (getf info :ns))
          ((eq (h:dnode-namespace el) :html) *html-ns*)
          ((eq (h:dnode-namespace el) :svg) "http://www.w3.org/2000/svg")
          (t nil))))

(defun element-wrapper-proto (ctx el)
  "The prototype for EL's JS wrapper (DOM §): the per-tag HTML interface for an
   HTML-namespace element, SVGElement for SVG, else the generic Element."
  (let ((ns (element-real-ns ctx el)))
    (cond ((equal ns "http://www.w3.org/2000/svg") (proto ctx :svg-element))
          ((equal ns *html-ns*)
           (let ((tbl (proto ctx :html-tag-protos)))
             ;; Key on the local name (createElementNS "foo:div" -> "div").
             (or (and tbl (gethash (element-local ctx el) tbl))
                 (proto ctx :html-unknown))))
          (t (proto ctx :element)))))

(defun element-prefix (ctx el)
  "EL's namespace prefix, or NIL."
  (let ((info (gethash el (context-ns-info ctx))))
    (and info (getf info :prefix))))

(defun element-local (ctx el)
  "EL's local name (from createElementNS ns-info, else the stored name)."
  (let ((info (gethash el (context-ns-info ctx))))
    (if info (getf info :local) (h:dnode-name el))))

(defun ns-arg (v)
  "Coerce a namespace/prefix IDL argument: null/undefined and \"\" both -> NIL."
  (if (nullish v) nil (let ((s (jstr v))) (if (zerop (length s)) nil s))))

(defun document-element-of (doc)
  "The document element (first element child) of a :document node, or NIL."
  (loop for ch across (h:dnode-children doc)
        when (eq (h:dnode-kind ch) :element) return ch))

;;; DOM §"locate a namespace" — recursive walk up the element ancestors honoring
;;; xmlns / xmlns:prefix declarations.  Prefix NIL means the default namespace.
(defun element-locate-namespace (ctx el prefix)
  ;; The "xml" and "xmlns" prefixes are predefined (XML Namespaces §): every
  ;; element implicitly binds them, so they short-circuit the ancestor walk.
  (when (equal prefix "xml") (return-from element-locate-namespace
                               "http://www.w3.org/XML/1998/namespace"))
  (when (equal prefix "xmlns") (return-from element-locate-namespace
                                 "http://www.w3.org/2000/xmlns/"))
  (let ((ns (element-real-ns ctx el)))
    (cond
      ((and ns (equal prefix (element-prefix ctx el))) ns)
      (t (let* ((target (if prefix (format nil "xmlns:~a" prefix) "xmlns"))
                (cell (assoc target (h:dnode-attrs el) :test #'string=)))
           (if cell
               (if (plusp (length (cdr cell))) (cdr cell) nil)
               (let ((p (h:dnode-parent el)))
                 (when (and p (eq (h:dnode-kind p) :element))
                   (element-locate-namespace ctx p prefix)))))))))

(defun locate-namespace (ctx node prefix)
  (case (h:dnode-kind node)
    (:element (element-locate-namespace ctx node prefix))
    (:document (let ((de (document-element-of node)))
                 (and de (element-locate-namespace ctx de prefix))))
    ((:doctype :fragment) nil)
    (t (let ((p (h:dnode-parent node)))
         (when (and p (eq (h:dnode-kind p) :element))
           (element-locate-namespace ctx p prefix))))))

;;; DOM §"locate a namespace prefix".
(defun element-locate-prefix (ctx el ns)
  (cond
    ((and (equal (element-real-ns ctx el) ns) (element-prefix ctx el))
     (element-prefix ctx el))
    (t (or (loop for (q . v) in (h:dnode-attrs el)
                 when (and (> (length q) 6) (string= (subseq q 0 6) "xmlns:")
                           (equal v ns))
                 return (subseq q 6))
           (let ((p (h:dnode-parent el)))
             (when (and p (eq (h:dnode-kind p) :element))
               (element-locate-prefix ctx p ns)))))))

(defun tag-ns-match (ctx el nsraw local)
  "Match predicate for getElementsByTagNameNS (DOM §): NSRAW/LOCAL are the raw
   IDL arguments; \"*\" is a wildcard for either component."
  (let ((ns (if (string= nsraw "*") :any (ns-arg nsraw))))
    (and (or (string= local "*") (string= (element-local ctx el) local))
         (or (eq ns :any) (equal (element-real-ns ctx el) ns)))))

(defun context-element-for-ns (ctx node)
  "The element used to resolve namespace queries for NODE (DOM §)."
  (case (h:dnode-kind node)
    (:element node)
    (:document (document-element-of node))
    ((:doctype :fragment) nil)
    (t (let ((p (h:dnode-parent node)))
         (and p (eq (h:dnode-kind p) :element) p)))))

(defun html-attr-el-p (ctx el)
  "T if EL is an HTML-namespace element in an HTML document — the case where
   setAttribute/getAttribute/hasAttribute ASCII-lowercase the qualified name and
   the NamedNodeMap omits mixed-case supported property names (DOM §)."
  (and (equal (element-real-ns ctx el) *html-ns*)
       (html-doc-p ctx (node-document ctx el))))

(defun adjust-qname (ctx el name)
  "The lookup/store qualified name: ASCII-lowercased for an HTML element, else
   verbatim (XML/SVG attribute names are case-sensitive)."
  (let ((name (jstr name)))
    (if (html-attr-el-p ctx el) (string-downcase name) name)))

(defun some-upper-ascii-p (s) (some (lambda (c) (char<= #\A c #\Z)) s))

(defun supported-attr-names (ctx el)
  "NamedNodeMap supported property names (DOM §): the qualified names in order,
   de-duplicated, with mixed-case names omitted for an HTML element."
  (let ((html (html-attr-el-p ctx el)) (seen '()) (out '()))
    (dolist (c (h:dnode-attrs el) (nreverse out))
      (let ((q (car c)))
        (unless (or (member q seen :test #'string=) (and html (some-upper-ascii-p q)))
          (push q seen) (push q out))))))

(defun validate-extract (ctx ns qname)
  "DOM 'validate and extract' (§validate-and-extract): (values namespace prefix
   localName) for QNAME in NS.  A qualified name that does not match the QName
   production throws InvalidCharacterError; a namespace-well-formedness violation
   throws NamespaceError."
  (let* ((ns (ns-arg ns)) (qname (jstr qname))
         (colon (position #\: qname)))
    ;; QName production (structural): non-empty, no leading/trailing colon, one colon.
    (when (or (zerop (length qname))
              (and colon (or (zerop colon) (= colon (1- (length qname)))
                             (position #\: qname :start (1+ colon)))))
      (throw-dom ctx "InvalidCharacterError" 5 "not a valid qualified name"))
    (let ((prefix (and colon (subseq qname 0 colon)))
          (local (if colon (subseq qname (1+ colon)) qname)))
      (when (or (and prefix (null ns))
                (and (equal prefix "xml") (not (equal ns "http://www.w3.org/XML/1998/namespace")))
                (and (or (equal qname "xmlns") (equal prefix "xmlns"))
                     (not (equal ns "http://www.w3.org/2000/xmlns/")))
                (and (equal ns "http://www.w3.org/2000/xmlns/")
                     (not (or (equal qname "xmlns") (equal prefix "xmlns")))))
        (throw-dom ctx "NamespaceError" 14 "namespace well-formedness violation"))
      (values ns prefix local))))

(defun set-attr-ns (ctx el ns qname value)
  "The setAttributeNS core: validate+extract, then set the (ns,local) attribute's
   value — updating in place or appending a new cons (DOM §setAttributeNS)."
  (multiple-value-bind (ns prefix local) (validate-extract ctx ns qname)
    (let ((cell (find-attr-ns ctx el ns local)))
      (if cell
          (setf (cdr cell) (jstr value))
          (let ((c (cons (jstr qname) (jstr value))))
            (setf (h:dnode-attrs el) (append (h:dnode-attrs el) (list c)))
            (setf (gethash c (context-attr-recs ctx))
                  (make-attr-rec :cell c :ns ns :prefix prefix :local local :owner el))))
      (setf (context-dirty ctx) t))))

(defun detach-attr-cell (ctx el cell)
  "Remove attribute CELL from EL, clearing its Attr wrapper's owner."
  (setf (h:dnode-attrs el) (remove cell (h:dnode-attrs el) :test #'eq))
  (let ((rec (gethash cell (context-attr-recs ctx))))
    (when rec (setf (attr-rec-owner rec) nil)))
  (setf (context-dirty ctx) t))

;;; ---- Attr wrapper + prototype ---------------------------------------------
(defun wrap-attr (ctx cell &optional owner)
  "The JS Attr wrapper for attribute CELL, memoized on the cons (Attr identity)."
  (let ((rec (attr-rec-of ctx cell owner)))
    (or (gethash cell (context-attr-objs ctx))
        (let ((o (js:make-object :proto (proto ctx :attr))))
          (setf (gethash cell (context-attr-objs ctx)) o
                (gethash o (context-attr-of ctx)) rec)
          o))))

(defun attr-rec-arg (ctx this)
  (or (gethash this (context-attr-of ctx))
      (js:js-throw (js:make-native-error "TypeError" "not an Attr"))))

(defun install-attr-proto (ctx ap)
  (macrolet ((rec (this) `(attr-rec-arg ctx ,this)))
    (defget ctx ap "nodeType" (this) (declare (ignore this)) (num 2))
    (flet ((qname (r) (car (attr-rec-cell r))))
      (defget ctx ap "name" (this) (qname (rec this)))
      (defget ctx ap "nodeName" (this) (qname (rec this))))
    (defget ctx ap "localName" (this) (attr-rec-local (rec this)))
    (defget ctx ap "prefix" (this) (opt (attr-rec-prefix (rec this))))
    (defget ctx ap "namespaceURI" (this) (opt (attr-rec-ns (rec this))))
    (defget ctx ap "specified" (this) (declare (ignore this)) js:*true*)
    (defget ctx ap "ownerElement" (this) (wrap ctx (attr-rec-owner (rec this))))
    (defget ctx ap "ownerDocument" (this)
      (let ((o (attr-rec-owner (rec this))))
        (wrap ctx (and o (or (gethash o (context-owner-docs ctx)) (context-document ctx))))))
    (flet ((getv (this) (cdr (attr-rec-cell (rec this))))
           (setv (this v) (let ((r (rec this)))
                            (setf (cdr (attr-rec-cell r)) (jstr v))
                            (when (attr-rec-owner r) (setf (context-dirty ctx) t)))))
      (defgetset ctx ap "value" (this) (getv this) (v) (setv this v))
      (defgetset ctx ap "nodeValue" (this) (getv this) (v) (setv this v))
      (defgetset ctx ap "textContent" (this) (getv this) (v) (setv this v)))
    ;; DOM §Node namespace lookups resolve against the Attr's owner element.
    (defmethod* ctx ap "lookupNamespaceURI" 1 (this a)
      (let ((o (attr-rec-owner (rec this))))
        (opt (and o (element-locate-namespace ctx o (ns-arg (arg a 0)))))))
    (defmethod* ctx ap "isDefaultNamespace" 1 (this a)
      (let ((o (attr-rec-owner (rec this))))
        (jbool (equal (and o (element-locate-namespace ctx o nil)) (ns-arg (arg a 0))))))
    (defmethod* ctx ap "lookupPrefix" 1 (this a)
      (let ((o (attr-rec-owner (rec this))) (ns (ns-arg (arg a 0))))
        (opt (and o ns (element-locate-prefix ctx o ns)))))
    (defmethod* ctx ap "getRootNode" 1 (this a) (declare (ignore a))
      ;; An Attr has no parent, so it is its own root (DOM §).
      this)
    ;; Cloning an Attr yields a detached Attr with the same namespace/name/value.
    (defmethod* ctx ap "cloneNode" 1 (this a) (declare (ignore a))
      (let* ((r (rec this)) (cell (attr-rec-cell r)) (nc (cons (car cell) (cdr cell))))
        (setf (gethash nc (context-attr-recs ctx))
              (make-attr-rec :cell nc :ns (attr-rec-ns r) :prefix (attr-rec-prefix r)
                             :local (attr-rec-local r) :owner nil))
        (wrap-attr ctx nc)))
    ;; DOM §Node.isSameNode/isEqualNode for Attr (identity / ns+local+value).
    (defmethod* ctx ap "isSameNode" 1 (this a) (jbool (eq this (arg a 0))))
    (defmethod* ctx ap "isEqualNode" 1 (this a)
      (let* ((r1 (rec this)) (o (arg a 0))
             (r2 (and (js:js-object-p o) (gethash o (context-attr-of ctx)))))
        (jbool (and r2 (equal (attr-rec-ns r1) (attr-rec-ns r2))
                    (equal (attr-rec-local r1) (attr-rec-local r2))
                    (equal (cdr (attr-rec-cell r1)) (cdr (attr-rec-cell r2)))))))))

;;; ---- NamedNodeMap (element.attributes) ------------------------------------
(defun make-attr-map (ctx el)
  "A live NamedNodeMap over EL's attributes (DOM §NamedNodeMap): indexed Attr
   access, .length, and named access to an Attr by qualified name (shadowed by any
   prototype member — item/getNamedItem/… live on NamedNodeMap.prototype so
   `map.item === NamedNodeMap.prototype.item`).  The map -> element link rides
   CONTEXT-OBJ-NODES so the shared prototype methods recover EL from `this`."
  (let ((m (js:make-host-object (context-realm ctx)
             :proto (proto ctx :namednodemap)
             :get (lambda (o key rcv) (declare (ignore rcv))
                    (let ((key (js:to-property-key key)))
                      (cond
                        ((and (stringp key) (string= key "length"))
                         (num (length (h:dnode-attrs el))))
                        ;; any member on the prototype chain shadows named access
                        ((and (stringp key) (js:js-has (js:js-object-proto o) key))
                         (js:js-get (js:js-object-proto o) key o))
                        ((index-string-p key)
                         (let ((l (h:dnode-attrs el)) (i (parse-integer key)))
                           (if (< i (length l)) (wrap-attr ctx (nth i l) el) js:*undefined*)))
                        ((and (stringp key) (find-attr-qname el key))
                         (wrap-attr ctx (find-attr-qname el key) el))
                        (t (js:js-get (js:js-object-proto o) key o)))))
             :has (lambda (o key)
                    (let ((key (js:to-property-key key)))
                      (or (and (stringp key) (string= key "length"))
                          (and (index-string-p key) (< (parse-integer key) (length (h:dnode-attrs el))))
                          (and (stringp key) (find-attr-qname el key) t)
                          (js:js-has (js:js-object-proto o) key))))
             ;; own keys: the indices, then the supported property names (DOM §)
             :own-keys (lambda (o) (declare (ignore o))
                         (append (loop for i from 0 below (length (h:dnode-attrs el))
                                       collect (princ-to-string i))
                                 (supported-attr-names ctx el))))))
    ;; [[GetOwnProperty]]: indices are enumerable data props; supported named
    ;; properties are non-enumerable (NamedNodeMap has [LegacyUnenumerableNamedProperties]).
    (setf (getf (js::js-object-internal m) :get-own-property)
          (lambda (o key) (declare (ignore o))
            (let ((l (h:dnode-attrs el)))
              (cond
                ((and (index-string-p key) (< (parse-integer key) (length l)))
                 (js::make-prop :value (wrap-attr ctx (nth (parse-integer key) l) el)
                                :enumerable t :configurable t :writable nil))
                ((and (stringp key) (member key (supported-attr-names ctx el) :test #'string=))
                 (js::make-prop :value (wrap-attr ctx (find-attr-qname el key) el)
                                :enumerable nil :configurable t :writable nil))
                (t nil)))))
    (setf (gethash m (context-obj-nodes ctx)) el)   ; map -> element, for the prototype methods
    m))

(defun install-namednodemap-proto (ctx np)
  (macrolet ((el (this) `(require-node ctx ,this)))
    (defmethod* ctx np "item" 1 (this a)
      (let ((l (h:dnode-attrs (el this))) (i (int-arg a 0)))
        (if (< -1 i (length l)) (wrap-attr ctx (nth i l) (el this)) js:*null*)))
    (defmethod* ctx np "getNamedItem" 1 (this a)
      (let ((c (find-attr-qname (el this) (jstr (arg a 0)))))
        (if c (wrap-attr ctx c (el this)) js:*null*)))
    (defmethod* ctx np "getNamedItemNS" 2 (this a)
      (let ((c (find-attr-ns ctx (el this) (ns-arg (arg a 0)) (jstr (arg a 1)))))
        (if c (wrap-attr ctx c (el this)) js:*null*)))
    (defmethod* ctx np "setNamedItem" 1 (this a) (attr-map-set ctx (el this) (arg a 0)))
    (defmethod* ctx np "setNamedItemNS" 1 (this a) (attr-map-set ctx (el this) (arg a 0)))
    (defmethod* ctx np "removeNamedItem" 1 (this a)
      (let* ((e (el this)) (c (find-attr-qname e (jstr (arg a 0)))))
        (unless c (throw-dom ctx "NotFoundError" 8 "no such attribute"))
        (let ((w (wrap-attr ctx c e))) (detach-attr-cell ctx e c) w)))
    (defmethod* ctx np "removeNamedItemNS" 2 (this a)
      (let* ((e (el this)) (c (find-attr-ns ctx e (ns-arg (arg a 0)) (jstr (arg a 1)))))
        (unless c (throw-dom ctx "NotFoundError" 8 "no such attribute"))
        (let ((w (wrap-attr ctx c e))) (detach-attr-cell ctx e c) w)))))

(defun attr-map-set (ctx el attr-obj)
  "NamedNodeMap.setNamedItem / Element.setAttributeNode: attach ATTR-OBJ to EL,
   replacing a same-(namespace,localName) attribute and returning the old Attr
   (or null)."
  (let ((rec (gethash attr-obj (context-attr-of ctx))))
    (unless rec (js:js-throw (js:make-native-error "TypeError" "not an Attr")))
    (when (and (attr-rec-owner rec) (not (eq (attr-rec-owner rec) el)))
      (throw-dom ctx "InUseAttributeError" 10 "attribute in use"))
    (let* ((cell (attr-rec-cell rec))
           (old (find-attr-ns ctx el (attr-rec-ns rec) (attr-rec-local rec))))
      (cond ((eq old cell) (wrap-attr ctx cell el))  ; already set — no-op
            (old ;; replace OLD in place (setAttributeNode keeps attribute order)
             (let ((w (wrap-attr ctx old el)) (tail (member old (h:dnode-attrs el) :test #'eq)))
               (setf (car tail) cell)                 ; splice CELL into OLD's slot
               (let ((r (gethash old (context-attr-recs ctx))))
                 (when r (setf (attr-rec-owner r) nil)))
               (setf (attr-rec-owner rec) el (context-dirty ctx) t)
               w))
            (t (setf (h:dnode-attrs el) (append (h:dnode-attrs el) (list cell)))
               (setf (attr-rec-owner rec) el (context-dirty ctx) t)
               js:*null*)))))

(defun make-standalone-attr (ctx ns qname)
  "document.createAttribute[NS]: a detached Attr (owner NIL)."
  (multiple-value-bind (ns prefix local)
      (if ns (validate-extract ctx ns qname)
          (progn (when (zerop (length (jstr qname)))
                   (throw-dom ctx "InvalidCharacterError" 5 "empty attribute name"))
                 (values nil nil (jstr qname))))
    (let ((c (cons (jstr qname) "")))
      (setf (gethash c (context-attr-recs ctx))
            (make-attr-rec :cell c :ns ns :prefix prefix :local local :owner nil))
      (wrap-attr ctx c))))

;;; ---- checkbox/radio checkedness -------------------------------------------
;;; Live checkedness is tracked in a reserved `weft-checked` attribute (so the
;;; CSS engine's :checked sees it) and hidden from the public attribute API.
(defun input-type (node) (string-downcase (or (get-attr node "type") "text")))
(defun checked-p (node)
  (let ((wc (get-attr node "weft-checked")))
    (if wc (string= wc "1") (dom:has-attribute node "checked"))))
(defun tree-root (node) (loop for a = node then (h:dnode-parent a)
                              when (null (h:dnode-parent a)) return a))
(defun set-checked (ctx node checked)
  (set-attr node "weft-checked" (if checked "1" "0"))
  (when (and checked (string= (input-type node) "radio"))
    (let ((name (dom:get-attribute node "name")))
      (dolist (other (dom:get-elements-by-tag-name (tree-root node) "input"))
        (when (and (not (eq other node)) (string= (input-type other) "radio")
                   (equal (dom:get-attribute other "name") name))
          (set-attr other "weft-checked" "0")))))
  (setf (context-dirty ctx) t))

;;; ---- live collections -----------------------------------------------------
(defun index-string-p (k)
  (and (stringp k) (plusp (length k)) (every #'digit-char-p k)))

(defun collection-named-node (ctx list name)
  "HTMLCollection namedItem (DOM §): the first element with id=NAME, else the first
   HTML-namespace element with name=NAME."
  (or (find-if (lambda (el) (equal (dom:get-attribute el "id") name)) list)
      (find-if (lambda (el) (and (equal (element-real-ns ctx el) *html-ns*)
                                 (equal (dom:get-attribute el "name") name)))
               list)))

(defun collection-supported-names (ctx list)
  "HTMLCollection supported property names (DOM §): each element's id, then (for
   HTML elements) its name, in tree order, de-duplicated."
  (let ((seen '()) (out '()))
    (flet ((add (v) (when (and v (plusp (length v)) (not (member v seen :test #'string=)))
                      (push v seen) (push v out))))
      (dolist (el list (nreverse out))
        (add (dom:get-attribute el "id"))
        (when (equal (element-real-ns ctx el) *html-ns*) (add (dom:get-attribute el "name")))))))

(defun make-collection (ctx list-fn &optional name-fn (kind :htmlcollection))
  "A live NodeList/HTMLCollection: length + integer indexing + item(), reading
   LIST-FN (-> a fresh CL list of weft nodes) on every access.  For an
   :htmlcollection, NAME-FN overrides namedItem semantics (form controls); when
   omitted the standard id/name lookup applies.  KIND selects the prototype and
   whether named access + supported property names are exposed."
  (let* ((realm (context-realm ctx))
         (html (eq kind :htmlcollection))
         (lookup (cond (name-fn (lambda (nm) (funcall name-fn nm)))
                       (html (lambda (nm) (collection-named-node ctx (funcall list-fn) nm)))
                       (t (constantly nil))))
         (names (if html (lambda () (collection-supported-names ctx (funcall list-fn)))
                    (constantly nil)))
         (item (js:native-function realm "item"
                 (lambda (this a) (declare (ignore this))
                   (let ((i (int-arg a 0)) (l (funcall list-fn)))
                     (if (< -1 i (length l)) (wrap ctx (nth i l)) js:*null*)))
                 1))
         (named (js:native-function realm "namedItem"
                  (lambda (this a) (declare (ignore this))
                    (let ((node (funcall lookup (jstr (arg a 0)))))
                      (if node (wrap ctx node) js:*null*)))
                  1))
         (m (js:make-host-object realm
      :proto (or (proto ctx kind) (js:eval-script realm "Object.prototype"))
      :get (lambda (o key rcv) (declare (ignore rcv))
             (let ((key (js:to-property-key key)))   ; obj[0] arrives as a number
               (cond
                 ((and (stringp key) (string= key "length"))
                  (num (length (funcall list-fn))))
                 ((and (stringp key) (string= key "item")) item)
                 ((and (stringp key) (string= key "namedItem")) named)
                 ((index-string-p key)
                  (let ((i (parse-integer key)) (l (funcall list-fn)))
                    (if (< i (length l)) (wrap ctx (nth i l)) js:*undefined*)))
                 ((and html (stringp key) (funcall lookup key))
                  (wrap ctx (funcall lookup key)))
                 (t (js:js-get (js:js-object-proto o) key o)))))
      ;; [[HasProperty]] so `"length" in coll`, `0 in coll` and named lookups
      ;; work (assert_array_equals probes `"length" in actual`).
      :has (lambda (o key)
             (let ((key (js:to-property-key key)))
               (or (and (stringp key)
                        (or (string= key "length") (string= key "item") (string= key "namedItem")))
                   (and (index-string-p key) (< (parse-integer key) (length (funcall list-fn))))
                   (and html (stringp key) (funcall lookup key) t)
                   (js:js-has (js:js-object-proto o) key))))
      ;; own keys: the integer indices then the supported property names.
      :own-keys (lambda (o) (declare (ignore o))
                  (append (loop for i from 0 below (length (funcall list-fn))
                                collect (princ-to-string i))
                          (funcall names))))))
    ;; [[GetOwnProperty]]: indices are enumerable; named properties are not
    ;; ([LegacyUnenumerableNamedProperties] on HTMLCollection).
    (setf (getf (js::js-object-internal m) :get-own-property)
          (lambda (o key) (declare (ignore o))
            (let ((l (funcall list-fn)))
              (cond
                ((and (index-string-p key) (< (parse-integer key) (length l)))
                 (js::make-prop :value (wrap ctx (nth (parse-integer key) l))
                                :enumerable t :configurable t :writable nil))
                ((and html (stringp key) (member key (funcall names) :test #'string=))
                 (js::make-prop :value (wrap ctx (funcall lookup key))
                                :enumerable nil :configurable t :writable nil))
                (t nil)))))
    m))

(defparameter +form-control-tags+
  '("input" "button" "select" "textarea" "fieldset" "object" "output"))

(defun form-controls (form)
  (remove-if-not (lambda (n) (member (h:dnode-name n) +form-control-tags+ :test #'string=))
                 (dom:get-elements-by-tag-name form "*")))

(defun named-control (form name)
  (find-if (lambda (n) (or (equal (dom:get-attribute n "name") name)
                           (equal (dom:get-attribute n "id") name)))
           (form-controls form)))

(defun children-list (node)
  (coerce (h:dnode-children node) 'list))
(defun element-children-list (node)
  (loop for c across (h:dnode-children node)
        when (eq (h:dnode-kind c) :element) collect c))

;;; ---------------------------------------------------------------------------
;;; Node.prototype
;;; ---------------------------------------------------------------------------
(defun install-node-proto (ctx np)
  (macrolet ((n (this) `(require-node ctx ,this)))
    (defget ctx np "nodeType" (this) (node-type-of (n this)))
    (defget ctx np "nodeName" (this) (node-name-of (n this) ctx))
    (defget ctx np "localName" (this) js:*null*)   ; Element overrides; null elsewhere
    (defget ctx np "parentNode" (this) (wrap ctx (h:dnode-parent (n this))))
    ;; isConnected: the node's shadow-including root is a document (DOM §Node).
    (defget ctx np "isConnected" (this)
      (jbool (eq (h:dnode-kind (tree-root (n this))) :document)))
    (defget ctx np "parentElement" (this)
      (let ((p (h:dnode-parent (n this))))
        (wrap ctx (and p (eq (h:dnode-kind p) :element) p))))
    (defget ctx np "ownerDocument" (this)
      ;; A node in a document tree reports that document (this is how being moved
      ;; into another document — "adoption" — takes effect); a detached/fragment
      ;; node falls back to the recorded owner document (DOM §Node ownerDocument).
      (let* ((node (n this))
             (root (loop for p = node then (h:dnode-parent p)
                         when (null (h:dnode-parent p)) return p)))
        (cond ((eq (h:dnode-kind node) :document) js:*null*)
              ((eq (h:dnode-kind root) :document) (wrap ctx root))
              ((gethash node (context-owner-docs ctx))
               (wrap ctx (gethash node (context-owner-docs ctx))))
              (t js:*null*))))
    (defget ctx np "firstChild" (this)
      (let ((ch (h:dnode-children (n this))))
        (if (plusp (length ch)) (wrap ctx (aref ch 0)) js:*null*)))
    (defget ctx np "lastChild" (this) (wrap ctx (h:dom-last-child (n this))))
    (defget ctx np "nextSibling" (this)
      (let* ((node (n this)) (p (h:dnode-parent node)))
        (if p (let* ((ch (h:dnode-children p)) (i (position node ch)))
                (if (and i (< (1+ i) (length ch))) (wrap ctx (aref ch (1+ i))) js:*null*))
            js:*null*)))
    (defget ctx np "previousSibling" (this)
      (let* ((node (n this)) (p (h:dnode-parent node)))
        (wrap ctx (and p (h:dom-prev-sibling p node)))))
    ;; childNodes is [SameObject] (DOM §Node): memoize the live NodeList on the
    ;; wrapper so `node.childNodes === node.childNodes`.
    (defget ctx np "childNodes" (this)
      (let ((existing (js:js-get this "__weft_childNodes")))
        (if (js:js-object-p existing) existing
            (let* ((node (n this))
                   (nl (make-collection ctx (lambda () (children-list node)) nil :nodelist)))
              (js:put this "__weft_childNodes" nl :enumerable nil :configurable t)
              nl))))
    (defgetset ctx np "nodeValue" (this)
      (if (char-data-p (n this)) (h:dnode-data (n this)) js:*null*)
      ;; DOM §nodeValue setter: a null/undefined value acts as the empty string.
      (v) (when (char-data-p (n this))
            (setf (h:dnode-data (n this)) (nullable-string v)) (setf (context-dirty ctx) t)))
    (defgetset ctx np "textContent" (this)
      ;; DOM §textContent: a CharacterData/PI node returns its own data; Document /
      ;; DocumentType return null; other nodes return descendant text concatenated.
      (let ((node (n this)))
        (cond ((member (h:dnode-kind node) '(:document :doctype)) js:*null*)
              ((char-data-p node) (h:dnode-data node))
              (t (dom:text-content node))))
      ;; DOM §textContent setter: null/undefined acts as the empty string; on a
      ;; Document or DocumentType it does nothing.
      (v) (let ((node (n this)))
            (unless (member (h:dnode-kind node) '(:document :doctype))
              (set-text-content node (nullable-string v)) (setf (context-dirty ctx) t))))

    (defmethod* ctx np "hasChildNodes" 0 (this a)
      (jbool (plusp (length (h:dnode-children (n this))))))
    (defmethod* ctx np "appendChild" 1 (this a)
      (let* ((parent (n this)) (child (require-node ctx (arg a 0)))
             (inserted (if (eq (h:dnode-kind child) :fragment)
                           (coerce (h:dnode-children child) 'list) (list child))))
        (ensure-pre-insertion-validity ctx child parent nil)
        (insert-into parent child nil) (setf (context-dirty ctx) t)
        (dolist (n2 inserted) (run-inserted-scripts ctx n2))
        (arg a 0)))
    (defmethod* ctx np "insertBefore" 2 (this a)
      (let* ((parent (n this)) (new (require-node ctx (arg a 0)))
             (ref-obj (arg a 1))
             ;; insertBefore(node, child): child is a required nullable Node — a
             ;; missing/undefined or non-Node value is a TypeError (WebIDL).
             (ref (cond ((eq ref-obj js:*null*) nil)
                        ((eq ref-obj js:*undefined*)
                         (js:js-throw (js:make-native-error
                                       "TypeError" "insertBefore requires 2 arguments")))
                        (t (require-node ctx ref-obj))))
             (inserted (if (eq (h:dnode-kind new) :fragment)
                           (coerce (h:dnode-children new) 'list) (list new))))
        (ensure-pre-insertion-validity ctx new parent ref)
        ;; Inserting a node before itself is a no-op move: the reference becomes
        ;; the node's next sibling (DOM §pre-insert).
        (when (eq ref new) (setf ref (node-next-sibling new)))
        (insert-into parent new ref) (setf (context-dirty ctx) t)
        (dolist (n2 inserted) (run-inserted-scripts ctx n2))
        (arg a 0)))
    (defmethod* ctx np "removeChild" 1 (this a)
      (let ((parent (n this)) (child (require-node ctx (arg a 0))))
        (unless (eq (h:dnode-parent child) parent)
          (throw-dom ctx "NotFoundError" 8 "node is not a child"))
        (adjust-ranges-for-removal ctx child)
        (ni-pre-remove ctx child)
        (h:dom-remove child) (setf (context-dirty ctx) t) (arg a 0)))
    (defmethod* ctx np "replaceChild" 2 (this a)
      (let* ((parent (n this)) (new (require-node ctx (arg a 0)))
             (old (require-node ctx (arg a 1)))
             (inserted (if (eq (h:dnode-kind new) :fragment)
                           (coerce (h:dnode-children new) 'list) (list new))))
        (ensure-replace-validity ctx new parent old)
        ;; Reference child is old's next sibling; if that is NEW itself, advance
        ;; past it (DOM §replace), then swap old out for NEW.
        (let ((ref (node-next-sibling old)))
          (when (eq ref new) (setf ref (node-next-sibling new)))
          (h:dom-remove old)
          (insert-into parent new ref))
        (setf (context-dirty ctx) t)
        (dolist (n2 inserted) (run-inserted-scripts ctx n2))
        (arg a 1)))
    (defmethod* ctx np "normalize" 0 (this a) (declare (ignore a))
      (normalize-node ctx (n this)) (setf (context-dirty ctx) t) js:*undefined*)
    (defmethod* ctx np "cloneNode" 1 (this a)
      (let* ((node (n this)) (clone (clone-dnode ctx node (truthy (arg a 0)))))
        ;; The clone's node document is the original's (DOM §clone) — record it so
        ;; ownerDocument is right while the clone is detached.
        (unless (eq (h:dnode-kind node) :document)
          (setf (gethash clone (context-owner-docs ctx)) (node-document ctx node)))
        (wrap ctx clone)))
    (defmethod* ctx np "contains" 1 (this a)
      (let ((node (n this)) (other (node-of ctx (arg a 0))))
        (jbool (and other (loop for p = other then (h:dnode-parent p)
                                while p thereis (eq p node))))))
    (defmethod* ctx np "isEqualNode" 1 (this a)
      (let ((other (node-of ctx (arg a 0))))
        (jbool (and other (nodes-equal-p ctx (n this) other)))))
    (defmethod* ctx np "isSameNode" 1 (this a)
      (jbool (eq (n this) (node-of ctx (arg a 0)))))
    ;; DOM §Node.compareDocumentPosition (bitmask: DISCONNECTED 1, PRECEDING 2,
    ;; FOLLOWING 4, CONTAINS 8, CONTAINED_BY 16, IMPLEMENTATION_SPECIFIC 32).
    (defmethod* ctx np "compareDocumentPosition" 1 (this a)
      (let ((node (n this)) (other (require-node ctx (arg a 0))))
        (num (compare-document-position node other))))
    ;; DOM §Node.getRootNode — the topmost inclusive ancestor.  Weft has no shadow
    ;; trees, so the `composed` option makes no difference.
    (defmethod* ctx np "getRootNode" 1 (this a) (declare (ignore a))
      (wrap ctx (loop for p = (n this) then (h:dnode-parent p)
                      when (null (h:dnode-parent p)) return p)))
    ;; DOM §Node.lookupNamespaceURI / lookupPrefix / isDefaultNamespace.
    (defmethod* ctx np "lookupNamespaceURI" 1 (this a)
      (opt (locate-namespace ctx (n this) (ns-arg (arg a 0)))))
    (defmethod* ctx np "isDefaultNamespace" 1 (this a)
      (jbool (equal (locate-namespace ctx (n this) nil) (ns-arg (arg a 0)))))
    (defmethod* ctx np "lookupPrefix" 1 (this a)
      (let ((ns (ns-arg (arg a 0))))
        (if (null ns) js:*null*
            (let ((el (context-element-for-ns ctx (n this))))
              (opt (and el (element-locate-prefix ctx el ns)))))))
    ;; Node type constants (also mirrored on the constructor in Acid tests).
    (dolist (pair '(("ELEMENT_NODE" . 1) ("ATTRIBUTE_NODE" . 2) ("TEXT_NODE" . 3)
                    ("CDATA_SECTION_NODE" . 4) ("ENTITY_REFERENCE_NODE" . 5)
                    ("ENTITY_NODE" . 6) ("PROCESSING_INSTRUCTION_NODE" . 7)
                    ("COMMENT_NODE" . 8) ("DOCUMENT_NODE" . 9) ("DOCUMENT_TYPE_NODE" . 10)
                    ("DOCUMENT_FRAGMENT_NODE" . 11) ("NOTATION_NODE" . 12)
                    ("DOCUMENT_POSITION_DISCONNECTED" . 1)
                    ("DOCUMENT_POSITION_PRECEDING" . 2) ("DOCUMENT_POSITION_FOLLOWING" . 4)
                    ("DOCUMENT_POSITION_CONTAINS" . 8) ("DOCUMENT_POSITION_CONTAINED_BY" . 16)
                    ("DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC" . 32)))
      (js:put np (car pair) (num (cdr pair)) :enumerable nil :writable nil :configurable nil))))

(defun image-preload (ctx node src)
  "A script set NODE's (an <img>) src to SRC: fetch it on a macrotask, then fire
   `load` (or `error` if it can't be decoded) on NODE.  Detached images created
   with `new Image()` and lazy-loaders that swap a real URL in on `onload` rely on
   this event to run their swap; without it the swap never fires in a static
   render.  Fetching happens under the image loader bound around the script phase;
   data: URLs decode at paint time, so they just report success."
  (when (and (stringp src) (plusp (length src)))
    (schedule-task ctx
      (lambda ()
        (let ((ok (if (and (>= (length src) 5) (string-equal (subseq src 0 5) "data:"))
                      t
                      (ignore-errors (and (weft.render:fetch-image src) t)))))
          (ignore-errors
            (dispatch-event ctx node (make-event-object ctx (if ok "load" "error") nil))))))))

;;; ---------------------------------------------------------------------------
;;; Element.prototype (<- Node.prototype)
;;; ---------------------------------------------------------------------------
(defun install-element-proto (ctx ep)
  (macrolet ((n (this) `(require-node ctx ,this)))
    (defget ctx ep "tagName" (this) (element-tag-name (n this) ctx))
    (defget ctx ep "localName" (this)
      (let ((info (gethash (n this) (context-ns-info ctx))))
        (if info (getf info :local) (h:dnode-name (n this)))))
    (defget ctx ep "prefix" (this)
      (let ((info (gethash (n this) (context-ns-info ctx))))
        (if (and info (getf info :prefix)) (getf info :prefix) js:*null*)))
    (defget ctx ep "namespaceURI" (this)
      (let ((info (gethash (n this) (context-ns-info ctx))))
        (cond (info (or (getf info :ns) js:*null*))
              ((eq (h:dnode-namespace (n this)) :html) *html-ns*)
              (t js:*null*))))
    (defgetset ctx ep "id" (this) (or (get-attr (n this) "id") "")
      (v) (progn (set-attr (n this) "id" v) (setf (context-dirty ctx) t)))
    (defgetset ctx ep "className" (this) (or (get-attr (n this) "class") "")
      (v) (progn (set-attr (n this) "class" v) (setf (context-dirty ctx) t)))
    ;; classList: a live DOMTokenList (DOM §Element), memoized on the wrapper so
    ;; el.classList === el.classList (SameObject) holds.  [PutForwards=value]:
    ;; `el.classList = x` assigns the class attribute.
    (defgetset ctx ep "classList" (this)
      (let ((existing (js:js-get this "__weft_classList")))
        (if (js:js-object-p existing) existing
            (let ((tl (make-class-list ctx (n this))))
              (js:put this "__weft_classList" tl :enumerable nil :configurable t)
              tl)))
      (v) (progn (set-attr (n this) "class" (jstr v)) (setf (context-dirty ctx) t)))
    ;; Reflected IDL attributes whose property name differs from the content
    ;; attribute (DOM2 HTML): htmlFor<->for, httpEquiv<->http-equiv.
    (defgetset ctx ep "name" (this) (or (get-attr (n this) "name") "")
      (v) (progn (set-attr (n this) "name" (jstr v)) (setf (context-dirty ctx) t)))
    (defgetset ctx ep "title" (this) (or (get-attr (n this) "title") "")
      (v) (progn (set-attr (n this) "title" (jstr v)) (setf (context-dirty ctx) t)))
    (defgetset ctx ep "dir" (this) (or (get-attr (n this) "dir") "")
      (v) (progn (set-attr (n this) "dir" (jstr v)) (setf (context-dirty ctx) t)))
    (defgetset ctx ep "lang" (this) (or (get-attr (n this) "lang") "")
      (v) (progn (set-attr (n this) "lang" (jstr v)) (setf (context-dirty ctx) t)))
    (defgetset ctx ep "htmlFor" (this) (or (get-attr (n this) "for") "")
      (v) (progn (set-attr (n this) "for" (jstr v)) (setf (context-dirty ctx) t)))
    (defgetset ctx ep "httpEquiv" (this) (or (get-attr (n this) "http-equiv") "")
      (v) (progn (set-attr (n this) "http-equiv" (jstr v)) (setf (context-dirty ctx) t)))
    (defget ctx ep "children" (this)
      (let ((node (n this))) (make-collection ctx (lambda () (element-children-list node)))))
    (defget ctx ep "firstElementChild" (this) (wrap ctx (dom:first-element-child (n this))))
    (defget ctx ep "lastElementChild" (this) (wrap ctx (dom:last-element-child (n this))))
    (defget ctx ep "nextElementSibling" (this) (wrap ctx (dom:next-element-sibling (n this))))
    (defget ctx ep "previousElementSibling" (this) (wrap ctx (dom:previous-element-sibling (n this))))
    (defget ctx ep "childElementCount" (this) (num (dom:child-element-count (n this))))
    ;; the style IDL attribute is [PutForwards=cssText] (CSSOM §6.4): `el.style = s`
    ;; forwards to `el.style.cssText = s`, i.e. replaces the inline style declaration.
    (defgetset ctx ep "style" (this) (element-style-object ctx (n this))
      (v) (progn (set-attr (n this) "style" (jstr v)) (setf (context-dirty ctx) t)))
    ;; src/data reflect their attribute and, for a browsing context (iframe/
    ;; object/frame), start loading the referenced document.
    (defgetset ctx ep "src" (this) (or (get-attr (n this) "src") "")
      (v) (let ((node (n this)))
            (set-attr node "src" (jstr v)) (setf (context-dirty ctx) t)
            (cond ((member (h:dnode-name node) '("iframe" "frame") :test #'string=)
                   (load-frame ctx node (jstr v)))
                  ;; setting an <img>'s src fetches it and fires load/error on a
                  ;; macrotask — so `new Image(); img.onload=swap; img.src=url`
                  ;; (lazy-image loaders) actually runs its swap before the render.
                  ((string= (h:dnode-name node) "img")
                   (image-preload ctx node (jstr v))))))
    (defgetset ctx ep "data" (this)
      (let ((node (n this)))
        (if (string= (h:dnode-name node) "object")
            (resolve-url ctx (or (get-attr node "data") "")) (or (get-attr node "data") "")))
      (v) (let ((node (n this)))
            (set-attr node "data" (jstr v)) (setf (context-dirty ctx) t)
            (when (string= (h:dnode-name node) "object")
              (load-frame ctx node (jstr v)))))
    (defgetset ctx ep "href" (this)
      (let ((node (n this)))
        (if (member (h:dnode-name node) '("a" "area" "link" "base") :test #'string=)
            (resolve-url ctx (or (get-attr node "href") "")) (or (get-attr node "href") "")))
      (v) (progn (set-attr (n this) "href" (jstr v)) (setf (context-dirty ctx) t)))
    ;; HTMLFormElement.elements / .length (live, with named access).
    (defget ctx ep "elements" (this)
      (let ((node (n this)))
        (if (string= (h:dnode-name node) "form")
            (make-collection ctx (lambda () (form-controls node))
                             (lambda (name) (named-control node name)))
            js:*undefined*)))
    (defget ctx ep "length" (this)
      (let ((node (n this)))
        (if (string= (h:dnode-name node) "form")
            (num (length (form-controls node))) js:*undefined*)))
    ;; img/iframe/object/canvas/embed height/width: the rendered (computed) box,
    ;; settable via the content attribute.
    (defgetset ctx ep "height" (this)
      (let ((node (n this)))
        (if (member (h:dnode-name node) '("img" "iframe" "object" "canvas" "embed" "video") :test #'string=)
            (num (computed-px ctx node "height")) js:*undefined*))
      (v) (progn (set-attr (n this) "height" (jstr v)) (setf (context-dirty ctx) t)))
    (defgetset ctx ep "width" (this)
      (let ((node (n this)))
        (if (member (h:dnode-name node) '("img" "iframe" "object" "canvas" "embed" "video") :test #'string=)
            (num (computed-px ctx node "width")) js:*undefined*))
      (v) (progn (set-attr (n this) "width" (jstr v)) (setf (context-dirty ctx) t)))
    ;; HTMLSelectElement.options / .add() / .selectedIndex.
    (defget ctx ep "options" (this)
      (let ((node (n this)))
        (if (string= (h:dnode-name node) "select")
            (make-collection ctx (lambda () (dom:get-elements-by-tag-name node "option")))
            js:*undefined*)))
    (defget ctx ep "selectedIndex" (this)
      (let ((node (n this)))
        (if (string= (h:dnode-name node) "select")
            (let* ((opts (dom:get-elements-by-tag-name node "option"))
                   (sel (position-if (lambda (o) (dom:has-attribute o "selected")) opts)))
              (num (or sel (if opts 0 -1))))
            js:*undefined*)))
    (defmethod* ctx ep "add" 2 (this a)
      (let ((node (n this)))
        (when (string= (h:dnode-name node) "select")
          (let ((el (require-node ctx (arg a 0))) (before (arg a 1)))
            (dom-detach el)
            (if (nullish before) (h:dom-append node el)
                (h:dom-insert-before node el (require-node ctx before)))
            (setf (context-dirty ctx) t))))
      js:*undefined*)
    ;; HTMLOptionElement.defaultSelected (reflects the boolean `selected` attribute).
    (defgetset ctx ep "defaultSelected" (this) (jbool (dom:has-attribute (n this) "selected"))
      (v) (if (js:js-truthy v) (set-attr (n this) "selected" "") (remove-attr (n this) "selected")))
    ;; iframe/object contentDocument: hand back a fresh, empty document the test
    ;; can build into (Acid3's getTestDocument path).
    (defget ctx ep "contentDocument" (this) (content-document ctx (n this)))
    (defget ctx ep "contentWindow" (this) (proto ctx :window))
    ;; GetSVGDocument: an <iframe>/<object> referencing an SVG document exposes it
    ;; via getSVGDocument() (== contentDocument).  Clears Acid3 74.
    (defmethod* ctx ep "getSVGDocument" 0 (this a)
      (let ((node (n this)))
        (if (member (h:dnode-name node) '("iframe" "object" "embed" "frame") :test #'string=)
            (content-document ctx node)
            js:*undefined*)))
    ;; <canvas>.getContext('2d') -> a CanvasRenderingContext2D over gesso.
    (defmethod* ctx ep "getContext" 1 (this a)
      (let ((node (n this)))
        (if (string= (h:dnode-name node) "canvas")
            (canvas-rendering-context ctx node (arg a 0))
            js:*null*)))

    (defmethod* ctx ep "getAttribute" 1 (this a)
      (let* ((el (n this)) (name (adjust-qname ctx el (arg a 0))))
        (if (internal-attr-p name) js:*null* (opt (dom:get-attribute el name)))))
    (defmethod* ctx ep "setAttribute" 2 (this a)
      (let* ((el (n this)) (raw (jstr (arg a 0))) (name (adjust-qname ctx el raw)))
        (when (zerop (length raw))
          (throw-dom ctx "InvalidCharacterError" 5 "empty attribute name"))
        (unless (internal-attr-p name)
          (let ((cell (find-attr-qname el name)) (v (jstr (arg a 1))))
            (if cell (setf (cdr cell) v)
                (setf (h:dnode-attrs el) (append (h:dnode-attrs el) (list (cons name v))))))
          (setf (context-dirty ctx) t)
          (when (on-event-attr-p name)
            (register-inline-handler ctx el name (jstr (arg a 1))))))
      js:*undefined*)
    (defmethod* ctx ep "removeAttribute" 1 (this a)
      (let* ((el (n this)) (name (adjust-qname ctx el (arg a 0))))
        (unless (internal-attr-p name)
          (let ((cell (find-attr-qname el name)))
            (when cell (detach-attr-cell ctx el cell)))))
      js:*undefined*)
    (defmethod* ctx ep "toggleAttribute" 2 (this a)
      ;; DOM §toggleAttribute: add/remove a null-namespace attribute; the optional
      ;; FORCE pins the direction.  Returns whether the attribute is present after.
      (let* ((el (n this)) (raw (jstr (arg a 0))) (lname (adjust-qname ctx el raw))
             (has (nthcdr 1 a)) (force (and has (truthy (arg a 1)))))
        (when (zerop (length raw))
          (throw-dom ctx "InvalidCharacterError" 5 "empty attribute name"))
        (if (internal-attr-p lname)
            js:*false*
            (let ((cell (find-attr-qname el lname)))
              (cond (cell (cond ((and has force) js:*true*)
                                (t (detach-attr-cell ctx el cell) js:*false*)))
                    ((and has (not force)) js:*false*)
                    (t (setf (h:dnode-attrs el) (append (h:dnode-attrs el) (list (cons lname ""))))
                       (setf (context-dirty ctx) t) js:*true*))))))
    (defmethod* ctx ep "hasAttribute" 1 (this a)
      (let* ((el (n this)) (name (adjust-qname ctx el (arg a 0))))
        (jbool (and (not (internal-attr-p name)) (dom:has-attribute el name)))))
    (defmethod* ctx ep "hasAttributes" 0 (this a)
      (jbool (and (h:dnode-attrs (n this)) t)))
    (defget ctx ep "attributes" (this)
      (let ((existing (js:js-get this "__weft_attrs")))
        (if (js:js-object-p existing) existing
            (let ((m (make-attr-map ctx (n this))))
              (js:put this "__weft_attrs" m :enumerable nil :configurable t) m))))
    (defmethod* ctx ep "getAttributeNames" 0 (this a)
      (js::make-array-object (loop for c in (h:dnode-attrs (n this)) collect (car c))))
    (defmethod* ctx ep "getAttributeNS" 2 (this a)
      (let ((c (find-attr-ns ctx (n this) (ns-arg (arg a 0)) (jstr (arg a 1)))))
        (if c (opt (cdr c)) js:*null*)))
    (defmethod* ctx ep "setAttributeNS" 3 (this a)
      (let ((el (n this)))
        (set-attr-ns ctx el (arg a 0) (jstr (arg a 1)) (arg a 2))
        (when (on-event-attr-p (jstr (arg a 1)))
          (register-inline-handler ctx el (jstr (arg a 1)) (jstr (arg a 2)))))
      js:*undefined*)
    (defmethod* ctx ep "hasAttributeNS" 2 (this a)
      (jbool (find-attr-ns ctx (n this) (ns-arg (arg a 0)) (jstr (arg a 1)))))
    (defmethod* ctx ep "removeAttributeNS" 2 (this a)
      (let ((c (find-attr-ns ctx (n this) (ns-arg (arg a 0)) (jstr (arg a 1)))))
        (when c (detach-attr-cell ctx (n this) c)))
      js:*undefined*)
    (defmethod* ctx ep "getAttributeNode" 1 (this a)
      (let ((c (find-attr-qname (n this) (attr-name (arg a 0)))))
        (if c (wrap-attr ctx c (n this)) js:*null*)))
    (defmethod* ctx ep "getAttributeNodeNS" 2 (this a)
      (let ((c (find-attr-ns ctx (n this) (ns-arg (arg a 0)) (jstr (arg a 1)))))
        (if c (wrap-attr ctx c (n this)) js:*null*)))
    (defmethod* ctx ep "setAttributeNode" 1 (this a) (attr-map-set ctx (n this) (arg a 0)))
    (defmethod* ctx ep "setAttributeNodeNS" 1 (this a) (attr-map-set ctx (n this) (arg a 0)))
    (defmethod* ctx ep "removeAttributeNode" 1 (this a)
      (let* ((el (n this)) (rec (gethash (arg a 0) (context-attr-of ctx)))
             (cell (and rec (attr-rec-cell rec))))
        (unless (and cell (member cell (h:dnode-attrs el) :test #'eq))
          (throw-dom ctx "NotFoundError" 8 "attribute not found"))
        (let ((w (wrap-attr ctx cell el))) (detach-attr-cell ctx el cell) w)))
    (defmethod* ctx ep "getElementsByTagName" 1 (this a)
      (let ((node (n this)) (tag (string-downcase (jstr (arg a 0)))))
        (make-collection ctx (lambda ()
                               ;; getElementsByTagName excludes the context node itself
                               (remove node (dom:get-elements-by-tag-name node tag))))))
    (defmethod* ctx ep "getElementsByTagNameNS" 2 (this a)
      (let ((node (n this)) (nsraw (jstr (arg a 0))) (local (jstr (arg a 1))))
        (make-collection ctx (lambda ()
                               (remove-if-not (lambda (el) (tag-ns-match ctx el nsraw local))
                                              (remove node (dom:get-elements-by-tag-name node "*")))))))
    (defmethod* ctx ep "getElementsByClassName" 1 (this a)
      (let ((node (n this)) (cls (jstr (arg a 0))))
        (make-collection ctx (lambda () (dom:get-elements-by-class-name node cls)))))
    (flet ((matches-selector (this a)
             ;; matches() with no argument throws TypeError; a syntactically
             ;; invalid selector throws SyntaxError (DOM §Element.matches).
             (when (null a) (js:js-throw (js:make-native-error
                                          "TypeError" "matches requires 1 argument")))
             (let ((sl (parse-selector-or-throw ctx (jstr (arg a 0)))))
               (jbool (css:selector-matches-p sl (n this))))))
      (defmethod* ctx ep "matches" 1 (this a) (matches-selector this a))
      ;; legacy aliases (DOM §Element): both delegate to matches().
      (defmethod* ctx ep "webkitMatchesSelector" 1 (this a) (matches-selector this a))
      (defmethod* ctx ep "matchesSelector" 1 (this a) (matches-selector this a)))
    (defmethod* ctx ep "querySelector" 1 (this a)
      (let ((sl (parse-selector-or-throw ctx (jstr (arg a 0)))))
        (let ((m (and sl (qs-first (n this) sl)))) (if m (wrap ctx m) js:*null*))))
    (defmethod* ctx ep "querySelectorAll" 1 (this a)
      (let ((sl (parse-selector-or-throw ctx (jstr (arg a 0)))))
        (make-collection ctx (lambda () (and sl (qs-all (n this) sl))) nil :nodelist)))
    (defmethod* ctx ep "closest" 1 (this a)
      ;; closest() also throws SyntaxError on an invalid selector (DOM §Element).
      (let ((sl (parse-selector-or-throw ctx (jstr (arg a 0)))))
        (if sl
            (loop for e = (n this) then (h:dnode-parent e)
                  while (and e (eq (h:dnode-kind e) :element))
                  when (css:selector-matches-p sl e) return (wrap ctx e)
                  finally (return js:*null*))
            js:*null*)))
    ;; checkbox/radio checkedness + disabled, as live properties.
    (defgetset ctx ep "checked" (this) (jbool (checked-p (n this)))
      (v) (set-checked ctx (n this) (js:js-truthy v)))
    (defgetset ctx ep "disabled" (this) (jbool (dom:has-attribute (n this) "disabled"))
      (v) (progn (if (js:js-truthy v) (set-attr (n this) "disabled" "") (remove-attr (n this) "disabled"))
                 (setf (context-dirty ctx) t)))
    (defgetset ctx ep "action" (this) (or (get-attr (n this) "action") "")
      (v) (progn (set-attr (n this) "action" (jstr v)) (setf (context-dirty ctx) t)))
    (defgetset ctx ep "method" (this) (or (get-attr (n this) "method") "")
      (v) (progn (set-attr (n this) "method" (jstr v)) (setf (context-dirty ctx) t)))
    ;; HTMLElement.click(): run the control's pre-click activation (toggle a
    ;; checkbox, select a radio) then dispatch a bubbling, cancelable click; a
    ;; submit control that isn't cancelled then fires the form's submit event.
    (defmethod* ctx ep "click" 0 (this a)
      (let* ((node (n this)) (tag (and (eq (h:dnode-kind node) :element) (h:dnode-name node)))
             (type (and (member tag '("input" "button") :test #'equal) (input-type node))))
        (cond ((equal type "checkbox") (set-checked ctx node (not (checked-p node))))
              ((equal type "radio") (set-checked ctx node t)))
        (let* ((ev (make-event-object ctx "click" nil)) (e (evt-of ctx ev)))
          (setf (evt-bubbles e) t (evt-cancelable e) t)
          (dispatch-event ctx node ev)
          ;; default action for a submit button: fire the form's submit event
          (when (and (or (and (equal tag "input") (member type '("submit" "image") :test #'equal))
                         (and (equal tag "button") (equal type "submit")))
                     (not (evt-default-prevented e)))
            (let ((form (loop for a2 = (h:dnode-parent node) then (h:dnode-parent a2)
                              while a2 when (tag= a2 "form") return a2)))
              (when form
                (let* ((sev (make-event-object ctx "submit" nil)) (se (evt-of ctx sev)))
                  (setf (evt-bubbles se) t (evt-cancelable se) t)
                  (dispatch-event ctx form sev)))))))
      js:*undefined*)
    ;; type/value reflections for form controls.
    (defgetset ctx ep "type" (this)
      (let* ((node (n this)) (tag (h:dnode-name node)) (raw (get-attr node "type")))
        (cond ((string= tag "button")
               (let ((v (and raw (string-downcase raw))))
                 (if (member v '("submit" "reset" "button" "menu") :test #'equal) v "submit")))
              ((string= tag "input")
               (let ((v (and raw (string-downcase raw))))
                 (if (member v '("text" "password" "checkbox" "radio" "submit" "reset"
                                 "button" "hidden" "image" "file" "color" "date" "email"
                                 "number" "range" "search" "tel" "time" "url" "month"
                                 "week" "datetime-local") :test #'equal)
                     v "text")))
              (t (or raw ""))))
      (v) (progn (set-attr (n this) "type" (jstr v)) (setf (context-dirty ctx) t)))
    (defgetset ctx ep "value" (this)
      (let ((node (n this)))
        (if (string= (h:dnode-name node) "input")
            (multiple-value-bind (v present) (gethash node (context-input-values ctx))
              (if present v (or (get-attr node "value") "")))
            (or (get-attr node "value") "")))
      (v) (let ((node (n this)))
            (if (string= (h:dnode-name node) "input")
                (setf (gethash node (context-input-values ctx)) (jstr v))
                (progn (set-attr node "value" (jstr v)) (setf (context-dirty ctx) t)))))
    ;; innerHTML / outerHTML / insertAdjacentHTML (DOM Parsing & Serialization §2).
    (defgetset ctx ep "innerHTML" (this) (h:serialize-html-fragment (n this))
      (v) (let* ((node (n this))
                 (frag (h:parse-fragment (null->empty v) (h:dnode-name node))))
            (adopt-parsed-fragment ctx frag (owner-doc-node ctx node))
            (replace-all-children node frag)
            (setf (context-dirty ctx) t)))
    (defgetset ctx ep "outerHTML" (this) (h:serialize-html-outer (n this))
      (v) (let* ((node (n this)) (parent (h:dnode-parent node)))
            (cond ((null parent)
                   (throw-dom ctx "NoModificationAllowedError" 7 "no parent for outerHTML"))
                  ((eq (h:dnode-kind parent) :document)
                   (throw-dom ctx "NoModificationAllowedError" 7 "cannot set outerHTML on a document child")))
            (let* ((ctxname (if (eq (h:dnode-kind parent) :element) (h:dnode-name parent) "body"))
                   (frag (h:parse-fragment (null->empty v) ctxname))
                   (ref (node-next-sibling node)))
              (adopt-parsed-fragment ctx frag (owner-doc-node ctx node))
              (h:dom-remove node)
              (loop for c across (copy-seq (h:dnode-children frag))
                    do (h:dom-remove c)
                       (if ref (h:dom-insert-before parent c ref) (h:dom-append parent c)))
              (setf (context-dirty ctx) t))))
    (defmethod* ctx ep "insertAdjacentHTML" 2 (this a)
      (let* ((node (n this))
             (pos (string-downcase (jstr (arg a 0))))
             (html (jstr (arg a 1)))
             (parent (h:dnode-parent node)))
        (flet ((need-parent ()
                 (when (or (null parent) (eq (h:dnode-kind parent) :document))
                   (throw-dom ctx "NoModificationAllowedError" 7 "no valid parent"))))
          (cond
            ((string= pos "beforebegin")
             (need-parent)
             (let ((frag (h:parse-fragment html (h:dnode-name parent))))
               (adopt-parsed-fragment ctx frag (owner-doc-node ctx node))
               (loop for c across (copy-seq (h:dnode-children frag))
                     do (h:dom-remove c) (h:dom-insert-before parent c node))))
            ((string= pos "afterbegin")
             (let* ((frag (h:parse-fragment html (h:dnode-name node)))
                    (ref (and (plusp (length (h:dnode-children node)))
                              (aref (h:dnode-children node) 0))))
               (adopt-parsed-fragment ctx frag (owner-doc-node ctx node))
               (loop for c across (copy-seq (h:dnode-children frag))
                     do (h:dom-remove c)
                        (if ref (h:dom-insert-before node c ref) (h:dom-append node c)))))
            ((string= pos "beforeend")
             (let ((frag (h:parse-fragment html (h:dnode-name node))))
               (adopt-parsed-fragment ctx frag (owner-doc-node ctx node))
               (loop for c across (copy-seq (h:dnode-children frag))
                     do (h:dom-remove c) (h:dom-append node c))))
            ((string= pos "afterend")
             (need-parent)
             (let ((frag (h:parse-fragment html (h:dnode-name parent)))
                   (ref (node-next-sibling node)))
               (adopt-parsed-fragment ctx frag (owner-doc-node ctx node))
               (loop for c across (copy-seq (h:dnode-children frag))
                     do (h:dom-remove c)
                        (if ref (h:dom-insert-before parent c ref) (h:dom-append parent c)))))
            (t (throw-dom ctx "SyntaxError" 12 "invalid insertAdjacentHTML position")))
          (setf (context-dirty ctx) t)))
      js:*undefined*)
    ;; DOM §Element.insertAdjacentElement / insertAdjacentText: the "insert
    ;; adjacent" algorithm shared with insertAdjacentHTML, but moving an existing
    ;; node / a fresh Text node and honoring pre-insertion validity.
    (defmethod* ctx ep "insertAdjacentElement" 2 (this a)
      (let* ((element (n this)) (where (jstr (arg a 0)))
             (node (node-of ctx (arg a 1))))
        (unless (and node (eq (h:dnode-kind node) :element))
          (js:js-throw (js:make-native-error "TypeError" "insertAdjacentElement expects an Element")))
        (let ((r (insert-adjacent ctx element where node)))
          (setf (context-dirty ctx) t) (if r (wrap ctx r) js:*null*))))
    (defmethod* ctx ep "insertAdjacentText" 2 (this a)
      (let* ((element (n this)) (where (jstr (arg a 0)))
             (text (h:make-text (jstr (arg a 1)))))
        (setf (gethash text (context-owner-docs ctx)) (node-document ctx element))
        (insert-adjacent ctx element where text)
        (setf (context-dirty ctx) t)
        js:*undefined*))))

;;; ---------------------------------------------------------------------------
;;; CharacterData (Text, Comment)  (<- Node.prototype)
;;; ---------------------------------------------------------------------------
(defun install-chardata-proto (ctx cp)
  (macrolet ((n (this) `(require-node ctx ,this))
             (cd-data (this) `(or (h:dnode-data (n ,this)) ""))
             (store (this val) `(progn (setf (h:dnode-data (n ,this)) ,val)
                                       (setf (context-dirty ctx) t))))
    (flet ((need (a k) "WebIDL: throw TypeError when fewer than K arguments were passed."
             (when (< (length a) k)
               (js:js-throw (js:make-native-error "TypeError" "not enough arguments")))))
      (defgetset ctx cp "data" (this) (cd-data this)
        (v) (store this (null->empty v)))     ; [LegacyNullToEmptyString]
      (defget ctx cp "length" (this) (num (length (cd-data this))))
      ;; ProcessingInstruction.target (DOM §PI) — the PI's target name.
      (defget ctx cp "target" (this) (or (h:dnode-name (n this)) ""))
      ;; Text.wholeText (DOM §Text): concatenated data of the run of Text nodes
      ;; contiguous with THIS (previous + this + following), in tree order.
      (defget ctx cp "wholeText" (this)
        (let ((node (n this)))
          (if (member (h:dnode-kind node) '(:text :cdata))
              (let* ((p (h:dnode-parent node)))
                (if (null p) (or (h:dnode-data node) "")
                    (let* ((sibs (h:dnode-children p)) (idx (position node sibs))
                           (start idx) (end idx))
                      (loop while (and (> start 0)
                                       (member (h:dnode-kind (aref sibs (1- start))) '(:text :cdata)))
                            do (decf start))
                      (loop while (and (< (1+ end) (length sibs))
                                       (member (h:dnode-kind (aref sibs (1+ end))) '(:text :cdata)))
                            do (incf end))
                      (with-output-to-string (o)
                        (loop for k from start to end
                              do (write-string (or (h:dnode-data (aref sibs k)) "") o))))))
              (cd-data this))))
      ;; Text.splitText (DOM §Text): split at OFFSET, returning the new second
      ;; half (inserted as the next sibling); offset > length -> IndexSizeError.
      (defmethod* ctx cp "splitText" 1 (this a)
        (need a 1)
        (let* ((node (n this)) (len (length (cd-data this)))
               (off (mod (int-arg a 0) (expt 2 32))))
          (when (> off len) (throw-dom ctx "IndexSizeError" 1 "offset > length"))
          (wrap ctx (split-text ctx node off))))
      (defmethod* ctx cp "appendData" 1 (this a)
        (need a 1)
        (store this (concatenate 'string (cd-data this) (jstr (arg a 0))))
        js:*undefined*)
      ;; substringData/insertData/deleteData/replaceData (DOM §CharacterData):
      ;; all validate offset <= length with IndexSizeError and clamp count.
      (defmethod* ctx cp "substringData" 2 (this a)
        (need a 2)
        (let* ((s (cd-data this)) (len (length s)) (off (mod (int-arg a 0) (expt 2 32)))
               (cnt (mod (int-arg a 1) (expt 2 32))))
          (when (> off len) (throw-dom ctx "IndexSizeError" 1 "offset > length"))
          (subseq s off (min len (+ off cnt)))))
      (defmethod* ctx cp "insertData" 2 (this a)
        (need a 2)
        (let* ((s (cd-data this)) (len (length s)) (off (mod (int-arg a 0) (expt 2 32))))
          (when (> off len) (throw-dom ctx "IndexSizeError" 1 "offset > length"))
          (store this (concatenate 'string (subseq s 0 off) (jstr (arg a 1)) (subseq s off))))
        js:*undefined*)
      (defmethod* ctx cp "deleteData" 2 (this a)
        (need a 2)
        (let* ((s (cd-data this)) (len (length s)) (off (mod (int-arg a 0) (expt 2 32)))
               (cnt (mod (int-arg a 1) (expt 2 32))))
          (when (> off len) (throw-dom ctx "IndexSizeError" 1 "offset > length"))
          (store this (concatenate 'string (subseq s 0 off) (subseq s (min len (+ off cnt))))))
        js:*undefined*)
      (defmethod* ctx cp "replaceData" 3 (this a)
        (need a 3)
        (let* ((s (cd-data this)) (len (length s)) (off (mod (int-arg a 0) (expt 2 32)))
               (cnt (mod (int-arg a 1) (expt 2 32))))
          (when (> off len) (throw-dom ctx "IndexSizeError" 1 "offset > length"))
          (store this (concatenate 'string (subseq s 0 off) (jstr (arg a 2))
                                   (subseq s (min len (+ off cnt))))))
        js:*undefined*))))

;;; ---------------------------------------------------------------------------
;;; Document.prototype (<- Node.prototype)
;;; ---------------------------------------------------------------------------
(defun install-document-proto (ctx dp)
  (macrolet ((n (this) `(require-node ctx ,this)))
    (defget ctx dp "documentElement" (this)
      (wrap ctx (dom:first-element-child (n this))))
    (defget ctx dp "doctype" (this)
      (wrap ctx (find-if (lambda (c) (eq (h:dnode-kind c) :doctype))
                         (h:dnode-children (n this)))))
    (defget ctx dp "body" (this) (wrap ctx (find-tag (n this) "body")))
    (defget ctx dp "head" (this) (wrap ctx (find-tag (n this) "head")))
    (defget ctx dp "forms" (this)
      (let ((node (n this)))
        (make-collection ctx (lambda () (dom:get-elements-by-tag-name node "form"))
                         (lambda (name)
                           (find-if (lambda (f) (or (equal (dom:get-attribute f "name") name)
                                                    (equal (dom:get-attribute f "id") name)))
                                    (dom:get-elements-by-tag-name node "form"))))))
    (defget ctx dp "images" (this)
      (let ((node (n this))) (make-collection ctx (lambda () (dom:get-elements-by-tag-name node "img")))))
    (defget ctx dp "links" (this)
      (let ((node (n this)))
        (make-collection ctx (lambda ()
                               (remove-if-not
                                (lambda (e) (and (member (h:dnode-name e) '("a" "area") :test #'string=)
                                                 (dom:has-attribute e "href")))
                                (dom:get-elements-by-tag-name node "*"))))))
    (defget ctx dp "anchors" (this)
      (let ((node (n this)))
        (make-collection ctx (lambda ()
                               (remove-if-not (lambda (e) (dom:has-attribute e "name"))
                                              (dom:get-elements-by-tag-name node "a"))))))
    ;; DOM §Document.importNode: a clone of an external node into this document.
    (defmethod* ctx dp "importNode" 2 (this a)
      (let* ((obj (arg a 0)) (doc (n this))
             (arec (and (js:js-object-p obj) (gethash obj (context-attr-of ctx))))
             (src (node-of ctx obj)))
        (cond
          (arec (let ((nc (cons (car (attr-rec-cell arec)) (cdr (attr-rec-cell arec)))))
                  (setf (gethash nc (context-attr-recs ctx))
                        (make-attr-rec :cell nc :ns (attr-rec-ns arec)
                                       :prefix (attr-rec-prefix arec)
                                       :local (attr-rec-local arec) :owner nil))
                  (wrap-attr ctx nc)))
          ((or (null src) (member (h:dnode-kind src) '(:document)))
           (throw-dom ctx "NotSupportedError" 9 "cannot import this node"))
          (t (let ((clone (clone-dnode ctx src (truthy (arg a 1)))))
               (adopt-subtree ctx clone doc)
               (wrap ctx clone))))))
    ;; DOM §Document.adoptNode: move an external node into this document.
    (defmethod* ctx dp "adoptNode" 1 (this a)
      (let* ((obj (arg a 0)) (doc (n this))
             (arec (and (js:js-object-p obj) (gethash obj (context-attr-of ctx))))
             (src (node-of ctx obj)))
        (cond
          (arec (setf (attr-rec-owner arec) nil) obj)
          ((null src) (throw-dom ctx "NotSupportedError" 9 "cannot adopt this node"))
          ((eq (h:dnode-kind src) :document)
           (throw-dom ctx "NotSupportedError" 9 "cannot adopt a document"))
          (t (dom-detach src) (adopt-subtree ctx src doc) (wrap ctx src)))))
    (defget ctx dp "defaultView" (this) (proto ctx :window))
    (defget ctx dp "styleSheets" (this) (make-stylesheet-list ctx (n this)))
    (defget ctx dp "readyState" (this) "complete")
    ;; A document created by createHTMLDocument/createDocument has no browsing
    ;; context: its URL is "about:blank".  A DOMParser document and the primary
    ;; document both carry the page URL (DOM §Document url).
    (flet ((doc-url (this) (if (gethash (n this) (context-blank-url-docs ctx))
                               "about:blank" (context-base ctx))))
      (defget ctx dp "URL" (this) (doc-url this))
      (defget ctx dp "documentURI" (this) (doc-url this))
      (defget ctx dp "baseURI" (this) (doc-url this)))
    ;; The primary document carries an own `location` property (set in
    ;; install-globals) that shadows this; every other document has none.
    (defget ctx dp "location" (this) (declare (ignore this)) js:*null*)
    (defget ctx dp "characterSet" (this) "UTF-8")
    (defget ctx dp "charset" (this) "UTF-8")
    (defget ctx dp "inputEncoding" (this) "UTF-8")
    (defget ctx dp "compatMode" (this)
      (if (eq (h:dnode-mode (n this)) :quirks) "BackCompat" "CSS1Compat"))
    (defget ctx dp "contentType" (this)
      (or (gethash (n this) (context-doc-content-types ctx)) "text/html"))
    (defget ctx dp "hidden" (this) js:*false*)
    (defget ctx dp "visibilityState" (this) "visible")
    (defgetset ctx dp "cookie" (this) (context-cookie ctx)
      (v) (let* ((s (jstr v)) (semi (position #\; s))
                 (pair (string-trim " " (subseq s 0 (or semi (length s))))))
            (when (plusp (length pair))
              (setf (context-cookie ctx)
                    (if (plusp (length (context-cookie ctx)))
                        (concatenate 'string (context-cookie ctx) "; " pair) pair)))))
    (defgetset ctx dp "title" (this)
      (let ((tn (find-tag (n this) "title"))) (if tn (dom:text-content tn) ""))
      (v) (let* ((doc (n this)) (tn (find-tag doc "title")))
            (when tn (set-text-content tn (jstr v)) (setf (context-dirty ctx) t))))
    (defmethod* ctx dp "getElementById" 1 (this a)
      ;; An empty-string id never matches (DOM §getElementById), even against an
      ;; element carrying id="".
      (let ((id (jstr (arg a 0))))
        (wrap ctx (and (plusp (length id)) (dom:get-element-by-id (n this) id)))))
    (defmethod* ctx dp "querySelector" 1 (this a)
      (let ((sl (parse-selector-or-throw ctx (jstr (arg a 0)))))
        (let ((m (and sl (qs-first (n this) sl)))) (if m (wrap ctx m) js:*null*))))
    (defmethod* ctx dp "querySelectorAll" 1 (this a)
      (let ((sl (parse-selector-or-throw ctx (jstr (arg a 0)))))
        (make-collection ctx (lambda () (and sl (qs-all (n this) sl))) nil :nodelist)))
    (defmethod* ctx dp "getElementsByTagName" 1 (this a)
      (let ((node (n this)) (tag (string-downcase (jstr (arg a 0)))))
        (make-collection ctx (lambda () (remove node (dom:get-elements-by-tag-name node tag))))))
    (defmethod* ctx dp "getElementsByTagNameNS" 2 (this a)
      (let ((node (n this)) (nsraw (jstr (arg a 0))) (local (jstr (arg a 1))))
        (make-collection ctx (lambda ()
                               (remove-if-not (lambda (el) (tag-ns-match ctx el nsraw local))
                                              (remove node (dom:get-elements-by-tag-name node "*")))))))
    (defmethod* ctx dp "getElementsByClassName" 1 (this a)
      (let ((node (n this)) (cls (jstr (arg a 0))))
        (make-collection ctx (lambda () (dom:get-elements-by-class-name node cls)))))
    (defmethod* ctx dp "createElement" 1 (this a)
      (let ((name (jstr (arg a 0))))
        (unless (valid-name-p name)
          (throw-dom ctx "InvalidCharacterError" 5 "invalid element name"))
        (new-node ctx this (h:make-element (string-downcase name)))))
    (defmethod* ctx dp "createElementNS" 2 (this a)
      (let* ((ns (ns-arg (arg a 0))) (name (jstr (arg a 1)))
             (colon (position #\: name)))
        (unless (and (plusp (length name))
                     (name-start-char-p (char name (if colon (1+ colon) 0)))
                     (every (lambda (c) (and (>= (char-code c) #x21)
                                             (not (member c '(#\< #\> #\& #\" #\' #\/ #\=)))))
                            name))
          (throw-dom ctx "InvalidCharacterError" 5 "invalid element name"))
        ;; DOM "validate and extract" namespace rules.
        (let ((prefix (and colon (subseq name 0 colon))))
          (when (or (and colon (or (zerop colon) (= colon (1- (length name)))
                                   (position #\: name :start (1+ colon))))
                    (and prefix (null ns))
                    (and (equal prefix "xml") (not (equal ns "http://www.w3.org/XML/1998/namespace")))
                    (and (or (equal name "xmlns") (equal prefix "xmlns"))
                         (not (equal ns "http://www.w3.org/2000/xmlns/")))
                    (and (equal ns "http://www.w3.org/2000/xmlns/")
                         (not (or (equal name "xmlns") (equal prefix "xmlns")))))
            (throw-dom ctx "NamespaceError" 14 "malformed or invalid qualified name")))
        ;; A namespaced element keeps the given case (XML is case-sensitive); only
        ;; the HTML parser lowercases.  Record ns/prefix/localName for reflection.
        (let ((el (h:make-element name nil
                                  (cond ((and ns (search "svg" ns)) :svg)
                                        ((and ns (search "MathML" ns)) :math) (t :html)))))
          (setf (gethash el (context-ns-info ctx))
                (list :ns ns :qname name
                      :prefix (and colon (subseq name 0 colon))
                      :local (if colon (subseq name (1+ colon)) name)))
          (new-node ctx this el))))
    (defmethod* ctx dp "createTextNode" 1 (this a) (new-node ctx this (h:make-text (jstr (arg a 0))))) 
    (defmethod* ctx dp "createComment" 1 (this a) (new-node ctx this (h:make-comment (jstr (arg a 0)))))
    (defmethod* ctx dp "createAttribute" 1 (this a)
      (let ((name (jstr (arg a 0))))
        (when (zerop (length name))
          (throw-dom ctx "InvalidCharacterError" 5 "empty attribute name"))
        (make-standalone-attr ctx nil (if (html-doc-p ctx (n this)) (string-downcase name) name))))
    (defmethod* ctx dp "createAttributeNS" 2 (this a)
      (make-standalone-attr ctx (arg a 0) (jstr (arg a 1))))
    (defmethod* ctx dp "createCDATASection" 1 (this a)
      ;; DOM §Document: HTML documents cannot hold CDATA sections.
      (let ((s (jstr (arg a 0))))
        (when (equal (or (gethash (require-node ctx this) (context-doc-content-types ctx))
                         "text/html") "text/html")
          (throw-dom ctx "NotSupportedError" 9 "createCDATASection is not supported in HTML documents"))
        (when (search "]]>" s)
          (throw-dom ctx "InvalidCharacterError" 5 "']]>' not allowed in CDATA"))
        (new-node ctx this (h:make-cdata s))))
    (defmethod* ctx dp "createProcessingInstruction" 2 (this a)
      (let ((target (jstr (arg a 0))) (data (jstr (arg a 1))))
        (unless (valid-qname-p target)
          (throw-dom ctx "InvalidCharacterError" 5 "invalid PI target"))
        (when (search "?>" data)
          (throw-dom ctx "InvalidCharacterError" 5 "'?>' not allowed in PI data"))
        (new-node ctx this (h:make-processing-instruction target data))))
    (defmethod* ctx dp "createDocumentFragment" 0 (this a) (new-node ctx this (h:make-fragment)))
    (defmethod* ctx dp "createEvent" 1 (this a) (make-event-object ctx "" nil))
    (defmethod* ctx dp "write" 1 (this a)
      (document-write* ctx (n this) (apply #'concatenate 'string (mapcar #'jstr a)))
      js:*undefined*)
    (defmethod* ctx dp "writeln" 1 (this a)
      (document-write* ctx (n this)
                       (concatenate 'string (apply #'concatenate 'string (mapcar #'jstr a)) (string #\Newline)))
      js:*undefined*)
    (defmethod* ctx dp "open" 0 (this a)
      (let ((doc (n this)))
        (loop for c across (h:dnode-children doc) do (setf (h:dnode-parent c) nil))
        (setf (fill-pointer (h:dnode-children doc)) 0)
        (setf (gethash doc (context-write-buffers ctx)) "" (context-dirty ctx) t))
      (wrap ctx (n this)))
    (defmethod* ctx dp "close" 0 (this a)
      (let ((doc (n this)))
        (multiple-value-bind (buf active) (gethash doc (context-write-buffers ctx))
          (when active
            (document-replace ctx doc buf)
            (remhash doc (context-write-buffers ctx)))))
      js:*undefined*)
    ;; document.implementation is per-document and [SameObject] (DOM §Document):
    ;; memoize the DOMImplementation on the document's own wrapper.
    (defget ctx dp "implementation" (this)
      (let ((existing (js:js-get this "__weft_impl")))
        (if (js:js-object-p existing) existing
            (let ((impl (js:make-object :proto (proto ctx :domimplementation))))
              ;; Remember which document this implementation belongs to, so its
              ;; factory methods can set the created node's ownerDocument (DOM §).
              (js:put impl "__weft_doc" this :enumerable nil :configurable t)
              (js:put this "__weft_impl" impl :enumerable nil :configurable t) impl))))))

(defun document-write* (ctx doc source)
  "document.write: buffer into an open() document, else splice at the running
   <script> (the classic in-parse write)."
  (multiple-value-bind (buf active) (gethash doc (context-write-buffers ctx))
    (if active
        (setf (gethash doc (context-write-buffers ctx)) (concatenate 'string buf source))
        (dom-write ctx source))))

(defun valid-qname-p (qname)
  "A qualified name: an optional single prefix and a local part, each a valid
   Name, with the colon neither leading nor trailing."
  (let ((colon (position #\: qname)))
    (and (plusp (length qname))
         (or (null (position #\: qname :start (1+ (or colon -1))))  ; at most one colon
             (null colon))
         (if colon
             (and (plusp colon) (< (1+ colon) (length qname))
                  (name-start-char-p (char qname 0))
                  (name-start-char-p (char qname (1+ colon))))
             (name-start-char-p (char qname 0))))))

(defun install-dom-implementation-proto (ctx impl)
  "DOMImplementation.prototype — the factory methods are context-scoped and
   ignore their receiver, so all per-document instances share this prototype."
  (progn
        (defmethod* ctx impl "hasFeature" 2 (this a) js:*true*)
        (defmethod* ctx impl "createDocument" 3 (this a)
          (let* ((d (h:make-document)) (qn (arg a 1)) (dt (arg a 2))
                 (ns (if (nullish (arg a 0)) nil (jstr (arg a 0)))))
            ;; contentType per DOM §createDocument: XHTML/SVG/other-XML.
            (setf (gethash d (context-doc-content-types ctx))
                  (cond ((equal ns *html-ns*) "application/xhtml+xml")
                        ((equal ns "http://www.w3.org/2000/svg") "image/svg+xml")
                        (t "application/xml")))
            (unless (nullish dt)
              (let ((dtn (node-of ctx dt)))
                (when dtn (h:dom-append d dtn)
                      (setf (gethash dtn (context-owner-docs ctx)) d))))  ; adopt the doctype
            ;; An empty (or absent) qualifiedName yields a document with no
            ;; document element (DOM §createDocument step "if not the empty string").
            (unless (or (nullish qn) (zerop (length (jstr qn))))
              (unless (valid-qname-p (jstr qn))
                (throw-dom ctx "InvalidCharacterError" 5 "invalid qualified name"))
              (let ((el (h:make-element (jstr qn))))
                (h:dom-append d el)
                (setf (gethash el (context-owner-docs ctx)) d)))
            (setf (gethash d (context-blank-url-docs ctx)) t)
            (wrap ctx d)))
        (defmethod* ctx impl "createHTMLDocument" 1 (this a)
          ;; DOM §createHTMLDocument appends a `html` doctype first, then the
          ;; html>head(>title?)>body skeleton.
          (let* ((d (h:make-document)) (dt (h:make-doctype "html" "" ""))
                 (html (h:make-element "html"))
                 (head (h:make-element "head"))
                 (body (h:make-element "body")))
            (h:dom-append d dt) (setf (gethash dt (context-owner-docs ctx)) d)
            (h:dom-append d html) (h:dom-append html head)
            ;; DOM §createHTMLDocument: the title element exists only when a title
            ;; argument was supplied.
            (unless (js:js-undefined-p (arg a 0))
              (let ((title (h:make-element "title")))
                (h:dom-append head title)
                (h:dom-append title (h:make-text (jstr (arg a 0))))))
            (h:dom-append html body)
            (setf (gethash d (context-blank-url-docs ctx)) t)
            (wrap ctx d)))
        (defmethod* ctx impl "createDocumentType" 3 (this a)
          ;; QName validation here (InvalidCharacterError for stray chars,
          ;; NamespaceError for a malformed qualified name) is required by Acid3
          ;; test 25 (`createDocumentType('a:', …)` must throw NAMESPACE_ERR); the
          ;; returned doctype's node document is the implementation's document.
          (let ((qname (jstr (arg a 0))))
            (unless (every (lambda (c) (>= (char-code c) #x21)) qname)
              (throw-dom ctx "InvalidCharacterError" 5 "invalid doctype name"))
            (unless (valid-qname-p qname)
              (throw-dom ctx "NamespaceError" 14 "malformed qualified name"))
            (let ((dt (h:make-doctype qname (jstr (arg a 1)) (jstr (arg a 2))))
                  (docwrap (js:js-get this "__weft_doc")))
              (setf (gethash dt (context-owner-docs ctx))
                    (or (and (js:js-object-p docwrap) (node-of ctx docwrap))
                        (context-document ctx)))
              (wrap ctx dt))))
        impl))

(defun dom-write (ctx str)
  "document.write: parse STR as an HTML fragment and splice its body-level nodes
   into the DOM right after the currently-executing <script> (or at end of body)."
  (let* ((frag (h:parse-html str))
         (body (find-tag frag "body"))
         (nodes (and body (coerce (h:dnode-children body) 'list)))
         (script (context-current-script ctx)))
    (if (and script (h:dnode-parent script))
        (let* ((parent (h:dnode-parent script)) (ch (h:dnode-children parent))
               (i (position script ch))
               (ref (and i (< (1+ i) (length ch)) (aref ch (1+ i)))))
          (dolist (nd nodes)
            (h:dom-remove nd)
            (if ref (h:dom-insert-before parent nd ref) (h:dom-append parent nd))))
        (let ((b (find-tag (context-document ctx) "body")))
          (when b (dolist (nd nodes) (h:dom-remove nd) (h:dom-append b nd)))))
    (setf (context-dirty ctx) t)))

(defun install-doctype-proto (ctx dtp)
  (macrolet ((n (this) `(require-node ctx ,this)))
    (defget ctx dtp "name" (this) (or (h:dnode-name (n this)) ""))
    (defget ctx dtp "publicId" (this) (or (h:dnode-public (n this)) ""))
    (defget ctx dtp "systemId" (this) (or (h:dnode-system (n this)) ""))
    (defget ctx dtp "internalSubset" (this) js:*null*)))

(defun find-tag (root tag)
  (first (dom:get-elements-by-tag-name root tag)))

(defun content-document (ctx element)
  "Lazily give an <iframe>/<object> a fresh, empty child document (memoized on
   the element) so a script can populate it (Acid3's getTestDocument)."
  (if (member (h:dnode-name element) '("iframe" "object" "frame") :test #'string=)
      (let ((doc (or (gethash element (context-iframe-docs ctx))
                     (let ((d (h:make-document)))
                       (h:dom-append d (h:make-element "html"))
                       (setf (gethash element (context-iframe-docs ctx)) d)
                       d))))
        (wrap ctx doc))
      js:*null*))
