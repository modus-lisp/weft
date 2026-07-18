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

(defun element-tag-name (node &optional ctx)
  "tagName/nodeName for an element: uppercased for an HTML-namespace element,
   case-preserved otherwise (XML namespaces are case-sensitive)."
  (let ((info (and ctx (gethash node (context-ns-info ctx)))))
    (cond (info (getf info :qname))
          ((eq (h:dnode-namespace node) :html) (string-upcase (h:dnode-name node)))
          (t (h:dnode-name node)))))

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
        (when ins (insert-into parent ins nil) (setf (context-dirty ctx) t)))
      js:*undefined*)
    (defmethod* ctx proto "prepend" 1 (this a)
      (let* ((parent (n this)) (ins (args->insertion ctx (owner-doc-node ctx parent) a))
             (ch (h:dnode-children parent)) (ref (and (plusp (length ch)) (aref ch 0))))
        (when ins (insert-into parent ins ref) (setf (context-dirty ctx) t)))
      js:*undefined*)
    (defmethod* ctx proto "replaceChildren" 1 (this a)
      (let* ((parent (n this)) (ins (args->insertion ctx (owner-doc-node ctx parent) a)))
        (loop for c across (copy-seq (h:dnode-children parent)) do (h:dom-remove c))
        (when ins (insert-into parent ins nil))
        (setf (context-dirty ctx) t))
      js:*undefined*)))

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
    (defmethod* ctx proto "before" 1 (this a)
      (let* ((node (n this)) (parent (h:dnode-parent node)))
        (when parent
          (let ((ins (args->insertion ctx (owner-doc-node ctx node) a)))
            (when ins (insert-into parent ins node) (setf (context-dirty ctx) t)))))
      js:*undefined*)
    (defmethod* ctx proto "after" 1 (this a)
      (let* ((node (n this)) (parent (h:dnode-parent node)) (ref (node-next-sibling node)))
        (when parent
          (let ((ins (args->insertion ctx (owner-doc-node ctx node) a)))
            (when ins (insert-into parent ins ref) (setf (context-dirty ctx) t)))))
      js:*undefined*)
    (defmethod* ctx proto "replaceWith" 1 (this a)
      (let* ((node (n this)) (parent (h:dnode-parent node)) (ref (node-next-sibling node)))
        (when parent
          (let ((ins (args->insertion ctx (owner-doc-node ctx node) a)))
            (adjust-ranges-for-removal ctx node)
            (h:dom-remove node)
            (when ins (insert-into parent ins ref))
            (setf (context-dirty ctx) t))))
      js:*undefined*)))

(defun copy-dnode (node deep)
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
      (loop for ch across (h:dnode-children node)
            do (h:dom-append c (copy-dnode ch t))))
    c))

(defun attrs-equal-p (a b)
  "Attribute lists (name . value alists) compared as unordered sets."
  (and (= (length a) (length b))
       (every (lambda (pair)
                (let ((m (assoc (car pair) b :test #'equal)))
                  (and m (equal (cdr pair) (cdr m)))))
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
          (let ((ia (gethash a (context-ns-info ctx))) (ib (gethash b (context-ns-info ctx))))
            (and (eq (h:dnode-namespace a) (h:dnode-namespace b))
                 (equal (getf ia :ns) (getf ib :ns))
                 (equal (getf ia :prefix) (getf ib :prefix))
                 (equal (or (getf ia :local) (h:dnode-name a))
                        (or (getf ib :local) (h:dnode-name b)))
                 (attrs-equal-p (h:dnode-attrs a) (h:dnode-attrs b)))))
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
   writes the attribute on every operation so it always reflects the current value."
  (let* ((realm (context-realm ctx))
         (op (js:eval-script realm "Object.prototype"))
         (tl (js:make-object :proto op)))
    (labels ((toks () (ordered-set (split-tokens (get-attr node "class"))))
             (store (list)
               (set-attr node "class" (format nil "~{~a~^ ~}" list))
               (setf (context-dirty ctx) t))
             (check (tk)
               (when (zerop (length tk))
                 (throw-dom ctx "SyntaxError" 12 "empty token"))
               (when (some #'ascii-ws-p tk)
                 (throw-dom ctx "InvalidCharacterError" 5 "token contains whitespace"))
               tk)
             (meth (name arity fn) (js:put tl name (js:native-function realm name fn arity)
                                           :enumerable nil)))
      (js:put-accessor tl "length"
        :get (js:native-function realm "get length"
               (lambda (this ig) (declare (ignore this ig)) (num (length (toks)))) 0)
        :enumerable t :configurable t)
      (js:put-accessor tl "value"
        :get (js:native-function realm "get value"
               (lambda (this ig) (declare (ignore this ig)) (or (get-attr node "class") "")) 0)
        :set (js:native-function realm "set value"
               (lambda (this a) (declare (ignore this)) (set-attr node "class" (jstr (arg a 0)))
                 (setf (context-dirty ctx) t) js:*undefined*) 1)
        :enumerable t :configurable t)
      (meth "item" 1 (lambda (this a) (declare (ignore this))
                       (let* ((i (truncate (js:to-number (arg a 0)))) (ts (toks)))
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
                          (let* ((old (check (jstr (arg a 0)))) (new (check (jstr (arg a 1))))
                                 (ts (toks)))
                            (if (member old ts :test #'string=)
                                (progn (store (ordered-set (substitute new old ts :test #'string=)))
                                       js:*true*)
                                js:*false*))))
      (meth "toString" 0 (lambda (this a) (declare (ignore this a)) (or (get-attr node "class") "")))
      (meth "forEach" 1 (lambda (this a) (declare (ignore this))
                          (let ((cb (arg a 0)) (i 0))
                            (dolist (tk (toks)) (js:js-call cb (arg a 1) (list tk (num i) tl)) (incf i)))
                          js:*undefined*))
      ;; Integer-indexed getter (classList[0]) and length are surfaced as own
      ;; data properties; the DOMTokenList is live, so refresh them whenever the
      ;; class attribute is (re)read.  A closure over TL installs the stringifier
      ;; tag and the iteration protocol (Symbol.iterator/keys/values/entries).
      (let ((installer (js:eval-script realm "(function(tl){
        tl[Symbol.toStringTag]='DOMTokenList';
        function arr(){var a=[];for(var i=0,n=tl.length;i<n;i++)a.push(tl.item(i));return a;}
        tl[Symbol.iterator]=function(){return arr()[Symbol.iterator]();};
        tl.keys=function(){return arr().map(function(_,i){return i;})[Symbol.iterator]();};
        tl.values=function(){return arr()[Symbol.iterator]();};
        tl.entries=function(){return arr().map(function(v,i){return [i,v];})[Symbol.iterator]();};
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
  (setf (h:dnode-attrs node)
        (remove (attr-name name) (h:dnode-attrs node) :key #'car :test #'string=)))

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

(defun make-collection (ctx list-fn &optional name-fn)
  "A live NodeList/HTMLCollection: length + integer indexing + item(), reading
   LIST-FN (-> a fresh CL list of weft nodes) on every access. If NAME-FN is
   given, an unknown string key is looked up by name (namedItem semantics)."
  (let* ((realm (context-realm ctx))
         (item (js:native-function realm "item"
                 (lambda (this a) (declare (ignore this))
                   (let ((i (int-arg a 0)) (l (funcall list-fn)))
                     (if (< -1 i (length l)) (wrap ctx (nth i l)) js:*null*)))
                 1))
         (named (js:native-function realm "namedItem"
                  (lambda (this a) (declare (ignore this))
                    (let ((node (and name-fn (funcall name-fn (jstr (arg a 0))))))
                      (if node (wrap ctx node) js:*null*)))
                  1)))
    (js:make-host-object realm
      :get (lambda (o key rcv) (declare (ignore rcv))
             (let ((key (js:to-property-key key)))   ; obj[0] arrives as a number
               (cond
                 ((and (stringp key) (string= key "length"))
                  (num (length (funcall list-fn))))
                 ((index-string-p key)
                  (let ((i (parse-integer key)) (l (funcall list-fn)))
                    (if (< i (length l)) (wrap ctx (nth i l)) js:*undefined*)))
                 ((and (stringp key) (string= key "item")) item)
                 ((and (stringp key) (string= key "namedItem")) named)
                 ((and name-fn (stringp key) (funcall name-fn key))
                  (wrap ctx (funcall name-fn key)))
                 (t (js:js-get (js:js-object-proto o) key o))))))))

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
    (defget ctx np "parentElement" (this)
      (let ((p (h:dnode-parent (n this))))
        (wrap ctx (and p (eq (h:dnode-kind p) :element) p))))
    (defget ctx np "ownerDocument" (this)
      (let ((node (n this)))
        (cond ((eq (h:dnode-kind node) :document) js:*null*)
              ((gethash node (context-owner-docs ctx)) (wrap ctx (gethash node (context-owner-docs ctx))))
              (t (loop for p = (h:dnode-parent node) then (h:dnode-parent p)
                       while p when (eq (h:dnode-kind p) :document) return (wrap ctx p)
                       finally (return js:*null*))))))
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
    (defget ctx np "childNodes" (this)
      (let ((node (n this))) (make-collection ctx (lambda () (children-list node)))))
    (defgetset ctx np "nodeValue" (this)
      (if (char-data-p (n this)) (h:dnode-data (n this)) js:*null*)
      (v) (when (char-data-p (n this))
            (setf (h:dnode-data (n this)) (jstr v)) (setf (context-dirty ctx) t)))
    (defgetset ctx np "textContent" (this)
      ;; DOM §textContent: a CharacterData/PI node returns its own data; Document /
      ;; DocumentType return null; other nodes return descendant text concatenated.
      (let ((node (n this)))
        (cond ((member (h:dnode-kind node) '(:document :doctype)) js:*null*)
              ((char-data-p node) (h:dnode-data node))
              (t (dom:text-content node))))
      (v) (progn (set-text-content (n this) (jstr v)) (setf (context-dirty ctx) t)))

    (defmethod* ctx np "hasChildNodes" 0 (this a)
      (jbool (plusp (length (h:dnode-children (n this))))))
    (defmethod* ctx np "appendChild" 1 (this a)
      (let* ((parent (n this)) (child (require-node ctx (arg a 0)))
             (inserted (if (eq (h:dnode-kind child) :fragment)
                           (coerce (h:dnode-children child) 'list) (list child))))
        (when (ancestor-or-self-p child parent)
          (throw-dom ctx "HierarchyRequestError" 3 "would create a cycle"))
        (insert-into parent child nil) (setf (context-dirty ctx) t)
        (dolist (n2 inserted) (run-inserted-scripts ctx n2))
        (arg a 0)))
    (defmethod* ctx np "insertBefore" 2 (this a)
      (let* ((parent (n this)) (new (require-node ctx (arg a 0)))
             (ref-obj (arg a 1))
             (ref (and (not (nullish ref-obj)) (require-node ctx ref-obj)))
             (inserted (if (eq (h:dnode-kind new) :fragment)
                           (coerce (h:dnode-children new) 'list) (list new))))
        (when (ancestor-or-self-p new parent)
          (throw-dom ctx "HierarchyRequestError" 3 "would create a cycle"))
        (when (and ref (not (eq (h:dnode-parent ref) parent)))
          (throw-dom ctx "NotFoundError" 8 "reference node is not a child"))
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
      (let ((parent (n this)) (new (require-node ctx (arg a 0)))
            (old (require-node ctx (arg a 1))))
        (unless (eq (h:dnode-parent old) parent)
          (throw-dom ctx "NotFoundError" 8 "node is not a child"))
        (when (ancestor-or-self-p new parent)
          (throw-dom ctx "HierarchyRequestError" 3 "would create a cycle"))
        (insert-into parent new old) (h:dom-remove old)
        (setf (context-dirty ctx) t) (arg a 1)))
    (defmethod* ctx np "cloneNode" 1 (this a)
      (wrap ctx (copy-dnode (n this) (truthy (arg a 0)))))
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
    ;; Node type constants (also mirrored on the constructor in Acid tests).
    (dolist (pair '(("ELEMENT_NODE" . 1) ("ATTRIBUTE_NODE" . 2) ("TEXT_NODE" . 3)
                    ("CDATA_SECTION_NODE" . 4) ("ENTITY_REFERENCE_NODE" . 5)
                    ("ENTITY_NODE" . 6) ("PROCESSING_INSTRUCTION_NODE" . 7)
                    ("COMMENT_NODE" . 8) ("DOCUMENT_NODE" . 9) ("DOCUMENT_TYPE_NODE" . 10)
                    ("DOCUMENT_FRAGMENT_NODE" . 11) ("NOTATION_NODE" . 12)))
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
    ;; el.classList === el.classList (SameObject) holds.
    (defget ctx ep "classList" (this)
      (let ((existing (js:js-get this "__weft_classList")))
        (if (js:js-object-p existing) existing
            (let ((tl (make-class-list ctx (n this))))
              (js:put this "__weft_classList" tl :enumerable nil :configurable t)
              tl))))
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
      (let ((name (jstr (arg a 0))))
        (if (internal-attr-p name) js:*null* (opt (get-attr (n this) name)))))
    (defmethod* ctx ep "setAttribute" 2 (this a)
      (let ((name (jstr (arg a 0))))
        (unless (internal-attr-p name)
          (set-attr (n this) name (arg a 1)) (setf (context-dirty ctx) t)
          (when (on-event-attr-p name)
            (register-inline-handler ctx (n this) name (jstr (arg a 1))))))
      js:*undefined*)
    (defmethod* ctx ep "removeAttribute" 1 (this a)
      (let ((name (jstr (arg a 0))))
        (unless (internal-attr-p name)
          (remove-attr (n this) name) (setf (context-dirty ctx) t)))
      js:*undefined*)
    (defmethod* ctx ep "hasAttribute" 1 (this a)
      (let ((name (attr-name (arg a 0))))
        (jbool (and (not (internal-attr-p name)) (dom:has-attribute (n this) name)))))
    (defmethod* ctx ep "hasAttributes" 0 (this a)
      (jbool (and (h:dnode-attrs (n this)) t)))
    (defmethod* ctx ep "getElementsByTagName" 1 (this a)
      (let ((node (n this)) (tag (string-downcase (jstr (arg a 0)))))
        (make-collection ctx (lambda ()
                               ;; getElementsByTagName excludes the context node itself
                               (remove node (dom:get-elements-by-tag-name node tag))))))
    (defmethod* ctx ep "getElementsByClassName" 1 (this a)
      (let ((node (n this)) (cls (jstr (arg a 0))))
        (make-collection ctx (lambda () (dom:get-elements-by-class-name node cls)))))
    (defmethod* ctx ep "matches" 1 (this a)
      (jbool (ignore-errors (css:selector-matches-p
                             (css:parse-selector-list (jstr (arg a 0))) (n this)))))
    (defmethod* ctx ep "querySelector" 1 (this a)
      (let ((sl (ignore-errors (css:parse-selector-list (jstr (arg a 0))))))
        (let ((m (and sl (qs-first (n this) sl)))) (if m (wrap ctx m) js:*null*))))
    (defmethod* ctx ep "querySelectorAll" 1 (this a)
      (let ((sl (ignore-errors (css:parse-selector-list (jstr (arg a 0))))))
        (make-collection ctx (lambda () (and sl (qs-all (n this) sl))))))
    (defmethod* ctx ep "closest" 1 (this a)
      (let ((sl (ignore-errors (css:parse-selector-list (jstr (arg a 0))))))
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
      js:*undefined*)))

;;; ---------------------------------------------------------------------------
;;; CharacterData (Text, Comment)  (<- Node.prototype)
;;; ---------------------------------------------------------------------------
(defun install-chardata-proto (ctx cp)
  (macrolet ((n (this) `(require-node ctx ,this)))
    (defgetset ctx cp "data" (this) (or (h:dnode-data (n this)) "")
      (v) (progn (setf (h:dnode-data (n this)) (jstr v)) (setf (context-dirty ctx) t)))
    (defget ctx cp "length" (this) (num (length (or (h:dnode-data (n this)) ""))))
    (defmethod* ctx cp "appendData" 1 (this a)
      (setf (h:dnode-data (n this)) (concatenate 'string (or (h:dnode-data (n this)) "") (jstr (arg a 0))))
      (setf (context-dirty ctx) t) js:*undefined*)
    (defmethod* ctx cp "substringData" 2 (this a)
      (let* ((s (or (h:dnode-data (n this)) "")) (off (int-arg a 0))
             (cnt (int-arg a 1)))
        (subseq s (min off (length s)) (min (length s) (+ off cnt)))))))

;;; ---------------------------------------------------------------------------
;;; Document.prototype (<- Node.prototype)
;;; ---------------------------------------------------------------------------
(defun install-document-proto (ctx dp)
  (macrolet ((n (this) `(require-node ctx ,this)))
    (defget ctx dp "documentElement" (this)
      (wrap ctx (dom:first-element-child (n this))))
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
    (defget ctx dp "defaultView" (this) (proto ctx :window))
    (defget ctx dp "styleSheets" (this) (make-stylesheet-list ctx (n this)))
    (defget ctx dp "readyState" (this) "complete")
    (defget ctx dp "URL" (this) (context-base ctx))
    (defget ctx dp "documentURI" (this) (context-base ctx))
    (defget ctx dp "characterSet" (this) "UTF-8")
    (defget ctx dp "charset" (this) "UTF-8")
    (defget ctx dp "compatMode" (this) "CSS1Compat")
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
      (wrap ctx (dom:get-element-by-id (n this) (jstr (arg a 0)))))
    (defmethod* ctx dp "querySelector" 1 (this a)
      (let ((sl (ignore-errors (css:parse-selector-list (jstr (arg a 0))))))
        (let ((m (and sl (qs-first (n this) sl)))) (if m (wrap ctx m) js:*null*))))
    (defmethod* ctx dp "querySelectorAll" 1 (this a)
      (let ((sl (ignore-errors (css:parse-selector-list (jstr (arg a 0))))))
        (make-collection ctx (lambda () (and sl (qs-all (n this) sl))))))
    (defmethod* ctx dp "getElementsByTagName" 1 (this a)
      (let ((node (n this)) (tag (string-downcase (jstr (arg a 0)))))
        (make-collection ctx (lambda () (remove node (dom:get-elements-by-tag-name node tag))))))
    (defmethod* ctx dp "getElementsByClassName" 1 (this a)
      (let ((node (n this)) (cls (jstr (arg a 0))))
        (make-collection ctx (lambda () (dom:get-elements-by-class-name node cls)))))
    (defmethod* ctx dp "createElement" 1 (this a)
      (let ((name (jstr (arg a 0))))
        (unless (valid-name-p name)
          (throw-dom ctx "InvalidCharacterError" 5 "invalid element name"))
        (new-node ctx this (h:make-element (string-downcase name)))))
    (defmethod* ctx dp "createElementNS" 2 (this a)
      (let* ((nsv (arg a 0)) (ns (if (nullish nsv) nil (jstr nsv))) (name (jstr (arg a 1)))
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
    (defmethod* ctx dp "createCDATASection" 1 (this a)
      ;; DOM §Document: a "]]>"-bearing string is a parse hazard.
      (let ((s (jstr (arg a 0))))
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
    (defget ctx dp "implementation" (this) (dom-implementation ctx))))

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

(defun dom-implementation (ctx)
  "The shared DOMImplementation object for this context."
  (or (proto ctx :implementation)
      (let ((impl (js:make-object :proto (js:eval-script (context-realm ctx) "Object.prototype"))))
        (setf (proto ctx :implementation) impl)
        (defmethod* ctx impl "hasFeature" 2 (this a) js:*true*)
        (defmethod* ctx impl "createDocument" 3 (this a)
          (let ((d (h:make-document)) (qn (arg a 1)) (dt (arg a 2)))
            (unless (nullish dt)
              (let ((dtn (node-of ctx dt)))
                (when dtn (h:dom-append d dtn)
                      (setf (gethash dtn (context-owner-docs ctx)) d))))  ; adopt the doctype
            (unless (nullish qn)
              (unless (valid-qname-p (jstr qn))
                (throw-dom ctx "InvalidCharacterError" 5 "invalid qualified name"))
              (let ((el (h:make-element (jstr qn))))
                (h:dom-append d el)
                (setf (gethash el (context-owner-docs ctx)) d)))
            (wrap ctx d)))
        (defmethod* ctx impl "createHTMLDocument" 1 (this a)
          (let* ((d (h:make-document)) (html (h:make-element "html"))
                 (head (h:make-element "head")) (title (h:make-element "title"))
                 (body (h:make-element "body")))
            (h:dom-append d html) (h:dom-append html head)
            (h:dom-append head title) (h:dom-append html body)
            (unless (js:js-undefined-p (arg a 0))
              (h:dom-append title (h:make-text (jstr (arg a 0)))))
            (wrap ctx d)))
        (defmethod* ctx impl "createDocumentType" 3 (this a)
          (let ((qname (jstr (arg a 0))))
            (unless (every (lambda (c) (>= (char-code c) #x21)) qname)
              (throw-dom ctx "InvalidCharacterError" 5 "invalid doctype name"))
            (unless (valid-qname-p qname)
              (throw-dom ctx "NamespaceError" 14 "malformed qualified name"))
            (wrap ctx (h:make-doctype qname (jstr (arg a 1)) (jstr (arg a 2))))))
        impl)))

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
