;;;; src/html/dom.lisp — minimal DOM node kernel + html5lib tree serializer.
;;;;
;;;; The serial kernel the tree builder constructs and the (future) DOM interface
;;;; surface binds to.  Nodes: document / doctype / element / text / comment.
(in-package #:weft.html)

(defstruct (dnode (:constructor %dnode))
  kind                                  ; :document :doctype :element :text :comment
  name                                  ; element tag (lowercased) or doctype name
  (attrs nil)                           ; alist (name . value)
  data                                  ; text / comment string
  public system                        ; doctype ids
  (namespace :html)                     ; :html :svg :math
  (mode :no-quirks)                     ; document only: :no-quirks :limited-quirks :quirks
  (children (make-array 0 :adjustable t :fill-pointer 0))
  parent)

(defun dom-append (parent child)
  (setf (dnode-parent child) parent)
  (vector-push-extend child (dnode-children parent))
  child)

(defun dom-last-child (node)
  (let ((ch (dnode-children node)))
    (when (plusp (length ch)) (aref ch (1- (length ch))))))

(defun dom-insert-before (parent node ref)
  "Insert NODE into PARENT's children immediately before REF (a current child).
If REF is NIL, append.  Used for foster parenting."
  (if (null ref)
      (dom-append parent node)
      (let* ((ch (dnode-children parent)) (idx (position ref ch)))
        (setf (dnode-parent node) parent)
        (vector-push-extend node ch)                 ; grow by one
        (loop for k from (1- (length ch)) above idx do (setf (aref ch k) (aref ch (1- k))))
        (setf (aref ch idx) node)
        node)))

(defun dom-remove (node)
  "Detach NODE from its parent's children."
  (let ((parent (dnode-parent node)))
    (when parent
      (let* ((ch (dnode-children parent)) (idx (position node ch)))
        (when idx
          (loop for k from idx below (1- (length ch)) do (setf (aref ch k) (aref ch (1+ k))))
          (decf (fill-pointer ch))))
      (setf (dnode-parent node) nil)))
  node)

(defun dom-move-children (from to)
  "Move all children of FROM to TO (appended in order)."
  (loop for c across (copy-seq (dnode-children from)) do (dom-remove c) (dom-append to c)))

(defun dom-prev-sibling (parent ref)
  "The child of PARENT immediately before REF, or the last child if REF is NIL."
  (let* ((ch (dnode-children parent)) (idx (and ref (position ref ch))))
    (cond ((null ref) (when (plusp (length ch)) (aref ch (1- (length ch)))))
          ((and idx (plusp idx)) (aref ch (1- idx))))))

(defun make-document () (%dnode :kind :document))
(defun make-element (name &optional attrs (ns :html))
  (%dnode :kind :element :name name :attrs attrs :namespace ns))
(defun make-text (data) (%dnode :kind :text :data data))
(defun make-comment (data) (%dnode :kind :comment :data data))
(defun make-cdata (data) (%dnode :kind :cdata :data data))
(defun make-processing-instruction (target data)
  (%dnode :kind :processing-instruction :name target :data data))
(defun make-fragment () (%dnode :kind :fragment))
(defun make-doctype (name &optional public system)
  (%dnode :kind :doctype :name name :public public :system system))

;;; ---- html5lib tree serialization --------------------------------------
;;; Lines: "| " + 2*depth spaces + content; attributes sorted, one per line at
;;; depth+1; text quoted; comments "<!-- data -->"; SVG/MathML get a ns prefix.

(defun %ns-prefix (ns) (ecase ns (:html "") (:svg "svg ") (:math "math ")))

(defun serialize-node (node depth out)
  (let ((pad (make-string (* 2 depth) :initial-element #\Space)))
    (ecase (dnode-kind node)
      (:doctype
       (format out "| ~a<!DOCTYPE ~a" pad (or (dnode-name node) ""))
       (when (or (dnode-public node) (dnode-system node))
         (format out " \"~a\" \"~a\"" (or (dnode-public node) "") (or (dnode-system node) "")))
       (format out ">~%"))
      (:comment (format out "| ~a<!-- ~a -->~%" pad (or (dnode-data node) "")))
      (:cdata (format out "| ~a<![CDATA[~a]]>~%" pad (or (dnode-data node) "")))
      (:processing-instruction
       (format out "| ~a<?~a ~a>~%" pad (or (dnode-name node) "") (or (dnode-data node) "")))
      (:fragment
       (loop for ch across (dnode-children node) do (serialize-node ch depth out)))
      (:text (format out "| ~a\"~a\"~%" pad (or (dnode-data node) "")))   ; raw (no escaping)
      (:element
       (format out "| ~a<~a~a>~%" pad (%ns-prefix (dnode-namespace node)) (dnode-name node))
       (dolist (a (sort (copy-alist (dnode-attrs node)) #'string< :key #'car))
         (format out "| ~a~a=~s~%" (make-string (* 2 (1+ depth)) :initial-element #\Space)
                 (car a) (cdr a)))
       (loop for ch across (dnode-children node) do (serialize-node ch (1+ depth) out))))))

(defun serialize-tree (document)
  "Serialize DOCUMENT's subtree to the html5lib tree-construction text format."
  (with-output-to-string (out)
    (loop for ch across (dnode-children document) do (serialize-node ch 0 out))))

;;; ---- HTML fragment serialization (WHATWG HTML §13.3) -------------------
;;; Produces real HTML markup (for Element.innerHTML/outerHTML,
;;; insertAdjacentHTML round-trips, and the XMLSerializer's HTML path).

(defparameter *html-void*
  '("area" "base" "br" "col" "embed" "hr" "img" "input" "keygen" "link"
    "meta" "param" "source" "track" "wbr"))
;; Elements whose text-child contents are serialized literally (no escaping).
(defparameter *html-rawtext*
  '("style" "script" "xmp" "iframe" "noembed" "noframes" "plaintext" "noscript"))

(defun %escape-html (s attribute-p)
  "HTML text/attribute-value escaping (§13.3): & \\u00a0 always; in text also
< and >; in an attribute value also \"."
  (with-output-to-string (o)
    (loop for c across s do
      (cond ((char= c #\&) (write-string "&amp;" o))
            ((char= c #\No-Break_Space) (write-string "&nbsp;" o))
            ((and attribute-p (char= c #\")) (write-string "&quot;" o))
            ((and (not attribute-p) (char= c #\<)) (write-string "&lt;" o))
            ((and (not attribute-p) (char= c #\>)) (write-string "&gt;" o))
            (t (write-char c o))))))

(defun %attr-qualified-name (a)
  "The serialized attribute name.  A namespaced attr may carry a stored
prefix (\"xml:lang\", \"xlink:href\", \"xmlns:x\") in its car already; use it as-is."
  (car a))

(defun serialize-html-node (node out)
  "Serialize NODE (an element/text/comment/…) as HTML markup into stream OUT,
including the node itself and its descendants (the 'outer' form)."
  (ecase (dnode-kind node)
    (:element
     (let* ((ns (dnode-namespace node))
            (tag (dnode-name node)))
       (write-char #\< out) (write-string tag out)
       (dolist (a (dnode-attrs node))
         (write-char #\Space out)
         (write-string (%attr-qualified-name a) out)
         (write-string "=\"" out)
         (write-string (%escape-html (or (cdr a) "") t) out)
         (write-char #\" out))
       (write-char #\> out)
       (unless (and (eq ns :html) (member tag *html-void* :test #'string=))
         (serialize-html-children node out)
         (write-string "</" out) (write-string tag out) (write-char #\> out))))
    (:text
     (let ((parent (dnode-parent node)))
       (if (and parent (eq (dnode-kind parent) :element)
                (eq (dnode-namespace parent) :html)
                (member (dnode-name parent) *html-rawtext* :test #'string=))
           (write-string (or (dnode-data node) "") out)
           (write-string (%escape-html (or (dnode-data node) "") nil) out))))
    (:comment
     (write-string "<!--" out) (write-string (or (dnode-data node) "") out)
     (write-string "-->" out))
    (:cdata
     (write-string "<![CDATA[" out) (write-string (or (dnode-data node) "") out)
     (write-string "]]>" out))
    (:processing-instruction
     (write-string "<?" out) (write-string (or (dnode-name node) "") out)
     (write-char #\Space out) (write-string (or (dnode-data node) "") out)
     (write-char #\> out))
    (:doctype
     (write-string "<!DOCTYPE " out) (write-string (or (dnode-name node) "") out)
     (write-char #\> out))
    (:fragment (serialize-html-children node out))
    (:document (serialize-html-children node out))))

(defun serialize-html-children (node out)
  (loop for ch across (dnode-children node) do (serialize-html-node ch out)))

(defun serialize-html-fragment (node)
  "The HTML serialization of NODE's *children* (Element.innerHTML getter)."
  (with-output-to-string (out) (serialize-html-children node out)))

(defun serialize-html-outer (node)
  "The HTML serialization of NODE itself (Element.outerHTML getter)."
  (with-output-to-string (out) (serialize-html-node node out)))
