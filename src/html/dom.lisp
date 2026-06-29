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
  (children (make-array 0 :adjustable t :fill-pointer 0))
  parent)

(defun dom-append (parent child)
  (setf (dnode-parent child) parent)
  (vector-push-extend child (dnode-children parent))
  child)

(defun dom-last-child (node)
  (let ((ch (dnode-children node)))
    (when (plusp (length ch)) (aref ch (1- (length ch))))))

(defun make-document () (%dnode :kind :document))
(defun make-element (name &optional attrs (ns :html))
  (%dnode :kind :element :name name :attrs attrs :namespace ns))
(defun make-text (data) (%dnode :kind :text :data data))
(defun make-comment (data) (%dnode :kind :comment :data data))

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
      (:text (format out "| ~a~s~%" pad (or (dnode-data node) "")))
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
