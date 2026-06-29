;;;; src/dom/by-tag.lisp — getElementsByTagName.
(in-package #:weft.dom)

(defun get-elements-by-tag-name (root tag)
  "Return a list (tree order) of element descendants of ROOT whose name = TAG.
TAG \"*\" matches every element."
  (let ((result '()))
    (labels ((walk (node)
               (when (and (eq (h:dnode-kind node) :element)
                          (or (string= tag "*")
                              (string= (h:dnode-name node) tag)))
                 (push node result))
               (loop for ch across (h:dnode-children node)
                     do (walk ch))))
      (walk root)
      (nreverse result))))