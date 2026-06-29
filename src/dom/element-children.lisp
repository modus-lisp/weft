;;;; src/dom/element-children.lisp — firstElementChild/lastElementChild/childElementCount.
(in-package #:weft.dom)
(defun first-element-child (el)
  (loop for c across (h:dnode-children el) when (eq (h:dnode-kind c) :element) return c))
(defun last-element-child (el)
  (let (r) (loop for c across (h:dnode-children el) when (eq (h:dnode-kind c) :element) do (setf r c)) r))
(defun child-element-count (el)
  (count :element (h:dnode-children el) :key #'h:dnode-kind))
