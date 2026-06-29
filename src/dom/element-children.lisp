;;;; src/dom/element-children.lisp — firstElementChild/lastElementChild/childElementCount.
(in-package #:weft.dom)

(defun first-element-child (el)
  "Return the first child of EL that has kind :ELEMENT, or NIL."
  (let ((children (dnode-children el)))
    (dotimes (i (length children) nil)
      (let ((child (aref children i)))
        (when (eq (dnode-kind child) :element)
          (return child))))))

(defun last-element-child (el)
  "Return the last child of EL that has kind :ELEMENT, or NIL."
  (let ((children (dnode-children el)))
    (dotimes (i (length children) nil)
      (let* ((idx (- (length children) 1 i))
             (child (aref children idx)))
        (when (eq (dnode-kind child) :element)
          (return child))))))

(defun child-element-count (el)
  "Return the number of child nodes of EL that have kind :ELEMENT."
  (let ((children (dnode-children el))
        (count 0))
    (dotimes (i (length children) count)
      (when (eq (dnode-kind (aref children i)) :element)
        (incf count)))))
