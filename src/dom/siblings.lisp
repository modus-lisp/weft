;;;; src/dom/siblings.lisp — nextElementSibling/previousElementSibling.
(in-package #:weft.dom)

(defun next-element-sibling (el)
  "Return the next sibling of EL that is an element node, or NIL."
  (let* ((parent (h:dnode-parent el))
         (children (and parent (h:dnode-children parent))))
    (when children
      (let ((pos (position el children)))
        (when pos
          (loop for i from (1+ pos) below (length children)
                for child = (aref children i)
                when (eq (h:dnode-kind child) :element)
                  do (return child)))))))

(defun previous-element-sibling (el)
  "Return the previous sibling of EL that is an element node, or NIL."
  (let* ((parent (h:dnode-parent el))
         (children (and parent (h:dnode-children parent))))
    (when children
      (let ((pos (position el children)))
        (when pos
          (loop for i from (1- pos) downto 0
                for child = (aref children i)
                when (eq (h:dnode-kind child) :element)
                  do (return child)))))))
