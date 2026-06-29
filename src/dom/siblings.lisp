;;;; src/dom/siblings.lisp — nextElementSibling/previousElementSibling.
(in-package #:weft.dom)
(defun next-element-sibling (el)
  (let* ((p (h:dnode-parent el)) (ch (and p (h:dnode-children p))) (i (and ch (position el ch))))
    (when i (loop for j from (1+ i) below (length ch)
                  when (eq (h:dnode-kind (aref ch j)) :element) return (aref ch j)))))
(defun previous-element-sibling (el)
  (let* ((p (h:dnode-parent el)) (ch (and p (h:dnode-children p))) (i (and ch (position el ch))))
    (when i (loop for j from (1- i) downto 0
                  when (eq (h:dnode-kind (aref ch j)) :element) return (aref ch j)))))
