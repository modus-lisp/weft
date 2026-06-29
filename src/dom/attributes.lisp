;;;; src/dom/attributes.lisp — getAttribute/hasAttribute.
(in-package #:weft.dom)
(defun get-attribute (el name) (cdr (assoc name (h:dnode-attrs el) :test #'string=)))
(defun has-attribute (el name) (and (assoc name (h:dnode-attrs el) :test #'string=) t))
