;;;; src/dom/text-content.lisp — textContent (STUB).
(in-package #:weft.dom)

(defun text-content (node)
  "Return the concatenation of all descendant text-node data of NODE."
  (with-output-to-string (s)
    (labels ((walk (n)
               (when (eq (h:dnode-kind n) :text)
                 (write-string (h:dnode-data n) s))
               (loop for ch across (h:dnode-children n)
                     do (walk ch))))
      (walk node))))