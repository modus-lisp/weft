;;;; src/dom/by-id.lisp — getElementById
(in-package #:weft.dom)

(defun get-element-by-id (root id)
  "Return the first element in tree order under ROOT whose id attribute = ID, or NIL."
  ;; Check root first (preorder: visit node before children)
  (when (and (eq (h:dnode-kind root) :element)
             (let ((id-val (cdr (assoc "id" (h:dnode-attrs root) :test #'string=))))
               (and id-val (string= id id-val))))
    (return-from get-element-by-id root))
  ;; Recurse children left-to-right
  (loop for ch across (h:dnode-children root)
        for found = (get-element-by-id ch id)
        when found do (return-from get-element-by-id found))
  nil)