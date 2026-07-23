;;;; forms-labels.lisp — HTMLInputElement IDL surface: labels.
;;;;
;;;; Self-registering feature file (see core.lisp *element-proto-extensions*).
;;;; install-element-proto funcalls INSTALL-FORMS-labels with (ctx ep) after the
;;;; built-in accessors.  Fill the installer below; keep every helper prefixed
;;;; so this file never collides with a sibling feature file at load time.
(in-package #:weft.script)

(defun install-forms-labels (ctx ep)
  (declare (ignorable ctx ep))
  (macrolet ((n (this) `(require-node ctx ,this)))
    (flet ((labels-for (node)
             (let ((id (get-attr node "id"))
                   (root (tree-root node))
                   (result '()))
               (dolist (lbl (dom:get-elements-by-tag-name root "label"))
                 (let ((lbl-for (get-attr lbl "for")))
                   (cond
                     ;; Implicit: label is ancestor of node, no conflicting for
                     ((ancestor-p lbl node)
                      (when (or (null lbl-for)
                                (and id (string= lbl-for id)))
                        (pushnew lbl result :test #'eq)))
                     ;; Explicit: label's for= matches this node's id
                     ((and id lbl-for (string= lbl-for id))
                      (pushnew lbl result :test #'eq)))))
               (nreverse result))))
      (defgetset ctx ep "labels" (this)
        (let ((node (n this)))
          (if (string= (input-type node) "hidden")
              js:*null*
              (make-collection ctx (lambda () (labels-for node)) nil :nodelist)))
        (v)
        (declare (ignore v))))))

(register-element-proto-extension :labels #'install-forms-labels)
