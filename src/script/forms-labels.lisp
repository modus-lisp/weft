;;;; forms-labels.lisp — HTMLInputElement IDL surface: labels.
;;;;
;;;; Self-registering feature file (see core.lisp *element-proto-extensions*).
;;;; install-element-proto funcalls INSTALL-FORMS-labels with (ctx ep) after the
;;;; built-in accessors.  Fill the installer below; keep every helper prefixed
;;;; so this file never collides with a sibling feature file at load time.
(in-package #:weft.script)

;;; Labelable elements per HTML spec: button, input (except type=hidden),
;;; meter, output, progress, select, textarea.
;;; Shared with forms-label.lisp (loaded after this file).
(defparameter +labelable-tags+
  '("button" "input" "meter" "output" "progress" "select" "textarea"))

(defun labels-labelable-p (node)
  "True if NODE is a labelable element (button, input (not hidden), meter, output,
   progress, select, or textarea)."
  (and (eq (h:dnode-kind node) :element)
       (member (h:dnode-name node) +labelable-tags+ :test #'string=)
       (or (not (string= (h:dnode-name node) "input"))
           (not (string= (input-type node) "hidden")))))

(defun labels-first-labelable-descendant (node)
  "Return the first labelable element descendant of NODE in tree order, or NIL."
  (let ((children (h:dnode-children node)))
    (loop for i from 0 below (length children)
          do (let ((child (aref children i)))
               (when (labels-labelable-p child)
                 (return-from labels-first-labelable-descendant child))
               (let ((found (labels-first-labelable-descendant child)))
                 (when found (return found)))))))

(defun labels-label-control (node)
  "Return the labeled control of a <label> element, or NIL.
   With a non-empty 'for' attribute, it is the element whose id matches (only if
   labelable).  Without 'for' (or with empty for), it is the first labelable
   descendant."
  (let ((for-val (get-attr node "for")))
    (cond
      ((and for-val (plusp (length for-val)))
       (let* ((root (tree-root node))
              (target (dom:get-element-by-id root for-val)))
         (and target (labels-labelable-p target) target)))
      ((not for-val)
       (labels-first-labelable-descendant node))
      (t nil))))

(defun install-forms-labels (ctx ep)
  (declare (ignorable ctx ep))
  (let ((labels-cache (make-hash-table :test 'eq)))
    (macrolet ((n (this) `(require-node ctx ,this)))
      (flet ((labels-for (node)
               (let ((root (tree-root node))
                     (result '()))
                 (dolist (lbl (dom:get-elements-by-tag-name root "label"))
                   (when (eq (labels-label-control lbl) node)
                     (pushnew lbl result :test #'eq)))
                 (nreverse result))))
        (defgetset ctx ep "labels" (this)
          (let* ((node (n this))
                 (hidden-p (and (string= (h:dnode-name node) "input")
                                (string= (input-type node) "hidden"))))
            (if hidden-p
                js:*null*
                (if (labels-labelable-p node)
                    (or (gethash node labels-cache)
                        (let ((coll (make-collection ctx (lambda () (labels-for node)) nil :nodelist)))
                          (setf (gethash node labels-cache) coll)))
                    js:*undefined*)))
          (v)
          (declare (ignore v)))))))

(register-element-proto-extension :labels #'install-forms-labels)