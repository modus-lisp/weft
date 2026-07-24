;;;; forms-textarea.lisp — HTMLTextAreaElement IDL (the <textarea>-only members).
;;;;
;;;; value / defaultValue / type live in the shared accessors in dom.lisp (they are
;;;; shared with <input>); this file adds the textarea-only surface, gated on the
;;;; tag, via the element-proto hook.
(in-package #:weft.script)

(defun ta-p (node) (string= (h:dnode-name node) "textarea"))
(defun ta-value (ctx node)
  "The textarea's API value: the stored current value, else its child text content."
  (multiple-value-bind (v p) (gethash node (context-input-values ctx))
    (if p v (child-text-content node))))

(defun install-forms-textarea (ctx ep)
  (macrolet ((n (this) `(require-node ctx ,this)))
    ;; textLength = length of the API value
    (defget ctx ep "textLength" (this)
      (let ((node (n this))) (if (ta-p node) (num (length (ta-value ctx node))) js:*undefined*)))
    ;; wrap reflects the content attribute as a string
    (defgetset ctx ep "wrap" (this)
      (let ((node (n this))) (if (ta-p node) (or (get-attr node "wrap") "") js:*undefined*))
      (v) (let ((node (n this))) (when (ta-p node)
                                   (set-attr node "wrap" (jstr v)) (setf (context-dirty ctx) t))))))

(register-element-proto-extension :textarea #'install-forms-textarea)
