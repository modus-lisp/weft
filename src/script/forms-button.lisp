;;;; forms-button.lisp — HTMLElement IDL surface for <button> (WPT swarm unit).
;;;; Self-registering into the element-proto hook (see core.lisp).  The installer
;;;; runs on the shared element prototype; each accessor must gate on the node's
;;;; tag ((string= (h:dnode-name (n this)) "button")) so it only acts on <button>.
(in-package #:weft.script)

(defun button-type (node)
  "Return the canonical type for a <button> node."
  (let ((raw (get-attr node "type")))
    (if raw
        (let ((v (string-downcase raw)))
          (if (member v '("submit" "reset" "button") :test #'equal) v "submit"))
        "submit")))

(defun install-forms-button (ctx ep)
  (declare (ignorable ctx ep))
  (macrolet ((n (this) `(require-node ctx ,this)))
    ;; willValidate — true for submit buttons, false for reset/button
    (defget ctx ep "willValidate" (this)
      (let ((node (n this)))
        (if (string= (h:dnode-name node) "button")
            (let ((type (button-type node)))
              (if (string= type "submit") js:*true* js:*false*))
            js:*undefined*)))))

(register-element-proto-extension :button #'install-forms-button)
