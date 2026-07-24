;;;; forms-select.lisp — HTMLElement IDL surface for <select> (WPT swarm unit).
;;;; Self-registering into the element-proto hook (see core.lisp).  The installer
;;;; runs on the shared element prototype; each accessor must gate on the node's
;;;; tag ((string= (h:dnode-name (n this)) "select")) so it only acts on <select>.
(in-package #:weft.script)

(defun install-forms-select (ctx ep)
  (declare (ignorable ctx ep))
  (macrolet ((n (this) `(require-node ctx ,this)))
    ;; TODO (swarm): install the <select> IDL surface (gate each accessor on the tag).
    (macrolet ((%noop () nil)) (%noop))))

(register-element-proto-extension :select #'install-forms-select)
