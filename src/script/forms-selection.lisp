;;;; forms-selection.lisp — HTMLInputElement IDL surface: selection.
;;;;
;;;; Self-registering feature file (see core.lisp *element-proto-extensions*).
;;;; install-element-proto funcalls INSTALL-FORMS-selection with (ctx ep) after the
;;;; built-in accessors.  Fill the installer below; keep every helper prefixed
;;;; so this file never collides with a sibling feature file at load time.
(in-package #:weft.script)

(defun install-forms-selection (ctx ep)
  (declare (ignorable ctx ep))
  (macrolet ((n (this) `(require-node ctx ,this)))
    ;; TODO (swarm): install the selection IDL surface with defgetset / defmethod*.
    (macrolet ((%noop () nil)) (%noop))))

(register-element-proto-extension :selection #'install-forms-selection)
