;;;; forms-valueasnumber.lisp — HTMLInputElement IDL surface: valueasnumber.
;;;;
;;;; Self-registering feature file (see core.lisp *element-proto-extensions*).
;;;; install-element-proto funcalls INSTALL-FORMS-valueasnumber with (ctx ep) after the
;;;; built-in accessors.  Fill the installer below; keep every helper prefixed
;;;; so this file never collides with a sibling feature file at load time.
(in-package #:weft.script)

(defun install-forms-valueasnumber (ctx ep)
  (declare (ignorable ctx ep))
  (macrolet ((n (this) `(require-node ctx ,this)))
    ;; TODO (swarm): install the valueasnumber IDL surface with defgetset / defmethod*.
    (macrolet ((%noop () nil)) (%noop))))

(register-element-proto-extension :valueasnumber #'install-forms-valueasnumber)
