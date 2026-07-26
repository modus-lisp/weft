;;;; forms-label.lisp — HTMLLabelElement IDL surface: htmlFor, control, form.
;;;;
;;;; Self-registering feature file (see core.lisp *element-proto-extensions*).
;;;; install-element-proto funcalls INSTALL-FORMS-label with (ctx ep) after the
;;;; built-in accessors.  Uses shared helpers from forms-labels.lisp (loaded first).
(in-package #:weft.script)

(defun label-label-form (ctx node)
  "Return the form owner of the label's control, or NIL.  Goes through the
   canonical ELEMENT-FORM-OWNER so the label agrees with the control's own
   .form — including the parser form pointer, which is the only thing that
   associates a control with a <form> that self-closed inside a table."
  (let ((control (labels-label-control node)))
    (when control
      (element-form-owner ctx control))))

(defun install-forms-label (ctx ep)
  (declare (ignorable ctx ep))
  (macrolet ((n (this) `(require-node ctx ,this)))
    ;; control — the labeled control element
    (defget-for ctx ep "label" "control" (this)
      (let ((node (n this)))
        (if (string= (h:dnode-name node) "label")
            (let ((ctl (labels-label-control node)))
              (if ctl (wrap ctx ctl) js:*null*))
            js:*undefined*)))
    ;; form — the control's form owner
    (defget-for ctx ep "label" "form" (this)
      (let ((node (n this)))
        (if (string= (h:dnode-name node) "label")
            (let ((form (label-label-form ctx node)))
              (if form (wrap ctx form) js:*null*))
            js:*undefined*)))))

(register-element-proto-extension :label #'install-forms-label)