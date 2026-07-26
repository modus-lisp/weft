;;;; forms-constraints.lisp — the constraint validation API (WPT swarm unit).
;;;;
;;;; Pre-wired stub.  Unlike the other forms-*.lisp files this one is NOT about
;;;; a single element: the constraint validation API is defined once and applies
;;;; to every submittable/listed element — input, textarea, select, button,
;;;; fieldset, output — and to <form> itself.  It is the natural home for the
;;;; parts that are genuinely shared:
;;;;
;;;;   - "barred from constraint validation": which elements are candidates at
;;;;     all (a datalist ancestor, disabled, readonly, type=hidden/reset/button,
;;;;     an output element, ...) — this is what willValidate reports.
;;;;   - the validity flags themselves (valueMissing, typeMismatch,
;;;;     patternMismatch, tooLong, tooShort, rangeUnderflow, rangeOverflow,
;;;;     stepMismatch, badInput, customError) and `valid' = none of them set.
;;;;   - static validation of a form: collect the unhandled invalid controls.
;;;;   - checkValidity / reportValidity on both a control and a form, and the
;;;;     `invalid' event they fire.
;;;;
;;;; src/script/forms-validity.lisp holds the per-input ValidityState work that
;;;; already exists; move shared pieces HERE rather than growing a second copy
;;;; there.  Registered after it, so anything installed here runs later.
(in-package #:weft.script)

(defun install-forms-constraints (ctx ep)
  (declare (ignorable ctx ep))
  nil)

(register-element-proto-extension :constraints #'install-forms-constraints)
