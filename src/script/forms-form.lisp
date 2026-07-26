;;;; forms-form.lisp — HTMLFormElement IDL (WPT swarm unit).
;;;;
;;;; Pre-wired stub.  The <form> element's own surface: elements (an
;;;; HTMLFormControlsCollection, live, with the named/indexed getters and the
;;;; past-names behaviour), length, the named and indexed getters on the form
;;;; itself, action/method/enctype/target/name/noValidate reflections,
;;;; acceptCharset, autocomplete (an enumerated reflection whose only valid
;;;; values are "on" and "off", with "on" as both the missing and invalid
;;;; default — and which each control's own autocomplete resolves against),
;;;; checkValidity/reportValidity, and requestSubmit(submitter) with its
;;;; argument validation.
;;;;
;;;; Form-owner lookup already lives in ONE place — ELEMENT-FORM-OWNER in
;;;; src/script/dom.lisp — and form.elements must agree with control.form in
;;;; both directions, so build `elements' on top of it rather than on a fresh
;;;; descendant walk.
(in-package #:weft.script)

(defun install-forms-form (ctx ep)
  (declare (ignorable ctx ep))
  nil)

(register-element-proto-extension :form #'install-forms-form)
