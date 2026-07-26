;;;; forms-valuemode.lisp — the <input> VALUE MODE (wave-9 swarm unit).
;;;;
;;;; Self-registering feature file (see core.lisp *element-proto-extensions*).
;;;; Loads BEFORE forms-constraints so the constraint code can call the
;;;; sanitizer defined here as an ordinary top-level function.
;;;;
;;;; What this file is for
;;;; ---------------------
;;;; HTML gives every input type one of four VALUE MODES — value, default,
;;;; default/on, filename — and the mode decides what the `value' IDL attribute
;;;; reads and writes, whether there is a dirty value flag, and what a clone
;;;; keeps.  Layered on top is the VALUE SANITIZATION ALGORITHM: a per-type
;;;; rewrite of the stored string (text strips CR/LF, url and email also strip
;;;; leading/trailing whitespace, the temporal and numeric types discard
;;;; anything they cannot parse, color normalises, and so on).
;;;;
;;;; The sanitizer must run when the value is set AND AGAIN WHENEVER THE TYPE
;;;; ATTRIBUTE CHANGES — that second half is the whole of type-change-state.html
;;;; (380 subtests: the full old-type x new-type cross product).
;;;;
;;;; Take over the accessors with the `-for' variants, exactly as the sibling
;;;; feature files do; the previous accessor from dom.lisp remains reachable
;;;; through PREVIOUS-ACCESSOR, so this file never has to edit shared core.
(in-package #:weft.script)

(defun install-forms-valuemode (ctx ep)
  (declare (ignorable ctx ep))
  nil)

(register-element-proto-extension :valuemode #'install-forms-valuemode)
