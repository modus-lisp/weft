;;;; forms-formaction.lisp — the form* submission-override attributes
;;;; (wave-10 swarm unit).
;;;;
;;;; Self-registering feature file (see core.lisp *element-proto-extensions*).
;;;;
;;;; What this file is for
;;;; ---------------------
;;;; A submit button may override its form's submission parameters:
;;;; `formAction', `formMethod', `formEnctype', `formTarget', `formNoValidate'.
;;;; This unit is about REFLECTION, not submission — nothing here navigates, and
;;;; that is precisely why it is reachable when form-submission-0/ (137 subtests
;;;; behind real navigation) is not.  Do not try to implement submission here.
;;;;
;;;; The one rule that carries formAction_document_address.html:
;;;;
;;;;   `formAction' is a URL-reflecting IDL attribute.  The GETTER parses the
;;;;   content attribute RELATIVE TO THE DOCUMENT'S BASE URL and returns the
;;;;   resulting ABSOLUTE url — so formaction="" returns the document's own
;;;;   address, not the empty string, and formaction="x" returns a full URL.
;;;;   If the attribute is ABSENT the getter returns the document address too.
;;;;   If it is present but does not parse, the getter returns the attribute
;;;;   value verbatim.  The SETTER just stores the string unchanged.
;;;;
;;;; `formMethod' and `formEnctype' are ENUMERATED reflections: an unrecognised
;;;; (or absent) value reads back as the default ("get" / the urlencoded type),
;;;; matching is ASCII-case-insensitive, and the getter returns the canonical
;;;; lowercase keyword rather than what the author wrote.  `formTarget' is a
;;;; plain string reflection.  `formNoValidate' is a boolean (presence) one.
;;;;
;;;; disabled-elements-01.html is the shared half: which controls the `disabled'
;;;; state actually applies to, and that a disabled control is inert for the
;;;; purposes the test checks.  It is grouped here because it is the same
;;;; "attributes common to form controls" directory and the same reflection
;;;; machinery, not because it is the same feature.
;;;;
;;;; weft already has a WHATWG URL parser — use it (weft.url), do not
;;;; hand-splice strings.  The document's base URL is the thing to resolve
;;;; against; find how the existing code reaches it rather than inventing a
;;;; constant.
;;;;
;;;; Take over the accessors with the `-for' variants, as the sibling feature
;;;; files do; the previous accessor stays reachable through PREVIOUS-ACCESSOR,
;;;; so this file never has to edit shared core.
(in-package #:weft.script)

(defun install-forms-formaction (ctx ep)
  (declare (ignorable ctx ep))
  nil)

(register-element-proto-extension :formaction #'install-forms-formaction)
