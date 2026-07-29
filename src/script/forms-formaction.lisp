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

(defun form-action-resolve (ctx node)
  "Resolve formAction: attribute absent/empty -> document base URL;
   present -> resolve against base URL; unparseable -> raw value."
  (let ((raw (get-attr node "formaction")))
    (if (or (null raw) (zerop (length raw)))
        (let ((base (context-base ctx)))
          (if (string= base "") "" base))
        (resolve-url ctx raw))))

(defun form-method-enum (node)
  "Enumerated reflection for formMethod: valid values 'get'/'post',
   ASCII case-insensitive.  Missing default = 'get', invalid default = 'get'."
  (let ((raw (get-attr node "formmethod")))
    (if raw
        (let ((lower (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return) raw))))
          (if (member lower '("get" "post") :test #'string=) lower "get"))
        "get")))

(defun form-enctype-enum (node)
  "Enumerated reflection for formEnctype: valid values
   'application/x-www-form-urlencoded', 'multipart/form-data', 'text/plain'.
   Missing default = application/x-www-form-urlencoded,
   invalid default = application/x-www-form-urlencoded."
  (let ((raw (get-attr node "formenctype")))
    (if raw
        (let ((lower (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return) raw))))
          (if (member lower '("application/x-www-form-urlencoded" "multipart/form-data" "text/plain") :test #'string=)
              lower "application/x-www-form-urlencoded"))
        "application/x-www-form-urlencoded")))

(defun install-forms-formaction (ctx ep)
  (macrolet ((n (this) `(require-node ctx ,this)))
    ;; formAction: URL-reflecting attribute for button and input
    (defgetset-for ctx ep "button" "formAction" (this)
      (form-action-resolve ctx (n this))
      (v) (progn (set-attr (n this) "formaction" (jstr v))
                 (setf (context-dirty ctx) t)))
    (defgetset-for ctx ep "input" "formAction" (this)
      (form-action-resolve ctx (n this))
      (v) (progn (set-attr (n this) "formaction" (jstr v))
                 (setf (context-dirty ctx) t)))
    ;; formMethod: enumerated "get"/"post"
    (defgetset-for ctx ep "button" "formMethod" (this)
      (form-method-enum (n this))
      (v) (progn (set-attr (n this) "formmethod" (jstr v))
                 (setf (context-dirty ctx) t)))
    (defgetset-for ctx ep "input" "formMethod" (this)
      (form-method-enum (n this))
      (v) (progn (set-attr (n this) "formmethod" (jstr v))
                 (setf (context-dirty ctx) t)))
    ;; formEnctype: enumerated
    (defgetset-for ctx ep "button" "formEnctype" (this)
      (form-enctype-enum (n this))
      (v) (progn (set-attr (n this) "formenctype" (jstr v))
                 (setf (context-dirty ctx) t)))
    (defgetset-for ctx ep "input" "formEnctype" (this)
      (form-enctype-enum (n this))
      (v) (progn (set-attr (n this) "formenctype" (jstr v))
                 (setf (context-dirty ctx) t)))
    ;; formTarget: plain string reflection
    (defgetset-for ctx ep "button" "formTarget" (this)
      (or (get-attr (n this) "formtarget") "")
      (v) (progn (set-attr (n this) "formtarget" (jstr v))
                 (setf (context-dirty ctx) t)))
    (defgetset-for ctx ep "input" "formTarget" (this)
      (or (get-attr (n this) "formtarget") "")
      (v) (progn (set-attr (n this) "formtarget" (jstr v))
                 (setf (context-dirty ctx) t)))
    ;; formNoValidate: boolean reflection
    (defgetset-for ctx ep "button" "formNoValidate" (this)
      (jbool (dom:has-attribute (n this) "formnovalidate"))
      (v) (progn (if (js:js-truthy v) (set-attr (n this) "formnovalidate" "")
                     (remove-attr (n this) "formnovalidate"))
                 (setf (context-dirty ctx) t)))
    (defgetset-for ctx ep "input" "formNoValidate" (this)
      (jbool (dom:has-attribute (n this) "formnovalidate"))
      (v) (progn (if (js:js-truthy v) (set-attr (n this) "formnovalidate" "")
                     (remove-attr (n this) "formnovalidate"))
                 (setf (context-dirty ctx) t)))))

(register-element-proto-extension :formaction #'install-forms-formaction)