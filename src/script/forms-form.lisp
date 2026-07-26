;;;; forms-form.lisp — HTMLFormElement IDL (WPT swarm unit).
;;;;
;;;; Implements the form element's IDL surface: autocomplete (enumerated
;;;; "on"/"off"), action (URL-resolved), requestSubmit(submitter), and the
;;;; named/indexed getter for form controls.
(in-package #:weft.script)

;;; --- helpers ---

(defun form-autocomplete-value (node)
  "Return the form's autocomplete IDL value per spec: 'on' is the missing
   value default AND the invalid value default; only 'off' yields 'off'."
  (let ((raw (get-attr node "autocomplete")))
    (if (and raw (string= (string-downcase raw) "off"))
        "off" "on")))

(defun form-action-value (ctx node)
  "Return the form's action IDL value: resolved against the document base URL.
   If the attribute is missing, return the document's URL."
  (let ((raw (get-attr node "action")))
    (if (or (null raw) (zerop (length raw)))
        ;; Missing or empty-string action returns document URL
        (let ((base (context-base ctx)))
          (if (string= base "") "" base))
        ;; Resolve relative to the document's base URL
        (resolve-url ctx raw))))

(defun form-method-value (node)
  "Enumerated attribute: valid values 'get' and 'post', ASCII case-insensitive.
   Missing default = 'get', invalid default = 'get'."
  (let ((raw (get-attr node "method")))
    (if raw
        (let ((lower (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return) raw))))
          (if (member lower '("get" "post") :test #'string=) lower "get"))
        "get")))

(defun form-enctype-value (node)
  "Enumerated attribute: valid values 'application/x-www-form-urlencoded',
   'multipart/form-data', 'text/plain'.  Missing default = application/x-www-form-urlencoded,
   invalid default = application/x-www-form-urlencoded."
  (let ((raw (get-attr node "enctype")))
    (if raw
        (let ((lower (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return) raw))))
          (if (member lower '("application/x-www-form-urlencoded" "multipart/form-data" "text/plain") :test #'string=)
              lower "application/x-www-form-urlencoded"))
        "application/x-www-form-urlencoded")))

(defun form-submittable-button-p (ctx node)
  "Return T if NODE is a submittable button (button[type=submit],
   input[type=submit], input[type=image]) owned by its form."
  (let ((tag (h:dnode-name node)))
    (and (member tag '("button" "input") :test #'string=)
         (or (and (string= tag "button")
                  (string= (button-type node) "submit"))
             (and (string= tag "input")
                  (member (input-type node) '("submit" "image") :test #'string=))))))

;;; ---- form-controls (exclude input type=image) -----------------------------
(defun form-controls-for-form (form)
  "Returns all listed elements whose form owner is FORM, in tree order.
   Excludes input[type=image] per spec.
   Includes elements outside the form with form=FORM-ID."
  (let* ((root (tree-root form))
         (all (if (eq (h:dnode-kind root) :document)
                  (dom:get-elements-by-tag-name root "*")
                  (form-controls form)))
         (form-id (get-attr form "id")))
    (remove-if-not
     (lambda (n)
       (and (member (h:dnode-name n) +form-control-tags+ :test #'string=)
            (not (and (string= (h:dnode-name n) "input")
                      (string= (or (get-attr n "type") "text") "image")))
            (or (ancestor-or-self-p form n)
                (and form-id (equal (get-attr n "form") form-id)))))
     all)))

;;; ---- form named-control helpers -------------------------------------------
(defun form-named-control (form name)
  "Find a control by name or id in the form's elements collection."
  (find-if (lambda (n) (or (equal (get-attr n "name") name)
                           (equal (get-attr n "id") name)))
           (form-controls-for-form form)))

(defun form-named-controls (form name)
  "Find all controls with the given name/id in the form's elements collection."
  (remove-if-not (lambda (n) (or (equal (get-attr n "name") name)
                                 (equal (get-attr n "id") name)))
                 (form-controls-for-form form)))

;;; ---- submit button check --------------------------------------------------
(defun submit-button-p (node)
  "True if NODE is a submit button: button[type=submit], input[type=submit],
   input[type=image], or button with no type attribute (default submit)."
  (and (eq (h:dnode-kind node) :element)
       (let ((tag (h:dnode-name node)))
         (or (and (string= tag "button")
                  (string= (button-type node) "submit"))
             (and (string= tag "input")
                  (let ((type-val (get-attr node "type")))
                    (or (null type-val) (string= type-val "submit")
                        (string= type-val "image"))))))))

;;; ---- form elements collection (SameObject) --------------------------------
(defun get-form-elements (ctx wrapper node)
  "Get or create the memoized form.elements HTMLCollection (SameObject)."
  (let ((existing (js:js-get wrapper "__weft_form_elements")))
    (if (js:js-object-p existing)
        existing
        (let ((coll (make-collection ctx
                      (lambda () (form-controls-for-form node))
                      (lambda (name)
                        (let ((matches (form-named-controls node name)))
                          (cond ((null matches) nil)
                                ((null (cdr matches)) (car matches))
                                (t (car matches))))))))
          (js:put wrapper "__weft_form_elements" coll
                  :enumerable nil :configurable t)
          coll))))

;;; ---- form named getter (past names map) -----------------------------------
(defvar *form-past-names* (make-hash-table :test 'eq)
  "Hash table: form node -> alist of (past-name . element).")

(defun form-past-names (form)
  (gethash form *form-past-names*))

(defun set-form-past-name (form name element)
  (let ((alist (gethash form *form-past-names*)))
    (unless (assoc name alist :test #'string=)
      (push (cons name element) alist))
    (setf (gethash form *form-past-names*) alist)))

(defun form-clear-past-names (form element)
  (let ((alist (gethash form *form-past-names*)))
    (setf (gethash form *form-past-names*)
          (remove element alist :key #'cdr :test #'eq))))

(defun form-lookup-past-name (form name)
  (let ((alist (gethash form *form-past-names*)))
    (cdr (assoc name alist :test #'string=))))

;;; ---- requestSubmit implementation -----------------------------------------
(defun form-check-validity (ctx form)
  "Interactive form validation.  Returns T if valid, NIL if invalid.
   Fires 'invalid' events on invalid controls."
  (let ((any-invalid nil))
    (dolist (c (submittable-controls-of-form form))
      (multiple-value-bind (valid-p fired)
          (constraints-check-valid ctx c)
        (declare (ignore fired))
        (unless valid-p (setf any-invalid t))))
    (not any-invalid)))

(defun form-fire-submit (ctx form submitter)
  "Fire the 'submit' event on FORM.  Since we have no navigation,
   this is as far as we go — we do not navigate."
  (let* ((sev (make-event-object ctx "submit" nil))
         (se (evt-of ctx sev)))
    (setf (evt-bubbles se) t (evt-cancelable se) t)
    (dispatch-event ctx form sev)))

(defun form-request-submit (ctx wrapper form submitter-arg)
  "Implement requestSubmit(submitter)."
  (if (js:js-undefined-p submitter-arg)
      ;; No argument: submit as if the form itself were the submitter
      (progn
        (unless (form-check-validity ctx form)
          (return-from form-request-submit js:*undefined*))
        (form-fire-submit ctx form nil))
      ;; With argument
      (let ((submitter-node (node-of ctx submitter-arg)))
        (unless submitter-node
          (js:js-throw (js:make-native-error "TypeError" "submitter is not an element")))
        (unless (submit-button-p submitter-node)
          (js:js-throw (js:make-native-error "TypeError" "submitter is not a submit button")))
        (let ((owner (element-form-owner ctx submitter-node)))
          (unless (eq owner form)
            (throw-dom ctx "NotFoundError" 8 "submitter's form owner is not this form")))
        (unless (form-check-validity ctx form)
          (return-from form-request-submit js:*undefined*))
        (form-fire-submit ctx form submitter-node)))
  js:*undefined*)

;;; Install form-specific accessors and methods.
(defun install-forms-form (ctx ep)
  (macrolet ((n (this) `(require-node ctx ,this)))
    ;; autocomplete: enumerated attribute "on"/"off", default "on"
    (defgetset-for ctx ep "form" "autocomplete" (this)
      (form-autocomplete-value (n this))
      (v) (progn (set-attr (n this) "autocomplete" (jstr v))
                 (setf (context-dirty ctx) t)))

    ;; action: URL resolution
    (defgetset-for ctx ep "form" "action" (this)
      (form-action-value ctx (n this))
      (v) (progn (set-attr (n this) "action" (jstr v))
                 (setf (context-dirty ctx) t)))

    ;; method: enumerated attribute "get"/"post"
    (defgetset-for ctx ep "form" "method" (this)
      (form-method-value (n this))
      (v) (progn (set-attr (n this) "method" (jstr v))
                 (setf (context-dirty ctx) t)))

    ;; enctype: enumerated attribute
    (defgetset-for ctx ep "form" "enctype" (this)
      (form-enctype-value (n this))
      (v) (progn (set-attr (n this) "enctype" (jstr v))
                 (setf (context-dirty ctx) t)))

    ;; encoding: same as enctype
    (defgetset-for ctx ep "form" "encoding" (this)
      (form-enctype-value (n this))
      (v) (progn (set-attr (n this) "enctype" (jstr v))
                 (setf (context-dirty ctx) t)))

    ;; target: reflects the content attribute
    (defgetset-for ctx ep "form" "target" (this)
      (or (get-attr (n this) "target") "")
      (v) (progn (set-attr (n this) "target" (jstr v))
                 (setf (context-dirty ctx) t)))

    ;; name: reflects the name attribute
    (defgetset-for ctx ep "form" "name" (this)
      (or (get-attr (n this) "name") "")
      (v) (progn (set-attr (n this) "name" (jstr v))
                 (setf (context-dirty ctx) t)))

    ;; noValidate: boolean reflection
    (defgetset-for ctx ep "form" "noValidate" (this)
      (jbool (dom:has-attribute (n this) "novalidate"))
      (v) (progn (if (js:js-truthy v) (set-attr (n this) "novalidate" "")
                     (remove-attr (n this) "novalidate"))
                 (setf (context-dirty ctx) t)))

    ;; acceptCharset: reflects accept-charset attribute
    (defgetset-for ctx ep "form" "acceptCharset" (this)
      (or (get-attr (n this) "accept-charset") "")
      (v) (progn (set-attr (n this) "accept-charset" (jstr v))
                 (setf (context-dirty ctx) t)))

    ;; elements: SameObject HTMLFormControlsCollection
    (defget-for ctx ep "form" "elements" (this)
      (let ((node (n this)))
        (get-form-elements ctx this node)))

    ;; length: number of form controls (excluding type=image)
    (defget-for ctx ep "form" "length" (this)
      (num (length (form-controls-for-form (n this)))))

    ;; checkValidity: runs constraint validation, returns true if valid
    (defmethod-for ctx ep "form" "checkValidity" 0 (this a)
      (declare (ignore a))
      (jbool (form-check-validity ctx (n this))))

    ;; reportValidity: runs constraint validation and reports results
    (defmethod-for ctx ep "form" "reportValidity" 0 (this a)
      (declare (ignore a))
      (jbool (form-check-validity ctx (n this))))

    ;; requestSubmit(submitter): submit with validation
    (defmethod-for ctx ep "form" "requestSubmit" 1 (this a)
      (form-request-submit ctx this (n this) (arg a 0)))

    ;; submit(): submit without validation
    (defmethod-for ctx ep "form" "submit" 0 (this a)
      (declare (ignore a))
      (form-fire-submit ctx (n this) nil)
      js:*undefined*)

    ;; reset(): reset the form
    (defmethod-for ctx ep "form" "reset" 0 (this a)
      (declare (ignore a))
      js:*undefined*)))

(register-element-proto-extension :form #'install-forms-form)