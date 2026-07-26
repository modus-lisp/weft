;;;; forms-fieldset.lisp — HTMLFieldSetElement IDL surface.
;;;;
;;;; Self-registering feature file (see core.lisp *element-proto-extensions*).
;;;; install-element-proto funcalls INSTALL-FORMS-fieldset with (ctx ep) after the
;;;; built-in accessors.
(in-package #:weft.script)

(defun fieldset-listed-controls (node)
  "Return a list of form-control descendants of the fieldset NODE
   (same as +form-control-tags+), in tree order."
  (remove-if-not (lambda (n) (member (h:dnode-name n) +form-control-tags+ :test #'string=))
                 (dom:get-elements-by-tag-name node "*")))

;; Form-owner lookup lives in ONE place: ELEMENT-FORM-OWNER in dom.lisp.  Two
;; private copies grew here during the wave (a nearest-ancestor-only one and an
;; id-scanning one); both were strictly weaker than the shared version — neither
;; consulted the parser's form pointer — and both are gone.  Call the shared one.

(defun fieldset-first-legend (node)
  "Return the first <legend> child of the fieldset, or NIL."
  (loop for c across (h:dnode-children node)
        when (tag= c "legend") return c))

(defun fieldset-in-first-legend-p (node fieldset)
  "Return T if NODE is a descendant of FIELDSET's first <legend> child."
  (let ((legend (fieldset-first-legend fieldset)))
    (and legend (proper-ancestor-p legend node))))

(defun fieldset-disabled-ancestor-p (node)
  "Return the nearest disabled <fieldset> ancestor that bars NODE from
   constraint validation, or NIL.
   An element is barred if it is a descendant of a disabled fieldset,
   UNLESS it is a descendant of that fieldset's first <legend> child."
  (loop for a = (h:dnode-parent node) then (h:dnode-parent a)
        while a
        when (and (tag= a "fieldset")
                  (dom:has-attribute a "disabled")
                  (not (fieldset-in-first-legend-p node a)))
        return a))

(defun install-forms-fieldset (ctx ep)
  (declare (ignorable ctx ep))
  (macrolet ((n (this) `(require-node ctx ,this)))
    ;; type — always "fieldset"
    (defget-for ctx ep "fieldset" "type" (this)
      (declare (ignore this))
      "fieldset")
    ;; form — form owner (respects form= content attribute)
    (defget-for ctx ep "fieldset" "form" (this)
      (let ((node (n this)))
        (let ((form (element-form-owner ctx node)))
          (if form (wrap ctx form) js:*null*))))
    ;; elements — live HTMLCollection of descendant form controls
    (defget-for ctx ep "fieldset" "elements" (this)
      (let ((node (n this)))
        (make-collection ctx (lambda () (fieldset-listed-controls node))
                         (lambda (name)
                           (find-if (lambda (c)
                                      (or (equal (get-attr c "name") name)
                                          (equal (get-attr c "id") name)))
                                    (fieldset-listed-controls node)))
                         :htmlcollection)))
    ;; name — reflected from content attribute
    (defgetset-for ctx ep "fieldset" "name" (this)
      (let ((node (n this)))
        (or (get-attr node "name") ""))
      (v) (let ((node (n this)))
            (set-attr node "name" (jstr v))
            (setf (context-dirty ctx) t)))
    ;; disabled — reflected from content attribute
    (defgetset-for ctx ep "fieldset" "disabled" (this)
      (let ((node (n this)))
        (jbool (dom:has-attribute node "disabled")))
      (v) (let ((node (n this)))
            (if (js:js-truthy v)
                (set-attr node "disabled" "")
                (remove-attr node "disabled"))
            (setf (context-dirty ctx) t)))
    ;; willValidate — fieldset is never a candidate for constraint validation
    (defget-for ctx ep "fieldset" "willValidate" (this)
      (declare (ignore this))
      js:*false*)
    ;; willValidate for input — false if inside a disabled fieldset ancestor
    ;; (outside the first legend child), true otherwise.
    (defget-for ctx ep "input" "willValidate" (this)
      (let ((node (n this)))
        (if (fieldset-disabled-ancestor-p node)
            js:*false*
            js:*true*)))
    ;; willValidate for textarea — same disabled-fieldset rule
    (defget-for ctx ep "textarea" "willValidate" (this)
      (let ((node (n this)))
        (if (fieldset-disabled-ancestor-p node)
            js:*false*
            js:*true*)))
    ;; willValidate for button — same disabled-fieldset rule
    (defget-for ctx ep "button" "willValidate" (this)
      (let ((node (n this)))
        (if (fieldset-disabled-ancestor-p node)
            js:*false*
            (if (string= (button-type node) "submit") js:*true* js:*false*))))
    ;; checkValidity — fieldset has no validation constraints, so always true
    (defmethod-for ctx ep "fieldset" "checkValidity" 0 (this a)
      (declare (ignore this a))
      js:*true*)
    ;; setCustomValidity with local storage
    (let ((fieldset-custom-errors (make-hash-table :test 'eq)))
      (defmethod-for ctx ep "fieldset" "setCustomValidity" 1 (this a)
        (let ((node (n this))
              (msg (jstr (arg a 0))))
          (if (string= msg "")
              (remhash node fieldset-custom-errors)
              (setf (gethash node fieldset-custom-errors) msg)))
        js:*undefined*)
      ;; validationMessage — reflects stored custom error
      (defget-for ctx ep "fieldset" "validationMessage" (this)
        (let ((node (n this)))
          (or (gethash node fieldset-custom-errors) "")))
      ;; validity — always valid except for customError
      (defget-for ctx ep "fieldset" "validity" (this)
        (let* ((node (n this))
               (obj (js:make-object
                     :proto (js:eval-script (context-realm ctx) "Object.prototype")))
               (custom-msg (gethash node fieldset-custom-errors))
               (custom-error-p (and custom-msg (plusp (length custom-msg)))))
          (flet ((vp (k v) (js:put obj k (if v js:*true* js:*false*)
                                   :writable nil :enumerable t :configurable t)))
            (vp "valueMissing" nil)
            (vp "typeMismatch" nil)
            (vp "patternMismatch" nil)
            (vp "tooLong" nil)
            (vp "tooShort" nil)
            (vp "rangeUnderflow" nil)
            (vp "rangeOverflow" nil)
            (vp "stepMismatch" nil)
            (vp "badInput" nil)
            (vp "customError" custom-error-p)
            (vp "valid" (not custom-error-p)))
          obj)))))

(register-element-proto-extension :fieldset #'install-forms-fieldset)
;; ---- HTMLFormElement named access (form[name] -> element) ------------------
;; HTMLFormElement has [LegacyUnenumerableNamedProperties]: form[name] returns
;; the element(s) with that name/id.  This is per-instance exotic behaviour.
(register-element-exotic "form"
  (lambda (ectx node obj)
    (declare (ignore obj))
    (list
     :get
     (lambda (o key rcv)
       (let ((k (js:to-property-key key)))
         (if (and (stringp k) (not (js::ordinary-has o k)))
             (let ((ctl (named-control node k)))
               (if ctl (wrap ectx ctl) (js::ordinary-get o k (or rcv o))))
             (js::ordinary-get o k (or rcv o)))))
     :has
     (lambda (o key)
       (or (js::ordinary-has o key)
           (let ((k (js:to-property-key key)))
             (and (stringp k) (named-control node k) t))))
     :get-own-property
     (lambda (o key)
       (or (gethash key (js::js-object-props o))
           (when (stringp key)
             (let ((ctl (named-control node key)))
               (when ctl
                 (js::make-prop :value (wrap ectx ctl)
                                :enumerable nil :configurable t :writable nil)))))))))
