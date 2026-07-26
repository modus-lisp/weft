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
               (obj (make-validity-state ctx))
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
;; ---- HTMLFormElement named/indexed access (form[name] -> element) ----------
;; HTMLFormElement has [LegacyUnenumerableNamedProperties]: form[name] returns
;; the element(s) with that name/id.  Indexed access (form[0]) returns the
;; control at that index, excluding input type=image.
(register-element-exotic "form"
  (lambda (ectx node obj)
    (declare (ignore obj))
    ;; There is deliberately no "item"/"namedItem" exclusion here.  A wave patch
    ;; added one to satisfy form-nameditem.html's "Forms should not have an item
    ;; method", but that test uses an EMPTY form: it passes because nothing is
    ;; named "item", not because the name is banned.  HTMLFormElement's IDL has
    ;; no item/namedItem, so as long as the prototype does not define them the
    ;; named lookup simply misses and `form.item` is undefined — while a control
    ;; actually named "item" stays reachable, which the ban broke.
    (labels ((form-ctl (name)
               (and (stringp name) (named-control node name)))
             (form-ctls ()
               "List of form controls (excluding input type=image)."
               (remove-if (lambda (n)
                            (and (string= (h:dnode-name n) "input")
                                 (string= (or (get-attr n "type") "text") "image")))
                          (form-controls node)))
             (form-by-index (i)
               "Index into the form controls list."
               (let ((ctls (form-ctls)))
                 (if (and (>= i 0) (< i (length ctls))) (nth i ctls) nil))))
      (list
       :get
       (lambda (o key rcv)
         (let ((k (js:to-property-key key)))
           (cond
             ((index-string-p k)
              (let ((ctl (form-by-index (parse-integer k))))
                (if ctl (wrap ectx ctl) js:*undefined*)))
             ((and (stringp k) (form-ctl k))
              (wrap ectx (form-ctl k)))
             (t (js::ordinary-get o k (or rcv o))))))
       :has
       (lambda (o key)
         (let ((k (js:to-property-key key)))
           (or (and (index-string-p k) (form-by-index (parse-integer k)) t)
               (and (stringp k) (form-ctl k) t)
               (js::ordinary-has o key))))
       ;; Order is index, then name, then ordinary own property — WebIDL §3.9.1
       ;; for a legacy platform object, and HTMLFormElement carries
       ;; [LegacyOverrideBuiltIns], so a supported name wins even over an own
       ;; property.  The wave patch had the ordinary lookup first.
       ;;
       ;; PROPS-GET, not GETHASH: shuttle's own-property map is a hybrid — NIL
       ;; while empty, an alist while small, an EQUAL hash-table only once the key
       ;; count passes +props-small-limit+ (8).  GETHASH therefore works ONLY on
       ;; objects that happen to have been promoted, and a <form> wrapper usually
       ;; has not been, so this raised "NIL is not of type HASH-TABLE" — a LISP
       ;; error, for which bridge.lisp abandons the entire <script> block.  The
       ;; key needs coercing here too, exactly as the traps above do.
       :get-own-property
       (lambda (o key)
         (let* ((k (js:to-property-key key))
                (idx (and (index-string-p k) (form-by-index (parse-integer k)))))
           (cond
             ;; indices are enumerable, named properties are not
             ;; ([LegacyUnenumerableNamedProperties])
             (idx (js::make-prop :value (wrap ectx idx)
                                 :enumerable t :configurable t :writable nil))
             ((form-ctl k)
              (js::make-prop :value (wrap ectx (form-ctl k))
                             :enumerable nil :configurable t :writable nil))
             (t (js::props-get o k)))))))))
