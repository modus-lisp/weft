;;;; forms-constraints.lisp — the constraint validation API (WPT swarm unit).
;;;;
;;;; This file adds reportValidity to controls and checkValidity/reportValidity
;;;; to <form>.  It implements constraint validation inline rather than calling
;;;; the JS checkValidity, because the JS version (forms-validity.lisp) only
;;;; checks customError — we need to check valueMissing, patternMismatch,
;;;; typeMismatch, rangeOverflow, rangeUnderflow, stepMismatch too.
;;;;
;;;; Installed LAST (after forms-validity, forms-select, forms-textarea, etc)
;;;; so it wraps whatever they installed.
(in-package #:weft.script)

;;; Submittable elements per HTML spec §4.10.22
(defparameter +submittable-tags+
  '("button" "input" "select" "textarea" "fieldset" "object" "output"))

;;; Fire an "invalid" event at NODE.
(defun fire-invalid-event (ctx node)
  (let* ((evt-obj (make-event-object ctx "invalid" nil))
         (ev (evt-of ctx evt-obj)))
    (setf (evt-bubbles ev) nil
          (evt-cancelable ev) t
          (evt-trusted ev) t)
    (dispatch-event ctx node evt-obj)
    (not (evt-default-prevented ev))))

;;; Collect all submittable elements descendant from FORM (recursive).
(defun submittable-controls-of-form (form)
  (let ((result '()))
    (labels ((walk (n)
               (when (eq (h:dnode-kind n) :element)
                 (let ((name (h:dnode-name n)))
                   (when (member name +submittable-tags+ :test #'string=)
                     (push n result)))
                 (loop for ch across (h:dnode-children n) do (walk ch)))))
      (walk form))
    (nreverse result)))

;;; --- Constraint checking helpers (prefixed with constraints- to avoid clashes) ---

(defun constraints-input-value (node ctx)
  (multiple-value-bind (v present) (gethash node (context-input-values ctx))
    (if present v (or (get-attr node "value") ""))))

;; CHECKED-P (dom.lisp) is the one true checkedness: the live `weft-checked'
;; marker if it exists, else the `checked' content attribute.  The private copy
;; that lived here compared weft-checked against the string "checked", which it
;; is never set to (it holds "1"/"0"), so a parser-checked radio read as
;; unchecked and every markup-driven radio test failed.
(defun constraints-input-checked (node) (checked-p node))

(defun constraints-input-required-p (node)
  (dom:has-attribute node "required"))

(defun constraints-input-pattern (node)
  (get-attr node "pattern"))

(defun constraints-input-min (node)
  (get-attr node "min"))

(defun constraints-input-max (node)
  (get-attr node "max"))

(defun constraints-input-step (node)
  (get-attr node "step"))

;;; Check if node is disabled or has a disabled ancestor fieldset
(defun node-disabled-p (node)
  (or (dom:has-attribute node "disabled")
      (fieldset-disabled-ancestor-p node)))

;;; Check if node is readonly (only for input/textarea)
(defun node-readonly-p (node)
  (dom:has-attribute node "readonly"))

;;; A radio's constraint is a property of its GROUP, not of the element: the
;;; group is missing a value when ANY member carries `required' and NO member is
;;; checked, and then EVERY member reports valueMissing.  Checking the element
;;; alone (what this did) gets the sibling cases exactly backwards — a required
;;; radio reads as missing while its partner is checked.
;;;
;;; The group is the radios sharing a non-empty `name' AND a form owner, inside
;;; one tree.  A nameless radio is a group of one.
(defun constraints-radio-group (ctx node)
  (let ((name (get-attr node "name")))
    (if (or (null name) (string= name ""))
        (list node)
        (let ((root (tree-root node))
              (owner (element-form-owner ctx node)))
          (remove-if-not
           (lambda (e)
             (and (string= (or (input-type e) "") "radio")
                  (equal (get-attr e "name") name)
                  (eq (element-form-owner ctx e) owner)))
           (dom:get-elements-by-tag-name root "input"))))))

;;; HTML's VALUE SANITIZATION ALGORITHM, as far as being missing cares about it:
;;; for the date/time and number types a string that is not a valid value for
;;; the type is DISCARDED, not kept and complained about.  `dateInput.value =
;;; 1234567' leaves the value the empty string, so a required date input with
;;; garbage in it is suffering from being MISSING — the whole -weekmonth file
;;; and half the date rows of form-validation-validity-valueMissing.html test
;;; exactly this, and we were reporting the raw string as a present value.
;;; "range" is excluded: its sanitizer CLAMPS to the nearest allowed value
;;; rather than emptying, so a range control is never missing.
(defun constraints-sanitized-value (node ctx)
  (let ((type (input-type node))
        (v (constraints-input-value node ctx)))
    (if (and (fdt-supports-number-p type)
             (not (string= type "range"))
             (not (string= v ""))
             (not (realp (fdt-value->number type v))))
        ""
        v)))

;;; valueMissing: for text-like inputs it's suppressed when disabled/readonly.
;;; For checkbox/radio/file it's NOT suppressed (per WPT expectations).
(defun constraints-check-value-missing (node ctx)
  (let ((type (input-type node)))
    (cond
      ((string= type "radio")
       (let ((group (constraints-radio-group ctx node)))
         (and (some #'constraints-input-required-p group)
              (notany #'constraints-input-checked group))))
      ((string= type "checkbox")
       (and (constraints-input-required-p node) (not (constraints-input-checked node))))
      ((member type '("file") :test #'string=)
       (and (constraints-input-required-p node) (string= (constraints-input-value node ctx) "")))
      ((member type '("text" "search" "tel" "password" "url" "email"
                      "date" "time" "datetime-local" "month" "week" "number")
               :test #'string=)
       (and (not (node-disabled-p node))
            (not (node-readonly-p node))
            (constraints-input-required-p node)
            (string= (constraints-sanitized-value node ctx) "")))
      (t nil))))

(defun constraints-check-type-mismatch (node ctx)
  (let ((type (input-type node))
        (val (constraints-input-value node ctx)))
    (when (not (string= val ""))
      (cond
        ((string= type "email")
         (not (and (find #\@ val) (find #\. val))))
        ((string= type "url")
         (not (>= (length val) 4)))
        (t nil)))))

;;; HTML compiles `pattern' as ^(?:P)$ with the `v' flag, and IGNORES the
;;; attribute entirely when that does not compile.  Both halves are load-bearing:
;;; unanchored, "a" satisfies pattern="[0-9]" whenever any subset matches, and an
;;; uncompilable pattern used to let a JS SyntaxError escape into Lisp, which
;;; abandons the whole <script> block (bridge.lisp) rather than failing one test.
(defun constraints-pattern-regex (ctx pattern-str)
  (handler-case
      (js:js-construct (js:eval-script (context-realm ctx) "RegExp")
                       (list (jstr (concatenate 'string "^(?:" pattern-str ")$"))
                             (jstr "v")))
    (error () nil)))

(defun constraints-check-pattern-mismatch (node ctx)
  (let ((pattern-str (constraints-input-pattern node))
        (val (constraints-input-value node ctx)))
    (when (and pattern-str (not (string= val "")))
      (let ((regex (constraints-pattern-regex ctx pattern-str)))
        (when regex
          (handler-case
              (not (js:js-truthy (js:js-call (js:js-get regex "test") regex
                                             (list (jstr val)))))
            (error () nil)))))))

;;; The number S denotes in TYPE's value space, or NIL when the attribute is
;;; absent/empty, when min|max does not apply to TYPE, or when S is not a valid
;;; value string for TYPE.  All three cases mean "no constraint".
;;;
;;; This replaced a raw STRING comparison for the date-ish types.  Lexicographic
;;; order is not value order: it called the invalid "2000-99" greater than
;;; "2000-12", and called year 10000 SMALLER than 9999.  Harmless while the
;;; validity flags were hardcoded false; worth −20 subtests the moment they went
;;; live.
(defun constraints-limit (type s)
  (and (fdt-supports-number-p type)
       s (not (string= s ""))
       (let ((n (fdt-value->number type s)))
         (and (realp n) n))))

(defun constraints-check-range-overflow (node ctx)
  (let* ((type (input-type node))
         (maxv (constraints-limit type (constraints-input-max node)))
         (v (constraints-limit type (constraints-input-value node ctx))))
    (and maxv v (> v maxv))))

(defun constraints-check-range-underflow (node ctx)
  (let* ((type (input-type node))
         (minv (constraints-limit type (constraints-input-min node)))
         (v (constraints-limit type (constraints-input-value node ctx))))
    (and minv v (< v minv))))

;;; HTML's step base: `min' if it parses, else the `value' CONTENT attribute if
;;; it parses, else the type's default.  The value-attribute fallback is the one
;;; people forget — it is why `<input type=number value=1.5>' with no min is
;;; step-valid at 1.5 and invalid the moment the attribute changes underneath it.
(defun constraints-step-base (type node)
  (or (constraints-limit type (constraints-input-min node))
      (constraints-limit type (get-attr node "value"))
      (fdt-default-step-base type)))

(defun constraints-check-step-mismatch (node ctx)
  (let* ((type (input-type node))
         (step-str (constraints-input-step node))
         (val (constraints-limit type (constraints-input-value node ctx))))
    (when (and val (fdt-supports-step-p type)
               (not (and step-str (string-equal step-str "any"))))
      (let* ((declared (and step-str (not (string= step-str ""))
                            (fdt-parse-float step-str)))
             (n (if (and declared (plusp declared)) declared (fdt-default-step type)))
             (step (* n (fdt-step-scale type)))
             (base (constraints-step-base type node)))
        (when (plusp step)
          ;; RATIONALIZE, not the raw doubles: 0.3d0 REM 0.1d0 is 0.0999...,
          ;; so an exactly-on-step value read as a mismatch.  RATIONALIZE maps
          ;; each double back to the short decimal that printed it (1/10, 3/10),
          ;; and CL's MOD on rationals is exact.
          (not (zerop (mod (rationalize (- val base)) (rationalize step)))))))))

;;; Check if an <input> element is valid according to all constraints.
(defun constraints-check-input-valid (node ctx)
  (not (or (constraints-check-value-missing node ctx)
           (constraints-check-type-mismatch node ctx)
           (constraints-check-pattern-mismatch node ctx)
           (constraints-check-range-overflow node ctx)
           (constraints-check-range-underflow node ctx)
           (constraints-check-step-mismatch node ctx))))

;;; Check if a <textarea> element is valid (valueMissing).
(defun constraints-check-textarea-valid (node ctx)
  (if (or (node-disabled-p node) (node-readonly-p node))
      t
      (let ((required (dom:has-attribute node "required"))
            (val (multiple-value-bind (v p) (gethash node (context-input-values ctx))
                   (if p v (child-text-content node)))))
        (not (and required (string= val ""))))))

;;; <select> selectedness, as forms-select.lisp models it: the `selected'
;;; content attribute, plus the "ask for a reset" auto-selection of the first
;;; non-disabled option when the select shows one row and is not `multiple'.
(defun constraints-option-value (o)
  (or (get-attr o "value")
      (let ((out ""))
        (loop for c across (h:dnode-children o)
              when (eq (h:dnode-kind c) :text)
              do (setf out (concatenate 'string out (h:dnode-data c))))
        out)))

(defun constraints-select-selected (node)
  (let* ((opts (select-all-options node))
         (sel (remove-if-not (lambda (o) (dom:has-attribute o "selected")) opts)))
    (cond (sel sel)
          ((and opts (select-one-row-p node))
           (let ((f (find-if (lambda (o) (not (dom:has-attribute o "disabled"))) opts)))
             (and f (list f))))
          (t nil))))

;;; The placeholder label option: the FIRST option in the list of options, when
;;; its value is the empty string and its parent is the select itself — never an
;;; <optgroup> child — and the select shows one row and is not `multiple'.
(defun constraints-select-placeholder (node)
  (let ((opts (select-all-options node)))
    (and opts
         (select-one-row-p node)
         (eq (h:dnode-parent (first opts)) node)
         (string= (constraints-option-value (first opts)) "")
         (first opts))))

;;; A required <select> is suffering from being missing when no option is
;;; selected, or when the ONLY selected option is the placeholder label option.
;;; The previous version compared against the first option's `value' ATTRIBUTE
;;; (so an option whose value came from its text was a placeholder), and never
;;; consulted `multiple' or the display size at all — which is why a `multiple'
;;; select with a selected empty option read as invalid.
(defun constraints-check-select-valid (node ctx)
  (declare (ignore ctx))
  (if (not (dom:has-attribute node "required"))
      t
      (let ((sel (constraints-select-selected node))
            (ph (constraints-select-placeholder node)))
        (not (or (null sel)
                 (and ph (null (cdr sel)) (eq (first sel) ph)))))))

;;; Check if a <button> element is valid (always valid for constraints).
(defun constraints-check-button-valid (node ctx)
  (declare (ignore node ctx))
  t)

;;; Check if a <fieldset> element is valid (always true).
(defun constraints-check-fieldset-valid (node ctx)
  (declare (ignore node ctx))
  t)

;;; Check if an <output> element is valid (always true).
(defun constraints-check-output-valid (node ctx)
  (declare (ignore node ctx))
  t)

;;; Check if a control is barred from constraint validation.
(defun constraints-barred-p (node)
  (let ((tag (h:dnode-name node)))
    (cond
      ((string= tag "fieldset") t)
      ((string= tag "output") t)
      ((string= tag "button")
       (or (dom:has-attribute node "disabled")
           (dom:has-attribute node "readonly")
           (fieldset-disabled-ancestor-p node)
           (let ((type (button-type node)))
             (not (string= type "submit")))))
      ((string= tag "input")
       (or (dom:has-attribute node "disabled")
           (dom:has-attribute node "readonly")
           (fieldset-disabled-ancestor-p node)
           (let ((type (input-type node)))
             (member type '("hidden" "reset" "button") :test #'string=))))
      ((string= tag "textarea")
       (or (dom:has-attribute node "disabled")
           (dom:has-attribute node "readonly")
           (fieldset-disabled-ancestor-p node)))
      ((string= tag "select")
       (or (dom:has-attribute node "disabled")
           (dom:has-attribute node "readonly")
           (fieldset-disabled-ancestor-p node)))
      (t t))))

;;; Main constraint validation check — returns (values valid-p invalid-event-fired-p)
;;; Barred controls are always valid (for checkValidity/reportValidity purposes).
(defun constraints-check-valid (ctx node)
  (if (constraints-barred-p node)
      (values t nil)
      (let* ((tag (h:dnode-name node))
             (valid-p
              (cond
                ((string= tag "input") (constraints-check-input-valid node ctx))
                ((string= tag "textarea") (constraints-check-textarea-valid node ctx))
                ((string= tag "select") (constraints-check-select-valid node ctx))
                ((string= tag "button") (constraints-check-button-valid node ctx))
                ((string= tag "fieldset") (constraints-check-fieldset-valid node ctx))
                ((string= tag "output") (constraints-check-output-valid node ctx))
                (t t))))
        (values valid-p
                (if valid-p nil (fire-invalid-event ctx node))))))

;;; The nine non-custom ValidityState flags for NODE, as an alist of
;;; (name . generalized-boolean).
;;;
;;; These are the SAME predicates checkValidity has always used.  Until now the
;;; `validity' getter published a hardcoded all-false record, so one subsystem
;;; gave two answers: `input.checkValidity()' correctly said false for a
;;; required empty input while `input.validity.valueMissing' said false too.
;;; Everything that reads a flag rather than calling the method — most of
;;; form-validation-validity-*.html — was scoring on the constant.
;;;
;;; tooLong/tooShort stay false: both require a DIRTY value (a user edit), and
;;; we have no user editing, which is what the WPT files assert.  badInput
;;; likewise needs an unparseable UI state no script can reach.
(defun constraints-validity-flags (ctx node)
  (let* ((tag (h:dnode-name node))
         (input-p (string= tag "input"))
         (missing (cond (input-p (constraints-check-value-missing node ctx))
                        ((string= tag "textarea")
                         (not (constraints-check-textarea-valid node ctx)))
                        ((string= tag "select")
                         (not (constraints-check-select-valid node ctx)))
                        (t nil))))
    (flet ((in (p) (and input-p (funcall p node ctx) t)))
      (list (cons "valueMissing"    (and missing t))
            (cons "typeMismatch"    (in #'constraints-check-type-mismatch))
            (cons "patternMismatch" (in #'constraints-check-pattern-mismatch))
            (cons "tooLong"         nil)
            (cons "tooShort"        nil)
            (cons "rangeUnderflow"  (in #'constraints-check-range-underflow))
            (cons "rangeOverflow"   (in #'constraints-check-range-overflow))
            (cons "stepMismatch"    (in #'constraints-check-step-mismatch))
            (cons "badInput"        nil)))))

;;; select-all-options: walk all option descendants of a select element.
(defun select-all-options (node)
  (let ((result nil))
    (labels ((walk (n inside-optgroup)
               (loop for c across (h:dnode-children n)
                     do (let ((tag (h:dnode-name c)))
                          (cond ((string= tag "option") (push c result))
                                ((string= tag "optgroup")
                                 (unless inside-optgroup (walk c t)))
                                ((member tag '("hr" "select") :test #'string=)
                                 nil)
                                (t (walk c inside-optgroup)))))))
      (walk node nil))
    (nreverse result)))

(defun install-forms-constraints (ctx ep)
  (macrolet ((n (this) `(require-node ctx ,this)))
    (let ((select-custom-errors (make-hash-table :test 'eq))
          (textarea-custom-errors (make-hash-table :test 'eq)))
      ;; --- reportValidity on each control tag ---
      (dolist (tag '("input" "textarea" "select" "button" "fieldset" "output"))
        (defmethod-for ctx ep tag "reportValidity" 0 (this a)
          (declare (ignore a))
          (let ((node (n this)))
            (multiple-value-bind (valid-p fired)
                (constraints-check-valid ctx node)
              (declare (ignore fired))
              (if valid-p js:*true* js:*false*)))))

      ;; --- checkValidity on <form> ---
      (defmethod-for ctx ep "form" "checkValidity" 0 (this a)
        (declare (ignore a))
        (let ((node (n this))
              (any-invalid nil))
          (dolist (c (submittable-controls-of-form node))
            (multiple-value-bind (valid-p fired)
                (constraints-check-valid ctx c)
              (declare (ignore fired))
              (unless valid-p
                (setf any-invalid t))))
          (if any-invalid js:*false* js:*true*)))

      ;; --- reportValidity on <form> ---
      (defmethod-for ctx ep "form" "reportValidity" 0 (this a)
        (declare (ignore a))
        (let ((node (n this))
              (any-invalid nil))
          (dolist (c (submittable-controls-of-form node))
            (multiple-value-bind (valid-p fired)
                (constraints-check-valid ctx c)
              (declare (ignore fired))
              (unless valid-p
                (setf any-invalid t))))
          (if any-invalid js:*false* js:*true*)))

      ;; ====================================================================
      ;; Override checkValidity on input to check ALL constraints.
      ;; ====================================================================
      (defmethod-for ctx ep "input" "checkValidity" 0 (this a)
        (declare (ignore a))
        (let ((node (n this)))
          (if (constraints-barred-p node)
              js:*true*
              (multiple-value-bind (valid-p fired)
                  (constraints-check-valid ctx node)
                (declare (ignore fired))
                (if valid-p js:*true* js:*false*)))))

      ;; ====================================================================
      ;; Override checkValidity on textarea.
      ;; ====================================================================
      (defmethod-for ctx ep "textarea" "checkValidity" 0 (this a)
        (declare (ignore a))
        (let ((node (n this)))
          (if (constraints-barred-p node)
              js:*true*
              (multiple-value-bind (valid-p fired)
                  (constraints-check-valid ctx node)
                (declare (ignore fired))
                (if valid-p js:*true* js:*false*)))))

      ;; ====================================================================
      ;; Override checkValidity on select.
      ;; ====================================================================
      (defmethod-for ctx ep "select" "checkValidity" 0 (this a)
        (declare (ignore a))
        (let ((node (n this)))
          (if (constraints-barred-p node)
              js:*true*
              (multiple-value-bind (valid-p fired)
                  (constraints-check-valid ctx node)
                (declare (ignore fired))
                (if valid-p js:*true* js:*false*)))))

      ;; ====================================================================
      ;; Override checkValidity on button.
      ;; ====================================================================
      (defmethod-for ctx ep "button" "checkValidity" 0 (this a)
        (declare (ignore a))
        (let ((node (n this)))
          (if (constraints-barred-p node)
              js:*true*
              (multiple-value-bind (valid-p fired)
                  (constraints-check-valid ctx node)
                (declare (ignore fired))
                (if valid-p js:*true* js:*false*)))))

      ;; ====================================================================
      ;; Override validity getter on input/textarea/select/button to build
      ;; a complete ValidityState with all flags computed.
      ;; ====================================================================

      (multiple-value-bind (pg ps) (previous-accessor ep "validity")
        (declare (ignore ps))
        (js:put-accessor ep "validity"
          :get (js:native-function (context-realm ctx) "get validity"
                 (lambda (this ig)
                   (declare (ignore ig))
                   (let ((tag (let ((n (node-of ctx this))) (and n (h:dnode-name n)))))
                     (if (member tag '("input" "textarea" "select" "button")
                                 :test #'string=)
                         (let* ((node (n this))
                                ;; Call the previous getter to get the base ValidityState
                                ;; (this preserves individual flag computations from
                                ;; forms-validity.lisp, forms-select.lisp, etc.)
                                ;; ...but PG is ONE accessor shared by every tag,
                                ;; and it dispatches: for a tag no earlier file
                                ;; claimed (<select> has no `validity' getter of
                                ;; its own) it returns undefined, and every
                                ;; js:put below then died with "#<js undefined>
                                ;; is not of type JS-OBJECT" — a Lisp error, so
                                ;; bridge.lisp abandoned the whole <script>.
                                (base (and pg (js:js-call pg this '())))
                                (obj (if (js:js-object-p base)
                                         base
                                         (make-validity-state ctx))))
                           ;; Publish the real flags (the base getter's record
                           ;; is all-false boilerplate; only its customError,
                           ;; which comes from setCustomValidity, is live).
                           (loop for (k . v) in (constraints-validity-flags ctx node)
                                 do (js:put obj k (if v js:*true* js:*false*)
                                            :writable nil :enumerable t :configurable t))
                           ;; For select/textarea, set customError flag from our hash
                           (when (member tag '("select" "textarea") :test #'string=)
                             (let* ((custom-msg
                                     (if (string= tag "textarea")
                                         (gethash node textarea-custom-errors)
                                         (gethash node select-custom-errors)))
                                    (custom-p (and custom-msg (plusp (length custom-msg)))))
                               (js:put obj "customError"
                                       (if custom-p js:*true* js:*false*)
                                       :writable nil :enumerable t :configurable t)))
                           ;; Patch the `valid` flag: any flag set => valid = false
                           (let ((any-flag (dolist (k '("valueMissing" "typeMismatch" "patternMismatch"
                                                        "tooLong" "tooShort" "rangeUnderflow"
                                                        "rangeOverflow" "stepMismatch" "badInput"
                                                        "customError"))
                                             (when (js:js-truthy (js:js-get obj k))
                                               (return t)))))
                             (js:put obj "valid"
                                     (if any-flag js:*false* js:*true*)
                                     :writable nil :enumerable t :configurable t))
                           obj)
                         (if pg (js:js-call pg this '()) js:*undefined*))))
                 0)
          :set nil :enumerable t :configurable t))

      ;; ====================================================================
      ;; Override validationMessage on all controlled elements.
      ;; ====================================================================

      (multiple-value-bind (pg ps) (previous-accessor ep "validationMessage")
        (declare (ignore ps))
        (js:put-accessor ep "validationMessage"
          :get (js:native-function (context-realm ctx) "get validationMessage"
                 (lambda (this ig)
                   (declare (ignore ig))
                   (let ((tag (let ((n (node-of ctx this))) (and n (h:dnode-name n)))))
                     (if (member tag '("input" "textarea" "select" "button")
                                 :test #'string=)
                         (let ((node (n this)))
                           (if (constraints-barred-p node)
                               ""
                               (cond
                                 ((string= tag "textarea") (or (gethash node textarea-custom-errors) ""))
                                 ((string= tag "select") (or (gethash node select-custom-errors) ""))
                                 (t (if pg (js:js-call pg this '()) "")))))
                         (if pg (js:js-call pg this '()) js:*undefined*))))
                 0)
          :set nil :enumerable t :configurable t))

      ;; ====================================================================
      ;; setCustomValidity for select and textarea (missing from their units).
      ;; ====================================================================
      (defmethod-for ctx ep "select" "setCustomValidity" 1 (this a)
        (let ((node (n this))
              (msg (jstr (arg a 0))))
          (if (string= msg "")
              (remhash node select-custom-errors)
              (setf (gethash node select-custom-errors) msg)))
        js:*undefined*)

      (defmethod-for ctx ep "textarea" "setCustomValidity" 1 (this a)
        (let ((node (n this))
              (msg (jstr (arg a 0))))
          (if (string= msg "")
              (remhash node textarea-custom-errors)
              (setf (gethash node textarea-custom-errors) msg)))
        js:*undefined*))))

(register-element-proto-extension :constraints #'install-forms-constraints)