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

(defun constraints-input-checked (node)
  (string= (get-attr node "weft-checked") "checked"))

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

;;; valueMissing: for text-like inputs it's suppressed when disabled/readonly.
;;; For checkbox/radio/file it's NOT suppressed (per WPT expectations).
(defun constraints-check-value-missing (node ctx)
  (let ((type (input-type node)))
    (cond
      ((member type '("checkbox" "radio") :test #'string=)
       (and (constraints-input-required-p node) (not (constraints-input-checked node))))
      ((member type '("file") :test #'string=)
       (and (constraints-input-required-p node) (string= (constraints-input-value node ctx) "")))
      ((member type '("text" "search" "tel" "password" "url" "email"
                      "date" "time" "datetime-local" "month" "week" "number")
               :test #'string=)
       (and (not (node-disabled-p node))
            (not (node-readonly-p node))
            (constraints-input-required-p node)
            (string= (constraints-input-value node ctx) "")))
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

(defun constraints-check-pattern-mismatch (node ctx)
  (let ((pattern-str (constraints-input-pattern node))
        (val (constraints-input-value node ctx)))
    (when (and pattern-str (not (string= val "")))
      (let* ((realm (context-realm ctx))
             (regex (js:js-construct (js:eval-script realm "RegExp") (list (jstr pattern-str))))
             (result (js:js-call (js:js-get regex "test") regex (list (jstr val)))))
        (not (js:js-truthy result))))))

(defun constraints-check-range-overflow (node ctx)
  (let ((type (input-type node))
        (max-str (constraints-input-max node))
        (val (constraints-input-value node ctx)))
    (when (and max-str (not (string= val "")))
      (cond
        ((member type '("date" "time" "datetime-local" "month" "week") :test #'string=)
         (string> val max-str))
        (t
         (handler-case
             (let ((max-val (read-from-string max-str))
                   (num-val (read-from-string val)))
               (when (and (numberp max-val) (numberp num-val))
                 (> num-val max-val)))
           (error () nil)))))))

(defun constraints-check-range-underflow (node ctx)
  (let ((type (input-type node))
        (min-str (constraints-input-min node))
        (val (constraints-input-value node ctx)))
    (when (and min-str (not (string= val "")))
      (cond
        ((member type '("date" "time" "datetime-local" "month" "week") :test #'string=)
         (string< val min-str))
        (t
         (handler-case
             (let ((min-val (read-from-string min-str))
                   (num-val (read-from-string val)))
               (when (and (numberp min-val) (numberp num-val))
                 (< num-val min-val)))
           (error () nil)))))))

(defun constraints-check-step-mismatch (node ctx)
  (let* ((type (input-type node))
         (step-str (constraints-input-step node))
         (min-str (constraints-input-min node))
         (val (constraints-input-value node ctx)))
    (when (and step-str (not (string= val "")) (not (string= step-str "any")))
      (if (fdt-supports-step-p type)
          (let* ((num-val (fdt-value->number type val))
                 (min-val (if min-str (fdt-value->number type min-str) :nan))
                 (step-base (if (and min-str (not (eq min-val :nan))) min-val
                                (fdt-default-step-base type)))
                 (step-val (handler-case (read-from-string step-str)
                             (error () 1)))
                 (scale (fdt-step-scale type))
                 (step-ms (* step-val scale)))
            (when (and (not (eq num-val :nan))
                       (not (eq step-base :nan))
                       (not (zerop step-ms))
                       (numberp num-val) (numberp step-base))
              (let ((diff (- num-val step-base)))
                (not (zerop (rem diff step-ms))))))
          (handler-case
              (let* ((step-val (read-from-string step-str))
                     (min-val (if min-str (read-from-string min-str) 0))
                     (num-val (read-from-string val)))
                (when (and (numberp step-val) (numberp min-val) (numberp num-val)
                           (not (zerop step-val)))
                  (let ((diff (- num-val min-val)))
                    (not (zerop (rem diff step-val))))))
            (error () nil))))))

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

;;; Check if a <select> element is valid (valueMissing).
(defun constraints-check-select-valid (node ctx)
  (declare (ignore ctx))
  (let ((required (dom:has-attribute node "required")))
    (if (not required)
        t
        (let* ((opts (select-all-options node))
               (sel (find-if (lambda (o) (dom:has-attribute o "selected")) opts)))
          (if sel
              (let* ((first (car opts))
                     (not-inside-optgroup
                      (let ((p (h:dnode-parent sel)))
                        (and p (not (string= (h:dnode-name p) "optgroup")))))
                     (first-val (get-attr first "value")))
                (not (and first
                          (eq sel first)
                          not-inside-optgroup
                          (or (null first-val)
                              (string= first-val "")))))
              nil)))))

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
                                (obj (if pg (js:js-call pg this '())
                                         (js:make-object
                                          :proto (js:eval-script (context-realm ctx) "Object.prototype")))))
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