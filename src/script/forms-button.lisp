;;;; forms-button.lisp — HTMLElement IDL surface for <button> (WPT swarm unit).
;;;; Self-registering into the element-proto hook (see core.lisp).  The installer
;;;; runs on the shared element prototype; each accessor must gate on the node's
;;;; tag ((string= (h:dnode-name (n this)) "button")) so it only acts on <button>.
(in-package #:weft.script)

(defun button-type (node)
  "Return the canonical type for a <button> node."
  (let ((raw (get-attr node "type")))
    (if raw
        (let ((v (string-downcase raw)))
          (if (member v '("submit" "reset" "button") :test #'equal) v "submit"))
        "submit")))

(defun install-forms-button (ctx ep)
  (declare (ignorable ctx ep))
  (macrolet ((n (this) `(require-node ctx ,this)))
    (let ((button-custom-errors (make-hash-table :test 'eq)))
      ;; willValidate — false for type=button and type=reset, and for disabled
      ;; buttons (including disabled by an ancestor fieldset).
      (defget-for ctx ep "button" "willValidate" (this)
        (let ((node (n this)))
          (if (or (dom:has-attribute node "disabled")
                  (fieldset-disabled-ancestor-p node))
              js:*false*
              (let ((type (button-type node)))
                (if (string= type "submit") js:*true* js:*false*)))))
      ;; checkValidity — returns true if no constraints are violated
      (defmethod-for ctx ep "button" "checkValidity" 0 (this a)
        (declare (ignore a))
        (let* ((node (n this))
               (custom-msg (gethash node button-custom-errors))
               (custom-error-p (and custom-msg (plusp (length custom-msg)))))
          (if custom-error-p js:*false* js:*true*)))
      ;; setCustomValidity(error) — stores/clears custom error message
      (defmethod-for ctx ep "button" "setCustomValidity" 1 (this a)
        (let ((node (n this))
              (msg (jstr (arg a 0))))
          (if (string= msg "")
              (remhash node button-custom-errors)
              (setf (gethash node button-custom-errors) msg)))
        js:*undefined*)
      ;; validationMessage — reflects stored custom error
      (defget-for ctx ep "button" "validationMessage" (this)
        (let ((node (n this)))
          (or (gethash node button-custom-errors) "")))
      ;; validity — ValidityState object reflecting current state
      (defget-for ctx ep "button" "validity" (this)
        (let* ((node (n this))
               (obj (js:make-object
                     :proto (js:eval-script (context-realm ctx) "Object.prototype")))
               (custom-msg (gethash node button-custom-errors))
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

(register-element-proto-extension :button #'install-forms-button)