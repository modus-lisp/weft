;;;; forms-validity.lisp — HTMLInputElement IDL surface: validity.
;;;;
;;;; Self-registering feature file (see core.lisp *element-proto-extensions*).
;;;; install-element-proto funcalls INSTALL-FORMS-validity with (ctx ep) after the
;;;; built-in accessors.  Fill the installer below; keep every helper prefixed
;;;; so this file never collides with a sibling feature file at load time.
(in-package #:weft.script)

(defun install-forms-validity (ctx ep)
  (declare (ignorable ctx ep))
  (macrolet ((n (this) `(require-node ctx ,this)))
    (let ((validity-custom-errors (make-hash-table :test 'eq)))
      ;; validity getter — returns a ValidityState object reflecting current state
      (defget ctx ep "validity" (this)
        (let* ((node (n this))
               (obj (js:make-object
                     :proto (js:eval-script (context-realm ctx) "Object.prototype")))
               (custom-msg (gethash node validity-custom-errors))
               (custom-error-p (and custom-msg (plusp (length custom-msg)))))
          (flet ((vp (k v) (js:put obj k (if v js:*true* js:*false*) :writable nil :enumerable t :configurable t)))
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
          obj))
      ;; checkValidity — returns true if no constraints are violated
      (defmethod* ctx ep "checkValidity" 0 (this a)
        (declare (ignore a))
        (let* ((node (n this))
               (custom-msg (gethash node validity-custom-errors))
               (custom-error-p (and custom-msg (plusp (length custom-msg)))))
          (if custom-error-p js:*false* js:*true*)))
      ;; setCustomValidity(error) — stores/clears custom error message
      (defmethod* ctx ep "setCustomValidity" 1 (this a)
        (let ((node (n this))
              (msg (jstr (arg a 0))))
          (if (string= msg "")
              (remhash node validity-custom-errors)
              (setf (gethash node validity-custom-errors) msg)))
        js:*undefined*)
      ;; validationMessage getter — returns the custom error or ""
      (defget ctx ep "validationMessage" (this)
        (let ((node (n this)))
          (or (gethash node validity-custom-errors) ""))))))

(register-element-proto-extension :validity #'install-forms-validity)