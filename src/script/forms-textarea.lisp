;;;; forms-textarea.lisp — HTMLTextAreaElement IDL surface.
;;;;
;;;; Installs the textarea-specific IDL accessors onto the shared element prototype
;;;; using the -for macros so that only <textarea> elements are affected; other
;;;; elements fall through to whatever was already installed.
(in-package #:weft.script)

(defun ta-p (node) (string= (h:dnode-name node) "textarea"))
(defun ta-value (ctx node)
  "The textarea's API value: the stored current value, else its child text content."
  (multiple-value-bind (v p) (gethash node (context-input-values ctx))
    (if p v (child-text-content node))))

(defun ta-normalize (s)
  "Normalize \\r\\n and \\r to \\n (the API value normalization)."
  (let ((out (make-array (length s) :element-type 'character :fill-pointer 0)))
    (loop with prev = nil
          for c across s
          do (cond ((char= c #\Return)
                    (vector-push-extend #\Newline out)
                    (setf prev :cr))
                   ((and (char= c #\Newline) (eq prev :cr))
                    (setf prev nil))
                   (t (vector-push-extend c out)
                      (setf prev nil))))
    (coerce out 'string)))

(defun install-forms-textarea (ctx ep)
  (macrolet ((n (this) `(require-node ctx ,this)))
    ;; textLength = length of the API value (UTF-16 code units)
    (defget-for ctx ep "textarea" "textLength" (this)
      (let ((node (n this)))
        (num (length (ta-value ctx node)))))

    ;; wrap reflects the content attribute; default is "soft"
    (defgetset-for ctx ep "textarea" "wrap" (this)
      (or (get-attr (n this) "wrap") "soft")
      (v) (progn (set-attr (n this) "wrap" (jstr v)) (setf (context-dirty ctx) t)))

    ;; cols: reflect, default 20
    (defgetset-for ctx ep "textarea" "cols" (this)
      (let ((s (get-attr (n this) "cols")))
        (num (let ((k (and s (parse-integer s :junk-allowed t))))
               (if (and k (>= k 0)) k 20))))
      (v) (let* ((d (js:to-number v))
                 (k (if (or (sb-ext:float-nan-p d) (sb-ext:float-infinity-p d)) 0 (truncate d))))
            (when (< k 0) (throw-dom ctx "IndexSizeError" 1 "value cannot be negative"))
            (set-attr (n this) "cols" (princ-to-string k))
            (setf (context-dirty ctx) t)))

    ;; rows: reflect, default 2
    (defgetset-for ctx ep "textarea" "rows" (this)
      (let ((s (get-attr (n this) "rows")))
        (num (let ((k (and s (parse-integer s :junk-allowed t))))
               (if (and k (>= k 0)) k 2))))
      (v) (let* ((d (js:to-number v))
                 (k (if (or (sb-ext:float-nan-p d) (sb-ext:float-infinity-p d)) 0 (truncate d))))
            (when (< k 0) (throw-dom ctx "IndexSizeError" 1 "value cannot be negative"))
            (set-attr (n this) "rows" (princ-to-string k))
            (setf (context-dirty ctx) t)))

    ;; value: override the shared value getter/setter to add CRLF normalization
    ;; and null->empty-string handling.
    (defgetset-for ctx ep "textarea" "value" (this)
      (let ((node (n this)))
        (ta-normalize (ta-value ctx node)))
      (v) (setf (gethash (n this) (context-input-values ctx))
                (if (nullish v) "" (jstr v))))

    ;; validity, checkValidity, setCustomValidity: mirror forms-validity.lisp
    (let ((textarea-validity-custom-errors (make-hash-table :test 'eq)))
      (defget-for ctx ep "textarea" "validity" (this)
        (let* ((node (n this))
               (obj (make-validity-state ctx))
               (custom-msg (gethash node textarea-validity-custom-errors))
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
          obj))
      (defmethod-for ctx ep "textarea" "checkValidity" 0 (this a)
        (declare (ignore a))
        (let* ((node (n this))
               (custom-msg (gethash node textarea-validity-custom-errors))
               (custom-error-p (and custom-msg (plusp (length custom-msg)))))
          (if custom-error-p js:*false* js:*true*)))
      (defmethod-for ctx ep "textarea" "setCustomValidity" 1 (this a)
        (let ((node (n this))
              (msg (jstr (arg a 0))))
          (if (string= msg "")
              (remhash node textarea-validity-custom-errors)
              (setf (gethash node textarea-validity-custom-errors) msg)))
        js:*undefined*))))

(register-element-proto-extension :textarea #'install-forms-textarea)