;;;; forms-valueasdate.lisp — HTMLInputElement.valueAsDate (get/set).
;;;;
;;;; A Date object (or null) for the date-family types (date/month/week/time).
;;;; Registers into the element-proto hook.
(in-package #:weft.script)

(defun vad-cur-value (ctx node)
  (multiple-value-bind (v p) (gethash node (context-input-values ctx))
    (if p v (or (get-attr node "value") ""))))

(defun vad-make-date (ctx ms)
  "A fresh JS Date at epoch-ms MS."
  (js:js-construct (js:eval-script (context-realm ctx) "Date")
                   (list (num (coerce ms 'double-float)))))

(defun install-forms-valueasdate (ctx ep)
  (macrolet ((n (this) `(require-node ctx ,this)))
    (defgetset ctx ep "valueAsDate" (this)
      (let* ((node (n this)) (type (input-type node)))
        (if (fdt-supports-date-p type)
            (let ((ms (fdt-value->ms type (vad-cur-value ctx node))))
              (if ms (vad-make-date ctx ms) js:*null*))
            js:*null*))
      (v)
      (let* ((node (n this)) (type (input-type node)))
        (unless (fdt-supports-date-p type)
          (throw-dom ctx "InvalidStateError" 11 "valueAsDate does not apply to this input type"))
        (cond
          ((eq v js:*null*)                                ; null clears the value
           (setf (gethash node (context-input-values ctx)) "" (context-dirty ctx) t))
          (t (let ((d (js:to-number v)))                   ; a Date -> its epoch-ms (valueOf)
               (when (sb-ext:float-nan-p d)                ; an invalid Date -> TypeError
                 (js:js-throw (js:make-native-error "TypeError" "valueAsDate is not a valid Date")))
               (setf (gethash node (context-input-values ctx)) (fdt-ms->value-date type (floor d))
                     (context-dirty ctx) t))))))))

(register-element-proto-extension :valueasdate #'install-forms-valueasdate)
