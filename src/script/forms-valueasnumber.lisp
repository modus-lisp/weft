;;;; forms-valueasnumber.lisp — HTMLInputElement.valueAsNumber (get/set).
;;;;
;;;; Maps the control's value string to/from a Double via each type's algorithm
;;;; (see forms-datetime.lisp).  Registers into the element-proto hook.
(in-package #:weft.script)

(defun van-cur-value (ctx node)
  (multiple-value-bind (v p) (gethash node (context-input-values ctx))
    (if p v (or (get-attr node "value") ""))))

(defun van-attr-number (node name)
  "Parse a min/max content attribute as a float, or NIL."
  (let ((s (get-attr node name))) (and s (fdt-parse-float s))))

(defun van-range-value (ctx node)
  "Range always yields a number: the parsed value clamped to [min,max], or the
   default (min + (max-min)/2) clamped, when the value is empty/invalid."
  (let* ((mn (or (van-attr-number node "min") 0d0))
         (mx (or (van-attr-number node "max") 100d0))
         (mx (if (< mx mn) mn mx))                        ; a broken range collapses to min
         (dflt (+ mn (/ (- mx mn) 2)))
         (v (fdt-parse-float (van-cur-value ctx node))))
    (max mn (min mx (or v dflt)))))

(defun install-forms-valueasnumber (ctx ep)
  (macrolet ((n (this) `(require-node ctx ,this)))
    (defgetset ctx ep "valueAsNumber" (this)
      (let* ((node (n this)) (type (input-type node)))
        (cond
          ((string= type "range") (fdt-num (van-range-value ctx node)))
          ((fdt-supports-number-p type)
           (let ((r (fdt-value->number type (van-cur-value ctx node))))
             (if (eq r :nan) (num (fdt-nan)) (fdt-num r))))
          (t (num (fdt-nan)))))
      (v)
      (let* ((node (n this)) (type (input-type node)) (d (js:to-number v)))
        (when (or (sb-ext:float-nan-p d) (sb-ext:float-infinity-p d)) ; not finite -> TypeError (before type check)
          (js:js-throw (js:make-native-error "TypeError" "valueAsNumber must be finite")))
        (unless (fdt-supports-number-p type)
          (throw-dom ctx "InvalidStateError" 11 "valueAsNumber does not apply to this input type"))
        (setf (gethash node (context-input-values ctx)) (fdt-number->value type d)
              (context-dirty ctx) t)))))

(register-element-proto-extension :valueasnumber #'install-forms-valueasnumber)
