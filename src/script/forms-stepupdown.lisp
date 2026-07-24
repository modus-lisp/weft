;;;; forms-stepupdown.lisp — HTMLInputElement.stepUp(n) / stepDown(n).
;;;;
;;;; The HTML "step up or step down by n" algorithm, in valueAsNumber space using
;;;; the per-type step / scale / step-base from forms-datetime.lisp.  Registers
;;;; into the element-proto hook.
(in-package #:weft.script)

(defun sud-cur-value (ctx node)
  (multiple-value-bind (v p) (gethash node (context-input-values ctx))
    (if p v (or (get-attr node "value") ""))))

(defun sud-attr->number (node name type)
  "min/max attribute in valueAsNumber space, or NIL."
  (let ((s (get-attr node name)))
    (when s (let ((r (fdt-value->number type s))) (unless (eq r :nan) r)))))

(defun sud-step (ctx node factor)
  "Run 'step up or step down' with FACTOR = n*direction (n>0 up, n<0 down)."
  (let ((type (input-type node)))
    (unless (fdt-supports-step-p type)
      (throw-dom ctx "InvalidStateError" 11 "this input type has no allowed value step"))
    (let ((step-attr (get-attr node "step")))
      (when (and step-attr (string-equal (string-trim " " step-attr) "any"))
        (throw-dom ctx "InvalidStateError" 11 "step is \"any\": no allowed value step"))
      (let* ((scale (fdt-step-scale type))
             (sv (and step-attr (fdt-parse-float step-attr)))
             (step (* (if (and sv (> sv 0)) sv (fdt-default-step type)) scale))
             (mn (sud-attr->number node "min" type))
             (mx (sud-attr->number node "max" type))
             (base (or mn (fdt-default-step-base type))))
        (when (and mn mx (> mn mx)) (return-from sud-step))     ; inconsistent bounds: do nothing
        ;; empty/invalid value defaults to 0, but CLAMPED into [min,max] (a valid
        ;; out-of-range value like "3" is NOT clamped — that's the whole difference).
        (let* ((cur (let ((r (fdt-value->number type (sud-cur-value ctx node))))
                      (if (eq r :nan) (cond ((and mn (< 0 mn)) mn) ((and mx (> 0 mx)) mx) (t 0)) r)))
               (before cur) (delta (* factor step)) (value cur))
          (if (not (zerop (mod (- value base) step)))            ; misaligned: snap in delta's direction
              (let ((k (floor (- value base) step)))
                (setf value (if (> delta 0) (+ base (* (1+ k) step)) (+ base (* k step)))))
              (setf value (+ value delta)))                      ; aligned: take the step
          (when (and mn (< value mn))                            ; clamp up to the smallest aligned >= min
            (setf value (+ base (* (ceiling (- mn base) step) step))))
          (when (and mx (> value mx))                            ; clamp down to the largest aligned <= max
            (setf value (+ base (* (floor (- mx base) step) step))))
          (when (or (and (> delta 0) (< value before))           ; never move opposite the requested step
                    (and (< delta 0) (> value before)))
            (return-from sud-step))
          (setf (gethash node (context-input-values ctx)) (fdt-number->value type value)
                (context-dirty ctx) t))))))

(defun sud-arg-n (a) (let ((x (arg a 0))) (if (eq x js:*undefined*) 1 (truncate (js:to-number x)))))

(defun install-forms-stepupdown (ctx ep)
  (macrolet ((n (this) `(require-node ctx ,this)))
    (defmethod* ctx ep "stepUp" 1 (this a)
      (sud-step ctx (n this) (sud-arg-n a)) js:*undefined*)
    (defmethod* ctx ep "stepDown" 1 (this a)
      (sud-step ctx (n this) (- (sud-arg-n a))) js:*undefined*)))

(register-element-proto-extension :stepupdown #'install-forms-stepupdown)
