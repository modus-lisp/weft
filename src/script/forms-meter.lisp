;;;; forms-meter.lisp — HTMLMeterElement IDL: value, min, max, low, high, optimum
;;;;
;;;; Self-registering feature file.  Each attribute is a double-reflection:
;;;; the getter parses the content attribute (fdt-parse-float), falls back to a
;;;; default, and clamps per the HTML spec order; the setter converts the JS
;;;; value to a number, rejects NaN/Infinity via TypeError, and writes the
;;;; string representation to the content attribute.
(in-package #:weft.script)

(defun meter-parse-attr (node name)
  "Parse the content attribute NAME as a floating-point number, or return NIL."
  (let ((s (get-attr node name)))
    (and s (fdt-parse-float s))))

(defun meter-num-to-string (x)
  "Convert a double-float to a string suitable for a content attribute."
  (format nil "~f" (coerce x 'double-float)))

(defun meter-calc (ctx node)
  "Compute all six values in dependency order, returning
   (values value min max low high optimum)."
  (let* ((min-raw (or (meter-parse-attr node "min") 0d0))
         (max-raw (or (meter-parse-attr node "max") 1d0))
         (max-val (if (< max-raw min-raw) min-raw max-raw))
         (min-val min-raw)
         (value-raw (or (meter-parse-attr node "value") 0d0))
         (value-val (max min-val (min max-val value-raw)))
         (low-raw (or (meter-parse-attr node "low") min-val))
         (low-val (max min-val (min max-val low-raw)))
         (high-raw (or (meter-parse-attr node "high") max-val))
         (high-val (max min-val (min max-val high-raw)))
         ;; If high < low, clamp high up to low
         (high-val (if (< high-val low-val) low-val high-val))
         (optimum-raw (or (meter-parse-attr node "optimum")
                          (/ (+ min-val max-val) 2d0)))
         (optimum-val (max min-val (min max-val optimum-raw))))
    (values value-val min-val max-val low-val high-val optimum-val)))

(defun install-forms-meter (ctx ep)
  (macrolet ((n (this) `(require-node ctx ,this)))
    (flet ((meter-getter (this which)
             (let ((node (n this)))
               (multiple-value-bind (v mn mx l h o) (meter-calc ctx node)
                 (num (ecase which
                        (:value v) (:min mn) (:max mx)
                        (:low l) (:high h) (:optimum o))))))
           (meter-setter (this v attr)
             (let* ((node (n this))
                    (d (js:to-number v)))
               (when (or (sb-ext:float-nan-p d) (sb-ext:float-infinity-p d))
                 (js:js-throw
                  (js:make-native-error "TypeError" "The value must be a finite number")))
               (set-attr node attr (meter-num-to-string d))
               (setf (context-dirty ctx) t))))
      ;; value
      (defgetset-for ctx ep "meter" "value" (this)
        (meter-getter this :value)
        (v) (meter-setter this v "value"))
      ;; min
      (defgetset-for ctx ep "meter" "min" (this)
        (meter-getter this :min)
        (v) (meter-setter this v "min"))
      ;; max
      (defgetset-for ctx ep "meter" "max" (this)
        (meter-getter this :max)
        (v) (meter-setter this v "max"))
      ;; low
      (defgetset-for ctx ep "meter" "low" (this)
        (meter-getter this :low)
        (v) (meter-setter this v "low"))
      ;; high
      (defgetset-for ctx ep "meter" "high" (this)
        (meter-getter this :high)
        (v) (meter-setter this v "high"))
      ;; optimum
      (defgetset-for ctx ep "meter" "optimum" (this)
        (meter-getter this :optimum)
        (v) (meter-setter this v "optimum")))))

(register-element-proto-extension :meter #'install-forms-meter)