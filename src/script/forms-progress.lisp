;;;; forms-progress.lisp — HTMLProgressElement IDL surface: value, max, position.
;;;;
;;;; Self-registering feature file (see core.lisp *element-proto-extensions*).
;;;; install-element-proto funcalls INSTALL-FORMS-progress with (ctx ep) after the
;;;; built-in accessors.  Fill the installer below; keep every helper prefixed
;;;; so this file never collides with a sibling feature file at load time.
(in-package #:weft.script)

(defun progress-parse-double (node attr)
  "Parse content attribute ATTR as a double.  Returns NIL on missing/invalid."
  (let ((s (get-attr node attr)))
    (and s (fdt-parse-float s))))

(defun progress-max-val (node)
  "The element's maximum value: the max content attribute parsed as a valid
   floating-point number > 0, or 1.0 otherwise."
  (let ((m (progress-parse-double node "max")))
    (if (or (null m) (<= m 0d0)) 1d0 m)))

(defun progress-value-val (node)
  "The element's value: the value content attribute parsed as a valid
   floating-point number, defaulting to 0, then clamped to [0, max]."
  (let* ((raw (progress-parse-double node "value"))
         (v (or raw 0d0)))
    (if (minusp v) 0d0
        (min v (progress-max-val node)))))

(defun progress-format-number (n)
  "Format a double-float as a JS number string (no trailing '.0' for integers)."
  (let* ((s (format nil "~f" n))
         (dot (position #\. s)))
    (if (and dot (loop for i from (1+ dot) below (length s)
                       always (char= (char s i) #\0)))
        (subseq s 0 dot)
        s)))

(defun install-forms-progress (ctx ep)
  (declare (ignorable ctx ep))
  (macrolet ((n (this) `(require-node ctx ,this)))
    (defgetset-for ctx ep "progress" "value" (this)
      (fdt-num (progress-value-val (n this)))
      (v)
      (let* ((node (n this))
             (d (js:to-number v))
             (val (if (or (sb-ext:float-nan-p d) (minusp d)) 0d0 d))
             (mx (progress-max-val node))
             (clamped (max 0d0 (min val mx))))
        (set-attr node "value" (progress-format-number clamped))
        (setf (context-dirty ctx) t)))

    (defgetset-for ctx ep "progress" "max" (this)
      (fdt-num (progress-max-val (n this)))
      (v)
      (let* ((node (n this))
             (d (js:to-number v)))
        (unless (or (sb-ext:float-nan-p d) (<= d 0d0))
          (set-attr node "max" (progress-format-number d))
          (setf (context-dirty ctx) t))))

    (defget-for ctx ep "progress" "position" (this)
      (let* ((node (n this))
             (has-value (get-attr node "value")))
        (if (null has-value)
            (num -1d0)
            (let ((v (progress-value-val node))
                  (m (progress-max-val node)))
              (fdt-num (/ v m))))))))

(register-element-proto-extension :progress #'install-forms-progress)