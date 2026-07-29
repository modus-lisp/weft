;;;; forms-valuemode.lisp — the <input> VALUE MODE (wave-9 swarm unit).
;;;;
;;;; Self-registering feature file (see core.lisp *element-proto-extensions*).
;;;; Loads BEFORE forms-constraints so the constraint code can call the
;;;; sanitizer defined here as an ordinary top-level function.
;;;;
;;;; What this file is for
;;;; ---------------------
;;;; HTML gives every input type one of four VALUE MODES — value, default,
;;;; default/on, filename — and the mode decides what the `value' IDL attribute
;;;; reads and writes, whether there is a dirty value flag, and what a clone
;;;; keeps.  Layered on top is the VALUE SANITIZATION ALGORITHM: a per-type
;;;; rewrite of the stored string (text strips CR/LF, url and email also strip
;;;; leading/trailing whitespace, the temporal and numeric types discard
;;;; anything they cannot parse, color normalises, and so on).
;;;;
;;;; The sanitizer must run when the value is set AND AGAIN WHENEVER THE TYPE
;;;; ATTRIBUTE CHANGES — that second half is the whole of type-change-state.html
;;;; (380 subtests: the full old-type x new-type cross product).
;;;;
;;;; Take over the accessors with the `-for' variants, exactly as the sibling
;;;; feature files do; the previous accessor from dom.lisp remains reachable
;;;; through PREVIOUS-ACCESSOR, so this file never has to edit shared core.
(in-package #:weft.script)

;;; Types that undergo value sanitization
(defparameter +valuemode-sanitized-types+
  '("text" "search" "tel" "password" "url" "email"
    "number" "range" "date" "month" "week" "time"
    "datetime-local" "color"))

;;; ---- value sanitization algorithm (HTML §4.10.5.3) -------------------------

(defun valuemode-strip-crlf (s)
  "Strip U+000A (LF) and U+000D (CR) from S."
  (remove-if (lambda (c) (or (char= c #\Newline) (char= c #\Return))) s))

(defun valuemode-trim-ascii-ws (s)
  "Trim leading/trailing ASCII whitespace from S."
  (string-trim '(#\Space #\Tab #\Newline #\Return #\Page) s))

(defun fdt-step-align-range (value step base)
  "Align VALUE to the nearest STEP multiple from BASE.
   Uses >= step/2 rounding (rounds up when remainder >= step/2)."
  (let* ((diff (- value base))
         (rem (mod diff step))
         (half (/ step 2)))
    (if (>= rem half)
        (+ value (- step rem))
        (- value rem))))

(defun valuemode-range-default (node)
  "Range's DEFAULT VALUE: the minimum plus half the difference between the
   minimum and the maximum, or the minimum when the maximum is below it.
   min defaults to 0 and max to 100, so the bare case is \"50\" — but a range
   with min/max attributes has a different default, and hardcoding 50 there
   silently invents a value the author never wrote.
   The default is then aligned to the step (default 1) from the step base."
  (let* ((min (or (and node (fdt-parse-float (or (get-attr node "min") ""))) 0))
         (max (or (and node (fdt-parse-float (or (get-attr node "max") ""))) 100))
         (raw-default (if (< max min) min (+ min (/ (- max min) 2))))
         (step-attr (and node (get-attr node "step")))
         (step (if (and step-attr (fdt-parse-float step-attr))
                   (fdt-parse-float step-attr)
                   1.0d0))
         (step-base (if (and node (get-attr node "min")
                             (fdt-parse-float (get-attr node "min")))
                        min 0.0d0))
         (aligned (fdt-step-align-range raw-default step step-base))
         (final (max (min aligned max) min)))
    (fdt-number->value "range" final)))

(defun valuemode-sanitize (type v &optional node)
  "HTML value sanitization algorithm.  Returns the sanitized string.
   Only call this for types in +valuemode-sanitized-types+.  NODE is needed
   only by range, whose default value comes from its min/max attributes."
  (cond
    ;; text, search, tel, password: strip CR/LF only
    ((member type '("text" "search" "tel" "password") :test #'string=)
     (valuemode-strip-crlf v))
    ;; url: trim ASCII whitespace then strip CR/LF
    ((string= type "url")
     (valuemode-strip-crlf (valuemode-trim-ascii-ws v)))
    ;; email: same as url
    ((string= type "email")
     (valuemode-strip-crlf (valuemode-trim-ascii-ws v)))
    ;; number: if empty or not a valid float → ""
    ((string= type "number")
     (if (or (string= v "") (not (fdt-parse-float v))) "" v))
    ;; range: anything that is not a valid floating-point number becomes the
    ;; default value, which is derived from min/max (50 only by their defaults).
    ;; Otherwise, clamp to min/max and align to step via the step-up algorithm.
    ((string= type "range")
     (if (or (string= v "") (not (fdt-parse-float v)))
         (valuemode-range-default node)
         (let* ((val (fdt-parse-float v))
                (min (or (and node (fdt-parse-float (or (get-attr node "min") ""))) 0))
                (max (or (and node (fdt-parse-float (or (get-attr node "max") ""))) 100))
                (step-attr (and node (get-attr node "step")))
                (step (if (and step-attr (fdt-parse-float step-attr))
                          (fdt-parse-float step-attr) 1.0d0))
                (step-base (if (and node (get-attr node "min")
                                    (fdt-parse-float (get-attr node "min")))
                               min 0.0d0))
                ;; Clamp to [min, max]
                (clamped (max (min val max) min))
                ;; Align to step
                (aligned (fdt-step-align-range clamped step step-base)))
           ;; Re-clamp aligned value to [min, max]
           (let ((final (max (min aligned max) min)))
             (fdt-number->value "range" final)))))
    ;; date: if empty or not a valid date → ""
    ((string= type "date")
     (if (or (string= v "") (not (fdt-parse-date v))) "" v))
    ;; month: if empty or not valid → ""
    ((string= type "month")
     (if (or (string= v "") (not (fdt-parse-month v))) "" v))
    ;; week: if empty or not valid → ""
    ((string= type "week")
     (if (or (string= v "") (not (fdt-parse-week v))) "" v))
    ;; time: if empty or not valid → ""
    ((string= type "time")
     (if (or (string= v "") (not (fdt-parse-hms v))) "" v))
    ;; datetime-local: if empty or not valid → ""
    ;; Also normalize separator: space → "T" before returning
    ((string= type "datetime-local")
     (if (or (string= v "") (not (fdt-parse-datetime-local v)))
         ""
         (let* ((tpos (or (position #\T v) (position #\Space v)))
                (date-s (subseq v 0 tpos))
                (time-s (subseq v (1+ tpos)))
                (day (fdt-parse-date date-s))
                (tod (fdt-parse-hms time-s)))
           (if (and day tod)
               (format nil "~aT~a" (fdt-fmt-date-ed day) (fdt-fmt-time-ms tod))
               v))))
    ;; color: use the CSS parser; valid → "#rrggbb", invalid → "#000000"
    ((string= type "color")
     (let* ((c (css:parse-value "color" v))
            (r (and (consp c) (not (eq c :invalid)) (first c)))
            (g (and (consp c) (not (eq c :invalid)) (second c)))
            (b (and (consp c) (not (eq c :invalid)) (third c))))
       (if (and r g b)
           (string-downcase (format nil "#~2,'0x~2,'0x~2,'0x" r g b))
           "#000000")))
    (t v)))

;;; ---- re-sanitize the stored value when type changes -------------------------

(defun valuemode-re-sanitize (ctx node)
  "Re-run the value sanitization algorithm on NODE's value.
   Handles both context-input-values and attribute origins."
  (let* ((type (input-type node))
         (raw (multiple-value-bind (v present) (gethash node (context-input-values ctx))
                (if present v (get-attr node "value")))))
    (cond
      ;; File type: value is always "" (filename mode)
      ((string= type "file")
       (setf (gethash node (context-input-values ctx)) ""))
      ;; Checkbox/radio: default to "on" if no dirty value attribute
      ((member type '("checkbox" "radio") :test #'string=)
       ;; If the stored value is empty, use the default "on" value
       (if (or (null raw) (string= raw ""))
           (remhash node (context-input-values ctx))
           nil))
      ;; Hidden/submit/image/reset/button: keep the raw value (no sanitization)
      ((member type '("hidden" "submit" "image" "reset" "button") :test #'string=)
       nil)
      ;; Sanitized types
      ((member type +valuemode-sanitized-types+ :test #'string=)
       (let ((cleaned (valuemode-sanitize type (or raw "") node)))
         (unless (string= (or raw "") cleaned)
           (setf (gethash node (context-input-values ctx)) cleaned))))
      (t nil))))

;;; -----------------------------------------------------------------------------

(defun install-forms-valuemode (ctx ep)
  (declare (ignorable ctx ep))
  ;; Install the value accessor for <input> with per-type sanitization on set.
  (defgetset-for ctx ep "input" "value" (this)
    ;; Getter
    (let* ((node (require-node ctx this))
           (type (input-type node)))
      (cond
        ((string= type "file")
         "")
        ((member type '("checkbox" "radio") :test #'string=)
         (multiple-value-bind (v present) (gethash node (context-input-values ctx))
           (if present v (or (get-attr node "value") "on"))))
        (t
         (multiple-value-bind (v present) (gethash node (context-input-values ctx))
           (if present v
               (let ((raw (or (get-attr node "value") "")))
                 (if (member type +valuemode-sanitized-types+ :test #'string=)
                     (valuemode-sanitize type raw node)
                     raw)))))))
    (v)
    ;; Setter — sanitize the value before storing
    (let* ((node (require-node ctx this))
           (raw (if (eq v js:*null*) "" (jstr v)))
           (type (input-type node)))
      (cond
        ((string= type "file")
         (if (string= raw "")
             (setf (gethash node (context-input-values ctx)) "")
             (throw-dom ctx "InvalidStateError" 11 "Cannot set value on a file input")))
        ((member type +valuemode-sanitized-types+ :test #'string=)
         (let ((cleaned (valuemode-sanitize type raw node)))
           (setf (gethash node (context-input-values ctx)) cleaned)))
        (t
         ;; For non-sanitized types (hidden, checkbox, radio, submit, etc.),
         ;; store the raw value as-is
         (setf (gethash node (context-input-values ctx)) raw))))))

(register-element-proto-extension :valuemode #'install-forms-valuemode)