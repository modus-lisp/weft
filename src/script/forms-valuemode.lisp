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

(defun valuemode-range-default (node)
  "Range's DEFAULT VALUE: the minimum plus half the difference between the
   minimum and the maximum, or the minimum when the maximum is below it.
   min defaults to 0 and max to 100, so the bare case is \"50\" — but a range
   with min/max attributes has a different default, and hardcoding 50 there
   silently invents a value the author never wrote."
  (let* ((min (or (and node (fdt-parse-float (or (get-attr node "min") ""))) 0))
         (max (or (and node (fdt-parse-float (or (get-attr node "max") ""))) 100))
         (default (if (< max min) min (+ min (/ (- max min) 2)))))
    (fdt-number->value "range" default)))

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
    ((string= type "range")
     (if (or (string= v "") (not (fdt-parse-float v))) (valuemode-range-default node) v))
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
    ((string= type "datetime-local")
     (if (or (string= v "") (not (fdt-parse-datetime-local v))) "" v))
    ;; color: valid lowercase simple color → lowercase; invalid → "#000000"
    ((string= type "color")
     (if (and (= (length v) 7) (char= (char v 0) #\#)
              (loop for i from 1 to 6 always (digit-char-p (char v i) 16)))
         (string-downcase v)
         "#000000"))
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
           (if present v (or (get-attr node "value") ""))))))
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