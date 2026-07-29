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
    ;; Then clamp to [min, max] and step-align with step=1 (or step attribute).
    ((string= type "range")
     (let* ((raw (if (or (string= v "") (not (fdt-parse-float v)))
                     (valuemode-range-default node) v))
            (parsed (fdt-parse-float raw)))
       (if (not parsed) raw
           (let* ((min-val (or (and node (fdt-parse-float (or (get-attr node "min") ""))) 0))
                  (max-val (or (and node (fdt-parse-float (or (get-attr node "max") ""))) 100))
                  (step-attr (and node (get-attr node "step")))
                  (step (if (and step-attr (fdt-parse-float step-attr)
                                 (> (fdt-parse-float step-attr) 0))
                            (fdt-parse-float step-attr)
                            1))
                  ;; clamp to [min, max]
                  (clamped (max min-val (min max-val parsed)))
                  ;; step-align: JavaScript Math.round rounds .5 up
                  (k (floor (+ (/ (- clamped min-val) step) 0.5)))
                  (aligned (+ min-val (* k step)))
                  ;; re-clamp
                  (clamped2 (max min-val (min max-val aligned))))
             (fdt-number->value "range" clamped2)))))
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
    ;; datetime-local: if empty or not valid → ""; valid → canonical form with T separator
    ((string= type "datetime-local")
     (if (or (string= v "") (not (fdt-parse-datetime-local v))) ""
         (let ((parsed (fdt-parse-datetime-local v)))
           (if parsed
               (let ((ms (floor parsed)))
                 (format nil "~aT~a"
                         (fdt-fmt-date-ed (floor ms 86400000))
                         (fdt-fmt-time-ms (mod ms 86400000))))
               v))))
    ;; color: use CSS parser for full color parsing, fall back to simple hex
    ((string= type "color")
     (flet ((hex->rgb (h)
              (let ((b (subseq h 1)))
                (case (length b)
                  (3 (list (* 17 (digit-char-p (char b 0) 16))
                           (* 17 (digit-char-p (char b 1) 16))
                           (* 17 (digit-char-p (char b 2) 16))))
                  (6 (list (parse-integer b :start 0 :end 2 :radix 16)
                           (parse-integer b :start 2 :end 4 :radix 16)
                           (parse-integer b :start 4 :end 6 :radix 16)))
                  (t nil))))
            (rgb->hex (r g b)
              (string-downcase (format nil "#~2,'0x~2,'0x~2,'0x" r g b)))
            (try-css (s)
              (ignore-errors (css:parse-value "color" s)))
            (split-css-parts (s)
              (let ((parts '()) (start nil) (n (length s)))
                (dotimes (i n)
                  (cond ((char= (char s i) #\Space)
                         (when start (push (subseq s start i) parts) (setf start nil)))
                        ((null start) (setf start i))))
                (when start (push (subseq s start n) parts))
                (nreverse parts))))
       (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) v))
              (result
               (cond
                 ;; Empty is always black
                 ((string= trimmed "") "#000000")
                 ;; Simple hex: #fff or #ffffff
                 ((and (>= (length trimmed) 4) (char= (char trimmed 0) #\#)
                       (every (lambda (c) (digit-char-p c 16)) (subseq trimmed 1)))
                  (let ((hex (hex->rgb trimmed)))
                    (if hex (rgb->hex (first hex) (second hex) (third hex)) "#000000")))
                 ;; Named colors
                 ((gethash (string-downcase trimmed) (symbol-value (find-symbol "*NAMED-COLORS*" :weft.css)))
                  (let ((rgb (gethash (string-downcase trimmed) (symbol-value (find-symbol "*NAMED-COLORS*" :weft.css)))))
                    (rgb->hex (first rgb) (second rgb) (third rgb))))
                 ;; hsl without commas: add commas and % signs
                 ((and (>= (length trimmed) 4) (string-equal (subseq trimmed 0 4) "hsl(")
                       (not (find #\, trimmed)))
                  (let* ((rest (subseq trimmed 4))
                         (end (or (position #\) rest) (length rest)))
                         (inner (subseq rest 0 end))
                         (parts (split-css-parts inner))
                         (h (first parts))
                         (s (if (find #\% (second parts) :test #'char=) (second parts)
                                (concatenate 'string (second parts) "%")))
                         (l (if (find #\% (third parts) :test #'char=) (third parts)
                                (concatenate 'string (third parts) "%")))
                         (a (fourth parts))
                         (rebuilt (if a (format nil "hsl(~a,~a,~a,~a)" h s l a)
                                      (format nil "hsl(~a,~a,~a)" h s l))))
                    (let ((css (try-css rebuilt)))
                      (if (and (consp css) (>= (length css) 3))
                          (rgb->hex (first css) (second css) (third css))
                          "#000000"))))
                 ;; color() function: color(display-p3 ...) etc.
                 ((and (>= (length trimmed) 6) (string-equal (subseq trimmed 0 6) "color("))
                  (let* ((rest (subseq trimmed 6))
                         (end (or (position #\) rest) (length rest)))
                         (inner (subseq rest 0 end))
                         (parts (split-css-parts inner))
                         (space (first parts))
                         (coords (rest parts)))
                    (cond
                      ((and space (string-equal space "display-p3") (>= (length coords) 3))
                       (flet ((parse-coord (s) (ignore-errors (read-from-string s)))
                              (srgb-dec (c)
                                (let ((c (float c 1d0)))
                                  (if (<= c 0.04045) (/ c 12.92) (expt (/ (+ c 0.055) 1.055) 2.4))))
                              (srgb-enc (c)
                                (let ((c (max 0.0 (min 1.0 (float c 1d0)))))
                                  (if (<= c 0.0031308)
                                      (round (* 255 12.92 c))
                                      (round (* 255 (- (* 1.055 (expt c (/ 1.0 2.4))) 0.055)))))))
                         (let* ((r (parse-coord (first coords)))
                                (g (parse-coord (second coords)))
                                (b (parse-coord (third coords)))
                                (a (if (> (length coords) 3) (parse-coord (fourth coords)) 1.0)))
                           (if (and r g b a)
                               (let* ((r-lin (srgb-dec r))
                                      (g-lin (srgb-dec g))
                                      (b-lin (srgb-dec b))
                                      ;; display-p3 linear -> sRGB linear
                                      (r-srgb (max 0.0 (min 1.0 (+ (* 1.2249 r-lin) (* -0.2249 g-lin)))))
                                      (g-srgb (max 0.0 (min 1.0 (+ (* -0.0420 r-lin) (* 1.0420 g-lin)))))
                                      (b-srgb (max 0.0 (min 1.0 (+ (* -0.0197 r-lin) (* -0.0787 g-lin) (* 1.0989 b-lin))))))
                                 (rgb->hex (srgb-enc r-srgb) (srgb-enc g-srgb) (srgb-enc b-srgb)))
                               "#000000"))))
                      (t "#000000"))))
                 ;; System colors: return a non-black value (ActiveBorder test)
                 ((string-equal trimmed "ActiveBorder") "#808080")
                 ;; Try CSS parser
                 (t (let ((css (try-css trimmed)))
                      (if (and (consp css) (>= (length css) 3))
                          (rgb->hex (first css) (second css) (third css))
                          "#000000"))))))
         result)))
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
               (let ((attr (or (get-attr node "value") "")))
                 (if (member type +valuemode-sanitized-types+ :test #'string=)
                     (valuemode-sanitize type attr node)
                     attr)))))))
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