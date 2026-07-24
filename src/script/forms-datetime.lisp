;;;; forms-datetime.lisp — shared date/number machinery for the HTMLInputElement
;;;; value-conversion IDL surface (valueAsNumber, valueAsDate, stepUp/stepDown).
;;;;
;;;; Every type-specific input (date/month/week/time/datetime-local/number/range)
;;;; defines an "algorithm to convert a string to a number" and its inverse.  This
;;;; file implements those once, in exact integer/rational arithmetic (CL bignums
;;;; = JS BigInt-exact for the ms ranges involved), plus the ISO-8601 week calendar
;;;; and the per-type step parameters.  The three feature files are thin wrappers.
;;;; All helpers are prefixed FDT-.  (See HTML §the-input-element, §common-input-apis.)
(in-package #:weft.script)

;;; ---- a quiet NaN double (tests use Number.isNaN, so it must be a real NaN) ---
(defparameter +fdt-nan+ (sb-int:with-float-traps-masked (:invalid)
                          (- sb-ext:double-float-positive-infinity
                             sb-ext:double-float-positive-infinity)))
(defun fdt-nan () +fdt-nan+)
(defun fdt-num (x) "X (integer/rational/double) as a JS number value." (num (coerce x 'double-float)))

;;; ---- integer proleptic-Gregorian calendar (Howard Hinnant's algorithms) -----
(defun fdt-days-from-civil (y m d)
  "Days from 1970-01-01 to Y-M-D (may be negative)."
  (let* ((y (if (<= m 2) (1- y) y))
         (era (floor (if (>= y 0) y (- y 399)) 400))
         (yoe (- y (* era 400)))
         (doy (+ (floor (+ (* 153 (if (> m 2) (- m 3) (+ m 9))) 2) 5) (1- d)))
         (doe (+ (* yoe 365) (floor yoe 4) (- (floor yoe 100)) doy)))
    (+ (* era 146097) doe -719468)))

(defun fdt-civil-from-days (z)
  "(values Y M D) for the day Z days from 1970-01-01."
  (let* ((z (+ z 719468))
         (era (floor (if (>= z 0) z (- z 146096)) 146097))
         (doe (- z (* era 146097)))
         (yoe (floor (+ (- doe (floor doe 1460)) (floor doe 36524) (- (floor doe 146096))) 365))
         (y (+ yoe (* era 400)))
         (doy (- doe (+ (* 365 yoe) (floor yoe 4) (- (floor yoe 100)))))
         (mp (floor (+ (* 5 doy) 2) 153))
         (d (+ (- doy (floor (+ (* 153 mp) 2) 5)) 1))
         (m (if (< mp 10) (+ mp 3) (- mp 9))))
    (values (if (<= m 2) (1+ y) y) m d)))

(defun fdt-leap-p (y) (and (zerop (mod y 4)) (or (not (zerop (mod y 100))) (zerop (mod y 400)))))
(defun fdt-mdays (y m) (aref (if (fdt-leap-p y) #(31 29 31 30 31 30 31 31 30 31 30 31)
                                 #(31 28 31 30 31 30 31 31 30 31 30 31)) (1- m)))
(defun fdt-iso-dow (ed) "ISO weekday of epoch-day ED: Mon=1 .. Sun=7." (1+ (mod (+ ed 3) 7)))

(defun fdt-weeks-in-iso-year (y)
  (let ((jan1-dow (fdt-iso-dow (fdt-days-from-civil y 1 1))))
    (if (or (= jan1-dow 4) (and (fdt-leap-p y) (= jan1-dow 3))) 53 52)))

(defun fdt-iso-week-monday (y w)
  "Epoch-day of the Monday of ISO year-week Y-Www (assumes W already validated)."
  (let* ((jan4 (fdt-days-from-civil y 1 4))
         (week1-mon (- jan4 (1- (fdt-iso-dow jan4)))))
    (+ week1-mon (* (1- w) 7))))

(defun fdt-iso-year-week (ed)
  "(values ISO-YEAR ISO-WEEK) for epoch-day ED."
  (let* ((thursday (+ ed (- 4 (fdt-iso-dow ed)))))
    (multiple-value-bind (y m d) (fdt-civil-from-days thursday)
      (declare (ignore m d))
      (values y (1+ (floor (- thursday (fdt-days-from-civil y 1 1)) 7))))))

;;; ---- small parse helpers ---------------------------------------------------
(defun fdt-all-digits-p (s a b) (and (< a b) (loop for i from a below b always (digit-char-p (char s i)))))
(defun fdt-int (s a b) (parse-integer s :start a :end b))

(defun fdt-parse-date (s)
  "\"YYYY-MM-DD\" -> epoch-day, or NIL if not a valid date string."
  (let ((n (length s)))
    (when (and (>= n 10) (= n 10) (char= (char s 4) #\-) (char= (char s 7) #\-)
               (fdt-all-digits-p s 0 4) (fdt-all-digits-p s 5 7) (fdt-all-digits-p s 8 10))
      (let ((y (fdt-int s 0 4)) (m (fdt-int s 5 7)) (d (fdt-int s 8 10)))
        (when (and (>= y 1) (<= 1 m 12) (<= 1 d (fdt-mdays y m)))
          (fdt-days-from-civil y m d))))))

(defun fdt-parse-month (s)
  "\"YYYY-MM\" -> months since 1970-01, or NIL."
  (when (and (= (length s) 7) (char= (char s 4) #\-)
             (fdt-all-digits-p s 0 4) (fdt-all-digits-p s 5 7))
    (let ((y (fdt-int s 0 4)) (m (fdt-int s 5 7)))
      (when (and (>= y 1) (<= 1 m 12)) (+ (* (- y 1970) 12) (1- m))))))

(defun fdt-parse-week (s)
  "\"YYYY-Www\" -> epoch-ms of the Monday, or NIL."
  (when (and (= (length s) 8) (char= (char s 4) #\-) (char-equal (char s 5) #\W)
             (fdt-all-digits-p s 0 4) (fdt-all-digits-p s 6 8))
    (let ((y (fdt-int s 0 4)) (w (fdt-int s 6 8)))
      (when (and (>= y 1) (<= 1 w (fdt-weeks-in-iso-year y)))
        (* (fdt-iso-week-monday y w) 86400000)))))

(defun fdt-parse-hms (s)
  "\"HH:MM\" / \"HH:MM:SS\" / \"HH:MM:SS.fff\" -> ms of day, or NIL."
  (let ((n (length s)))
    (when (and (>= n 5) (char= (char s 2) #\:)
               (fdt-all-digits-p s 0 2) (fdt-all-digits-p s 3 5))
      (let ((h (fdt-int s 0 2)) (mi (fdt-int s 3 5)) (se 0) (ms 0))
        (when (and (<= 0 h 23) (<= 0 mi 59))
          (cond
            ((= n 5))                                       ; HH:MM
            ((and (>= n 8) (char= (char s 5) #\:) (fdt-all-digits-p s 6 8))
             (setf se (fdt-int s 6 8))
             (unless (<= 0 se 59) (return-from fdt-parse-hms nil))
             (cond ((= n 8))                                ; HH:MM:SS
                   ((and (> n 9) (char= (char s 8) #\.) (fdt-all-digits-p s 9 n))
                    (let ((frac (subseq s 9 (min n 12))))   ; up to ms precision
                      (setf ms (* (fdt-int frac 0 (length frac))
                                  (aref #(0 100 10 1) (length frac))))))
                   (t (return-from fdt-parse-hms nil))))
            (t (return-from fdt-parse-hms nil)))
          (+ (* h 3600000) (* mi 60000) (* se 1000) ms))))))

(defun fdt-parse-datetime-local (s)
  "\"YYYY-MM-DDTHH:MM[:SS[.fff]]\" -> epoch-ms (local as UTC), or NIL."
  (let ((tpos (or (position #\T s) (position #\Space s))))
    (when (and tpos (= tpos 10))
      (let ((day (fdt-parse-date (subseq s 0 10)))
            (tod (fdt-parse-hms (subseq s 11))))
        (when (and day tod) (+ (* day 86400000) tod))))))

;;; ---- formatters (number/ms -> value string) --------------------------------
(defun fdt-pad (n w) (format nil "~v,'0d" w n))

(defun fdt-fmt-date-ed (ed)
  (multiple-value-bind (y m d) (fdt-civil-from-days ed)
    (format nil "~a-~a-~a" (fdt-pad y 4) (fdt-pad m 2) (fdt-pad d 2))))

(defun fdt-fmt-time-ms (ms)
  "ms of day -> shortest valid time string."
  (let* ((h (floor ms 3600000)) (r (mod ms 3600000))
         (mi (floor r 60000)) (r2 (mod r 60000))
         (se (floor r2 1000)) (frac (mod r2 1000)))
    (cond ((plusp frac) (format nil "~a:~a:~a.~a" (fdt-pad h 2) (fdt-pad mi 2) (fdt-pad se 2) (fdt-pad frac 3)))
          ((plusp se)   (format nil "~a:~a:~a" (fdt-pad h 2) (fdt-pad mi 2) (fdt-pad se 2)))
          (t            (format nil "~a:~a" (fdt-pad h 2) (fdt-pad mi 2))))))

;;; ---- per-type "convert to/from number" (valueAsNumber space) ----------------
(defparameter +fdt-number-types+ '("number" "range" "date" "month" "week" "time" "datetime-local"))
(defun fdt-supports-number-p (type) (member type +fdt-number-types+ :test #'string=))

;;; valid-floating-point-number (HTML) -> double, or NIL.
(defun fdt-parse-float (s)
  (let ((n (length s)) (i 0))
    (when (zerop n) (return-from fdt-parse-float nil))
    (when (or (char= (char s 0) #\-) (char= (char s 0) #\+)) (incf i))
    (let ((int-start i))
      (loop while (and (< i n) (digit-char-p (char s i))) do (incf i))
      (let ((had-int (> i int-start)) (had-frac nil))
        (when (and (< i n) (char= (char s i) #\.))
          (incf i) (let ((fs i)) (loop while (and (< i n) (digit-char-p (char s i))) do (incf i))
                     (setf had-frac (> i fs))))
        (unless (or had-int had-frac) (return-from fdt-parse-float nil))
        (when (and (< i n) (or (char= (char s i) #\e) (char= (char s i) #\E)))
          (incf i) (when (and (< i n) (or (char= (char s i) #\+) (char= (char s i) #\-))) (incf i))
          (let ((es i)) (loop while (and (< i n) (digit-char-p (char s i))) do (incf i))
            (when (= i es) (return-from fdt-parse-float nil))))
        (when (= i n)
          (let ((*read-default-float-format* 'double-float))
            (ignore-errors (coerce (read-from-string s) 'double-float))))))))

(defun fdt-value->number (type valuestr)
  "The type's 'convert a string to a number'.  Returns a double, or :nan."
  (flet ((ms-week (v) v) (or-nan (x scale) (if x (* x scale) :nan)))
    (cond
      ((or (string= type "number") (string= type "range"))
       (or (fdt-parse-float valuestr) :nan))
      ((string= type "date")     (or-nan (fdt-parse-date valuestr) 86400000))
      ((string= type "month")    (let ((m (fdt-parse-month valuestr))) (if m m :nan)))
      ((string= type "week")     (let ((v (fdt-parse-week valuestr))) (ms-week (or v :nan))))
      ((string= type "time")     (let ((v (fdt-parse-hms valuestr))) (if v v :nan)))
      ((string= type "datetime-local") (let ((v (fdt-parse-datetime-local valuestr))) (if v v :nan)))
      (t :nan))))

(defun fdt-number->value (type x)
  "The type's inverse: a real X (in valueAsNumber space) -> a value string, or \"\"
   when X does not correspond to a representable value for TYPE."
  (handler-case
      (cond
        ((or (string= type "number") (string= type "range"))
         (js:to-string (num (coerce x 'double-float))))
        ((string= type "date")  (fdt-fmt-date-ed (floor x 86400000)))
        ((string= type "month")
         (let ((mo (floor x))) (multiple-value-bind (dy dm) (floor mo 12)
                                 (format nil "~a-~a" (fdt-pad (+ 1970 dy) 4) (fdt-pad (1+ dm) 2)))))
        ((string= type "week")
         (multiple-value-bind (y w) (fdt-iso-year-week (floor x 86400000))
           (format nil "~a-W~a" (fdt-pad y 4) (fdt-pad w 2))))
        ((string= type "time") (fdt-fmt-time-ms (mod (floor x) 86400000)))
        ((string= type "datetime-local")
         (if (> (abs x) 8.64d15) ""
             (let ((ms (floor x)))
               (format nil "~aT~a" (fdt-fmt-date-ed (floor ms 86400000)) (fdt-fmt-time-ms (mod ms 86400000))))))
        (t ""))
    (error () "")))

;;; ---- per-type valueAsDate (epoch-ms <-> value; date/month/week/time only) ----
(defparameter +fdt-date-types+ '("date" "month" "week" "time"))
(defun fdt-supports-date-p (type) (member type +fdt-date-types+ :test #'string=))

(defun fdt-value->ms (type valuestr)
  "Epoch-ms for the value, or NIL.  For valueAsDate: a Date at that UTC instant."
  (cond
    ((string= type "date")  (let ((ed (fdt-parse-date valuestr))) (and ed (* ed 86400000))))
    ((string= type "month") (let ((m (fdt-parse-month valuestr)))
                              (and m (multiple-value-bind (dy dm) (floor m 12)
                                       (* (fdt-days-from-civil (+ 1970 dy) (1+ dm) 1) 86400000)))))
    ((string= type "week")  (fdt-parse-week valuestr))
    ((string= type "time")  (fdt-parse-hms valuestr))
    (t nil)))

(defun fdt-ms->value-date (type ms)
  "Value string for an epoch-ms MS (from a Date), for a date-supporting TYPE.
   date/week/time share ms with valueAsNumber space; month does NOT (its number
   space is months-since-1970), so convert it from the epoch day here."
  (if (string= type "month")
      (multiple-value-bind (y m d) (fdt-civil-from-days (floor ms 86400000))
        (declare (ignore d))
        (format nil "~a-~a" (fdt-pad y 4) (fdt-pad m 2)))
      (fdt-number->value type ms)))

;;; ---- step parameters -------------------------------------------------------
(defun fdt-supports-step-p (type)
  (member type '("date" "datetime-local" "month" "number" "range" "time" "week") :test #'string=))
(defun fdt-default-step (type)
  (cond ((member type '("datetime-local" "time") :test #'string=) 60)
        (t 1)))                                          ; date/month/number/range/week
(defun fdt-step-scale (type)
  (cond ((string= type "date") 86400000)
        ((string= type "datetime-local") 1000)
        ((string= type "time") 1000)
        ((string= type "week") 604800000)
        (t 1)))                                          ; month/number/range
(defun fdt-default-step-base (type)
  "Step base when there is no min: 0 for all but week, whose weeks align to the
   Monday of 1970-W01 (epoch-day -3)."
  (if (string= type "week") (* -3 86400000) 0))
