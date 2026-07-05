;;;; src/css/kernel.lisp — value-parser registry (the serial kernel the swarm binds to).
;;;;
;;;; Each value parser is a function (string) -> typed value or :invalid, keyed by
;;;; a CSS value-type name ("color", "length", ...).  Mirrors the encoding kernel.
(in-package #:weft.css)

(defun safe-number (str)
  "Parse a CSS numeric literal into a single-float without crashing or evaluating.
Reads with *READ-EVAL* off (a CSS value must never run `#.` code) and as double-float
(so large exponents read at all), then clamps the magnitude to +/-1e7 — far beyond any
real dimension — so an absurd value like `1e308px` neither overflows the reader nor a
later float multiply.  Returns 0.0 for anything unreadable."
  (let ((*read-eval* nil) (*read-default-float-format* 'double-float))
    (handler-case
        (let ((v (read-from-string str nil nil)))
          (cond ((integerp v) (max -10000000 (min 10000000 v)))     ; keep integers integral (z-index, rgb, weight)
                ((realp v) (float (max -1d7 (min 1d7 v)) 1.0))       ; floats -> single, magnitude-clamped
                (t 0.0)))
      (error () 0.0))))

(defvar *value-parsers* (make-hash-table :test 'equal))
(defun register-value-parser (type fn) (setf (gethash type *value-parsers*) fn))

(defmacro define-value-parser (type (s) &body body)
  "Define + register a value parser for TYPE.  S is bound to the input string;
the body returns the parsed value or :invalid."
  `(register-value-parser ,type (lambda (,s) (declare (ignorable ,s)) ,@body)))

(defun parse-value (type string)
  (let ((fn (gethash type *value-parsers*)))
    (unless fn (error "weft.css: no parser for type ~s" type))
    (funcall fn string)))

;;; small shared helpers for value parsers
(defun css-trim (s) (string-trim '(#\Space #\Tab #\Newline #\Return #\Page) s))
(defun ascii-downcase (s) (string-downcase s))
