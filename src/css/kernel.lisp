;;;; src/css/kernel.lisp — value-parser registry (the serial kernel the swarm binds to).
;;;;
;;;; Each value parser is a function (string) -> typed value or :invalid, keyed by
;;;; a CSS value-type name ("color", "length", ...).  Mirrors the encoding kernel.
(in-package #:weft.css)

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
