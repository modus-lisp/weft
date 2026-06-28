;;;; src/encoding/kernel.lisp — the decoder registry + dispatch (WHATWG Encoding).
;;;;
;;;; Each decoder is a function (byte-vector) -> string, decoding with the
;;;; "replacement" error mode (ill-formed input -> U+FFFD).  Registered under its
;;;; canonical WHATWG label.  This is the serial kernel every decoder binds to.
(in-package #:weft.encoding)

(defconstant +replacement+ #\Replacement_Character)   ; U+FFFD

(defvar *decoders* (make-hash-table :test 'equal)
  "Canonical encoding label -> decoder function (byte-vector -> string).")

(defun register-decoder (label fn) (setf (gethash label *decoders*) fn))

(defmacro define-decoder (label (bytes-var) &body body)
  "Define and register a decoder for LABEL.  BYTES-VAR is bound to a vector of
octets; the body returns a string."
  `(register-decoder ,label (lambda (,bytes-var)
                              (declare (ignorable ,bytes-var))
                              ,@body)))

(defun normalize-label (label)
  "WHATWG label normalization: strip leading/trailing whitespace, lowercase."
  (string-downcase
   (string-trim '(#\Tab #\Newline #\Return #\Page #\Space) label)))

(defun get-decoder (label) (gethash (normalize-label label) *decoders*))

(defun decode (label bytes)
  "Decode BYTES (a sequence of octets) per encoding LABEL.  Returns a string."
  (let ((fn (get-decoder label)))
    (unless fn (error "weft.encoding: no decoder for label ~s" label))
    (funcall fn (coerce bytes '(simple-array (unsigned-byte 8) (*))))))
