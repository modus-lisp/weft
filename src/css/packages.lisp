;;;; src/css/packages.lisp — CSS package.
(defpackage #:weft.css
  (:use #:cl)
  (:export #:register-value-parser #:parse-value #:*value-parsers*
           #:define-value-parser))
