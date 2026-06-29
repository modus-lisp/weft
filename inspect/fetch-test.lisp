;;;; inspect/fetch-test.lisp — offline gate for the resource loader.
;;;;
;;;; Vendored vectors under inspect/vectors/fetch/ are real-world compressed
;;;; bodies (python gzip / zlib / brotli + zstd) and charset-declared bodies.
;;;; BODY-TEXT must turn each (headers + raw body) into the expected UTF-8 text,
;;;; exercising Content-Encoding removal (our pure-CL codecs) and charset decode.
(defpackage #:weft.fetch.test
  (:use #:cl) (:local-nicknames (#:f #:weft.fetch)) (:export #:run))
(in-package #:weft.fetch.test)

(defparameter *ce-cases*
  '(("sample.gz"   "gzip"    "sample.txt")
    ("sample.zlib" "deflate" "sample.txt")
    ("sample.br"   "br"      "sample.txt")
    ("sample.zst"  "zstd"    "sample.txt")))

(defparameter *cs-cases*
  '(("cs_windows-1252.bin" "windows-1252" "cs_windows-1252.txt")
    ("cs_shift_jis.bin"    "shift_jis"    "cs_shift_jis.txt")
    ("cs_utf-16le.bin"     "utf-16le"     "cs_utf-16le.txt")
    ("cs_utf-8-bom.bin"    "utf-8"        "cs_utf-8-bom.txt")))

(defun vec (name) (asdf:system-relative-pathname "weft" (format nil "inspect/vectors/fetch/~a" name)))
(defun read-bytes (name)
  (with-open-file (s (vec name) :element-type '(unsigned-byte 8))
    (let ((b (make-array (file-length s) :element-type '(unsigned-byte 8)))) (read-sequence b s) b)))
(defun read-utf8 (name)
  (with-open-file (s (vec name) :external-format :utf-8)
    (let ((str (make-string (file-length s)))) (subseq str 0 (read-sequence str s)))))

(defun run ()
  (let ((pass 0) (fail 0))
    (format t "~&=== weft fetch gate (Content-Encoding + charset) ===~%")
    (format t "~%Content-Encoding (decode real gzip/deflate/br/zstd bodies):~%")
    (dolist (c *ce-cases*)
      (destructuring-bind (file ce expect) c
        (let* ((headers (list (cons "Content-Encoding" ce)
                              (cons "Content-Type" "text/plain; charset=utf-8")))
               (got (ignore-errors (f:body-text headers (read-bytes file))))
               (want (read-utf8 expect)))
          (if (and (stringp got) (string= got want)) (progn (incf pass) (format t "  ok   ~a (~a)~%" file ce))
              (progn (incf fail) (format t "  FAIL ~a (~a): ~d vs ~d chars~%" file ce
                                         (and (stringp got) (length got)) (length want)))))))
    (format t "~%charset (Content-Type / BOM):~%")
    (dolist (c *cs-cases*)
      (destructuring-bind (file charset expect) c
        (let* ((headers (list (cons "Content-Type" (format nil "text/plain; charset=~a" charset))))
               (got (ignore-errors (f:body-text headers (read-bytes file))))
               (want (read-utf8 expect)))
          (if (and (stringp got) (string= got want)) (progn (incf pass) (format t "  ok   ~a (~a)~%" file charset))
              (progn (incf fail) (format t "  FAIL ~a (~a)~%" file charset))))))
    ;; a couple of unit checks on the helpers
    (flet ((chk (name good) (if good (incf pass) (progn (incf fail) (format t "  FAIL ~a~%" name)))))
      (chk "charset-parse" (string-equal (f:content-type-charset "text/html; charset=ISO-8859-2") "ISO-8859-2"))
      (chk "header-ci" (string= (f:get-header '(("Content-Type" . "x")) "content-type") "x")))
    (format t "~%~d passed, ~d failed~%" pass fail)
    (values pass fail)))
