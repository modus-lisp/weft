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

;;; ---- transport: a peer that closes before answering -------------------------
;;;
;;; The pool retries a *reused* socket, which covers a server that timed the socket
;;; out.  It did not cover the other shape: a reverse tunnel or load balancer that
;;; drops the FIRST connection after an idle period.  That arrives on a fresh socket,
;;; so the request died with no status line and the caller saw a hard error on a URL
;;; that works when you try it again — which, upstream in an app, looks like a button
;;; that silently does nothing.
;;;
;;; A loopback server that drops one connection and then answers pins both halves: the
;;; idempotent GET must come back with the answer, and a POST must NOT be re-sent.

(defun listen-loopback ()
  "A listening socket on 127.0.0.1 and the port the kernel picked."
  (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address s) t)
    (sb-bsd-sockets:socket-bind s #(127 0 0 1) 0)
    (sb-bsd-sockets:socket-listen s 5)
    (values s (nth-value 1 (sb-bsd-sockets:socket-name s)))))

(defun drop-then-answer (listener drops body)
  "Accept and close DROPS connections without a word, then answer one request."
  (sb-thread:make-thread
   (lambda ()
     (ignore-errors
      (dotimes (i drops)
        (sb-bsd-sockets:socket-close (sb-bsd-sockets:socket-accept listener)))
      (let* ((c (sb-bsd-sockets:socket-accept listener))
             (s (sb-bsd-sockets:socket-make-stream c :input t :output t
                                                     :element-type 'character
                                                     :external-format :latin-1)))
        (loop for line = (read-line s nil nil)
              while (and line (plusp (length (string-right-trim '(#\Return) line)))))
        (format s "HTTP/1.1 200 OK~c~cContent-Length: ~d~c~cConnection: close~c~c~c~c~a"
                #\Return #\Newline (length body) #\Return #\Newline #\Return #\Newline
                #\Return #\Newline body)
        (force-output s)
        (close s))))
   :name "fetch-test drop-then-answer"))

(defun check-transport (chk)
  (multiple-value-bind (l port) (listen-loopback)
    (unwind-protect
         (progn
           (drop-then-answer l 1 "hi")
           (let ((r (ignore-errors (f:fetch (format nil "http://127.0.0.1:~d/x" port)))))
             (funcall chk "a GET survives a peer that drops the first connection"
                      (and r (= (f:response-status r) 200)
                           (string= (map 'string #'code-char (f:response-body r)) "hi")))))
      (ignore-errors (sb-bsd-sockets:socket-close l))))
  ;; The other half: re-sending a POST could repeat whatever it did, so it must not be
  ;; retried even though nothing was answered.  The caller gets the condition.
  (multiple-value-bind (l port) (listen-loopback)
    (unwind-protect
         (progn
           (drop-then-answer l 1 "hi")
           (funcall chk "a POST is NOT re-sent — the caller is told instead"
                    (typep (nth-value 1 (ignore-errors
                                         (f:fetch (format nil "http://127.0.0.1:~d/x" port)
                                                  :method "POST")))
                           'weft.fetch::connection-closed-early)))
      (ignore-errors (sb-bsd-sockets:socket-close l)))))

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
      (chk "header-ci" (string= (f:get-header '(("Content-Type" . "x")) "content-type") "x"))
      (format t "~%transport (a peer that closes before answering):~%")
      (check-transport (lambda (name good)
                         (if good (progn (incf pass) (format t "  ok   ~a~%" name))
                             (progn (incf fail) (format t "  FAIL ~a~%" name))))))
    (format t "~%~d passed, ~d failed~%" pass fail)
    (values pass fail)))
