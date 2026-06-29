;;;; src/fetch.lisp — resource loader: URL -> transport -> content-decode.
;;;;
;;;; Ties P0 together: parse the URL (weft.url), fetch via a pluggable transport
;;;; (default: a curl backend — swap *HTTP-TRANSPORT* for the real TLS/HTTP
;;;; stack), strip Content-Encoding using the pure-CL codecs (brotli-pure,
;;;; zstd-pure, chipz for gzip/deflate), then charset-decode the body with the
;;;; weft.encoding decoders (Content-Type charset / BOM sniff / UTF-8 default).
(defpackage #:weft.fetch
  (:use #:cl)
  (:local-nicknames (#:url #:weft.url) (#:enc #:weft.encoding))
  (:export #:response #:make-response #:response-status #:response-headers
           #:response-body #:response-url
           #:*http-transport* #:fetch #:fetch-text
           #:get-header #:content-type-charset #:decompress-body #:body-text))
(in-package #:weft.fetch)

(defstruct response status headers body url)

;;; ---- header helpers ---------------------------------------------------

(defun get-header (headers name)
  "Case-insensitive header lookup over an alist of (name . value)."
  (cdr (assoc name headers :test #'string-equal)))

(defun split-commas (s)
  (let ((parts '()) (start 0))
    (dotimes (i (length s))
      (when (char= (char s i) #\,) (push (subseq s start i) parts) (setf start (1+ i))))
    (push (subseq s start) parts)
    (mapcar (lambda (x) (string-trim '(#\Space #\Tab) x)) (nreverse parts))))

(defun content-type-charset (ct)
  "Extract the charset parameter from a Content-Type value, or NIL."
  (when ct
    (let ((p (search "charset=" ct :test #'char-equal)))
      (when p
        (let* ((s (+ p 8)) (e (or (position #\; ct :start s) (length ct))))
          (string-trim '(#\Space #\" #\Tab) (subseq ct s e)))))))

;;; ---- Content-Encoding (pure-CL codecs) --------------------------------

(defun %octets (v) (coerce v '(simple-array (unsigned-byte 8) (*))))

(defun decode-one-encoding (token bytes)
  (cond
    ((member token '("gzip" "x-gzip") :test #'string-equal)
     (chipz:decompress nil 'chipz:gzip bytes))
    ((string-equal token "deflate")
     (handler-case (chipz:decompress nil 'chipz:zlib bytes)
       (error () (chipz:decompress nil 'chipz:deflate bytes))))
    ((string-equal token "br") (brotli-pure:decompress bytes))
    ((string-equal token "zstd") (zstd-pure:decompress bytes))
    ((or (string-equal token "identity") (string= token "")) bytes)
    (t (error "weft.fetch: unsupported Content-Encoding ~s" token))))

(defun decompress-body (headers bytes)
  "Strip Content-Encoding from BYTES (codings are decoded in reverse order)."
  (let ((ce (get-header headers "content-encoding")) (b (%octets bytes)))
    (if (null ce) b
        (progn (dolist (tok (reverse (split-commas ce)))
                 (setf b (%octets (decode-one-encoding tok b))))
               b))))

;;; ---- charset selection + decode ---------------------------------------

(defun sniff-bom (bytes)
  "Return (values charset bom-length) from a leading byte-order mark, or NIL."
  (let ((n (length bytes)))
    (cond ((and (>= n 3) (= (aref bytes 0) #xEF) (= (aref bytes 1) #xBB) (= (aref bytes 2) #xBF))
           (values "utf-8" 3))
          ((and (>= n 2) (= (aref bytes 0) #xFF) (= (aref bytes 1) #xFE)) (values "utf-16le" 2))
          ((and (>= n 2) (= (aref bytes 0) #xFE) (= (aref bytes 1) #xFF)) (values "utf-16be" 2))
          (t (values nil 0)))))

(defun body-text (headers bytes)
  "Decompress then charset-decode a response body to a string.
Returns (values string charset-used)."
  (let ((b (decompress-body headers bytes)))
    (multiple-value-bind (bom-cs bom-len) (sniff-bom b)
      (let* ((ct-cs (content-type-charset (get-header headers "content-type")))
             ;; a BOM overrides the declared charset (per the Encoding standard)
             (charset (or bom-cs ct-cs "utf-8")))
        (values (enc:decode charset (if bom-cs (subseq b bom-len) b)) charset)))))

;;; ---- transport (pluggable; default = curl) ----------------------------

(defun read-file-bytes (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((b (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence b s) b)))

(defun parse-curl-headers (text)
  "Parse a curl -D dump (possibly several blocks across redirects); return
(values status alist) from the LAST response block."
  (let ((blocks '()) (cur '()) (status 0))
    (dolist (line (uiop:split-string text :separator '(#\Newline)))
      (setf line (string-right-trim '(#\Return) line))
      (cond ((string= line "") (when cur (push (nreverse cur) blocks) (setf cur '())))
            ((and (>= (length line) 5) (string-equal (subseq line 0 5) "HTTP/"))
             (push (cons :status line) cur))
            (t (let ((c (position #\: line)))
                 (when c (push (cons (string-trim " " (subseq line 0 c))
                                     (string-trim " " (subseq line (1+ c)))) cur))))))
    (when cur (push (nreverse cur) blocks))
    (let ((last (first blocks)))
      (let ((sl (cdr (assoc :status last))))
        (when sl (let ((parts (uiop:split-string sl :separator '(#\Space))))
                   (setf status (or (ignore-errors (parse-integer (second parts))) 0)))))
      (values status (remove :status last :key #'car)))))

(defun curl-transport (method url req-headers)
  "Default transport backend.  Returns a RESPONSE.  Advertises our codecs in
Accept-Encoding so servers may reply with br/zstd/gzip/deflate."
  (uiop:with-temporary-file (:pathname hdrf)
    (uiop:with-temporary-file (:pathname bodyf)
      (let ((args (append (list "curl" "-sL" "--max-time" "30" "-X" method
                                "-D" (namestring hdrf) "-o" (namestring bodyf)
                                "-H" "Accept-Encoding: gzip, deflate, br, zstd")
                          (loop for (k . v) in req-headers append (list "-H" (format nil "~a: ~a" k v)))
                          (list url))))
        (uiop:run-program args :ignore-error-status t :error-output nil)
        (multiple-value-bind (status headers) (parse-curl-headers (uiop:read-file-string hdrf))
          (make-response :status status :headers headers
                         :body (read-file-bytes bodyf) :url url))))))

(defvar *http-transport* #'curl-transport
  "Function (method url req-headers) -> RESPONSE.  Rebind to plug in the real
TLS/HTTP transport in place of the curl backend.")

(defun fetch (url-string &key (method "GET") headers)
  "Fetch URL-STRING.  Returns a RESPONSE (body is the raw, still-encoded bytes)."
  (let ((u (url:parse url-string)))
    (unless u (error "weft.fetch: invalid URL ~s" url-string))
    (funcall *http-transport* method (url:href u) headers)))

(defun fetch-text (url-string &rest args)
  "Fetch and fully decode to text.  Returns (values string charset response)."
  (let ((r (apply #'fetch url-string args)))
    (multiple-value-bind (text cs) (body-text (response-headers r) (response-body r))
      (values text cs r))))
