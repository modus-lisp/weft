;;;; src/fetch.lisp — resource loader: URL -> transport -> content-decode.
;;;;
;;;; Ties the stack together: parse the URL (weft.url), fetch it over a pure-CL
;;;; transport — a raw sb-bsd-sockets stream for http://, the same HTTP/1.1
;;;; exchange run over a seal TLS 1.3 stream (validated cert, :verify t) for
;;;; https:// — following 3xx redirects (http<->https) with a hop limit, then
;;;; strip Content-Encoding using the pure-CL codecs (brotli-pure, zstd-pure,
;;;; chipz for gzip/deflate) and charset-decode the body with the weft.encoding
;;;; decoders (Content-Type charset / BOM sniff / UTF-8 default).  No FFI: seal
;;;; + sb-bsd-sockets only.
(defpackage #:weft.fetch
  (:use #:cl)
  (:local-nicknames (#:url #:weft.url) (#:enc #:weft.encoding))
  (:export #:response #:make-response #:response-status #:response-headers
           #:response-body #:response-url
           #:*http-transport* #:*read-timeout* #:*max-redirects* #:*user-agent*
           #:*progress*
           #:fetch #:fetch-text
           ;; per-context cookie jar — bind *COOKIE-JAR* to isolate a browsing context
           #:cookie-jar #:make-cookie-jar #:cookie-jar-cookies #:*cookie-jar* #:*enable-cookies*
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

;;; ---- transport (pure CL: raw socket for http, seal TLS for https) -----
;;;;
;;;; The transport is one HTTP/1.1 request/response exchange over a bidirectional
;;;; binary stream.  The ONLY thing that differs between http:// and https:// is
;;;; how that stream is opened — a raw sb-bsd-sockets stream, or a seal TLS 1.3
;;;; stream (seal:make-tls-stream) over the same socket machinery — so the
;;;; request/response code below is shared verbatim.  Redirects are handled a
;;;; layer up, in FETCH.

(defvar *read-timeout* 30
  "Per-connection socket read timeout, seconds (also the TLS handshake timeout).")
(defvar *user-agent*
  "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36"
  "User-Agent advertised on outgoing requests.  weft is a real rendering engine, so it
identifies as a browser — many sites serve a bot interstitial (\"enable JS and cookies\")
to any non-browser UA.")

(defvar *progress* nil
  "When bound to a function of (PHASE &optional DETAIL), the transport reports
   fetch sub-phases — :resolving :connecting :downloading :decoding :redirecting
   — so a host can surface fine-grained progress.  NIL (the default) disables it.")

(defun note-progress (phase &optional detail)
  "Call the progress hook if one is bound; never signals."
  (when *progress* (ignore-errors (funcall *progress* phase detail))))

(defun %ascii-octets (string)
  "Encode an ASCII/Latin-1 request STRING to octets."
  (map '(simple-array (unsigned-byte 8) (*)) #'char-code string))

(defun %latin1 (octets)
  "Decode header-line OCTETS to a string (bytes are Latin-1: 1:1 with codes)."
  (map 'string #'code-char octets))

(defun scheme-default-port (scheme)
  (cond ((string-equal scheme "https") 443)
        ((string-equal scheme "http") 80)
        (t (error "weft.fetch: unsupported URL scheme ~s" scheme))))

(defun open-tcp-stream (host port timeout)
  "Open a bidirectional binary stream to HOST:PORT over TCP (raw, no TLS)."
  (let* ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
         (addr (progn (note-progress :resolving host)
                      (sb-bsd-sockets:host-ent-address (sb-bsd-sockets:get-host-by-name host)))))
    (handler-case
        (progn
          (note-progress :connecting host)
          (sb-bsd-sockets:socket-connect socket addr port)
          (sb-bsd-sockets:socket-make-stream socket :element-type '(unsigned-byte 8)
                                                     :input t :output t :buffering :full
                                                     :timeout timeout))
      (error (e) (ignore-errors (sb-bsd-sockets:socket-close socket)) (error e)))))

(defun open-tls-stream (host port timeout)
  "Open a validated (:verify t) TLS 1.3 stream to HOST:PORT via seal, with SNI =
HOST and an HTTP/1.1 ALPN offer.  Signals on a handshake / certificate failure."
  (seal:make-tls-stream
   (seal:connect host port :verify t :timeout timeout :alpn '("http/1.1"))))

(defun open-stream-for (scheme host port timeout)
  (if (string-equal scheme "https")
      (open-tls-stream host port timeout)
      (open-tcp-stream host port timeout)))

;;; ---- HTTP/1.1 over an arbitrary binary stream -------------------------

(defun merge-headers (defaults extra)
  "DEFAULTS, with any EXTRA header overriding one by case-insensitive name; EXTRA
   names not present in DEFAULTS are appended (e.g. Cookie).  Lets a caller replace
   Accept for image requests, which don't want the avif/webp the HTML path advertises."
  (let ((result '()))
    (dolist (d defaults)
      (push (or (assoc (car d) extra :test #'string-equal) d) result))
    (dolist (e extra)
      (unless (assoc (car e) defaults :test #'string-equal) (push e result)))
    (nreverse result)))

(defun write-request (stream method host path req-headers)
  "Write an HTTP/1.1 request line + headers to STREAM and flush it.  Keeps the
connection alive and advertises our decoders; REQ-HEADERS override the defaults."
  (let* ((defaults `(("Host" . ,host)
                     ("User-Agent" . ,*user-agent*)
                     ;; advertise only what we can actually decode — claiming avif/webp is
                     ;; why content-negotiating servers (Next.js /_next/image, CDNs) hand us
                     ;; images we can't render.
                     ("Accept" . "text/html,application/xhtml+xml,application/xml;q=0.9,image/svg+xml,image/png,image/jpeg,image/gif,*/*;q=0.8")
                     ("Accept-Language" . "en-US,en;q=0.9")
                     ("Accept-Encoding" . "gzip, deflate, br, zstd")
                     ("Connection" . "keep-alive")))
         (out (with-output-to-string (s)
                (format s "~a ~a HTTP/1.1~c~c" method path #\Return #\Linefeed)
                (loop for (k . v) in (merge-headers defaults req-headers)
                      do (format s "~a: ~a~c~c" k v #\Return #\Linefeed))
                (format s "~c~c" #\Return #\Linefeed))))
    (write-sequence (%ascii-octets out) stream)
    (finish-output stream)))

(defun read-header-line (stream)
  "Read one CRLF-terminated line from STREAM as a string (CRLF stripped).
Returns NIL at end of stream with no bytes read."
  (let ((buf (make-array 128 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for b = (read-byte stream nil :eof)
          do (cond ((eq b :eof) (return (when (plusp (fill-pointer buf)) (%latin1 buf))))
                   ((= b 10) (when (and (plusp (fill-pointer buf))
                                        (= (aref buf (1- (fill-pointer buf))) 13))
                               (decf (fill-pointer buf)))
                    (return (%latin1 buf)))
                   (t (vector-push-extend b buf))))))

(defun parse-status-line (line)
  "Parse `HTTP/1.1 200 OK` -> integer status (0 if unparseable)."
  (or (and line
           (let ((sp (position #\Space line)))
             (when sp
               (let ((sp2 (position #\Space line :start (1+ sp))))
                 (ignore-errors (parse-integer line :start (1+ sp) :end sp2 :junk-allowed t))))))
      0))

(defun read-headers (stream)
  "Read response header lines until the blank line.  Returns an alist."
  (let ((headers '()))
    (loop for line = (read-header-line stream)
          while (and line (plusp (length line)))
          do (let ((c (position #\: line)))
               (when c (push (cons (string-trim " " (subseq line 0 c))
                                   (string-trim " " (subseq line (1+ c)))) headers))))
    (nreverse headers)))

(defun read-n-bytes (stream n)
  (let ((buf (make-array n :element-type '(unsigned-byte 8))))
    (let ((got (read-sequence buf stream)))
      (if (= got n) buf (subseq buf 0 got)))))

(defun read-n-bytes-reporting (stream n)
  "Like READ-N-BYTES, but read in 64 KB blocks and report :downloading progress
   (got/total) between them — used when a progress hook is set and the body length
   is known, so a large document shows a live byte counter."
  (let ((buf (make-array n :element-type '(unsigned-byte 8))) (pos 0)
        (total-kb (round n 1024)))
    (loop while (< pos n)
          for got = (read-sequence buf stream :start pos :end (min n (+ pos 65536)))
          do (when (= got pos) (return))       ; EOF before Content-Length
             (setf pos got)
             (note-progress :downloading (format nil "~d/~d KB" (round pos 1024) total-kb)))
    (if (= pos n) buf (subseq buf 0 pos))))

(defun read-to-eof (stream)
  (let ((out (make-array 4096 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (chunk (make-array 4096 :element-type '(unsigned-byte 8))))
    (loop for got = (read-sequence chunk stream)
          while (plusp got)
          do (loop for i from 0 below got do (vector-push-extend (aref chunk i) out))
          while (= got (length chunk)))
    (%octets out)))

(defun read-chunked-body (stream)
  "Decode a Transfer-Encoding: chunked body from STREAM into one octet vector."
  (let ((parts '()) (total 0))
    (loop
      (let* ((line (read-header-line stream))
             (semi (and line (position #\; line)))
             (size (and line (ignore-errors
                              (parse-integer line :end semi :radix 16 :junk-allowed t)))))
        (when (or (null size) (not (integerp size)) (zerop size))
          (return))
        (push (read-n-bytes stream size) parts)
        (incf total size)
        (when *progress* (note-progress :downloading (format nil "~d KB" (round total 1024))))
        (read-header-line stream)))            ; trailing CRLF after each chunk
    ;; consume any trailer headers up to the final blank line
    (loop for line = (read-header-line stream)
          while (and line (plusp (length line))))
    (apply #'concatenate '(simple-array (unsigned-byte 8) (*)) (nreverse parts))))

(defun read-body (stream headers method status)
  "Read the response body per RFC 7230 framing (chunked / Content-Length / EOF)."
  (cond
    ((or (string-equal method "HEAD") (= status 204) (= status 304)
         (<= 100 status 199))
     (%octets #()))
    ((let ((te (get-header headers "transfer-encoding")))
       (and te (search "chunked" te :test #'char-equal)))
     (note-progress :downloading)
     (read-chunked-body stream))
    ((get-header headers "content-length")
     (let ((n (ignore-errors (parse-integer (get-header headers "content-length")
                                            :junk-allowed t))))
       (cond ((not (and n (plusp n))) (%octets #()))
             (*progress* (read-n-bytes-reporting stream n))
             (t (read-n-bytes stream n)))))
    (t (note-progress :downloading) (read-to-eof stream))))

;;; ---- cookie jar -------------------------------------------------------
;;; A process-wide session jar: Set-Cookie is parsed and stored; matching cookies
;;; are replayed as a Cookie header.  Enough for logins and the "enable cookies"
;;; bot interstitials — domain/path/secure matching, Max-Age=0 deletion; no
;;; persistence to disk (the session lasts as long as the process).

(defun split-on (string char)
  "Split STRING on CHAR into a list of substrings (CHAR removed)."
  (loop with start = 0
        for pos = (position char string :start start)
        collect (subseq string start pos)
        while pos do (setf start (1+ pos))))

(defstruct cookie name value domain path secure)
(defstruct (cookie-jar (:constructor make-cookie-jar))
  "One browsing context's cookies.  Distinct contexts hold distinct jars — a fresh
   URL open gets a fresh jar, so a site can't be re-identified across visits — while
   a tab that follows links (or a popup sharing its opener) keeps using the same one.
   Each jar carries its own lock, so isolated contexts never contend."
  (cookies '())
  (lock (sb-thread:make-mutex)))
(defvar *cookie-jar* (make-cookie-jar)
  "The cookie jar of the browsing context this thread is fetching for.  Rebind
   per context (LET) to isolate identity; the default jar serves standalone use.")
(defvar *enable-cookies* t)

(defun %domain-match (host domain)
  "HOST is DOMAIN or a subdomain of it (RFC 6265 domain-match)."
  (let ((h (string-downcase host)) (d (string-downcase domain)))
    (or (string= h d)
        (and (> (length h) (length d))
             (string= d (subseq h (- (length h) (length d))))
             (char= (char h (- (length h) (length d) 1)) #\.)))))

(defun store-cookies (headers request-host)
  "Parse every Set-Cookie in HEADERS into the jar (or delete on Max-Age<=0)."
  (when *enable-cookies*
    (dolist (h headers)
      (when (string-equal (car h) "set-cookie")
        (let* ((parts (mapcar (lambda (s) (string-trim " " s)) (split-on (cdr h) #\;)))
               (nv (first parts))
               (eq (position #\= nv)))
          (when (and eq (plusp eq))
            (let ((name (string-trim " " (subseq nv 0 eq)))
                  (val (string-trim " " (subseq nv (1+ eq))))
                  (domain request-host) (path "/") (secure nil) (delete nil))
              (dolist (attr (rest parts))
                (let* ((ae (position #\= attr))
                       (k (string-downcase (if ae (subseq attr 0 ae) attr)))
                       (v (and ae (subseq attr (1+ ae)))))
                  (cond ((and (string= k "domain") v) (setf domain (string-left-trim "." v)))
                        ((and (string= k "path") v (plusp (length v))) (setf path v))
                        ((string= k "secure") (setf secure t))
                        ((and (string= k "max-age") v (<= (or (ignore-errors (parse-integer v)) 1) 0))
                         (setf delete t)))))
              (sb-thread:with-mutex ((cookie-jar-lock *cookie-jar*))
                (setf (cookie-jar-cookies *cookie-jar*)
                      (remove-if (lambda (c) (and (string= (cookie-name c) name)
                                                  (string-equal (cookie-domain c) domain)
                                                  (string= (cookie-path c) path)))
                                 (cookie-jar-cookies *cookie-jar*)))
                (unless delete
                  (push (make-cookie :name name :value val :domain domain :path path :secure secure)
                        (cookie-jar-cookies *cookie-jar*)))))))))))

(defun cookie-header (host path https)
  "The Cookie header value for cookies matching HOST/PATH/scheme, or NIL."
  (when *enable-cookies*
    (let ((matches (sb-thread:with-mutex ((cookie-jar-lock *cookie-jar*))
                     (remove-if-not (lambda (c)
                                      (and (%domain-match host (cookie-domain c))
                                           (let ((cp (cookie-path c)))
                                             (or (string= cp "/")
                                                 (and (>= (length path) (length cp))
                                                      (string= cp (subseq path 0 (length cp))))))
                                           (or (not (cookie-secure c)) https)))
                                    (cookie-jar-cookies *cookie-jar*)))))
      (when matches
        (format nil "~{~a~^; ~}"
                (mapcar (lambda (c) (format nil "~a=~a" (cookie-name c) (cookie-value c))) matches))))))

;;; ---- connection pool (HTTP/1.1 keep-alive) ----------------------------
(defvar *conn-pool* (make-hash-table :test 'equal) "pool-key -> list of idle streams.")
(defvar *pool-lock* (sb-thread:make-mutex))
(defvar *reuse-connections* t)
(defparameter *pool-max-idle* 4 "Idle connections kept per host.")

(defun pool-key (scheme host port) (format nil "~(~a~)://~a:~a" scheme host port))

(defun acquire-stream (scheme host port timeout)
  "An idle pooled stream for the host if any (reused), else a fresh one.  Returns
   (values stream pooled-p)."
  (or (and *reuse-connections*
           (sb-thread:with-mutex (*pool-lock*)
             (let ((idle (gethash (pool-key scheme host port) *conn-pool*)))
               (when idle
                 (setf (gethash (pool-key scheme host port) *conn-pool*) (rest idle))
                 (values (first idle) t)))))
      (values (open-stream-for scheme host port timeout) nil)))

(defun release-stream (scheme host port stream keep)
  "Return STREAM to the pool if KEEP and there's room, else close it."
  (if (and keep *reuse-connections*)
      (let ((closed nil))
        (sb-thread:with-mutex (*pool-lock*)
          (let* ((k (pool-key scheme host port)) (idle (gethash k *conn-pool*)))
            (if (< (length idle) *pool-max-idle*)
                (setf (gethash k *conn-pool*) (cons stream idle))
                (setf closed t))))
        (when closed (ignore-errors (close stream))))
      (ignore-errors (close stream))))

(defun keep-alive-p (headers)
  "Whether the connection may be reused: the body was self-delimiting (Content-Length
   or chunked) and the server didn't ask to close OR switch protocols."
  (let ((conn (get-header headers "connection")))
    (and (not (and conn (or (search "close" conn :test #'char-equal)
                            ;; `Connection: Upgrade` leaves the socket in a protocol-
                            ;; switch limbo (some Apache configs send it on plain 200s);
                            ;; pooling it makes the *next* request read garbage/EOF.
                            (search "upgrade" conn :test #'char-equal))))
         (or (get-header headers "content-length")
             (let ((te (get-header headers "transfer-encoding")))
               (and te (search "chunked" te :test #'char-equal)))))))

(defun socket-transport (method url req-headers)
  "One HTTP/1.1 exchange to URL over a pooled raw socket (http) or seal TLS stream
(https).  Connections are kept alive and reused per host; a connection-level failure
(a pooled socket the server has since closed, or a transient reset) is retried on a
fresh connection, up to *TRANSPORT-TRIES* attempts.  Cookies are attached and
Set-Cookie stored."
  (let* ((u (url:parse url))
         (scheme (url:url-scheme u))
         (host (url:hostname u))
         (https (string-equal scheme "https"))
         (port (or (url:url-port u) (scheme-default-port scheme)))
         (path (let ((p (url:pathname-str u)) (q (url:search-str u)))
                 (concatenate 'string (if (plusp (length p)) p "/") q)))
         (ck (cookie-header host path https))
         (all-headers (if ck (cons (cons "Cookie" ck) req-headers) req-headers)))
    (flet ((exchange (stream)
             (write-request stream method host path all-headers)
             (let ((line (read-header-line stream)))
               ;; No status line means the peer closed the connection before
               ;; answering — a keep-alive socket the server timed out since we
               ;; pooled it.  Signal so the loop retries fresh instead of returning
               ;; an empty 0-status response.
               (unless (and line (position #\Space line))
                 (error "weft.fetch: no HTTP status line (stale/closed connection)"))
               (let* ((status (parse-status-line line))
                      (headers (read-headers stream))
                      (body (read-body stream headers method status)))
                 (store-cookies headers host)
                 (values (make-response :status status :headers headers :body body :url url)
                         (keep-alive-p headers))))))
      (labels ((attempt (stream pooled)
                 (handler-case
                     (multiple-value-bind (resp keep) (exchange stream)
                       (release-stream scheme host port stream keep)
                       resp)
                   (error (e)
                     (ignore-errors (close stream))
                     ;; only a *reused* socket is expected to have gone stale — retry it
                     ;; once on a fresh connection; a fresh failure is a real error.
                     (if pooled
                         (attempt (open-stream-for scheme host port *read-timeout*) nil)
                         (error e))))))
        (multiple-value-bind (stream pooled) (acquire-stream scheme host port *read-timeout*)
          (attempt stream pooled))))))

(defvar *http-transport* #'socket-transport
  "Function (method url req-headers) -> RESPONSE.  The default is a pure-CL
socket/TLS transport (seal + sb-bsd-sockets); rebind to plug in another.")

;;; ---- redirect following + the public API ------------------------------

(defvar *max-redirects* 5 "Maximum number of 3xx hops FETCH will follow.")

(defun redirect-status-p (status) (member status '(301 302 303 307 308)))

(defun fetch (url-string &key (method "GET") headers (max-redirects *max-redirects*))
  "Fetch URL-STRING, following up to MAX-REDIRECTS 3xx redirects (absolute or
relative, http<->https), with loop protection.  Returns a RESPONSE whose
RESPONSE-URL is the FINAL URL (the document base).  Body is the raw, still-
encoded bytes.  A network/TLS/cert failure signals a clean CL condition."
  (let ((u (url:parse url-string)))
    (unless u (error "weft.fetch: invalid URL ~s" url-string))
    (let ((current (url:href u)) (seen '()) (m method))
      (loop for hop from 0 to max-redirects do
        (when (member current seen :test #'string=)
          (error "weft.fetch: redirect loop at ~s" current))
        (push current seen)
        (let* ((r (funcall *http-transport* m current headers))
               (loc (get-header (response-headers r) "location")))
          (if (and (redirect-status-p (response-status r)) loc (< hop max-redirects))
              (let ((next (url:parse loc current)))
                (unless next (return r))
                ;; 303 (and, conventionally, 301/302) become GET; 307/308 preserve.
                (when (member (response-status r) '(301 302 303))
                  (setf m "GET"))
                (note-progress :redirecting (url:hostname next))
                (setf current (url:href next)))
              (return r)))))))

(defun fetch-text (url-string &rest args)
  "Fetch and fully decode to text.  Returns (values string charset response)."
  (let ((r (apply #'fetch url-string args)))
    (note-progress :decoding)
    (multiple-value-bind (text cs) (body-text (response-headers r) (response-body r))
      (values text cs r))))
