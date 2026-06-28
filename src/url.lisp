;;;; src/url.lisp — WHATWG URL Standard parser (https://url.spec.whatwg.org/).
;;;;
;;;; A faithful implementation of the "basic URL parser" state machine plus host
;;;; parsing (domain / IPv4 / IPv6 / opaque), IDNA ToASCII (punycode), percent
;;;; encoding, and URL serialization.  Differential-tested against the Web
;;;; Platform Tests url corpus (inspect/vectors/urltestdata.json).
(in-package #:weft.url)

;;; ---- code-point helpers ----------------------------------------------

(declaim (inline alpha-p digitp alnum-p hexp))
(defun alpha-p (c) (and c (or (char<= #\a c #\z) (char<= #\A c #\Z))))
(defun digitp (c) (and c (char<= #\0 c #\9)))
(defun alnum-p (c) (or (alpha-p c) (digitp c)))
(defun hexp (c) (and c (digit-char-p c 16)))
(defun c0-or-space-p (cp) (<= cp #x20))
(defun tab-nl-p (cp) (or (= cp #x09) (= cp #x0a) (= cp #x0d)))

(defparameter *special-ports*
  '(("ftp" . 21) ("file" . nil) ("http" . 80) ("https" . 443) ("ws" . 80) ("wss" . 443)))
(defun special-scheme-p (s) (and (assoc s *special-ports* :test #'string=) t))
(defun default-port (s) (cdr (assoc s *special-ports* :test #'string=)))

;;; ---- percent encoding -------------------------------------------------

(defun cp->utf8 (cp)
  (cond ((< cp #x80) (list cp))
        ((< cp #x800) (list (logior #xc0 (ash cp -6)) (logior #x80 (logand cp #x3f))))
        ((< cp #x10000) (list (logior #xe0 (ash cp -12))
                              (logior #x80 (logand (ash cp -6) #x3f)) (logior #x80 (logand cp #x3f))))
        (t (list (logior #xf0 (ash cp -18)) (logior #x80 (logand (ash cp -12) #x3f))
                 (logior #x80 (logand (ash cp -6) #x3f)) (logior #x80 (logand cp #x3f))))))

(defun pct-cp (cp)
  (with-output-to-string (s) (dolist (b (cp->utf8 cp)) (format s "%~2,'0X" b))))

;; percent-encode sets (each as a predicate on a code point)
(defun s-c0 (cp) (or (<= cp #x1f) (> cp #x7e)))
(defun s-frag (cp) (or (s-c0 cp) (member cp '(#x20 #x22 #x3c #x3e #x60))))
(defun s-query (cp) (or (s-c0 cp) (member cp '(#x20 #x22 #x23 #x3c #x3e))))
(defun s-special-query (cp) (or (s-query cp) (= cp #x27)))
(defun s-path (cp) (or (s-query cp) (member cp '(#x3f #x5e #x60 #x7b #x7d))))
(defun s-userinfo (cp) (or (s-path cp) (member cp '(#x2f #x3a #x3b #x3d #x40 #x5b #x5c #x5d #x5e #x7c))))

(defun enc (cp set) (if (funcall set cp) (pct-cp cp) (string (code-char cp))))

(defun percent-decode (string)
  "Percent-decode STRING to a byte list."
  (let ((bytes '()) (i 0) (n (length string)))
    (loop while (< i n) do
      (let ((c (char string i)))
        (if (and (char= c #\%) (< (+ i 2) n) (hexp (char string (1+ i))) (hexp (char string (+ i 2))))
            (progn (push (+ (* 16 (digit-char-p (char string (1+ i)) 16))
                            (digit-char-p (char string (+ i 2)) 16)) bytes)
                   (incf i 3))
            (progn (dolist (b (cp->utf8 (char-code c))) (push b bytes)) (incf i)))))
    (nreverse bytes)))

(defun utf8->string (bytes)
  "Decode a UTF-8 BYTE list to a string (lossy: malformed -> U+FFFD)."
  (let ((out (make-string-output-stream)) (v (coerce bytes 'vector)) (i 0) (n (length bytes)))
    (flet ((cont (k) (and (< k n) (= (logand (aref v k) #xc0) #x80))))
      (loop while (< i n) do
        (let ((b (aref v i)) cp len)
          (cond ((< b #x80) (setf cp b len 1))
                ((= (logand b #xe0) #xc0) (setf cp (logand b #x1f) len 2))
                ((= (logand b #xf0) #xe0) (setf cp (logand b #x0f) len 3))
                ((= (logand b #xf8) #xf0) (setf cp (logand b #x07) len 4))
                (t (setf cp #xfffd len 1)))
          (if (and (> len 1) (loop for k from 1 below len always (cont (+ i k))))
              (progn (loop for k from 1 below len do (setf cp (logior (ash cp 6) (logand (aref v (+ i k)) #x3f))))
                     (incf i len))
              (progn (when (> len 1) (setf cp #xfffd)) (incf i)))
          (write-char (code-char cp) out))))
    (get-output-stream-string out)))

;;; ---- punycode (RFC 3492) for IDNA ToASCII -----------------------------

(defun punycode-encode (cps)
  "Encode a list of Unicode code points (one label) to a punycode ASCII string."
  (let* ((base 36) (tmin 1) (tmax 26) (skew 38) (damp 700) (ibias 72)
         (n #x80) (delta 0) (bias ibias) (out (make-string-output-stream))
         (input (coerce cps 'vector)) (total (length input)) (h 0) (b 0))
    (flet ((dchar (d) (code-char (if (< d 26) (+ d (char-code #\a)) (+ (- d 26) (char-code #\0)))))
           (adapt (d np first)
             (setf d (if first (floor d damp) (floor d 2)))
             (incf d (floor d np))
             (let ((k 0))
               (loop while (> d (floor (* (- base tmin) tmax) 2)) do
                 (setf d (floor d (- base tmin))) (incf k base))
               (+ k (floor (* (1+ (- base tmin)) d) (+ d skew))))))
      (loop for cp across input when (< cp #x80) do (write-char (code-char cp) out) (incf b))
      (setf h b)
      (when (> b 0) (write-char #\- out))
      (loop while (< h total) do
        (let ((m most-positive-fixnum))
          (loop for cp across input when (and (>= cp n) (< cp m)) do (setf m cp))
          (incf delta (* (- m n) (1+ h)))
          (setf n m)
          (loop for cp across input do
            (when (< cp n) (incf delta))
            (when (= cp n)
              (let ((q delta))
                (loop for k from base by base do
                  (let ((tt (cond ((<= k bias) tmin) ((>= k (+ bias tmax)) tmax) (t (- k bias)))))
                    (when (< q tt) (return))
                    (write-char (dchar (+ tt (mod (- q tt) (- base tt)))) out)
                    (setf q (floor (- q tt) (- base tt)))))
                (write-char (dchar q) out)
                (setf bias (adapt delta (1+ h) (= h b)))
                (setf delta 0) (incf h))))
          (incf delta) (incf n))))
    (get-output-stream-string out)))

(defun domain-to-ascii (domain)
  "IDNA ToASCII: lowercase ASCII labels, punycode non-ASCII labels.  Returns the
ASCII domain string, or :failure."
  (when (string= domain "") (return-from domain-to-ascii :failure))
  (let ((labels (split domain #\.)))
    (handler-case
        (let ((out (mapcar
                    (lambda (lbl)
                      (if (every (lambda (c) (< (char-code c) #x80)) lbl)
                          (string-downcase lbl)
                          (concatenate 'string "xn--"
                                       (punycode-encode (map 'list (lambda (c)
                                                                     (let ((cc (char-code c)))
                                                                       (if (char<= #\A c #\Z) (+ cc 32) cc)))
                                                             lbl)))))
                    labels)))
          (format nil "~{~a~^.~}" out))
      (error () :failure))))

;;; ---- misc string helpers ---------------------------------------------

(defun split (string ch)
  (let ((parts '()) (start 0))
    (dotimes (i (length string))
      (when (char= (char string i) ch) (push (subseq string start i) parts) (setf start (1+ i))))
    (push (subseq string start) parts)
    (nreverse parts)))

;;; ---- the URL record ---------------------------------------------------

(defstruct (url (:constructor %make-url))
  (scheme "") (username "") (password "") host port
  (path (make-array 0 :adjustable t :fill-pointer 0)) query fragment
  (opaque-path-p nil))

(defun special-p (u) (special-scheme-p (url-scheme u)))

(defun copy-path (p)
  (if (stringp p) p
      (let ((v (make-array (length p) :adjustable t :fill-pointer (length p))))
        (replace v p) v)))

;;; ---- host parsing -----------------------------------------------------

(defun forbidden-host-p (cp)
  (or (= cp 0) (member cp '(#x09 #x0a #x0d #x20 #x23 #x2f #x3a #x3c #x3e #x3f #x40 #x5b #x5c #x5d #x5e #x7c))))
(defun forbidden-domain-p (cp)
  (or (forbidden-host-p cp) (<= cp #x1f) (= cp #x25) (= cp #x7f)))

(defun parse-ipv4-number (s)
  (when (string= s "") (return-from parse-ipv4-number :failure))
  (let ((r 10))
    (cond ((and (>= (length s) 2) (char= (char s 0) #\0) (member (char s 1) '(#\x #\X)))
           (setf r 16 s (subseq s 2)))
          ((and (>= (length s) 1) (char= (char s 0) #\0))
           (setf r 8 s (subseq s 1))))
    (when (string= s "") (return-from parse-ipv4-number 0))
    (unless (every (lambda (c) (digit-char-p c r)) s) (return-from parse-ipv4-number :failure))
    (parse-integer s :radix r)))

(defun ends-in-number-p (host)
  (let ((parts (split host #\.)))
    (when (string= (car (last parts)) "")
      (when (= (length parts) 1) (return-from ends-in-number-p nil))
      (setf parts (butlast parts)))
    (let ((last (car (last parts))))
      (cond ((string= last "") nil)
            ((every #'digitp last) t)
            (t (not (eq (parse-ipv4-number last) :failure)))))))

(defun parse-ipv4 (input)
  (let ((parts (split input #\.)))
    (when (string= (car (last parts)) "")
      (when (> (length parts) 1) (setf parts (butlast parts))))
    (when (> (length parts) 4) (return-from parse-ipv4 :failure))
    (let ((numbers '()))
      (dolist (p parts)
        (let ((r (parse-ipv4-number p)))
          (when (eq r :failure) (return-from parse-ipv4 :failure))
          (push r numbers)))
      (setf numbers (nreverse numbers))
      (loop for n in (butlast numbers) when (> n 255) do (return-from parse-ipv4 :failure))
      (let ((last (car (last numbers))))
        (when (>= last (expt 256 (- 5 (length numbers)))) (return-from parse-ipv4 :failure))
        (let ((ipv4 last) (counter 0))
          (dolist (n (butlast numbers)) (incf ipv4 (* n (expt 256 (- 3 counter)))) (incf counter))
          ipv4)))))

(defun parse-ipv6 (input)
  (let ((address (make-array 8 :initial-element 0)) (piece-index 0) (compress nil)
        (cps (coerce input 'vector)) (ptr 0))
    (let ((len (length cps)))
      (flet ((c () (when (< ptr len) (aref cps ptr))))
        (when (and (c) (char= (c) #\:))
          (unless (and (< (1+ ptr) len) (char= (aref cps (1+ ptr)) #\:)) (return-from parse-ipv6 :failure))
          (incf ptr 2) (incf piece-index) (setf compress piece-index))
        (loop while (c) do
          (block iter
            (when (= piece-index 8) (return-from parse-ipv6 :failure))
            (when (char= (c) #\:)
              (when compress (return-from parse-ipv6 :failure))
              (incf ptr) (incf piece-index) (setf compress piece-index) (return-from iter))
            (let ((value 0) (vlen 0))
              (loop while (and (c) (hexp (c)) (< vlen 4)) do
                (setf value (+ (* value 16) (digit-char-p (c) 16))) (incf ptr) (incf vlen))
              (cond
                ((and (c) (char= (c) #\.))
                 (when (zerop vlen) (return-from parse-ipv6 :failure))
                 (decf ptr vlen)
                 (when (> piece-index 6) (return-from parse-ipv6 :failure))
                 (let ((numbers-seen 0))
                   (loop while (c) do
                     (let ((ipv4piece nil))
                       (when (> numbers-seen 0)
                         (if (and (char= (c) #\.) (< numbers-seen 4)) (incf ptr)
                             (return-from parse-ipv6 :failure)))
                       (unless (and (c) (digitp (c))) (return-from parse-ipv6 :failure))
                       (loop while (and (c) (digitp (c))) do
                         (let ((num (digit-char-p (c) 10)))
                           (cond ((null ipv4piece) (setf ipv4piece num))
                                 ((zerop ipv4piece) (return-from parse-ipv6 :failure))
                                 (t (setf ipv4piece (+ (* ipv4piece 10) num))))
                           (when (> ipv4piece 255) (return-from parse-ipv6 :failure))
                           (incf ptr)))
                       (setf (aref address piece-index) (+ (* (aref address piece-index) 256) ipv4piece))
                       (incf numbers-seen)
                       (when (or (= numbers-seen 2) (= numbers-seen 4)) (incf piece-index))))
                   (unless (= numbers-seen 4) (return-from parse-ipv6 :failure)))
                 (return-from iter))
                ((and (c) (char= (c) #\:))
                 (incf ptr) (unless (c) (return-from parse-ipv6 :failure)))
                ((c) (return-from parse-ipv6 :failure)))
              (setf (aref address piece-index) value) (incf piece-index))))
        (cond
          (compress
           (let ((swaps (- piece-index compress)) (idx 7))
             (loop while (and (/= idx 0) (> swaps 0)) do
               (rotatef (aref address idx) (aref address (+ compress swaps -1)))
               (decf idx) (decf swaps))))
          ((/= piece-index 8) (return-from parse-ipv6 :failure)))
        address))))

(defun parse-opaque-host (input)
  (loop for c across input do
    (when (and (forbidden-host-p (char-code c)) (/= (char-code c) #x25))
      (return-from parse-opaque-host :failure)))
  (with-output-to-string (s) (loop for c across input do (write-string (enc (char-code c) #'s-c0) s))))

(defun parse-host (input special)
  (cond
    ((and (> (length input) 0) (char= (char input 0) #\[))
     (if (char= (char input (1- (length input))) #\])
         (parse-ipv6 (subseq input 1 (1- (length input))))
         :failure))
    ((not special) (parse-opaque-host input))
    (t (let* ((decoded (utf8->string (percent-decode input)))
              (ascii (domain-to-ascii decoded)))
         ;; U+FFFD means invalid UTF-8 / a disallowed code point -> domain failure
         (when (find (code-char #xfffd) decoded) (return-from parse-host :failure))
         (when (eq ascii :failure) (return-from parse-host :failure))
         (when (some (lambda (c) (forbidden-domain-p (char-code c))) ascii)
           (return-from parse-host :failure))
         (if (ends-in-number-p ascii) (parse-ipv4 ascii) ascii)))))

;;; ---- the basic URL parser state machine -------------------------------

(defun windows-drive-p (s &optional (i 0))
  (and (>= (- (length s) i) 2) (alpha-p (char s i)) (member (char s (1+ i)) '(#\: #\|))))
(defun drive-letter-p (s)                               ; "is" a drive letter: exactly 2 chars
  (and (= (length s) 2) (alpha-p (char s 0)) (member (char s 1) '(#\: #\|))))
(defun starts-windows-drive-p (s)
  (and (windows-drive-p s)
       (or (= (length s) 2) (member (char s 2) '(#\/ #\\ #\? #\#)))))
(defun normalized-windows-drive-p (s)
  (and (= (length s) 2) (alpha-p (char s 0)) (char= (char s 1) #\:)))

(defun path-shorten (u)
  (let ((p (url-path u)))
    (unless (and (string= (url-scheme u) "file") (= (length p) 1)
                 (normalized-windows-drive-p (aref p 0)))
      (when (> (fill-pointer p) 0) (decf (fill-pointer p))))))

(defun basic-parse (input &optional base)
  "Run the basic URL parser over INPUT (a string) with optional BASE url.
Returns a URL record or :failure."
  ;; preprocess: trim leading/trailing C0-or-space, remove all tab/newline
  (let ((s (string-trim '(#\Nul) input)))
    (declare (ignore s))
    (let* ((start 0) (end (length input)))
      (loop while (and (< start end) (c0-or-space-p (char-code (char input start)))) do (incf start))
      (loop while (and (> end start) (c0-or-space-p (char-code (char input (1- end))))) do (decf end))
      (let ((clean (with-output-to-string (o)
                     (loop for i from start below end
                           for ch = (char input i)
                           unless (tab-nl-p (char-code ch)) do (write-char ch o)))))
        (let* ((cps (coerce clean 'vector)) (len (length cps))
               (u (%make-url)) (state :scheme-start)
               (buf (make-array 16 :element-type 'character :adjustable t :fill-pointer 0))
               (ptr 0) (at-sign nil) (brackets nil) (pw-seen nil) (guard 0))
          (labels ((c () (when (< ptr len) (aref cps ptr)))
                   (cc () (let ((x (c))) (and x (char-code x))))
                   (rest-str () (if (< ptr len) (subseq clean ptr) ""))
                   (bstr () (subseq buf 0))
                   (bclear () (setf (fill-pointer buf) 0))
                   (bpush (ch) (vector-push-extend ch buf))
                   (bappend (str) (loop for ch across str do (vector-push-extend ch buf)))
                   (ppush (seg) (vector-push-extend seg (url-path u)))
                   (special () (special-p u)))
            (loop
              (when (> (incf guard) 1000000) (return-from basic-parse :failure))
              (let ((ch (c)))
                (ecase state
                  (:scheme-start
                   (cond ((alpha-p ch) (bpush (char-downcase ch)) (setf state :scheme))
                         (t (setf state :no-scheme) (decf ptr))))
                  (:scheme
                   (cond
                     ((or (alnum-p ch) (member ch '(#\+ #\- #\.))) (bpush (char-downcase ch)))
                     ((eql ch #\:)
                      (setf (url-scheme u) (bstr)) (bclear)
                      (cond
                        ((string= (url-scheme u) "file") (setf state :file))
                        ((and (special) base (string= (url-scheme base) (url-scheme u)))
                         (setf state :special-relative-or-authority))
                        ((special) (setf state :special-authority-slashes))
                        ((and (< (1+ ptr) (1+ len)) (eql (when (< (1+ ptr) len) (aref cps (1+ ptr))) #\/))
                         (setf state :path-or-authority) (incf ptr))
                        (t (setf (url-opaque-path-p u) t (url-path u) "") (setf state :opaque-path))))
                     (t (bclear) (setf state :no-scheme ptr -1))))
                  (:no-scheme
                   (cond
                     ((or (null base) (and (url-opaque-path-p base) (not (eql ch #\#))))
                      (return-from basic-parse :failure))
                     ((and (url-opaque-path-p base) (eql ch #\#))
                      (setf (url-scheme u) (url-scheme base) (url-path u) (url-path base)
                            (url-opaque-path-p u) t (url-query u) (url-query base) (url-fragment u) "")
                      (setf state :fragment))
                     ((not (string= (url-scheme base) "file")) (setf state :relative) (decf ptr))
                     (t (setf state :file) (decf ptr))))
                  (:special-relative-or-authority
                   (cond ((and (eql ch #\/) (eql (when (< (1+ ptr) len) (aref cps (1+ ptr))) #\/))
                          (setf state :special-authority-ignore-slashes) (incf ptr))
                         (t (setf state :relative) (decf ptr))))
                  (:path-or-authority
                   (cond ((eql ch #\/) (setf state :authority))
                         (t (setf state :path) (decf ptr))))
                  (:relative
                   (setf (url-scheme u) (url-scheme base))
                   (cond
                     ((eql ch #\/) (setf state :relative-slash))
                     ((and (special) (eql ch #\\)) (setf state :relative-slash))
                     (t (setf (url-username u) (url-username base) (url-password u) (url-password base)
                              (url-host u) (url-host base) (url-port u) (url-port base)
                              (url-path u) (copy-path (url-path base)) (url-query u) (url-query base))
                        (cond ((eql ch #\?) (setf (url-query u) "" state :query))
                              ((eql ch #\#) (setf (url-fragment u) "" state :fragment))
                              ((c) (setf (url-query u) nil) (path-shorten u)
                               (setf state :path) (decf ptr))))))
                  (:relative-slash
                   (cond
                     ((and (special) (member ch '(#\/ #\\))) (setf state :special-authority-ignore-slashes))
                     ((eql ch #\/) (setf state :authority))
                     (t (setf (url-username u) (url-username base) (url-password u) (url-password base)
                              (url-host u) (url-host base) (url-port u) (url-port base))
                        (setf state :path) (decf ptr))))
                  (:special-authority-slashes
                   (cond ((and (eql ch #\/) (eql (when (< (1+ ptr) len) (aref cps (1+ ptr))) #\/))
                          (setf state :special-authority-ignore-slashes) (incf ptr))
                         (t (setf state :special-authority-ignore-slashes) (decf ptr))))
                  (:special-authority-ignore-slashes
                   (cond ((not (member ch '(#\/ #\\))) (setf state :authority) (decf ptr))))
                  (:authority
                   (cond
                     ((eql ch #\@)
                      (when at-sign (setf buf (let ((nb (make-array (+ 3 (length buf)) :element-type 'character
                                                                    :adjustable t :fill-pointer 0)))
                                                (loop for x across "%40" do (vector-push-extend x nb))
                                                (loop for x across buf do (vector-push-extend x nb)) nb)))
                      (setf at-sign t)
                      (loop for x across (bstr) do
                        (if (and (char= x #\:) (not pw-seen)) (setf pw-seen t)
                            (let ((e (enc (char-code x) #'s-userinfo)))
                              (if pw-seen (setf (url-password u) (concatenate 'string (url-password u) e))
                                  (setf (url-username u) (concatenate 'string (url-username u) e))))))
                      (bclear))
                     ((or (null ch) (member ch '(#\/ #\? #\#)) (and (special) (eql ch #\\)))
                      (when (and at-sign (= (fill-pointer buf) 0)) (return-from basic-parse :failure))
                      (decf ptr (1+ (fill-pointer buf))) (bclear) (setf state :host))
                     (t (bpush ch))))
                  ((:host :hostname)
                   (cond
                     ((and (eql ch #\:) (not brackets))
                      (when (= (fill-pointer buf) 0) (return-from basic-parse :failure))
                      (let ((h (parse-host (bstr) (special))))
                        (when (eq h :failure) (return-from basic-parse :failure))
                        (setf (url-host u) h) (bclear) (setf state :port)))
                     ((or (null ch) (member ch '(#\/ #\? #\#)) (and (special) (eql ch #\\)))
                      (decf ptr)
                      (when (and (special) (= (fill-pointer buf) 0)) (return-from basic-parse :failure))
                      (let ((h (parse-host (bstr) (special))))
                        (when (eq h :failure) (return-from basic-parse :failure))
                        (setf (url-host u) h) (bclear) (setf state :path-start)))
                     (t (when (eql ch #\[) (setf brackets t))
                        (when (eql ch #\]) (setf brackets nil))
                        (bpush ch))))
                  (:port
                   (cond
                     ((digitp ch) (bpush ch))
                     ((or (null ch) (member ch '(#\/ #\? #\#)) (and (special) (eql ch #\\)))
                      (when (> (fill-pointer buf) 0)
                        (let ((p (parse-integer (bstr))))
                          (when (> p 65535) (return-from basic-parse :failure))
                          (setf (url-port u) (if (eql p (default-port (url-scheme u))) nil p))
                          (bclear)))
                      (setf state :path-start) (decf ptr))
                     (t (return-from basic-parse :failure))))
                  (:file
                   (setf (url-scheme u) "file" (url-host u) "")
                   (cond
                     ((member ch '(#\/ #\\)) (setf state :file-slash))
                     ((and base (string= (url-scheme base) "file"))
                      (setf (url-host u) (url-host base) (url-path u) (copy-path (url-path base))
                            (url-query u) (url-query base))
                      (cond ((eql ch #\?) (setf (url-query u) "" state :query))
                            ((eql ch #\#) (setf (url-fragment u) "" state :fragment))
                            ((c) (setf (url-query u) nil)
                             (if (starts-windows-drive-p (rest-str))
                                 (setf (url-path u) (make-array 0 :adjustable t :fill-pointer 0))
                                 (path-shorten u))
                             (setf state :path) (decf ptr))))
                     (t (setf state :path) (decf ptr))))
                  (:file-slash
                   (cond
                     ((member ch '(#\/ #\\)) (setf state :file-host))
                     (t (when (and base (string= (url-scheme base) "file"))
                          (setf (url-host u) (url-host base))
                          (when (and (not (starts-windows-drive-p (rest-str)))
                                     (stringp (url-path base)) nil))   ; (path is vector)
                          (when (and (not (starts-windows-drive-p (rest-str)))
                                     (vectorp (url-path base)) (> (length (url-path base)) 0)
                                     (normalized-windows-drive-p (aref (url-path base) 0)))
                            (ppush (aref (url-path base) 0))))
                        (setf state :path) (decf ptr))))
                  (:file-host
                   (cond
                     ((or (null ch) (member ch '(#\/ #\\ #\? #\#)))
                      (decf ptr)
                      (cond
                        ((drive-letter-p (bstr)) (setf state :path))
                        ((= (fill-pointer buf) 0) (setf (url-host u) "") (setf state :path-start))
                        (t (let ((h (parse-host (bstr) t)))
                             (when (eq h :failure) (return-from basic-parse :failure))
                             (setf (url-host u) (if (equal h "localhost") "" h)) (bclear)
                             (setf state :path-start)))))
                     (t (bpush ch))))
                  (:path-start
                   (cond
                     ((special) (setf state :path) (unless (member ch '(#\/ #\\)) (decf ptr)))
                     ((eql ch #\?) (setf (url-query u) "" state :query))
                     ((eql ch #\#) (setf (url-fragment u) "" state :fragment))
                     ((c) (setf state :path) (unless (eql ch #\/) (decf ptr)))))
                  (:path
                   (cond
                     ((or (null ch) (eql ch #\/) (and (special) (eql ch #\\))
                          (eql ch #\?) (eql ch #\#))
                      (let ((seg (bstr)))
                        (cond
                          ((dbl-dot-p seg)
                           (path-shorten u)
                           (unless (or (eql ch #\/) (and (special) (eql ch #\\))) (ppush "")))
                          ((sgl-dot-p seg)
                           (unless (or (eql ch #\/) (and (special) (eql ch #\\))) (ppush "")))
                          (t (when (and (string= (url-scheme u) "file") (= (fill-pointer (url-path u)) 0)
                                        (drive-letter-p seg))
                               (setf seg (format nil "~c:" (char seg 0))))
                             (ppush seg))))
                      (bclear)
                      (cond ((eql ch #\?) (setf (url-query u) "" state :query))
                            ((eql ch #\#) (setf (url-fragment u) "" state :fragment))))
                     (t (bappend (enc (char-code ch) #'s-path)))))
                  (:opaque-path
                   (cond
                     ((eql ch #\?) (setf (url-query u) "" state :query))
                     ((eql ch #\#) (setf (url-fragment u) "" state :fragment))
                     ((c) (setf (url-path u) (concatenate 'string (url-path u) (enc (char-code ch) #'s-c0))))))
                  (:query
                   (cond
                     ((and (eql ch #\#)) (setf (url-fragment u) "" state :fragment))
                     ((c) (setf (url-query u)
                                (concatenate 'string (url-query u)
                                             (enc (char-code ch) (if (special) #'s-special-query #'s-query)))))))
                  (:fragment
                   (when (c) (setf (url-fragment u)
                                   (concatenate 'string (url-fragment u) (enc (char-code ch) #'s-frag)))))))
              (incf ptr)
              (when (> ptr len) (return))))
          u)))))

(defun sgl-dot-p (s) (or (string= s ".") (string-equal s "%2e")))
(defun dbl-dot-p (s)
  (member s '(".." ".%2e" "%2e." "%2e%2e") :test #'string-equal))

;;; ---- public entry -----------------------------------------------------

(defun parse (input &optional base-string)
  "Parse INPUT (optionally against BASE-STRING).  Returns a URL or NIL on failure."
  (let ((base (when (and base-string (stringp base-string))
                (let ((b (basic-parse base-string))) (unless (eq b :failure) b)))))
    (when (and base-string (stringp base-string) (null base)) (return-from parse nil))
    (let ((u (basic-parse input base))) (unless (eq u :failure) u))))

;;; ---- serialization ----------------------------------------------------

(defun serialize-host (host)
  (cond
    ((null host) "")
    ((stringp host) host)
    ((integerp host)                                    ; IPv4
     (format nil "~d.~d.~d.~d" (ldb (byte 8 24) host) (ldb (byte 8 16) host)
             (ldb (byte 8 8) host) (ldb (byte 8 0) host)))
    (t (concatenate 'string "[" (serialize-ipv6 host) "]"))))

(defun serialize-ipv6 (addr)
  ;; find longest run (>1) of zero pieces to compress
  (let ((best-i -1) (best-len 0) (cur-i -1) (cur-len 0))
    (dotimes (i 8)
      (if (zerop (aref addr i))
          (progn (when (< cur-i 0) (setf cur-i i)) (incf cur-len)
                 (when (> cur-len best-len) (setf best-len cur-len best-i cur-i)))
          (setf cur-i -1 cur-len 0)))
    (when (< best-len 2) (setf best-i -1))
    (with-output-to-string (s)
      (let ((i 0))
        (loop while (< i 8) do
          (cond
            ((= i best-i) (write-string (if (= i 0) "::" ":") s) (incf i best-len))
            (t (format s "~(~x~)" (aref addr i))
               (incf i)
               (when (< i 8) (write-char #\: s)))))))))

(defun protocol (u) (concatenate 'string (url-scheme u) ":"))
(defun username (u) (url-username u))
(defun password (u) (url-password u))
(defun hostname (u) (serialize-host (url-host u)))
(defun host-str (u)
  (if (url-port u) (format nil "~a:~d" (serialize-host (url-host u)) (url-port u))
      (serialize-host (url-host u))))
(defun port-str (u) (if (url-port u) (format nil "~d" (url-port u)) ""))

(defun pathname-str (u)
  (if (url-opaque-path-p u) (url-path u)
      (with-output-to-string (s) (loop for seg across (url-path u) do (write-char #\/ s) (write-string seg s)))))

(defun search-str (u) (if (and (url-query u) (not (string= (url-query u) ""))) (concatenate 'string "?" (url-query u)) ""))
(defun hash-str (u) (if (and (url-fragment u) (not (string= (url-fragment u) ""))) (concatenate 'string "#" (url-fragment u)) ""))

(defun href (u)
  (with-output-to-string (s)
    (write-string (url-scheme u) s) (write-char #\: s)
    (when (url-host u)
      (write-string "//" s)
      (when (or (not (string= (url-username u) "")) (not (string= (url-password u) "")))
        (write-string (url-username u) s)
        (unless (string= (url-password u) "") (write-char #\: s) (write-string (url-password u) s))
        (write-char #\@ s))
      (write-string (serialize-host (url-host u)) s)
      (when (url-port u) (write-char #\: s) (format s "~d" (url-port u))))
    (when (and (null (url-host u)) (not (url-opaque-path-p u))
               (> (length (url-path u)) 1) (string= (aref (url-path u) 0) ""))
      (write-string "/." s))
    (write-string (pathname-str u) s)
    (when (url-query u) (write-char #\? s) (write-string (url-query u) s))
    (when (url-fragment u) (write-char #\# s) (write-string (url-fragment u) s))))

(defun origin (u)
  (let ((sch (url-scheme u)))
    (cond
      ((member sch '("http" "https" "ws" "wss" "ftp") :test #'string=)
       (format nil "~a://~a~@[:~d~]" sch (serialize-host (url-host u)) (url-port u)))
      ((string= sch "blob")
       (let ((inner (ignore-errors (parse (pathname-str u)))))
         (if (and inner (member (url-scheme inner) '("http" "https") :test #'string=))
             (origin inner) "null")))
      (t "null"))))
