;;;; inspect/encoding-test.lisp — differential gate for the Encoding decoders.
;;;;
;;;; Each inspect/vectors/encoding/<label>.json holds {charset, python_codec,
;;;; cases:[[hex-input, expected-string]...]} produced by the reference codec
;;;; (errors="replace").  Decoding each hex input under weft.encoding must equal
;;;; the expected string.  RUN with an optional label to test one charset (used
;;;; by the parallel build workers as their oracle).

(defpackage #:weft.encoding.test
  (:use #:cl) (:local-nicknames (#:e #:weft.encoding)) (:export #:run))
(in-package #:weft.encoding.test)

;;; minimal JSON reader (objects/arrays/strings/true/false/null/number)
(defun json-parse (string)
  (let ((i 0) (n (length string)))
    (labels
        ((peek () (when (< i n) (char string i)))
         (next () (prog1 (char string i) (incf i)))
         (ws () (loop while (and (< i n) (member (char string i) '(#\Space #\Tab #\Newline #\Return))) do (incf i)))
         (value () (ws) (let ((c (peek)))
                          (cond ((char= c #\{) (object)) ((char= c #\[) (array)) ((char= c #\") (jstring))
                                ((or (digit-char-p c) (char= c #\-)) (number))
                                ((char= c #\t) (incf i 4) :true) ((char= c #\f) (incf i 5) :false)
                                ((char= c #\n) (incf i 4) :null))))
         (object () (next) (ws)
           (let ((al '())) (when (char= (peek) #\}) (next) (return-from object '()))
             (loop (ws) (let ((k (jstring))) (ws) (next) (push (cons k (value)) al)) (ws)
                   (if (char= (peek) #\,) (next) (progn (next) (return)))) (nreverse al)))
         (array () (next) (ws)
           (let ((items '())) (when (char= (peek) #\]) (next) (return-from array '()))
             (loop (push (value) items) (ws) (if (char= (peek) #\,) (next) (progn (next) (return)))) (nreverse items)))
         (jstring () (next)
           (with-output-to-string (o)
             (loop for c = (next) until (char= c #\") do
               (if (char= c #\\)
                   (let ((e (next)))
                     (case e (#\n (write-char #\Newline o)) (#\t (write-char #\Tab o))
                       (#\r (write-char #\Return o)) (#\b (write-char #\Backspace o)) (#\f (write-char #\Page o))
                       (#\/ (write-char #\/ o)) (#\\ (write-char #\\ o)) (#\" (write-char #\" o))
                       (#\u (let ((code (parse-integer string :start i :end (+ i 4) :radix 16)))
                              (incf i 4) (write-char (code-char code) o)))
                       (t (write-char e o))))
                   (write-char c o)))))
         (number () (let ((s i)) (loop while (and (< i n) (or (digit-char-p (char string i)) (member (char string i) '(#\- #\+ #\. #\e #\E)))) do (incf i))
                      (read-from-string (subseq string s i)))))
      (value))))

(defun slurp (path)
  (with-open-file (s path :element-type 'character :external-format :utf-8)
    (let ((str (make-string (file-length s)))) (subseq str 0 (read-sequence str s)))))

(defun field (obj key) (cdr (assoc key obj :test #'string=)))

(defun hex->bytes (hex)
  (let ((b (make-array (floor (length hex) 2) :element-type '(unsigned-byte 8))))
    (dotimes (k (length b)) (setf (aref b k) (parse-integer hex :start (* 2 k) :end (+ 2 (* 2 k)) :radix 16)))
    b))

(defun test-file (path)
  "Returns (values charset pass fail first-failure-string)."
  (let* ((obj (json-parse (slurp path)))
         (charset (field obj "charset")) (cases (field obj "cases"))
         (pass 0) (fail 0) (firstbad nil))
    (dolist (cs cases)
      (let* ((hex (first cs)) (want (second cs))
             (got (ignore-errors (e:decode charset (hex->bytes hex)))))
        (if (and (stringp got) (string= got want)) (incf pass)
            (progn (incf fail)
                   (unless firstbad
                     (setf firstbad (format nil "~a: in=~a want=~s got=~s" charset hex want got)))))))
    (values charset pass fail firstbad)))

(defun run (&optional only)
  "Run the encoding gate.  ONLY restricts to one charset label (substring match)."
  (let* ((dir (asdf:system-relative-pathname "weft" "inspect/vectors/encoding/"))
         (files (sort (directory (merge-pathnames "*.json" dir)) #'string< :key #'namestring))
         (tp 0) (tf 0))
    (format t "~&=== weft encoding gate ===~%")
    (dolist (f files)
      (when (or (null only) (search only (pathname-name f)))
        (multiple-value-bind (charset pass fail bad) (test-file f)
          (incf tp pass) (incf tf fail)
          (format t "  ~a ~14a ~5d/~d~@[  ~a~]~%"
                  (if (zerop fail) "ok  " "FAIL") charset pass (+ pass fail)
                  (and bad (zerop pass) bad))
          (when (and (plusp fail) bad) (format t "       e.g. ~a~%" bad)))))
    (format t "~%~d passed, ~d failed~%" tp tf)
    (when (and only (plusp tf)) (format t "(charset ~a NOT yet passing)~%" only))
    (when (and only (zerop tf) (plusp tp)) (format t "(charset ~a: ALL PASS)~%" only))
    (values tp tf)))
