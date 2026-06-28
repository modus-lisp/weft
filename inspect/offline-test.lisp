;;;; inspect/offline-test.lisp — weft offline gate.
;;;;
;;;; Differential-tests the URL parser against the Web Platform Tests url corpus
;;;; (inspect/vectors/urltestdata.json): for each case, parse INPUT against BASE
;;;; and compare every serialized component to the expected value (or, for
;;;; "failure" cases, require a parse failure).  A self-contained JSON reader
;;;; keeps the oracle in its canonical upstream form.

(defpackage #:weft.test (:use #:cl) (:local-nicknames (#:u #:weft.url)) (:export #:run))
(in-package #:weft.test)

;;; ---- minimal JSON reader ---------------------------------------------

(defun json-parse (string)
  (let ((i 0) (n (length string)))
    (labels
        ((peek () (when (< i n) (char string i)))
         (next () (prog1 (char string i) (incf i)))
         (ws () (loop while (and (< i n) (member (char string i) '(#\Space #\Tab #\Newline #\Return))) do (incf i)))
         (value ()
           (ws)
           (let ((c (peek)))
             (cond ((char= c #\{) (object)) ((char= c #\[) (array)) ((char= c #\") (jstring))
                   ((or (digit-char-p c) (char= c #\-)) (number))
                   ((char= c #\t) (incf i 4) :true)
                   ((char= c #\f) (incf i 5) :false)
                   ((char= c #\n) (incf i 4) :null))))
         (object ()
           (next) (ws)
           (let ((alist '()))
             (when (char= (peek) #\}) (next) (return-from object '()))
             (loop
               (ws) (let ((k (jstring))) (ws) (next) ; ':'
                      (push (cons k (value)) alist)) (ws)
               (if (char= (peek) #\,) (next) (progn (next) (return))))
             (nreverse alist)))
         (array ()
           (next) (ws)
           (let ((items '()))
             (when (char= (peek) #\]) (next) (return-from array '()))
             (loop (push (value) items) (ws)
                   (if (char= (peek) #\,) (next) (progn (next) (return))))
             (nreverse items)))
         (jstring ()
           (next)                                       ; opening "
           (with-output-to-string (o)
             (loop for c = (next) until (char= c #\") do
               (if (char= c #\\)
                   (let ((e (next)))
                     (case e
                       (#\n (write-char #\Newline o)) (#\t (write-char #\Tab o))
                       (#\r (write-char #\Return o)) (#\b (write-char #\Backspace o))
                       (#\f (write-char #\Page o)) (#\/ (write-char #\/ o))
                       (#\\ (write-char #\\ o)) (#\" (write-char #\" o))
                       (#\u (let ((code (parse-integer string :start i :end (+ i 4) :radix 16)))
                              (incf i 4) (write-char (code-char code) o)))
                       (t (write-char e o))))
                   (write-char c o)))))
         (number ()
           (let ((start i))
             (loop while (and (< i n) (or (digit-char-p (char string i)) (member (char string i) '(#\- #\+ #\. #\e #\E)))) do (incf i))
             (read-from-string (subseq string start i)))))
      (value))))

;;; ---- harness ----------------------------------------------------------

(defun slurp (path)
  (with-open-file (s path :element-type 'character :external-format :utf-8)
    (let ((str (make-string (file-length s)))) (subseq str 0 (read-sequence str s)))))

(defun field (case key) (cdr (assoc key case :test #'string=)))

(defparameter *checks*
  `(("protocol" . ,#'u:protocol) ("username" . ,#'u:username) ("password" . ,#'u:password)
    ("host" . ,#'u:host-str) ("hostname" . ,#'u:hostname) ("port" . ,#'u:port-str)
    ("pathname" . ,#'u:pathname-str) ("search" . ,#'u:search-str) ("hash" . ,#'u:hash-str)
    ("href" . ,#'u:href) ("origin" . ,#'u:origin)))

(defun run ()
  (let* ((path (asdf:system-relative-pathname "weft" "inspect/vectors/urltestdata.json"))
         (cases (remove-if-not #'consp (json-parse (slurp path))))
         (pass 0) (fail 0) (fails '()))
    (format t "~&=== weft URL gate (WPT urltestdata.json) ===~%")
    (dolist (cs cases)
      (let* ((input (field cs "input"))
             (base (let ((b (field cs "base"))) (if (stringp b) b nil)))
             (expect-fail (eq (field cs "failure") :true)))
        (when (stringp input)
          (let ((url (ignore-errors (u:parse input base))))
            (cond
              (expect-fail
               (if (null url) (incf pass)
                   (progn (incf fail) (push (format nil "~s base=~s: expected FAILURE, got ~s" input base (ignore-errors (u:href url))) fails))))
              ((null url)
               (incf fail) (push (format nil "~s base=~s: unexpected failure" input base) fails))
              (t (let ((bad nil))
                   (dolist (chk *checks*)
                     (let ((want (field cs (car chk))))
                       (when (stringp want)
                         (let ((got (ignore-errors (funcall (cdr chk) url))))
                           (unless (equal got want)
                             (setf bad (format nil "~s base=~s [~a] want ~s got ~s" input base (car chk) want got)))))))
                   (if bad (progn (incf fail) (push bad fails)) (incf pass)))))))))
    (format t "~%~d passed, ~d failed (of ~d)~%" pass fail (+ pass fail))
    (when fails
      (format t "~%first failures:~%~{  ~a~%~}" (subseq (reverse fails) 0 (min 25 (length fails)))))
    (when (plusp fail) (format t "~%(conformance: ~,1f%)~%" (* 100.0 (/ pass (+ pass fail)))))
    (values pass fail)))
