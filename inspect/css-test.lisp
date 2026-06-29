;;;; inspect/css-test.lisp — CSS value-parser differential gate.
;;;;
;;;; inspect/vectors/css/<type>.json = {type, cases:[[input, expected]]} produced
;;;; by a reference (webcolors / spec-precise Python).  PARSE-VALUE of each input
;;;; must equal the expected normalized form (a list, number, or :invalid for null).
(defpackage #:weft.css.test (:use #:cl) (:local-nicknames (#:c #:weft.css)) (:export #:run))
(in-package #:weft.css.test)

(defun json-parse (string)
  (let ((i 0) (n (length string)))
    (labels
        ((peek () (when (< i n) (char string i)))
         (next () (prog1 (char string i) (incf i)))
         (ws () (loop while (and (< i n) (member (char string i) '(#\Space #\Tab #\Newline #\Return))) do (incf i)))
         (value () (ws) (let ((ch (peek)))
                          (cond ((char= ch #\{) (object)) ((char= ch #\[) (array)) ((char= ch #\") (jstr))
                                ((or (digit-char-p ch) (member ch '(#\- #\+ #\.))) (jnum))
                                ((char= ch #\t) (incf i 4) :true) ((char= ch #\f) (incf i 5) :false)
                                ((char= ch #\n) (incf i 4) :null))))
         (object () (next) (ws) (let ((al '())) (when (char= (peek) #\}) (next) (return-from object '()))
                                  (loop (ws) (let ((k (jstr))) (ws) (next) (push (cons k (value)) al)) (ws)
                                        (if (char= (peek) #\,) (next) (progn (next) (return)))) (nreverse al)))
         (array () (next) (ws) (let ((items '())) (when (char= (peek) #\]) (next) (return-from array '()))
                                 (loop (push (value) items) (ws) (if (char= (peek) #\,) (next) (progn (next) (return)))) (nreverse items)))
         (jstr () (next) (with-output-to-string (o)
                           (loop for ch = (next) until (char= ch #\") do
                             (if (char= ch #\\)
                                 (let ((e (next))) (case e (#\n (write-char #\Newline o)) (#\t (write-char #\Tab o))
                                                     (#\/ (write-char #\/ o)) (#\\ (write-char #\\ o)) (#\" (write-char #\" o))
                                                     (#\u (let ((cp (parse-integer string :start i :end (+ i 4) :radix 16))) (incf i 4) (write-char (code-char cp) o)))
                                                     (t (write-char e o))))
                                 (write-char ch o)))))
         (jnum () (let ((st i)) (loop while (and (< i n) (or (digit-char-p (char string i)) (member (char string i) '(#\- #\+ #\. #\e #\E)))) do (incf i))
                    (read-from-string (subseq string st i)))))
      (value))))

(defun slurp (p) (with-open-file (s p :external-format :utf-8) (let ((str (make-string (file-length s)))) (subseq str 0 (read-sequence str s)))))
(defun field (o k) (cdr (assoc k o :test #'string=)))

(defun ~= (a b)
  "Loose equality: :null<->:invalid, numeric tolerance, recurse lists."
  (cond
    ((and (member a '(:invalid :null)) (member b '(:invalid :null))) t)
    ((and (numberp a) (numberp b)) (< (abs (- a b)) 1d-3))
    ((and (listp a) (listp b)) (and (= (length a) (length b)) (every #'~= a b)))
    (t (equal a b))))

(defun run (&optional only)
  (let* ((dir (asdf:system-relative-pathname "weft" "inspect/vectors/css/"))
         (files (sort (directory (merge-pathnames "*.json" dir)) #'string< :key #'namestring))
         (tp 0) (tf 0) (fails '()))
    (format t "~&=== weft CSS value gate ===~%")
    (dolist (f files)
      (when (or (null only) (search only (pathname-name f)))
        (let* ((obj (json-parse (slurp f))) (type (field obj "type")) (pass 0) (fail 0))
          (when type                            ; skip non-value-parser json (e.g. selectors)
            (dolist (cs (field obj "cases"))
              (let* ((in (first cs)) (want (second cs))
                     (got (ignore-errors (c:parse-value type in))))
                (if (~= (if (eq got :invalid) :null got) want) (incf pass)
                    (progn (incf fail) (when (< (length fails) 12)
                                         (push (format nil "[~a] ~s -> want ~s got ~s" type in want got) fails))))))
            (incf tp pass) (incf tf fail)
            (format t "  ~a ~10a ~4d/~d~%" (if (zerop fail) "ok  " "FAIL") type pass (+ pass fail))))))
    (when (and only (zerop (+ tp tf))) (format t "NO CASES (type ~a)~%" only) (setf tf 1))
    (format t "~%~d passed, ~d failed~%" tp tf)
    (when fails (format t "~%sample:~%~{  ~a~%~}" (reverse fails)))
    (values tp tf)))
