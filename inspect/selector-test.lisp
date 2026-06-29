;;;; inspect/selector-test.lisp — CSS selector differential gate (vs soupsieve).
;;;;
;;;; inspect/vectors/css/selectors.json = {html, cases:[{selector, matches}]}
;;;; where matches is a list of node-paths produced by soupsieve on the same
;;;; HTML.  query-select-all on the weft-parsed DOM must match the same set.
(defpackage #:weft.css.select-test
  (:use #:cl) (:local-nicknames (#:c #:weft.css) (#:h #:weft.html)) (:export #:run))
(in-package #:weft.css.select-test)

(defun json-parse (string)
  (let ((i 0) (n (length string)))
    (labels
        ((peek () (when (< i n) (char string i)))
         (next () (prog1 (char string i) (incf i)))
         (ws () (loop while (and (< i n) (member (char string i) '(#\Space #\Tab #\Newline #\Return))) do (incf i)))
         (value () (ws) (let ((ch (peek)))
                          (cond ((char= ch #\{) (object)) ((char= ch #\[) (array)) ((char= ch #\") (jstr))
                                ((or (digit-char-p ch) (member ch '(#\- #\+))) (jnum))
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

(defun node-path (node)
  (let ((p '()) (n node))
    (loop for par = (h:dnode-parent n) while par do
      (push (position n (h:dnode-children par)) p) (setf n par))
    p))

(defun run ()
  (let* ((obj (json-parse (slurp (asdf:system-relative-pathname "weft" "inspect/vectors/css/selectors.json"))))
         (doc (h:parse-html (field obj "html"))) (pass 0) (fail 0) (fails '()))
    (format t "~&=== weft CSS selector gate (vs soupsieve) ===~%")
    (dolist (cs (field obj "cases"))
      (let* ((sel (field cs "selector")) (want (field cs "matches")))
        (unless (eq want :null)
          (let* ((got (ignore-errors (mapcar #'node-path (c:query-select-all doc sel))))
                 (want* (mapcar (lambda (p) (if (eq p :null) nil p)) want)))
            (if (equal got want*) (incf pass)
                (progn (incf fail) (when (< (length fails) 14)
                                     (push (format nil "~s want ~s got ~s" sel want* got) fails))))))))
    (format t "~%~d passed, ~d failed~%" pass fail)
    (when fails (format t "~%sample:~%~{  ~a~%~}" (reverse fails)))
    (values pass fail)))
