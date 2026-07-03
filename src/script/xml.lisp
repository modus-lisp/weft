;;;; src/script/xml.lisp — a small, well-formedness-checking XML parser for
;;;; XHTML frame documents, plus namespace-scoped script execution.
;;;;
;;;; Enough XML to load an application/xhtml+xml document into a browsing context:
;;;; elements with quoted attributes, self-closing tags, character data, comments,
;;;; PIs and doctype skipping, the predefined entities and numeric references, and
;;;; xmlns default-namespace tracking.  Unlike the HTML parser it is strict: a
;;;; mismatched or unclosed tag, or more than one root element, makes the parse
;;;; non-well-formed (so a broken XHTML document runs no scripts), and only
;;;; http://www.w3.org/1999/xhtml <script> elements execute.
(in-package #:weft.script)

(defparameter +xhtml-ns+ "http://www.w3.org/1999/xhtml")

(defun xml-decode-entities (s)
  "Decode &amp; &lt; &gt; &quot; &apos; and &#nn; / &#xhh; references in S."
  (if (null (position #\& s)) s
      (with-output-to-string (o)
        (let ((i 0) (n (length s)))
          (loop while (< i n) do
            (let ((c (char s i)))
              (if (and (char= c #\&) (< (1+ i) n))
                  (let ((semi (position #\; s :start i :end (min n (+ i 12)))))
                    (if (null semi) (progn (write-char c o) (incf i))
                        (let ((ent (subseq s (1+ i) semi)))
                          (cond
                            ((string= ent "amp") (write-char #\& o))
                            ((string= ent "lt") (write-char #\< o))
                            ((string= ent "gt") (write-char #\> o))
                            ((string= ent "quot") (write-char #\" o))
                            ((string= ent "apos") (write-char #\' o))
                            ((and (plusp (length ent)) (char= (char ent 0) #\#))
                             (let* ((hex (and (> (length ent) 1) (member (char ent 1) '(#\x #\X))))
                                    (code (ignore-errors
                                           (parse-integer ent :start (if hex 2 1) :radix (if hex 16 10)))))
                               (if code (write-char (code-char code) o) (write-string (subseq s i (1+ semi)) o))))
                            (t (write-string (subseq s i (1+ semi)) o)))
                          (setf i (1+ semi)))))
                  (progn (write-char c o) (incf i)))))))))

(defun xml-name-end (s i n)
  (loop while (and (< i n) (not (member (char s i) '(#\Space #\Tab #\Newline #\Return #\/ #\> #\=)))) do (incf i))
  i)

(defun xml-skip-ws (s i n)
  (loop while (and (< i n) (member (char s i) '(#\Space #\Tab #\Newline #\Return))) do (incf i))
  i)

(defun xml-parse-attrs (s i n)
  "Parse attributes from I up to '>' or '/>'.  Returns (values alist end quoted-ok);
QUOTED-OK is NIL if any value is unquoted (a well-formedness error in XML)."
  (let ((attrs '()) (quoted-ok t))
    (loop
      (setf i (xml-skip-ws s i n))
      (when (or (>= i n) (member (char s i) '(#\> #\/))) (return))
      (let ((ne (xml-name-end s i n)))
        (if (= ne i)
            (progn (setf quoted-ok nil) (incf i))       ; stray char
            (let ((name (subseq s i ne)) (j (xml-skip-ws s ne n)))
              (if (and (< j n) (char= (char s j) #\=))
                  (let ((k (xml-skip-ws s (1+ j) n)))
                    (if (and (< k n) (member (char s k) '(#\" #\')))
                        (let ((close (or (position (char s k) s :start (1+ k) :end n) n)))
                          (push (cons name (xml-decode-entities (subseq s (1+ k) close))) attrs)
                          (setf i (min n (1+ close))))
                        (let ((e (xml-name-end s k n)))   ; unquoted value: not well-formed
                          (setf quoted-ok nil)
                          (push (cons name (subseq s k e)) attrs) (setf i e))))
                  (progn (push (cons name "") attrs) (setf i j)))))))
    (values (nreverse attrs) i quoted-ok)))

(defun xml-parse-document (source)
  "Parse SOURCE as XML into a weft DOM document.  Returns (values document
well-formed-p ns-map), where NS-MAP maps each element dnode to its namespace URI.
Well-formedness requires matched start/end tags and exactly one root element."
  (let* ((s source) (n (length s)) (i 0)
         (doc (h:make-document)) (stack '()) (names '())
         (nsstack (list nil)) (nsmap (make-hash-table :test 'eq))
         (root-count 0) (ok t))
    (labels ((cur () (first stack))
             (add-text (str)
               (let ((d (xml-decode-entities str)))
                 (when (and (cur) (plusp (length d)))
                   (h:dom-append (cur) (h:make-text d)))))
             (elt-ns (attrs)
               (or (cdr (assoc "xmlns" attrs :test #'string=)) (first nsstack))))
      (loop while (< i n) do
        (let ((lt (position #\< s :start i)))
          (cond
            ((null lt) (add-text (subseq s i n)) (setf i n))
            (t
             (when (> lt i) (add-text (subseq s i lt)))
             (setf i lt)
             (cond
               ((and (<= (+ i 4) n) (string= (subseq s i (+ i 4)) "<!--"))
                (let ((e (search "-->" s :start2 (+ i 4)))) (if e (setf i (+ e 3)) (setf ok nil i n))))
               ((and (<= (+ i 9) n) (string= (subseq s i (+ i 9)) "<![CDATA["))
                (let ((e (search "]]>" s :start2 (+ i 9))))
                  (when (and e (cur)) (h:dom-append (cur) (h:make-text (subseq s (+ i 9) e))))
                  (if e (setf i (+ e 3)) (setf ok nil i n))))
               ((and (< (1+ i) n) (char= (char s (1+ i)) #\!))
                (let ((e (position #\> s :start i))) (if e (setf i (1+ e)) (setf ok nil i n))))
               ((and (< (1+ i) n) (char= (char s (1+ i)) #\?))
                (let ((e (search "?>" s :start2 i))) (if e (setf i (+ e 2)) (setf ok nil i n))))
               ;; end tag: must match the currently open element
               ((and (< (1+ i) n) (char= (char s (1+ i)) #\/))
                (let* ((e (position #\> s :start i))
                       (nm (string-trim '(#\Space #\Tab #\Newline #\Return)
                                        (subseq s (+ i 2) (or e n)))))
                  (if (and stack (string= (first names) nm))
                      (progn (pop stack) (pop names) (pop nsstack))
                      (setf ok nil))
                  (setf i (if e (1+ e) n))))
               ;; start tag
               (t
                (let ((ne (xml-name-end s (1+ i) n)))
                  (if (= ne (1+ i))
                      (progn (setf ok nil) (incf i))
                      (let ((name (subseq s (1+ i) ne)))
                        (multiple-value-bind (attrs j quoted-ok) (xml-parse-attrs s ne n)
                          (unless quoted-ok (setf ok nil))
                          (let* ((self (and (< j n) (char= (char s j) #\/)))
                                 (nsuri (elt-ns attrs))
                                 (kw (cond ((equal nsuri +xhtml-ns+) :html)
                                           ((and nsuri (search "svg" nsuri)) :svg)
                                           (t :html)))
                                 (el (h:make-element name attrs kw)))
                            (setf (gethash el nsmap) nsuri)
                            (if (cur) (h:dom-append (cur) el)
                                (progn (h:dom-append doc el) (incf root-count)))
                            (unless self (push el stack) (push name names) (push nsuri nsstack))
                            (let ((e (position #\> s :start j))) (setf i (if e (1+ e) n)))))))))))))))
    (when (or stack (/= root-count 1)) (setf ok nil))
    (values doc ok nsmap)))

(defun xhtml-content-p (s)
  "True when S looks like an XHTML document: an <html> root element carrying an
xmlns declaration (so it parses as XML, not HTML).  Leading BOM/whitespace, an XML
declaration, comments and a doctype are skipped."
  (let ((i 0) (n (length s)))
    (loop
      (setf i (xml-skip-ws s i n))
      (cond
        ((>= i n) (return nil))
        ((not (char= (char s i) #\<)) (return nil))
        ((and (< (1+ i) n) (char= (char s (1+ i)) #\?))
         (let ((e (search "?>" s :start2 i))) (setf i (if e (+ e 2) n))))
        ((and (<= (+ i 4) n) (string= (subseq s i (+ i 4)) "<!--"))
         (let ((e (search "-->" s :start2 i))) (setf i (if e (+ e 3) n))))
        ((and (< (1+ i) n) (char= (char s (1+ i)) #\!))
         (let ((e (position #\> s :start i))) (setf i (if e (1+ e) n))))
        (t
         (let* ((ne (xml-name-end s (1+ i) n))
                (name (string-downcase (subseq s (1+ i) ne)))
                (gt (position #\> s :start i))
                (tag (subseq s i (if gt (1+ gt) n))))
           (return (and (string= name "html") (search "xmlns" tag)))))))))

(defun run-xhtml-frame-scripts (ctx doc nsmap)
  "Execute the classic-JavaScript <script> elements of a parsed XHTML frame DOC
whose namespace is exactly http://www.w3.org/1999/xhtml, against CTX's realm (so
they see the parent window).  A wrong-namespace script is left un-run."
  (let ((realm (context-realm ctx)))
    (dolist (script (dom:get-elements-by-tag-name doc "script"))
      (when (and (equal (gethash script nsmap) +xhtml-ns+)
                 (classic-javascript-p script))
        (let ((src (dom:text-content script)))
          (when (and src (plusp (length src)))
            (handler-case (js:eval-script realm src)
              (error (e)
                (format *error-output* "~&weft.script: xhtml frame script error: ~a~%" e)))))))))
