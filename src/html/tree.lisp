;;;; src/html/tree.lisp — HTML tree construction (WHATWG §13.2.6), core modes.
;;;;
;;;; Consumes tokenizer tokens into a DOM document via the insertion-mode state
;;;; machine.  This is the start of the tall pole: it covers the document
;;;; skeleton (initial/before-html/before-head/in-head/after-head) and the bulk
;;;; of "in body" (text, blocks with implied <p> closing, headings, list items,
;;;; phrasing/formatting, generic start/end with implied end tags).  Adoption
;;;; agency, tables, foreign content, fragments, and tokenizer-state reentrancy
;;;; (script/style/title raw text) are follow-ups.
(in-package #:weft.html)

(defparameter *void* '("area" "base" "br" "col" "embed" "hr" "img" "input"
                       "keygen" "link" "meta" "param" "source" "track" "wbr"))
(defparameter *head-void* '("base" "basefont" "bgsound" "link" "meta"))
(defparameter *block-closes-p*
  '("address" "article" "aside" "blockquote" "center" "details" "dialog" "dir"
    "div" "dl" "fieldset" "figcaption" "figure" "footer" "header" "hgroup" "main"
    "menu" "nav" "ol" "p" "section" "summary" "ul" "pre" "listing" "table"
    "hr" "form" "fieldset"))
(defparameter *headings* '("h1" "h2" "h3" "h4" "h5" "h6"))
(defparameter *implied-end* '("dd" "dt" "li" "optgroup" "option" "p" "rb" "rp" "rt" "rtc"))
(defparameter *ws* '(#\Tab #\Newline #\Page #\Return #\Space))

(defun ws-char-tok-p (tk)
  (and (eq (tok-type tk) :char) (= 1 (length (tok-data tk)))
       (member (char (tok-data tk) 0) *ws*)))

(defparameter *rawtext-els* '("style" "xmp" "iframe" "noembed" "noframes" "script"))
(defparameter *rcdata-els* '("title" "textarea"))

(defun parse-html (input)
  "Parse an HTML string into a DOM document (core tree construction)."
  (multiple-value-bind (tklist src) (tokenize input)
   (let* ((toks (coerce tklist 'vector)) (ntok (length toks)) (s src)
         (doc (make-document)) (open '()) (mode :initial) (head nil) (i 0))
    (labels ((current () (car open))
             (rcdata-decode (raw)
               (with-output-to-string (o)
                 (dolist (x (tokenize raw :state :rcdata))
                   (when (eq (tok-type x) :char) (write-string (tok-data x) o)))))
             (raw-element (tk rcdata-p)
               ;; insert a raw-text/RCDATA element, fill it with the raw source up
               ;; to the matching end tag, pop it, and skip past that end tag.
               (let* ((name (tok-name tk)) (el (make-element name (tok-attrs tk))))
                 (dom-append (current) el)
                 (let* ((end-i (position-if (lambda (x) (and (eq (tok-type x) :end-tag)
                                                             (equal (tok-name x) name)))
                                            toks :start (1+ i)))
                        (endtok (and end-i (aref toks end-i)))
                        (raw (subseq s (1+ (tok-cend tk)) (if endtok (tok-pos endtok) (length s)))))
                   (when (plusp (length raw))
                     (dom-append el (make-text (if rcdata-p (rcdata-decode raw) raw))))
                   (setf i (if end-i end-i (1- ntok))))))
             (top-name () (and open (dnode-name (current))))
             (push-el (el) (push el open) el)
             (insert-element (name &optional attrs (ns :html))
               (let ((el (make-element name attrs ns))) (dom-append (current) el) (push-el el) el))
             (insert-void (name &optional attrs)
               (dom-append (current) (make-element name attrs)))
             (insert-text (data)
               (let ((last (dom-last-child (current))))
                 (if (and last (eq (dnode-kind last) :text))
                     (setf (dnode-data last) (concatenate 'string (dnode-data last) data))
                     (dom-append (current) (make-text data)))))
             (in-scope (name &optional (bounds '("html" "table" "td" "th" "caption" "button" "marquee" "object")))
               (loop for el in open do
                 (cond ((equal (dnode-name el) name) (return t))
                       ((member (dnode-name el) bounds :test #'equal) (return nil)))))
             (gen-implied (&optional except)
               (loop while (and open (member (top-name) *implied-end* :test #'equal)
                                (not (equal (top-name) except)))
                     do (pop open)))
             (close-p () (when (in-scope "p" '("html" "table" "td" "th" "caption" "button" "marquee" "object"))
                           (gen-implied "p")
                           (loop while (and open (not (equal (top-name) "p"))) do (pop open))
                           (when open (pop open))))
             (pop-until (name)
               (gen-implied name)
               (when (in-scope name)
                 (loop while (and open (not (equal (top-name) name))) do (pop open))
                 (when open (pop open)))))
      (flet ((switch (m) (setf mode m)))
        (loop
          (when (>= i ntok) (return))
          (let* ((tk (aref toks i)) (reconsume nil) (ty (tok-type tk)))
            (macrolet ((reproc () `(setf reconsume t)))
              (ecase mode
                (:initial
                 (cond ((eq ty :doctype)
                        (dom-append doc (%dnode :kind :doctype :name (tok-name tk)
                                                :public (tok-public tk) :system (tok-system tk)))
                        (switch :before-html))
                       ((eq ty :comment) (dom-append doc (make-comment (tok-data tk))))
                       ((ws-char-tok-p tk))
                       (t (switch :before-html) (reproc))))
                (:before-html
                 (cond ((eq ty :comment) (dom-append doc (make-comment (tok-data tk))))
                       ((ws-char-tok-p tk))
                       ((and (eq ty :start-tag) (equal (tok-name tk) "html"))
                        (let ((el (make-element "html" (tok-attrs tk)))) (dom-append doc el) (push-el el))
                        (switch :before-head))
                       (t (let ((el (make-element "html"))) (dom-append doc el) (push-el el))
                          (switch :before-head) (reproc))))
                (:before-head
                 (cond ((ws-char-tok-p tk))
                       ((eq ty :comment) (dom-append (current) (make-comment (tok-data tk))))
                       ((and (eq ty :start-tag) (equal (tok-name tk) "head"))
                        (setf head (insert-element "head" (tok-attrs tk))) (switch :in-head))
                       (t (setf head (insert-element "head")) (switch :in-head) (reproc))))
                (:in-head
                 (cond ((ws-char-tok-p tk) (insert-text (tok-data tk)))
                       ((eq ty :comment) (dom-append (current) (make-comment (tok-data tk))))
                       ((and (eq ty :start-tag) (member (tok-name tk) *head-void* :test #'equal))
                        (insert-void (tok-name tk) (tok-attrs tk)))
                       ((and (eq ty :start-tag) (equal (tok-name tk) "meta"))
                        (insert-void "meta" (tok-attrs tk)))
                       ((and (eq ty :start-tag) (member (tok-name tk) *rcdata-els* :test #'equal)) (raw-element tk t))
                       ((and (eq ty :start-tag) (member (tok-name tk) *rawtext-els* :test #'equal)) (raw-element tk nil))
                       ((and (eq ty :end-tag) (equal (tok-name tk) "head")) (pop open) (switch :after-head))
                       (t (pop open) (switch :after-head) (reproc))))
                (:after-head
                 (cond ((ws-char-tok-p tk) (insert-text (tok-data tk)))
                       ((eq ty :comment) (dom-append (current) (make-comment (tok-data tk))))
                       ((and (eq ty :start-tag) (equal (tok-name tk) "body"))
                        (insert-element "body" (tok-attrs tk)) (switch :in-body))
                       ((and (eq ty :start-tag) (equal (tok-name tk) "html")) (switch :in-body))
                       (t (insert-element "body") (switch :in-body) (reproc))))
                (:in-body
                 (cond
                   ((eq ty :char) (insert-text (tok-data tk)))
                   ((eq ty :comment) (dom-append (current) (make-comment (tok-data tk))))
                   ((eq ty :doctype))                                   ; ignore
                   ((eq ty :start-tag)
                    (let ((name (tok-name tk)))
                      (cond
                        ((equal name "html"))                           ; ignore (attr merge: follow-up)
                        ((equal name "body"))
                        ((member name *rcdata-els* :test #'equal) (raw-element tk t))
                        ((member name *rawtext-els* :test #'equal) (raw-element tk nil))
                        ((member name *headings* :test #'equal)
                         (when (in-scope "p" '("html" "button")) (close-p))
                         (when (member (top-name) *headings* :test #'equal) (pop open))
                         (insert-element name (tok-attrs tk)))
                        ((equal name "li")
                         (when (in-scope "p" '("html" "button")) (close-p))
                         (when (in-scope "li") (pop-until "li"))
                         (insert-element "li" (tok-attrs tk)))
                        ((member name '("dd" "dt") :test #'equal)
                         (when (in-scope "p" '("html" "button")) (close-p))
                         (insert-element name (tok-attrs tk)))
                        ((equal name "hr")        ; closes an open <p>, then a void
                         (when (in-scope "p" '("html" "button")) (close-p))
                         (insert-void "hr" (tok-attrs tk)))
                        ((member name *void* :test #'equal) (insert-void name (tok-attrs tk)))
                        ((member name *block-closes-p* :test #'equal)
                         (when (in-scope "p" '("html" "button")) (close-p))
                         (insert-element name (tok-attrs tk)))
                        (t (insert-element name (tok-attrs tk))))))
                   ((eq ty :end-tag)
                    (let ((name (tok-name tk)))
                      (cond
                        ((equal name "body") (switch :after-body))
                        ((equal name "html") (switch :after-body) (reproc))
                        ((equal name "p")
                         (if (in-scope "p" '("html" "button")) (pop-until "p")
                             (progn (close-p))))           ; missing p: insert+close (simplified)
                        ((member name *headings* :test #'equal)
                         (when (loop for el in open thereis (member (dnode-name el) *headings* :test #'equal))
                           (gen-implied) (loop while (and open (not (member (top-name) *headings* :test #'equal))) do (pop open))
                           (when open (pop open))))
                        (t (when (in-scope name) (pop-until name))))))
                   ((eq ty :eof) (return))))
                (:after-body
                 (cond ((eq ty :comment) (dom-append (car (last open)) (make-comment (tok-data tk))))
                       ((and (eq ty :end-tag) (equal (tok-name tk) "html")) (switch :after-after-body))
                       ((eq ty :eof) (return))
                       (t (switch :in-body) (reproc))))
                (:after-after-body
                 (cond ((eq ty :comment) (dom-append doc (make-comment (tok-data tk))))
                       ((eq ty :eof) (return))
                       (t (switch :in-body) (reproc))))))
            (unless reconsume (incf i))))))
    doc)))
