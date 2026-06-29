;;;; src/html/tree.lisp — HTML tree construction (WHATWG §13.2.6).
;;;;
;;;; Consumes tokenizer tokens into a DOM document via the insertion-mode state
;;;; machine.  Covers the document skeleton (initial/before-html/before-head/
;;;; in-head/after-head), the bulk of "in body" (text, blocks with implied <p>
;;;; closing, headings, list items, generic start/end with implied end tags),
;;;; raw-text/RCDATA reentrancy, and the TABLE modes (in-table / in-table-text /
;;;; in-table-body / in-row / in-cell) with foster parenting.  Still follow-ups:
;;;; the adoption agency algorithm (formatting misnesting), <select>, captions/
;;;; colgroups in full, foreign content, and fragment parsing.
(in-package #:weft.html)

(defparameter *void* '("area" "base" "br" "col" "embed" "hr" "img" "input"
                       "keygen" "link" "meta" "param" "source" "track" "wbr"))
(defparameter *head-void* '("base" "basefont" "bgsound" "link" "meta"))
(defparameter *block-closes-p*
  '("address" "article" "aside" "blockquote" "center" "details" "dialog" "dir"
    "div" "dl" "fieldset" "figcaption" "figure" "footer" "header" "hgroup" "main"
    "menu" "nav" "ol" "p" "section" "summary" "ul" "pre" "listing"
    "form" "fieldset"))
(defparameter *headings* '("h1" "h2" "h3" "h4" "h5" "h6"))
(defparameter *implied-end* '("dd" "dt" "li" "optgroup" "option" "p" "rb" "rp" "rt" "rtc"))
(defparameter *ws* '(#\Tab #\Newline #\Page #\Return #\Space))
;; start tags ignored in "in body" (they belong to table/frame/head contexts)
(defparameter *in-body-ignored-starts*
  '("caption" "col" "colgroup" "frame" "head" "tbody" "td" "tfoot" "th" "thead" "tr"))

(defun ws-char-tok-p (tk)
  (and (eq (tok-type tk) :char) (= 1 (length (tok-data tk)))
       (member (char (tok-data tk) 0) *ws*)))

(defparameter *rawtext-els* '("style" "xmp" "iframe" "noembed" "noframes" "script"))
(defparameter *rcdata-els* '("title" "textarea"))

(defun parse-html (input)
  "Parse an HTML string into a DOM document."
  (multiple-value-bind (tklist src) (tokenize input)
   (let* ((toks (coerce tklist 'vector)) (ntok (length toks)) (s src)
          (doc (make-document)) (open '()) (mode :initial) (orig-mode nil)
          (head nil) (i 0) (reconsume nil) (fostering nil) (pending nil))
    (labels
        ((current () (car open))
         (top-name () (and open (dnode-name (current))))
         (switch (m) (setf mode m))
         (reproc () (setf reconsume t))
         ;; ---- insertion location (with foster parenting) ----
         (foster-place ()
           (if (and fostering
                    (member (top-name) '("table" "tbody" "tfoot" "thead" "tr") :test #'equal))
               (let ((last-table (find "table" open :key #'dnode-name :test #'equal)))
                 (if (and last-table (dnode-parent last-table))
                     (values (dnode-parent last-table) last-table)
                     (let ((pos (position last-table open)))
                       (values (nth (1+ pos) open) nil))))
               (values (current) nil)))
         (insert-node (node)
           (multiple-value-bind (parent before) (foster-place)
             (dom-insert-before parent node before)))
         (push-el (el) (push el open) el)
         (insert-element (name &optional attrs (ns :html))
           (let ((el (make-element name attrs ns))) (insert-node el) (push-el el) el))
         (insert-void (name &optional attrs)
           (insert-node (make-element name attrs)))
         (insert-text (data)
           (multiple-value-bind (parent before) (foster-place)
             (let ((prev (dom-prev-sibling parent before)))
               (if (and prev (eq (dnode-kind prev) :text))
                   (setf (dnode-data prev) (concatenate 'string (dnode-data prev) data))
                   (dom-insert-before parent (make-text data) before)))))
         ;; ---- raw text / RCDATA (tokenizer reentrancy) ----
         ;; Re-tokenize the source after the start tag in the element's content
         ;; model (RAWTEXT or RCDATA), take its leading characters as the element
         ;; text, then splice the post-end-tag tokens back onto the main stream.
         (raw-element (tk rcdata-p)
           (let* ((name (tok-name tk)) (el (make-element name (tok-attrs tk)))
                  (state (if rcdata-p :rcdata :rawtext))
                  (rest (subseq s (1+ (tok-cend tk))))
                  (rtoks (coerce (tokenize rest :state state :last-start-tag name) 'vector))
                  (endpos (position-if (lambda (x) (and (eq (tok-type x) :end-tag)
                                                        (equal (tok-name x) name)))
                                       rtoks))
                  (text (with-output-to-string (o)
                          (loop for k from 0 below (or endpos (length rtoks))
                                for x = (aref rtoks k)
                                when (eq (tok-type x) :char) do (write-string (tok-data x) o)))))
             (insert-node el)
             (when (plusp (length text)) (dom-append el (make-text text)))
             ;; rebase the token stream + source onto the remainder past the end tag
             (let ((tail (if endpos (subseq rtoks (1+ endpos))
                             (vector (make-tok :type :eof)))))
               (setf toks (concatenate 'vector (subseq toks 0 (1+ i)) tail)
                     ntok (length toks)
                     s rest))))
         ;; ---- scope queries ----
         (in-scope (name &optional (bounds '("html" "table" "td" "th" "caption" "button" "marquee" "object")))
           (loop for el in open do
             (cond ((equal (dnode-name el) name) (return t))
                   ((member (dnode-name el) bounds :test #'equal) (return nil)))))
         (in-table-scope (name)
           (in-scope name '("html" "table" "template")))
         (gen-implied (&optional except)
           (loop while (and open (member (top-name) *implied-end* :test #'equal)
                            (not (equal (top-name) except)))
                 do (pop open)))
         (close-p ()
           (when (in-scope "p" '("html" "table" "td" "th" "caption" "button" "marquee" "object"))
             (gen-implied "p")
             (loop while (and open (not (equal (top-name) "p"))) do (pop open))
             (when open (pop open))))
         (pop-until (name)
           (gen-implied name)
           (when (in-scope name)
             (loop while (and open (not (equal (top-name) name))) do (pop open))
             (when open (pop open))))
         ;; ---- table stack clearing ----
         (clear-to (names)
           (loop while (and open (not (member (top-name) names :test #'equal))) do (pop open)))
         (reset-mode ()
           (loop for el in open do
             (let ((n (dnode-name el)))
               (cond ((member n '("td" "th") :test #'equal) (return (switch :in-cell)))
                     ((equal n "tr") (return (switch :in-row)))
                     ((member n '("tbody" "tfoot" "thead") :test #'equal) (return (switch :in-table-body)))
                     ((equal n "table") (return (switch :in-table)))
                     ((equal n "head") (return (switch :in-body)))
                     ((equal n "body") (return (switch :in-body)))
                     ((equal n "html") (return (switch :in-body)))))))
         ;; ---- "in body" start/end, reusable from table fallthroughs ----
         (body-start (tk name)
           (cond
             ((equal name "html"))
             ((equal name "body"))
             ((member name *in-body-ignored-starts* :test #'equal))   ; parse error, ignore
             ((member name *rcdata-els* :test #'equal) (raw-element tk t))
             ((member name *rawtext-els* :test #'equal) (raw-element tk nil))
             ((equal name "table")
              (when (in-scope "p" '("html" "button")) (close-p))
              (insert-element "table" (tok-attrs tk)) (switch :in-table))
             ((member name *headings* :test #'equal)
              (when (in-scope "p" '("html" "button")) (close-p))
              (when (member (top-name) *headings* :test #'equal) (pop open))
              (insert-element name (tok-attrs tk)))
             ((equal name "li")
              (when (in-scope "p" '("html" "button")) (close-p))
              (when (in-scope "li") (pop-until "li"))
              (insert-element "li" (tok-attrs tk)))
             ((member name '("dd" "dt") :test #'equal)
              (when (in-scope "dd") (pop-until "dd"))
              (when (in-scope "dt") (pop-until "dt"))
              (when (in-scope "p" '("html" "button")) (close-p))
              (insert-element name (tok-attrs tk)))
             ((equal name "hr")
              (when (in-scope "p" '("html" "button")) (close-p))
              (insert-void "hr" (tok-attrs tk)))
             ((member name *void* :test #'equal) (insert-void name (tok-attrs tk)))
             ((member name *block-closes-p* :test #'equal)
              (when (in-scope "p" '("html" "button")) (close-p))
              (insert-element name (tok-attrs tk)))
             (t (insert-element name (tok-attrs tk)))))
         (body-end (name)
           (cond
             ((equal name "body") (switch :after-body))
             ((equal name "html") (switch :after-body) (reproc))
             ((equal name "p")
              (if (in-scope "p" '("html" "button")) (pop-until "p")
                  (progn (insert-element "p") (pop open))))
             ((member name *headings* :test #'equal)
              (when (loop for el in open thereis (member (dnode-name el) *headings* :test #'equal))
                (gen-implied)
                (loop while (and open (not (member (top-name) *headings* :test #'equal))) do (pop open))
                (when open (pop open))))
             (t (when (in-scope name) (pop-until name)))))
         ;; ---- table cell helpers ----
         (close-cell ()
           (gen-implied)
           (loop while (and open (not (member (top-name) '("td" "th") :test #'equal))) do (pop open))
           (when open (pop open))
           (switch :in-row)))
      (loop
        (when (>= i ntok) (return))
        (let* ((tk (aref toks i)) (ty (tok-type tk)))
          (setf reconsume nil)
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
               ((eq ty :doctype))
               ((eq ty :start-tag) (body-start tk (tok-name tk)))
               ((eq ty :end-tag) (body-end (tok-name tk)))
               ((eq ty :eof) (return))))
            ;; ---- TABLE ----
            (:in-table
             (cond
               ((and (eq ty :char))                      ; collect into in-table-text
                (setf pending '() orig-mode :in-table) (switch :in-table-text) (reproc))
               ((eq ty :comment) (dom-append (current) (make-comment (tok-data tk))))
               ((eq ty :start-tag)
                (let ((name (tok-name tk)))
                  (cond
                    ((member name '("tbody" "tfoot" "thead") :test #'equal)
                     (clear-to '("table" "html")) (insert-element name (tok-attrs tk)) (switch :in-table-body))
                    ((member name '("td" "th" "tr") :test #'equal)
                     (clear-to '("table" "html")) (insert-element "tbody") (switch :in-table-body) (reproc))
                    ((equal name "caption")
                     (clear-to '("table" "html")) (insert-element "caption" (tok-attrs tk)) (switch :in-caption))
                    ((equal name "colgroup")
                     (clear-to '("table" "html")) (insert-element "colgroup" (tok-attrs tk)) (switch :in-column-group))
                    ((equal name "col")
                     (clear-to '("table" "html")) (insert-element "colgroup") (switch :in-column-group) (reproc))
                    ((equal name "table")                ; nested: act as </table>, reprocess
                     (when (in-table-scope "table")
                       (loop while (and open (not (equal (top-name) "table"))) do (pop open))
                       (when open (pop open)) (reset-mode) (reproc)))
                    ((member name '("style" "script") :test #'equal) (raw-element tk nil))
                    (t (let ((fostering t)) (body-start tk name))))))
               ((eq ty :end-tag)
                (let ((name (tok-name tk)))
                  (cond
                    ((equal name "table")
                     (when (in-table-scope "table")
                       (loop while (and open (not (equal (top-name) "table"))) do (pop open))
                       (when open (pop open)) (reset-mode)))
                    ((member name '("body" "caption" "col" "colgroup" "html" "tbody"
                                    "td" "tfoot" "th" "thead" "tr") :test #'equal))   ; ignore
                    (t (let ((fostering t)) (body-end name))))))
               ((eq ty :eof) (return))))
            (:in-table-text
             (cond
               ((eq ty :char) (push (tok-data tk) pending))
               (t ;; flush: foster-parent if any non-whitespace, else plain-insert
                (let ((text (apply #'concatenate 'string (nreverse pending))))
                  (if (some (lambda (c) (not (member c *ws*))) text)
                      (let ((fostering t)) (insert-text text))
                      (insert-text text)))
                (setf pending nil) (switch :in-table) (reproc))))
            (:in-table-body
             (cond
               ((eq ty :start-tag)
                (let ((name (tok-name tk)))
                  (cond
                    ((equal name "tr")
                     (clear-to '("tbody" "tfoot" "thead" "html")) (insert-element "tr" (tok-attrs tk)) (switch :in-row))
                    ((member name '("td" "th") :test #'equal)
                     (clear-to '("tbody" "tfoot" "thead" "html")) (insert-element "tr") (switch :in-row) (reproc))
                    ((member name '("caption" "col" "colgroup" "tbody" "tfoot" "thead") :test #'equal)
                     (when (or (in-table-scope "tbody") (in-table-scope "thead") (in-table-scope "tfoot"))
                       (clear-to '("tbody" "tfoot" "thead" "html")) (pop open) (switch :in-table) (reproc)))
                    (t (switch :in-table) (reproc)))))
               ((eq ty :end-tag)
                (let ((name (tok-name tk)))
                  (cond
                    ((member name '("tbody" "tfoot" "thead") :test #'equal)
                     (when (in-table-scope name)
                       (clear-to '("tbody" "tfoot" "thead" "html")) (pop open) (switch :in-table)))
                    ((equal name "table")
                     (when (or (in-table-scope "tbody") (in-table-scope "thead") (in-table-scope "tfoot"))
                       (clear-to '("tbody" "tfoot" "thead" "html")) (pop open) (switch :in-table) (reproc)))
                    ((member name '("body" "caption" "col" "colgroup" "html" "td" "th" "tr") :test #'equal))
                    (t (switch :in-table) (reproc)))))
               ((eq ty :char) (switch :in-table) (reproc))
               ((eq ty :comment) (dom-append (current) (make-comment (tok-data tk))))
               ((eq ty :eof) (return))))
            (:in-row
             (cond
               ((eq ty :start-tag)
                (let ((name (tok-name tk)))
                  (cond
                    ((member name '("td" "th") :test #'equal)
                     (clear-to '("tr" "html")) (insert-element name (tok-attrs tk)) (switch :in-cell))
                    ((member name '("caption" "col" "colgroup" "tbody" "tfoot" "thead" "tr") :test #'equal)
                     (when (in-table-scope "tr")
                       (clear-to '("tr" "html")) (pop open) (switch :in-table-body) (reproc)))
                    (t (switch :in-table) (reproc)))))
               ((eq ty :end-tag)
                (let ((name (tok-name tk)))
                  (cond
                    ((equal name "tr")
                     (when (in-table-scope "tr")
                       (clear-to '("tr" "html")) (pop open) (switch :in-table-body)))
                    ((member name '("tbody" "tfoot" "thead") :test #'equal)
                     (when (in-table-scope name)
                       (when (in-table-scope "tr") (clear-to '("tr" "html")) (pop open))
                       (switch :in-table-body) (reproc)))
                    ((equal name "table")
                     (when (in-table-scope "tr")
                       (clear-to '("tr" "html")) (pop open) (switch :in-table-body) (reproc)))
                    ((member name '("body" "caption" "col" "colgroup" "html" "td" "th") :test #'equal))
                    (t (switch :in-table) (reproc)))))
               ((eq ty :char) (switch :in-table) (reproc))
               ((eq ty :comment) (dom-append (current) (make-comment (tok-data tk))))
               ((eq ty :eof) (return))))
            (:in-cell
             (cond
               ((eq ty :end-tag)
                (let ((name (tok-name tk)))
                  (cond
                    ((member name '("td" "th") :test #'equal)
                     (when (in-table-scope name)
                       (gen-implied)
                       (loop while (and open (not (equal (top-name) name))) do (pop open))
                       (when open (pop open)) (switch :in-row)))
                    ((member name '("table" "tbody" "tfoot" "thead" "tr") :test #'equal)
                     (when (in-table-scope name) (close-cell) (reproc)))
                    ((member name '("body" "caption" "col" "colgroup" "html") :test #'equal))
                    (t (body-end name)))))
               ((eq ty :start-tag)
                (let ((name (tok-name tk)))
                  (cond
                    ((member name '("caption" "col" "colgroup" "tbody" "td" "tfoot" "th" "thead" "tr") :test #'equal)
                     (when (or (in-table-scope "td") (in-table-scope "th")) (close-cell) (reproc)))
                    (t (body-start tk name)))))
               ((eq ty :char) (insert-text (tok-data tk)))
               ((eq ty :comment) (dom-append (current) (make-comment (tok-data tk))))
               ((eq ty :eof) (return))))
            ;; minimal caption / colgroup: delegate content, close on their end tags
            (:in-caption
             (cond
               ((and (eq ty :end-tag) (equal (tok-name tk) "caption"))
                (when (in-table-scope "caption")
                  (gen-implied)
                  (loop while (and open (not (equal (top-name) "caption"))) do (pop open))
                  (when open (pop open)) (switch :in-table)))
               ((and (eq ty :start-tag)
                     (member (tok-name tk) '("caption" "col" "colgroup" "tbody" "td" "tfoot" "th" "thead" "tr") :test #'equal))
                (when (in-table-scope "caption")
                  (gen-implied) (loop while (and open (not (equal (top-name) "caption"))) do (pop open))
                  (when open (pop open)) (switch :in-table) (reproc)))
               ((eq ty :eof) (return))
               (t (case ty
                    (:char (insert-text (tok-data tk)))
                    (:comment (dom-append (current) (make-comment (tok-data tk))))
                    (:start-tag (body-start tk (tok-name tk)))
                    (:end-tag (body-end (tok-name tk)))))))
            (:in-column-group
             (cond
               ((and (eq ty :start-tag) (equal (tok-name tk) "col")) (insert-void "col" (tok-attrs tk)))
               ((and (eq ty :end-tag) (equal (tok-name tk) "colgroup"))
                (when (equal (top-name) "colgroup") (pop open) (switch :in-table)))
               ((ws-char-tok-p tk) (insert-text (tok-data tk)))
               ((eq ty :comment) (dom-append (current) (make-comment (tok-data tk))))
               ((eq ty :eof) (return))
               (t (when (equal (top-name) "colgroup") (pop open)) (switch :in-table) (reproc))))
            (:after-body
             (cond ((eq ty :comment) (dom-append (car (last open)) (make-comment (tok-data tk))))
                   ((and (eq ty :end-tag) (equal (tok-name tk) "html")) (switch :after-after-body))
                   ((eq ty :eof) (return))
                   (t (switch :in-body) (reproc))))
            (:after-after-body
             (cond ((eq ty :comment) (dom-append doc (make-comment (tok-data tk))))
                   ((eq ty :eof) (return))
                   (t (switch :in-body) (reproc)))))
          (unless reconsume (incf i)))))
    doc)))
