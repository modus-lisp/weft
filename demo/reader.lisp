;;;; demo/reader.lisp — "reader view" for weft (Readability-style extraction).
;;;;
;;;; Fetches a page through the full weft stack, scores block elements by text
;;;; density / class-id hints (à la Mozilla Readability), picks the main content
;;;; container, strips boilerplate, and prints a clean article: title, byline,
;;;; and body text with headings, lists, and inline [links](url).
;;;;
;;;; Usage: sbcl --script demo/reader.lisp <url> [--md]
(require :asdf)
(handler-bind ((warning #'muffle-warning)) (asdf:load-system "weft/fetch"))

(defpackage #:weft.reader
  (:use #:cl)
  (:local-nicknames (#:f #:weft.fetch) (#:h #:weft.html) (#:d #:weft.dom)))
(in-package #:weft.reader)

;;; ---- tree helpers ------------------------------------------------------
(defun el-p (n) (eq (h:dnode-kind n) :element))
(defun name= (n s) (and (el-p n) (string= (h:dnode-name n) s)))
(defun attr (n a) (or (and (el-p n) (d:get-attribute n a)) ""))
(defun classid (n) (format nil "~a ~a" (attr n "class") (attr n "id")))

(defun walk (node fn) (funcall fn node)
  (when (el-p node) (loop for c across (h:dnode-children node) do (walk c fn))))

(defun own-text (n)
  "All descendant text of N, whitespace-collapsed."
  (let ((raw (d:text-content n)))
    (with-output-to-string (o)
      (let ((sp nil) (started nil))
        (loop for c across raw do
          (if (member c '(#\Space #\Tab #\Newline #\Return)) (setf sp t)
              (progn (when (and sp started) (write-char #\Space o)) (setf sp nil started t) (write-char c o))))))))

(defun link-density (n)
  (let ((total (length (own-text n))) (linked 0))
    (when (zerop total) (return-from link-density 0))
    (walk n (lambda (x) (when (name= x "a") (incf linked (length (own-text x))))))
    (/ linked (float total))))

;;; ---- scoring (Readability heuristic) -----------------------------------
(defparameter *positive* '("article" "body" "content" "entry" "hentry" "main"
                           "page" "post" "text" "blog" "story" "column" "prose"))
(defparameter *negative* '("combx" "comment" "contact" "foot" "footer" "footnote"
                           "masthead" "media" "meta" "outbrain" "promo" "related"
                           "scroll" "shoutbox" "sidebar" "sponsor" "shopping" "tags"
                           "widget" "nav" "menu" "banner" "ad-" "social" "share"))
(defparameter *strip* '("script" "style" "noscript" "nav" "aside" "form" "footer"
                        "header" "button" "iframe" "svg" "figure" "figcaption"))
(defparameter *scorable* '("p" "td" "pre" "article" "section" "div" "blockquote"))

(defun weight (n)
  (let ((s (string-downcase (classid n))) (w 0))
    (dolist (p *positive*) (when (search p s) (incf w 25) (return)))
    (dolist (p *negative*) (when (search p s) (decf w 25) (return)))
    w))

(defun direct-text-len (n)
  "Length of N's immediate child text nodes (not nested elements)."
  (let ((len 0))
    (loop for c across (h:dnode-children n)
          when (eq (h:dnode-kind c) :text)
            do (incf len (length (string-trim '(#\Space #\Newline #\Tab) (h:dnode-data c)))))
    len))

(defun score-tree (root)
  "Return a hash-table node->score for candidate containers.  Combines the
classic <p>-parent scoring with a direct-text fallback so pages that lack
semantic <p> markup (text in <font>/<td> with <br>s) still get a candidate."
  (let ((scores (make-hash-table :test 'eq)))
    (flet ((bump (el amt)
             (when (and el (el-p el))
               (unless (gethash el scores) (setf (gethash el scores) (weight el)))
               (incf (gethash el scores) amt))))
      (walk root
            (lambda (n)
              (when (el-p n)
                ;; (a) <p>/<pre>/<td> contribute to parent + grandparent
                (when (member (h:dnode-name n) '("p" "pre" "td") :test #'string=)
                  (let* ((txt (own-text n)) (len (length txt)))
                    (when (>= len 25)
                      (let ((base (+ 1 (count #\, txt) (min (floor len 100) 3))))
                        (bump (h:dnode-parent n) base)
                        (bump (and (h:dnode-parent n) (h:dnode-parent (h:dnode-parent n))) (/ base 2))))))
                ;; (b) any container with substantial DIRECT text scores itself
                (let ((dt (direct-text-len n)))
                  (when (>= dt 60) (bump n (+ 1 (min (floor dt 100) 8)))))))))
    ;; scale by (1 - link density)
    (maphash (lambda (n s) (setf (gethash n scores) (* s (- 1 (link-density n))))) scores)
    scores))

(defun best-candidate (root)
  (let ((scores (score-tree root)) (best nil) (best-s -1))
    (maphash (lambda (n s) (when (> s best-s) (setf best n best-s s))) scores)
    (or best root)))

;;; ---- rendering the chosen article --------------------------------------
(defparameter *md* nil)

(defun collapse-ws (s)
  "Collapse whitespace runs to single spaces, keeping boundary spaces."
  (with-output-to-string (o)
    (let ((sp nil))
      (loop for c across s do
        (if (member c '(#\Space #\Tab #\Newline #\Return)) (setf sp t)
            (progn (when sp (write-char #\Space o)) (setf sp nil) (write-char c o))))
      (when sp (write-char #\Space o)))))   ; keep a trailing space (boundary)

(defun inline-text (n out)
  "Render inline content of N, turning <a> into [text](href) when --md."
  (cond
    ((eq (h:dnode-kind n) :text)
     (write-string (collapse-ws (h:dnode-data n)) out))
    ((name= n "a")
     (let ((href (attr n "href")) (txt (own-text n)))
       (if (and *md* (plusp (length href)) (plusp (length txt)) (not (char= (char href 0) #\#)))
           (format out "[~a](~a)" txt href)
           (write-string txt out))))
    ((el-p n) (loop for c across (h:dnode-children n) do (inline-text c out)))))

(defun emit-block (n out)
  (let ((txt (string-trim '(#\Space) (collapse-ws (with-output-to-string (s) (inline-text n s))))))
    (when (plusp (length txt))
      (cond
        ((member (h:dnode-name n) '("h1" "h2" "h3" "h4" "h5" "h6") :test #'string=)
         (let ((lvl (digit-char-p (char (h:dnode-name n) 1))))
           (format out "~%~a ~a~%~%" (if *md* (make-string lvl :initial-element #\#) "##") txt)))
        ((name= n "li") (format out "~a ~a~%" (if *md* "-" "•") txt))
        ((name= n "blockquote") (format out "~%> ~a~%~%" txt))
        ((name= n "pre") (format out "~%~a~%~%" txt))
        (t (format out "~%~a~%~%" txt))))))

(defparameter *blocks* '("p" "h1" "h2" "h3" "h4" "h5" "h6" "li" "blockquote" "pre"))

(defun render-article (node out)
  "Render NODE as a flat text stream: emit semantic blocks specially, treat
<br><br> as paragraph breaks, and emit loose inline text (for pages without
<p> markup).  Skips boilerplate."
  (let ((brs 0))
    (labels ((para () (terpri out) (terpri out))
             (rec (n)
               (cond
                 ((eq (h:dnode-kind n) :text)
                  (let ((s (collapse-ws (h:dnode-data n))))
                    (when (plusp (length (string-trim '(#\Space) s)))
                      (when (>= brs 2) (para)) (setf brs 0)
                      (write-string s out))))
                 ((not (el-p n)) (loop for c across (h:dnode-children n) do (rec c)))
                 ((member (h:dnode-name n) *strip* :test #'string=))      ; skip boilerplate
                 ((string= (h:dnode-name n) "br") (incf brs))
                 ((member (h:dnode-name n) *blocks* :test #'string=)
                  (setf brs 0) (emit-block n out))
                 ((name= n "a")                                          ; inline link
                  (when (>= brs 2) (para)) (setf brs 0) (inline-text n out))
                 (t (loop for c across (h:dnode-children n) do (rec c))))))
      (rec node))))

(defun squeeze (s)
  (with-output-to-string (o)
    (let ((blank 0))
      (dolist (line (uiop:split-string s :separator '(#\Newline)))
        (if (string= (string-trim '(#\Space) line) "")
            (progn (when (< blank 1) (terpri o)) (incf blank))
            (progn (setf blank 0) (write-line (string-right-trim '(#\Space) line) o)))))))

(defun page-title (doc)
  (let ((h1 (first (d:get-elements-by-tag-name doc "h1")))
        (tt (first (d:get-elements-by-tag-name doc "title"))))
    (string-trim '(#\Space #\Newline) (own-text (or h1 tt doc)))))

(defun byline (doc)
  (let (found)
    (walk doc (lambda (n)
                (when (and (not found) (el-p n)
                           (let ((s (string-downcase (classid n))))
                             (or (search "byline" s) (search "author" s) (search "rel=\"author\"" s)))
                           (< (length (own-text n)) 100) (plusp (length (own-text n))))
                  (setf found (own-text n)))))
    found))

(defun main (url)
  (multiple-value-bind (text charset resp) (f:fetch-text url)
    (declare (ignore charset))
    (let* ((doc (h:parse-html text))
           (body (or (first (d:get-elements-by-tag-name doc "body")) doc))
           (art (best-candidate body))
           (rendered (squeeze (with-output-to-string (o) (render-article art o)))))
      (format t "~&# ~a~%" (page-title doc))
      (let ((by (byline doc))) (when by (format t "_by ~a_~%" (string-trim '(#\Space) by))))
      (format t "~%~a~%" url)
      (format t "~a~%" (make-string 72 :initial-element #\─))
      (write-string rendered)
      (format t "~%~a~%[weft reader — HTTP ~d, ~a, ~:d B, ~d words]~%"
              (make-string 72 :initial-element #\─)
              (f:response-status resp)
              (or (f:get-header (f:response-headers resp) "content-encoding") "identity")
              (length text)
              (1+ (count #\Space rendered))))))

(let* ((args (rest sb-ext:*posix-argv*))
       (url (or (find-if (lambda (a) (not (string= a "--md"))) args) "https://example.com/")))
  (setf *md* (and (member "--md" args :test #'string=) t))
  (handler-case (main url) (error (e) (format t "~&error: ~a~%" e))))
