;;;; demo/browse.lisp — a tiny text-mode "browser" driving the whole weft stack.
;;;;
;;;;   URL parse -> fetch (transport) -> Content-Encoding decode (pure-CL
;;;;   br/zstd/gzip) -> charset decode (36 decoders) -> HTML tokenize -> DOM
;;;;   tree construction -> DOM queries + a plain-text render.
;;;;
;;;; Usage: sbcl --script demo/browse.lisp <url>
(require :asdf)
(handler-bind ((warning #'muffle-warning)) (asdf:load-system "weft/fetch"))

(defpackage #:weft.demo
  (:use #:cl)
  (:local-nicknames (#:f #:weft.fetch) (#:h #:weft.html) (#:d #:weft.dom)))
(in-package #:weft.demo)

(defun bar (title) (format t "~&~%~a~%~a~%" title (make-string 64 :initial-element #\=)))

;;; ---- a minimal plain-text renderer (text browser style) ----------------
(defparameter *skip* '("script" "style" "head" "title" "noscript"))
(defparameter *block* '("p" "div" "h1" "h2" "h3" "h4" "h5" "h6" "li" "br" "tr"
                        "section" "article" "header" "footer" "ul" "ol" "table"))

(defun render-text (node out)
  (case (h:dnode-kind node)
    (:text (let ((s (h:dnode-data node)))
             (loop for c across s do (write-char (if (member c '(#\Newline #\Tab #\Return)) #\Space c) out))))
    (:element
     (unless (member (h:dnode-name node) *skip* :test #'string=)
       (when (member (h:dnode-name node) *block* :test #'string=) (write-char #\Newline out))
       (loop for c across (h:dnode-children node) do (render-text c out))
       (when (member (h:dnode-name node) *block* :test #'string=) (write-char #\Newline out))))
    (t (loop for c across (h:dnode-children node) do (render-text c out)))))

(defun squeeze (s)
  "Collapse runs of blank lines / spaces for readable terminal output."
  (with-output-to-string (o)
    (let ((blank 0))
      (dolist (line (uiop:split-string s :separator '(#\Newline)))
        (let ((trimmed (string-trim '(#\Space) line)))
          ;; collapse internal whitespace
          (setf trimmed (with-output-to-string (l)
                          (let ((sp nil))
                            (loop for c across trimmed do
                              (if (char= c #\Space) (setf sp t)
                                  (progn (when sp (write-char #\Space l)) (setf sp nil) (write-char c l)))))))
          (cond ((string= trimmed "") (when (< blank 1) (write-line "" o)) (incf blank))
                (t (setf blank 0) (write-line trimmed o))))))))

;;; ---- DOM helpers over our query API ------------------------------------
(defun el-text (el) (string-trim '(#\Space #\Newline #\Tab) (d:text-content el)))

(defun main (url)
  (bar (format nil "weft browsing  ~a" url))
  (multiple-value-bind (text charset resp) (f:fetch-text url)
    (format t "HTTP ~d   Content-Encoding: ~a   charset: ~a   ~:d bytes decoded~%"
            (f:response-status resp)
            (or (f:get-header (f:response-headers resp) "content-encoding") "identity")
            charset (length text))

    (bar "DOM tree (built by weft.html:parse-html)")
    (let* ((doc (h:parse-html text)))
      ;; print a shallow view of the tree
      (labels ((show (n depth)
                 (when (< depth 4)
                   (case (h:dnode-kind n)
                     (:element (format t "~a<~a>~@[ #~a~]~%"
                                       (make-string (* 2 depth) :initial-element #\Space)
                                       (h:dnode-name n)
                                       (d:get-attribute n "id"))
                               (loop for c across (h:dnode-children n) do (show c (1+ depth))))
                     (:document (loop for c across (h:dnode-children n) do (show c depth)))))))
        (show doc 0))

      (bar "DOM queries (weft.dom API)")
      (let* ((titles (d:get-elements-by-tag-name doc "title"))
             (links  (d:get-elements-by-tag-name doc "a"))
             (heads  (loop for tag in '("h1" "h2" "h3")
                           append (d:get-elements-by-tag-name doc tag)))
             (paras  (d:get-elements-by-tag-name doc "p")))
        (format t "title:     ~a~%" (if titles (el-text (first titles)) "(none)"))
        (format t "<a> links: ~d   <h1-3>: ~d   <p>: ~d~%" (length links) (length heads) (length paras))
        (when heads
          (format t "~%headings:~%")
          (dolist (hd heads) (format t "  • ~a~%" (el-text hd))))
        (when links
          (format t "~%links (first 10):~%")
          (dolist (a (subseq links 0 (min 10 (length links))))
            (format t "  → ~a~@[  [~a]~]~%" (el-text a) (d:get-attribute a "href")))))

      (bar "Plain-text render")
      (let ((body (first (d:get-elements-by-tag-name doc "body"))))
        (write-string (squeeze (with-output-to-string (o) (when body (render-text body o)))))))))

(let ((url (or (second sb-ext:*posix-argv*) "https://example.com/")))
  (handler-case (main url)
    (error (e) (format t "~&error: ~a~%" e))))
