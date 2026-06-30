;;;; src/css/parser.lisp — CSS rule/declaration parser (CSS Syntax §5, pragmatic).
;;;;
;;;; Parses a stylesheet into qualified rules.  Each rule is (selector-text .
;;;; declarations) where declarations is a list of (property value-text
;;;; . important-p).  @-rules: @media blocks are flattened (their inner rules are
;;;; included); other at-rules are skipped.  Enough to drive selector matching +
;;;; the cascade.
(in-package #:weft.css)

(defstruct css-rule selector decls)        ; selector: string; decls: list of (prop value . important)
(defstruct css-decl prop value important)

(defun css-escape-ident (s)
  "Re-escape an ident value when reconstructing source text.  The tokenizer
already decoded CSS escapes (e.g. `\\.parser` -> the ident \".parser\"), which
would otherwise be re-parsed as a `.parser` CLASS selector.  Backslash-escape any
character that is not a valid CSS name char so the escape survives the round-trip
and the selector parser sees it as a literal (type) identifier."
  (if (every (lambda (c) (or (alphanumericp c) (member c '(#\- #\_)) (> (char-code c) 127))) s)
      s
      (with-output-to-string (o)
        (loop for c across s do
          (if (or (alphanumericp c) (member c '(#\- #\_)) (> (char-code c) 127))
              (write-char c o)
              (progn (write-char #\\ o) (write-char c o)))))))

(defun toks-text (toks start end)
  "Reconstruct source-ish text from token range [start,end)."
  (with-output-to-string (o)
    (loop for k from start below end
          for tk = (aref toks k)
          do (case (ctok-type tk)
               (:ws (write-char #\Space o))
               (:ident (write-string (css-escape-ident (ctok-value tk)) o))
               (:function (format o "~a(" (ctok-value tk)))
               (:hash (format o "#~a" (ctok-value tk)))
               (:string (format o "\"~a\"" (ctok-value tk)))
               (:url (format o "url(~a)" (ctok-value tk)))
               (:number (format o "~a" (ctok-value tk)))
               (:percentage (format o "~a%" (ctok-value tk)))
               (:dimension (format o "~a~a" (ctok-value tk) (ctok-unit tk)))
               (:at-keyword (format o "@~a" (ctok-value tk)))
               (:delim (write-string (ctok-value tk) o))
               (:colon (write-char #\: o)) (:comma (write-char #\, o))
               (:semicolon (write-char #\; o))
               (:lparen (write-char #\( o)) (:rparen (write-char #\) o))
               (:lbracket (write-char #\[ o)) (:rbracket (write-char #\] o))
               (:lbrace (write-char #\{ o)) (:rbrace (write-char #\} o))))))

(defun parse-declarations (toks start end)
  "Parse a declaration block token range into a list of CSS-DECLs."
  (let ((decls '()) (i start))
    (loop while (< i end) do
      ;; skip whitespace/semicolons
      (loop while (and (< i end) (member (ctok-type (aref toks i)) '(:ws :semicolon))) do (incf i))
      (when (>= i end) (return))
      ;; property name (ident)
      (if (eq (ctok-type (aref toks i)) :ident)
          (let ((prop (string-downcase (ctok-value (aref toks i)))) (j (1+ i)))
            (loop while (and (< j end) (eq (ctok-type (aref toks j)) :ws)) do (incf j))
            (if (and (< j end) (eq (ctok-type (aref toks j)) :colon))
                (let ((vstart (1+ j)) (vend (1+ j)))
                  ;; value runs to the next top-level semicolon
                  (loop with depth = 0 for k from vstart below end
                        for ty = (ctok-type (aref toks k))
                        do (cond ((member ty '(:lparen :lbracket)) (incf depth))
                                 ((member ty '(:rparen :rbracket)) (decf depth))
                                 ((and (eq ty :semicolon) (<= depth 0)) (setf vend k) (return)))
                        finally (setf vend end))
                  (let* ((vtext (string-trim '(#\Space) (toks-text toks vstart vend)))
                         (imp nil))
                    ;; strip !important
                    (let ((p (search "!important" vtext :test #'char-equal)))
                      (when p (setf imp t vtext (string-trim '(#\Space) (subseq vtext 0 p)))))
                    (when (plusp (length vtext))
                      (push (make-css-decl :prop prop :value vtext :important imp) decls)))
                  (setf i (1+ vend)))
                ;; malformed; skip to next semicolon
                (progn (loop while (and (< i end) (not (eq (ctok-type (aref toks i)) :semicolon))) do (incf i)))))
          (loop while (and (< i end) (not (eq (ctok-type (aref toks i)) :semicolon))) do (incf i))))
    (nreverse decls)))

(defun match-brace (toks start)
  "START is at an :lbrace; return the index of the matching :rbrace (or end)."
  (let ((depth 0) (n (length toks)))
    (loop for k from start below n
          for ty = (ctok-type (aref toks k))
          do (cond ((eq ty :lbrace) (incf depth))
                   ((eq ty :rbrace) (decf depth) (when (zerop depth) (return-from match-brace k)))))
    n))

(defun parse-stylesheet (css)
  "Parse a CSS string into a list of CSS-RULEs."
  (let* ((toks (css-tokenize css)) (n (length toks)) (i 0) (rules '()))
    (labels ((collect-rules (start end)
               (let ((i start))
                 (loop while (< i end) do
                   (loop while (and (< i end) (eq (ctok-type (aref toks i)) :ws)) do (incf i))
                   (when (>= i end) (return))
                   (let ((tk (aref toks i)))
                     (cond
                       ((eq (ctok-type tk) :at-keyword)
                        ;; @media: recurse into its block; others: skip to ; or block end
                        (let ((kw (string-downcase (ctok-value tk))) (j (1+ i)))
                          (loop while (and (< j end) (not (member (ctok-type (aref toks j)) '(:lbrace :semicolon)))) do (incf j))
                          (cond
                            ((and (< j end) (eq (ctok-type (aref toks j)) :lbrace))
                             (let ((close (match-brace toks j)))
                               (when (string= kw "media") (collect-rules (1+ j) close))
                               (setf i (1+ close))))
                            (t (setf i (if (< j end) (1+ j) end))))))
                       (t
                        ;; qualified rule: prelude up to '{'
                        (let ((pstart i) (j i))
                          (loop while (and (< j end) (not (eq (ctok-type (aref toks j)) :lbrace))) do (incf j))
                          (if (< j end)
                              (let* ((close (match-brace toks j))
                                     (sel (string-trim '(#\Space) (toks-text toks pstart j)))
                                     (decls (parse-declarations toks (1+ j) close)))
                                (when (plusp (length sel))
                                  (push (make-css-rule :selector sel :decls decls) rules))
                                (setf i (1+ close)))
                              (setf i end))))))))))
      (collect-rules 0 n))
    (nreverse rules)))
