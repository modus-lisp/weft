;;;; src/css/parser.lisp — CSS rule/declaration parser (CSS Syntax §5, pragmatic).
;;;;
;;;; Parses a stylesheet into qualified rules.  Each rule is (selector-text .
;;;; declarations) where declarations is a list of (property value-text
;;;; . important-p).  @-rules: @media blocks are flattened (their inner rules are
;;;; included); other at-rules are skipped.  Enough to drive selector matching +
;;;; the cascade.
(in-package #:weft.css)

(declaim (special *viewport-w*))   ; defined in style.lisp; used for @media evaluation

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

(defun %media-lead-number (s)
  "Read the leading numeric value of S (e.g. \"690px\" -> 690, \"2\" -> 2), or NIL."
  (let ((end 0) (n (length s)))
    (loop while (and (< end n) (or (digit-char-p (char s end)) (member (char s end) '(#\. #\-)))) do (incf end))
    (when (plusp end)
      (ignore-errors (let ((*read-default-float-format* 'double-float))
                       (let ((v (read-from-string (subseq s 0 end)))) (and (numberp v) v)))))))

(defun %media-feature-match (feat vw dpr)
  "Evaluate a single media feature FEAT (interior of parens, e.g.
\"min-width: 690px\") against viewport width VW and device-pixel-ratio DPR.
Unknown features do not match (conservative)."
  (let* ((colon (position #\: feat))
         (name (string-trim " " (string-downcase (if colon (subseq feat 0 colon) feat))))
         (vraw (and colon (string-trim " " (subseq feat (1+ colon)))))
         (val (and vraw (%media-lead-number vraw))))
    ;; drop any vendor prefix (-webkit-, -moz-, ...)
    (when (and (plusp (length name)) (char= (char name 0) #\-))
      (let ((p (position #\- name :start 1))) (when p (setf name (subseq name (1+ p))))))
    (cond
      ((null val) nil)
      ((string= name "min-width")  (>= vw val))
      ((string= name "max-width")  (<= vw val))
      ((string= name "width")      (= vw val))
      ((string= name "min-device-width") (>= vw val))
      ((string= name "max-device-width") (<= vw val))
      ((string= name "min-device-pixel-ratio") (>= dpr val))
      ((string= name "max-device-pixel-ratio") (<= dpr val))
      ((string= name "device-pixel-ratio") (= dpr val))
      ((string= name "min-resolution") (>= (* dpr 96) val))
      ((string= name "max-resolution") (<= (* dpr 96) val))
      (t nil))))

(defun %media-split (s ch)
  "Split S on character CH into a list of substrings."
  (let ((parts '()) (start 0))
    (loop for i from 0 below (length s)
          when (char= (char s i) ch) do (push (subseq s start i) parts) (setf start (1+ i)))
    (push (subseq s start) parts)
    (nreverse parts)))

(defun %media-query-match (q vw dpr)
  "Evaluate one comma-separated media query Q (e.g. \"only screen and
(min-width: 690px) and (max-width: 809px)\") against VW/DPR."
  (let* ((q (string-trim " " (string-downcase q)))
         (negate nil))
    ;; leading 'not' / 'only' keyword
    (cond ((and (>= (length q) 4) (string= (subseq q 0 4) "not ")) (setf negate t q (string-trim " " (subseq q 4))))
          ((and (>= (length q) 5) (string= (subseq q 0 5) "only ")) (setf q (string-trim " " (subseq q 5)))))
    ;; media type: the token before the first '(' (or the whole string)
    (let* ((lp (position #\( q))
           (head (string-trim " " (subseq q 0 (or lp (length q)))))
           ;; strip a trailing 'and' from the head
           (mtype (let ((parts (remove "" (%media-split head #\Space) :test #'string=)))
                    (or (first parts) "all")))
           (type-ok (member mtype '("all" "screen" "") :test #'string=))
           (result t))
      (unless type-ok (setf result nil))
      ;; evaluate each (feature) group, ANDed
      (when result
        (let ((i (or lp (length q))))
          (loop while (< i (length q)) do
            (let ((open (position #\( q :start i)))
              (unless open (return))
              (let ((close (position #\) q :start open)))
                (unless close (setf result nil) (return))
                (unless (%media-feature-match (subseq q (1+ open) close) vw dpr)
                  (setf result nil) (return))
                (setf i (1+ close)))))))
      (if negate (not result) result))))

(defun media-matches-p (prelude)
  "True when the @media PRELUDE (comma-separated media query list) applies at the
current viewport width (*viewport-w*, defaulting to 800) and device-pixel-ratio 1.
An empty prelude matches (a bare @media)."
  (let ((vw (float (or *viewport-w* 800)))
        (dpr 1.0)
        (text (string-trim '(#\Space #\Tab #\Newline) prelude)))
    (or (zerop (length text))
        (some (lambda (q) (%media-query-match q vw dpr)) (%media-split text #\,)))))

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
                               (when (and (string= kw "media")
                                          (media-matches-p (toks-text toks (1+ i) j)))
                                 (collect-rules (1+ j) close))
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
