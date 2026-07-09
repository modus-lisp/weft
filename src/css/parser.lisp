;;;; src/css/parser.lisp — CSS rule/declaration parser (CSS Syntax §5, pragmatic).
;;;;
;;;; Parses a stylesheet into qualified rules.  Each rule is (selector-text .
;;;; declarations) where declarations is a list of (property value-text
;;;; . important-p).  @-rules: @media blocks are flattened (their inner rules are
;;;; included); other at-rules are skipped.  Enough to drive selector matching +
;;;; the cascade.
(in-package #:weft.css)

(declaim (special *viewport-w* *viewport-h*))   ; defined in style.lisp; used for @media evaluation

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
          (let* ((raw (ctok-value (aref toks i)))
                 ;; custom properties (--*) are case-sensitive per CSS Variables;
                 ;; normal property names are ASCII case-insensitive.
                 (prop (if (and (>= (length raw) 2) (char= (char raw 0) #\-) (char= (char raw 1) #\-))
                           raw (string-downcase raw)))
                 (j (1+ i)))
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

(defun %media-feature-match (feat vw vh dpr)
  "Evaluate a single media feature FEAT (interior of parens, e.g.
\"min-width: 690px\") against viewport width VW, height VH and device-pixel-ratio
DPR.  Returns T/NIL.  A colour display is assumed: 8 bits per colour component,
0 monochrome bits.  A malformed feature (no value where one is required) returns
NIL — the caller treats that as a non-matching query."
  (let* ((colon (position #\: feat))
         (name (string-trim " " (string-downcase (if colon (subseq feat 0 colon) feat))))
         (vraw (and colon (string-trim " " (subseq feat (1+ colon)))))
         (val (and vraw (%media-lead-number vraw))))
    ;; drop any vendor prefix (-webkit-, -moz-, ...)
    (when (and (plusp (length name)) (char= (char name 0) #\-))
      (let ((p (position #\- name :start 1))) (when p (setf name (subseq name (1+ p))))))
    (cond
      ;; boolean-context colour/monochrome features (no value): any colour
      ;; display matches `color` and does not match `monochrome`.
      ((and (null val) (string= name "color")) t)
      ((and (null val) (string= name "monochrome")) nil)
      ((null val) nil)
      ((string= name "min-width")  (>= vw val))
      ((string= name "max-width")  (<= vw val))
      ((string= name "width")      (= vw val))
      ((string= name "min-height") (>= vh val))
      ((string= name "max-height") (<= vh val))
      ((string= name "height")     (= vh val))
      ((string= name "min-device-width") (>= vw val))
      ((string= name "max-device-width") (<= vw val))
      ((string= name "min-device-pixel-ratio") (>= dpr val))
      ((string= name "max-device-pixel-ratio") (<= dpr val))
      ((string= name "device-pixel-ratio") (= dpr val))
      ((string= name "min-resolution") (>= (* dpr 96) val))
      ((string= name "max-resolution") (<= (* dpr 96) val))
      ((string= name "min-color") (>= 8 val))
      ((string= name "max-color") (<= 8 val))
      ((string= name "color") (= 8 val))
      ((string= name "min-monochrome") (>= 0 val))
      ((string= name "max-monochrome") (<= 0 val))
      ((string= name "monochrome") (= 0 val))
      (t nil))))

(defun %media-split (s ch)
  "Split S on character CH into a list of substrings."
  (let ((parts '()) (start 0))
    (loop for i from 0 below (length s)
          when (char= (char s i) ch) do (push (subseq s start i) parts) (setf start (1+ i)))
    (push (subseq s start) parts)
    (nreverse parts)))

(defun %media-tokens (q)
  "Tokenize a media query on whitespace, keeping a (…) group as a single token."
  (let ((toks '()) (i 0) (n (length q)))
    (loop while (< i n) do
      (loop while (and (< i n) (member (char q i) '(#\Space #\Tab #\Newline))) do (incf i))
      (when (>= i n) (return))
      (if (char= (char q i) #\()
          (let ((close (position #\) q :start i)))
            (push (subseq q i (if close (1+ close) n)) toks)
            (setf i (if close (1+ close) n)))
          (let ((start i))
            (loop while (and (< i n) (not (member (char q i) '(#\Space #\Tab #\Newline #\()))) do (incf i))
            (push (subseq q start i) toks))))
    (nreverse toks)))

(defun %paren-group-p (tok) (and (plusp (length tok)) (char= (char tok 0) #\()))
(defun %paren-interior (tok) (string-trim " " (subseq tok 1 (or (position #\) tok) (length tok)))))

(defun %media-query-match (q vw vh dpr)
  "Evaluate one media query Q (a single comma-separated component) against the
viewport. Grammar: [not|only]? (media-type | (feature)) (and (feature))* .  A
malformed query (a bare token where a feature group is required) evaluates to
`not all` — i.e. it does not match."
  (let ((toks (%media-tokens (string-trim " " (string-downcase q)))) (negate nil))
    (when (null toks) (return-from %media-query-match t))     ; empty => all
    (when (member (first toks) '("not" "only") :test #'string=)
      (when (string= (first toks) "not") (setf negate t))
      (pop toks))
    (let ((result t) (first (first toks)))
      (cond
        ((null first))                                        ; bare not/only => all
        ((%paren-group-p first)                               ; implicit `all and (feature)`
         (pop toks)
         (unless (%media-feature-match (%paren-interior first) vw vh dpr) (setf result nil)))
        (t                                                    ; a media type
         (pop toks)
         ;; This is a screen user agent: only `all`, `screen`, and a bare/empty
         ;; type match.  Other valid types (print, speech, tv, handheld, …) name
         ;; a different device and must NOT match — so `@media print` rules are
         ;; ignored and `@media not print` applies here (negate handles the flip).
         (unless (member first '("all" "screen" "") :test #'string=)
           (setf result nil))))
      ;; the tail must be a sequence of `and (feature)` pairs
      (loop while (and (not (eq result :bad)) toks) do
        (let ((connector (pop toks)) (grp (pop toks)))
          (cond ((not (string= connector "and")) (setf result :bad))
                ((or (null grp) (not (%paren-group-p grp))) (setf result :bad))
                ((not (%media-feature-match (%paren-interior grp) vw vh dpr)) (setf result nil)))))
      (cond ((eq result :bad) nil)                            ; malformed => not all
            (negate (not result))
            (t result)))))

(defun media-matches-p (prelude)
  "True when the @media PRELUDE (comma-separated media query list) applies at the
current viewport (*viewport-w*/*viewport-h*) and device-pixel-ratio 1.  An empty
prelude matches (a bare @media)."
  (let ((vw (float (or *viewport-w* 800)))
        (vh (float (or *viewport-h* 600)))
        (dpr 1.0)
        (text (string-trim '(#\Space #\Tab #\Newline) prelude)))
    (or (zerop (length text))
        (some (lambda (q) (%media-query-match q vw vh dpr)) (%media-split text #\,)))))

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
                               (cond
                                 ;; @media: recurse only when the query matches this viewport.
                                 ((string= kw "media")
                                  (when (media-matches-p (toks-text toks (1+ i) j))
                                    (collect-rules (1+ j) close)))
                                 ;; @layer / @supports wrap ordinary rules — flatten them in.
                                 ;; Tailwind v4 puts EVERY utility inside `@layer utilities`
                                 ;; (with the responsive variants in a nested @media), so
                                 ;; skipping @layer dropped the whole framework.  @supports
                                 ;; conditions are assumed met (progressive enhancement).
                                 ((member kw '("layer" "supports") :test #'string=)
                                  (collect-rules (1+ j) close))
                                 ;; @font-face / @keyframes / @page hold descriptors, not
                                 ;; qualified rules — skip their blocks.
                                 (t nil))
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
