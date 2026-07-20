;;;; src/css/parser.lisp — CSS rule/declaration parser (CSS Syntax §5, pragmatic).
;;;;
;;;; Parses a stylesheet into qualified rules.  Each rule is (selector-text .
;;;; declarations) where declarations is a list of (property value-text
;;;; . important-p).  @-rules: @media blocks are flattened (their inner rules are
;;;; included); other at-rules are skipped.  Enough to drive selector matching +
;;;; the cascade.
(in-package #:weft.css)

(declaim (special *viewport-w* *viewport-h*))   ; defined in style.lisp; used for @media evaluation

(defstruct css-rule selector decls
  ;; CSS Containment 3 §container queries: NIL for an unconditional rule, else a
  ;; list of enclosing @container query specs (each (NAME . COND-AST), innermost
  ;; last), ALL of which must evaluate true for the rule's declarations to apply.
  container)
(defstruct css-decl prop value important)

(defvar *container-cq* nil
  "Dynamic list of enclosing @container query specs stamped onto every CSS-RULE
emitted while parsing inside an @container block (see CSS-RULE-CONTAINER).")

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

(defun css-escape-string (s)
  "Escape a string's contents for re-serialisation inside \"...\" delimiters:
backslash and the double-quote delimiter are backslash-escaped so a value that
round-trips through TOKS-TEXT (e.g. `quotes: '\"' ...`) is re-readable rather than
collapsing `\"` into an ambiguous bare quote."
  (if (find-if (lambda (c) (or (char= c #\\) (char= c #\"))) s)
      (with-output-to-string (o)
        (loop for c across s do
          (when (or (char= c #\\) (char= c #\")) (write-char #\\ o))
          (write-char c o)))
      s))

(defun toks-text (toks start end)
  "Reconstruct source-ish text from token range [start,end).  A comment in the
source leaves no token, so two source tokens it separated become adjacent in the
stream; CSS requires such tokens stay separated (a comment is a token boundary,
not glue), so a space is inserted between two adjacent word-like tokens — e.g.
`1/**/0px` reconstructs to `1 0px` (invalid), not `10px` (valid)."
  (with-output-to-string (o)
    (let ((prev nil))                   ; the previous emitted token
     (loop for k from start below end
          for tk = (aref toks k)
          for ty = (ctok-type tk)
          for pty = (and prev (ctok-type prev))
          do (when (or ;; word-like glued to word-like: only pairs that CANNOT be
                       ;; adjacent in source without a separator (so a comment sat
                       ;; between them and must be preserved as a boundary), e.g.
                       ;; `1/**/0px` -> `1 0px`, `div/**/p` -> `div p`.  A #hash or
                       ;; function token is self-delimiting (`.class#id` is one
                       ;; compound), so it never takes a leading space.
                       (and (member pty '(:ident :number :dimension :percentage :at-keyword))
                            (member ty '(:ident :number :dimension :percentage)))
                       ;; a sign / dot delim glued to a number would re-tokenize as a
                       ;; signed number (e.g. `-/**/10px` -> `- 10px`, invalid)
                       (and (eq pty :delim) (member (ctok-value prev) '("+" "-" ".") :test #'string=)
                            (member ty '(:number :dimension :percentage))))
               (write-char #\Space o))
             (setf prev tk)
             (case ty
               (:ws (write-char #\Space o))
               (:ident (write-string (css-escape-ident (ctok-value tk)) o))
               (:function (format o "~a(" (ctok-value tk)))
               (:hash (format o "#~a" (css-escape-ident (ctok-value tk))))
               (:string (format o "\"~a\"" (css-escape-string (ctok-value tk))))
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
               (:lbrace (write-char #\{ o)) (:rbrace (write-char #\} o)))))))

(defun parse-declarations (toks start end)
  "Parse a declaration block token range into a list of CSS-DECLs."
  (let ((decls '()) (i start))
   (labels ((skip-bad (k)
              ;; Consume the remnants of a malformed declaration (CSS Syntax §5.4.4):
              ;; advance past the next top-level semicolon, treating any {}/()/[]
              ;; block as a unit so a semicolon or nested declaration INSIDE the
              ;; block (e.g. `color{;color:red;}`) is discarded with it, not parsed.
              (let ((depth 0))
                (loop while (< k end) do
                  (let ((ty (ctok-type (aref toks k))))
                    (cond ((member ty '(:lparen :lbracket :lbrace)) (incf depth) (incf k))
                          ((member ty '(:rparen :rbracket :rbrace))
                           (when (plusp depth) (decf depth)) (incf k))
                          ((and (eq ty :semicolon) (zerop depth)) (incf k) (return))
                          (t (incf k))))))
              k))
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
                        do (cond ((member ty '(:lparen :lbracket :lbrace)) (incf depth))
                                 ((member ty '(:rparen :rbracket :rbrace)) (decf depth))
                                 ((and (eq ty :semicolon) (<= depth 0)) (setf vend k) (return)))
                        finally (setf vend end))
                  (let* ((vtext (string-trim '(#\Space) (toks-text toks vstart vend)))
                         (imp nil))
                    ;; strip a trailing !important — CSS Syntax allows whitespace
                    ;; (and comments) between the `!` and `important`, so Acid3's
                    ;; `border: 1px solid ! important` counts as important too.
                    (let ((p (position #\! vtext :from-end t)))
                      (when p
                        (let ((rest (string-left-trim '(#\Space #\Tab #\Newline #\Return)
                                                      (subseq vtext (1+ p)))))
                          (when (and (>= (length rest) 9)
                                     (string-equal (subseq rest 0 9) "important")
                                     (every (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return)))
                                            (subseq rest 9)))
                            (setf imp t
                                  vtext (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                     (subseq vtext 0 p)))))))
                    (when (plusp (length vtext))
                      (push (make-css-decl :prop prop :value vtext :important imp) decls)))
                  (setf i (1+ vend)))
                ;; property not followed by a colon: malformed declaration.
                (setf i (skip-bad i))))
          ;; token where a property name was expected: malformed too.
          (setf i (skip-bad i))))
    (nreverse decls))))

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
      ;; the raster is a static snapshot — treat as unscripted (`scripting: none`),
      ;; matching a JS-off render.  So JS-only chrome (`@media(scripting:none){…}`
      ;; hides a copy button that needs JS) is handled as the no-script author meant.
      ((string= name "scripting") (string= (or vraw "") "none"))
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

(defun %sel-top-comma-p (s)
  "True when S has a comma at the top level (outside ()/[])."
  (let ((depth 0))
    (loop for c across s do
      (case c ((#\( #\[) (incf depth)) ((#\) #\]) (when (plusp depth) (decf depth)))
        (#\, (when (zerop depth) (return-from %sel-top-comma-p t)))))
    nil))

(defun %sel-split-top-commas (s)
  "Split S on top-level commas (outside ()/[]) into a list of parts."
  (let ((out '()) (depth 0) (start 0))
    (dotimes (i (length s))
      (case (char s i)
        ((#\( #\[) (incf depth)) ((#\) #\]) (when (plusp depth) (decf depth)))
        (#\, (when (zerop depth) (push (subseq s start i) out) (setf start (1+ i))))))
    (push (subseq s start (length s)) out)
    (nreverse out)))

(defun %replace-amp (s parent)
  "Replace every top-level nesting selector `&` in S with PARENT."
  (with-output-to-string (o)
    (loop for c across s do (if (char= c #\&) (write-string parent o) (write-char c o)))))

(defun resolve-nesting-selector (nested parent)
  "Combine a NESTED rule's selector with its PARENT selector, per CSS Nesting L1:
each comma-part containing `&` has it replaced by the parent; a part with no `&`
is made a descendant of the parent (`&` implicitly prepended).  A parent that is a
selector list is wrapped in :is() so specificity/grouping stay correct."
  (let* ((parent (css-trim parent))
         (pwrap (if (%sel-top-comma-p parent)
                    (concatenate 'string ":is(" parent ")")
                    parent)))
    (format nil "~{~a~^, ~}"
            (mapcar (lambda (part)
                      (let ((p (css-trim part)))
                        (if (find #\& p)
                            (%replace-amp p pwrap)
                            (concatenate 'string pwrap " " p))))
                    (%sel-split-top-commas nested)))))

;;; ---- @container query preludes (CSS Containment 3 §container queries) ----
(defun %cq-split-tokens (s)
  "Tokenize a container condition on whitespace, keeping a balanced (...) group
(nesting-aware) as a single token — e.g. `(a) and (b)` -> (\"(a)\" \"and\" \"(b)\")."
  (let ((toks '()) (i 0) (n (length s)))
    (loop while (< i n) do
      (loop while (and (< i n) (member (char s i) '(#\Space #\Tab #\Newline #\Return))) do (incf i))
      (when (>= i n) (return))
      (if (char= (char s i) #\()
          (let ((depth 0) (start i))
            (loop while (< i n) do
              (case (char s i) (#\( (incf depth)) (#\) (decf depth)))
              (incf i)
              (when (<= depth 0) (return)))
            (push (subseq s start i) toks))
          (let ((start i))
            (loop while (and (< i n) (not (member (char s i) '(#\Space #\Tab #\Newline #\Return #\())))
                  do (incf i))
            (push (subseq s start i) toks))))
    (nreverse toks)))

(defparameter *cq-size-features* '("width" "height" "inline-size" "block-size")
  "Size features weft evaluates in a container query; others -> :unknown.")

(defun %cq-op-sym (s)
  (cond ((string= s "<=") :<=) ((string= s ">=") :>=)
        ((string= s "<") :<) ((string= s ">") :>) ((string= s "=") :=) (t nil)))

(defun %cq-op-flip (op)
  "Reverse a comparison operator when its operands are swapped."
  (ecase op (:< :>) (:<= :>=) (:> :<) (:>= :<=) (:= :=)))

(defun %cq-value-token-p (s)
  "True when S looks like a <length>/<number> value (vs a feature name)."
  (and (plusp (length s))
       (let ((c (char s 0))) (or (digit-char-p c) (member c '(#\+ #\- #\.))))))

(defun %cq-scan-ops (s)
  "Return an alist (POS . OP-STRING) of top-level comparison operators in S."
  (let ((out '()) (i 0) (n (length s)))
    (loop while (< i n) do
      (let ((c (char s i)))
        (cond ((and (member c '(#\< #\>)) (< (1+ i) n) (char= (char s (1+ i)) #\=))
               (push (cons i (subseq s i (+ i 2))) out) (incf i 2))
              ((member c '(#\< #\>)) (push (cons i (string c)) out) (incf i))
              ((and (char= c #\=) (or (zerop i) (not (member (char s (1- i)) '(#\< #\>)))))
               (push (cons i "=") out) (incf i))
              (t (incf i)))))
    (nreverse out)))

(defun parse-size-feature (inner)
  "Parse a size-feature interior (the text inside `(...)`) into an AST node:
(:feature NAME OP VALUE-STRING) or (:unknown).  Handles the colon forms
(min-/max-width:), single comparisons (width > 300px) and ranges
(200px <= width < 400px)."
  (let* ((inner (css-trim inner))
         (colon (position #\: inner)))
    (cond
      ;; `[min-|max-]<feature>: <value>`
      (colon
       (let* ((name (string-downcase (css-trim (subseq inner 0 colon))))
              (val (css-trim (subseq inner (1+ colon))))
              (op :=))
         (cond ((and (> (length name) 4) (string= (subseq name 0 4) "min-"))
                (setf op :>= name (subseq name 4)))
               ((and (> (length name) 4) (string= (subseq name 0 4) "max-"))
                (setf op :<= name (subseq name 4))))
         (if (and (member name *cq-size-features* :test #'string=) (plusp (length val)))
             (list :feature name op val)
             '(:unknown))))
      ;; comparison / range form
      (t
       (let ((ops (%cq-scan-ops inner)))
         (case (length ops)
           (1 (let* ((p (caar ops)) (op (%cq-op-sym (cdar ops)))
                     (lhs (string-downcase (css-trim (subseq inner 0 p))))
                     (rhs (css-trim (subseq inner (+ p (length (cdar ops)))))))
                (cond
                  ((null op) '(:unknown))
                  ;; feature OP value
                  ((member lhs *cq-size-features* :test #'string=)
                   (list :feature lhs op rhs))
                  ;; value OP feature  -> flip
                  ((member (string-downcase rhs) *cq-size-features* :test #'string=)
                   (list :feature (string-downcase rhs) (%cq-op-flip op) lhs))
                  (t '(:unknown)))))
           (2 (let* ((p1 (car (first ops))) (o1 (cdr (first ops)))
                     (p2 (car (second ops))) (o2 (cdr (second ops)))
                     (v1 (css-trim (subseq inner 0 p1)))
                     (name (string-downcase (css-trim (subseq inner (+ p1 (length o1)) p2))))
                     (v2 (css-trim (subseq inner (+ p2 (length o2)))))
                     (op1 (%cq-op-sym o1)) (op2 (%cq-op-sym o2)))
                ;; `v1 op1 name op2 v2`  ->  name (flip op1) v1  AND  name op2 v2
                (if (and op1 op2 (member name *cq-size-features* :test #'string=)
                         (%cq-value-token-p v1) (%cq-value-token-p v2))
                    (list :and (list :feature name (%cq-op-flip op1) v1)
                          (list :feature name op2 v2))
                    '(:unknown))))
           (t '(:unknown))))))))

(defun %cq-query-in-parens (tok)
  "Parse a `(...)` query token into an AST — either a nested condition or a size
feature.  A non-paren token is :unknown."
  (if (and tok (plusp (length tok)) (char= (char tok 0) #\())
      (let* ((close (position #\) tok :from-end t))
             (inner (css-trim (subseq tok 1 (or close (length tok))))))
        (cond ((zerop (length inner)) '(:unknown))
              ;; nested condition: starts with `(` or leads with `not`
              ((or (char= (char inner 0) #\()
                   (let ((ts (%cq-split-tokens inner)))
                     (and (rest ts) (member (string-downcase (first ts)) '("not") :test #'string=))))
               (parse-container-condition inner))
              ;; grouped and/or: a `(...)` immediately followed by and/or at top level
              ((let ((ts (%cq-split-tokens inner)))
                 (and (rest ts) (%paren-group-p (first ts))
                      (member (string-downcase (second ts)) '("and" "or") :test #'string=)))
               (parse-container-condition inner))
              (t (parse-size-feature inner))))
      '(:unknown)))

(defun parse-container-condition (s)
  "Parse a <container-condition> string into an evaluable AST:
(:and c...) | (:or c...) | (:not c) | (:feature name op val) | (:unknown)."
  (let ((toks (%cq-split-tokens (css-trim s))))
    (cond
      ((null toks) '(:unknown))
      ((string-equal (first toks) "not")
       (list :not (%cq-query-in-parens (second toks))))
      (t
       (let ((terms (list (%cq-query-in-parens (first toks)))) (op nil) (rest (rest toks)))
         (block chain
           (loop while rest do
             (let ((conn (string-downcase (first rest))))
               (cond ((member conn '("and" "or") :test #'string=)
                      (when (and op (not (string= op conn)))
                        (return-from chain '(:unknown)))   ; mixing and/or needs grouping
                      (setf op conn)
                      (push (%cq-query-in-parens (second rest)) terms)
                      (setf rest (cddr rest)))
                     (t (return-from chain '(:unknown)))))
             finally (return-from chain
                       (cond ((null op) (first terms))
                             ((string= op "and") (cons :and (nreverse terms)))
                             (t (cons :or (nreverse terms))))))))))))

(defun parse-container-prelude (prelude)
  "Parse an @container prelude `[<name>]? <container-condition>` into a query spec
(NAME . COND-AST): NAME a lowercased ident string or NIL."
  (let* ((s (css-trim prelude))
         (toks (%cq-split-tokens s)))
    (if (and toks
             (not (%paren-group-p (first toks)))
             (not (member (string-downcase (first toks)) '("not") :test #'string=)))
        ;; leading ident = container-name; the rest is the condition
        (let* ((name (string-downcase (first toks)))
               (rest (css-trim (subseq s (+ (or (search (first toks) s) 0) (length (first toks)))))))
          (cons name (parse-container-condition rest)))
        (cons nil (parse-container-condition s)))))

(defun parse-stylesheet (css)
  "Parse a CSS string into a list of CSS-RULEs."
  (let* ((toks (css-tokenize css)) (n (length toks)) (i 0) (rules '()))
    (labels ((emit-rule (sel bstart bend)
               ;; CSS Nesting L1: parse a qualified rule's block [BSTART,BEND) whose
               ;; parent selector is SEL.  Direct declarations form a rule with SEL
               ;; (hoisted above nested rules, per spec); nested qualified rules and
               ;; nested @media/@supports/@layer are flattened with `&` resolved
               ;; against SEL.  A non-nested block yields exactly one rule (== the
               ;; pre-nesting behaviour), so ordinary stylesheets are untouched.
               (let ((k bstart) (direct '()) (nested '()))
                 (loop while (< k bend) do
                   (loop while (and (< k bend) (member (ctok-type (aref toks k)) '(:ws :semicolon))) do (incf k))
                   (when (>= k bend) (return))
                   (let ((tk (aref toks k)))
                     (cond
                       ;; nested at-rule (@media / @supports / @layer): keep parent SEL
                       ((eq (ctok-type tk) :at-keyword)
                        (let ((kw (string-downcase (ctok-value tk))) (j (1+ k)))
                          (loop while (and (< j bend) (not (member (ctok-type (aref toks j)) '(:lbrace :semicolon)))) do (incf j))
                          (cond
                            ((and (< j bend) (eq (ctok-type (aref toks j)) :lbrace))
                             (let ((close (match-brace toks j)))
                               (cond
                                 ((string= kw "media")
                                  (when (media-matches-p (toks-text toks (1+ k) j))
                                    (push (list sel (1+ j) close) nested)))
                                 ((member kw '("layer" "supports") :test #'string=)
                                  (push (list sel (1+ j) close nil) nested))
                                 ;; nested @container: defer with the query recorded so
                                 ;; inner rules are stamped when emitted (source order).
                                 ((string= kw "container")
                                  (push (list sel (1+ j) close
                                              (parse-container-prelude (toks-text toks (1+ k) j)))
                                        nested))
                                 (t nil))
                               (setf k (1+ close))))
                            (t (setf k (if (< j bend) (1+ j) bend))))))
                       (t
                        ;; a declaration or a nested qualified rule: scan to the first
                        ;; top-level `{` (=> nested rule) or `;`/block-end (=> decl).
                        (let ((j k) (depth 0) (kind :decl))
                          (loop while (< j bend) do
                            (let ((ty (ctok-type (aref toks j))))
                              (cond ((member ty '(:lparen :lbracket)) (incf depth) (incf j))
                                    ((member ty '(:rparen :rbracket)) (when (plusp depth) (decf depth)) (incf j))
                                    ((and (eq ty :lbrace) (zerop depth)) (setf kind :rule) (return))
                                    ((and (eq ty :semicolon) (zerop depth)) (setf kind :decl) (return))
                                    (t (incf j)))))
                          (cond
                            ((eq kind :rule)
                             (let* ((close (match-brace toks j))
                                    (nsel (string-left-trim '(#\Space #\Tab #\Newline) (toks-text toks k j)))
                                    (rsel (if (plusp (length nsel)) (resolve-nesting-selector nsel sel) sel)))
                               (push (list rsel (1+ j) close nil) nested)
                               (setf k (1+ close))))
                            (t
                             (dolist (d (parse-declarations toks k j)) (push d direct))
                             (setf k (if (< j bend) (1+ j) bend)))))))))
                 ;; hoist direct declarations above nested rules (source order kept
                 ;; within each group via the nreverse below).
                 (when direct
                   (push (make-css-rule :selector sel :decls (nreverse direct)
                                        :container *container-cq*)
                         rules))
                 (dolist (nr (nreverse nested))
                   (let ((*container-cq* (if (fourth nr)
                                             (append *container-cq* (list (fourth nr)))
                                             *container-cq*)))
                     (emit-rule (first nr) (second nr) (third nr))))))
             (collect-rules (start end)
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
                                 ;; @container: capture inner rules, stamping each with
                                 ;; the (name . condition) query so the cascade applies
                                 ;; them only when the queried container matches
                                 ;; (CSS Containment 3 §container queries).  Nested
                                 ;; @container appends, so all enclosing queries must hold.
                                 ((string= kw "container")
                                  (let ((*container-cq*
                                          (append *container-cq*
                                                  (list (parse-container-prelude
                                                         (toks-text toks (1+ i) j))))))
                                    (collect-rules (1+ j) close)))
                                 ;; @font-face holds descriptors (font-family, src,
                                 ;; font-weight/style), not a qualified rule — capture
                                 ;; them under the sentinel selector "@font-face" so a
                                 ;; consumer can fetch+register the web font.  Its
                                 ;; block parses like a declaration block.
                                 ((string= kw "font-face")
                                  (push (make-css-rule :selector "@font-face"
                                                       :decls (parse-declarations toks (1+ j) close))
                                        rules))
                                 ;; @keyframes / @page — skip their blocks.
                                 (t nil))
                               (setf i (1+ close))))
                            (t (setf i (if (< j end) (1+ j) end))))))
                       (t
                        ;; qualified rule: prelude up to '{'
                        (let ((pstart i) (j i))
                          (loop while (and (< j end) (not (eq (ctok-type (aref toks j)) :lbrace))) do (incf j))
                          (if (< j end)
                              (let* ((close (match-brace toks j))
                                     ;; trim only leading whitespace — a trailing space
                                     ;; may be an escaped part of an identifier (`#\ `),
                                     ;; and the selector parser trims safely per part.
                                     (sel (string-left-trim '(#\Space #\Tab #\Newline) (toks-text toks pstart j))))
                                (when (plusp (length sel))
                                  ;; nesting-aware: emit direct decls + any nested rules
                                  (emit-rule sel (1+ j) close))
                                (setf i (1+ close)))
                              (setf i end))))))))))
      (collect-rules 0 n))
    (nreverse rules)))

(defun sheet-has-container-queries-p (stylesheet)
  "True when any rule in STYLESHEET was captured inside an @container block — the
signal that a post-layout container-query resolution pass is needed."
  (some #'css-rule-container stylesheet))
