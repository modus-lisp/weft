;;;; src/css/selector.lisp — CSS Selectors (parse + specificity + match a DOM).
;;;;
;;;; Parses a selector list into complex selectors (compound selectors joined by
;;;; combinators), matched right-to-left against weft DOM elements.  Supports
;;;; type/universal, .class, #id, [attr] (=, ~=, |=, ^=, $=, *=), the descendant
;;;; / child (>) / adjacent (+) / general-sibling (~) combinators, and the common
;;;; structural pseudo-classes (:root :empty :first/last/only-child :nth-child()
;;;; :first/last-of-type :nth-of-type() :not()).  Oracle: soupsieve.
(in-package #:weft.css)

;;; ---- DOM access (via weft.html dnode) ----------------------------------
(defun el-p (n) (eq (weft.html:dnode-kind n) :element))
(defun el-name (n) (weft.html:dnode-name n))
(defun el-attr (n a) (cdr (assoc a (weft.html:dnode-attrs n) :test #'string-equal)))
(defun el-parent (n) (weft.html:dnode-parent n))
(defun el-children (n) (weft.html:dnode-children n))
(defvar *el-classes-cache* nil
  "Bound to a fresh EQ hash for one COMPUTE-STYLES pass: an element's class list is
tested against every class selector, so splitting its class attribute once per pass
instead of once per rule removes an O(elements x rules) cost on rule-heavy pages.")
(defun %el-classes (n) (let ((c (el-attr n "class"))) (when c (split-ws c))))
(defun el-classes (n)
  (if *el-classes-cache*
      (multiple-value-bind (v found) (gethash n *el-classes-cache*)
        (if found v (setf (gethash n *el-classes-cache*) (%el-classes n))))
      (%el-classes n)))
(defun split-ws (s)
  (let ((out '()) (b (make-string-output-stream)) (any nil))
    (loop for c across s do
      (if (member c '(#\Space #\Tab #\Newline #\Return #\Page))
          (when any (push (get-output-stream-string b) out) (setf any nil b (make-string-output-stream)))
          (progn (write-char c b) (setf any t))))
    (when any (push (get-output-stream-string b) out))
    (nreverse out)))

(defun element-children (n) (loop for c across (el-children n) when (el-p c) collect c))
(defun el-index (n)   ; 1-based index among element siblings
  (let ((p (el-parent n))) (if p (1+ (position n (element-children p))) 1)))
(defun el-index-of-type (n)
  (let ((p (el-parent n)))
    (if p (1+ (count (el-name n) (subseq (element-children p) 0 (position n (element-children p)))
                     :key #'el-name :test #'string=)) 1)))
(defun el-count-of-type (n)
  "How many siblings (including N) share N's element type."
  (let ((p (el-parent n)))
    (if p (count (el-name n) (element-children p) :key #'el-name :test #'string=) 1)))
(defun el-index-of-type-from-end (n)
  (1+ (- (el-count-of-type n) (el-index-of-type n))))
(defun prev-element (n)
  (let ((p (el-parent n)))
    (when p (let ((sibs (element-children p))) (let ((i (position n sibs))) (when (and i (plusp i)) (nth (1- i) sibs)))))))

;;; ---- simple-selector AST -----------------------------------------------
;;; a compound is a list of simple selectors:
;;;   (:type "div") (:universal) (:class "x") (:id "y")
;;;   (:attr name op value)  op in (nil = ~ \| ^ $ *)
;;;   (:pseudo name arg)     arg = string or sub-selector-list

(defun match-attr (n name op value &optional ci)
  "CI (Selectors 4 attribute case-insensitivity, the `i` flag) compares the value
   ASCII-case-insensitively."
  (let ((have (el-attr n name))
        (test (if ci #'char-equal #'char=)))
    (flet ((s= (a b) (and (= (length a) (length b)) (every test a b)))
           (pre (a b) (and (<= (length a) (length b)) (every test a (subseq b 0 (length a)))))
           (suf (a b) (and (<= (length a) (length b))
                           (every test a (subseq b (- (length b) (length a))))))
           (has (a b) (search a b :test test)))
      (cond ((null op) (and have t))
            ((null have) nil)
            ((string= op "=") (s= have value))
            ((string= op "~=") (member value (split-ws have) :test (if ci #'string-equal #'string=)))
            ((string= op "|=") (or (s= have value)
                                   (and (> (length have) (length value))
                                        (pre value have)
                                        (char= (char have (length value)) #\-))))
            ((string= op "^=") (and (plusp (length value)) (pre value have)))
            ((string= op "$=") (and (plusp (length value)) (suf value have)))
            ((string= op "*=") (and (plusp (length value)) (has value have) t))
            (t nil)))))

(defun css-ws-p (c)
  "CSS whitespace (CSS Syntax §4): space, tab, newline, carriage return, form feed."
  (member c '(#\Space #\Tab #\Newline #\Return #\Page)))

(defun parse-nth (arg)
  "Parse an An+B argument; return (values a b) or NIL."
  (let ((s (string-downcase (remove-if #'css-ws-p arg))))
    (cond
      ((string= s "odd") (values 2 1))
      ((string= s "even") (values 2 0))
      ((find #\n s)
       (let* ((np (position #\n s)) (apart (subseq s 0 np)) (bpart (subseq s (1+ np))))
         (let ((a (cond ((string= apart "") 1) ((string= apart "+") 1) ((string= apart "-") -1)
                        (t (or (ignore-errors (parse-integer apart)) 0))))
               (b (if (string= bpart "") 0 (or (ignore-errors (parse-integer bpart)) 0))))
           (values a b))))
      (t (let ((b (ignore-errors (parse-integer s)))) (when b (values 0 b)))))))

(defun nth-match-p (index a b)
  "Does INDEX satisfy An+B?"
  (if (zerop a) (= index b)
      (let ((q (/ (- index b) a))) (and (integerp q) (>= q 0)))))

(defun match-pseudo (n name arg)
  (let ((nm (string-downcase name)))
    (cond
      ((string= nm "root") (and (el-parent n) (eq (weft.html:dnode-kind (el-parent n)) :document)))
      ;; :empty ignores comments and zero-length text nodes (Selectors).
      ((string= nm "empty")
       (every (lambda (c) (case (weft.html:dnode-kind c)
                            (:comment t)
                            (:text (zerop (length (or (weft.html:dnode-data c) ""))))
                            (t nil)))
              (el-children n)))
      ;; :first-child requires a parent element (the root, whose parent is the
      ;; document, does not match).
      ((string= nm "first-child")
       (and (el-parent n) (eq (weft.html:dnode-kind (el-parent n)) :element) (= (el-index n) 1)))
      ;; Form-control UI state.  With no interactivity every eligible control is
      ;; enabled unless it carries the disabled attribute.
      ((member nm '("enabled" "disabled") :test #'string=)
       (and (member (string-downcase (el-name n))
                    '("input" "button" "select" "textarea" "optgroup" "option" "fieldset")
                    :test #'string=)
            (let ((dis (and (el-attr n "disabled") t)))
              (if (string= nm "disabled") dis (not dis)))))
      ;; :checked — a checkbox/radio (or option) whose "checkedness" is set.  A
      ;; host (weft/script) tracks live state in a reserved `weft-checked`
      ;; attribute; with no scripting it falls back to the checked/selected
      ;; content attribute (so static checked controls render checked).
      ((string= nm "checked")
       (let ((tag (string-downcase (el-name n))) (wc (el-attr n "weft-checked")))
         (cond
           ((string= tag "input")
            (let ((type (string-downcase (or (el-attr n "type") "text"))))
              (and (member type '("checkbox" "radio") :test #'string=)
                   (if wc (string= wc "1") (and (el-attr n "checked") t)))))
           ((string= tag "option")
            (if wc (string= wc "1") (and (el-attr n "selected") t)))
           (t nil))))
      ;; :lang(x) — the nearest ancestor lang attribute is x or an x-* subtag.
      ((string= nm "lang")
       (let ((want (string-downcase (or arg ""))))
         (loop for a = n then (el-parent a) while a
               for lang = (el-attr a "lang")
               when lang do
                 (let ((lang (string-downcase lang)))
                   (return (or (string= lang want)
                               (and (> (length lang) (length want))
                                    (string= lang want :end1 (length want))
                                    (char= (char lang (length want)) #\-)))))
               finally (return nil))))
      ((string= nm "last-child") (let ((p (el-parent n))) (or (null p) (eq n (car (last (element-children p)))))))
      ((string= nm "only-child") (let ((p (el-parent n))) (or (null p) (= 1 (length (element-children p))))))
      ((string= nm "first-of-type") (= (el-index-of-type n) 1))
      ((string= nm "last-of-type")
       (let ((p (el-parent n)))
         (or (null p) (eq n (car (last (remove (el-name n) (element-children p)
                                               :key #'el-name :test (complement #'string=))))))))
      ((string= nm "nth-child")
       (multiple-value-bind (a b) (parse-nth arg) (and a (nth-match-p (el-index n) a b))))
      ((string= nm "nth-of-type")
       (multiple-value-bind (a b) (parse-nth arg) (and a (nth-match-p (el-index-of-type n) a b))))
      ((string= nm "nth-last-of-type")
       (multiple-value-bind (a b) (parse-nth arg) (and a (nth-match-p (el-index-of-type-from-end n) a b))))
      ((string= nm "only-of-type") (= (el-count-of-type n) 1))
      ((string= nm "not")
       (notany (lambda (cx) (match-cx cx n)) arg))   ; arg = parsed selector list
      ((member nm '("is" "where" "matches" "any") :test #'string=)
       (some (lambda (cx) (match-cx cx n)) arg))
      ;; :link matches any hyperlink (a/area/link with href). We have no history,
      ;; so every link is unvisited: :link matches, :visited never does.  (Sites
      ;; routinely colour links via a:link — without this they keep the UA blue.)
      ((string= nm "link")
       (and (member (string-downcase (el-name n)) '("a" "area" "link") :test #'string=)
            (el-attr n "href") t))
      ((string= nm "visited") nil)
      ;; unknown / hover/active/focus/etc — non-matching (no interactive state)
      (t nil))))

(defun match-simple (n simple)
  (ecase (first simple)
    (:universal t)
    (:type (string-equal (el-name n) (second simple)))
    (:class (member (second simple) (el-classes n) :test #'string=))
    (:id (let ((id (el-attr n "id"))) (and id (string= id (second simple)))))
    (:attr (match-attr n (second simple) (third simple) (fourth simple) (fifth simple)))
    (:pseudo (match-pseudo n (second simple) (third simple)))))

(defun match-compound (compound n)
  (and (el-p n) (every (lambda (s) (match-simple n s)) compound)))

;;; ---- complex selector (compounds + combinators), matched right-to-left -
(defstruct cx compounds combs keybits)   ; compounds: vector; combs: vector len-1 (combinator BEFORE each compound); keybits: lazily-cached ancestor-filter key bits per compound

(defun cx-pseudo-element (cx)
  "If CX's final compound names a ::before/::after pseudo-element, return
(values :before|:after stripped-cx) where stripped-cx has that simple removed;
else (values NIL CX).  first-line/first-letter are treated as NIL (no box)."
  (let* ((comps (cx-compounds cx)) (n (length comps)))
    (if (zerop n) (values nil cx)
        (let* ((last (aref comps (1- n)))
               (pe (find-if (lambda (s) (and (eq (first s) :pseudo)
                                             (member (string-downcase (second s)) '("before" "after") :test #'string=)))
                            last)))
          (if (null pe) (values nil cx)
              (let ((new (copy-seq comps)))
                (setf (aref new (1- n)) (or (remove pe last) (list '(:universal))))
                (values (if (string-equal (second pe) "before") :before :after)
                        (make-cx :compounds new :combs (cx-combs cx)))))))))

;;; ---- ancestor Bloom filter ---------------------------------------------
;;; Descendant/child combinators walk an element's ancestor chain, which on a
;;; deeply nested DOM costs O(depth) per candidate rule.  Summarize every
;;; element's ancestors' identifiers (downcased tag, each class, id) in a Bloom
;;; filter — an integer whose set bits are two hashes of each identifier.  Before
;;; walking at a :descendant/:child step, take the most-keyable simple of the
;;; target compound (id > class > tag); if either of its two bits is clear in the
;;; filter, no ancestor carries that identifier, so the walk cannot match and is
;;; skipped.  Bloom filters yield false positives but never false negatives, so a
;;; "maybe" still runs the full walk and the set of matched elements is unchanged.
;;; The filter must be wide enough to stay sparse over a deep DOM: a page nesting
;;; ~25 levels with several classes each sets a few hundred bits, which would
;;; saturate a fixnum (turning every query into a false positive) and disable the
;;; skip.  +BLOOM-BITS+ keeps the load factor — and thus the false-positive rate —
;;; low.  The target compound's two bit positions are the same for every element,
;;; so they are precomputed once per selector (CX-KEYBITS) rather than re-hashed.

(defconstant +bloom-bits+ 512
  "Filter width in bits.  Wide enough that a deep ancestor chain leaves the filter
sparse, keeping the false-positive rate low; the filter is an ordinary integer.")

(defun bloom-positions (string)
  "Two independent bit positions in [0,+BLOOM-BITS+) from two hashes of STRING
(FNV-1a and djb2).  Tags must be downcased by the caller so build and check agree;
classes and ids are hashed verbatim (case-sensitive)."
  (let ((h1 2166136261) (h2 5381))
    (loop for c across string
          for code = (char-code c) do
            (setf h1 (logand (* (logxor h1 code) 16777619) #xffffffff))
            (setf h2 (logand (+ (* h2 33) code) #xffffffff)))
    (values (mod h1 +bloom-bits+) (mod h2 +bloom-bits+))))

(defun bloom-add (bits string)
  "Return BITS with STRING's two hash bits set."
  (multiple-value-bind (p1 p2) (bloom-positions string)
    (logior bits (ash 1 p1) (ash 1 p2))))

(defun el-own-bloom (n)
  "Filter bits for N's own identifiers: downcased tag, each class, and id."
  (let ((bits (bloom-add 0 (string-downcase (el-name n)))))
    (dolist (c (el-classes n)) (setf bits (bloom-add bits c)))
    (let ((id (el-attr n "id"))) (when id (setf bits (bloom-add bits id))))
    bits))

(defun compound-key-bits (compound)
  "For COMPOUND's most-keyable simple (id > class > tag), the two bit positions to
test, as a cons (P1 . P2); NIL when COMPOUND has no id/class/type simple (only
:universal/:attr/:pseudo) and so cannot be filtered."
  (let ((id nil) (class nil) (tag nil))
    (dolist (s compound)
      (case (first s)
        (:id (unless id (setf id (second s))))
        (:class (unless class (setf class (second s))))
        (:type (unless tag (setf tag (second s))))))
    (let ((key (cond (id id) (class class) (tag (string-downcase tag)) (t nil))))
      (when key
        (multiple-value-bind (p1 p2) (bloom-positions key) (cons p1 p2))))))

(defun cx-key-bits (cx)
  "Vector parallel to CX's compounds: each slot holds that compound's ancestor
key bits (a (P1 . P2) cons) or NIL if unkeyable, computed once and cached."
  (or (cx-keybits cx)
      (setf (cx-keybits cx) (map 'vector #'compound-key-bits (cx-compounds cx)))))

(defvar *ancestor-bloom* nil
  "Bound to an EQ hash element->integer for one COMPUTE-STYLES pass: each value is
a Bloom filter of that element's ancestors' identifiers, used to skip descendant
/child walks that provably cannot match.  NIL for callers that don't build it
(e.g. query-select), which then always take the full walk.")

(declaim (inline ancestors-lack-bits-p))
(defun ancestors-lack-bits-p (n need)
  "T only when N's ancestor filter proves the identifier keyed by NEED (a (P1 . P2)
cons of bit positions) is absent from every ancestor of N — the sole case where
the ancestor walk may be skipped.  An ancestor's own bits are a subset of N's
ancestor filter, so the same test is also sound for the :child parent (parent's
bits ⊆ N's filter).  NEED NIL (an unkeyable compound) never skips."
  (and *ancestor-bloom* need
       (let ((f (gethash n *ancestor-bloom*)))
         (and f (not (and (logbitp (car need) f) (logbitp (cdr need) f)))))))

(defun match-complex (compounds combs keybits k n)
  (and (match-compound (aref compounds k) n)
       (or (zerop k)
           (let ((comb (aref combs (1- k))))
             (ecase comb
               (:descendant (unless (ancestors-lack-bits-p n (and keybits (aref keybits (1- k))))
                              (loop for a = (el-parent n) then (el-parent a)
                                    while (and a (el-p a))
                                    thereis (match-complex compounds combs keybits (1- k) a))))
               (:child (unless (ancestors-lack-bits-p n (and keybits (aref keybits (1- k))))
                         (let ((p (el-parent n))) (and p (el-p p) (match-complex compounds combs keybits (1- k) p)))))
               (:adjacent (let ((s (prev-element n))) (and s (match-complex compounds combs keybits (1- k) s))))
               (:sibling (loop for s = (prev-element n) then (prev-element s)
                               while s thereis (match-complex compounds combs keybits (1- k) s))))))))

(defun match-cx (cx n)
  "Does element N match complex selector CX (right-to-left)?"
  (let ((comps (cx-compounds cx)))
    (and (plusp (length comps))
         (match-complex comps (cx-combs cx) (cx-key-bits cx) (1- (length comps)) n))))

;;; ---- rule index (bucket rules by rightmost key) ------------------------
;;; Matching every rule against every element is O(elements x rules) and
;;; dominates rule-heavy pages.  Bucket each rule by the most-specific simple
;;; selector in its rightmost compound (id > class > type > universal); an
;;; element then only tests rules keyed on its own id, classes, tag, or the
;;; universal bucket.  match-complex still runs in full on each candidate, so
;;; correctness is unchanged — the buckets only skip rules that provably cannot
;;; match the element's rightmost compound.

(defstruct rindex by-id by-class by-tag universal)

(defun rule-key (cx)
  "For CX's rightmost compound, return (values kind value): the most-specific
keyable simple selector.  kind = :id | :class | :tag | :universal."
  (let* ((comps (cx-compounds cx)) (n (length comps)))
    (if (zerop n) (values :universal nil)
        (let ((last (aref comps (1- n))) (id nil) (class nil) (tag nil))
          (dolist (s last)
            (case (first s)
              (:id    (unless id (setf id (second s))))
              (:class (unless class (setf class (second s))))
              (:type  (unless tag (setf tag (string-downcase (second s)))))))
          (cond (id (values :id id))
                (class (values :class class))
                (tag (values :tag tag))
                (t (values :universal nil)))))))

(defun build-rindex (rules)
  "RULES is a list of (match-cx pe spec order decls); bucket each by RULE-KEY of
its match-cx.  Bucket order reverses the input, but callers re-sort by (spec,
order), so collection order does not matter."
  (let ((idx (make-rindex :by-id (make-hash-table :test 'equal)
                          :by-class (make-hash-table :test 'equal)
                          :by-tag (make-hash-table :test 'equal)
                          :universal '())))
    (dolist (ru rules idx)
      (multiple-value-bind (kind val) (rule-key (first ru))
        (ecase kind
          (:id        (push ru (gethash val (rindex-by-id idx))))
          (:class     (push ru (gethash val (rindex-by-class idx))))
          (:tag       (push ru (gethash val (rindex-by-tag idx))))
          (:universal (push ru (rindex-universal idx))))))))

(defun map-candidate-rules (fn idx n tag)
  "Call FN on each rule in IDX that could match element N (TAG = its downcased
name): the universal bucket plus rules keyed on N's id, classes, and tag.  No
list is built — each rule is keyed to exactly one bucket, so no duplicates."
  (dolist (ru (rindex-universal idx)) (funcall fn ru))
  (let ((id (el-attr n "id")))
    (when id (dolist (ru (gethash id (rindex-by-id idx))) (funcall fn ru))))
  (dolist (c (el-classes n))
    (dolist (ru (gethash c (rindex-by-class idx))) (funcall fn ru)))
  (dolist (ru (gethash tag (rindex-by-tag idx))) (funcall fn ru)))

;;; ---- selector parsing --------------------------------------------------
(defun parse-compound (s i end)
  "Parse one compound selector from S[i:end]; return (values compound new-i)."
  (let ((simples '()))
    (loop while (< i end) do
      (let ((c (char s i)))
        (cond
          ((char= c #\*) (push '(:universal) simples) (incf i))
          ((char= c #\.) (multiple-value-bind (name j) (read-ident s (1+ i) end)
                           (push (list :class name) simples) (setf i j)))
          ((char= c #\#) (multiple-value-bind (name j) (read-ident s (1+ i) end)
                           (push (list :id name) simples) (setf i j)))
          ((char= c #\[) (multiple-value-bind (sel j) (read-attr s (1+ i) end)
                           (push sel simples) (setf i j)))
          ((char= c #\:) (multiple-value-bind (sel j) (read-pseudo s i end)
                           (push sel simples) (setf i j)))
          ((or (alpha-char-p c) (char= c #\-) (char= c #\_) (char= c #\\))
           ;; A leading backslash starts an escaped identifier (type selector).
           ;; e.g. `\.parser` is a TYPE selector for an element literally named
           ;; ".parser" — NOT the .parser class.  read-ident decodes the escape.
           (multiple-value-bind (name j) (read-ident s i end)
             (push (list :type name) simples) (setf i j)))
          (t (return)))))
    (values (nreverse simples) i)))

(defun read-ident (s i end)
  ;; CSS escapes: a backslash takes the next char literally (e.g. `second\ two`
  ;; -> "second two").  Hex escapes (\20) are not decoded — only the literal form
  ;; Acid2 needs.  Without this, `[class=second\ two]` would parse as "second".
  (let ((out (make-string-output-stream)))
    (loop while (< i end)
          for c = (char s i) do
          (cond ((char= c #\\)
                 (when (< (1+ i) end) (write-char (char s (1+ i)) out))
                 (incf i 2))
                ((or (alphanumericp c) (member c '(#\- #\_)) (> (char-code c) 127))
                 (write-char c out) (incf i))
                (t (return))))
    (values (get-output-stream-string out) i)))

(defun read-attr (s i end)
  "I points just after '['.  Returns (values (:attr name op value) new-i)."
  (loop while (and (< i end) (css-ws-p (char s i))) do (incf i))
  (multiple-value-bind (name j) (read-ident s i end)
    (setf i j)
    (loop while (and (< i end) (css-ws-p (char s i))) do (incf i))
    (if (and (< i end) (char= (char s i) #\]))
        (values (list :attr name nil nil) (1+ i))
        (let ((op (cond ((and (< (1+ i) end) (member (char s i) '(#\~ #\| #\^ #\$ #\*)) (char= (char s (1+ i)) #\=))
                         (prog1 (subseq s i (+ i 2)) (incf i 2)))
                        ((and (< i end) (char= (char s i) #\=)) (incf i) "=")
                        (t nil))))
          (loop while (and (< i end) (css-ws-p (char s i))) do (incf i))
          (let* ((quoted (and (< i end) (member (char s i) '(#\" #\'))))
                 (val (cond (quoted
                            (let ((q (char s i)) (start (1+ i)))
                              (incf i) (loop while (and (< i end) (not (char= (char s i) q))) do (incf i))
                              (prog1 (subseq s start i) (when (< i end) (incf i)))))
                           ;; Unquoted value: read up to ']' (honoring '\' escapes),
                           ;; then trim surrounding whitespace.  This keeps internal
                           ;; spaces that came from a decoded escape (e.g. Acid2's
                           ;; `[class=second\ two]`, whose backslash the tokenizer
                           ;; already resolved into a literal space) — read-ident
                           ;; alone would stop at that space and lose "two".
                           (t (let ((out (make-string-output-stream)))
                                (loop while (< i end)
                                      for c = (char s i) do
                                  (cond ((char= c #\\)
                                         (when (< (1+ i) end) (write-char (char s (1+ i)) out))
                                         (incf i 2))
                                        ((char= c #\]) (return))
                                        (t (write-char c out) (incf i))))
                                (string-trim '(#\Space #\Tab #\Newline)
                                             (get-output-stream-string out))))))
                 (ci nil))
            ;; Selectors 4 attribute-value flag: a trailing `i`/`s` (whitespace-
            ;; separated) before ']'.  For a quoted value it sits after the close
            ;; quote; for an unquoted value it is the last space-separated token.
            (if quoted
                (progn
                  (loop while (and (< i end) (member (char s i) '(#\Space #\Tab #\Newline))) do (incf i))
                  (when (and (< i end) (member (char s i) '(#\i #\I #\s #\S))
                             (or (>= (1+ i) end)
                                 (member (char s (1+ i)) '(#\Space #\Tab #\Newline #\]))))
                    (setf ci (member (char s i) '(#\i #\I))) (incf i)))
                (let ((sp (position #\Space val :from-end t)))
                  (when (and sp (member (subseq val (1+ sp)) '("i" "I" "s" "S") :test #'string=))
                    (setf ci (member (subseq val (1+ sp)) '("i" "I") :test #'string=)
                          val (string-right-trim '(#\Space #\Tab #\Newline) (subseq val 0 sp))))))
            (loop while (and (< i end) (not (char= (char s i) #\]))) do (incf i))
            (values (list :attr name op val (and ci t)) (if (< i end) (1+ i) i)))))))

(defun read-pseudo (s i end)
  "I points at ':' (one or two)."
  (incf i) (when (and (< i end) (char= (char s i) #\:)) (incf i))   ; ::pseudo-element
  (multiple-value-bind (name j) (read-ident s i end)
    (setf i j)
    (if (and (< i end) (char= (char s i) #\())
        (let ((depth 1) (start (1+ i)))
          (incf i)
          (loop while (and (< i end) (plusp depth)) do
            (case (char s i) (#\( (incf depth)) (#\) (decf depth))) (incf i))
          (let ((arg (subseq s start (1- i))))
            (if (member (string-downcase name) '("not" "is" "where" "matches" "any") :test #'string=)
                (values (list :pseudo name (parse-selector-list arg)) i)
                (values (list :pseudo name arg) i))))
        (values (list :pseudo name nil) i))))

(defun parse-complex (s)
  "Parse one complex selector string into a CX."
  (let ((compounds '()) (combs '()) (i 0) (n (length s)) (pending :descendant) (first t) (explicit nil) (invalid nil))
    (loop while (< i n) do
      (let ((c (char s i)))
        (cond
          ((css-ws-p c) (incf i) (unless (or first explicit) (setf pending :descendant)))
          ((char= c #\>) (incf i) (setf pending :child explicit t))
          ((char= c #\+) (incf i) (setf pending :adjacent explicit t))
          ((char= c #\~) (incf i) (setf pending :sibling explicit t))
          (t (multiple-value-bind (compound j) (parse-compound s i n)
               (cond
                 ((> j i)                       ; consumed a compound selector
                  (when compound
                    (unless first (push pending combs))
                    (push compound compounds) (setf first nil pending :descendant explicit nil))
                  (setf i j))
                 ;; An unrecognized token that parse-compound can't consume (e.g.
                 ;; a stray `;` from `.parser {...};` running into the next rule's
                 ;; selector) makes the WHOLE selector invalid — drop the rule.
                 (t (setf invalid t) (return))))))))
    (if invalid
        (make-cx :compounds #() :combs #())
        (make-cx :compounds (coerce (nreverse compounds) 'vector)
                 :combs (coerce (nreverse combs) 'vector)))))

(defun trim-selector (s)
  "Trim leading whitespace and trailing UNESCAPED whitespace from a selector part.
An escaped trailing space (`#\\ ` — an odd run of backslashes before it) is part of
the identifier and is kept, so `#\\ ` still selects id=\" \"."
  (let ((start 0) (end (length s)))
    (loop while (and (< start end) (css-ws-p (char s start))) do (incf start))
    (loop while (and (> end start) (css-ws-p (char s (1- end)))
                     (evenp (loop for k downfrom (- end 2) to start
                                  while (char= (char s k) #\\) count t)))
          do (decf end))
    (subseq s start end)))

(defun parse-selector-list (string)
  "Parse a comma-separated selector list into a list of CX."
  (let ((parts '()) (depth 0) (start 0) (n (length string)))
    (loop for i from 0 below n for c = (char string i) do
      (case c ((#\( #\[) (incf depth)) ((#\) #\]) (decf depth))
        (#\, (when (zerop depth) (push (subseq string start i) parts) (setf start (1+ i))))))
    (push (subseq string start) parts)
    (loop for p in (nreverse parts)
          for trimmed = (trim-selector p)
          for cx = (and (plusp (length trimmed)) (parse-complex trimmed))
          ;; drop invalid selectors (parse-complex yields no compounds), e.g. a
          ;; selector containing a stray `;` from a botched preceding rule.
          when (and cx (plusp (length (cx-compounds cx)))) collect cx)))

;;; ---- public API --------------------------------------------------------
(defun selector-matches-p (selector-list n)
  "Does element N match any complex selector in SELECTOR-LIST?"
  (some (lambda (cx) (match-cx cx n)) selector-list))

(defun specificity (cx)
  "(a b c): id count, class/attr/pseudo-class count, type count."
  (let ((a 0) (b 0) (cc 0))
    (loop for compound across (cx-compounds cx) do
      (dolist (simple compound)
        (case (first simple)
          (:id (incf a))
          ((:class :attr) (incf b))
          (:pseudo (if (member (string-downcase (second simple))
                               '("before" "after" "first-line" "first-letter") :test #'string=)
                       (incf cc) (incf b)))
          (:type (incf cc)))))
    (list a b cc)))

(defun query-select-all (root selector-string)
  "All elements under ROOT (document or element) matching SELECTOR-STRING, in tree order."
  (let ((sl (parse-selector-list selector-string)) (out '()))
    (labels ((walk (n) (when (el-p n) (when (selector-matches-p sl n) (push n out)))
               (loop for c across (el-children n) do (walk c))))
      (loop for c across (el-children root) do (walk c)))
    (nreverse out)))

(defun query-select (root selector-string)
  (first (query-select-all root selector-string)))
