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
(defun el-classes (n) (let ((c (el-attr n "class"))) (when c (split-ws c))))
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
(defun prev-element (n)
  (let ((p (el-parent n)))
    (when p (let ((sibs (element-children p))) (let ((i (position n sibs))) (when (and i (plusp i)) (nth (1- i) sibs)))))))

;;; ---- simple-selector AST -----------------------------------------------
;;; a compound is a list of simple selectors:
;;;   (:type "div") (:universal) (:class "x") (:id "y")
;;;   (:attr name op value)  op in (nil = ~ \| ^ $ *)
;;;   (:pseudo name arg)     arg = string or sub-selector-list

(defun match-attr (n name op value)
  (let ((have (el-attr n name)))
    (cond ((null op) (and have t))
          ((null have) nil)
          ((string= op "=") (string= have value))
          ((string= op "~=") (member value (split-ws have) :test #'string=))
          ((string= op "|=") (or (string= have value)
                                 (and (> (length have) (length value))
                                      (string= value (subseq have 0 (length value)))
                                      (char= (char have (length value)) #\-))))
          ((string= op "^=") (and (plusp (length value)) (<= (length value) (length have))
                                  (string= value (subseq have 0 (length value)))))
          ((string= op "$=") (and (plusp (length value)) (<= (length value) (length have))
                                  (string= value (subseq have (- (length have) (length value))))))
          ((string= op "*=") (and (plusp (length value)) (search value have) t))
          (t nil))))

(defun parse-nth (arg)
  "Parse an An+B argument; return (values a b) or NIL."
  (let ((s (string-downcase (remove #\Space arg))))
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
      ((string= nm "empty") (zerop (length (el-children n))))
      ((string= nm "first-child") (= (el-index n) 1))
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
      ((string= nm "not")
       (notany (lambda (cx) (match-complex (cx-compounds cx) (cx-combs cx)
                                           (1- (length (cx-compounds cx))) n))
               arg))   ; arg = parsed selector list
      ((member nm '("is" "where" "matches" "any") :test #'string=)
       (some (lambda (cx) (match-complex (cx-compounds cx) (cx-combs cx)
                                         (1- (length (cx-compounds cx))) n))
             arg))
      ;; unknown / link/hover/etc — treat as non-matching (no interactive state)
      (t nil))))

(defun match-simple (n simple)
  (ecase (first simple)
    (:universal t)
    (:type (string-equal (el-name n) (second simple)))
    (:class (member (second simple) (el-classes n) :test #'string=))
    (:id (let ((id (el-attr n "id"))) (and id (string= id (second simple)))))
    (:attr (match-attr n (second simple) (third simple) (fourth simple)))
    (:pseudo (match-pseudo n (second simple) (third simple)))))

(defun match-compound (compound n)
  (and (el-p n) (every (lambda (s) (match-simple n s)) compound)))

;;; ---- complex selector (compounds + combinators), matched right-to-left -
(defstruct cx compounds combs)   ; compounds: vector; combs: vector len-1 (combinator BEFORE each compound)

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

(defun match-complex (compounds combs k n)
  (and (match-compound (aref compounds k) n)
       (or (zerop k)
           (let ((comb (aref combs (1- k))))
             (ecase comb
               (:descendant (loop for a = (el-parent n) then (el-parent a)
                                  while (and a (el-p a))
                                  thereis (match-complex compounds combs (1- k) a)))
               (:child (let ((p (el-parent n))) (and p (el-p p) (match-complex compounds combs (1- k) p))))
               (:adjacent (let ((s (prev-element n))) (and s (match-complex compounds combs (1- k) s))))
               (:sibling (loop for s = (prev-element n) then (prev-element s)
                               while s thereis (match-complex compounds combs (1- k) s))))))))

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
          ((or (alpha-char-p c) (char= c #\-) (char= c #\_))
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
  (loop while (and (< i end) (member (char s i) '(#\Space))) do (incf i))
  (multiple-value-bind (name j) (read-ident s i end)
    (setf i j)
    (loop while (and (< i end) (member (char s i) '(#\Space))) do (incf i))
    (if (and (< i end) (char= (char s i) #\]))
        (values (list :attr name nil nil) (1+ i))
        (let ((op (cond ((and (< (1+ i) end) (member (char s i) '(#\~ #\| #\^ #\$ #\*)) (char= (char s (1+ i)) #\=))
                         (prog1 (subseq s i (+ i 2)) (incf i 2)))
                        ((and (< i end) (char= (char s i) #\=)) (incf i) "=")
                        (t nil))))
          (loop while (and (< i end) (member (char s i) '(#\Space))) do (incf i))
          (let ((val (cond ((and (< i end) (member (char s i) '(#\" #\')))
                            (let ((q (char s i)) (start (1+ i)))
                              (incf i) (loop while (and (< i end) (not (char= (char s i) q))) do (incf i))
                              (prog1 (subseq s start i) (when (< i end) (incf i)))))
                           (t (multiple-value-bind (v k) (read-ident s i end) (setf i k) v)))))
            (loop while (and (< i end) (not (char= (char s i) #\]))) do (incf i))
            (values (list :attr name op val) (if (< i end) (1+ i) i)))))))

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
  (let ((compounds '()) (combs '()) (i 0) (n (length s)) (pending :descendant) (first t) (explicit nil))
    (loop while (< i n) do
      (let ((c (char s i)))
        (cond
          ((member c '(#\Space #\Tab #\Newline)) (incf i) (unless (or first explicit) (setf pending :descendant)))
          ((char= c #\>) (incf i) (setf pending :child explicit t))
          ((char= c #\+) (incf i) (setf pending :adjacent explicit t))
          ((char= c #\~) (incf i) (setf pending :sibling explicit t))
          (t (multiple-value-bind (compound j) (parse-compound s i n)
               (when compound
                 (unless first (push pending combs))
                 (push compound compounds) (setf first nil pending :descendant explicit nil))
               (if (> j i) (setf i j) (incf i)))))))
    (make-cx :compounds (coerce (nreverse compounds) 'vector)
             :combs (coerce (nreverse combs) 'vector))))

(defun parse-selector-list (string)
  "Parse a comma-separated selector list into a list of CX."
  (let ((parts '()) (depth 0) (start 0) (n (length string)))
    (loop for i from 0 below n for c = (char string i) do
      (case c ((#\( #\[) (incf depth)) ((#\) #\]) (decf depth))
        (#\, (when (zerop depth) (push (subseq string start i) parts) (setf start (1+ i))))))
    (push (subseq string start) parts)
    (loop for p in (nreverse parts)
          for trimmed = (string-trim '(#\Space #\Tab #\Newline) p)
          when (plusp (length trimmed)) collect (parse-complex trimmed))))

;;; ---- public API --------------------------------------------------------
(defun selector-matches-p (selector-list n)
  "Does element N match any complex selector in SELECTOR-LIST?"
  (some (lambda (cx) (and (plusp (length (cx-compounds cx)))
                          (match-complex (cx-compounds cx) (cx-combs cx)
                                         (1- (length (cx-compounds cx))) n)))
        selector-list))

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
