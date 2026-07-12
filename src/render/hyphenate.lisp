;;;; src/render/hyphenate.lisp — Liang's hyphenation algorithm (CSS `hyphens: auto`).
;;;;
;;;; Franklin Mark Liang's competing-patterns method (the one TeX uses): a word is
;;;; scored against a dictionary of patterns, each pattern assigning odd/even break
;;;; priorities to the gaps it covers; the maximum priority wins at each gap, and an
;;;; ODD final priority marks a legal break.  A 2-letter minimum head and 3-letter
;;;; minimum tail are enforced (\lefthyphenmin / \righthyphenmin), matching browsers.
(in-package #:weft.render)

(defvar *hyphen-trie-en* nil
  "Hash: pattern letters (incl. word-boundary `.`) -> a priority vector, one slot
per inter-letter gap of the pattern.  Built lazily from *HYPHEN-PATTERNS-EN*.")
(defvar *hyphen-exc-en* nil
  "Hash: a lowercased whole word -> a sorted list of legal break offsets (the count
of characters left of the break), from the TeX exception log.")

(defun %parse-hyphen-pattern (p)
  "Split a TeX pattern like \"a1bc\" / \".ach4\" into (values LETTERS PRIORITIES):
LETTERS is the pattern with digits removed; PRIORITIES[i] is the digit that stood
immediately before the i-th letter (0 where absent), length = (1+ letters)."
  (let ((letters (make-string-output-stream)) (pairs '()) (li 0))
    (loop for ch across p do
      (let ((d (digit-char-p ch)))
        (if d (push (cons li d) pairs)
            (progn (write-char ch letters) (incf li)))))
    (let ((v (make-array (1+ li) :initial-element 0)))
      (loop for (i . d) in pairs do (setf (aref v i) d))
      (values (get-output-stream-string letters) v))))

(defun %ensure-hyphen-en ()
  "Build the en-US pattern trie and exception table once, on first use."
  (unless *hyphen-trie-en*
    (let ((trie (make-hash-table :test 'equal :size 5000)))
      (loop for p across *hyphen-patterns-en* do
        (multiple-value-bind (letters vals) (%parse-hyphen-pattern p)
          (setf (gethash letters trie) vals)))
      (setf *hyphen-trie-en* trie))
    (let ((exc (make-hash-table :test 'equal :size 1500)))
      (loop for w across *hyphen-exceptions-en* do
        (let ((bare (remove #\- w)) (offs '()) (n 0))
          (loop for ch across w do
            (if (char= ch #\-) (push n offs) (incf n)))
          (setf (gethash (string-downcase bare) exc) (nreverse offs))))
      (setf *hyphen-exc-en* exc))))

(defun hyphenate-word (word &optional (left-min 2) (right-min 3))
  "Legal hyphenation offsets of WORD (English): a sorted list of head-lengths — a
break may follow that many leading characters.  NIL for a word with no letters, a
too-short word, or one with no interior opportunity.  Only [a-z] words hyphenate;
a word carrying digits/punctuation is left whole (its break behaviour, if any, is
the caller's — e.g. explicit U+002D or soft hyphens)."
  (%ensure-hyphen-en)
  (let ((n (length word)))
    (when (and (>= n (+ left-min right-min))
               (loop for ch across word always (alpha-char-p ch)))
      (let ((lw (string-downcase word)))
        (or (gethash lw *hyphen-exc-en*)
            (let* ((dotted (concatenate 'string "." lw "."))
                   (m (length dotted))
                   (pri (make-array (1+ m) :initial-element 0)))
              ;; score every substring of the dotted word against the trie
              (loop for i from 0 below m do
                (loop for j from (1+ i) to m do
                  (let ((v (gethash (subseq dotted i j) *hyphen-trie-en*)))
                    (when v
                      (loop for k from 0 below (length v)
                            do (setf (aref pri (+ i k)) (max (aref pri (+ i k)) (aref v k))))))))
              ;; a break after Q leading letters is legal when the gap before
              ;; dotted[Q+1] scored odd; enforce the head/tail minimums.
              (loop for q from left-min to (- n right-min)
                    when (oddp (aref pri (1+ q))) collect q)))))))

(defun node-lang (node)
  "The effective language of NODE: the nearest `lang` attribute on it or an
ancestor, lowercased, else NIL.  Used to pick a hyphenation dictionary."
  (loop for n = node then (h:dnode-parent n)
        while n
        do (when (eq (h:dnode-kind n) :element)
             (let ((l (cdr (assoc "lang" (h:dnode-attrs n) :test #'string-equal))))
               (when (and l (plusp (length l))) (return (string-downcase l)))))))

(defun hyphenation-lang-ok-p (node)
  "True when NODE's effective language is English — the only bundled dictionary.
An explicit non-English language disables automatic hyphenation (its words are
left whole); other languages fall back the same way until their patterns ship."
  (let ((l (node-lang node)))
    (and l (>= (length l) 2) (string= (subseq l 0 2) "en"))))
