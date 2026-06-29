;;;; src/html/tokenizer.lisp — WHATWG HTML tokenizer (https://html.spec.whatwg.org/#tokenization).
;;;;
;;;; Code points in -> tokens out.  Covers the data / RCDATA / RAWTEXT /
;;;; script-data / PLAINTEXT content models, tags + attributes, comments,
;;;; DOCTYPE, CDATA, and character references (named + numeric).  Parse-error
;;;; signalling is omitted (the gate compares token streams).  TREE construction
;;;; consumes these tokens (next P1 step).
(in-package #:weft.html)

(defstruct tok type name data attrs self-closing public system force-quirks
  pos cend)   ; pos = source index of the tag's '<'; cend = index of its '>'

;;; numeric character reference: C1 overrides + invalid -> U+FFFD
(defparameter *c1-table*
  '((#x80 . #x20AC) (#x82 . #x201A) (#x83 . #x0192) (#x84 . #x201E) (#x85 . #x2026)
    (#x86 . #x2020) (#x87 . #x2021) (#x88 . #x02C6) (#x89 . #x2030) (#x8A . #x0160)
    (#x8B . #x2039) (#x8C . #x0152) (#x8E . #x017D) (#x91 . #x2018) (#x92 . #x2019)
    (#x93 . #x201C) (#x94 . #x201D) (#x95 . #x2022) (#x96 . #x2013) (#x97 . #x2014)
    (#x98 . #x02DC) (#x99 . #x2122) (#x9A . #x0161) (#x9B . #x203A) (#x9C . #x0153)
    (#x9E . #x017E) (#x9F . #x0178)))

(defun numeric-ref-char (code)
  (cond ((or (zerop code) (> code #x10ffff) (<= #xd800 code #xdfff)) #\Replacement_Character)
        ((cdr (assoc code *c1-table*)) (code-char (cdr (assoc code *c1-table*))))
        (t (code-char code))))

(defun consume-char-ref (s n i in-attr)
  "I points just after '&'.  Returns (values replacement-string new-index)."
  (if (>= i n) (values "&" i)
      (let ((c (char s i)))
        (cond
          ((char= c #\#)
           (let ((j (1+ i)) (hex nil) (code 0) (got nil))
             (when (and (< j n) (member (char s j) '(#\x #\X))) (setf hex t) (incf j))
             (loop for d = (and (< j n) (digit-char-p (char s j) (if hex 16 10)))
                   while d do (setf code (+ (* code (if hex 16 10)) d) got t) (incf j))
             (when (and (< j n) (char= (char s j) #\;)) (incf j))
             (if got (values (string (numeric-ref-char code)) j) (values "&" i))))
          ((alphanumericp c)
           (let ((best-len 0) (best-val nil) (maxk (min *max-entity-len* (- n i))))
             (loop for l from 1 to maxk
                   for v = (gethash (subseq s i (+ i l)) *entities*)
                   when v do (setf best-len l best-val v))
             (if (zerop best-len) (values "&" i)
                 (let* ((endpos (+ i best-len)) (semi (char= (char s (1- endpos)) #\;)))
                   (if (and in-attr (not semi) (< endpos n)
                            (let ((nx (char s endpos))) (or (char= nx #\=) (alphanumericp nx))))
                       (values "&" i)
                       (values best-val endpos))))))
          (t (values "&" i))))))

(defun preprocess (input)
  "Normalise newlines: CRLF and lone CR -> LF."
  (let ((out (make-array (length input) :element-type 'character :adjustable t :fill-pointer 0))
        (n (length input)) (i 0))
    (loop while (< i n) do
      (let ((c (char input i)))
        (cond ((char= c #\Return)
               (vector-push-extend #\Newline out)
               (when (and (< (1+ i) n) (char= (char input (1+ i)) #\Newline)) (incf i)))
              (t (vector-push-extend c out)))
        (incf i)))
    (coerce out 'simple-string)))

(defun tokenize (input &key (state :data) last-start-tag)
  "Tokenize INPUT.  STATE may be :data :rcdata :rawtext :script-data :plaintext;
LAST-START-TAG sets the appropriate-end-tag name for the content-model states."
  (let* ((s (preprocess input)) (n (length s)) (i 0) (tokens '())
         (mk (lambda () (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
         (tname nil) (tattrs nil) (aname nil) (aval nil) (end-tag nil) (self-close nil)
         (comment nil) (dname nil) (dpublic nil) (dsystem nil) (fquirks nil)
         (temp nil) (return-state :data) (tagstart 0)
         (last-tag (and last-start-tag (string last-start-tag))))
    (labels ((emit (tk) (push tk tokens))
             (emit-char (c) (push (make-tok :type :char :data (string c)) tokens))
             (emit-str (str) (loop for c across str do (emit-char c)))
             (pushc (c buf) (vector-push-extend (if (eql c #\Nul) #\Replacement_Character c) buf))
             (new-tag (endp) (setf tname (funcall mk) tattrs nil aname nil aval nil
                                   end-tag endp self-close nil))
             (finish-attr ()
               (when aname
                 (let ((nm (coerce aname 'simple-string)))
                   (when (and (plusp (length nm)) (not (assoc nm tattrs :test #'string=)))
                     (push (cons nm (coerce (or aval "") 'simple-string)) tattrs)))
                 (setf aname nil aval nil)))
             (emit-tag ()
               (finish-attr)
               (let ((nm (coerce tname 'simple-string)))
                 (if end-tag
                     (emit (make-tok :type :end-tag :name nm :pos tagstart :cend i))
                     (progn (setf last-tag nm)
                            (emit (make-tok :type :start-tag :name nm :pos tagstart :cend i
                                            :attrs (reverse tattrs) :self-closing self-close))))))
             (emit-doctype ()
               (emit (make-tok :type :doctype
                               :name (and dname (coerce dname 'simple-string))
                               :public (and dpublic (coerce dpublic 'simple-string))
                               :system (and dsystem (coerce dsystem 'simple-string))
                               :force-quirks fquirks)))
             (appropriate-end-p ()
               (and end-tag last-tag (string= (coerce tname 'simple-string) last-tag)))
             (cur () (if (< i n) (char s i) :eof))
             (adv () (incf i)))
      (loop
        (let ((c (cur)))
          (macrolet ((go-to (st) `(progn (setf state ,st)))
                     (consume-go (st) `(progn (adv) (setf state ,st))))
            (ecase state
              ;; ---- data / content models ----
              (:data
               (cond ((eq c :eof) (emit (make-tok :type :eof)) (return))
                     ((char= c #\&) (adv) (setf return-state :data) (go-to :char-ref))
                     ((char= c #\<) (setf tagstart i) (consume-go :tag-open))
                     (t (emit-char c) (adv))))
              (:plaintext
               (if (eq c :eof) (progn (emit (make-tok :type :eof)) (return))
                   (progn (emit-char c) (adv))))
              (:rcdata
               (cond ((eq c :eof) (emit (make-tok :type :eof)) (return))
                     ((char= c #\&) (adv) (setf return-state :rcdata) (go-to :char-ref))
                     ((char= c #\<) (setf temp (funcall mk)) (consume-go :rcdata-lt))
                     (t (emit-char c) (adv))))
              (:rawtext
               (cond ((eq c :eof) (emit (make-tok :type :eof)) (return))
                     ((char= c #\<) (setf temp (funcall mk)) (consume-go :rawtext-lt))
                     (t (emit-char c) (adv))))
              (:script-data
               (cond ((eq c :eof) (emit (make-tok :type :eof)) (return))
                     ((char= c #\<) (setf temp (funcall mk)) (consume-go :script-lt))
                     (t (emit-char c) (adv))))
              ;; ---- generic less-than for content models ----
              ((:rcdata-lt :rawtext-lt :script-lt)
               (let ((back (ecase state (:rcdata-lt :rcdata) (:rawtext-lt :rawtext) (:script-lt :script-data))))
                 (cond ((and (characterp c) (char= c #\/))
                        (setf temp (funcall mk)) (adv)
                        (go-to (ecase back (:rcdata :rcdata-end-open) (:rawtext :rawtext-end-open) (:script-data :script-end-open))))
                       ((and (eq back :script-data) (characterp c) (char= c #\!))
                        (emit-str "<!") (adv) (go-to :script-data)) ; (script escape simplified)
                       (t (emit-char #\<) (go-to back)))))
              ((:rcdata-end-open :rawtext-end-open :script-end-open)
               (let ((back (ecase state (:rcdata-end-open :rcdata) (:rawtext-end-open :rawtext) (:script-end-open :script-data))))
                 (if (and (characterp c) (alpha-char-p c))
                     (progn (new-tag t) (go-to (ecase back (:rcdata :rcdata-end-name) (:rawtext :rawtext-end-name) (:script-data :script-end-name))))
                     (progn (emit-char #\<) (emit-char #\/) (go-to back)))))
              ((:rcdata-end-name :rawtext-end-name :script-end-name)
               (let ((back (ecase state (:rcdata-end-name :rcdata) (:rawtext-end-name :rawtext) (:script-end-name :script-data))))
                 (cond
                   ((and (characterp c) (member c '(#\Tab #\Newline #\Page #\Space)) (appropriate-end-p))
                    (consume-go :before-attr-name))
                   ((and (characterp c) (char= c #\/) (appropriate-end-p)) (consume-go :self-closing))
                   ((and (characterp c) (char= c #\>) (appropriate-end-p)) (emit-tag) (consume-go :data))
                   ((and (characterp c) (alpha-char-p c))
                    (pushc (char-downcase c) tname)
                    (vector-push-extend c temp) (adv))
                   (t (emit-char #\<) (emit-char #\/) (emit-str (coerce temp 'string)) (go-to back)))))
              ;; ---- tags ----
              (:tag-open
               (cond ((and (characterp c) (char= c #\!)) (consume-go :markup-decl))
                     ((and (characterp c) (char= c #\/)) (consume-go :end-tag-open))
                     ((and (characterp c) (alpha-char-p c)) (new-tag nil) (go-to :tag-name))
                     ((and (characterp c) (char= c #\?)) (setf comment (funcall mk)) (go-to :bogus-comment))
                     (t (emit-char #\<) (go-to :data))))
              (:end-tag-open
               (cond ((and (characterp c) (alpha-char-p c)) (new-tag t) (go-to :tag-name))
                     ((and (characterp c) (char= c #\>)) (consume-go :data))
                     ((eq c :eof) (emit-char #\<) (emit-char #\/) (emit (make-tok :type :eof)) (return))
                     (t (setf comment (funcall mk)) (go-to :bogus-comment))))
              (:tag-name
               (cond ((eq c :eof) (emit (make-tok :type :eof)) (return))
                     ((member c '(#\Tab #\Newline #\Page #\Space)) (consume-go :before-attr-name))
                     ((char= c #\/) (consume-go :self-closing))
                     ((char= c #\>) (emit-tag) (consume-go :data))
                     (t (pushc (char-downcase c) tname) (adv))))
              (:before-attr-name
               (cond ((eq c :eof) (go-to :after-attr-name))
                     ((member c '(#\Tab #\Newline #\Page #\Space)) (adv))
                     ((member c '(#\/ #\>)) (go-to :after-attr-name))
                     ((eql c #\=)        ; unexpected '=' starts an attr named "="
                      (finish-attr) (setf aname (funcall mk) aval (funcall mk))
                      (vector-push-extend #\= aname) (consume-go :attr-name))
                     (t (finish-attr) (setf aname (funcall mk) aval (funcall mk)) (go-to :attr-name))))
              (:attr-name
               (cond ((or (eq c :eof) (member c '(#\Tab #\Newline #\Page #\Space #\/ #\>))) (go-to :after-attr-name))
                     ((char= c #\=) (consume-go :before-attr-value))
                     (t (pushc (char-downcase c) aname) (adv))))
              (:after-attr-name
               (cond ((eq c :eof) (finish-attr) (emit (make-tok :type :eof)) (return))
                     ((member c '(#\Tab #\Newline #\Page #\Space)) (adv))
                     ((char= c #\/) (finish-attr) (consume-go :self-closing))
                     ((char= c #\=) (consume-go :before-attr-value))
                     ((char= c #\>) (emit-tag) (consume-go :data))
                     (t (finish-attr) (setf aname (funcall mk) aval (funcall mk)) (go-to :attr-name))))
              (:before-attr-value
               (cond ((member c '(#\Tab #\Newline #\Page #\Space)) (adv))
                     ((eql c #\") (consume-go :attr-value-dq))
                     ((eql c #\') (consume-go :attr-value-sq))
                     ((eql c #\>) (emit-tag) (consume-go :data))
                     (t (go-to :attr-value-uq))))
              (:attr-value-dq
               (cond ((eq c :eof) (emit (make-tok :type :eof)) (return))
                     ((char= c #\") (consume-go :after-attr-value-q))
                     ((char= c #\&) (adv) (setf return-state :attr-value-dq) (go-to :char-ref))
                     (t (pushc c aval) (adv))))
              (:attr-value-sq
               (cond ((eq c :eof) (emit (make-tok :type :eof)) (return))
                     ((char= c #\') (consume-go :after-attr-value-q))
                     ((char= c #\&) (adv) (setf return-state :attr-value-sq) (go-to :char-ref))
                     (t (pushc c aval) (adv))))
              (:attr-value-uq
               (cond ((eq c :eof) (emit (make-tok :type :eof)) (return))
                     ((member c '(#\Tab #\Newline #\Page #\Space)) (consume-go :before-attr-name))
                     ((char= c #\&) (adv) (setf return-state :attr-value-uq) (go-to :char-ref))
                     ((char= c #\>) (emit-tag) (consume-go :data))
                     (t (pushc c aval) (adv))))
              (:after-attr-value-q
               (cond ((eq c :eof) (emit (make-tok :type :eof)) (return))
                     ((member c '(#\Tab #\Newline #\Page #\Space)) (consume-go :before-attr-name))
                     ((char= c #\/) (consume-go :self-closing))
                     ((char= c #\>) (emit-tag) (consume-go :data))
                     (t (go-to :before-attr-name))))
              (:self-closing
               (cond ((eq c :eof) (emit (make-tok :type :eof)) (return))
                     ((char= c #\>) (setf self-close t) (emit-tag) (consume-go :data))
                     (t (go-to :before-attr-name))))
              ;; ---- comments / markup declaration ----
              (:markup-decl
               (cond ((and (<= (+ i 2) n) (string= (subseq s i (+ i 2)) "--"))
                      (incf i 2) (setf comment (funcall mk)) (go-to :comment-start))
                     ((and (<= (+ i 7) n) (string-equal (subseq s i (+ i 7)) "doctype"))
                      (incf i 7) (go-to :doctype))
                     ((and (<= (+ i 7) n) (string= (subseq s i (+ i 7)) "[CDATA["))
                      (incf i 7) (setf comment (funcall mk)) (go-to :cdata))
                     (t (setf comment (funcall mk)) (go-to :bogus-comment))))
              (:bogus-comment
               (cond ((or (eq c :eof) (and (characterp c) (char= c #\>)))
                      (emit (make-tok :type :comment :data (coerce comment 'simple-string)))
                      (if (eq c :eof) (progn (emit (make-tok :type :eof)) (return)) (consume-go :data)))
                     (t (pushc c comment) (adv))))
              (:comment-start
               (cond ((and (characterp c) (char= c #\-)) (consume-go :comment-start-dash))
                     ((and (characterp c) (char= c #\>)) (emit (make-tok :type :comment :data (coerce comment 'simple-string))) (consume-go :data))
                     (t (go-to :comment))))
              (:comment-start-dash
               (cond ((and (characterp c) (char= c #\-)) (consume-go :comment-end))
                     ((and (characterp c) (char= c #\>)) (emit (make-tok :type :comment :data (coerce comment 'simple-string))) (consume-go :data))
                     ((eq c :eof) (emit (make-tok :type :comment :data (coerce comment 'simple-string))) (emit (make-tok :type :eof)) (return))
                     (t (vector-push-extend #\- comment) (go-to :comment))))
              (:comment
               (cond ((eq c :eof) (emit (make-tok :type :comment :data (coerce comment 'simple-string))) (emit (make-tok :type :eof)) (return))
                     ((char= c #\-) (consume-go :comment-end-dash))
                     (t (pushc c comment) (adv))))
              (:comment-end-dash
               (cond ((eq c :eof) (emit (make-tok :type :comment :data (coerce comment 'simple-string))) (emit (make-tok :type :eof)) (return))
                     ((char= c #\-) (consume-go :comment-end))
                     (t (vector-push-extend #\- comment) (go-to :comment))))
              (:comment-end
               (cond ((eq c :eof) (emit (make-tok :type :comment :data (coerce comment 'simple-string))) (emit (make-tok :type :eof)) (return))
                     ((char= c #\>) (emit (make-tok :type :comment :data (coerce comment 'simple-string))) (consume-go :data))
                     ((char= c #\!) (consume-go :comment-end-bang))
                     ((char= c #\-) (vector-push-extend #\- comment) (adv))
                     (t (vector-push-extend #\- comment) (vector-push-extend #\- comment) (go-to :comment))))
              (:comment-end-bang
               (cond ((eql c #\-) (vector-push-extend #\- comment) (vector-push-extend #\- comment) (vector-push-extend #\! comment) (consume-go :comment-end-dash))
                     ((eql c #\>) (emit (make-tok :type :comment :data (coerce comment 'simple-string))) (consume-go :data))
                     ((eq c :eof) (emit (make-tok :type :comment :data (coerce comment 'simple-string))) (emit (make-tok :type :eof)) (return))
                     (t (vector-push-extend #\- comment) (vector-push-extend #\- comment) (vector-push-extend #\! comment) (go-to :comment))))
              (:cdata
               (cond ((eq c :eof) (emit-str (coerce comment 'string)) (emit (make-tok :type :eof)) (return))
                     ((and (char= c #\]) (<= (+ i 3) n) (string= (subseq s i (+ i 3)) "]]>"))
                      (incf i 3) (emit-str (coerce comment 'string)) (go-to :data))
                     (t (pushc c comment) (adv))))
              ;; ---- doctype ----
              (:doctype
               (cond ((eq c :eof) (emit (make-tok :type :doctype :force-quirks t)) (emit (make-tok :type :eof)) (return))
                     ((member c '(#\Tab #\Newline #\Page #\Space)) (consume-go :before-doctype-name))
                     (t (go-to :before-doctype-name))))
              (:before-doctype-name
               (cond ((eq c :eof) (emit (make-tok :type :doctype :force-quirks t)) (emit (make-tok :type :eof)) (return))
                     ((member c '(#\Tab #\Newline #\Page #\Space)) (adv))
                     ((char= c #\>) (emit (make-tok :type :doctype :force-quirks t)) (consume-go :data))
                     (t (setf dname (funcall mk) dpublic nil dsystem nil fquirks nil)
                        (pushc (char-downcase c) dname) (consume-go :doctype-name))))
              (:doctype-name
               (cond ((eq c :eof) (setf fquirks t) (emit-doctype) (emit (make-tok :type :eof)) (return))
                     ((member c '(#\Tab #\Newline #\Page #\Space)) (consume-go :after-doctype-name))
                     ((char= c #\>) (emit-doctype) (consume-go :data))
                     (t (pushc (char-downcase c) dname) (adv))))
              (:after-doctype-name
               (cond ((eq c :eof) (setf fquirks t) (emit-doctype) (emit (make-tok :type :eof)) (return))
                     ((member c '(#\Tab #\Newline #\Page #\Space)) (adv))
                     ((char= c #\>) (emit-doctype) (consume-go :data))
                     ((and (<= (+ i 6) n) (string-equal (subseq s i (+ i 6)) "public"))
                      (incf i 6) (go-to :after-dt-public-kw))
                     ((and (<= (+ i 6) n) (string-equal (subseq s i (+ i 6)) "system"))
                      (incf i 6) (go-to :after-dt-system-kw))
                     (t (setf fquirks t) (go-to :bogus-doctype))))
              ((:after-dt-public-kw :before-dt-public-id)
               (cond ((eq c :eof) (setf fquirks t) (emit-doctype) (emit (make-tok :type :eof)) (return))
                     ((member c '(#\Tab #\Newline #\Page #\Space))
                      (if (eq state :after-dt-public-kw) (consume-go :before-dt-public-id) (adv)))
                     ((char= c #\") (setf dpublic (funcall mk)) (consume-go :dt-public-dq))
                     ((char= c #\') (setf dpublic (funcall mk)) (consume-go :dt-public-sq))
                     ((char= c #\>) (setf fquirks t) (emit-doctype) (consume-go :data))
                     (t (setf fquirks t) (go-to :bogus-doctype))))
              (:dt-public-dq
               (cond ((eq c :eof) (setf fquirks t) (emit-doctype) (emit (make-tok :type :eof)) (return))
                     ((char= c #\") (consume-go :after-dt-public-id))
                     ((char= c #\>) (setf fquirks t) (emit-doctype) (consume-go :data))
                     (t (pushc c dpublic) (adv))))
              (:dt-public-sq
               (cond ((eq c :eof) (setf fquirks t) (emit-doctype) (emit (make-tok :type :eof)) (return))
                     ((char= c #\') (consume-go :after-dt-public-id))
                     ((char= c #\>) (setf fquirks t) (emit-doctype) (consume-go :data))
                     (t (pushc c dpublic) (adv))))
              ((:after-dt-public-id :between-dt-public-system)
               (cond ((eq c :eof) (setf fquirks t) (emit-doctype) (emit (make-tok :type :eof)) (return))
                     ((member c '(#\Tab #\Newline #\Page #\Space))
                      (if (eq state :after-dt-public-id) (consume-go :between-dt-public-system) (adv)))
                     ((char= c #\>) (emit-doctype) (consume-go :data))
                     ((char= c #\") (setf dsystem (funcall mk)) (consume-go :dt-system-dq))
                     ((char= c #\') (setf dsystem (funcall mk)) (consume-go :dt-system-sq))
                     (t (setf fquirks t) (go-to :bogus-doctype))))
              ((:after-dt-system-kw :before-dt-system-id)
               (cond ((eq c :eof) (setf fquirks t) (emit-doctype) (emit (make-tok :type :eof)) (return))
                     ((member c '(#\Tab #\Newline #\Page #\Space))
                      (if (eq state :after-dt-system-kw) (consume-go :before-dt-system-id) (adv)))
                     ((char= c #\") (setf dsystem (funcall mk)) (consume-go :dt-system-dq))
                     ((char= c #\') (setf dsystem (funcall mk)) (consume-go :dt-system-sq))
                     ((char= c #\>) (setf fquirks t) (emit-doctype) (consume-go :data))
                     (t (setf fquirks t) (go-to :bogus-doctype))))
              (:dt-system-dq
               (cond ((eq c :eof) (setf fquirks t) (emit-doctype) (emit (make-tok :type :eof)) (return))
                     ((char= c #\") (consume-go :after-dt-system-id))
                     ((char= c #\>) (setf fquirks t) (emit-doctype) (consume-go :data))
                     (t (pushc c dsystem) (adv))))
              (:dt-system-sq
               (cond ((eq c :eof) (setf fquirks t) (emit-doctype) (emit (make-tok :type :eof)) (return))
                     ((char= c #\') (consume-go :after-dt-system-id))
                     ((char= c #\>) (setf fquirks t) (emit-doctype) (consume-go :data))
                     (t (pushc c dsystem) (adv))))
              (:after-dt-system-id
               (cond ((eq c :eof) (setf fquirks t) (emit-doctype) (emit (make-tok :type :eof)) (return))
                     ((member c '(#\Tab #\Newline #\Page #\Space)) (adv))
                     ((char= c #\>) (emit-doctype) (consume-go :data))
                     (t (go-to :bogus-doctype))))   ; no force-quirks here
              (:bogus-doctype
               (cond ((eq c :eof) (emit-doctype) (emit (make-tok :type :eof)) (return))
                     ((char= c #\>) (emit-doctype) (consume-go :data))
                     (t (adv))))
              ;; ---- character reference ----
              (:char-ref
               (multiple-value-bind (str ni) (consume-char-ref s n i (member return-state '(:attr-value-dq :attr-value-sq :attr-value-uq)))
                 (setf i ni)
                 (if (member return-state '(:attr-value-dq :attr-value-sq :attr-value-uq))
                     (loop for ch across str do (vector-push-extend ch aval))
                     (emit-str str))
                 (go-to return-state))))))))
    (values (nreverse tokens) s)))   ; 2nd value: preprocessed source (for raw-text extraction)
