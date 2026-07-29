;;;; src/script/editing.lisp — typing into a form control.
;;;;
;;;; The forms IDL knows what a control's value IS; nothing knew how a value
;;;; CHANGES under a keyboard.  This is that model, and it is deliberately small:
;;;; a caret offset into the focused control's value, the edits a text field
;;;; supports, and the two events HTML says they fire — `input' on every change,
;;;; `change' once on commit (blur or Enter), never on each keystroke.
;;;;
;;;; The shell calls HANDLE-TEXT-INPUT / HANDLE-EDITING-KEY *after* dispatching
;;;; the keypress/keydown event and only if it was not cancelled, so a page that
;;;; calls preventDefault() on a keystroke suppresses the edit exactly as it does
;;;; in a browser.
(in-package #:weft.script)

(defun text-control-p (node)
  "True for a control the user can type free text into: a text-like <input> or a
   <textarea> that is neither disabled nor readonly."
  (and node (eq (h:dnode-kind node) :element)
       (not (node-disabled-p node))          ; incl. a disabled <fieldset> ancestor
       (not (dom:has-attribute node "readonly"))
       (let ((tag (h:dnode-name node)))
         (or (string= tag "textarea")
             (and (string= tag "input")
                  (not (member (input-type node)
                               '("checkbox" "radio" "button" "submit" "reset" "image"
                                 "file" "hidden" "range" "color")
                               :test #'string=)))))))

(defun editing-target (ctx)
  "The focused control, when it is one we can type into."
  (let ((f (context-focus ctx)))
    (and f (text-control-p f) f)))

(defun control-value (ctx node)
  "NODE's current value: the live one if it has been edited, else its default
   (the `value' attribute for an <input>, the child text for a <textarea>)."
  (or (input-live-value ctx node)
      (if (string= (h:dnode-name node) "textarea")
          (child-text-content node)
          (or (get-attr node "value") ""))))

(defun set-control-value (ctx node value)
  (setf (gethash node (context-input-values ctx)) value
        (context-dirty ctx) t)
  value)

(defun control-maxlength (node)
  (let ((v (get-attr node "maxlength")))
    (and v (ignore-errors (parse-integer (string-trim " " v) :junk-allowed t)))))

;;; ---- caret and selection ---------------------------------------------------
;;; There is no separate caret: a caret is a COLLAPSED SELECTION, kept in the one
;;; per-node table `input.selectionStart' reads (core.lisp).  DIRECTION says
;;; which edge the user is moving; the other edge is the anchor a shift-arrow or
;;; a drag pivots around.

(defun selection-of (ctx node)
  "NODE's selection clamped to its current value: (values start end direction).
   Clamped on the way OUT because the value can shrink under a stored selection
   — a script assigning a shorter one — and nothing tells us when it does."
  (multiple-value-bind (s e d) (node-selection ctx node)
    (let* ((n (length (control-value ctx node)))
           (e (min n (max 0 e)))
           (s (min e (max 0 s))))
      (values s e d))))

(defun caret-of (ctx node)
  "Where NODE's caret sits: the moving edge of its selection."
  (multiple-value-bind (s e d) (selection-of ctx node)
    (if (equal d "backward") s e)))

(defun anchor-of (ctx node)
  "The fixed edge of NODE's selection — what a shift-arrow or a drag pivots around."
  (multiple-value-bind (s e d) (selection-of ctx node)
    (if (equal d "backward") e s)))

(defun set-selection (ctx node start end dir)
  "Store a clamped selection on NODE and ask for a repaint — it is painted, so
   moving it changes the picture even when the value did not."
  (let* ((n (length (control-value ctx node)))
         (e (min n (max 0 end)))
         (s (min e (max 0 start))))
    (set-node-selection ctx node s e dir)
    (setf (context-dirty ctx) t)
    (values s e)))

(defun place-caret (ctx node index)
  "Collapse NODE's selection at INDEX — what a click inside a text control does
   once the shell has turned the pixel it hit into an offset."
  (when (and node (eq (context-focus ctx) node))
    (set-selection ctx node index index "none")
    (caret-of ctx node)))

(defun extend-selection-to (ctx node index)
  "Move the selection's moving edge to INDEX, leaving the anchor where it is —
   a shift-arrow, or a drag with the button still down."
  (when (and node (eq (context-focus ctx) node))
    (let ((a (anchor-of ctx node)))
      (if (< index a)
          (set-selection ctx node index a "backward")
          (set-selection ctx node a index "forward")))
    (caret-of ctx node)))

(defun move-caret (ctx node to &optional extend)
  "Move the caret to TO.  With EXTEND (shift held) the anchor stays and the
   selection grows to TO; without it the selection collapses there."
  (if extend (extend-selection-to ctx node to) (set-selection ctx node to to "none"))
  t)

(defun note-edit-start (ctx node)
  "Remember the value as it stood before the first edit of this focus, so
   COMMIT-EDIT can tell whether anything actually changed.  NIL means untouched."
  (unless (context-edit-start ctx)
    (setf (context-edit-start ctx) (list node (control-value ctx node)))))

(defun fire-input-event (ctx node)
  (fire-at ctx node "input"))

(defun commit-edit (ctx)
  "Fire `change' at the focused control if the user actually altered it since it
   gained focus — HTML's \"user's edit is complete\" moment (blur, or Enter).
   A no-op when nothing was edited, so committing twice fires once."
  (let ((start (context-edit-start ctx)))
    (when start
      (destructuring-bind (node before) start
        (setf (context-edit-start ctx) nil)
        (unless (equal before (control-value ctx node))
          (fire-at ctx node "change")
          t)))))

;;; ---- the edits -------------------------------------------------------------

(defun handle-text-input (ctx text)
  "Insert TEXT at the caret of the focused control, REPLACING the selection if
   there is one.  Returns T when it edited."
  (let ((node (editing-target ctx)))
    (when (and node (plusp (length text)))
      (multiple-value-bind (from to) (selection-of ctx node)
        (let* ((value (control-value ctx node))
               (max (control-maxlength node)))
          ;; maxlength is a UA-enforced limit on *user* input only — a script may
          ;; still assign a longer value, which is why this is here and not in the
          ;; value setter.  It is tested against the RESULT, not the current
          ;; length: typing over a selection can leave a full field shorter, and
          ;; refusing that would wedge the control at its limit.
          (when (and max (>= (- (length value) (- to from)) max))
            (return-from handle-text-input nil))
          (let ((new (concatenate 'string (subseq value 0 from) text (subseq value to))))
            (note-edit-start ctx node)
            (set-control-value ctx node (if max (subseq new 0 (min (length new) max)) new))
            (set-selection ctx node (+ from (length text)) (+ from (length text)) "none")
            (fire-input-event ctx node)
            t))))))

(defun %delete-range (ctx node from to)
  (let ((value (control-value ctx node)))
    (when (< from to)
      (note-edit-start ctx node)
      (set-control-value ctx node
                         (concatenate 'string (subseq value 0 from) (subseq value to)))
      (set-selection ctx node from from "none")
      (fire-input-event ctx node)
      t)))

(defun implicit-submission (ctx node)
  "Enter in a text field submits its form (HTML implicit submission): fire the
   submit button's activation if there is one, else the form's submit event."
  (let ((form (element-form-owner ctx node)))
    (when form
      (let ((submitter (find-if #'submit-button-p
                                (append (dom:get-elements-by-tag-name form "button")
                                        (dom:get-elements-by-tag-name form "input")))))
        (if submitter
            (activate-on-click ctx submitter
                               (lambda () (fire-at ctx submitter "click" :cancelable t)))
            (form-fire-submit ctx form nil)))
      t)))

(defun line-start (value index)
  "The offset just after the newline preceding INDEX (0 on the first line)."
  (let ((nl (position #\Newline value :end (min index (length value)) :from-end t)))
    (if nl (1+ nl) 0)))

(defun line-end (value index)
  "The offset of the newline at or after INDEX, or the end of VALUE."
  (or (position #\Newline value :start (min index (length value))) (length value)))

(defun offset-by-line (value index delta)
  "INDEX moved DELTA lines, keeping its column where the target line is long
   enough.  On the first/last line the caret goes to the start/end instead —
   what every text editor does, and what makes ArrowUp reachable at all."
  (let* ((ls (line-start value index))
         (col (- index ls)))
    (if (minusp delta)
        (if (zerop ls) 0
            (let ((ps (line-start value (1- ls))))
              (min (+ ps col) (1- ls))))
        (let ((le (line-end value index)))
          (if (>= le (length value)) (length value)
              (min (+ (1+ le) col) (line-end value (1+ le))))))))

(defun handle-editing-key (ctx key &key shift)
  "Apply the editing action for a non-printable KEY (\"Backspace\", \"ArrowLeft\",
   \"Home\", \"Enter\", …) to the focused control.  SHIFT extends the selection
   instead of collapsing it.  Returns T when it changed the value, the selection,
   or submitted — i.e. when the shell should repaint."
  (let ((node (editing-target ctx)))
    (when node
      (multiple-value-bind (from to) (selection-of ctx node)
        (let* ((value (control-value ctx node))
               (caret (caret-of ctx node))
               (multiline (string= (h:dnode-name node) "textarea"))
               (n (length value))
               (selected (< from to)))
          (flet ((go-to (i) (move-caret ctx node i shift)))
            (cond
              ;; Backspace/Delete over a selection remove the selection itself —
              ;; the character either side of it is not part of the edit.
              ((string= key "Backspace")
               (if selected (%delete-range ctx node from to)
                   (%delete-range ctx node (max 0 (1- caret)) caret)))
              ((string= key "Delete")
               (if selected (%delete-range ctx node from to)
                   (%delete-range ctx node caret (min n (1+ caret)))))
              ;; A plain arrow against a selection collapses to its near edge
              ;; rather than stepping a character from the moving one.
              ((string= key "ArrowLeft")
               (go-to (if (and selected (not shift)) from (max 0 (1- caret)))))
              ((string= key "ArrowRight")
               (go-to (if (and selected (not shift)) to (min n (1+ caret)))))
              ((string= key "ArrowUp")
               (and multiline (go-to (offset-by-line value caret -1))))
              ((string= key "ArrowDown")
               (and multiline (go-to (offset-by-line value caret 1))))
              ((string= key "Home") (go-to (if multiline (line-start value caret) 0)))
              ((string= key "End")  (go-to (if multiline (line-end value caret) n)))
              ((string= key "Enter")
               (if multiline
                   (handle-text-input ctx (string #\Newline))
                   ;; a single-line field commits, then implicitly submits
                   (progn (commit-edit ctx) (implicit-submission ctx node) t)))
              (t nil))))))))

(defun focus-caret (ctx node)
  "What to paint inside NODE: (values caret selection-start selection-end), or
   NIL when it is not the focused text control.  Bound into
   WEFT.RENDER:*FORM-CARET-FN* by the shell — one hook, because the caret and
   the highlight are two views of the same selection and the painter should not
   have to ask twice and hope the answers agree."
  (when (and (eq (context-focus ctx) node) (text-control-p node))
    (multiple-value-bind (s e) (selection-of ctx node)
      (values (caret-of ctx node) s e))))

(defun live-form-caret-fn (ctx)
  (lambda (node) (focus-caret ctx node)))

(defun select-all (ctx node)
  "Select NODE's whole value — Ctrl-A, and what focusing a field by Tab does."
  (when (and node (text-control-p node))
    (set-selection ctx node 0 (length (control-value ctx node)) "forward")
    t))
