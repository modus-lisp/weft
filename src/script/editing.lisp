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

(defun clamp-caret (ctx node)
  (let ((n (length (control-value ctx node))))
    (setf (context-caret ctx) (max 0 (min n (context-caret ctx))))))

(defun caret-of (ctx node)
  "The caret offset in NODE, clamped to its value."
  (clamp-caret ctx node)
  (context-caret ctx))

(defun place-caret (ctx node index)
  "Put the caret at INDEX in NODE (clamped) — what a click inside a text control
   does once the shell has turned the pixel it hit into an offset."
  (when (and node (eq (context-focus ctx) node))
    (setf (context-caret ctx) index
          (context-caret-anchor ctx) nil)
    (clamp-caret ctx node)
    (setf (context-dirty ctx) t)
    (context-caret ctx)))

(defun move-caret (ctx to)
  "Set the caret to TO and ask for a repaint (the caret is painted, so moving it
   changes the picture even though the value did not)."
  (setf (context-caret ctx) to
        (context-caret-anchor ctx) nil
        (context-dirty ctx) t)
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
  "Insert TEXT at the caret of the focused control.  Returns T when it edited."
  (let ((node (editing-target ctx)))
    (when (and node (plusp (length text)))
      (let* ((value (control-value ctx node))
             (caret (caret-of ctx node))
             (max (control-maxlength node)))
        ;; maxlength is a UA-enforced limit on *user* input only — a script may
        ;; still assign a longer value, which is why this is here and not in the
        ;; value setter.
        (when (and max (>= (length value) max))
          (return-from handle-text-input nil))
        (let ((new (concatenate 'string (subseq value 0 caret) text (subseq value caret))))
          (note-edit-start ctx node)
          (set-control-value ctx node (if max (subseq new 0 (min (length new) max)) new))
          (setf (context-caret ctx) (+ caret (length text)))
          (fire-input-event ctx node)
          t)))))

(defun %delete-range (ctx node from to)
  (let ((value (control-value ctx node)))
    (when (< from to)
      (note-edit-start ctx node)
      (set-control-value ctx node
                         (concatenate 'string (subseq value 0 from) (subseq value to)))
      (setf (context-caret ctx) from)
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

(defun handle-editing-key (ctx key)
  "Apply the editing action for a non-printable KEY (\"Backspace\", \"ArrowLeft\",
   \"Home\", \"Enter\", …) to the focused control.  Returns T when it changed the
   value, the caret, or submitted — i.e. when the shell should repaint."
  (let ((node (editing-target ctx)))
    (when node
      (let* ((value (control-value ctx node))
             (caret (caret-of ctx node))
             (n (length value)))
        (cond
          ((string= key "Backspace") (%delete-range ctx node (max 0 (1- caret)) caret))
          ((string= key "Delete")    (%delete-range ctx node caret (min n (1+ caret))))
          ((string= key "ArrowLeft")  (move-caret ctx (max 0 (1- caret))))
          ((string= key "ArrowRight") (move-caret ctx (min n (1+ caret))))
          ((string= key "Home")       (move-caret ctx 0))
          ((string= key "End")        (move-caret ctx n))
          ((string= key "Enter")
           (if (string= (h:dnode-name node) "textarea")
               (handle-text-input ctx (string #\Newline))
               ;; a single-line field commits, then implicitly submits
               (progn (commit-edit ctx) (implicit-submission ctx node) t)))
          (t nil))))))

(defun focus-caret (ctx node)
  "The caret offset to paint inside NODE, or NIL when it is not the focused
   text control.  Bound into WEFT.RENDER:*FORM-CARET-FN* by the shell."
  (and (eq (context-focus ctx) node) (text-control-p node)
       (caret-of ctx node)))

(defun live-form-caret-fn (ctx)
  (lambda (node) (focus-caret ctx node)))
