;;;; forms-selection.lisp — HTMLInputElement IDL surface: selection.
;;;;
;;;; Self-registering feature file (see core.lisp *element-proto-extensions*).
;;;; install-element-proto funcalls INSTALL-FORMS-selection with (ctx ep) after the
;;;; built-in accessors.  Fill the installer below; keep every helper prefixed
;;;; so this file never collides with a sibling feature file at load time.
(in-package #:weft.script)

(defun selection-cur-value (ctx node)
  "The current value string for an <input> or <textarea> node."
  (multiple-value-bind (v present) (gethash node (context-input-values ctx))
    (if present v
        (if (string= (h:dnode-name node) "textarea")
            (child-text-content node)
            (or (get-attr node "value") "")))))

(defun queue-select-event (ctx node)
  "Queue a macrotask that fires a 'select' event at NODE (bubbles, not cancelable, trusted)."
  (let ((evt-obj (make-event-object ctx "select")))
    (let ((ev (evt-of ctx evt-obj)))
      (setf (evt-bubbles ev) t
            (evt-cancelable ev) nil
            (evt-trusted ev) t))
    (schedule-task ctx (lambda ()
                         (dispatch-event ctx node evt-obj)
                         js:*undefined*))))


(defun install-forms-selection (ctx ep)
  (macrolet ((n (this) `(require-node ctx ,this)))
    ;; The selection lives on the CONTEXT (core.lisp), not in closure tables
    ;; here: the caret the user drags and walks with the arrow keys IS this
    ;; selection, and the editor (editing.lisp) has to read and write the same
    ;; one.  The initial selection is 0,0 — NOT the end of the value.
    ;; select-event.html asserts `clone.selectionEnd === 0' on a freshly cloned
    ;; <textarea>foobar</textarea>, and defaulting to the value length made that
    ;; read 6.
    (progn
      (labels ((selection-supporting-p (node)
                 "Textual input types that support variable-length selection, plus textarea."
                 (or (string= (h:dnode-name node) "textarea")
                     (and (string= (h:dnode-name node) "input")
                          (member (input-type node)
                                  '("text" "search" "url" "tel" "password")
                                  :test #'equal))))
               (val-len (node) (length (selection-cur-value ctx node)))
               (cur-start (node) (nth-value 0 (node-selection ctx node)))
               (cur-end (node) (nth-value 1 (node-selection ctx node)))
               (cur-dir (node) (nth-value 2 (node-selection ctx node)))
               (set-selection-range (node start end dir)
                 "HTML \"set the selection range\": clamp START/END into the value,
                  normalise DIR, and queue a `select\' event ONLY IF the selection
                  actually moved.  The fire-if-changed test is the whole point —
                  select-event.html has a `must not fire the second time\' and a
                  `must fire select only once\' subtest for every one of the nine
                  ways to move a selection, so firing unconditionally costs more
                  subtests than not firing at all."
                 (let* ((len (val-len node))
                        (e (min len (max 0 end)))
                        (s (min e (min len (max 0 start))))
                        (d (if (member dir '("forward" "backward") :test #'equal)
                               dir "none")))
                   (when (set-node-selection ctx node s e d)
                     (setf (context-dirty ctx) t)   ; the caret is painted
                     (queue-select-event ctx node))))
               ;; Return the finished JS value: a number/string for a supporting
               ;; type, js:*null* otherwise (never (num null) — that would crash).
               (get-start (node)
                 (if (selection-supporting-p node) (num (cur-start node)) js:*null*))
               (get-end (node)
                 (if (selection-supporting-p node) (num (cur-end node)) js:*null*))
               (get-dir (node)
                 (if (selection-supporting-p node) (jstr (cur-dir node)) js:*null*)))
        ;; ---- selectionStart ----
        ;; Setter (HTML): let end be this\'s end; if end < the new value, set end
        ;; to the new value; then set the selection range.
        (defgetset ctx ep "selectionStart" (this)
          (let ((node (n this)))
            (get-start node))
          (v)
          (let ((node (n this)))
            (if (selection-supporting-p node)
                (let ((s (max 0 (js-int v))))
                  (set-selection-range node s (max s (cur-end node)) (cur-dir node)))
                (throw-dom ctx "InvalidStateError" 11
                           "selectionStart not supported for this input type"))))
        ;; ---- selectionEnd ----
        (defgetset ctx ep "selectionEnd" (this)
          (let ((node (n this)))
            (get-end node))
          (v)
          (let ((node (n this)))
            (if (selection-supporting-p node)
                (set-selection-range node (cur-start node) (max 0 (js-int v))
                                     (cur-dir node))
                (throw-dom ctx "InvalidStateError" 11
                           "selectionEnd not supported for this input type"))))
        ;; ---- selectionDirection ----
        (defgetset ctx ep "selectionDirection" (this)
          (let ((node (n this)))
            (get-dir node))
          (v)
          (let ((node (n this)))
            (if (selection-supporting-p node)
                (set-selection-range node (cur-start node) (cur-end node) (jstr v))
                (throw-dom ctx "InvalidStateError" 11
                           "selectionDirection not supported for this input type"))))
        ;; ---- select() ----
        ;; A no-op (and NO event) on a control with no selectable text; the
        ;; select event is queued by set-selection-range only when it moves.
        (defmethod* ctx ep "select" 0 (this args)
          (declare (ignore args))
          (let ((node (n this)))
            (when (selection-supporting-p node)
              (set-selection-range node 0 (val-len node) "none")))
          js:*undefined*)
        ;; ---- setSelectionRange(start, end, [direction]) ----
        (defmethod* ctx ep "setSelectionRange" 3 (this args)
          (let ((node (n this)))
            (if (selection-supporting-p node)
                (let ((dir (arg args 2)))
                  (set-selection-range node (int-arg args 0) (int-arg args 1)
                                       (if (js:js-undefined-p dir) "none" (jstr dir))))
                (throw-dom ctx "InvalidStateError" 11
                           "setSelectionRange not supported for this input type")))
          js:*undefined*)
        ;; ---- setRangeText(replacement, [start, end, [selectMode]]) ----
        (defmethod* ctx ep "setRangeText" 4 (this args)
          (let ((node (n this)))
            (if (selection-supporting-p node)
                (let* ((replacement (jstr (arg args 0)))
                       (len (length replacement))
                       (cur (selection-cur-value ctx node))
                       (cur-len (length cur))
                       (nargs (length args))
                       (one-arg (<= nargs 1))
                       (s (if one-arg (min cur-len (cur-start node))
                              (max 0 (min cur-len (int-arg args 1)))))
                       (e (if one-arg (min cur-len (cur-end node))
                              (max 0 (min cur-len (int-arg args 2)))))
                       (mode (if (> nargs 3) (jstr (arg args 3)) "preserve")))
                  (when (> s e) (rotatef s e))
                  (let* ((new-val (concatenate 'string (subseq cur 0 s) replacement
                                               (subseq cur e)))
                         (new-len (length new-val))
                         (delta (- new-len cur-len)))
                    (setf (gethash node (context-input-values ctx)) (jstr new-val))
                    ;; The one-argument form always selects the replacement.
                    (cond ((or one-arg (string= mode "select"))
                           (set-selection-range node s (+ s len) (cur-dir node)))
                          ((string= mode "start")
                           (set-selection-range node s s (cur-dir node)))
                          ((string= mode "end")
                           (set-selection-range node (+ s len) (+ s len) (cur-dir node)))
                          (t ;; "preserve"
                           (let ((old-s (cur-start node))
                                 (old-e (cur-end node)))
                             (set-selection-range node
                                                  (if (> old-s e) (+ old-s delta) old-s)
                                                  (if (> old-e e) (+ old-e delta) old-e)
                                                  (cur-dir node)))))))
                (throw-dom ctx "InvalidStateError" 11
                           "setRangeText not supported for this input type")))
          js:*undefined*)))))

(register-element-proto-extension :selection #'install-forms-selection)