;;;; forms-selection.lisp — HTMLInputElement IDL surface: selection.
;;;;
;;;; Self-registering feature file (see core.lisp *element-proto-extensions*).
;;;; install-element-proto funcalls INSTALL-FORMS-selection with (ctx ep) after the
;;;; built-in accessors.  Fill the installer below; keep every helper prefixed
;;;; so this file never collides with a sibling feature file at load time.
(in-package #:weft.script)

(defun selection-cur-value (ctx node)
  "The current value string for an <input> node."
  (multiple-value-bind (v present) (gethash node (context-input-values ctx))
    (if present v (or (get-attr node "value") ""))))

(defun install-forms-selection (ctx ep)
  (macrolet ((n (this) `(require-node ctx ,this)))
    (let ((starts (make-hash-table :test 'eq))
          (ends (make-hash-table :test 'eq))
          (dirs (make-hash-table :test 'eq)))
      ;; labels (not flet): get-start/get-end/get-dir call selection-supporting-p
      ;; and val-len — sibling locals must see each other.
      (labels ((selection-supporting-p (node)
               "Textual input types that support variable-length selection."
               (and (string= (h:dnode-name node) "input")
                    (let ((type (input-type node)))
                      (member type '("text" "search" "url" "tel" "password")
                              :test #'equal))))
             (val-len (node) (length (selection-cur-value ctx node)))
             ;; Return the finished JS value: a number/string for a supporting
             ;; type, js:*null* otherwise (never (num null) — that would crash).
             (get-start (node)
               (if (selection-supporting-p node)
                   (num (gethash node starts (val-len node)))
                   js:*null*))
             (get-end (node)
               (if (selection-supporting-p node)
                   (num (gethash node ends (val-len node)))
                   js:*null*))
             (get-dir (node)
               (if (selection-supporting-p node)
                   (jstr (or (gethash node dirs) "none"))
                   js:*null*)))
        ;; ---- selectionStart ----
        (defgetset ctx ep "selectionStart" (this)
          (let ((node (n this)))
            (get-start node))
          (v)
          (let ((node (n this)))
            (if (selection-supporting-p node)
                (setf (gethash node starts) (max 0 (min (val-len node) (js-int v))))
                (throw-dom ctx "InvalidStateError" 11
                           "selectionStart not supported for this input type"))))
        ;; ---- selectionEnd ----
        (defgetset ctx ep "selectionEnd" (this)
          (let ((node (n this)))
            (get-end node))
          (v)
          (let ((node (n this)))
            (if (selection-supporting-p node)
                (setf (gethash node ends) (max 0 (min (val-len node) (js-int v))))
                (throw-dom ctx "InvalidStateError" 11
                           "selectionEnd not supported for this input type"))))
        ;; ---- selectionDirection ----
        (defgetset ctx ep "selectionDirection" (this)
          (let ((node (n this)))
            (get-dir node))
          (v)
          (let ((node (n this)))
            (if (selection-supporting-p node)
                (setf (gethash node dirs)
                      (let ((s (jstr v)))
                        (if (member s '("forward" "backward" "none") :test #'equal)
                            s "none")))
                (throw-dom ctx "InvalidStateError" 11
                           "selectionDirection not supported for this input type"))))
        ;; ---- select() ----
        (defmethod* ctx ep "select" 0 (this args)
          (declare (ignore args))
          (let ((node (n this)))
            (when (selection-supporting-p node)
              (let ((len (val-len node)))
                (setf (gethash node starts) 0
                      (gethash node ends) len
                      (gethash node dirs) "none"))))
          js:*undefined*)
        ;; ---- setSelectionRange(start, end, [direction]) ----
        (defmethod* ctx ep "setSelectionRange" 3 (this args)
          (let ((node (n this)))
            (if (selection-supporting-p node)
                (let* ((start (int-arg args 0))
                       (end (int-arg args 1))
                       (dir (arg args 2))
                       (len (val-len node)))
                  (setf (gethash node starts) (max 0 (min len start))
                        (gethash node ends) (max 0 (min len end)))
                  (unless (js:js-undefined-p dir)
                    (setf (gethash node dirs)
                          (let ((s (jstr dir)))
                            (if (member s '("forward" "backward" "none") :test #'equal)
                                s "none")))))
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
                       (nargs (length args)))
                  (if (<= nargs 1)
                      ;; One-arg form: replace the current selection
                      (let* ((s (gethash node starts (val-len node)))
                             (e (gethash node ends (val-len node))))
                        (setf (gethash node (context-input-values ctx))
                              (jstr (concatenate 'string (subseq cur 0 s) replacement
                                                 (subseq cur e))))
                        (setf (gethash node starts) s
                              (gethash node ends) (+ s len)))
                      ;; Three/four-arg form: replace [start, end)
                      (let* ((s (max 0 (min cur-len (int-arg args 1))))
                             (e (max 0 (min cur-len (int-arg args 2))))
                             (new-val (concatenate 'string (subseq cur 0 s) replacement
                                                   (subseq cur e)))
                             (new-len (length new-val)))
                        (setf (gethash node (context-input-values ctx))
                              (jstr new-val))
                        (let ((mode (if (> nargs 3)
                                        (jstr (arg args 3))
                                        "preserve")))
                          (cond ((string= mode "select")
                                 (setf (gethash node starts) s
                                       (gethash node ends) (+ s len)))
                                ((string= mode "start")
                                 (setf (gethash node starts) s
                                       (gethash node ends) s))
                                ((string= mode "end")
                                 (setf (gethash node ends) (+ s len)
                                       (gethash node starts) (+ s len)))
                                (t ;; "preserve"
                                 (let* ((old-s (gethash node starts (val-len node)))
                                        (old-e (gethash node ends (val-len node)))
                                        (delta (- new-len cur-len)))
                                   (declare (ignore old-s))
                                   (setf (gethash node ends)
                                         (max 0 (min new-len (+ old-e delta)))))))))))
                (throw-dom ctx "InvalidStateError" 11
                           "setRangeText not supported for this input type")))
          js:*undefined*)))))

(register-element-proto-extension :selection #'install-forms-selection)