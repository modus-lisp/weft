;;;; forms-sweep-all.lisp — where the remaining forms headroom actually IS.
;;;;
;;;; Picking a wave target from the previous wave's narrative has been wrong
;;;; three times running (see feedback_swarm_oracle_focus); SWEEP has overruled
;;;; it every time.  So the roster starts here: score every subtree under
;;;; html/semantics/forms and rank by what is reachable but unclaimed.
;;;;
;;;;   sbcl --dynamic-space-size 4096 --disable-debugger \
;;;;        --load inspect/forms-sweep-all.lisp --quit
(require :asdf)
(asdf:load-system :weft/script)
(load (merge-pathnames "forms-oracle.lisp" (or *load-truename* *default-pathname-defaults*)))

(in-package #:weft.forms-oracle)

(defparameter *sweep-dirs*
  '("html/semantics/forms/the-input-element/"
    "html/semantics/forms/constraints/"
    "html/semantics/forms/textfieldselection/"
    "html/semantics/forms/the-form-element/"
    "html/semantics/forms/form-control-infrastructure/"
    "html/semantics/forms/the-select-element/"
    "html/semantics/forms/the-textarea-element/"
    "html/semantics/forms/the-option-element/"
    "html/semantics/forms/the-optgroup-element/"
    "html/semantics/forms/the-label-element/"
    "html/semantics/forms/the-legend-element/"
    "html/semantics/forms/the-button-element/"
    "html/semantics/forms/the-fieldset-element/"
    "html/semantics/forms/the-datalist-element/"
    "html/semantics/forms/the-output-element/"
    "html/semantics/forms/the-meter-element/"
    "html/semantics/forms/the-progress-element/"
    "html/semantics/forms/attributes-common-to-form-controls/"
    "html/semantics/forms/resetting-a-form/"
    "html/semantics/forms/form-submission-0/"))

;;; Files already inside the aperture do not count as headroom — they are being
;;; scored every wave.  Subtract them, or the ranking just re-finds the units we
;;; already own.
(let ((claimed (make-hash-table :test 'equal)))
  (dolist (u *units*)
    (let ((dir (or (cdr (assoc (car u) *unit-dirs* :test #'string=)) *input-dir*)))
      (dolist (f (cdr u)) (setf (gethash (concatenate 'string dir f) claimed) t))))
  (let ((rows '()))
    (dolist (d *sweep-dirs*)
      (format t "~&~%######## ~a~%" d)
      (multiple-value-bind (p n) (ignore-errors (sweep d))
        (when (and p n) (push (list d p n) rows))))
    (format t "~&~%======== RANKED BY UNREACHED SUBTESTS ========~%")
    (dolist (r (sort rows #'> :key (lambda (r) (- (third r) (second r)))))
      (destructuring-bind (d p n) r
        (format t "~&  -~5d   ~5d/~5d   ~a~%" (- n p) p n d)))
    (format t "~&  (aperture currently claims ~d files)~%"
            (hash-table-count claimed))))
