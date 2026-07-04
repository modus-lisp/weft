;;;; inspect/deep-nest.lisp — layout must stay LINEAR in table-nesting depth.
;;;;
;;;; Real pages (Wikipedia infoboxes) nest tables ~10-20 deep.  Layout used to be
;;;; EXPONENTIAL in that depth (~5x/level): the intrinsic-width functions and the
;;;; table column model were recomputed unmemoised, and every nested table re-
;;;; measured its whole subtree at each enclosing level.  Measured then:
;;;;   depth 6 = 0.055s   depth 8 = 1.06s   depth 10 = 24s   depth 12 = timeout
;;;; With a per-layout-pass intrinsic-width memo (and a table's intrinsic width
;;;; taken from its column model, not its flattened inline content) the recursion
;;;; is linear and deep nesting renders in bounded time on a normal stack.
;;;;
;;;; This is a REGRESSION GATE: it renders a synthetic linearly-nested table
;;;;   <table><tr><td>...recurse...</td></tr></table>
;;;; at increasing depths and asserts the per-level cost stays roughly constant
;;;; (never exponential) and nothing stack-overflows.
;;;;   sbcl --script inspect/deep-nest.lisp
(require :asdf)
(push (truename (merge-pathnames "../" (directory-namestring *load-truename*)))
      asdf:*central-registry*)
(handler-bind ((warning #'muffle-warning)) (asdf:load-system "weft/render"))

(defpackage #:weft.deep-nest
  (:use #:cl) (:local-nicknames (#:r #:weft.render))
  (:export #:run))
(in-package #:weft.deep-nest)

(defun nested-table-html (depth)
  "A DEPTH-deep linear chain of single-cell tables (depth = node count)."
  (with-output-to-string (s)
    (write-string "<html><body>" s)
    (dotimes (i depth) (write-string "<table><tr><td>" s))
    (write-string "hi" s)
    (dotimes (i depth) (write-string "</td></tr></table>" s))
    (write-string "</body></html>" s)))

(defun time-render (depth)
  "Seconds to render a DEPTH-deep nested table; NIL on stack exhaustion / error."
  (let ((html (nested-table-html depth)) (start (get-internal-real-time)))
    (handler-case
        (progn (r:render-to-canvas html nil 400 :min-height 50)
               (/ (- (get-internal-real-time) start) (float internal-time-units-per-second)))
      (storage-condition () :stack)
      (error (e) (declare (ignore e)) nil))))

(defun run ()
  (format t "~&=== deep table-nesting layout gate (must be ~~linear, not exponential) ===~%")
  ;; warm the code / caches so the first datapoint isn't skewed
  (time-render 4)
  (let* ((depths '(10 50 100 200))
         (times (mapcar #'time-render depths))
         (fails 0))
    (loop for d in depths for tm in times do
      (format t "  depth ~4d : ~a~%" d
              (cond ((eq tm :stack) "STACK-EXHAUSTED")
                    ((null tm) "ERROR")
                    (t (format nil "~,3f s" tm)))))
    ;; every depth must render (no stack overflow / error)
    (when (some (lambda (x) (or (null x) (eq x :stack))) times)
      (incf fails) (format t "  FAIL: a depth failed to render~%"))
    ;; linearity: doubling depth (100 -> 200) must NOT multiply cost the way an
    ;; exponential would.  Guard generously (<= 4x per doubling, plus a floor so
    ;; sub-millisecond noise never trips it) — a 5x/LEVEL blowup would be ~2^100x.
    (let ((t100 (nth 2 times)) (t200 (nth 3 times)))
      (when (and (numberp t100) (numberp t200)
                 (> t200 (max 0.5 (* 4.0 (max t100 0.001)))))
        (incf fails)
        (format t "  FAIL: depth 200 (~,3fs) not linear vs depth 100 (~,3fs)~%" t200 t100)))
    (format t "~a~%" (if (zerop fails) "deep-nest: PASS" "deep-nest: FAIL"))
    (values (if (zerop fails) 1 0) fails)))

(run)
