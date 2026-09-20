;;;; src/script/intersection.lisp — IntersectionObserver.
;;;;
;;;; This is what lazy-loading, infinite scroll, and view-tracking analytics
;;;; actually wait on: not a scroll handler, but a callback that fires when an
;;;; element enters or leaves the viewport.  It is buildable now only because
;;;; element rectangles exist (geometry.lisp) -- an observer with nothing to
;;;; measure could report only "not intersecting", forever, which is exactly the
;;;; kind of plausible-but-useless answer that would make a page's loading spinner
;;;; run to the end of time.
;;;;
;;;; WHEN OBSERVATION RUNS, said plainly, because this is where weft differs from a
;;;; live browser.  The spec runs the observation steps in "update the rendering",
;;;; once per animation frame, so a browser re-checks continuously as the user
;;;; scrolls.  weft lays a page out and the shell blits slices of it; there is no
;;;; frame loop to hang this on.  So observations run at the three moments the
;;;; answer can actually have changed:
;;;;   * observe() -- the spec's own initial observation, so a page that observes
;;;;     an already-visible element hears about it without waiting for anything;
;;;;   * when the shell publishes a new layout or a new scroll position (loom calls
;;;;     RUN-INTERSECTION-OBSERVATIONS after each render);
;;;;   * never otherwise.
;;;; A page that scrolls itself with JS and expects a callback mid-script will not
;;;; get one.  That is a real limitation and it is written down rather than papered
;;;; over with a timer that fires whether or not anything moved.
;;;;
;;;; Delivery is a MICROTASK, as MutationObserver's is: the callback runs at the
;;;; checkpoint after the current script, not synchronously inside observe().
(in-package #:weft.script)

;;; ---- data model -----------------------------------------------------------
(defstruct io
  "A live IntersectionObserver: its JS callback and wrapper, the root element to
   measure against (NIL = the viewport), the parsed rootMargin, the sorted
   threshold list, the observed targets, and the entries waiting for delivery."
  callback
  wrapper
  root                                  ; a dnode, or NIL for the viewport
  (margins '(0 0 0 0))                  ; top right bottom left, px
  (thresholds '(0.0))
  (targets nil)                         ; list of (node . last-ratio-index)
  (queue nil))                          ; entries pending delivery, newest first

(defun io-of (ctx obj)
  (or (gethash obj (context-io-objs ctx))
      (js:js-throw (js:make-native-error
                    "TypeError" "not an IntersectionObserver"))))

;;; ---- options --------------------------------------------------------------
(defun %parse-root-margin (s)
  "Parse a rootMargin into (top right bottom left) px, CSS-shorthand style.  Only
   px and % are legal here and % needs a root size to resolve, so a % component is
   refused rather than silently read as px -- a wrong margin moves every boundary
   the observer reports."
  (let ((parts '()) (start 0))
    (loop for i = (position #\Space s :start start)
          do (let ((tok (string-trim " " (subseq s start i))))
               (when (plusp (length tok)) (push tok parts)))
             (if i (setf start (1+ i)) (return)))
    (setf parts (nreverse parts))
    (when (null parts) (setf parts '("0px")))
    (let ((nums (mapcar (lambda (tok)
                          (let ((n (ignore-errors
                                     (let ((*read-eval* nil))
                                       (read-from-string
                                        (string-right-trim "px" tok))))))
                            (cond ((search "%" tok)
                                   (js:js-throw
                                    (js:make-native-error
                                     "SyntaxError"
                                     "rootMargin in % is not supported")))
                                  ((realp n) (round n))
                                  (t (js:js-throw
                                      (js:make-native-error
                                       "SyntaxError"
                                       (format nil "bad rootMargin component: ~a" tok)))))))
                        parts)))
      (case (length nums)
        (1 (list (first nums) (first nums) (first nums) (first nums)))
        (2 (list (first nums) (second nums) (first nums) (second nums)))
        (3 (list (first nums) (second nums) (third nums) (second nums)))
        (t (subseq nums 0 4))))))

(defun %parse-thresholds (v)
  "The threshold list, sorted ascending and clamped to [0,1].  A single number and
   a list are both legal; out of range is a RangeError, as the spec says."
  (flet ((one (x)
           (let ((n (js:to-number x)))
             (unless (and (realp n) (<= 0 n 1))
               (js:js-throw (js:make-native-error
                             "RangeError" "threshold must be in [0,1]")))
             (float n 1d0))))
    (cond
      ((js:js-undefined-p v) '(0.0d0))
      ((and (js:js-object-p v) (js:js-has v "length"))
       (let* ((n (round (js:to-number (js:js-get v "length"))))
              (xs (loop for i from 0 below n
                        collect (one (js:js-get v (princ-to-string i))))))
         (sort (or xs (list 0.0d0)) #'<)))
      (t (list (one v))))))

;;; ---- the geometry ---------------------------------------------------------
(defun %root-rect (ctx io)
  "(values x y w h) of the observer's root, in DOCUMENT coordinates, before
   rootMargin.  With no root element that is the viewport: the shell's scroll
   position and its viewport height, which is the rectangle the user can see."
  (let ((root (io-root io)))
    (if root
        (let ((box (%node-lbox ctx root)))
          (when box
            (multiple-value-bind (bt br bb bl) (%border-widths box)
              (values (+ (r:lbox-x box) bl) (+ (r:lbox-y box) bt)
                      (max 0 (- (r:lbox-w box) bl br))
                      (max 0 (- (r:lbox-h box) bt bb))))))
        (values 0 (context-scroll-y ctx)
                (context-width ctx)
                (or (context-viewport-height ctx)
                    ;; no shell has told us how tall the window is; the document's
                    ;; own height is the only honest stand-in, and it means "all of
                    ;; it is in view", which is what a static render IS.
                    (let ((root-box (ensure-layout ctx)))
                      (if root-box (r:lbox-h root-box) 0)))))))

(defun %intersect (ax ay aw ah bx by bw bh)
  "(values x y w h) of the overlap of two rectangles, zero-sized when disjoint."
  (let* ((x0 (max ax bx)) (y0 (max ay by))
         (x1 (min (+ ax aw) (+ bx bw))) (y1 (min (+ ay ah) (+ by bh))))
    (values x0 y0 (max 0 (- x1 x0)) (max 0 (- y1 y0)))))

(defun %threshold-index (thresholds ratio intersecting)
  "Which threshold band RATIO falls in -- the spec's way of deciding whether
   anything actually CHANGED.  An observer fires when the band changes, not on
   every pixel of movement, which is why a scroll does not produce a callback per
   row of pixels."
  (if (not intersecting)
      0
      (let ((idx 0))
        (loop for th in thresholds for i from 1
              do (when (>= ratio th) (setf idx i)))
        idx)))

(defun %io-observe-one (ctx io node)
  "Measure NODE against IO's root and, when its threshold band changed, queue an
   entry.  Returns T when an entry was queued."
  (let* ((cell (assoc node (io-targets io) :test #'eq))
         (rects (node-document-rects ctx node)))
    (multiple-value-bind (rx ry rw rh) (%root-rect ctx io)
      (when (null rx) (return-from %io-observe-one nil))
      (destructuring-bind (mt mr mb ml) (io-margins io)
        (let* ((rx (- rx ml)) (ry (- ry mt))
               (rw (+ rw ml mr)) (rh (+ rh mt mb)))
          (if (null rects)
              ;; not rendered: the spec reports a zero target rect, not "no entry"
              (let ((changed (and cell (/= (cdr cell) 0))))
                (when cell (setf (cdr cell) 0))
                (when changed
                  (push (list node 0 0 0 0 0 0 0 0 rx ry rw rh 0.0d0 nil)
                        (io-queue io)))
                changed)
              (let* ((x0 (reduce #'min rects :key #'first))
                     (y0 (reduce #'min rects :key #'second))
                     (x1 (reduce #'max rects :key (lambda (r) (+ (first r) (third r)))))
                     (y1 (reduce #'max rects :key (lambda (r) (+ (second r) (fourth r)))))
                     (tw (- x1 x0)) (th (- y1 y0)))
                (multiple-value-bind (ix iy iw ih) (%intersect x0 y0 tw th rx ry rw rh)
                  (let* ((area (* tw th))
                         (ratio (if (plusp area) (/ (float (* iw ih) 1d0) area) 0.0d0))
                         (intersecting (and (plusp iw) (plusp ih)))
                         (band (%threshold-index (io-thresholds io) ratio intersecting)))
                    (when (null cell)
                      (setf cell (cons node -1))
                      (push cell (io-targets io)))
                    (when (/= band (cdr cell))
                      (setf (cdr cell) band)
                      (push (list node x0 y0 tw th ix iy iw ih rx ry rw rh
                                  ratio intersecting)
                            (io-queue io))
                      t))))))))))

;;; ---- delivery -------------------------------------------------------------
(defun %io-entry->js (ctx entry)
  (destructuring-bind (node bx by bw bh ix iy iw ih rx ry rw rh ratio intersecting)
      entry
    (let* ((sy (context-scroll-y ctx))
           (o (js:make-object :proto (proto ctx :intersectionobserverentry))))
      (flet ((rect (x y w h)
               (make-dom-rect ctx x (- y sy) w h)))
        (js:put o "target" (wrap ctx node))
        (js:put o "boundingClientRect" (rect bx by bw bh))
        (js:put o "intersectionRect" (rect ix iy iw ih))
        (js:put o "rootBounds" (rect rx ry rw rh))
        (js:put o "intersectionRatio" (num ratio))
        (js:put o "isIntersecting" (if intersecting js:*true* js:*false*))
        (js:put o "time" (num (context-now ctx))))
      o)))

(defun %io-deliver (ctx io)
  "Hand IO's queued entries to its callback.  A microtask, like
   MutationObserver's: the page sees them at the checkpoint after the current
   script rather than re-entrantly inside observe()."
  (let ((entries (nreverse (io-queue io))))
    (setf (io-queue io) nil)
    (when entries
      (js::enqueue-microtask
       (lambda ()
         (let ((arr (js:eval-script (context-realm ctx) "[]")))
           (loop for e in entries for i from 0
                 do (js:put arr (princ-to-string i) (%io-entry->js ctx e)))
           (js:put arr "length" (num (length entries)))
           (ignore-errors
            (js:js-call (io-callback io) (io-wrapper io)
                        (list arr (io-wrapper io))))))))))

(defun run-intersection-observations (ctx)
  "Re-measure every observed element and deliver what changed.  The shell calls
   this after publishing a new layout or scroll position -- those are the moments
   an element's visibility can have changed without the DOM changing."
  (when (context-io-list ctx)
    ;; THE SHELL CALLS THIS, not a script, so neither the context nor the realm is
    ;; bound on the way in -- and queueing a microtask captures the current realm.
    ;; Without this the first scroll after an observe() died on an unbound realm,
    ;; which is a failure that only appears once something outside the engine
    ;; drives it.
    (let ((*ctx* ctx)
          (js::*current-realm* (context-realm ctx)))
      (dolist (io (context-io-list ctx))
        (dolist (cell (copy-list (io-targets io)))
          (%io-observe-one ctx io (car cell)))
        (%io-deliver ctx io)))))

;;; ---- the interface --------------------------------------------------------
(defun install-intersection-observer (ctx)
  "Install IntersectionObserver + IntersectionObserverEntry into CTX's realm."
  (let* ((realm (context-realm ctx))
         (op (js:eval-script realm "Object.prototype"))
         (iop (js:make-object :proto op))
         (iep (js:make-object :proto op)))
    (setf (proto ctx :intersectionobserver) iop
          (proto ctx :intersectionobserverentry) iep)
    (defmethod* ctx iop "observe" 1 (this a)
      (let ((io (io-of ctx this))
            (target (require-node ctx (arg a 0))))
        (unless (assoc target (io-targets io) :test #'eq)
          ;; -1 is "no band yet", so the FIRST measurement always differs and the
          ;; initial observation the spec requires is guaranteed to be delivered --
          ;; including for an element that is already out of view, whose band is 0.
          (push (cons target -1) (io-targets io)))
        (%io-observe-one ctx io target)
        (%io-deliver ctx io)
        js:*undefined*))
    (defmethod* ctx iop "unobserve" 1 (this a)
      (let ((io (io-of ctx this))
            (target (require-node ctx (arg a 0))))
        (setf (io-targets io) (remove target (io-targets io) :key #'car :test #'eq))
        js:*undefined*))
    (defmethod* ctx iop "disconnect" 0 (this a) (declare (ignore a))
      (let ((io (io-of ctx this)))
        (setf (io-targets io) nil (io-queue io) nil)
        js:*undefined*))
    (defmethod* ctx iop "takeRecords" 0 (this a) (declare (ignore a))
      (let* ((io (io-of ctx this))
             (entries (nreverse (io-queue io))))
        (setf (io-queue io) nil)
        (let ((arr (js:eval-script realm "[]")))
          (loop for e in entries for i from 0
                do (js:put arr (princ-to-string i) (%io-entry->js ctx e)))
          (js:put arr "length" (num (length entries)))
          arr)))
    (defget ctx iop "root" (this)
      (let ((r (io-root (io-of ctx this))))
        (if r (wrap ctx r) js:*null*)))
    (defget ctx iop "rootMargin" (this)
      (format nil "~{~dpx~^ ~}" (io-margins (io-of ctx this))))
    (defget ctx iop "thresholds" (this)
      (let* ((io (io-of ctx this))
             (arr (js:eval-script realm "[]")))
        (loop for th in (io-thresholds io) for i from 0
              do (js:put arr (princ-to-string i) (num th)))
        (js:put arr "length" (num (length (io-thresholds io))))
        arr))
    (let ((ctor (js:native-function realm "IntersectionObserver"
                  (lambda (this args) (declare (ignore this args))
                    (js:js-throw (js:make-native-error
                                  "TypeError"
                                  "Constructor IntersectionObserver requires 'new'")))
                  1)))
      (flet ((build (args)
               (let ((cb (arg args 0))
                     (opts (arg args 1)))
                 (unless (js:js-callable-p cb)
                   (js:js-throw (js:make-native-error
                                 "TypeError"
                                 "IntersectionObserver: callback is not a function")))
                 (let* ((obj (js:make-object :proto iop))
                        (root (and (js:js-object-p opts)
                                   (let ((r (js:js-get opts "root")))
                                     (and (js:js-object-p r) (node-of ctx r)))))
                        (margin (if (and (js:js-object-p opts)
                                         (not (js:js-undefined-p (js:js-get opts "rootMargin"))))
                                    (%parse-root-margin (jstr (js:js-get opts "rootMargin")))
                                    '(0 0 0 0)))
                        (thr (if (js:js-object-p opts)
                                 (%parse-thresholds (js:js-get opts "threshold"))
                                 '(0.0d0)))
                        (io (make-io :callback cb :wrapper obj :root root
                                     :margins margin :thresholds thr)))
                   (setf (gethash obj (context-io-objs ctx)) io)
                   (push io (context-io-list ctx))
                   obj))))
        (setf (js::js-object-construct ctor)
              (lambda (args new-target) (declare (ignore new-target)) (build args))))
      (js:put ctor "prototype" iop :enumerable nil :writable nil :configurable nil)
      (js:put iop "constructor" ctor :enumerable nil :writable t :configurable t)
      (js:define-global realm "IntersectionObserver" ctor))
    (let ((ector (js:native-function realm "IntersectionObserverEntry"
                   (lambda (this args) (declare (ignore this args))
                     (js:js-throw (js:make-native-error
                                   "TypeError" "Illegal constructor")))
                   0)))
      (js:put ector "prototype" iep :enumerable nil :writable nil :configurable nil)
      (js:put iep "constructor" ector :enumerable nil :writable t :configurable t)
      (js:define-global realm "IntersectionObserverEntry" ector))))
