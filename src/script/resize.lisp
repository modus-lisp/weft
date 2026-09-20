;;;; src/script/resize.lisp — ResizeObserver.
;;;;
;;;; The other half of the observer pair: IntersectionObserver watches the
;;;; VIEWPORT move over fixed geometry, this watches the GEOMETRY change under a
;;;; fixed viewport.  So they are driven by different events and the shell must not
;;;; confuse them -- a scroll can change what is intersecting but cannot change any
;;;; element's size, and re-running resize observations on every scroll would be
;;;; pure cost for an answer that cannot have changed.
;;;;
;;;; Same delivery as the other two observers: a microtask, at the checkpoint after
;;;; the current script.  Same honest limit as IntersectionObserver too -- weft has
;;;; no frame loop, so observations run when a layout is produced and at observe()
;;;; (the spec's initial observation, which is what makes a page that only ever
;;;; renders once still hear its sizes).
(in-package #:weft.script)

(defstruct ro
  "A live ResizeObserver: callback, wrapper, the boxes it observes and the size
   each was last REPORTED at -- the comparison that decides whether anything
   changed has to be against what the page was told, not against the last layout."
  callback
  wrapper
  (targets nil))                        ; list of (node box-kind . last-size)

(defun ro-of (ctx obj)
  (or (gethash obj (context-ro-objs ctx))
      (js:js-throw (js:make-native-error "TypeError" "not a ResizeObserver"))))

(defun %padding-widths (box)
  "(values top right bottom left) padding of BOX, as integers."
  (let ((cs (and box (r:lbox-style box))))
    (if (null cs)
        (values 0 0 0 0)
        (values (round (css:cstyle-padding-top cs))
                (round (css:cstyle-padding-right cs))
                (round (css:cstyle-padding-bottom cs))
                (round (css:cstyle-padding-left cs))))))

(defun %ro-sizes (ctx node)
  "(values content-w content-h border-w border-h pad-left pad-top) for NODE, or
   NIL when it has no box."
  (let ((box (%node-lbox ctx node)))
    (when box
      (multiple-value-bind (bt br bb bl) (%border-widths box)
        (multiple-value-bind (pt pr pb pl) (%padding-widths box)
          (let ((bw (round (r:lbox-w box)))
                (bh (round (r:lbox-h box))))
            (values (max 0 (- bw bl br pl pr))
                    (max 0 (- bh bt bb pt pb))
                    bw bh (+ bl pl) (+ bt pt))))))))

(defun %ro-entry->js (ctx node cw ch bw bh px py)
  (let* ((realm (context-realm ctx))
         (o (js:make-object :proto (proto ctx :resizeobserverentry))))
    (flet ((size-array (inline-size block-size)
             ;; borderBoxSize/contentBoxSize are SEQUENCES in the spec (one entry
             ;; per fragment) even though a non-fragmented box has exactly one --
             ;; code in the wild writes entry.contentBoxSize[0].inlineSize and
             ;; would break on a bare object.
             (let ((arr (js:eval-script realm "[]"))
                   (e (js:make-object :proto (js:eval-script realm "Object.prototype"))))
               (js:put e "inlineSize" (num inline-size))
               (js:put e "blockSize" (num block-size))
               (js:put arr "0" e)
               (js:put arr "length" (num 1))
               arr)))
      (js:put o "target" (wrap ctx node))
      ;; contentRect's origin is the PADDING box, not the document: x/y are the
      ;; left/top padding offsets, which is what makes it a box description rather
      ;; than a position on the page.
      (js:put o "contentRect" (make-dom-rect ctx px py cw ch))
      (js:put o "borderBoxSize" (size-array bw bh))
      (js:put o "contentBoxSize" (size-array cw ch))
      (js:put o "devicePixelContentBoxSize" (size-array cw ch)))
    o))

(defun run-resize-observations (ctx)
  "Re-measure every observed element and deliver what changed.  The shell calls
   this after producing a new layout -- and NOT after a scroll, which cannot
   change a size."
  (when (context-ro-list ctx)
    (let ((*ctx* ctx)
          (js::*current-realm* (context-realm ctx)))
      (dolist (ro (context-ro-list ctx))
        (let ((entries '()))
          (dolist (cell (copy-list (ro-targets ro)))
            (multiple-value-bind (cw ch bw bh px py) (%ro-sizes ctx (car cell))
              (let ((now (list cw ch bw bh)))
                (when (and cw (not (equal now (cddr cell))))
                  (setf (cddr cell) now)
                  (push (list (car cell) cw ch bw bh px py) entries)))))
          (when entries (%ro-deliver ctx ro (nreverse entries))))))))

(defun %ro-deliver (ctx ro entries)
  (js::enqueue-microtask
   (lambda ()
     (let ((arr (js:eval-script (context-realm ctx) "[]")))
       (loop for e in entries for i from 0
             do (js:put arr (princ-to-string i) (apply #'%ro-entry->js ctx e)))
       (js:put arr "length" (num (length entries)))
       (ignore-errors
        (js:js-call (ro-callback ro) (ro-wrapper ro) (list arr (ro-wrapper ro))))))))

(defun install-resize-observer (ctx)
  "Install ResizeObserver + ResizeObserverEntry into CTX's realm."
  (let* ((realm (context-realm ctx))
         (op (js:eval-script realm "Object.prototype"))
         (rop (js:make-object :proto op))
         (rep (js:make-object :proto op)))
    (setf (proto ctx :resizeobserver) rop
          (proto ctx :resizeobserverentry) rep)
    (defmethod* ctx rop "observe" 1 (this a)
      (let* ((ro (ro-of ctx this))
             (target (require-node ctx (arg a 0))))
        (unless (assoc target (ro-targets ro) :test #'eq)
          ;; :none as the remembered size means "never reported", so the first
          ;; measurement always differs and the spec's initial observation lands.
          (push (list* target :box :none) (ro-targets ro)))
        (multiple-value-bind (cw ch bw bh px py) (%ro-sizes ctx target)
          (when cw
            (let ((cell (assoc target (ro-targets ro) :test #'eq)))
              (setf (cddr cell) (list cw ch bw bh)))
            (%ro-deliver ctx ro (list (list target cw ch bw bh px py)))))
        js:*undefined*))
    (defmethod* ctx rop "unobserve" 1 (this a)
      (let ((ro (ro-of ctx this))
            (target (require-node ctx (arg a 0))))
        (setf (ro-targets ro) (remove target (ro-targets ro) :key #'car :test #'eq))
        js:*undefined*))
    (defmethod* ctx rop "disconnect" 0 (this a) (declare (ignore a))
      (setf (ro-targets (ro-of ctx this)) nil)
      js:*undefined*)
    (let ((ctor (js:native-function realm "ResizeObserver"
                  (lambda (this args) (declare (ignore this args))
                    (js:js-throw (js:make-native-error
                                  "TypeError" "Constructor ResizeObserver requires 'new'")))
                  1)))
      (flet ((build (args)
               (let ((cb (arg args 0)))
                 (unless (js:js-callable-p cb)
                   (js:js-throw (js:make-native-error
                                 "TypeError" "ResizeObserver: callback is not a function")))
                 (let* ((obj (js:make-object :proto rop))
                        (ro (make-ro :callback cb :wrapper obj)))
                   (setf (gethash obj (context-ro-objs ctx)) ro)
                   (push ro (context-ro-list ctx))
                   obj))))
        (setf (js::js-object-construct ctor)
              (lambda (args new-target) (declare (ignore new-target)) (build args))))
      (js:put ctor "prototype" rop :enumerable nil :writable nil :configurable nil)
      (js:put rop "constructor" ctor :enumerable nil :writable t :configurable t)
      (js:define-global realm "ResizeObserver" ctor))
    (let ((ector (js:native-function realm "ResizeObserverEntry"
                   (lambda (this args) (declare (ignore this args))
                     (js:js-throw (js:make-native-error "TypeError" "Illegal constructor")))
                   0)))
      (js:put ector "prototype" rep :enumerable nil :writable nil :configurable nil)
      (js:put rep "constructor" ector :enumerable nil :writable t :configurable t)
      (js:define-global realm "ResizeObserverEntry" ector))))
