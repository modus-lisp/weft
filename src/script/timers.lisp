;;;; src/script/timers.lisp — macrotasks: setTimeout/setInterval + the run loop.
;;;;
;;;; The event-loop split (per shuttle's README): shuttle owns the MICROTASK
;;;; queue (Promise reactions); weft owns the MACROTASK queue (timers, and later
;;;; DOM events / loads).  A virtual clock orders timers; RUN-EVENT-LOOP drains
;;;; the macrotask queue, running shuttle's microtasks after each macrotask.
(in-package #:weft.script)

(defstruct timer id when period callback args (cancelled nil))

(defun schedule-timer (ctx callback delay args &optional period)
  (let ((tm (make-timer :id (incf (context-timer-seq ctx))
                        :when (+ (context-now ctx) (max 0 delay))
                        :period period :callback callback :args args)))
    (push tm (context-timers ctx))
    (timer-id tm)))

(defun cancel-timer (ctx id)
  (dolist (tm (context-timers ctx))
    (when (= (timer-id tm) id) (setf (timer-cancelled tm) t))))

(defun next-timer (ctx)
  "The earliest live timer (ties broken by insertion id), or NIL."
  (let ((best nil))
    (dolist (tm (context-timers ctx) best)
      (unless (timer-cancelled tm)
        (when (or (null best)
                  (< (timer-when tm) (timer-when best))
                  (and (= (timer-when tm) (timer-when best))
                       (< (timer-id tm) (timer-id best))))
          (setf best tm))))))

(defun run-event-loop (ctx &key (max-tasks 200000) until)
  "Drain the macrotask queue: run each due timer (advancing the virtual clock),
   then shuttle's microtasks, until nothing remains or MAX-TASKS is hit (a guard
   against a runaway setInterval/setTimeout loop). Returns the tasks run.

   UNTIL bounds the virtual clock: when supplied, only timers due at or before
   that virtual time run, and the clock is advanced to it — so an interactive
   shell can pump ~one frame of time per real frame instead of draining a
   self-rescheduling setTimeout to the task cap. UNTIL NIL keeps the full-drain
   behavior (unchanged)."
  (let ((*ctx* ctx))
  (js:drain-microtasks)
  (let ((n 0))
    (loop
      (let ((tm (next-timer ctx)))
        (when (null tm) (return))
        (when (and until (> (timer-when tm) until))
          (setf (context-now ctx) (max (context-now ctx) until))
          (return))
        (when (>= n max-tasks) (return))
        (incf n)
        (setf (context-now ctx) (max (context-now ctx) (timer-when tm)))
        (if (timer-period tm)
            (setf (timer-when tm) (+ (context-now ctx) (timer-period tm)))
            (setf (timer-cancelled tm) t))          ; one-shot: retire it
        (setf (context-timers ctx)
              (remove-if #'timer-cancelled (context-timers ctx)))
        (handler-case
            (let ((cb (timer-callback tm)))
              (if (functionp cb)                    ; a host task (e.g. a load event)
                  (funcall cb)
                  (js:invoke (context-realm ctx) cb js:*undefined* (timer-args tm))))
          (js:shuttle-error (e)
            ;; WHOSE TIMER.  A callback has no URL and no place in the document, so "in timer"
            ;; named nothing at all; its function name, when it has one, is the only identity it
            ;; carries into the queue.
            (format *error-output* "~&weft.script: uncaught in timer~@[ (~a)~]: ~a~%"
                    (let ((cb (timer-callback tm)))
                      (and (not (functionp cb))
                           (let ((n (ignore-errors (js:to-string (js:js-get cb "name")))))
                             (and (stringp n) (plusp (length n)) n))))
                    e))
          (error (e)
            (format *error-output* "~&weft.script: timer error: ~a~%" e)))
        (js:drain-microtasks)))
    n)))

(defun schedule-task (ctx thunk &optional (delay 0))
  "Queue a host-side CL THUNK as a macrotask (used for load/error events)."
  (schedule-timer ctx thunk delay nil))

(defun install-timers (ctx)
  (let* ((realm (context-realm ctx))
         (global (js:eval-script realm "globalThis")))
    (flet ((set-timer (args period-p)
             (let ((cb (arg args 0)) (delay (int-arg args 1)))
               (if (js:js-callable-p cb)
                   (num (schedule-timer ctx cb delay (cddr args)
                                        (and period-p delay)))
                   (num 0)))))
      (js:define-global realm "setTimeout"
        (js:native-function realm "setTimeout"
          (lambda (this args) (declare (ignore this)) (set-timer args nil)) 2))
      (js:define-global realm "setInterval"
        (js:native-function realm "setInterval"
          (lambda (this args) (declare (ignore this)) (set-timer args t)) 2))
      (js:define-global realm "clearTimeout"
        (js:native-function realm "clearTimeout"
          (lambda (this args) (declare (ignore this))
            (cancel-timer ctx (int-arg args 0)) js:*undefined*) 1))
      (js:define-global realm "clearInterval" (js:js-get global "clearTimeout"))
      ;; requestAnimationFrame — schedule the callback as a one-shot ~16ms timer
      ;; and invoke it with the frame's timestamp (HTML §animation-frames).  A
      ;; frame budget stops runaway self-rescheduling animation loops (headless
      ;; single-snapshot render); reftest-wait tests settle in a few frames.
      (js:define-global realm "requestAnimationFrame"
        (js:native-function realm "requestAnimationFrame"
          (lambda (this args) (declare (ignore this))
            (let ((cb (arg args 0)))
              (if (and (js:js-callable-p cb) (< (context-raf-count ctx) 600))
                  (progn
                    (incf (context-raf-count ctx))
                    (num (schedule-timer
                          ctx
                          (lambda ()
                            (js:invoke (context-realm ctx) cb js:*undefined*
                                       (list (num (float (context-now ctx) 1d0)))))
                          16 nil)))
                  (num 0)))) 1))
      (js:define-global realm "cancelAnimationFrame"
        (js:native-function realm "cancelAnimationFrame"
          (lambda (this args) (declare (ignore this))
            (cancel-timer ctx (int-arg args 0)) js:*undefined*) 1)))))

;;; ---- the clock a page observes -------------------------------------------
;;; TIMERS RUN ON A VIRTUAL CLOCK, so Date must too, or the two disagree in a way
;;; every page can see.  RUN-EVENT-LOOP fires a setTimeout by ADVANCING
;;; CONTEXT-NOW rather than sleeping -- that is what makes a headless render finish
;;; in seconds instead of honouring every delay a page asks for.  With a real-time
;;; Date, a page's timers then complete while no measurable time passes, and any
;;; duration measured across a timer comes back near zero.  Acid3 subtracts the
;;; delays it requested from wall-clock and so printed a NEGATIVE total.
;;;
;;; The rule is MAX(virtual, real), not simply virtual: virtual time keeps the
;;; page's own setTimeout arithmetic consistent, while real time keeps genuine
;;; compute visible -- a script that spends four seconds in a loop with no timers
;;; at all should not observe a frozen clock.  Taking the larger satisfies both and
;;; is monotonic, which matters more than either: a clock that goes backwards
;;; breaks retry backoff and animation pacing in ways that look like logic errors.
(defun context-clock-ms (ctx)
  "Unix-epoch milliseconds as CTX's page observes them."
  (let ((real (* 1000d0 (/ (float (- (get-internal-real-time)
                                     (context-start-real-time ctx)) 1d0)
                           internal-time-units-per-second))))
    (+ (context-start-wall-ms ctx)
       (max (float (context-now ctx) 1d0) real))))

(defun install-clock ()
  "Point shuttle's Date at the CURRENT context's clock.  Set once, globally, but
   it dispatches on *CTX* -- so two documents in one image each get their own
   timeline, and code running with no context at all still gets the real clock."
  (setf js:*clock-fn*
        (lambda () (if *ctx* (context-clock-ms *ctx*) (current-real-ms)))))

(defun current-real-ms ()
  (* 1000d0 (- (get-universal-time) 2208988800)))
