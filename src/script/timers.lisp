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
            (format *error-output* "~&weft.script: uncaught in timer: ~a~%" e))
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
      (js:define-global realm "clearInterval" (js:js-get global "clearTimeout")))))
