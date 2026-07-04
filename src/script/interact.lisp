;;;; src/script/interact.lisp — dispatch trusted UI events into the live DOM.
;;;;
;;;; The event model (addEventListener / dispatchEvent, capture->target->bubble,
;;;; and inline on<event>= handlers, which register as listeners) already exists.
;;;; This is the seam a platform shell (loom) drives: given the DOM node under
;;;; the pointer or the focused node, synthesize a *trusted* event of the right
;;;; type and dispatch it, so a page's click / hover / key handlers fire exactly
;;;; as they would in a browser.  Additive: no existing behavior changes.
(in-package #:weft.script)

(defun %put-own (obj name value)
  (js:put obj name value :enumerable t :writable t :configurable t))

(defun dispatch-mouse-event (ctx node type
                             &key (bubbles t) (cancelable t) (button 0)
                                  client-x client-y detail)
  "Create a trusted mouse event of TYPE (\"click\", \"mousedown\", \"mousemove\",
   …) and dispatch it to NODE (a weft h:dnode).  BUTTON is the DOM button code
   (0 left, 1 middle, 2 right); CLIENT-X/CLIENT-Y are viewport coordinates
   exposed as event.clientX/clientY.  Returns T unless a listener called
   preventDefault()."
  (when node
    (let* ((obj (make-event-object ctx type nil))
           (ev (evt-of ctx obj)))
      (setf (evt-bubbles ev) (and bubbles t)
            (evt-cancelable ev) (and cancelable t)
            (evt-trusted ev) t)
      (when detail (setf (evt-detail ev) (num detail)))
      (%put-own obj "button" (num button))
      (%put-own obj "buttons" (num (if (zerop button) 1 0)))
      (when client-x (%put-own obj "clientX" (num client-x)) (%put-own obj "pageX" (num client-x)))
      (when client-y (%put-own obj "clientY" (num client-y)) (%put-own obj "pageY" (num client-y)))
      (js:js-truthy (dispatch-event ctx node obj)))))

(defun dispatch-keyboard-event (ctx node type
                                &key (bubbles t) (cancelable t) key key-code char)
  "Create a trusted keyboard event of TYPE (\"keydown\", \"keyup\", \"keypress\")
   and dispatch it to NODE.  KEY is the DOM key string (\"a\", \"Enter\"), KEY-CODE
   the legacy numeric keyCode, CHAR the character for keypress.  Returns T unless
   a listener called preventDefault()."
  (when node
    (let* ((obj (make-event-object ctx type nil))
           (ev (evt-of ctx obj)))
      (setf (evt-bubbles ev) (and bubbles t)
            (evt-cancelable ev) (and cancelable t)
            (evt-trusted ev) t)
      (when key  (%put-own obj "key" key))
      (when char (%put-own obj "key" char)
                 (when (plusp (length char))
                   (%put-own obj "charCode" (num (char-code (char char 0))))))
      (when key-code (%put-own obj "keyCode" (num key-code))
                     (%put-own obj "which" (num key-code)))
      (js:js-truthy (dispatch-event ctx node obj)))))

(defun pump-timers (ctx &optional (ms 16))
  "Advance CTX's virtual clock by MS milliseconds and run the timers/microtasks
   that come due within that window (bounded) — one frame of animation progress
   for an interactive shell.  Unlike a full RUN-EVENT-LOOP, a self-rescheduling
   setTimeout advances by only ~one step per call instead of running to the task
   cap.  MS 0 runs just the already-due (0-delay) tasks.  Returns tasks run."
  (run-event-loop ctx :until (+ (context-now ctx) (max 0 ms))))

(defun dispatch-simple-event (ctx node type &key (bubbles t) (cancelable nil))
  "Create and dispatch a trusted plain Event of TYPE to NODE (input/change/
   focus/blur/…).  Returns T unless preventDefault() was called."
  (when node
    (let* ((obj (make-event-object ctx type nil))
           (ev (evt-of ctx obj)))
      (setf (evt-bubbles ev) (and bubbles t)
            (evt-cancelable ev) (and cancelable t)
            (evt-trusted ev) t)
      (js:js-truthy (dispatch-event ctx node obj)))))
