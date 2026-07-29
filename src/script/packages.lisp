;;;; src/script/packages.lisp — the weft <-> shuttle scripting bridge package.
;;;;
;;;; weft owns the DOM and layout; shuttle owns the JavaScript language. This
;;;; package is the seam: it backs `document`, the nodes it hands out, the CSSOM
;;;; and the event model with shuttle host objects, so a <script> can read and
;;;; mutate the live weft DOM and have the change reflected on relayout.
(defpackage #:weft.script
  (:use #:cl)
  (:local-nicknames (#:h #:weft.html) (#:dom #:weft.dom) (#:css #:weft.css)
                    (#:r #:weft.render) (#:js #:shuttle) (#:g #:gesso))
  (:export #:make-context #:context-realm #:context-document #:context-dirty
           #:element-object #:wrap #:run-inline-scripts #:run-event-loop
           #:fire-lifecycle-events
           #:render-scripted-to-canvas #:render-scripted-to-png
           ;; interactive-shell seam: dispatch trusted UI events into the DOM,
           ;; and pump one frame of the timer/animation clock
           #:dispatch-mouse-event #:dispatch-keyboard-event #:dispatch-simple-event
           #:pump-timers
           ;; live form state, so the shell can bind WEFT.RENDER:*FORM-VALUE-FN*
           ;; and have the painter show a control's CURRENT value, not its default
           #:input-live-value #:live-form-value-fn
           ;; focus + typing: the shell routes keys to the focused control and
           ;; asks us to apply the edit, having first dispatched (and possibly
           ;; had cancelled) the key event
           #:active-element #:set-focus #:commit-edit #:text-control-p
           #:control-value #:place-caret
           #:handle-text-input #:handle-editing-key #:focus-caret #:live-form-caret-fn
           ;; Web Font Loader (WebFontConfig) convention, for the shell to replay
           #:web-font-config))
