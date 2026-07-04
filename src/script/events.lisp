;;;; src/script/events.lisp — DOM Events: EventTarget + Event/UIEvent/CustomEvent.
;;;;
;;;; addEventListener/removeEventListener/dispatchEvent on any node, with the
;;;; capture -> target -> bubble propagation path, plus the legacy createEvent +
;;;; initEvent/initUIEvent/initCustomEvent that Acid3 uses.
(in-package #:weft.script)

(defstruct evt
  (type "") (bubbles nil) (cancelable nil) target current-target
  (phase 0) (default-prevented nil) (stopped nil) (stop-immediate nil)
  (detail js:*undefined*) (view js:*null*) (trusted nil) (dispatched nil))

(defun evt-of (ctx obj) (gethash obj (context-events ctx)))

(defun make-event-object (ctx type &optional (init nil))
  "Create an Event wrapper of TYPE, its EVT state registered in the context."
  (let* ((e (make-evt :type (or type "")))
         (obj (js:make-object :proto (proto ctx :event))))
    (when (js:js-object-p init)
      (when (js:js-truthy (js:js-get init "bubbles")) (setf (evt-bubbles e) t))
      (when (js:js-truthy (js:js-get init "cancelable")) (setf (evt-cancelable e) t))
      (let ((d (js:js-get init "detail"))) (unless (js:js-undefined-p d) (setf (evt-detail e) d))))
    (setf (gethash obj (context-events ctx)) e)
    obj))

(defun install-event-proto (ctx ep)
  (macrolet ((e (this) `(or (evt-of ctx ,this)
                            (js:js-throw (js:make-native-error "TypeError" "not an Event")))))
    (defget ctx ep "type" (this) (evt-type (e this)))
    (defget ctx ep "bubbles" (this) (jbool (evt-bubbles (e this))))
    (defget ctx ep "cancelable" (this) (jbool (evt-cancelable (e this))))
    (defget ctx ep "target" (this) (wrap ctx (evt-target (e this))))
    (defget ctx ep "srcElement" (this) (wrap ctx (evt-target (e this))))
    (defget ctx ep "currentTarget" (this) (wrap ctx (evt-current-target (e this))))
    (defget ctx ep "eventPhase" (this) (num (evt-phase (e this))))
    (defget ctx ep "defaultPrevented" (this) (jbool (evt-default-prevented (e this))))
    (defget ctx ep "detail" (this) (evt-detail (e this)))
    (defget ctx ep "view" (this) (evt-view (e this)))
    (defget ctx ep "isTrusted" (this) (jbool (evt-trusted (e this))))
    (dolist (pair '(("NONE" . 0) ("CAPTURING_PHASE" . 1) ("AT_TARGET" . 2) ("BUBBLING_PHASE" . 3)))
      (js:put ep (car pair) (num (cdr pair)) :enumerable nil :writable nil :configurable nil))
    (defmethod* ctx ep "preventDefault" 0 (this a)
      (let ((ev (e this))) (when (evt-cancelable ev) (setf (evt-default-prevented ev) t)) js:*undefined*))
    (defmethod* ctx ep "stopPropagation" 0 (this a)
      (setf (evt-stopped (e this)) t) js:*undefined*)
    (defmethod* ctx ep "stopImmediatePropagation" 0 (this a)
      (let ((ev (e this))) (setf (evt-stopped ev) t (evt-stop-immediate ev) t) js:*undefined*))
    (defmethod* ctx ep "initEvent" 3 (this a)
      (let ((ev (e this)))
        (setf (evt-type ev) (jstr (arg a 0))
              (evt-bubbles ev) (js:js-truthy (arg a 1))
              (evt-cancelable ev) (js:js-truthy (arg a 2))))
      js:*undefined*)
    (defmethod* ctx ep "initUIEvent" 5 (this a)
      (let ((ev (e this)))
        (setf (evt-type ev) (jstr (arg a 0))
              (evt-bubbles ev) (js:js-truthy (arg a 1))
              (evt-cancelable ev) (js:js-truthy (arg a 2))
              (evt-view ev) (arg a 3)
              (evt-detail ev) (arg a 4)))
      js:*undefined*)
    (defmethod* ctx ep "initCustomEvent" 4 (this a)
      (let ((ev (e this)))
        (setf (evt-type ev) (jstr (arg a 0))
              (evt-bubbles ev) (js:js-truthy (arg a 1))
              (evt-cancelable ev) (js:js-truthy (arg a 2))
              (evt-detail ev) (arg a 3)))
      js:*undefined*)))

;;; ---- EventTarget on Node.prototype ----------------------------------------
(defun node-listeners (ctx node) (gethash node (context-listeners ctx)))
(defun (setf node-listeners) (v ctx node) (setf (gethash node (context-listeners ctx)) v))

(defun add-listener (ctx node type listener capture)
  (pushnew (list type listener capture) (gethash node (context-listeners ctx))
           :test (lambda (a b) (and (string= (first a) (first b))
                                    (eq (second a) (second b))
                                    (eq (third a) (third b))))))

(defun remove-listener (ctx node type listener capture)
  (setf (gethash node (context-listeners ctx))
        (remove-if (lambda (l) (and (string= (first l) type)
                                    (eq (second l) listener)
                                    (eq (third l) capture)))
                   (gethash node (context-listeners ctx)))))

(defun event-path (node)
  "NODE's ancestors from itself up to (and including) the root, in order."
  (loop for p = node then (h:dnode-parent p) while p collect p))

(defun invoke-listeners (ctx node evt-obj ev capture-phase)
  (let ((entries (reverse (gethash node (context-listeners ctx)))))
    (dolist (entry entries)
      (destructuring-bind (type listener capture) entry
        (when (and (string= type (evt-type ev)) (eq (and capture t) capture-phase))
          (setf (evt-current-target ev) node)
          (js:invoke (context-realm ctx)
                     (if (js:js-callable-p listener) listener
                         (js:js-get listener "handleEvent"))
                     (wrap ctx node) (list evt-obj))
          (when (evt-stop-immediate ev) (return)))))))

(defun dispatch-event (ctx node evt-obj)
  (let ((ev (evt-of ctx evt-obj)))
    (unless ev (return-from dispatch-event js:*true*))
    (setf (evt-target ev) node (evt-dispatched ev) t
          (evt-stopped ev) nil (evt-stop-immediate ev) nil)
    (let* ((path (event-path node))            ; node .. root
           (ancestors (rest path)))
      ;; capture: root -> parent
      (setf (evt-phase ev) 1)
      (dolist (a (reverse ancestors))
        (when (evt-stopped ev) (return))
        (invoke-listeners ctx a evt-obj ev t))
      ;; at target
      (unless (evt-stopped ev)
        (setf (evt-phase ev) 2)
        (invoke-listeners ctx node evt-obj ev t)
        (invoke-listeners ctx node evt-obj ev nil))
      ;; bubble: parent -> root
      (when (and (evt-bubbles ev) (not (evt-stopped ev)))
        (setf (evt-phase ev) 3)
        (dolist (a ancestors)
          (when (evt-stopped ev) (return))
          (invoke-listeners ctx a evt-obj ev nil)))
      (setf (evt-phase ev) 0 (evt-current-target ev) nil)
      (jbool (not (evt-default-prevented ev))))))

(defun dispatch-to-window (ctx evt-obj)
  "Invoke WINDOW's own listeners for EVT-OBJ (window is the top of the tree, so
   there is no capture/bubble path — just its target-phase listeners)."
  (let ((window (proto ctx :window)) (ev (evt-of ctx evt-obj)))
    (when (and window ev)
      (setf (evt-target ev) window (evt-dispatched ev) t (evt-phase ev) 2)
      (invoke-listeners ctx window evt-obj ev t)
      (invoke-listeners ctx window evt-obj ev nil)
      (setf (evt-phase ev) 0 (evt-current-target ev) nil))
    (jbool (not (and ev (evt-default-prevented ev))))))

(defun fire-window-event (ctx type)
  "Fire a fresh simple event of TYPE at WINDOW's listeners."
  (dispatch-to-window ctx (make-event-object ctx type nil)))

(defun install-window-events (ctx)
  "Make WINDOW (globalThis) an EventTarget: addEventListener/removeEventListener/
   dispatchEvent keyed by the window object itself.  Real pages register
   load/resize/scroll/DOMContentLoaded here, not on a node."
  (let ((window (proto ctx :window)) (realm (context-realm ctx)))
    (flet ((put (name arity fn) (js:put window name (js:native-function realm name fn arity)
                                        :enumerable nil)))
      (put "addEventListener" 2
           (lambda (this a) (declare (ignore this))
             (let ((l (arg a 1)))
               (when (or (js:js-callable-p l) (js:js-object-p l))
                 (add-listener ctx window (jstr (arg a 0)) l (js:js-truthy (arg a 2)))))
             js:*undefined*))
      (put "removeEventListener" 2
           (lambda (this a) (declare (ignore this))
             (remove-listener ctx window (jstr (arg a 0)) (arg a 1) (js:js-truthy (arg a 2)))
             js:*undefined*))
      (put "dispatchEvent" 1
           (lambda (this a) (declare (ignore this)) (dispatch-to-window ctx (arg a 0)))))))

(defun install-events (ctx np)
  "EventTarget methods onto the Node prototype NP, plus the Event constructors."
  (let ((realm (context-realm ctx)))
    (macrolet ((n (this) `(require-node ctx ,this)))
      (defmethod* ctx np "addEventListener" 3 (this a)
        (let ((l (arg a 1)))
          (when (or (js:js-callable-p l) (js:js-object-p l))
            (add-listener ctx (n this) (jstr (arg a 0)) l (js:js-truthy (arg a 2)))))
        js:*undefined*)
      (defmethod* ctx np "removeEventListener" 3 (this a)
        (remove-listener ctx (n this) (jstr (arg a 0)) (arg a 1) (js:js-truthy (arg a 2)))
        js:*undefined*)
      (defmethod* ctx np "dispatchEvent" 1 (this a)
        (dispatch-event ctx (n this) (arg a 0))))
    ;; Event / CustomEvent / UIEvent constructors.
    (flet ((ctor (name)
             (let ((f (js:native-function realm name
                        (lambda (this args) (declare (ignore this))
                          (make-event-object ctx (jstr (arg args 0)) (arg args 1))) 1)))
               (js:define-global realm name f))))
      (ctor "Event") (ctor "CustomEvent") (ctor "UIEvent"))))
