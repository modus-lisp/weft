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
    ;; Legacy alias for the stop-propagation flag (DOM §Event cancelBubble).
    (defgetset ctx ep "cancelBubble" (this) (jbool (evt-stopped (e this)))
      (v) (when (js:js-truthy v) (setf (evt-stopped (e this)) t)))
    ;; Legacy alias for the canceled flag, inverted (DOM §Event returnValue).
    (defgetset ctx ep "returnValue" (this) (jbool (not (evt-default-prevented (e this))))
      (v) (let ((ev (e this)))
            (when (and (not (js:js-truthy v)) (evt-cancelable ev))
              (setf (evt-default-prevented ev) t))))
    (defmethod* ctx ep "initEvent" 3 (this a)
      (let ((ev (e this)))
        (setf (evt-type ev) (jstr (arg a 0))
              (evt-bubbles ev) (js:js-truthy (arg a 1))
              (evt-cancelable ev) (js:js-truthy (arg a 2))
              ;; initialize the flags (DOM §Event initialize): un-stop, un-cancel.
              (evt-stopped ev) nil (evt-stop-immediate ev) nil
              (evt-default-prevented ev) nil))
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

(defvar *label-activating* nil)

;;; ---- activation behaviour (HTML §interactive-elements / activation) --------
;;; A click on a form control does more than fire an event: a checkbox toggles,
;;; a radio takes its group, a submit button submits.  HTML splits this into
;;; three steps around the dispatch — PRE-click activation (which runs BEFORE
;;; the event, so a handler reading `this.checked' sees the new value), the
;;; activation behaviour proper (only if nothing called preventDefault), and the
;;; canceled steps (which undo the pre-click change if it did).
;;;
;;; This lives here, once, because there are two callers: the `click()' method
;;; and a TRUSTED click arriving from the shell (see INTERACT).  A private copy
;;; per caller is exactly how the weft-checked bug in FORMS-CONSTRAINTS got in.
(declaim (ftype function input-type checked-p set-checked set-attr remove-attr
                         element-form-owner tree-root form-fire-submit
                         node-disabled-p get-attr
                         ;; EDITING is compiled after the IDL files (it needs
                         ;; them), but focus/blur here and in DOM must commit a
                         ;; pending edit before the blur event.
                         commit-edit))

(defun fire-at (ctx node type &key (bubbles t) (cancelable nil))
  "Fire a plain event of TYPE at NODE.  Returns the event record."
  (let* ((obj (make-event-object ctx type nil)) (ev (evt-of ctx obj)))
    (setf (evt-bubbles ev) bubbles (evt-cancelable ev) cancelable (evt-trusted ev) t)
    (dispatch-event ctx node obj)
    ev))

(defun activation-target (node)
  "The element a click on NODE activates: NODE itself or the nearest ancestor
   with activation behaviour, so clicking the text inside a <button> (or the
   image in an <input type=image>) activates the control and not the text node.
   NIL when nothing in the ancestor chain activates, or the control is disabled."
  (loop for a = node then (h:dnode-parent a)
        while a
        when (and (eq (h:dnode-kind a) :element)
                  (member (h:dnode-name a) '("input" "button") :test #'string=))
          return (unless (dom:has-attribute a "disabled") a)))

(defun radio-group-checked (node)
  "The currently-checked member of NODE's radio group, or NIL."
  (let ((name (dom:get-attribute node "name")) (root (tree-root node)))
    (find-if (lambda (o) (and (string= (input-type o) "radio")
                              (equal (dom:get-attribute o "name") name)
                              (checked-p o)))
             (dom:get-elements-by-tag-name root "input"))))

(defun run-pre-click-activation (ctx target)
  "HTML's pre-click activation steps: flip the control's state BEFORE the click
   event is dispatched, so listeners observe what the user just did.  Returns a
   token for RUN-CANCELED-ACTIVATION."
  (let ((type (input-type target)))
    (cond ((string= type "checkbox")
           (let ((old (checked-p target)))
             (set-checked ctx target (not old))
             (list :checkbox old)))
          ((string= type "radio")
           ;; remember which member HELD the group, so a cancelled click puts it
           ;; back rather than leaving the whole group unchecked
           (let ((old (radio-group-checked target)))
             (set-checked ctx target t)
             (list :radio old)))
          (t nil))))

(defun run-canceled-activation (ctx target state)
  "Undo RUN-PRE-CLICK-ACTIVATION after a listener called preventDefault()."
  (case (first state)
    (:checkbox (set-checked ctx target (second state)))
    (:radio (let ((old (second state)))
              (set-attr target "weft-checked" "0")
              (when old (set-checked ctx old t))
              (setf (context-dirty ctx) t)))))

(defun run-form-reset (ctx form)
  "Reset FORM: fire a cancelable `reset' event and, unless cancelled, drop every
   control's dirty value/checkedness so each falls back to its default."
  (when (and form (not (evt-default-prevented (fire-at ctx form "reset" :cancelable t))))
    (dolist (tag '("input" "textarea" "select"))
      (dolist (el (dom:get-elements-by-tag-name form tag))
        (remhash el (context-input-values ctx))
        (when (member (input-type el) '("checkbox" "radio") :test #'string=)
          (remove-attr el "weft-checked"))))
    (setf (context-dirty ctx) t)
    t))

(defun run-activation-behaviour (ctx target)
  "The control's default action, run only when the click was not cancelled."
  (let ((tag (h:dnode-name target)) (type (input-type target)))
    (cond
      ;; a toggled checkbox/radio reports the change to the page
      ((member type '("checkbox" "radio") :test #'string=)
       (fire-at ctx target "input")
       (fire-at ctx target "change"))
      ((or (and (string= tag "input") (member type '("submit" "image") :test #'string=))
           (and (string= tag "button") (member type '("submit" "") :test #'string=)))
       (let ((form (element-form-owner ctx target)))
         (when form (form-fire-submit ctx form target))))
      ((or (and (string= tag "input") (string= type "reset"))
           (and (string= tag "button") (string= type "reset")))
       (run-form-reset ctx (element-form-owner ctx target))))))

;;; ---- focus (HTML §focus) ---------------------------------------------------
;;; Nothing here knew what was focused, so keyboard events had nowhere to go but
;;; the body and `document.activeElement' did not exist.  One slot on the context
;;; plus HTML's focus update steps is the whole model.

(defun body-element (ctx)
  (let ((doc (context-document ctx)))
    (and doc (or (first (dom:get-elements-by-tag-name doc "body"))
                 (first (dom:get-elements-by-tag-name doc "html"))))))

(defun focus-still-valid-p (ctx node)
  "True while NODE can still hold focus: it is an element, it is not disabled
   (directly or by a <fieldset> ancestor), and it is still in the document."
  (and node (eq (h:dnode-kind node) :element)
       (not (node-disabled-p node))
       (eq (tree-root node) (context-document ctx))))

(defun active-element (ctx)
  "The focused element — `document.activeElement', which is the body when
   nothing else holds focus.

   HTML's focus fixup runs here: an element that becomes disabled, or that is
   taken out of the document, loses focus.  It is checked on read because the
   mutation that invalidates it happens somewhere else entirely — disabling a
   <fieldset> disables everything nested inside it, and nothing tells us."
  (let ((f (context-focus ctx)))
    (unless (or (null f) (focus-still-valid-p ctx f))
      (setf (context-focus ctx) nil (context-edit-start ctx) nil f nil))
    (or f (body-element ctx))))

(defun set-focus (ctx node)
  "Move focus to NODE (NIL = back to the body), running HTML's focus update
   steps: blur then focusout at the element losing focus, focus then focusin at
   the one gaining it.  Committing a pending edit is the caller's job (see
   COMMIT-EDIT in EDITING) — it must happen before the blur event."
  (let ((old (context-focus ctx)))
    ;; focus() on something that cannot be focused (disabled, detached) is a
    ;; no-op, not a way to park focus on it.
    (when (and node (not (focus-still-valid-p ctx node)))
      (return-from set-focus nil))
    (unless (eq old node)
      ;; The selection is NOT reset here: it lives per node and survives losing
      ;; focus, which is what `input.selectionStart' reading back what you left
      ;; there depends on.
      (setf (context-focus ctx) node)
      (when old
        (fire-at ctx old "blur" :bubbles nil)
        (fire-at ctx old "focusout"))
      (when node
        (fire-at ctx node "focus" :bubbles nil)
        (fire-at ctx node "focusin"))
      (setf (context-dirty ctx) t))
    node))

;;; ---- sequential focus navigation (HTML §focus, the Tab order) --------------

(defun tabindex-of (node)
  "NODE's tabindex as an integer, or NIL when it has none (or an unparseable
   one, which HTML says to ignore rather than treat as 0)."
  (let ((v (get-attr node "tabindex")))
    (and v (ignore-errors (parse-integer (string-trim '(#\Space #\Tab #\Newline) v))))))

(defun sequentially-focusable-p (node)
  "True when Tab should be able to land on NODE: something focusable by default
   (an enabled form control, or a link with an href) or made focusable by a
   tabindex — but never one with a NEGATIVE tabindex, which is HTML's way of
   saying \"focusable by script and by click, but skipped by the keyboard\"."
  (and (eq (h:dnode-kind node) :element)
       (let ((ti (tabindex-of node)))
         (and (not (and ti (minusp ti)))
              (or ti
                  (and (not (node-disabled-p node))
                       (let ((tag (string-downcase (h:dnode-name node))))
                         (or (member tag '("button" "select" "textarea") :test #'string=)
                             (and (string= tag "input")
                                  (not (string-equal (or (input-type node) "") "hidden")))
                             (and (string= tag "a") (get-attr node "href"))))))))))

(defun focus-order (ctx)
  "Every sequentially focusable element, in HTML's tab order: the positive
   tabindexes first in ascending order, then everything else in tree order.
   Ties inside a tabindex value keep tree order, so the walk has to be stable."
  (let ((doc (context-document ctx)) (found '()))
    (labels ((walk (n)
               (when (eq (h:dnode-kind n) :element)
                 (when (sequentially-focusable-p n) (push n found)))
               (map nil #'walk (h:dnode-children n))))
      (when doc (walk doc)))
    (let ((in-order (nreverse found)))
      (stable-sort in-order #'<
                   :key (lambda (n)
                          (let ((ti (tabindex-of n)))
                            ;; positives sort among themselves; 0/absent all
                            ;; land in one bucket after them
                            (if (and ti (plusp ti)) ti most-positive-fixnum)))))))

(defun focus-navigate (ctx forward)
  "Move focus one step along the tab order (FORWARD nil = Shift-Tab) and return
   the element that got it, or NIL when there is nothing focusable.  Wraps at
   either end — a browser would go to the chrome instead, but a shell with no
   chrome to reach has nowhere else to send it."
  (let* ((order (focus-order ctx)))
    (when order
      (let* ((cur (context-focus ctx))
             (pos (position cur order))
             (n (length order))
             (next (cond ((null pos) (if forward 0 (1- n)))
                         (forward (mod (1+ pos) n))
                         (t (mod (1- pos) n)))))
        (set-focus ctx (nth next order))
        (nth next order)))))

(defun activate-on-click (ctx node thunk)
  "Run THUNK (which dispatches the click event) wrapped in NODE's activation
   behaviour.  Returns what THUNK returned."
  (let* ((target (activation-target node))
         (state (and target (run-pre-click-activation ctx target)))
         (result (funcall thunk)))
    (when target
      (if (js:js-truthy result)
          (run-activation-behaviour ctx target)
          (when state (run-canceled-activation ctx target state))))
    result))

(defun dispatch-event (ctx node evt-obj)
  (let ((ev (evt-of ctx evt-obj))
        (*ctx* ctx))
    (unless ev (return-from dispatch-event js:*true*))
    (setf (evt-target ev) node (evt-dispatched ev) t
          (evt-stopped ev) nil (evt-stop-immediate ev) nil)
    (let* ((path (event-path node))            ; node .. root
           (ancestors (rest path))
           (window (proto ctx :window)))
      ;; capture: root -> parent
      (setf (evt-phase ev) 1)
      ;; Window capture phase (before document)
      (when window
        (invoke-listeners ctx window evt-obj ev t))
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
          (invoke-listeners ctx a evt-obj ev nil))
        ;; Window bubble phase (after document)
        (when (and window (not (evt-stopped ev)))
          (invoke-listeners ctx window evt-obj ev nil)))
      (setf (evt-phase ev) 0 (evt-current-target ev) nil)
      ;; Label click activation default action: if a click event hasn't been
      ;; cancelled and the target is inside a <label>, fire a click at the
      ;; label's labeled control.
      (when (and (string= (evt-type ev) "click")
                 (not (evt-default-prevented ev))
                 (not *label-activating*))
        (let ((*label-activating* t))
          (loop for a = node then (h:dnode-parent a)
                while a
                when (and (eq (h:dnode-kind a) :element)
                          (string= (h:dnode-name a) "label"))
                do (let ((ctl (labels-label-control a)))
                     (when (and ctl (not (eq ctl node)))
                       (let ((ctrl-wrap (wrap ctx ctl)))
                         (js:invoke (context-realm ctx)
                                    (js:js-get ctrl-wrap "click")
                                    ctrl-wrap nil))))
                   (return t))))
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
    ;; All event wrappers share one Event.prototype (EVP); pointing every
    ;; constructor's .prototype at it makes `e instanceof Event` (and instanceof
    ;; the specific interface) hold for constructed events (WebIDL §interface-object).
    (flet ((ctor (name)
             (let ((f (js:native-function realm name
                        (lambda (this args) (declare (ignore this))
                          (make-event-object ctx (jstr (arg args 0)) (arg args 1))) 1)))
               (js:put f "prototype" (proto ctx :event) :enumerable nil :writable nil)
               (js:define-global realm name f))))
      (ctor "Event") (ctor "CustomEvent") (ctor "UIEvent")
      (ctor "MouseEvent") (ctor "KeyboardEvent"))))
