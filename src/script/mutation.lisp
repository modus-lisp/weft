;;;; src/script/mutation.lisp — MutationObserver (DOM §4.3).
;;;;
;;;; A per-context registry of MutationObservers; each DOM mutation site in
;;;; dom.lisp calls one of the MO-RECORD-* helpers, which walk the inclusive
;;;; ancestor chain of the affected node ("queue a mutation record", DOM §4.3.3),
;;;; enqueue a MutationRecord onto each interested observer's queue, and schedule
;;;; a single mutation-observer microtask on shuttle's microtask queue.  Delivery
;;;; runs at the microtask checkpoint (after the current script, before the next
;;;; task) — NOT via setTimeout.
;;;;
;;;; Fast path: every hook is gated on (CONTEXT-MO-ENABLED *CTX*), which only ever
;;;; flips true when a page calls observe().  A page that never uses
;;;; MutationObserver (Acid2/Acid3/real sites) pays a single boolean test per
;;;; mutation and nothing else.
(in-package #:weft.script)

;;; ---- data model -----------------------------------------------------------
(defstruct mo
  "A live MutationObserver: its JS callback, record queue (newest-first; reversed
   at delivery/takeRecords), the JS wrapper object, and the set of nodes it is
   registered on (for disconnect)."
  callback
  wrapper
  (records nil)
  (nodes nil))

(defstruct mo-reg
  "A registered observer on a node (DOM §registered observer): the MO plus the
   observe() options."
  observer
  options)                              ; plist, see MO-PARSE-OPTIONS

(defun mo-recording-p ()
  "True when the current context has at least one live observer and recording is
   not suppressed (the 'replace all' / 'replace' algorithms coalesce their own
   records and suppress the primitive ones)."
  (and *ctx* (context-mo-enabled *ctx*) (not *mo-suppress*)))

;;; ---- queue a mutation record (DOM §4.3.3) ---------------------------------
(defun mo-queue-record (ctx type target &key added removed prev next
                                              attr-name attr-ns old-value)
  "Queue a mutation record of TYPE for TARGET.  Walks TARGET's inclusive
   ancestors; for each registered observer whose options match (subtree,
   attributeFilter/namespace, characterData, childList) enqueues a per-observer
   MutationRecord, carrying OLD-VALUE only when the observer asked for it, then
   schedules the mutation-observer microtask."
  (let ((interested '()))               ; list of (mo . mapped-old-value)
    (loop for node = target then (h:dnode-parent node)
          while node do
      (dolist (reg (gethash node (context-mo-regs ctx)))
        (let* ((opts (mo-reg-options reg))
               (match (and (or (eq node target) (getf opts :subtree))
                           (cond
                             ((string= type "attributes")
                              (and (getf opts :attributes)
                                   (let ((filt (getf opts :attribute-filter)))
                                     (or (null filt)
                                         (and (null attr-ns)
                                              (member attr-name filt :test #'string=))))))
                             ((string= type "characterData") (getf opts :character-data))
                             ((string= type "childList") (getf opts :child-list))
                             (t nil)))))
          (when match
            (let* ((mo (mo-reg-observer reg))
                   (cell (assoc mo interested :test #'eq)))
              (unless cell
                (setf cell (cons mo nil))
                (push cell interested))
              (when (or (and (string= type "attributes") (getf opts :attribute-old-value))
                        (and (string= type "characterData") (getf opts :character-data-old-value)))
                (setf (cdr cell) old-value)))))))
    (when interested
      (dolist (cell interested)
        (push (list :type type :target target :added added :removed removed
                    :prev prev :next next :attr-name attr-name :attr-ns attr-ns
                    :old-value (cdr cell))
              (mo-records (car cell))))
      (mo-queue-microtask ctx))))

;;; ---- typed helpers called from the dom.lisp mutation sites ----------------
(defun mo-record-childlist (parent &key added removed prev next)
  (when (and (mo-recording-p) parent)
    (mo-queue-record *ctx* "childList" parent
                     :added added :removed removed :prev prev :next next)))

(defun mo-record-attr (ctx el local ns old-value)
  (when (and *ctx* (context-mo-enabled *ctx*) (not *mo-suppress*))
    (mo-queue-record ctx "attributes" el
                     :attr-name local :attr-ns ns :old-value old-value)))

(defun mo-record-chardata (node old-value)
  (when (mo-recording-p)
    (mo-queue-record *ctx* "characterData" node :old-value old-value)))

;;; ---- microtask delivery (DOM §"notify mutation observers") ----------------
(defun mo-queue-microtask (ctx)
  (unless (context-mo-microtask-queued ctx)
    (setf (context-mo-microtask-queued ctx) t)
    (js::enqueue-microtask (lambda () (mo-notify ctx)))))

(defun mo-notify (ctx)
  (setf (context-mo-microtask-queued ctx) nil)
  (let ((*ctx* ctx))
    (dolist (mo (reverse (context-mo-list ctx)))
      (let ((records (nreverse (mo-records mo))))
        (setf (mo-records mo) nil)
        (when records
          (let ((arr (mo-records->js ctx records)))
            (handler-case
                (js:invoke (context-realm ctx) (mo-callback mo)
                           (mo-wrapper mo) (list arr (mo-wrapper mo)))
              (js:shuttle-error () nil)
              (serious-condition () nil))))))))

;;; ---- MutationRecord JS objects --------------------------------------------
(defun mo-static-nodelist (ctx nodes)
  "A snapshot NodeList over NODES (addedNodes/removedNodes are static)."
  (let ((snap (copy-list nodes)))
    (make-collection ctx (lambda () snap) nil :nodelist)))

(defun mo-record->js (ctx rec)
  (let ((obj (js:make-object :proto (proto ctx :mutationrecord))))
    (flet ((p (k v) (js:put obj k v :enumerable t :writable nil :configurable t)))
      (p "type" (getf rec :type))
      (p "target" (wrap ctx (getf rec :target)))
      (p "addedNodes" (mo-static-nodelist ctx (getf rec :added)))
      (p "removedNodes" (mo-static-nodelist ctx (getf rec :removed)))
      (p "previousSibling" (wrap ctx (getf rec :prev)))
      (p "nextSibling" (wrap ctx (getf rec :next)))
      (p "attributeName" (opt (getf rec :attr-name)))
      (p "attributeNamespace" (opt (getf rec :attr-ns)))
      (p "oldValue" (opt (getf rec :old-value))))
    obj))

(defun mo-records->js (ctx records)
  (js::make-array-object (mapcar (lambda (r) (mo-record->js ctx r)) records)))

;;; ---- observe() option parsing ---------------------------------------------
(defun mo-read-filter (opts)
  (let ((arr (js:js-get opts "attributeFilter")))
    (when (js:js-object-p arr)
      (let ((len (js-int (js:js-get arr "length"))))
        (loop for i below len collect (jstr (js:js-get arr (princ-to-string i))))))))

(defun mo-parse-options (ctx opts)
  "Parse+validate an observe() init dictionary (DOM §MutationObserver.observe),
   returning the options plist.  Throws a JS TypeError on the invalid combos."
  (declare (ignore ctx))
  (let* ((child-list (truthy (js:js-get opts "childList")))
         (subtree (truthy (js:js-get opts "subtree")))
         (attrs-present (js:js-has opts "attributes"))
         (attributes (truthy (js:js-get opts "attributes")))
         (cdata-present (js:js-has opts "characterData"))
         (character-data (truthy (js:js-get opts "characterData")))
         (attr-old-present (js:js-has opts "attributeOldValue"))
         (attribute-old-value (truthy (js:js-get opts "attributeOldValue")))
         (cdata-old-present (js:js-has opts "characterDataOldValue"))
         (character-data-old-value (truthy (js:js-get opts "characterDataOldValue")))
         (filter-present (js:js-has opts "attributeFilter"))
         (attribute-filter (and filter-present (mo-read-filter opts))))
    ;; imply attributes/characterData when only their *OldValue/Filter is given
    (when (and (or attr-old-present filter-present) (not attrs-present))
      (setf attributes t))
    (when (and cdata-old-present (not cdata-present))
      (setf character-data t))
    (flet ((type-error (msg)
             (js:js-throw (js:make-native-error "TypeError" msg))))
      (unless (or child-list attributes character-data)
        (type-error "MutationObserver.observe: need childList, attributes or characterData"))
      (when (and attribute-old-value (not attributes))
        (type-error "MutationObserver.observe: attributeOldValue requires attributes"))
      (when (and filter-present (not attributes))
        (type-error "MutationObserver.observe: attributeFilter requires attributes"))
      (when (and character-data-old-value (not character-data))
        (type-error "MutationObserver.observe: characterDataOldValue requires characterData")))
    (list :child-list child-list :attributes attributes :character-data character-data
          :subtree subtree :attribute-old-value attribute-old-value
          :character-data-old-value character-data-old-value
          :attribute-filter attribute-filter)))

;;; ---- prototype + constructor installation ---------------------------------
(defun mo-of (ctx this)
  (or (gethash this (context-mo-objs ctx))
      (js:js-throw (js:make-native-error "TypeError" "not a MutationObserver"))))

(defun install-mutation-observer (ctx)
  "Install MutationObserver + MutationRecord (DOM §4.3) into CTX's realm."
  (let* ((realm (context-realm ctx))
         (op (js:eval-script realm "Object.prototype"))
         (mop (js:make-object :proto op))   ; MutationObserver.prototype
         (mrp (js:make-object :proto op)))  ; MutationRecord.prototype
    (setf (proto ctx :mutationobserver) mop
          (proto ctx :mutationrecord) mrp)
    ;; observe(target, options)
    (defmethod* ctx mop "observe" 2 (this a)
      (let* ((mo (mo-of ctx this))
             (target (require-node ctx (arg a 0)))
             (options (mo-parse-options ctx (arg a 1))))
        (setf (context-mo-enabled ctx) t)
        (let ((existing (find mo (gethash target (context-mo-regs ctx))
                              :key #'mo-reg-observer :test #'eq)))
          (if existing
              (setf (mo-reg-options existing) options)
              (progn
                (push (make-mo-reg :observer mo :options options)
                      (gethash target (context-mo-regs ctx)))
                (pushnew target (mo-nodes mo) :test #'eq))))
        js:*undefined*))
    ;; disconnect(): drop every registration + the pending record queue.
    (defmethod* ctx mop "disconnect" 0 (this a) (declare (ignore a))
      (let ((mo (mo-of ctx this)))
        (dolist (node (mo-nodes mo))
          (setf (gethash node (context-mo-regs ctx))
                (remove mo (gethash node (context-mo-regs ctx))
                        :key #'mo-reg-observer :test #'eq)))
        (setf (mo-nodes mo) nil
              (mo-records mo) nil)
        js:*undefined*))
    ;; takeRecords(): return + clear the queued records.
    (defmethod* ctx mop "takeRecords" 0 (this a) (declare (ignore a))
      (let* ((mo (mo-of ctx this))
             (records (nreverse (mo-records mo))))
        (setf (mo-records mo) nil)
        (mo-records->js ctx records)))
    ;; The MutationObserver constructor.  WebIDL interface objects are called with
    ;; `new`, so the work lives in [[Construct]]; a plain call throws.
    (let ((ctor (js:native-function realm "MutationObserver"
                  (lambda (this args) (declare (ignore this args))
                    (js:js-throw (js:make-native-error
                                  "TypeError" "Constructor MutationObserver requires 'new'")))
                  1)))
      (flet ((build (args)
               (let ((cb (arg args 0)))
                 (unless (js:js-callable-p cb)
                   (js:js-throw (js:make-native-error
                                 "TypeError" "MutationObserver: callback is not a function")))
                 (let* ((obj (js:make-object :proto mop))
                        (mo (make-mo :callback cb :wrapper obj)))
                   (setf (gethash obj (context-mo-objs ctx)) mo)
                   (push mo (context-mo-list ctx))
                   obj))))
        (setf (js::js-object-construct ctor)
              (lambda (args new-target) (declare (ignore new-target)) (build args))))
      (js:put ctor "prototype" mop :enumerable nil :writable nil :configurable nil)
      (js:put mop "constructor" ctor :enumerable nil :writable t :configurable t)
      (js:define-global realm "MutationObserver" ctor)
      ;; WebKitMutationObserver legacy alias.
      (js:define-global realm "WebKitMutationObserver" ctor))
    ;; A bare MutationRecord constructor object so `rec instanceof MutationRecord`
    ;; holds (the interface is not constructible, but the tests reference it).
    (let ((mrctor (js:native-function realm "MutationRecord"
                    (lambda (this args) (declare (ignore this args))
                      (js:js-throw (js:make-native-error
                                    "TypeError" "Illegal constructor"))) 0)))
      (js:put mrctor "prototype" mrp :enumerable nil :writable nil :configurable nil)
      (js:put mrp "constructor" mrctor :enumerable nil :writable t :configurable t)
      (js:define-global realm "MutationRecord" mrctor))))
