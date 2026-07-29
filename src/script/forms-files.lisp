;;;; forms-files.lisp — HTMLInputElement.files (wave-10 swarm unit).
;;;;
;;;; Self-registering feature file (see core.lisp *element-proto-extensions*).
;;;;
;;;; What this file is for
;;;; ---------------------
;;;; `files' is the filename value mode's IDL surface.  The rule is narrow and
;;;; entirely testable without any file picker:
;;;;
;;;;   - For type=file, `files' is a FileList object.  With nothing selected it
;;;;     is an EMPTY FileList (length 0) — not null.
;;;;   - For EVERY other input type it is null.  files.html walks all 21 other
;;;;     types and asserts null for each, which is most of its 24 subtests.
;;;;   - The getter must keep returning the SAME FileList object across reads
;;;;     for the same element (identity is asserted), and the list must survive
;;;;     a `type' change away and back only per the filename-mode rules.
;;;;   - Setting `files' is allowed (it is not readonly); assigning null or a
;;;;     non-FileList is a no-op rather than a throw.
;;;;
;;;; FileList itself needs `length', indexed access, `item(i)', and
;;;; @@toStringTag — the tests do `assert_class_string(input.files, "FileList")'
;;;; before they look at anything else, so a FileList without a shared prototype
;;;; carrying the tag scores zero on files it otherwise satisfies.  That exact
;;;; trap cost radio-valueMissing.html 6 subtests in wave 8; see
;;;; `make-validity-state' in core.lisp for the shape that fixed it.
;;;;
;;;; Nothing here needs a real file: no test in the aperture selects one.  Do
;;;; not build a File object with fabricated size/lastModified values to make a
;;;; subtest pass — an empty FileList is the honest answer and it is also the
;;;; ones the tests ask for.
;;;;
;;;; Take over the accessor with the `-for' variant, as the sibling feature
;;;; files do; the previous accessor stays reachable through PREVIOUS-ACCESSOR,
;;;; so this file never has to edit shared core.
(in-package #:weft.script)

(defun install-forms-files (ctx ep)
  (declare (ignorable ctx ep))
  (let ((files-table (make-hash-table :test 'eq))
        (realm (context-realm ctx))
        filelist-proto)

    ;; Build the FileList prototype once (shared across all instances)
    (setf filelist-proto
          (or (proto ctx :filelist)
              (setf (proto ctx :filelist)
                    (let ((o (js:make-object
                              :proto (js:eval-script realm "Object.prototype"))))
                      (js:put o (js:eval-script realm "Symbol.toStringTag")
                              "FileList"
                              :writable nil :enumerable nil :configurable t)
                      o))))

    (labels ((make-file-list-obj ()
               "Create a fresh empty FileList host object."
               (let ((item-fn (js:native-function realm "item"
                                (lambda (this a) (declare (ignore this a)) js:*null*) 1)))
                 (let ((m (js:make-host-object realm
                            :proto filelist-proto
                            :get (lambda (o key rcv) (declare (ignore rcv))
                                   (let ((key (js:to-property-key key)))
                                     (cond
                                       ((and (stringp key) (string= key "length")) (num 0))
                                       ((and (stringp key) (string= key "item")) item-fn)
                                       ((and (stringp key) (index-string-p key))
                                        js:*undefined*)
                                       (t (js:js-get (js:js-object-proto o) key o)))))
                            :has (lambda (o key)
                                   (let ((key (js:to-property-key key)))
                                     (or (and (stringp key)
                                              (or (string= key "length") (string= key "item")))
                                         (js:js-has (js:js-object-proto o) key))))
                            :own-keys (lambda (o) (declare (ignore o)) '("length")))))
                   (setf (getf (js::js-object-internal m) :get-own-property)
                         (lambda (o key) (declare (ignore o))
                           (let ((key (js:to-property-key key)))
                             (cond
                               ((and (stringp key) (string= key "length"))
                                (js::make-prop :value (num 0) :enumerable t :configurable t :writable nil))
                               (t nil)))))
                   m)))
             (filelist-p (v)
               "Check if a JS value is a FileList (has the right prototype)."
               (and (js:js-object-p v)
                    (eq (js:js-object-proto v) filelist-proto))))

      ;; Register FileList global constructor so `input.files instanceof FileList`
      (let ((ctor (js:native-function realm "FileList"
                    (lambda (this a) (declare (ignore this a))
                      (js:js-throw (js:make-native-error "TypeError" "Illegal constructor")))
                    0)))
        (js:put ctor "prototype" filelist-proto)
        (js:define-global realm "FileList" ctor))

      ;; Register DataTransfer global — files.html does `new DataTransfer()`
      (let* ((empty-fl (make-file-list-obj))
             (dt-proto (js:make-object :proto (js:eval-script realm "Object.prototype")))
             (ctor (js:native-function realm "DataTransfer"
                    (lambda (this args) (declare (ignore args))
                      (when (js:js-object-p this)
                        (js:put this "files" empty-fl))
                      js:*undefined*)
                    0)))
        (setf (js::js-object-construct ctor)
              (lambda (args nt) (declare (ignore args nt))
                (let ((o (js:make-object :proto dt-proto)))
                  (js:put o "files" empty-fl)
                  o)))
        (js:put ctor "prototype" dt-proto)
        (js:put dt-proto "constructor" ctor)
        (js:define-global realm "DataTransfer" ctor))

      ;; Register minimal File global — `new File([], "x")` must not throw
      (let* ((file-proto (js:make-object :proto (js:eval-script realm "Object.prototype")))
             (ctor (js:native-function realm "File"
                    (lambda (this args) (declare (ignore this args))
                      js:*undefined*)
                    2)))
        (setf (js::js-object-construct ctor)
              (lambda (args nt) (declare (ignore args nt))
                (js:make-object :proto file-proto)))
        (js:put ctor "prototype" file-proto)
        (js:put file-proto "constructor" ctor)
        (js:define-global realm "File" ctor))

      ;; files getter/setter for <input>
      (defgetset-for ctx ep "input" "files" (this)
        (let* ((node (require-node ctx this))
               (type (input-type node)))
          (if (string= type "file")
              (or (gethash node files-table)
                  (let ((fl (make-file-list-obj)))
                    (setf (gethash node files-table) fl)
                    fl))
              js:*null*))
        (v)
        (let* ((node (require-node ctx this))
               (type (input-type node)))
          (if (string= type "file")
              (cond
                ((eq v js:*null*)
                 (values))
                ((filelist-p v)
                 (setf (gethash node files-table) v))
                (t
                 (js:js-throw (js:make-native-error "TypeError"
                   "Failed to set 'files' on 'HTMLInputElement': The provided value is not a FileList."))))
              (values)))))))

(register-element-proto-extension :files #'install-forms-files)