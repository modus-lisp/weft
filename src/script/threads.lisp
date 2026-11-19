;;;; threads.lisp -- a whole page (DOM, styles, layout, paint, scripts) on a thread of its own.
;;;;
;;;; weft keeps its caches and registries in globals.  On SBCL a second thread rendering
;;;; merely races on them; on modus, whose threads share no state, a worker's first store into
;;;; one is refused.  WITH-THREAD-STATE gives BODY its own binding of each: registries filled at
;;;; load (CSS value parsers, decoders, DOM extensions, registered faces) are COPIED so the
;;;; thread sees them, caches start EMPTY and refill, and plain values start as the caller's.
;;;; It includes shuttle's and scribe's thread states, so a thread that owns a page needs only
;;;; this.  Names resolve when it runs, so weft/fetch stays optional.

(in-package #:weft.script)

(defparameter *thread-state-vars*
  '(;; registries: copied
    ("WEFT.CSS" "*VALUE-PARSERS*" :copy) ("WEFT.ENCODING" "*DECODERS*" :copy)
    ("WEFT.SCRIPT" "*ELEMENT-EXOTICS*" :copy) ("WEFT.SCRIPT" "*ELEMENT-PROTO-EXTENSIONS*" :copy)
    ("WEFT.RENDER" "*REGISTERED-FACES*" :copy)
    ;; loaded faces: copied, so a page does not parse its fonts again (they are only read)
    ("WEFT.RENDER" "*FACE-CACHE*" :copy) ("WEFT.RENDER" "*FONT-FACE-CACHE*" :copy)
    ;; caches: empty
    ("WEFT.RENDER" "*GLYPH-CACHE*" :fresh) ("WEFT.RENDER" "*SHAPE-CACHE*" :fresh)
    ("WEFT.RENDER" "*SEGMENT-CACHE*" :fresh) ("WEFT.RENDER" "*IMAGE-STORE*" :fresh)
    ("WEFT.RENDER" "*ELEMENT-CANVAS*" :fresh) ("WEFT.SCRIPT" "*FORM-PAST-NAMES*" :fresh)
    ("WEFT.FETCH" "*CONN-POOL*" :fresh)
    ;; values
    ("WEFT.CSS" "*ANCESTOR-BLOOM*" :value) ("WEFT.CSS" "*ANON-LAYER-COUNTER*" :value)
    ("WEFT.CSS" "*EL-CLASSES-CACHE*" :value) ("WEFT.CSS" "*LAYER-ORDER*" :value)
    ("WEFT.SCRIPT" "*CTX*" :value) ("WEFT.RENDER" "*HYPHEN-EXC-EN*" :value)
    ("WEFT.RENDER" "*HYPHEN-TRIE-EN*" :value) ("WEFT.RENDER" "*INTRINSIC-CACHE*" :value)
    ;; the cookie jar: the thread's own (a page worker is a browsing session)
    ("WEFT.FETCH" "*COOKIE-JAR*" :cookie-jar))
  "(package name kind) for every weft global a page STORES into.  A new one belongs here.")

(defun %thread-state-start (kind sym)
  (let ((v (and (boundp sym) (symbol-value sym))))
    (ecase kind
      (:value v)
      (:copy (if (hash-table-p v)
                 (let ((h (make-hash-table :test (hash-table-test v) :size (max 16 (hash-table-count v)))))
                   (maphash (lambda (k x) (setf (gethash k h) x)) v)
                   h)
                 v))
      (:fresh (if (hash-table-p v) (make-hash-table :test (hash-table-test v)) v))
      (:cookie-jar (funcall (find-symbol "MAKE-COOKIE-JAR" "WEFT.FETCH"))))))

(defun %thread-state-bindings ()
  "(values symbols starting-values) for the weft globals present in this image."
  (let ((syms '()) (vals '()))
    (dolist (e *thread-state-vars*)
      (destructuring-bind (pkg name kind) e
        (let ((sym (and (find-package pkg) (find-symbol name pkg))))
          (when (and sym (boundp sym))
            (push sym syms)
            (push (%thread-state-start kind sym) vals)))))
    (values syms vals)))

(defmacro with-thread-state (() &body body)
  "BODY with weft's globals -- and shuttle's and scribe's -- bound for this thread alone, so it
   can load, lay out, paint and script a page on a thread that may not store into shared memory
   (a modus worker)."
  (let ((syms (gensym)) (vals (gensym)))
    `(multiple-value-bind (,syms ,vals) (%thread-state-bindings)
       (progv ,syms ,vals
         (shuttle:with-thread-state ()
           (scribe:with-thread-state ()
             ,@body))))))

(export 'with-thread-state)
