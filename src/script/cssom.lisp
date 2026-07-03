;;;; src/script/cssom.lisp — the CSSOM surface: getComputedStyle + element.style.
;;;;
;;;; weft already resolves the cascade into cstyle structs; getComputedStyle
;;;; hands those back to script as a read-only style object keyed by camelCase
;;;; property. element.style is a small live inline-declaration object that
;;;; writes back through the element's `style` attribute so it re-cascades.
(in-package #:weft.script)

;;; ---- value formatting -----------------------------------------------------
(defun px (v)
  (cond ((eq v :auto) "auto") ((eq v :none) "none") ((eq v :normal) "normal")
        ((numberp v) (format nil "~apx" (if (= v (truncate v)) (truncate v) v)))
        ((stringp v) v) ((null v) "") (t (princ-to-string v))))

(defun rgb-str (c)
  (if (and (consp c) (>= (length c) 3))
      (destructuring-bind (r g b &optional (a 1.0)) c
        (if (and a (< a 1))
            (format nil "rgba(~a, ~a, ~a, ~a)" (round r) (round g) (round b) a)
            (format nil "rgb(~a, ~a, ~a)" (round r) (round g) (round b))))
      ""))

(defun computed-prop (cs dashed)
  "A JS string for computed property DASHED off cstyle CS."
  (macrolet ((s (accessor) `(,accessor cs)))
    (cond
      ((string= dashed "display") (s css:cstyle-display))
      ((string= dashed "white-space") (s css:cstyle-white-space))
      ((string= dashed "position") (s css:cstyle-position))
      ((string= dashed "float") (s css:cstyle-float))
      ((string= dashed "clear") (s css:cstyle-clear))
      ((string= dashed "overflow") (s css:cstyle-overflow))
      ((string= dashed "text-align") (s css:cstyle-text-align))
      ((string= dashed "cursor") (s css:cstyle-cursor))
      ((string= dashed "text-transform") (s css:cstyle-text-transform))
      ((string= dashed "box-sizing") (s css:cstyle-box-sizing))
      ((string= dashed "font-style") (s css:cstyle-font-style))
      ((string= dashed "z-index") (let ((z (s css:cstyle-z-index)))
                                    (if (integerp z) (princ-to-string z) (px z))))
      ((string= dashed "font-weight") (princ-to-string (s css:cstyle-font-weight)))
      ((string= dashed "font-size") (px (s css:cstyle-font-size)))
      ((string= dashed "line-height") (px (s css:cstyle-line-height)))
      ((string= dashed "color") (rgb-str (s css:cstyle-color)))
      ((string= dashed "width") (px (s css:cstyle-width)))
      ((string= dashed "height") (px (s css:cstyle-height)))
      ((string= dashed "min-width") (px (s css:cstyle-min-width)))
      ((string= dashed "max-width") (px (s css:cstyle-max-width)))
      ((string= dashed "min-height") (px (s css:cstyle-min-height)))
      ((string= dashed "max-height") (px (s css:cstyle-max-height)))
      ((string= dashed "margin-top") (px (s css:cstyle-margin-top)))
      ((string= dashed "margin-right") (px (s css:cstyle-margin-right)))
      ((string= dashed "margin-bottom") (px (s css:cstyle-margin-bottom)))
      ((string= dashed "margin-left") (px (s css:cstyle-margin-left)))
      ((string= dashed "padding-top") (px (s css:cstyle-padding-top)))
      ((string= dashed "padding-right") (px (s css:cstyle-padding-right)))
      ((string= dashed "padding-bottom") (px (s css:cstyle-padding-bottom)))
      ((string= dashed "padding-left") (px (s css:cstyle-padding-left)))
      ((string= dashed "top") (px (s css:cstyle-top)))
      ((string= dashed "left") (px (s css:cstyle-left)))
      ((string= dashed "right") (px (s css:cstyle-right)))
      ((string= dashed "bottom") (px (s css:cstyle-bottom)))
      (t ""))))

(defun prop->dashed (key)
  (cond ((string= key "cssFloat") "float")
        ((string= key "float") "float")
        (t (camel->dash key))))

;;; ---- getComputedStyle -----------------------------------------------------
(defun %inline-length-px (style prop)
  "The pixel value of PROP in inline STYLE (e.g. width: 100px -> 100.0), or NIL."
  (let ((cell (assoc prop (parse-inline-style style) :test #'string=)))
    (when cell
      (let* ((v (cdr cell))
             (end (position-if-not (lambda (c) (or (digit-char-p c) (member c '(#\. #\-)))) v)))
        (ignore-errors (float (read-from-string (subseq v 0 (or end (length v))))))))))

(defun frame-viewport (ctx doc)
  "The (width height) media viewport of a subframe DOC, from its owning iframe/
   object element's inline size; 0x0 when unknown."
  (let ((el (block found
              (maphash (lambda (k v) (when (eq v doc) (return-from found k)))
                       (context-iframe-docs ctx))
              nil)))
    (if el
        (let ((style (or (get-attr el "style") "")))
          (values (or (%inline-length-px style "width") 0.0)
                  (or (%inline-length-px style "height") 0.0)))
        (values 0.0 0.0))))

(defun document-styles (ctx doc)
  "The computed-style hash for document DOC (cached; recomputed after a
   DOM/attr mutation marks the context dirty)."
  (when (context-dirty ctx)
    (clrhash (context-styles ctx))
    (setf (context-dirty ctx) nil))
  (or (gethash doc (context-styles ctx))
      ;; @media evaluates against the document's viewport: the layout width for
      ;; the top document, and a subframe document's owning iframe/object box.
      ;; The prelude is evaluated while the sheet is parsed, so bind the viewport
      ;; around parsing too.
      (multiple-value-bind (fw fh) (frame-viewport ctx doc)
       (let ((css::*viewport-w* (if (eq doc (context-document ctx)) (float (context-width ctx)) fw))
             (css::*viewport-h* (if (eq doc (context-document ctx)) 600.0 fh)))
        (let ((sheet (css:parse-stylesheet
                      (concatenate 'string
                                   (if (eq doc (context-document ctx)) (or (context-css ctx) "") "")
                                   (string #\Newline)
                                   (weft.render::collect-stylesheets doc)))))
          (setf (gethash doc (context-styles ctx)) (css:compute-styles doc sheet)))))))

(defun owner-document (node)
  (loop for p = node then (h:dnode-parent p)
        while p when (eq (h:dnode-kind p) :document) return p))

(defun computed-style-object (ctx node)
  (let* ((realm (context-realm ctx))
         (getprop (js:native-function realm "getPropertyValue"
                    (lambda (this a) (declare (ignore this))
                      (let* ((doc (owner-document node)) (cs (and doc (gethash node (document-styles ctx doc)))))
                        (if cs (computed-prop cs (string-downcase (jstr (arg a 0)))) "")))
                    1)))
    (js:make-host-object realm
      :get (lambda (o key rcv) (declare (ignore rcv))
             (setf key (js:to-property-key key))
             (cond
               ((not (stringp key)) (js:js-get (js:js-object-proto o) key o))
               ((string= key "getPropertyValue") getprop)
               (t (let* ((doc (owner-document node))
                         (cs (and doc (gethash node (document-styles ctx doc)))))
                    (if cs (computed-prop cs (prop->dashed key))
                        (js:js-get (js:js-object-proto o) key o)))))))))

;;; ---- element.style (inline declarations) ----------------------------------
(defun parse-inline-style (str)
  "STR like \"a: b; c: d\" -> alist of (dashed-name . value)."
  (let ((out '()))
    (dolist (decl (uiop:split-string (or str "") :separator ";") (nreverse out))
      (let ((c (position #\: decl)))
        (when c
          (let ((k (string-downcase (string-trim " " (subseq decl 0 c))))
                (v (string-trim " " (subseq decl (1+ c)))))
            (when (plusp (length k)) (push (cons k v) out))))))))

(defun serialize-inline-style (alist)
  (with-output-to-string (o)
    (loop for (k . v) in alist for first = t then nil
          do (unless first (write-string " " o))
             (format o "~a: ~a;" k v))))

(defun element-style-object (ctx element)
  (let ((realm (context-realm ctx)))
    (flet ((decls () (parse-inline-style (get-attr element "style")))
           (store (alist)
             (set-attr element "style" (serialize-inline-style alist))
             (setf (context-dirty ctx) t)))
      (js:make-host-object realm
        :get (lambda (o key rcv) (declare (ignore rcv))
               (setf key (js:to-property-key key))
               (cond
                 ((not (stringp key)) (js:js-get (js:js-object-proto o) key o))
                 ((string= key "cssText") (or (get-attr element "style") ""))
                 (t (let ((cell (assoc (prop->dashed key) (decls) :test #'string=)))
                      (if cell (cdr cell) "")))))
        :set (lambda (o key v rcv) (declare (ignore o rcv))
               (cond
                 ((string= key "cssText") (set-attr element "style" (jstr v))
                  (setf (context-dirty ctx) t))
                 ((stringp key)
                  (let* ((d (prop->dashed key)) (alist (decls))
                         (cell (assoc d alist :test #'string=)) (val (jstr v)))
                    (if cell (setf (cdr cell) val)
                        (setf alist (append alist (list (cons d val)))))
                    (store alist))))
               js:*true*)))))

(defun install-cssom (ctx)
  (let* ((realm (context-realm ctx))
         (gcs (js:native-function realm "getComputedStyle"
                (lambda (this args) (declare (ignore this))
                  (let ((node (node-of ctx (arg args 0))))
                    (if node (computed-style-object ctx node)
                        (js:make-host-object realm))))
                2)))
    (js:define-global realm "getComputedStyle" gcs)
    (when (proto ctx :window)
      (js:put (proto ctx :window) "getComputedStyle" gcs :enumerable nil))))
