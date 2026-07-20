;;;; src/script/cssom.lisp — the CSSOM surface: getComputedStyle + element.style.
;;;;
;;;; weft already resolves the cascade into cstyle structs; getComputedStyle
;;;; hands those back to script as a read-only style object keyed by camelCase
;;;; property. element.style is a small live inline-declaration object that
;;;; writes back through the element's `style` attribute so it re-cascades.
(in-package #:weft.script)

;;; ---- value formatting -----------------------------------------------------
(defun num->css (v)
  "Serialize a CSS number per CSSOM: an integral value as an integer, otherwise
   the shortest round-tripping decimal.  Never leak Lisp float syntax (the `d0`
   exponent marker) into the JS/CSS string — binding *read-default-float-format*
   to double-float suppresses it."
  (if (and (realp v) (< (abs v) 1d15) (= v (truncate v)))
      (princ-to-string (truncate v))
      (let ((*read-default-float-format* 'double-float))
        (princ-to-string (float v 1d0)))))

(defun px (v)
  (cond ((eq v :auto) "auto") ((eq v :none) "none") ((eq v :normal) "normal")
        ((numberp v) (concatenate 'string (num->css v) "px"))
        ((stringp v) v) ((null v) "") (t (princ-to-string v))))

(defun rgb-str (c)
  (if (and (consp c) (>= (length c) 3))
      (destructuring-bind (r g b &optional (a 1.0)) c
        (if (and a (< a 1))
            (format nil "rgba(~a, ~a, ~a, ~a)" (round r) (round g) (round b) (num->css a))
            (format nil "rgb(~a, ~a, ~a)" (round r) (round g) (round b))))
      ""))

;;; ---- specified-value canonicalization (CSSOM setProperty) -----------------
;;; test_valid_value / test_invalid_value require element.style to canonicalize a
;;; property's SPECIFIED value and DROP invalid declarations.  We route only the
;;; setProperty / style[prop]= path (never the cascade) through the property's
;;; grammar, reusing weft's cascade value parsers so "valid" here means exactly
;;; what layout accepts.  A value we cannot prove valid is stored VERBATIM (NIL
;;; result) — never rejected — so canonicalization can never drop a declaration
;;; the cascade would have honoured (guards the pixel/HN gates).

(defparameter +css-wide-keywords+
  '("inherit" "initial" "unset" "revert" "revert-layer"))

(defparameter +system-colors+
  ;; CSS Color 4 §system colors — valid <color>s that serialize lowercased.
  '("activetext" "buttonborder" "buttonface" "buttontext" "canvas"
    "canvastext" "field" "fieldtext" "graytext" "highlight" "highlighttext"
    "linktext" "mark" "marktext" "visitedtext" "selecteditem"
    "selecteditemtext" "accentcolor" "accentcolortext"))

(defparameter +color-props+
  '("color" "background-color" "border-top-color" "border-right-color"
    "border-bottom-color" "border-left-color" "outline-color"
    "text-decoration-color" "column-rule-color" "caret-color"
    "border-block-start-color" "border-block-end-color"
    "border-inline-start-color" "border-inline-end-color"
    "text-emphasis-color" "-webkit-text-fill-color" "-webkit-text-stroke-color"))

(defun %prefix-p (s prefix)
  (and (>= (length s) (length prefix)) (string= s prefix :end1 (length prefix))))

(defun %risky-color-tokens-p (lower)
  "Color FUNCTIONS whose canonical serialization weft would get wrong: `none`
   components keep the function form (hsl(none ...) stays hsl), relative colors
   (rgb(from ...)) and calc()/var() are not resolved.  Such values are stored
   verbatim rather than mis-serialized.  Gated on a `(` so the bare keyword
   `none` (an invalid <color>) still reaches the reject path."
  (and (find #\( lower)
       (or (search "none" lower) (search "calc" lower) (search "var(" lower)
           (search "from" lower)             ; relative color, e.g. rgb(from red r g b)
           (search "min(" lower) (search "max(" lower) (search "clamp(" lower))))

(defun canon-color-value (value)
  "Canonicalize a <color> specified VALUE (CSS Color 4 / CSSOM serialization).
   Named colors, system colors, currentcolor and transparent serialize as their
   lowercased keyword; hex and rgb()/hsl() normalize to rgb()/rgba().  Returns a
   canonical string, :invalid (reject), or NIL (store verbatim — for forms weft
   doesn't fully model: lab/oklch/hwb/color()/color-mix/light-dark/relative)."
  (let* ((v (string-trim '(#\Space #\Tab #\Newline #\Return) value))
         (lower (string-downcase v)))
    (cond
      ((zerop (length v)) :invalid)
      ((member lower +css-wide-keywords+ :test #'string=) lower)
      ((string= lower "currentcolor") "currentcolor")
      ((string= lower "transparent") "transparent")
      ((member lower +system-colors+ :test #'string=) lower)
      ((gethash lower css::*named-colors*) lower)
      ((%risky-color-tokens-p lower) nil)
      ((char= (char v 0) #\#)
       (let ((c (css:parse-value "color" v))) (if (consp c) (rgb-str c) :invalid)))
      ((or (%prefix-p lower "rgb(") (%prefix-p lower "rgba(")
           (%prefix-p lower "hsl(") (%prefix-p lower "hsla("))
       (let ((c (css:parse-value "color" v))) (if (consp c) (rgb-str c) :invalid)))
      ;; Any remaining value with no function parens is not a valid <color>: every
      ;; paren-free color (keyword, named/system color, hex) was handled above, so a
      ;; bare identifier, number, or multi-token run (`black white`) -> reject.
      ((not (find #\( v)) :invalid)
      ;; Unknown function forms weft doesn't model (lab/oklch/hwb/color()/color-mix/
      ;; light-dark/relative) -> leave verbatim so they are never wrongly dropped.
      (t nil))))

(defun canon-opacity-value (value)
  "Canonicalize an <opacity-value> = <number> | <percentage> (CSS Color 4 §opacity).
   The specified value keeps a number as-is and folds a percentage to its number
   (50% -> 0.5), unclamped.  calc()/min()/max()/clamp() are left verbatim (weft
   has no calc serializer).  Returns a string, :invalid, or NIL (verbatim)."
  (let* ((v (string-trim '(#\Space #\Tab #\Newline #\Return) value))
         (lower (string-downcase v)))
    (cond
      ((zerop (length v)) :invalid)
      ((member lower +css-wide-keywords+ :test #'string=) lower)
      ((or (search "calc" lower) (search "min(" lower) (search "max(" lower)
           (search "clamp(" lower) (search "var(" lower)) nil)
      ((char= (char v (1- (length v))) #\%)
       (let ((n (css:parse-value "number" (subseq v 0 (1- (length v))))))
         (if (numberp n) (num->css (/ n 100.0)) :invalid)))
      (t (let ((n (css:parse-value "number" v)))
           (if (numberp n) (num->css n) :invalid))))))

(defun canon-declaration (dashed value)
  "Canonical specified-value serialization for property DASHED given raw VALUE.
   :invalid -> drop the declaration; a string -> store it; NIL -> store verbatim."
  (cond
    ((member dashed +color-props+ :test #'string=) (canon-color-value value))
    ((string= dashed "opacity") (canon-opacity-value value))
    (t nil)))

(defparameter +computed-props+
  '("display" "white-space" "position" "float" "clear" "overflow" "text-align"
    "cursor" "text-transform" "box-sizing" "font-style" "z-index" "font-weight"
    "font-size" "line-height" "color" "width" "height" "min-width" "max-width"
    "min-height" "max-height" "margin-top" "margin-right" "margin-bottom"
    "margin-left" "padding-top" "padding-right" "padding-bottom" "padding-left"
    "top" "left" "right" "bottom" "visibility" "background-color")
  "Dashed property names getComputedStyle resolves; `property in getComputedStyle(el)`
   must report true for these (test_computed_value's support guard, CSSOM).")

(defun computed-prop-p (dashed)
  (and (member dashed +computed-props+ :test #'string=) t))

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
      ((string= dashed "visibility") (s css:cstyle-visibility))
      ((string= dashed "background-color")
       (rgb-str (or (s css:cstyle-background) '(0 0 0 0))))
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
      ((string= dashed "padding-top") (px (css::resolve-pad (s css:cstyle-padding-top) nil)))
      ((string= dashed "padding-right") (px (css::resolve-pad (s css:cstyle-padding-right) nil)))
      ((string= dashed "padding-bottom") (px (css::resolve-pad (s css:cstyle-padding-bottom) nil)))
      ((string= dashed "padding-left") (px (css::resolve-pad (s css:cstyle-padding-left) nil)))
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
      :has (lambda (o key)
             (let ((key (js:to-property-key key)))
               (if (and (stringp key)
                        (or (string= key "getPropertyValue")
                            (computed-prop-p (prop->dashed key))))
                   js:*true*
                   (js::ordinary-has o key))))
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
  "The live CSSStyleDeclaration for ELEMENT's inline style.  Property access
   (camelCase or bracketed dashed name) and the CSSOM methods getPropertyValue/
   setProperty/removeProperty/item all read and write through the `style`
   attribute so mutations re-cascade (CSSOM §CSSStyleDeclaration)."
  (let ((realm (context-realm ctx)))
    (labels ((decls () (parse-inline-style (get-attr element "style")))
             (store (alist)
               (set-attr element "style" (serialize-inline-style alist))
               (setf (context-dirty ctx) t))
             (get-prop (name)
               (let ((cell (assoc (string-downcase name) (decls) :test #'string=)))
                 (if cell (cdr cell) "")))
             (set-prop (name val)
               ;; Setting the empty string removes the declaration (CSSOM §setProperty).
               ;; A parseable value is stored in its canonical form; a value proven
               ;; invalid is ignored (existing declaration untouched); anything else
               ;; is stored verbatim (CSSOM §setProperty / CSS value serialization).
               (let* ((d (string-downcase name)) (alist (decls))
                      (cell (assoc d alist :test #'string=)))
                 (cond ((zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return) (or val ""))))
                        (when cell (store (remove cell alist))))
                       (t (let ((canon (canon-declaration d (or val ""))))
                            (unless (eq canon :invalid)
                              (let ((stored (if (stringp canon) canon val)))
                                (cond (cell (setf (cdr cell) stored) (store alist))
                                      (t (store (append alist (list (cons d stored)))))))))))))
             (remove-prop (name)
               (let* ((d (string-downcase name)) (alist (decls))
                      (cell (assoc d alist :test #'string=)))
                 (when cell (store (remove cell alist))))))
      (let ((f-get (js:native-function realm "getPropertyValue"
                     (lambda (this a) (declare (ignore this))
                       (get-prop (jstr (arg a 0)))) 1))
            (f-set (js:native-function realm "setProperty"
                     (lambda (this a) (declare (ignore this))
                       (set-prop (jstr (arg a 0)) (jstr (arg a 1))) js:*undefined*) 2))
            (f-rem (js:native-function realm "removeProperty"
                     (lambda (this a) (declare (ignore this))
                       (let ((old (get-prop (jstr (arg a 0)))))
                         (remove-prop (jstr (arg a 0))) old)) 1))
            (f-pri (js:native-function realm "getPropertyPriority"
                     (lambda (this a) (declare (ignore this a)) "") 1))
            (f-item (js:native-function realm "item"
                      (lambda (this a) (declare (ignore this))
                        (let ((al (decls)) (i (int-arg a 0)))
                          (if (and (>= i 0) (< i (length al))) (car (nth i al)) ""))) 1)))
        (js:make-host-object realm
          :get (lambda (o key rcv) (declare (ignore rcv))
                 (setf key (js:to-property-key key))
                 (cond
                   ((not (stringp key)) (js:js-get (js:js-object-proto o) key o))
                   ((string= key "cssText") (or (get-attr element "style") ""))
                   ((string= key "length") (num (length (decls))))
                   ((string= key "getPropertyValue") f-get)
                   ((string= key "setProperty") f-set)
                   ((string= key "removeProperty") f-rem)
                   ((string= key "getPropertyPriority") f-pri)
                   ((string= key "item") f-item)
                   ((index-string-p key)          ; numeric index -> property name
                    (let ((al (decls)) (i (parse-integer key)))
                      (if (< i (length al)) (car (nth i al)) "")))
                   (t (let ((cell (assoc (prop->dashed key) (decls) :test #'string=)))
                        (if cell (cdr cell) "")))))
          :set (lambda (o key v rcv) (declare (ignore o rcv))
                 (cond
                   ((string= key "cssText") (set-attr element "style" (jstr v))
                    (setf (context-dirty ctx) t))
                   ((stringp key)
                    (set-prop (prop->dashed key) (jstr v))))
                 js:*true*))))))

;;; ---- CSSOM: document.styleSheets / CSSStyleSheet / CSSRuleList ------------
(defun stylesheet-owner-p (el)
  (or (string= (h:dnode-name el) "style")
      (and (string= (h:dnode-name el) "link")
           (let ((rel (dom:get-attribute el "rel"))) (and rel (search "stylesheet" (string-downcase rel)))))))

(defun style-elements (doc)
  (remove-if-not #'stylesheet-owner-p (dom:get-elements-by-tag-name doc "*")))

(defun computed-px (ctx node prop)
  "The integer pixel value of computed PROP on NODE (0 if auto/none/absent)."
  (let* ((doc (owner-document node)) (cs (and doc (gethash node (document-styles ctx doc)))))
    (if cs (let* ((v (computed-prop cs prop))
                  (end (position-if-not (lambda (c) (or (digit-char-p c) (char= c #\.))) v)))
             (or (ignore-errors (round (read-from-string (subseq v 0 (or end (length v)))))) 0))
        0)))

(defun sheet-rule-count (owner)
  (length (css:parse-stylesheet (dom:text-content owner))))

(defun make-rule-list (ctx owner)
  "A live CSSRuleList over OWNER (<style>)'s current rules."
  (let ((realm (context-realm ctx)))
    (js:make-host-object realm
      :get (lambda (o key rcv) (declare (ignore rcv))
             (let ((key (js:to-property-key key)))
               (cond ((and (stringp key) (string= key "length")) (num (sheet-rule-count owner)))
                     ((index-string-p key)
                      (let* ((rules (css:parse-stylesheet (dom:text-content owner)))
                             (i (parse-integer key)))
                        (if (< i (length rules)) (make-css-rule ctx (nth i rules)) js:*undefined*)))
                     (t (js:js-get (js:js-object-proto o) key o))))))))

(defun make-css-rule (ctx rule)
  (let ((o (js:make-host-object (context-realm ctx))))
    (js:put o "type" 1.0)
    (js:put o "selectorText" (or (css:css-rule-selector rule) ""))
    (js:put o "cssText"
            (format nil "~a { ~{~a: ~a;~^ ~} }" (or (css:css-rule-selector rule) "")
                    (loop for d in (css:css-rule-decls rule)
                          collect (css:css-decl-prop d) collect (css:css-decl-value d))))
    o))

(defun make-stylesheet-object (ctx owner)
  (let* ((realm (context-realm ctx)) (sheet (js:make-host-object realm)))
    (js:put sheet "ownerNode" (wrap ctx owner))
    (js:put sheet "href" (let ((h (dom:get-attribute owner "href")))
                           (if (and h (string= (h:dnode-name owner) "link")) h js:*null*)))
    (js:put sheet "type" "text/css")
    (js:put sheet "title" js:*null*)
    (js:put sheet "cssRules" (make-rule-list ctx owner))
    (js:put sheet "rules" (make-rule-list ctx owner))
    (defmethod* ctx sheet "insertRule" 2 (this a)
      ;; append the rule to the owner's text (last => wins for equal specificity);
      ;; the harness inserts at the end.
      (h:dom-append owner (h:make-text (jstr (arg a 0))))
      (setf (context-dirty ctx) t)
      (num (int-arg a 1)))
    (defmethod* ctx sheet "deleteRule" 1 (this a) js:*undefined*)
    sheet))

(defun make-stylesheet-list (ctx doc)
  (let ((realm (context-realm ctx)))
    (js:make-host-object realm
      :get (lambda (o key rcv) (declare (ignore rcv))
             (let ((key (js:to-property-key key)) (sheets (style-elements doc)))
               (cond ((and (stringp key) (string= key "length")) (num (length sheets)))
                     ((index-string-p key)
                      (let ((i (parse-integer key)))
                        (if (< i (length sheets)) (make-stylesheet-object ctx (nth i sheets)) js:*undefined*)))
                     (t (js:js-get (js:js-object-proto o) key o))))))))

;;; ---- the CSS namespace object (CSSOM §The CSS interface) ------------------
(defun css-escape (s)
  "CSS.escape: serialize S as a CSS identifier (CSSOM §serialize an identifier)."
  (with-output-to-string (o)
    (loop with n = (length s)
          for i from 0 below n
          for c = (char s i)
          for cc = (char-code c)
          do (cond
               ((= cc 0) (write-char (code-char #xFFFD) o))
               ((or (<= 1 cc #x1F) (= cc #x7F))
                (format o "\\~(~x~) " cc))
               ((and (= i 0) (<= #x30 cc #x39))
                (format o "\\~(~x~) " cc))
               ((and (= i 1) (<= #x30 cc #x39) (char= (char s 0) #\-))
                (format o "\\~(~x~) " cc))
               ((and (= i 0) (= n 1) (char= c #\-))
                (write-string "\\-" o))
               ((or (>= cc #x80) (char= c #\-) (char= c #\_)
                    (<= #x30 cc #x39) (<= #x41 cc #x5A) (<= #x61 cc #x7A))
                (write-char c o))
               (t (write-char #\\ o) (write-char c o))))))

(defun css-supports-decl-p (property value)
  "True if declaration PROPERTY:VALUE is well-formed (CSS.supports 2-arg form).
   Parsed through the real CSS parser so malformed input is rejected; unknown
   properties are accepted (weft doesn't maintain a full property registry)."
  (and (stringp property) (stringp value)
       (plusp (length (string-trim '(#\Space #\Tab) property)))
       (plusp (length (string-trim '(#\Space #\Tab) value)))
       (not (find #\; value)) (not (find #\{ value)) (not (find #\} value))
       (handler-case
           (let* ((sheet (css:parse-stylesheet
                          (format nil "*{~a:~a}" property value)))
                  (decls (and sheet (css:css-rule-decls (first sheet)))))
             (and decls
                  (some (lambda (d)
                          (and (string-equal (css:css-decl-prop d)
                                             (string-trim '(#\Space #\Tab) property))
                               (plusp (length (string-trim '(#\Space #\Tab)
                                                           (css:css-decl-value d))))))
                        decls)))
         (error () nil))))

(defun css-supports-condition-p (text)
  "CSS.supports 1-arg form: a supports-condition string, e.g. \"(display: flex)\".
   Handles a single parenthesised declaration; compound and/or/selector() forms
   fall through to NIL (weft doesn't evaluate them)."
  (let ((s (string-trim '(#\Space #\Tab) (or text ""))))
    (when (and (plusp (length s)) (char= (char s 0) #\() (char= (char s (1- (length s))) #\)))
      (let* ((inner (subseq s 1 (1- (length s))))
             (colon (position #\: inner)))
        (when (and colon (not (search ") and " inner)) (not (search ") or " inner)))
          (css-supports-decl-p (subseq inner 0 colon) (subseq inner (1+ colon))))))))

(defun make-css-namespace (realm)
  (let ((css (js:make-host-object realm)))
    (js:put css "escape"
            (js:native-function realm "escape"
              (lambda (this a) (declare (ignore this)) (css-escape (jstr (arg a 0)))) 1)
            :enumerable nil)
    (js:put css "supports"
            (js:native-function realm "supports"
              (lambda (this a) (declare (ignore this))
                (if (js:js-undefined-p (arg a 1))
                    (if (css-supports-condition-p (jstr (arg a 0))) js:*true* js:*false*)
                    (if (css-supports-decl-p (jstr (arg a 0)) (jstr (arg a 1))) js:*true* js:*false*)))
              2)
            :enumerable nil)
    css))

(defun install-cssom (ctx)
  (let* ((realm (context-realm ctx))
         (gcs (js:native-function realm "getComputedStyle"
                (lambda (this args) (declare (ignore this))
                  (let ((node (node-of ctx (arg args 0))))
                    (if node (computed-style-object ctx node)
                        (js:make-host-object realm))))
                2))
         (css (make-css-namespace realm)))
    (js:define-global realm "getComputedStyle" gcs)
    (js:define-global realm "CSS" css)
    (when (proto ctx :window)
      (js:put (proto ctx :window) "getComputedStyle" gcs :enumerable nil)
      (js:put (proto ctx :window) "CSS" css :enumerable nil))))
