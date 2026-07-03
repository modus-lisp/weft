;;;; src/script/svg.lisp — SVG DOM host interfaces (SVGElement subset).
;;;;
;;;; Elements in the SVG namespace (created via createElementNS, or parsed from an
;;;; SVG document) wrap onto a prototype that adds the SVG-specific IDL surface:
;;;; SVGRectElement.width/height/x/y (as SVGAnimatedLength) and
;;;; SVGTextContentElement.getNumberOfChars() — the Acid3 SVG-DOM cluster.
(in-package #:weft.script)

(defun svg-leading-number (s)
  "The leading numeric value of an SVG length string S (\"100\", \"1em\", \"50%\"),
or 0."
  (let* ((str (string-trim '(#\Space #\Tab) (or s "0"))) (len (length str)) (i 0))
    (when (and (> len 0) (member (char str 0) '(#\+ #\-))) (incf i))
    (loop while (and (< i len) (or (digit-char-p (char str i)) (char= (char str i) #\.)))
          do (incf i))
    (or (and (> i 0) (ignore-errors (float (read-from-string (subseq str 0 i)) 1d0))) 0d0)))

(defun svg-animated-length (ctx node attr)
  "An SVGAnimatedLength-shaped object for NODE's ATTR: { baseVal, animVal } each an
SVGLength { value, valueInSpecifiedUnits, unitType }."
  (let* ((realm (context-realm ctx)) (op (js:eval-script realm "Object.prototype"))
         (v (svg-leading-number (get-attr node attr)))
         (len (js:make-object :proto op)))
    (js:put len "value" (num v))
    (js:put len "valueInSpecifiedUnits" (num v))
    (js:put len "unitType" (num 1))               ; SVG_LENGTHTYPE_NUMBER
    (let ((o (js:make-object :proto op)))
      (js:put o "baseVal" len)
      (js:put o "animVal" len)
      o)))

(defun install-svg-element-proto (ctx svgep)
  "Install the SVGElement subset on SVGEP (whose [[Prototype]] is Element.prototype)."
  (macrolet ((n (this) `(require-node ctx ,this)))
    ;; SVGRectElement / geometry: length-valued animated attributes.
    (defget ctx svgep "width"  (this) (svg-animated-length ctx (n this) "width"))
    (defget ctx svgep "height" (this) (svg-animated-length ctx (n this) "height"))
    (defget ctx svgep "x"      (this) (svg-animated-length ctx (n this) "x"))
    (defget ctx svgep "y"      (this) (svg-animated-length ctx (n this) "y"))
    ;; SVGTextContentElement.
    (defmethod* ctx svgep "getNumberOfChars" 0 (this a)
      (num (length (dom:text-content (n this)))))
    (defmethod* ctx svgep "getComputedTextLength" 0 (this a) (num 0))))
