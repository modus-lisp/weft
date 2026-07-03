;;;; src/css/cursor.lisp — <cursor> keyword.
(in-package #:weft.css)
(defparameter *cursor-keywords*
  '("auto" "default" "none" "context-menu" "help" "pointer" "progress" "wait"
    "cell" "crosshair" "text" "vertical-text" "alias" "copy" "move" "no-drop"
    "not-allowed" "grab" "grabbing" "e-resize" "n-resize" "ne-resize" "nw-resize"
    "s-resize" "se-resize" "sw-resize" "w-resize" "ew-resize" "ns-resize"
    "nesw-resize" "nwse-resize" "col-resize" "row-resize" "all-scroll"
    "zoom-in" "zoom-out"))
(define-value-parser "cursor" (s)
  (let ((k (ascii-downcase (css-trim s))))
    (if (member k *cursor-keywords* :test #'string=) k :invalid)))
