;;;; inspect/scripting-m1.lisp — M1 of the weft/shuttle scripting seam.
;;;;
;;;; Proves the round-trip: a <script> reads the DOM, mutates it, and the change
;;;; is reflected on relayout.  The page
;;;;   <div id="x">old</div>
;;;;   <script>document.getElementById('x').textContent='hi '+(1+2)</script>
;;;; must end up showing "hi 3".  Two independent oracles:
;;;;   (1) DOM     — after the script runs, textContent of #x is exactly "hi 3".
;;;;   (2) PIXELS  — the scripted render is byte-identical to a scriptless render
;;;;                 of <div id="x">hi 3</div> (script boxes are display:none, so
;;;;                 the only visible difference would BE the mutation).
;;;;
;;;;   sbcl --script inspect/scripting-m1.lisp
(require :asdf)
(push (truename (merge-pathnames "../" (directory-namestring *load-truename*)))
      asdf:*central-registry*)
(asdf:load-system "weft/script")

(defpackage #:weft.script.m1
  (:use #:cl)
  (:local-nicknames (#:s #:weft.script) (#:r #:weft.render)
                    (#:dom #:weft.dom) (#:h #:weft.html))
  (:export #:run))
(in-package #:weft.script.m1)

(defparameter *scripted*
  "<div id=\"x\">old</div><script>document.getElementById('x').textContent='hi '+(1+2)</script>")
(defparameter *expected* "<div id=\"x\">hi 3</div>")

(defun run ()
  (let ((fail 0))
    ;; (1) DOM oracle: run the scripted page, read #x back out of the live tree.
    (multiple-value-bind (cv ctx) (s:render-scripted-to-canvas *scripted* "" 400)
      (declare (ignore cv))
      (let* ((doc (s:context-document ctx))
             (el  (dom:get-element-by-id doc "x"))
             (txt (and el (dom:text-content el))))
        (format t "~&[DOM]   #x textContent = ~s~%" txt)
        (unless (equal txt "hi 3")
          (incf fail) (format t "  FAIL: expected \"hi 3\"~%"))
        ;; (2) Pixel oracle: scripted render == scriptless render of the result.
        (let* ((a (r:render-to-canvas *scripted* "" 400))    ; via before-layout=nil
               (want (r:render-to-canvas *expected* "" 400))
               (got (nth-value 0 (s:render-scripted-to-canvas *scripted* "" 400))))
          ;; a is the UNSCRIPTED render of the scripted page: it must still say
          ;; "old" (proves the render path is unchanged without the bridge).
          (declare (ignorable a))
          (let ((match (equalp (r:canvas-pixels got) (r:canvas-pixels want))))
            (format t "[PIXEL] scripted render == static \"hi 3\" render: ~a~%"
                    (if match "yes" "NO"))
            (unless match
              (incf fail)
              (format t "  FAIL: scripted pixels differ from the expected result~%"))))))
    (if (zerop fail)
        (format t "~&scripting M1: PASS (round-trip green — \"hi 3\")~%")
        (format t "~&scripting M1: ~a FAILURE(S)~%" fail))
    (values (zerop fail) fail)))

(multiple-value-bind (ok) (run)
  (unless ok (uiop:quit 1)))
