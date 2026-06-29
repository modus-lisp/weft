;;;; src/render/packages.lisp — rendering package.
(defpackage #:weft.render
  (:use #:cl)
  (:local-nicknames (#:h #:weft.html) (#:css #:weft.css))
  (:export #:canvas #:make-canvas #:canvas-width #:canvas-height
           #:fill-rect #:draw-text #:write-png
           #:layout-tree #:render-to-png #:render-to-canvas #:canvas-ink #:canvas-pixels))
