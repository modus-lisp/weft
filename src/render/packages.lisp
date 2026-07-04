;;;; src/render/packages.lisp — rendering package.
(defpackage #:weft.render
  (:use #:cl)
  (:local-nicknames (#:h #:weft.html) (#:css #:weft.css)
                    (#:g #:gesso) (#:st #:stencil) (#:sc #:scribe))
  (:export #:canvas #:make-canvas #:canvas-width #:canvas-height
           #:fill-rect #:draw-text #:write-png
           #:layout-tree #:render-to-png #:render-to-canvas #:canvas-ink #:canvas-pixels
           #:element-canvas #:*element-canvas*
           ;; network image loading + cache (bind *IMAGE-LOADER* to a fetcher)
           #:*image-loader* #:*image-store* #:clear-image-cache
           #:fetch-image #:decode-image-bytes
           ;; interactive-shell support (hit-testing, box tree, viewport scroll)
           #:render-document #:box-at #:node-at #:point-in-box-p
           #:lbox #:lbox-p #:lbox-x #:lbox-y #:lbox-w #:lbox-h
           #:lbox-node #:lbox-kind #:lbox-children #:lbox-style))
