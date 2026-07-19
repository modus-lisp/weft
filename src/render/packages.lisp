;;;; src/render/packages.lisp — rendering package.
(defpackage #:weft.render
  (:use #:cl)
  (:local-nicknames (#:h #:weft.html) (#:css #:weft.css)
                    (#:g #:gesso) (#:st #:stencil) (#:sc #:scribe))
  ;; image/SVG codecs live in the standalone `pigment` library; weft imports the
  ;; IMG struct + accessors and the decode entry points so layout/vector/image
  ;; keep calling them unqualified.
  (:import-from #:pigment
   #:img #:make-img #:img-w #:img-h #:img-rgba #:img-sw #:img-sh #:img-sr
   #:decode-image #:decode-image-bytes #:rgba-canvas->img #:base64-decode)
  (:export #:canvas #:make-canvas #:canvas-width #:canvas-height
           #:fill-rect #:draw-text #:write-png #:canvas->png
           #:layout-tree #:render-to-png #:render-to-canvas #:canvas-ink #:canvas-pixels
           #:element-canvas #:*element-canvas*
           ;; network image loading + cache (bind *IMAGE-LOADER* to a fetcher)
           #:*image-loader* #:*image-store* #:clear-image-cache #:*image-fetch-deadline*
           #:fetch-image #:decode-image-bytes
           ;; live progress hook (:cascade :layout :painting) — NIL disables it
           #:*progress*
           ;; font registration (@font-face web fonts / the Ahem test font)
           #:register-font #:load-font-faces #:*font-loader* #:*font-load-budget*
           ;; interactive-shell support (hit-testing, box tree, viewport scroll)
           #:render-document #:box-at #:node-at #:point-in-box-p #:img-source-url
           #:lbox #:lbox-p #:lbox-x #:lbox-y #:lbox-w #:lbox-h
           #:lbox-node #:lbox-kind #:lbox-children #:lbox-style))
