;;;; src/packages.lisp — package layout for weft.

(defpackage #:weft.url
  (:use #:cl)
  (:export
   #:url #:url-p #:parse
   #:url-scheme #:url-username #:url-password #:url-host #:url-port
   #:url-path #:url-query #:url-fragment #:url-opaque-path-p
   #:special-p
   ;; serialized getters (the WHATWG URL API surface)
   #:href #:protocol #:username #:password #:host-str #:hostname
   #:port-str #:pathname-str #:search-str #:hash-str #:origin
   #:serialize-host))

(defpackage #:weft.encoding
  (:use #:cl)
  (:export
   #:decode #:get-decoder #:register-decoder #:define-decoder
   #:normalize-label #:+replacement+ #:*decoders*))

(defpackage #:weft.html
  (:use #:cl)
  (:export #:tokenize #:*entities*
           #:tok-type #:tok-name #:tok-data #:tok-attrs #:tok-self-closing
           #:tok-public #:tok-system #:tok-force-quirks
           ;; DOM + tree construction
           #:parse-html #:serialize-tree #:make-document
           #:dnode #:dnode-kind #:dnode-name #:dnode-attrs #:dnode-data
           #:dnode-children #:dnode-parent #:dnode-namespace
           ;; node constructors + mutation kernel (the DOM-scripting seam)
           #:make-element #:make-text #:make-comment #:make-fragment #:make-doctype
           #:dom-append #:dom-insert-before #:dom-remove
           #:dom-last-child #:dom-prev-sibling))

(defpackage #:weft.dom
  (:use #:cl)
  (:local-nicknames (#:h #:weft.html))
  (:export #:get-element-by-id #:get-elements-by-tag-name
           #:get-elements-by-class-name #:text-content
           #:first-element-child #:last-element-child #:child-element-count
           #:next-element-sibling #:previous-element-sibling
           #:get-attribute #:has-attribute))
