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
