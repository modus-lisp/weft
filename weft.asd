;;;; weft.asd
;;;;
;;;; weft — a self-sovereign web engine in pure Common Lisp, built clean-room
;;;; against the web platform specs with the Web Platform Tests as the oracle.
;;;; Ladybird-style: no WebKit/Blink/Gecko.  This is the start (P0: URL).

(defsystem "weft"
  :description "A from-scratch, pure Common Lisp web engine (no FFI).  P0: a
                WHATWG-conformant URL parser, differential-tested against the
                Web Platform Tests url corpus."
  :version "0.0.1" :author "ynniv" :license "MIT"
  :depends-on ()
  :serial t
  :components ((:module "src" :serial t
                :components ((:file "packages")
                             (:file "url")        ; WHATWG URL parser
                             (:module "encoding" :serial t
                              :components ((:file "kernel")
                                           (:file "labels")   ; WHATWG label aliases
                                           (:file "utf-8")
                                           (:file "utf-16le") (:file "utf-16be")
                                           (:file "windows-1252") (:file "windows-1251")
                                           (:file "iso-8859-2") (:file "koi8-r")
                                           (:file "shift_jis-table") (:file "shift_jis")
                                           (:file "euc-jp-table") (:file "euc-jp")
                                           (:file "big5")
                                           (:file "euc-kr-table") (:file "euc-kr")
                                           ;; wider single-byte set
                                           (:file "ibm866")
                                           (:file "iso-8859-3") (:file "iso-8859-4") (:file "iso-8859-5")
                                           (:file "iso-8859-6") (:file "iso-8859-7") (:file "iso-8859-8")
                                           (:file "iso-8859-10") (:file "iso-8859-13") (:file "iso-8859-14")
                                           (:file "iso-8859-15") (:file "iso-8859-16")
                                           (:file "koi8-u") (:file "macintosh") (:file "windows-874")
                                           (:file "windows-1250") (:file "windows-1253") (:file "windows-1254")
                                           (:file "windows-1255") (:file "windows-1256") (:file "windows-1257")
                                           (:file "windows-1258") (:file "x-mac-cyrillic")
                                           ;; wider multi-byte set
                                           (:file "gbk-table") (:file "gbk")
                                           (:file "gb18030-table") (:file "gb18030-ranges")
                                           (:file "gb18030")))
                             (:module "html" :serial t
                              :components ((:file "entities")
                                           (:file "tokenizer")
                                           (:file "dom")
                                           (:file "tree")))
                             (:module "dom" :serial t
                              :components ((:file "by-id") (:file "by-tag")
                                           (:file "by-class") (:file "text-content")
                                           (:file "element-children") (:file "siblings")
                                           (:file "attributes")))
                             (:module "css" :serial t
                              :components ((:file "packages") (:file "kernel")
                                           (:file "color-names") (:file "color") (:file "length")
                                           (:file "number") (:file "angle")
                                           (:file "percentage") (:file "integer") (:file "time")
                                           (:file "resolution") (:file "url") (:file "css-string")
                                           (:file "font-family") (:file "text-decoration") (:file "list-style-type") (:file "line-height")
                                           (:file "flex") (:file "box-sizing") (:file "font-style") (:file "text-transform") (:file "hyphens")
                                           (:file "overflow") (:file "position") (:file "float") (:file "border-radius") (:file "transform")
                                           (:file "cursor") (:file "visibility") (:file "direction") (:file "font-variant")
                                           (:file "z-index") (:file "letter-spacing") (:file "vertical-align") (:file "white-space")
                                           (:file "opacity") (:file "word-spacing") (:file "order")
                                           (:file "object-fit") (:file "aspect-ratio") (:file "text-indent")
                                           (:file "background-repeat") (:file "background-position")
                                           (:file "tokenizer") (:file "parser") (:file "selector") (:file "style"))))))
  :in-order-to ((test-op (test-op "weft/test"))))

;; The resource loader depends on the pure-CL codecs (sibling systems) + chipz
;; for gzip/deflate; kept separate so the core engine stays dependency-light.
(defsystem "weft/fetch"
  :depends-on ("weft" "seal" "brotli-pure" "zstd-pure" "chipz")
  :components ((:module "src" :components ((:file "fetch")))))

(defsystem "weft/render"
  :depends-on ("weft" "chipz" "scribe" "gesso" "stencil" "webp-pure")
  :components ((:module "src" :components
                ((:module "render" :serial t
                  :components ((:file "packages") (:file "font") (:file "font-bold")
                               (:file "canvas") (:file "image") (:file "jpeg") (:file "vector")
                               (:file "text") (:file "hyphen-en") (:file "hyphenate")
                               (:file "layout") (:file "grid") (:file "interact")))))))

;; The scripting seam: binds shuttle (a pure-CL JavaScript engine) to the weft
;; DOM so inline <script> can read and mutate the live tree, with the change
;; reflected on relayout.  Kept separate so the core + render stay JS-free.
(defsystem "weft/script"
  :depends-on ("weft/render" "shuttle")
  :components ((:module "src" :components
                ((:module "script" :serial t
                  :components ((:file "packages") (:file "core") (:file "timers")
                               (:file "events") (:file "cssom") (:file "dom")
                               (:file "mutation")
                               (:file "canvas") (:file "svg")
                               (:file "traversal") (:file "range") (:file "xml") (:file "loader")
                               (:file "tables") (:file "bridge") (:file "interact")))))))

(defsystem "weft/test"
  :depends-on ("weft" "weft/fetch" "weft/render")
  :components ((:module "inspect" :components ((:file "offline-test") (:file "encoding-test")
                                              (:file "fetch-test") (:file "html-test")
                                              (:file "tree-test")
                                              (:file "dom-test") (:file "css-test") (:file "selector-test")
                                              (:file "acid-test") (:file "acid2-conformance"))))
  :perform (test-op (o c)
             (let ((url-ok (uiop:symbol-call :weft.test :run))
                   (enc-ok (zerop (nth-value 1 (uiop:symbol-call :weft.encoding.test :run))))
                   (fetch-ok (zerop (nth-value 1 (uiop:symbol-call :weft.fetch.test :run))))
                   (html-ok (zerop (nth-value 1 (uiop:symbol-call :weft.html.test :run))))
                   (dom-ok (and (zerop (nth-value 1 (uiop:symbol-call :weft.dom.test :run)))
                                (zerop (nth-value 1 (uiop:symbol-call :weft.dom.test :run-traversal)))))
                   (css-ok (zerop (nth-value 1 (uiop:symbol-call :weft.css.test :run))))
                   (acid-ok (zerop (nth-value 1 (uiop:symbol-call :weft.acid.test :run))))
                   ;; Acid2 pixel + geometry vs the vendored real-browser ground truth
                   (acid2-ok (zerop (nth-value 1 (uiop:symbol-call :weft.acid2.conformance :run))))
                   (sel-ok (zerop (nth-value 1 (uiop:symbol-call :weft.css.select-test :run)))))
               ;; tree construction is in progress — run for visibility, don't gate on it yet
               (uiop:symbol-call :weft.html.tree-test :run)
               (unless (and url-ok enc-ok fetch-ok html-ok dom-ok css-ok sel-ok acid-ok acid2-ok)
                 (error "weft: gate failures")))))
