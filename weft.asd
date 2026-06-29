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
                                           (:file "tokenizer") (:file "parser") (:file "selector"))))))
  :in-order-to ((test-op (test-op "weft/test"))))

;; The resource loader depends on the pure-CL codecs (sibling systems) + chipz
;; for gzip/deflate; kept separate so the core engine stays dependency-light.
(defsystem "weft/fetch"
  :depends-on ("weft" "brotli-pure" "zstd-pure" "chipz")
  :components ((:module "src" :components ((:file "fetch")))))

(defsystem "weft/test"
  :depends-on ("weft" "weft/fetch")
  :components ((:module "inspect" :components ((:file "offline-test") (:file "encoding-test")
                                              (:file "fetch-test") (:file "html-test")
                                              (:file "tree-test")
                                              (:file "dom-test") (:file "css-test") (:file "selector-test"))))
  :perform (test-op (o c)
             (let ((url-ok (uiop:symbol-call :weft.test :run))
                   (enc-ok (zerop (nth-value 1 (uiop:symbol-call :weft.encoding.test :run))))
                   (fetch-ok (zerop (nth-value 1 (uiop:symbol-call :weft.fetch.test :run))))
                   (html-ok (zerop (nth-value 1 (uiop:symbol-call :weft.html.test :run))))
                   (dom-ok (and (zerop (nth-value 1 (uiop:symbol-call :weft.dom.test :run)))
                                (zerop (nth-value 1 (uiop:symbol-call :weft.dom.test :run-traversal)))))
                   (css-ok (zerop (nth-value 1 (uiop:symbol-call :weft.css.test :run))))
                   (sel-ok (zerop (nth-value 1 (uiop:symbol-call :weft.css.select-test :run)))))
               ;; tree construction is in progress — run for visibility, don't gate on it yet
               (uiop:symbol-call :weft.html.tree-test :run)
               (unless (and url-ok enc-ok fetch-ok html-ok dom-ok css-ok sel-ok) (error "weft: gate failures")))))
