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
                                           (:file "utf-8")
                                           (:file "utf-16le") (:file "utf-16be")
                                           (:file "windows-1252") (:file "windows-1251")
                                           (:file "iso-8859-2") (:file "koi8-r")
                                           (:file "shift_jis") (:file "euc-jp")
                                           (:file "big5") (:file "euc-kr"))))))
  :in-order-to ((test-op (test-op "weft/test"))))

(defsystem "weft/test"
  :depends-on ("weft")
  :components ((:module "inspect" :components ((:file "offline-test"))))
  :perform (test-op (o c) (uiop:symbol-call :weft.test :run)))
