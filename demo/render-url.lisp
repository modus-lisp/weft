;;;; demo/render-url.lisp — fetch a live page and render it to a PNG.
;;;;   sbcl --script demo/render-url.lisp <url> <out.png> [width]
;;;; Drives the whole stack: fetch -> Content-Encoding decode -> charset decode
;;;; -> HTML parse -> CSS cascade (incl. the page's <style> tags) -> layout ->
;;;; paint -> PNG.  Pure Common Lisp, no browser engine.
(require :asdf)
(handler-bind ((warning #'muffle-warning))
  (asdf:load-system "weft/fetch") (asdf:load-system "weft/render"))

(let* ((args (rest sb-ext:*posix-argv*))
       (url (or (first args) "https://example.com/"))
       (out (or (second args) "/tmp/weft-page.png"))
       (width (if (third args) (parse-integer (third args)) 800)))
  (multiple-value-bind (text charset resp) (weft.fetch:fetch-text url)
    (format t "~&fetched ~a  (HTTP ~d, ~a, ~a, ~:d bytes)~%" url
            (weft.fetch:response-status resp)
            (or (weft.fetch:get-header (weft.fetch:response-headers resp) "content-encoding") "identity")
            charset (length text))
    (multiple-value-bind (path w h) (weft.render:render-to-png text nil width out)
      (format t "rendered -> ~a  (~dx~d)~%" path w h))))
