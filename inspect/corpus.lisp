;;;; inspect/corpus.lisp — real-world breadth smoke test.
;;;;
;;;; Renders a corpus of vendored real pages (inspect/vectors/pages/corpus/)
;;;; through the FULL scripted pipeline (parse -> CSS -> run scripts -> drain
;;;; timers -> layout -> paint) and reports size / ink / script count / time,
;;;; plus any crashes or uncaught script errors.  This is the daily-driver
;;;; regression net: a real page must render without breaking.
;;;;   sbcl --script inspect/corpus.lisp
(require :asdf)
(push (truename (merge-pathnames "../" (directory-namestring *load-truename*)))
      asdf:*central-registry*)
(asdf:load-system "weft/script")

(defpackage #:weft.corpus
  (:use #:cl) (:local-nicknames (#:s #:weft.script) (#:r #:weft.render)
                                (#:h #:weft.html) (#:dom #:weft.dom))
  (:export #:run))
(in-package #:weft.corpus)

;;; file -> the URL it was fetched from (base for location / URL resolution)
(defparameter *corpus*
  '(("cern.html"      . "http://info.cern.ch/hypertext/WWW/TheProject.html")
    ("mfw.html"       . "http://motherfuckingwebsite.com/")
    ("danluu.html"    . "https://danluu.com/")
    ("python.html"    . "https://docs.python.org/3/")
    ("hn.html"        . "https://news.ycombinator.com/")
    ("wikipedia.html" . "https://en.wikipedia.org/wiki/HTML")
    ("bbc.html"       . "https://www.bbc.com/news")))

(defun slurp (p) (with-open-file (in p :external-format :latin-1)
                   (let ((str (make-string (file-length in)))) (subseq str 0 (read-sequence str in)))))

(defun run ()
  (let ((dir (asdf:system-relative-pathname "weft" "inspect/vectors/pages/corpus/"))
        (fail 0) (errors 0))
    (dolist (entry *corpus*)
      (let ((path (merge-pathnames (car entry) dir)))
        (if (not (probe-file path))
            (format t "~a: (missing — fetch it into the corpus dir)~%" (car entry))
            (let ((errs 0) (start (get-internal-real-time)))
              ;; count uncaught script errors reported on *error-output*
              (let* ((*error-output* (make-string-output-stream))
                     (result
                       (handler-case
                           (multiple-value-bind (cv ctx)
                               (s:render-scripted-to-canvas (slurp path) "" 1024
                                                            :min-height 400 :max-height 8000
                                                            :base (cdr entry))
                             (list :ok cv ctx))
                         (error (e) (list :crash e))))
                     (log (get-output-stream-string *error-output*)))
                (setf errs (count #\Newline log))
                (incf errors errs)
                (let ((ms (round (* 1000 (/ (- (get-internal-real-time) start)
                                            internal-time-units-per-second)))))
                  (if (eq (first result) :ok)
                      (destructuring-bind (cv ctx) (rest result)
                        (format t "~16a OK  ~4ax~5a  ink=~,3f  scripts=~2a  errs=~a  ~ams~%"
                                (car entry) (r:canvas-width cv) (r:canvas-height cv)
                                (r:canvas-ink cv)
                                (length (dom:get-elements-by-tag-name (s:context-document ctx) "script"))
                                errs ms))
                      (progn (incf fail)
                             (format t "~16a CRASH ~a~%" (car entry) (second result))))))))))
    (format t "~&corpus: ~a page(s) crashed, ~a uncaught script error(s)~%" fail errors)
    (values (zerop fail) fail)))

(run)
