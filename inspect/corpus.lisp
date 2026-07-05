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
(asdf:load-system "weft/fetch")

(defpackage #:weft.corpus
  (:use #:cl) (:local-nicknames (#:s #:weft.script) (#:r #:weft.render)
                                (#:h #:weft.html) (#:dom #:weft.dom)
                                (#:f #:weft.fetch))
  (:export #:run))
(in-package #:weft.corpus)

;;; A weft/fetch-backed subresource loader for the corpus.  It resolves external
;;; stylesheets to their real bytes so a vendored page renders as designed, and
;;; serves ONLY :css so the run stays a deterministic styling smoke test (no
;;; external scripts, frames or images run).  Fetched sheets are cached on disk
;;; (inspect/vectors/pages/corpus/assets/) so repeat runs are offline/repeatable;
;;; any fetch failure returns (values nil nil) and the page degrades to UA styling.
(defparameter *assets-dir*
  (asdf:system-relative-pathname "weft" "inspect/vectors/pages/corpus/assets/"))

(defun cache-path (url)
  (merge-pathnames (format nil "~(~36r~).cache" (logand (sxhash url) #xffffffffffff)) *assets-dir*))

(defun cache-read (path)
  "Read a cached (kind . content) pair, or NIL.  First line is the kind keyword."
  (when (probe-file path)
    (handler-case
        (let ((s (with-open-file (in path :external-format :utf-8)
                   (let ((buf (make-string (file-length in))))
                     (subseq buf 0 (read-sequence buf in))))))
          (let ((nl (position #\Newline s)))
            (when nl (cons (intern (string-upcase (subseq s 0 nl)) :keyword)
                           (subseq s (1+ nl))))))
      (error () nil))))

(defun cache-write (path kind content)
  (ignore-errors
   (ensure-directories-exist path)
   (with-open-file (out path :direction :output :if-exists :supersede
                             :if-does-not-exist :create :external-format :utf-8)
     (format out "~(~a~)~%" (symbol-name kind))
     (write-string content out))))

(defun content-kind (headers url)
  "Classify a fetched resource by Content-Type, falling back to URL extension."
  (let ((ct (f:get-header headers "content-type")))
    (cond
      ((and ct (search "css" ct :test #'char-equal)) :css)
      ((and ct (or (search "javascript" ct :test #'char-equal)
                   (search "ecmascript" ct :test #'char-equal))) :js)
      ((and ct (search "html" ct :test #'char-equal)) :html)
      ((and ct (or (search "svg" ct :test #'char-equal)
                   (search "xml" ct :test #'char-equal))) :xml)
      ((and ct (search "image/" ct :test #'char-equal)) :image)
      ((search ".css" url :test #'char-equal) :css)
      (t :text))))

(defun fetch-loader ()
  "Loader closure (ctx url) -> (values kind content).  Serves only :css."
  (lambda (ctx url) (declare (ignore ctx))
    (handler-case
        (let* ((cpath (cache-path url)) (hit (cache-read cpath)))
          (if hit
              (if (eq (car hit) :css) (values :css (cdr hit)) (values nil nil))
              (multiple-value-bind (text cs resp) (f:fetch-text url)
                (declare (ignore cs))
                (if (and text (<= 200 (f:response-status resp) 299))
                    (let ((k (content-kind (f:response-headers resp) url)))
                      (cache-write cpath k text)
                      (if (eq k :css) (values :css text) (values nil nil)))
                    (values nil nil)))))
      (error () (values nil nil)))))

;;; file -> the URL it was fetched from (base for location / URL resolution)
(defparameter *corpus*
  '(("cern.html"      . "http://info.cern.ch/hypertext/WWW/TheProject.html")
    ("mfw.html"       . "http://motherfuckingwebsite.com/")
    ("danluu.html"    . "https://danluu.com/")
    ("python.html"    . "https://docs.python.org/3/")
    ("hn.html"        . "https://news.ycombinator.com/")
    ("wikipedia.html" . "https://en.wikipedia.org/wiki/HTML")
    ("bbc.html"       . "https://www.bbc.com/news")
    ("svg-inline.html"  . "http://weft.local/svg-inline.html")
    ("canvas-draw.html" . "http://weft.local/canvas-draw.html")))

(defun slurp (p)
  "Read P's bytes and decode them the way the fetch path does (the corpus pages are
UTF-8), so the smoke test renders real text instead of latin-1 mojibake (– · “ ” etc.)."
  (let ((bytes (with-open-file (in p :element-type '(unsigned-byte 8))
                 (let ((b (make-array (file-length in) :element-type '(unsigned-byte 8))))
                   (read-sequence b in) b))))
    (weft.encoding:decode "utf-8" bytes)))

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
                                                            :base (cdr entry)
                                                            :loader (fetch-loader))
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
