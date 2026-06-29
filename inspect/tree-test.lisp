;;;; inspect/tree-test.lisp — html5lib tree-construction differential gate.
;;;;
;;;; Parses the .dat suite (inspect/vectors/html/tree/*.dat): for each non-
;;;; fragment test, PARSE-HTML of #data must serialize to the #document tree.
;;;; (Fragment tests — those with #document-fragment — are skipped for now.)
(defpackage #:weft.html.tree-test (:use #:cl) (:local-nicknames (#:h #:weft.html)) (:export #:run))
(in-package #:weft.html.tree-test)

(defun read-lines (path)
  (with-open-file (s path :external-format :utf-8)
    (loop for line = (read-line s nil :eof) until (eq line :eof) collect line)))

(defstruct tcase data document fragment)

(defun parse-dat (path)
  "Parse an html5lib .dat file into a list of TCASEs."
  (let ((lines (read-lines path)) (cases '()) (cur nil) (sect nil) (data '()) (doc '()) (frag nil))
    (flet ((flush () (when cur (setf (tcase-data cur) (format nil "~{~a~^~%~}" (reverse data))
                                     (tcase-document cur) (reverse doc)
                                     (tcase-fragment cur) frag)
                       (push cur cases))))
      (dolist (line lines)
        (cond
          ((string= line "#data") (flush) (setf cur (make-tcase) sect :data data '() doc '() frag nil))
          ((string= line "#errors") (setf sect :errors))
          ((string= line "#new-errors") (setf sect :errors))
          ((string= line "#document-fragment") (setf sect :fragment))
          ((string= line "#document") (setf sect :document))
          ((and (>= (length line) 1) (char= (char line 0) #\#)) (setf sect :other))
          (t (case sect
               (:data (push line data))
               (:fragment (setf frag line))
               (:document (if (string= line "") (setf sect :none) (push line doc)))))))
      (flush))
    (nreverse cases)))

(defun run (&optional only)
  (let* ((dir (asdf:system-relative-pathname "weft" "inspect/vectors/html/tree/"))
         (files (sort (directory (merge-pathnames "*.dat" dir)) #'string< :key #'namestring))
         (pass 0) (fail 0) (skip 0) (fails '()))
    (format t "~&=== weft HTML tree-construction gate (html5lib) ===~%")
    (dolist (f files)
      (when (or (null only) (search only (pathname-name f)))
        (let ((fp 0) (ff 0) (fs 0))
          (dolist (tc (parse-dat f))
            (cond
              ((tcase-fragment tc) (incf fs))
              (t (let* ((want (format nil "~{~a~%~}" (tcase-document tc)))
                        (got (handler-case (h:serialize-tree (h:parse-html (tcase-data tc)))
                               (error (e) (format nil "ERROR ~a" e)))))
                   (if (string= got want) (incf fp)
                       (progn (incf ff)
                              (when (< (length fails) 12)
                                (push (format nil "[~a] ~s~%   want|~a   got |~a"
                                              (pathname-name f) (tcase-data tc)
                                              (substitute #\| #\Newline want) (substitute #\| #\Newline got)) fails))))))))
          (incf pass fp) (incf fail ff) (incf skip fs)
          (format t "  ~a ~12a ~4d/~d  (~d fragment skipped)~%" (if (zerop ff) "ok  " "FAIL") (pathname-name f) fp (+ fp ff) fs))))
    (format t "~%~d passed, ~d failed, ~d fragment-skipped~%" pass fail skip)
    (when fails (format t "~%sample failures:~%~{  ~a~%~}" (reverse fails)))
    (values pass fail)))
