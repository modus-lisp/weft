;;;; src/dom/by-class.lisp — getElementsByClassName.
(in-package #:weft.dom)

(defun class-contains-p (class-value class)
  "Return T if CLASS-VALUE (a string), split on ASCII whitespace,
contains CLASS as a token."
  (let ((start 0)
        (end (length class-value)))
    (loop
      (when (>= start end) (return nil))
      ;; skip whitespace
      (loop while (and (< start end)
                       (or (char= (char class-value start) #\Space)
                           (char= (char class-value start) #\Tab)
                           (char= (char class-value start) #\Newline)
                           (char= (char class-value start) #\Return)
                           (char= (char class-value start) #\FormFeed)))
            do (incf start))
      (when (>= start end) (return nil))
      ;; find end of token
      (let ((token-start start))
        (loop while (and (< start end)
                         (not (or (char= (char class-value start) #\Space)
                                  (char= (char class-value start) #\Tab)
                                  (char= (char class-value start) #\Newline)
                                  (char= (char class-value start) #\Return)
                                  (char= (char class-value start) #\FormFeed))))
              do (incf start))
        (when (string= class-value class :start1 token-start :end1 start)
          (return t))))))

(defun get-elements-by-class-name (root class)
  "Return a list (tree order) of element descendants of ROOT whose class
attribute (whitespace-separated tokens) contains CLASS."
  (let ((result nil))
    ;; recursive preorder walk, collecting into result via tail
    (labels ((walk (node)
               (when (eql (h:dnode-kind node) :element)
                 (let ((class-val (cdr (assoc "class" (h:dnode-attrs node) :test #'string=))))
                   (when (and class-val (class-contains-p class-val class))
                     (push node result))))
               (loop for child across (h:dnode-children node)
                     do (walk child))))
      (walk root))
    (nreverse result)))