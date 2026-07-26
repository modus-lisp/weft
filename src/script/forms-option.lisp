;;;; forms-option.lisp — HTMLElement IDL surface for <option> (WPT swarm unit).
;;;;
;;;; Self-registering feature file.  Uses the -for variants exclusively so it
;;;; never clobbers sibling files' accessors on the shared element prototype.
(in-package #:weft.script)

(defun install-forms-option (ctx ep)
  (declare (ignorable ctx ep))
  (macrolet ((n (this) `(require-node ctx ,this)))
    (labels
        ((option-options-list (node)
           "Walk all option descendants of <select> in tree order. Block at
            <option>, <hr>, <select> elements.  For <optgroup>, block only
            if we are already inside an optgroup (prevents nested optgroups)."
           (let ((result nil))
             (labels ((walk (n inside-optgroup)
                        (loop for c across (h:dnode-children n)
                              do (let ((tag (h:dnode-name c)))
                                   (cond ((string= tag "option")
                                          (push c result))
                                         ((string= tag "optgroup")
                                          (unless inside-optgroup
                                            (walk c t)))
                                         ((member tag '("hr" "select") :test #'string=)
                                          nil) ; stop here
                                         (t (walk c inside-optgroup)))))))
               (walk node nil))
             (nreverse result)))
         (option-skip-element-p (node)
           "Returns T if the element is a <script> or <style> in the HTML or SVG
            namespace (these should be skipped when computing option text)."
           (let ((tag (h:dnode-name node)))
             (when (or (string= tag "script") (string= tag "style"))
               (let ((real-ns (element-real-ns ctx node)))
                 (or (string= real-ns *html-ns*)
                     (string= real-ns "http://www.w3.org/2000/svg"))))))
         (option-text-content (node)
           "The 'option text content': concatenation of descendant text nodes,
            skipping <script> and <style> elements in the HTML and SVG namespaces
            (but recursing into MathML and null-namespace ones), then stripping
            leading/trailing ASCII whitespace and collapsing internal ASCII
            whitespace sequences to a single space."
           (let ((parts nil))
             (labels ((walk (n)
                        (let ((kind (h:dnode-kind n)))
                          (cond ((eq kind :text)
                                 (push (or (h:dnode-data n) "") parts))
                                ((eq kind :element)
                                 (unless (option-skip-element-p n)
                                   (loop for c across (h:dnode-children n)
                                         do (walk c))))))))
               (walk node))
             (let* ((raw (apply #'concatenate 'string (nreverse parts)))
                    (stripped (string-trim '(#\Space #\Tab #\Newline #\Return #\Page)
                                          raw)))
               ;; Collapse internal ASCII whitespace runs to a single space
               (let ((out (make-array (length stripped) :element-type 'character
                                      :fill-pointer 0 :adjustable t))
                     (in-whitespace nil))
                 (loop for ch across stripped
                       do (cond ((member ch '(#\Space #\Tab #\Newline #\Return #\Page)
                                         :test #'char=)
                                 (unless in-whitespace
                                   (vector-push-extend #\Space out)
                                   (setf in-whitespace t)))
                                (t (vector-push-extend ch out)
                                   (setf in-whitespace nil))))
                 (coerce out 'string)))))
         (option-dirty-p (node)
           "Returns whether the option's selectedness is dirty (set via the IDL setter)."
           (dom:has-attribute node "data-option-dirty"))
         (option-selected-internal (node)
           "Returns the internal selectedness (used when dirty)."
           (dom:has-attribute node "weft-selected"))
         (option-selected-normal-p (node)
           "Returns whether the option is selected normally (not dirty, no constructor override)."
           (or (dom:has-attribute node "selected")
               (let ((parent (h:dnode-parent node)))
                 (and parent
                      (string= (h:dnode-name parent) "select")
                      (not (dom:has-attribute parent "multiple"))
                      (not (find-if (lambda (o) (dom:has-attribute o "selected"))
                                    (option-options-list parent)))
                      (eq (car (option-options-list parent)) node)))))
         (option-selected-p (node)
           "Returns whether the option is selected, respecting dirty flag and constructor init."
           (let ((init-sel (get-attr node "data-option-init-sel")))
             (cond ((and init-sel (not (dom:has-attribute node "selected")))
                    ;; Constructor-set initial selectedness, content attr not touched
                    (string= init-sel "1"))
                   (init-sel
                    ;; Content attr has been touched, clear the constructor flag
                    (remove-attr node "data-option-init-sel")
                    (if (option-dirty-p node)
                        (option-selected-internal node)
                        (option-selected-normal-p node)))
                   ((option-dirty-p node)
                    (option-selected-internal node))
                   (t
                    (option-selected-normal-p node)))))
         (option-set-selected (node val)
           "Set selectedness AND mark dirty."
           (remove-attr node "data-option-init-sel")  ; Clear constructor flag
           (set-attr node "data-option-dirty" "")
           (if (js:js-truthy val)
               (progn
                 (set-attr node "selected" "")      ; Set content attr for select code
                 (set-attr node "weft-selected" "1") ; Set internal value for dirty flag
                 (let ((parent (h:dnode-parent node)))
                   (when (and parent (string= (h:dnode-name parent) "select")
                              (not (dom:has-attribute parent "multiple")))
                     (dolist (o (option-options-list parent))
                       (unless (eq o node)
                         (remove-attr o "selected")      ; Clear content attr on others
                         (remove-attr o "weft-selected"))))))
               (progn
                 (remove-attr node "selected")          ; Clear content attr
                 (remove-attr node "weft-selected")))    ; Clear internal value
           (setf (context-dirty ctx) t))
         (option-index (node)
           (let ((parent (h:dnode-parent node)))
             (cond ((null parent) 0)
                   ((string= (h:dnode-name parent) "select")
                    (let ((pos (position node (option-options-list parent) :test #'eq)))
                      (if pos pos 0)))
                   (t 0))))
         (option-get-attr-non-ns (node name)
           "Get a non-namespaced attribute value."
           (let ((cell (assoc name (h:dnode-attrs node) :test #'string=)))
             (when cell
               (let ((rec (gethash cell (context-attr-recs ctx))))
                 (if (or (null rec) (null (attr-rec-ns rec)))
                     (cdr cell)
                     nil)))))
         (option-form-element (node)
           "HTMLOptionElement.form: if the option's parent is a <select>, or an
            <optgroup> whose parent is a <select>, return THAT SELECT's form
            owner; otherwise null.  The subject is the select, not the option —
            an option is not itself a form-associated element — and the lookup
            goes through the shared ELEMENT-FORM-OWNER, so a select carrying a
            `form' content attribute pointing elsewhere is honoured."
           (let* ((parent (h:dnode-parent node))
                  (select (cond ((null parent) nil)
                                ((string= (h:dnode-name parent) "select") parent)
                                ((string= (h:dnode-name parent) "optgroup")
                                 (let ((gp (h:dnode-parent parent)))
                                   (when (and gp (string= (h:dnode-name gp) "select"))
                                     gp))))))
             (if select
                 (let ((form (element-form-owner ctx select)))
                   (if form (wrap ctx form) js:*null*))
                 js:*null*))))
      ;; HTMLOptionElement.text — getter/setter
      (defgetset-for ctx ep "option" "text" (this)
        (let ((node (n this)))
          (option-text-content node))
        (v) (let ((node (n this)))
              (let ((new-text (jstr v)))
                ;; Remove all children
                (loop for c across (h:dnode-children node)
                      do (h:dom-remove c))
                ;; If new text is non-empty, add a text node
                (unless (string= new-text "")
                  (h:dom-append node (h:make-text new-text)))
                (setf (context-dirty ctx) t))))
      ;; HTMLOptionElement.value — getter/setter
      (defgetset-for ctx ep "option" "value" (this)
        (let ((node (n this)))
          (or (option-get-attr-non-ns node "value") (option-text-content node)))
        (v) (let ((node (n this)))
              (set-attr node "value" (jstr v))
              (setf (context-dirty ctx) t)))
      ;; HTMLOptionElement.label — getter/setter
      (defgetset-for ctx ep "option" "label" (this)
        (let ((node (n this)))
          (or (option-get-attr-non-ns node "label") (option-text-content node)))
        (v) (let ((node (n this)))
              (set-attr node "label" (jstr v))
              (setf (context-dirty ctx) t)))
      ;; HTMLOptionElement.index — getter
      (defget-for ctx ep "option" "index" (this)
        (let ((node (n this)))
          (num (option-index node))))
      ;; HTMLOptionElement.selected — getter/setter with dirty flag
      (defgetset-for ctx ep "option" "selected" (this)
        (let ((node (n this)))
          (jbool (option-selected-p node)))
        (v) (let ((node (n this)))
              (option-set-selected node v)))
      ;; HTMLOptionElement.defaultSelected — reflects the selected content attribute
      (defgetset-for ctx ep "option" "defaultSelected" (this)
        (let ((node (n this)))
          (jbool (dom:has-attribute node "selected")))
        (v) (let ((node (n this)))
              (if (js:js-truthy v)
                  (set-attr node "selected" "")
                  (remove-attr node "selected"))
              (setf (context-dirty ctx) t)))
      ;; HTMLOptionElement.form — getter
      (defget-for ctx ep "option" "form" (this)
        (let ((node (n this)))
          (option-form-element node))))))

(register-element-proto-extension :option #'install-forms-option)