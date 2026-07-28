;;;; forms-files.lisp — HTMLInputElement.files (wave-10 swarm unit).
;;;;
;;;; Self-registering feature file (see core.lisp *element-proto-extensions*).
;;;;
;;;; What this file is for
;;;; ---------------------
;;;; `files' is the filename value mode's IDL surface.  The rule is narrow and
;;;; entirely testable without any file picker:
;;;;
;;;;   - For type=file, `files' is a FileList object.  With nothing selected it
;;;;     is an EMPTY FileList (length 0) — not null.
;;;;   - For EVERY other input type it is null.  files.html walks all 21 other
;;;;     types and asserts null for each, which is most of its 24 subtests.
;;;;   - The getter must keep returning the SAME FileList object across reads
;;;;     for the same element (identity is asserted), and the list must survive
;;;;     a `type' change away and back only per the filename-mode rules.
;;;;   - Setting `files' is allowed (it is not readonly); assigning null or a
;;;;     non-FileList is a no-op rather than a throw.
;;;;
;;;; FileList itself needs `length', indexed access, `item(i)', and
;;;; @@toStringTag — the tests do `assert_class_string(input.files, "FileList")'
;;;; before they look at anything else, so a FileList without a shared prototype
;;;; carrying the tag scores zero on files it otherwise satisfies.  That exact
;;;; trap cost radio-valueMissing.html 6 subtests in wave 8; see
;;;; `make-validity-state' in core.lisp for the shape that fixed it.
;;;;
;;;; Nothing here needs a real file: no test in the aperture selects one.  Do
;;;; not build a File object with fabricated size/lastModified values to make a
;;;; subtest pass — an empty FileList is the honest answer and it is also the
;;;; one the tests ask for.
;;;;
;;;; Take over the accessor with the `-for' variant, as the sibling feature
;;;; files do; the previous accessor stays reachable through PREVIOUS-ACCESSOR,
;;;; so this file never has to edit shared core.
(in-package #:weft.script)

(defun install-forms-files (ctx ep)
  (declare (ignorable ctx ep))
  nil)

(register-element-proto-extension :files #'install-forms-files)
