;;;; src/css/length.lisp
(in-package #:weft.css)

(define-value-parser "length" (s)
  (let* ((trimmed (css-trim s))
         (lower (ascii-downcase trimmed))
         (len (length lower)))
    (if (zerop len)
        :invalid
        (block nil
          (let ((pos 0))
            ;; optional sign
            (when (and (< pos len)
                       (or (char= (char lower pos) #\-)
                           (char= (char lower pos) #\+)))
              (incf pos))
            ;; must start with a digit or a dot
            (when (or (>= pos len)
                      (not (or (digit-char-p (char lower pos))
                               (char= (char lower pos) #\.))))
              (return-from nil :invalid))
            ;; integer part
            (loop while (and (< pos len) (digit-char-p (char lower pos)))
                  do (incf pos))
            ;; optional fractional part
            (when (and (< pos len) (char= (char lower pos) #\.))
              (incf pos)
              (loop while (and (< pos len) (digit-char-p (char lower pos)))
                    do (incf pos)))
            ;; optional exponent (scientific notation) — only if 'e' is followed by
            ;; a digit or sign+digit (otherwise it's part of the unit, e.g. "1.5em")
            (when (and (< pos len) (char= (char lower pos) #\e))
              (let ((next (1+ pos)))
                (when (and (< next len)
                           (or (digit-char-p (char lower next))
                               (and (< (1+ next) len)
                                    (or (char= (char lower next) #\-)
                                        (char= (char lower next) #\+))
                                    (digit-char-p (char lower (1+ next))))))
                  (incf pos) ; consume 'e'
                  (when (and (< pos len)
                             (or (char= (char lower pos) #\-)
                                 (char= (char lower pos) #\+)))
                    (incf pos))
                  (when (or (>= pos len) (not (digit-char-p (char lower pos))))
                    (return-from nil :invalid))
                  (loop while (and (< pos len) (digit-char-p (char lower pos)))
                        do (incf pos)))))
            ;; now split into number and unit
            (let* ((num-str (subseq lower 0 pos))
                   (unit-str (subseq lower pos))
                   (value (safe-number num-str)))
              (if (string= unit-str "")
                  ;; no unit: only value 0 is valid
                  (if (= value 0)
                      (list (float value 0d0) "")
                      :invalid)
                  ;; has unit: check against valid CSS absolute/relative units
                  (if (member unit-str
                              '("px" "em" "rem" "ex" "ch"
                                "vw" "vh" "vmin" "vmax"
                                "cm" "mm" "in" "pt" "pc" "q")
                              :test #'string=)
                      (list (float value 0d0) unit-str)
                      :invalid))))))))
