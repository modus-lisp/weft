;;;; src/css/resolution.lisp
(in-package #:weft.css)

(define-value-parser "resolution" (s)
  (let* ((trimmed (css-trim s))
         (lower (ascii-downcase trimmed))
         (len (length lower)))
    (if (zerop len)
        :invalid
        (block nil
          ;; parse the number — scan forward to find where numeric part ends
          (let ((pos 0))
            ;; optional sign
            (when (and (< pos len)
                       (or (char= (char lower pos) #\-)
                           (char= (char lower pos) #\+)))
              (incf pos))
            ;; must start with digit or decimal point
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
            ;; optional exponent (CSS <resolution> values may have scientific notation)
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
                   (value (read-from-string num-str)))
              (cond
                ;; bare number without unit -> invalid per CSS spec for <resolution>
                ((string= unit-str "")
                 :invalid)
                ;; check valid <resolution> units
                ((member unit-str '("dpi" "dpcm" "dppx" "x") :test #'string=)
                 (when (minusp value)
                   (return-from nil :invalid))
                 (list (float value 0d0) unit-str))
                (t
                 :invalid))))))))