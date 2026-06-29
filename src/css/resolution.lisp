;;;; src/css/resolution.lisp — <resolution> value parser.
(in-package #:weft.css)

(define-value-parser "resolution" (s)
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
            ;; must start with a digit or dot
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
            ;; optional exponent: only consume if followed by digit or sign+digit
            (when (and (< pos len) (char= (char lower pos) #\e))
              (let ((next (1+ pos)))
                (when (and (< next len)
                           (or (digit-char-p (char lower next))
                               (and (< (1+ next) len)
                                    (or (char= (char lower next) #\-)
                                        (char= (char lower next) #\+))
                                    (digit-char-p (char lower (1+ next))))))
                  (incf pos)  ; consume 'e'
                  (when (and (< pos len)
                             (or (char= (char lower pos) #\-)
                                 (char= (char lower pos) #\+)))
                    (incf pos))
                  (when (or (>= pos len) (not (digit-char-p (char lower pos))))
                    (return-from nil :invalid))
                  (loop while (and (< pos len) (digit-char-p (char lower pos)))
                        do (incf pos)))))
            ;; now pos is the boundary between number and unit
            (let* ((num-str (subseq lower 0 pos))
                   (unit-str (subseq lower pos))
                   (value (ignore-errors (read-from-string num-str))))
              (if (or (null value) (not (numberp value)))
                  :invalid
                  (if (string= unit-str "")
                      :invalid   ;; bare number -> :invalid per CSS spec for <resolution>
                      (if (member unit-str '("dpi" "dpcm" "dppx" "x") :test #'string=)
                          (list (float value 0d0) unit-str)
                          :invalid)))))))))
