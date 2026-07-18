;;;; first-letter punctuation classification (CSS 2.1 §5.12.2).
;;;; Auto-generated: Unicode general categories Ps, Pe, Pi, Pf, Po as sorted
;;;; non-overlapping [lo hi] code-point ranges (194 ranges).
(in-package #:weft.render)

(declaim (type (simple-array (unsigned-byte 32) (*)) +first-letter-punct-ranges+))
(defparameter +first-letter-punct-ranges+
  (make-array 388 :element-type '(unsigned-byte 32)
              :initial-contents
              (list
    #x21 #x23 #x25 #x2A #x2C #x2C #x2E #x2F #x3A #x3B #x3F #x40 #x5B #x5D #x7B #x7B #x7D #x7D #xA1 #xA1 #xA7
    #xA7 #xAB #xAB #xB6 #xB7 #xBB #xBB #xBF #xBF #x37E #x37E #x387 #x387 #x55A #x55F #x589 #x589 #x5C0 #x5C0
    #x5C3 #x5C3 #x5C6 #x5C6 #x5F3 #x5F4 #x609 #x60A #x60C #x60D #x61B #x61B #x61D #x61F #x66A #x66D #x6D4
    #x6D4 #x700 #x70D #x7F7 #x7F9 #x830 #x83E #x85E #x85E #x964 #x965 #x970 #x970 #x9FD #x9FD #xA76 #xA76
    #xAF0 #xAF0 #xC77 #xC77 #xC84 #xC84 #xDF4 #xDF4 #xE4F #xE4F #xE5A #xE5B #xF04 #xF12 #xF14 #xF14 #xF3A
    #xF3D #xF85 #xF85 #xFD0 #xFD4 #xFD9 #xFDA #x104A #x104F #x10FB #x10FB #x1360 #x1368 #x166E #x166E #x169B
    #x169C #x16EB #x16ED #x1735 #x1736 #x17D4 #x17D6 #x17D8 #x17DA #x1800 #x1805 #x1807 #x180A #x1944 #x1945
    #x1A1E #x1A1F #x1AA0 #x1AA6 #x1AA8 #x1AAD #x1B5A #x1B60 #x1B7D #x1B7E #x1BFC #x1BFF #x1C3B #x1C3F #x1C7E
    #x1C7F #x1CC0 #x1CC7 #x1CD3 #x1CD3 #x2016 #x2027 #x2030 #x203E #x2041 #x2043 #x2045 #x2051 #x2053 #x2053
    #x2055 #x205E #x207D #x207E #x208D #x208E #x2308 #x230B #x2329 #x232A #x2768 #x2775 #x27C5 #x27C6 #x27E6
    #x27EF #x2983 #x2998 #x29D8 #x29DB #x29FC #x29FD #x2CF9 #x2CFC #x2CFE #x2CFF #x2D70 #x2D70 #x2E00 #x2E16
    #x2E18 #x2E19 #x2E1B #x2E2E #x2E30 #x2E39 #x2E3C #x2E3F #x2E41 #x2E4F #x2E52 #x2E5C #x3001 #x3003 #x3008
    #x3011 #x3014 #x301B #x301D #x301F #x303D #x303D #x30FB #x30FB #xA4FE #xA4FF #xA60D #xA60F #xA673 #xA673
    #xA67E #xA67E #xA6F2 #xA6F7 #xA874 #xA877 #xA8CE #xA8CF #xA8F8 #xA8FA #xA8FC #xA8FC #xA92E #xA92F #xA95F
    #xA95F #xA9C1 #xA9CD #xA9DE #xA9DF #xAA5C #xAA5F #xAADE #xAADF #xAAF0 #xAAF1 #xABEB #xABEB #xFD3E #xFD3F
    #xFE10 #xFE19 #xFE30 #xFE30 #xFE35 #xFE4C #xFE50 #xFE52 #xFE54 #xFE57 #xFE59 #xFE61 #xFE68 #xFE68 #xFE6A
    #xFE6B #xFF01 #xFF03 #xFF05 #xFF0A #xFF0C #xFF0C #xFF0E #xFF0F #xFF1A #xFF1B #xFF1F #xFF20 #xFF3B #xFF3D
    #xFF5B #xFF5B #xFF5D #xFF5D #xFF5F #xFF65 #x10100 #x10102 #x1039F #x1039F #x103D0 #x103D0 #x1056F #x1056F
    #x10857 #x10857 #x1091F #x1091F #x1093F #x1093F #x10A50 #x10A58 #x10A7F #x10A7F #x10AF0 #x10AF6 #x10B39
    #x10B3F #x10B99 #x10B9C #x10F55 #x10F59 #x10F86 #x10F89 #x11047 #x1104D #x110BB #x110BC #x110BE #x110C1
    #x11140 #x11143 #x11174 #x11175 #x111C5 #x111C8 #x111CD #x111CD #x111DB #x111DB #x111DD #x111DF #x11238
    #x1123D #x112A9 #x112A9 #x1144B #x1144F #x1145A #x1145B #x1145D #x1145D #x114C6 #x114C6 #x115C1 #x115D7
    #x11641 #x11643 #x11660 #x1166C #x116B9 #x116B9 #x1173C #x1173E #x1183B #x1183B #x11944 #x11946 #x119E2
    #x119E2 #x11A3F #x11A46 #x11A9A #x11A9C #x11A9E #x11AA2 #x11C41 #x11C45 #x11C70 #x11C71 #x11EF7 #x11EF8
    #x11FFF #x11FFF #x12470 #x12474 #x12FF1 #x12FF2 #x16A6E #x16A6F #x16AF5 #x16AF5 #x16B37 #x16B3B #x16B44
    #x16B44 #x16E97 #x16E9A #x16FE2 #x16FE2 #x1BC9F #x1BC9F #x1DA87 #x1DA8B #x1E95E #x1E95F))
  "Flat sorted pairs [lo0 hi0 lo1 hi1 ...] of Unicode code-point ranges in the
punctuation categories (Ps)(Pe)(Pi)(Pf)(Po) that ::first-letter absorbs.")

(defun first-letter-punct-p (code)
  "True when CODE (a character code point) is punctuation that ::first-letter
includes when it precedes or follows the first letter (CSS 2.1 §5.12.2)."
  (declare (type fixnum code))
  (let* ((v +first-letter-punct-ranges+) (n (length v)) (lo 0) (hi (1- (ash n -1))))
    (declare (type fixnum lo hi))
    (loop while (<= lo hi) do
      (let* ((mid (ash (+ lo hi) -1)) (a (aref v (ash mid 1))) (b (aref v (1+ (ash mid 1)))))
        (declare (type fixnum mid a b))
        (cond ((< code a) (setf hi (1- mid)))
              ((> code b) (setf lo (1+ mid)))
              (t (return-from first-letter-punct-p t)))))
    nil))
