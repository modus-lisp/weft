Implement the `stepUp(n)` and `stepDown(n)` methods on `<input>`.

WPT files (READ them under $WPT_ROOT/html/semantics/forms/the-input-element/):
  input-stepup.html, input-stepdown.html, input-stepdown-02.html

Both are methods taking an optional integer `n` (default 1); stepDown(n) ==
stepUp(-n).  Install with defmethod*:
  (defmethod* ctx ep "stepUp"   1 (this a) (stepupdown-step ctx (n this) (arg-n a 0 1))  js:*undefined*)
  (defmethod* ctx ep "stepDown" 1 (this a) (stepupdown-step ctx (n this) (- (arg-n a 0 1))) js:*undefined*)
where (arg-n a i default) = (let ((x (arg a i))) (if (eq x js:*undefined*) default
  (truncate (js:to-number x)))).

Spec "step up/down" algorithm (HTML §common-input-element-apis):
1. If the type does NOT support `step` (text, checkbox, radio, etc.), throw
   InvalidStateError.  Supported: number, range, date, month, week, time,
   datetime-local.
2. Compute:
   - allowed value step:  from the `step` content attribute. "any" => there is
     no allowed step => throw InvalidStateError. Missing/invalid => the type's
     DEFAULT step. Then multiply by the type's step scale factor to get an
     integer-space step. Default steps & scale factors:
       number: default 1, scale 1
       range:  default 1, scale 1
       date:   default 1 day,   scale 86400000 ms   (step counts DAYS)
       time:   default 60 s,    scale 1000 ms       (step counts SECONDS)
       week:   default 1 week,  scale 604800000 ms
       month:  default 1 month, scale 1             (step counts MONTHS)
       datetime-local: default 60 s, scale 1000 ms
   - step base: from `min` (or the type default 0 / for week the epoch Monday).
   - min, max: parsed from attributes (in the type's number space).
3. value = the control's current numeric value (its "algorithm to convert a
   string to a number"). If empty/invalid => 0 (or for range, the default).
4. If value is not an integral multiple of step above step-base, first SNAP it to
   the nearest step boundary in the direction of n (spec step 6-9 has the exact
   rounding); then add n*step.
5. Clamp: if result < min (when min present) and we can't reach, throw
   InvalidStateError per the spec's specific conditions; if > max, likewise.
   (Read input-stepup.html / input-stepdown-02.html for the exact boundary and
   throw expectations — they enumerate many cases.)
6. Set the control's value to the string for the new number (reuse per-type
   number->string formatting; write your own `stepupdown-` helpers).

Do all math in the integer/rational number space (CL rationals keep it exact),
then format. Write per-type parse/format as `stepupdown-` helpers; do not call
the JS Date. Get/set the value via the header's `-cur-value` helper and
(setf (gethash node (context-input-values ctx)) ...).
