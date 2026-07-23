Implement constraint validation on `<input>`:
  validity (ValidityState object), willValidate, validationMessage,
  checkValidity(), reportValidity(), setCustomValidity(message).

WPT files (READ them under /home/claude/wpt/html/semantics/forms/the-input-element/):
  input-validity.html, input-checkvalidity.html, input-setcustomvalidity.html,
  input-validationmessage.html
(These currently error out entirely because the whole API is missing — even a
partial correct implementation should flip several to passing.)

The ValidityState object exposes read-only boolean getters:
  valueMissing, typeMismatch, patternMismatch, tooLong, tooShort,
  rangeUnderflow, rangeOverflow, stepMismatch, badInput, customError, valid.
Build it once per input as a plain JS object with accessor getters computed live:
  (let ((vs (js:make-object)))
    (flet ((flag (name fn) (js:put-accessor vs name
              :get (js:native-function (context-realm ctx) (concatenate 'string "get " name)
                     (lambda (self ig) (declare (ignore self ig)) (jbool (funcall fn))) 0)
              :enumerable t :configurable t)))
      (flag "valueMissing" (lambda () ...)) ... (flag "valid" (lambda () ...)))
    vs)
Cache the object per node in a hash you close over, so `input.validity ===
input.validity`.

Custom validity message: store per node in a closed-over hash; setCustomValidity
sets it (a non-empty custom message makes customError true and the element
invalid).

Flag computation (implement the ones the tests exercise; read the files):
  valueMissing: `required` present AND value is empty (for checkbox/radio:
    not checked).
  typeMismatch: type=email/url and value doesn't match that type's syntax.
  patternMismatch: `pattern` attr present, value non-empty, and value does NOT
    fully match the pattern (anchored). NOTE: no regex lib — for the tests, a
    simple matcher may suffice, or skip patternMismatch if too costly and report.
  tooLong: value length > maxLength (only when value was set by the user — for
    the API path, compare directly). tooShort: length < minLength & non-empty.
  rangeUnderflow/Overflow: numeric value < min / > max.
  stepMismatch: numeric value not aligned to step from step-base.
  badInput: type=number and the value isn't a valid number.
  customError: custom message non-empty.
  valid: none of the above true AND the element is a candidate for constraint
    validation.

willValidate (getter, boolean): true when the element is a submittable candidate
  for constraint validation (NOT disabled, NOT readOnly, type not
  hidden/reset/button, not inside a datalist). validationMessage (string): "" if
  valid or not willValidate, else the customError message or a UA message.

checkValidity(): returns (jbool (valid?)); if invalid, fire an "invalid" event
  (you may skip firing if event plumbing is unclear — return the boolean, which
  is what the tests assert). reportValidity(): same boolean.
setCustomValidity(msg): store (jstr msg); returns js:*undefined*.

Prefix helpers `validity-`. Reuse the header's `-cur-value` for the value string.
Start with valid/valueMissing/customError/checkValidity/setCustomValidity/
willValidate/validationMessage (cheap, high pass yield); add range/step/type
mismatches as budget allows.
