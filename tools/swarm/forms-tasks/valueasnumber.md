Implement the `valueAsNumber` IDL attribute (getter + setter) on `<input>`.

WPT files under $WPT_ROOT/html/semantics/forms/the-input-element/ that the
oracle runs (READ them for exact expectations):
  input-valueasnumber.html, input-valueasnumber-stepping.html,
  input-valueasnumber-typeerror.html, input-valueasnumber-invalidstateerr.html

Per the HTML spec, `valueAsNumber` maps the control's value string to a Double
using each type's "algorithm to convert a string to a number" (and back):

- Types that DO NOT support valueAsNumber (text, search, tel, url, email,
  password, checkbox, radio, file, submit, image, reset, button, color, hidden):
    getter returns NaN.
    setter THROWS InvalidStateError:  (throw-dom ctx "InvalidStateError" 11 "...")
- number, range:  value parsed as a floating-point number (the HTML "valid
  floating-point number" grammar). Invalid/empty => getter NaN. Setter: set the
  value to the "best representation of the number as a floating-point number"
  (for an integer-valued double, no trailing ".0"; NaN passed to the setter =>
  set value to "" ... actually if the double is not finite the setter throws
  TypeError — check input-valueasnumber-typeerror.html).
- date:  value "YYYY-MM-DD" => milliseconds from 1970-01-01 UTC to that date at
  00:00 UTC. Setter: ms => that date string (ms must land on a day boundary is
  NOT required; floor to the day).
- month: value "YYYY-MM" => number of MONTHS between 1970-01 and that month
  (can be negative). Setter: month-count => "YYYY-MM".
- week:  value "YYYY-Www" => ms from epoch to 00:00 UTC of the Monday of that ISO
  week. Setter: ms => "YYYY-Www".
- time:  value "HH:MM" / "HH:MM:SS" / "HH:MM:SS.sss" => ms from midnight. Setter:
  ms-of-day => shortest valid time string.
- datetime-local: "YYYY-MM-DDTHH:MM[:SS[.sss]]" => ms from epoch (treat the local
  time AS IF UTC — no zone). Setter: ms => that string.

Setter with a non-finite Double (NaN/Inf) on a numeric type => throw TypeError
(see the typeerror test). Setter on an unsupported type => InvalidStateError.

Implementation notes:
- Write your own integer date math (days-from-civil / civil-from-days,
  Howard-Hinnant style) as `valueasnumber-` helpers — do NOT call the JS Date.
- Get the current value string with the `-cur-value` helper shown in the header;
  set it with (setf (gethash node (context-input-values ctx)) "STR").
- Build the returned number with (num x); NaN is (num (/ 0d0 0d0)) or use
  sb-ext:double-float-positive-infinity math — simplest: (num (coerce
  <ratio-or-int> 'double-float)); for NaN return (js:nan) if shuttle exports it,
  else compute (let ((z 0d0)) (num (- z z)))-style is wrong; instead keep a
  constant (defvar +nan+ (- sb-ext:double-float-positive-infinity
  sb-ext:double-float-positive-infinity)).
- The setter receives the JS value `v`; coerce with (js:to-number v) => a double.
