Implement the `valueAsDate` IDL attribute (getter + setter) on `<input>`.

WPT files (READ them under $WPT_ROOT/html/semantics/forms/the-input-element/):
  input-valueasdate.html, input-valueasdate-typeerror.html,
  input-valueasdate-invalidstateerr.html, input-valueasdate-stepping.html,
  datetime-local-valueasdate.html

`valueAsDate` returns a JS Date object (or null), for the types that support it:
  date, month, week, time.  (datetime-local does NOT support valueAsDate — its
  test expects the getter to throw/return per spec; check datetime-local-
  valueasdate.html for the exact expectation.)

- Unsupported types: getter returns null (js:*null*); setter THROWS
  InvalidStateError: (throw-dom ctx "InvalidStateError" 11 "...").
- Supported types, getter: parse the current value string to a moment in UTC and
  return a NEW Date for those milliseconds:
    (js:js-construct (js:eval-script (context-realm ctx) "Date") (list (num ms)))
  If the value is empty/invalid, return null.
    date "YYYY-MM-DD"  => 00:00:00 UTC of that day.
    month "YYYY-MM"    => 00:00:00 UTC of the FIRST day of that month.
    week "YYYY-Www"    => 00:00:00 UTC of the Monday of that ISO week.
    time "HH:MM[:SS[.sss]]" => that time on 1970-01-01 UTC (ms-of-day).
- Setter: the JS value is a Date object or null.
    null => set the value to "".
    a Date => read its time in ms:
      (js:to-number (js:js-invoke dateobj "getTime" '()))   ; ms since epoch
      (if such a helper name differs, use (js:get-method)/(js:call) — inspect
       shuttle's value.lisp exports; `js-invoke`/`js-call-method` style).
    Convert ms back to the type's value string and store it (for month, snap to
    the containing month; for week, to the containing ISO week; for time, take
    ms-of-day). A Date whose time is NaN => setter throws TypeError (see the
    typeerror test).

Notes:
- Do the ms<->Y/M/D math yourself (integer days-from-civil), as `valueasdate-`
  helpers. 86400000 ms/day, 3600000 ms/hour.
- To read a Date's getTime, first find shuttle's method-call export:
  run `grep -n "defun js-invoke\|js-call-method\|js-call\b\|call-method" ` over
  ../shuttle/src/value.lisp and use whatever exists; fall back to
  (js:to-number (js:js-invoke d "valueOf" nil)).
