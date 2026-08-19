Implement the text-selection API on `<input>`:
  selectionStart, selectionEnd, selectionDirection (get/set),
  setSelectionRange(start, end, [direction]), select(), setRangeText(...).

WPT file (READ it): $WPT_ROOT/html/semantics/forms/the-input-element/selection.html

These apply only to inputs that "support selection": text, search, tel, url,
password (NOT number, email, date, etc.).  For an input that does NOT support
selection:
  selectionStart / selectionEnd / selectionDirection getters return null.
  setSelectionRange / setRangeText / (and the setters) THROW InvalidStateError.

State: keep selection per node in a hash table you CLOSE OVER in the installer so
each context is independent:
  (let ((starts (make-hash-table)) (ends (make-hash-table)) (dirs (make-hash-table)))
    ... all the defgetset/defmethod* forms here, referencing those hashes ...)
Default when unset: selectionStart = selectionEnd = length of the value,
selectionDirection = "none".

Value length is (length (selection-cur-value ctx node)) using the header helper.
Clamp all indices to [0, len]; if start > end, set start = end (per spec).

- selectionStart get => the stored start (default len), as (num k).
  set => (setSelectionRange stored-start-as-given, end, dir) semantics: set start,
  clamp, if start>end then end=start.
- selectionEnd similarly.
- selectionDirection get => "forward" | "backward" | "none" (string); set stores
  it (invalid => "none").
- setSelectionRange(start,end,dir): coerce start/end with (js:to-number) then
  truncate; clamp to [0,len]; if end<start then start=end; store; dir defaults
  "none" (values other than forward/backward => "none").
- select(): setSelectionRange(0, len, "forward")  (actually direction "none" per
  spec — check selection.html).
- setRangeText(replacement, [start, end, selectMode]): replace the substring
  [start,end) (defaults = current selection) of the value with `replacement`,
  update the stored value via (setf (gethash node (context-input-values ctx)) ...),
  then adjust the selection per selectMode ("select"|"start"|"end"|"preserve",
  default "preserve"). Read selection.html for exact index expectations.

Build numbers with (num k), strings via (jstr v) for incoming JS strings, return
js:*null* for the unsupported-type getters, js:*undefined* from the methods.
Prefix any helper `selection-`.
