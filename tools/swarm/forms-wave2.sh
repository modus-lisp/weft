#!/usr/bin/env bash
# Round-2 forms swarm: the HARD units, fanned wider with diversified variants and
# a keep-best merge.  Usage: forms-wave2.sh <wave-dir> [pool] [unit ...]
#
# Per unit we launch 3 variants (a=narrow, b=narrow-then-extend, c=full/stronger
# model).  All 3*U jobs go into ONE work list consumed by a POOL of worker slots
# (a simple pull-queue via xargs -P) so a dozen jobs drain through `pool` slots
# without oversubscribing.  After the wave, for each unit we KEEP the best
# variant (fewest failed) and re-verify it in canonical weft.
set -u
WAVE="$1"; POOL="${2:-8}"; shift 2 || shift $#
SRC=/home/claude/weft; SELF="$(cd "$(dirname "$0")" && pwd)"; TASKS="$SELF/forms-tasks"
CANON=/home/claude/weft
units=("$@"); [ ${#units[@]} -eq 0 ] && units=(valueasnumber valueasdate stepupdown selection)

# The friction round 1 hit (API discovery + chasing date math) — answer it up front.
read -r -d '' CHEAT <<'CHEAT_EOF' || true

## API cheatsheet (these are the exact answers round-1 workers wasted iterations hunting for)
- Input type:            (input-type NODE)   ; already lowercased, defaults "text"
- Current value string:  use the header's `-cur-value` helper (falls back to the value attr)
- Set current value:     (setf (gethash NODE (context-input-values ctx)) "STR")
- A JS NaN double:        (- sb-ext:double-float-positive-infinity sb-ext:double-float-positive-infinity)
                          return it as a number with (num that).  Tests use Number.isNaN, so the
                          value must be a real NaN double (not null/0).
- Format a double as JS:  (js:to-string (num X))  => canonical JS number string ("123", "1.5", not "1.0")
- Throw a DOMException:   (throw-dom ctx "InvalidStateError" 11 "msg")   ; also "TypeError" is (js:js-throw (js:make-native-error "TypeError" "msg"))
- Coerce the JS setter value: (js:to-number v) => double ; (jstr v) => string
- Numbers/booleans out:   (num x) (jbool x) js:*true* js:*false* js:*null* js:*undefined*
- Integer date math is exact with CL rationals — write your own days<->Y/M/D; do NOT call the JS Date
  except where a spec explicitly returns a Date object (valueAsDate).

## Work in the number space, format last
For number/range/date/month/week/time/datetime-local, the spec defines a
"convert a string to a number" and its inverse.  Parse -> operate as an integer
or rational -> format back.  Get ONE type fully correct and green before moving
to the next; a partially-correct file that greens several subtests is kept over
an ambitious one that greens none.
CHEAT_EOF

variant_note() { # $1=unit $2=variant  -> echo the scope/strategy note
  local u="$1" v="$2"
  case "$u" in
    valueasnumber|stepupdown)
      case "$v" in
        a) echo "SCOPE (narrow): implement ONLY type=number and type=range. For every OTHER type, the getter returns NaN (valueAsNumber) / the method throws InvalidStateError (stepUp/stepDown), and the setter throws InvalidStateError. Get number+range 100% correct — parsing, formatting, and the invalid/empty cases — then STOP. This alone should green many subtests." ;;
        b) echo "SCOPE: first make type=number and type=range fully correct and green, THEN extend to type=date and type=time (integer ms math). Leave month/week/datetime-local as unsupported (NaN / InvalidStateError) if you run low on budget." ;;
        c) echo "SCOPE (full): implement ALL supported types — number, range, date, month, week, time, datetime-local — with correct step-base/min/max handling. Read every failing subtest message and fix per the HTML spec." ;;
      esac ;;
    valueasdate)
      case "$v" in
        a) echo "SCOPE (narrow): implement ONLY type=date (getter returns a Date at 00:00 UTC of that day; setter accepts a Date/null). All other types: getter null, setter throws InvalidStateError. Get date 100% correct, then STOP." ;;
        b) echo "SCOPE: make type=date correct and green first, THEN add type=time (Date on 1970-01-01 at the ms-of-day)." ;;
        c) echo "SCOPE (full): date, month, week, time — all four supported types, getter and setter, with correct snapping (month->first day, week->Monday). Read failing messages and fix per spec." ;;
      esac ;;
    selection)
      case "$v" in
        a) echo "SCOPE (narrow): implement selectionStart, selectionEnd (get/set), setSelectionRange(start,end,dir), and select(), with the per-node closed-over hashes and clamping. SKIP setRangeText and selectionDirection edge cases for now. Only for selection-supporting types (text/search/tel/url/password); others: getters null, methods throw InvalidStateError." ;;
        b) echo "SCOPE: get selectionStart/End/setSelectionRange/select correct and green first, THEN add selectionDirection (get/set) and setRangeText." ;;
        c) echo "SCOPE (full): the entire selection API including setRangeText's four selectMode behaviours. Read every failing subtest and fix." ;;
      esac ;;
  esac
}

rm -rf "$WAVE"; mkdir -p "$WAVE"
jobs=()
for unit in "${units[@]}"; do
  for v in a b c; do
    jobid="$unit-$v"; jobs+=("$jobid")
    WD="$WAVE/$jobid"; cp -r "$SRC" "$WD"; rm -rf "$WD/.git"; find "$WD" -name '*.fasl' -delete
    ORACLE="cd $WD && XDG_CACHE_HOME=$WAVE/.cache-$jobid CL_SOURCE_REGISTRY='(:source-registry (:tree \"$WD\") :ignore-inherited-configuration)' WPT_ROOT=/home/claude/wpt sbcl --dynamic-space-size 4096 --non-interactive --load inspect/forms-oracle.lisp --eval '(weft.forms-oracle:run \"$unit\")' 2>&1 | tail -14"
    {
      cat <<HDR
# Forms swarm (round 2) unit: $unit  — variant $v

You implement the HTMLInputElement IDL surface for **$unit** in the weft web
engine (pure Common Lisp; JS runs on the in-tree shuttle engine).

## Edit ONLY this one file
  $WD/src/script/forms-$unit.lisp
It is a stub with (defun install-forms-$unit (ctx ep) ...).  Fill that function.
Helpers you add MUST be prefixed "$unit-" or defined INSIDE the installer.

## Oracle — run after EVERY edit; drive "failed" toward 0
  $ORACLE

## Installer mechanics
Called with (ctx ep): ctx = script context, ep = HTMLElement prototype.  In scope:
  (n this) => the DOM node for a "this" wrapper.
  (defgetset ctx ep "prop" (this) GETTER (v) SETTER)   ; v = value being set
  (defget    ctx ep "prop" (this) GETTER)
  (defmethod* ctx ep "name" ARITY (this a) BODY...)     ; (arg a 0) = first arg
Read the built-in accessors in $WD/src/script/dom.lisp (search "defgetset ctx ep")
for concrete examples of the exact style.
$CHEAT

## $(variant_note "$unit" "$v")

## Hard rules
- PURE Common Lisp; NO regex/external libs (not loaded, won't compile). Parse by hand.
- File MUST compile (balanced parens; parens in "..." don't count). Oracle compiles first.
- Loop: edit -> oracle -> read the failing subtest messages -> fix. Keep going while the
  count drops. Stop at 0, or when you can no longer reduce it (then report the best count).

## Full spec for this unit
HDR
      cat "$TASKS/$unit.md"
    } > "$WAVE/$jobid.task.md"
  done
done

echo "[forms-wave2] ${#jobs[@]} jobs over ${#units[@]} units, pool=$POOL"
# variants a,b = cheap flash; variant c = a stronger model (the "good gate").
run_job() {
  local jobid="$1" WAVE="$2"; local unit="${jobid%-*}" v="${jobid##*-}"
  local model="deepseek/deepseek-v4-flash"; [ "$v" = c ] && model="${STRONG_MODEL:-deepseek/deepseek-v4-pro}"
  bash "$SELF/forms-worker2.sh" "$jobid" "$unit" "$WAVE" "$model"
}
export -f run_job; export SELF
printf '%s\n' "${jobs[@]}" | xargs -P "$POOL" -I{} bash -c 'run_job "$@"' _ {} "$WAVE"

echo "=== WAVE 2 COMPLETE — per-variant results ==="
failnum() { sed -n 's/.*, \([0-9]*\) failed.*/\1/p' <<<"$1"; }
for unit in "${units[@]}"; do
  best=""; bestf=999999; bestv=""
  for v in a b c; do
    line="$(grep -hE '^UNIT ' "$WAVE/$unit-$v.result" 2>/dev/null | tail -1)"
    f="$(failnum "$line")"; [ -z "$f" ] && f=999999
    printf '  %-16s %s\n' "$unit-$v" "${line:-<no result>}"
    if [ "$f" -lt "$bestf" ]; then bestf="$f"; best="$WAVE/$unit-$v/src/script/forms-$unit.lisp"; bestv="$unit-$v"; fi
  done
  echo "  -> best: $bestv (failed=$bestf)"
  # keep-best in canonical: copy in, re-verify, revert on regression/failure.
  if [ -n "$best" ] && [ -f "$best" ]; then
    dst="$CANON/src/script/forms-$unit.lisp"
    bak="/tmp/forms2-backup-$unit.lisp"; cp "$dst" "$bak"
    before="$( ( cd "$CANON" && WPT_ROOT=/home/claude/wpt sbcl --dynamic-space-size 4096 --non-interactive --load inspect/forms-oracle.lisp --eval "(weft.forms-oracle:run \"$unit\")" 2>&1 ) | grep -E '^UNIT ' | tail -1 )"
    cp "$best" "$dst"
    after="$( ( cd "$CANON" && WPT_ROOT=/home/claude/wpt sbcl --dynamic-space-size 4096 --non-interactive --load inspect/forms-oracle.lisp --eval "(weft.forms-oracle:run \"$unit\")" 2>&1 ) | grep -E '^UNIT ' | tail -1 )"
    bf="$(failnum "$before")"; af="$(failnum "$after")"
    if [ -z "$af" ] || [ "${af:-999}" -gt "${bf:-999}" ]; then
      cp "$bak" "$dst"; echo "  -> canonical: REVERTED ($before -> ${after:-LOAD-FAIL})"
    else
      echo "  -> canonical: KEEP  $before -> $after"
    fi
  fi
done
