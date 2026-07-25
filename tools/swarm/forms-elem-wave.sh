#!/usr/bin/env bash
# Focused per-element swarm wave (select, textarea) — the "corrected oracle" run.
# Oracle = a CURATED forms-oracle unit (achievable files, 0 reachable), NOT the
# whole subtree.  Usage: forms-elem-wave.sh <wave-dir> [pool]
set -u
WAVE="$1"; POOL="${2:-6}"
SRC=/home/claude/weft; SELF="$(cd "$(dirname "$0")" && pwd)"; CANON=/home/claude/weft
FLASH="deepseek/deepseek-v4-flash"; PRO="${STRONG_MODEL:-deepseek/deepseek-v4-pro}"
UNITS=(select textarea)                       # forms-oracle unit == forms-<unit>.lisp editfile
rm -rf "$WAVE"; mkdir -p "$WAVE"; : > "$WAVE/eval.tsv"

guidance() { case "$1" in
  select) cat <<'G'
HTMLSelectElement — implement the members the curated tests exercise:
  options (an HTMLOptionsCollection of <option> descendants), selectedOptions
  (NodeList of selected options), selectedIndex (get/set), value (get/set: the
  first selected option's value, or ""; setting selects the matching option),
  length (get: option count; set: truncate/extend), item(i)/namedItem(name),
  add(element[,before]), remove([index]), multiple (reflects), type
  ("select-one" / "select-multiple"), the named/indexed getter.
Read forms-validity.lisp for the validity pattern; use (make-collection ctx
(lambda () LIST) nil :nodelist) for live lists; walk <option>/<optgroup>
descendants via (h:dnode-children n).
G
  ;;
  textarea) cat <<'G'
HTMLTextAreaElement — implement:
  value (get: the raw value / API value, defaulting to the text content; set:
  stores the current value — use (context-input-values ctx) like <input>),
  defaultValue (reflects the text content / child text), textLength (length of
  value), type (always "textarea"), cols (reflect, default 20), rows (reflect,
  default 2), wrap (reflect), maxLength/minLength (reflect, limited-to-non-
  negative, default -1), setCustomValidity/validity/checkValidity (mirror
  forms-validity.lisp), placeholder/readOnly/required (reflect).
value defaults to the element's child text content when never set via the API.
G
  ;; esac; }

jobs=()
for unit in "${UNITS[@]}"; do
  editfile="src/script/forms-$unit.lisp"
  for v in a b c; do
    jobid="$unit-$v"; model="$FLASH"; [ "$v" = c ] && model="$PRO"
    jobs+=("$jobid|$unit|$model")
    WD="$WAVE/$jobid"; cp -r "$SRC" "$WD"; rm -rf "$WD/.git"; find "$WD" -name '*.fasl' -delete
    ORACLE="cd $WD && XDG_CACHE_HOME=$WAVE/.cache-$jobid CL_SOURCE_REGISTRY='(:source-registry (:tree \"$WD\") :ignore-inherited-configuration)' WPT_ROOT=/home/claude/wpt sbcl --dynamic-space-size 4096 --non-interactive --load inspect/forms-oracle.lisp --eval '(weft.forms-oracle:run \"$unit\")' 2>&1 | tail -20"
    { cat "$SELF/API.md"; echo; cat <<TASK
# Focused WPT swarm unit: <$unit> element IDL  (variant $v)

Implement HTMLElement IDL for <$unit> in the weft engine (pure Common Lisp; JS on
the in-tree shuttle engine) to pass a CURATED, ACHIEVABLE set of WPT tests.

## Edit ONLY this one file
  $WD/$editfile   — a stub with (defun install-forms-$unit (ctx ep) ...).
Prefix helpers "$unit-".  Do NOT touch any other file.

## The shared prototype is the main hazard — read this twice
Other feature files (forms-validity.lisp, forms-selection.lisp, ...) install
THEIR accessors onto the SAME prototype object, and whoever installs last wins.
A generic name — value, type, length, labels, checkValidity, willValidate,
setCustomValidity — registered with the PLAIN macros deletes the sibling's
working implementation for every element, and a tag guard whose else-branch
returns js:*undefined* is exactly as destructive as no guard at all.  Not
theoretical: one run scored 6 -> 36 on its own unit while destroying 131
subtests across five others, purely from js:*undefined* else-branches.

So install with the -for variants, which gate on the tag AND delegate to
whatever was installed before.  Write only the <$unit> case; there is no
else-branch to write.
  (defgetset-for ctx ep "$unit" "value" (this) <getter> (v) <setter>)
  (defget-for    ctx ep "$unit" "type"  (this) <getter>)
  (defmethod-for ctx ep "$unit" "add" 2 (this a) <body>)
Use the plain defmethod*/defget/defgetset only for a name nothing else could
plausibly define.

## Oracle — run after EVERY edit
  $ORACLE
It prints per-file pass/fail, the "UNIT $unit: P passed, F failed" line, a
SENTINEL line per OTHER unit, and finally:
  TOTAL <n> passed across 8 units, best-ever <b>
**TOTAL is your score.** A gain on $unit paid for out of a sentinel is not a
gain, and any "REGRESSION -n" line means you have broken code you never read.
Drive TOTAL up: get $unit's failures to 0 while every SENTINEL stays at its best.
The files listed ARE the whole job — read each failing one under
/home/claude/wpt/html/semantics/forms/the-$unit-element/ and make it pass.  Keep
going while TOTAL rises.

## $(guidance "$unit")

## Helpers / rules
- Read $WD/src/script/dom.lisp (search "defgetset ctx ep") for the accessor style
  and the <$unit> accessors that already exist (you are ADDING the missing ones);
  read the sibling forms-*.lisp for validity / selection / reflection patterns.
- JS values: (num x) (jbool x) (jstr v) js:*true* js:*false* js:*null* js:*undefined*;
  live lists via (make-collection ctx (lambda () LIST) nil :nodelist); attrs via
  (get-attr node "n")/(set-attr node "n" v)/(dom:has-attribute node "n"); current
  value hash (context-input-values ctx); throw via (throw-dom ctx "IndexSizeError" 1 "m")
  or (js:js-throw (js:make-native-error "TypeError" "m")).
- PURE Common Lisp, NO regex/external libs.  Balanced parens; the oracle compiles
  first — fix any READ/compile error before logic.  Loop edit->oracle->fix.
TASK
    } > "$WAVE/$jobid.task.md"
  done
done

echo "[forms-elem-wave] ${#jobs[@]} jobs, pool=$POOL"
printf '%s\n' "${jobs[@]}" | xargs -P "$POOL" -I{} bash -c \
  'IFS="|" read -r jid unit mdl <<<"$1"; bash "$2/forms-elem-worker.sh" "$jid" "$unit" "$3" "$mdl"' _ {} "$SELF" "$WAVE"

echo "=== FORMS-ELEM WAVE COMPLETE — keep-best + merge ==="
# Selection AND the merge gate are on TOTAL (all units), never on the unit's own
# tally: the whole point of the sentinels is that a local win can be net negative.
totnum()  { sed -n 's/^TOTAL \([0-9]*\) .*/\1/p' <<<"$1"; }
oracle_canon() { ( cd "$CANON" && WPT_ROOT=/home/claude/wpt sbcl --dynamic-space-size 4096 --non-interactive \
  --load inspect/forms-oracle.lisp --eval "(weft.forms-oracle:run \"$1\")" 2>&1 ) | grep -aE '^(UNIT|TOTAL) ' | tr '\n' '|'; }
for unit in "${UNITS[@]}"; do
  editfile="src/script/forms-$unit.lisp"; best=""; bestp=-1; bestv=""
  for v in a b c; do
    line="$(grep -haE '^(UNIT|TOTAL) ' "$WAVE/$unit-$v.result" 2>/dev/null | tr '\n' '|')"
    p="$(totnum "$(grep -haE '^TOTAL ' "$WAVE/$unit-$v.result" 2>/dev/null | tail -1)")"; [ -z "$p" ] && p=-1
    printf '  %-12s %s\n' "$unit-$v" "${line:-<no result>}"
    [ "$p" -gt "$bestp" ] && { bestp="$p"; best="$WAVE/$unit-$v/$editfile"; bestv="$unit-$v"; }
  done
  before="$(oracle_canon "$unit")"; bp="$(totnum "${before//|/$'\n'}")"
  if [ -n "$best" ] && [ -f "$best" ]; then
    cp "$CANON/$editfile" "/tmp/femerge-$unit.bak"; cp "$best" "$CANON/$editfile"
    after="$(oracle_canon "$unit")"; ap="$(totnum "${after//|/$'\n'}")"
    if [ -n "$ap" ] && [ "${ap:-0}" -gt "${bp:-0}" ]; then echo "  -> KEEP $bestv: $before -> $after"
    else cp "/tmp/femerge-$unit.bak" "$CANON/$editfile"; echo "  -> REVERT $bestv (TOTAL best=$bestp, $before -> ${after:-LOAD-FAIL})"; fi
  fi
done
echo "=== eval.tsv ==="; cat "$WAVE/eval.tsv"
