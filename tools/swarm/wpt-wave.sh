#!/usr/bin/env bash
# WPT swarm wave over per-element IDL units, with mechanism evaluation.
# Usage: wpt-wave.sh <wave-dir> [pool]
#
# Units (unit | wpt-subdir | edit-file | element):
#   each unit fills ONE disjoint self-registering file, so merges never conflict.
# Per unit we run 3 variants: a,b = deepseek-v4-flash (two cheap samples),
#   c = deepseek-v4-pro (the strong gate).  Keep the best variant (most subtests
#   passed), re-verify it in canonical (keep only if it strictly increases passes),
#   and record per-variant model/time/iters/cost for the evaluation.
set -u
WPT_ROOT="${WPT_ROOT:-$HOME/wpt}"
WAVE="$1"; POOL="${2:-9}"
SELF="$(cd "$(dirname "$0")" && pwd)"; SRC="$(cd "$SELF/../.." && pwd)"; CANON="$SRC"
FLASH="deepseek/deepseek-v4-flash"; PRO="${STRONG_MODEL:-deepseek/deepseek-v4-pro}"

# unit  subdir  editfile  element
UNITS=(
  "select|html/semantics/forms/the-select-element|src/script/forms-select.lisp|select"
  "textarea|html/semantics/forms/the-textarea-element|src/script/forms-textarea.lisp|textarea"
  "button|html/semantics/forms/the-button-element|src/script/forms-button.lisp|button"
)
rm -rf "$WAVE"; mkdir -p "$WAVE"; : > "$WAVE/eval.tsv"

jobs=()
for spec in "${UNITS[@]}"; do
  IFS='|' read -r unit subdir editfile element <<<"$spec"
  for v in a b c; do
    jobid="$unit-$v"; model="$FLASH"; [ "$v" = c ] && model="$PRO"
    jobs+=("$jobid|$subdir|$model")
    WD="$WAVE/$jobid"; cp -r "$SRC" "$WD"; rm -rf "$WD/.git"; find "$WD" -name '*.fasl' -delete
    ORACLE="cd $WD && XDG_CACHE_HOME=$WAVE/.cache-$jobid CL_SOURCE_REGISTRY='(:source-registry (:tree \"$WD\") :ignore-inherited-configuration)' WPT_ROOT=$WPT_ROOT WPT_SUBDIR=$subdir sbcl --dynamic-space-size 4096 --non-interactive --load inspect/wpt-oracle.lisp 2>&1 | tail -3"
    cat > "$WAVE/$jobid.task.md" <<TASK
# WPT swarm unit: <$element> element IDL  (variant $v)

Improve the weft web engine (pure Common Lisp; JS on the in-tree shuttle engine)
to pass more of the WPT test suite under:
  $WPT_ROOT/$subdir
READ the failing .html test files there to see exactly what is asserted.

## Edit ONLY this one file
  $WD/$editfile   — a stub with (defun install-forms-$element (ctx ep) ...).
Fill that installer.  It runs on the SHARED element prototype, so EVERY accessor
you add MUST gate on the tag, e.g.:
  (defgetset ctx ep "prop" (this)
    (let ((node (n this)))
      (if (string= (h:dnode-name node) "$element") <getter> js:*undefined*))
    (v) (let ((node (n this))) (when (string= (h:dnode-name node) "$element") <setter>)))
Prefix any helper "$element-".  Do NOT touch any other file.

## Oracle — run after EVERY edit; drive "failed" down
  $ORACLE
Success = the "UNIT ...: P passed, F failed" line, with F as low as you can get it.

## How to work
- Read $WD/src/script/dom.lisp (search "defgetset ctx ep" and "install-element-proto")
  for the EXACT accessor style and the helpers already available; the <$element>
  interface may already have a few accessors there — you are ADDING the missing ones.
- Build JS values with (num x) (jbool x) (jstr v) js:*true* js:*false* js:*null*
  js:*undefined*; a live NodeList/HTMLCollection with
  (make-collection ctx (lambda () LIST-OF-NODES) nil :nodelist); read/write content
  attributes with (get-attr node "n") / (set-attr node "n" v) / (dom:has-attribute node "n");
  reflect a boolean/long attr like the input files in the same dir do.
- Throw with (throw-dom ctx "IndexSizeError" 1 "msg") or
  (js:js-throw (js:make-native-error "TypeError" "msg")).

## Rules
- PURE Common Lisp, NO regex/external libs (won't compile).  Balanced parens.
- The oracle recompiles first — fix any READ/compile error before logic.
- Loop edit -> oracle -> read failing messages -> fix.  Keep going while F drops.
  Stop at 0, or when no longer improving (report the best count).
TASK
  done
done

echo "[wpt-wave] ${#jobs[@]} jobs (${#UNITS[@]} units x3), pool=$POOL"
# model is carried IN the job line (jobid|subdir|model); no exported fns/vars.
printf '%s\n' "${jobs[@]}" | xargs -P "$POOL" -I{} bash -c \
  'IFS="|" read -r jid sub mdl <<<"$1"; bash "$2/wpt-worker.sh" "$jid" "$sub" "$3" "$mdl"' _ {} "$SELF" "$WAVE"

echo "=== WPT WAVE COMPLETE — keep-best + merge ==="
failnum() { sed -n 's/.*, \([0-9]*\) failed.*/\1/p' <<<"$1"; }
passnum() { sed -n 's/.* \([0-9]*\) passed,.*/\1/p' <<<"$1"; }
oracle_canon() { WPT_ROOT=$WPT_ROOT WPT_SUBDIR="$1" sbcl --dynamic-space-size 4096 --non-interactive \
  --load "$CANON/inspect/wpt-oracle.lisp" 2>&1 | grep -E '^UNIT ' | tail -1; }
for spec in "${UNITS[@]}"; do
  IFS='|' read -r unit subdir editfile element <<<"$spec"
  best=""; bestp=-1; bestv=""
  for v in a b c; do
    line="$(grep -haE '^UNIT ' "$WAVE/$unit-$v.result" 2>/dev/null | tail -1)"
    p="$(passnum "$line")"; [ -z "$p" ] && p=-1
    printf '  %-12s %s\n' "$unit-$v" "${line:-<no result>}"
    if [ "$p" -gt "$bestp" ]; then bestp="$p"; best="$WAVE/$unit-$v/$editfile"; bestv="$unit-$v"; fi
  done
  before="$(oracle_canon "$subdir")"; bp="$(passnum "$before")"
  if [ -n "$best" ] && [ -f "$best" ]; then
    cp "$CANON/$editfile" "/tmp/wptmerge-$unit.bak"; cp "$best" "$CANON/$editfile"
    after="$(oracle_canon "$subdir")"; ap="$(passnum "$after")"
    if [ -n "$ap" ] && [ "${ap:-0}" -gt "${bp:-0}" ]; then
      echo "  -> KEEP $bestv: $before -> $after"
    else
      cp "/tmp/wptmerge-$unit.bak" "$CANON/$editfile"; echo "  -> REVERT $bestv (best=$bestp, canonical $before -> ${after:-LOAD-FAIL})"
    fi
  fi
done
echo "=== eval.tsv (jobid  model  wall_s  iters  cost) ==="; cat "$WAVE/eval.tsv"
