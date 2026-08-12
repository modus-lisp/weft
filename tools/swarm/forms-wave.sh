#!/usr/bin/env bash
# Forms swarm wave.  Usage: forms-wave.sh <wave-dir> [parallelism] [unit ...]
# With no units, runs all under tools/swarm/forms-tasks/*.md.
#
# Per unit: copies the weft checkout -> <wave-dir>/<unit>, assembles
# <wave-dir>/<unit>.task.md = shared header (paths + oracle + rules) + the
# per-unit spec (forms-tasks/<unit>.md), then runs a DeepSeek worker that edits
# ONLY src/script/forms-<unit>.lisp and loops the oracle.
set -u
WAVE="$1"; PAR="${2:-6}"; shift 2 || shift $#
SELF="$(cd "$(dirname "$0")" && pwd)"; SRC="$(cd "$SELF/../.." && pwd)"
TASKS="$SELF/forms-tasks"
units=("$@")
if [ ${#units[@]} -eq 0 ]; then
  for f in "$TASKS"/*.md; do units+=("$(basename "${f%.md}")"); done
fi
rm -rf "$WAVE"; mkdir -p "$WAVE"
for unit in "${units[@]}"; do
  WD="$WAVE/$unit"
  cp -r "$SRC" "$WD"
  rm -rf "$WD/.git"; find "$WD" -name '*.fasl' -delete
  ORACLE="cd $WD && XDG_CACHE_HOME=$WAVE/.cache-$unit CL_SOURCE_REGISTRY='(:source-registry (:tree \"$WD\") :ignore-inherited-configuration)' WPT_ROOT=/home/claude/wpt sbcl --dynamic-space-size 4096 --non-interactive --load inspect/forms-oracle.lisp --eval '(weft.forms-oracle:run \"$unit\")' 2>&1 | tail -14"
  {
    cat <<HDR
# Forms swarm unit: $unit

You implement the HTMLInputElement IDL surface for **$unit** in the weft web
engine (pure Common Lisp; JavaScript runs on the in-tree shuttle engine).

## Edit ONLY this one file
  $WD/src/script/forms-$unit.lisp
It is a stub containing (defun install-forms-$unit (ctx ep) ...).  Fill that
function.  Do NOT touch any other file.  Helpers you add MUST be prefixed
"$unit-" (or defined INSIDE the installer) so they never collide with a sibling
feature file that shares the weft.script package.

## The oracle — run after EVERY edit; drive "failed" toward 0
  $ORACLE

## How the installer works
install-element-proto (already loaded) calls your installer with (ctx ep):
  ctx = the script context, ep = the HTMLElement prototype object.
Inside the installer, the local macro (n this) => the underlying DOM node for a
"this" wrapper.  Install accessors/methods with these macros (already in scope):
  (defgetset ctx ep "prop" (this) GETTER-FORM (v) SETTER-FORM)   ; v = the JS value set
  (defget    ctx ep "prop" (this) GETTER-FORM)                   ; read-only
  (defmethod* ctx ep "name" ARITY (this a) BODY...)              ; a = args; (arg a 0) etc.
Return values: build JS values with (num x) [number], (jbool x) [boolean],
(jstr v) [-> Lisp string from a JS value], js:*undefined*, js:*null*, js:*true*,
js:*false*.  Throw a JS error with (js:js-throw (js:make-native-error "TypeError" "msg"))
or a DOMException via (throw-dom ctx "InvalidStateError" 11 "msg").

## Reading / writing an input's live value and attributes
  (get-attr NODE "name")           ; content attribute string, or NIL
  (set-attr NODE "name" "val")     ; set content attribute
  (dom:has-attribute NODE "name")  ; boolean
  (h:dnode-name NODE)              ; lowercased tag, e.g. "input"
  ;; The CURRENT value of an <input> lives in a hash, NOT the "value" attribute:
  (gethash NODE (context-input-values ctx))        ; (values current-value present-p)
  (setf (gethash NODE (context-input-values ctx)) "new")   ; set current value
  ;; The effective current value string (falls back to the value attr):
  (defun $unit-cur-value (ctx node)
    (multiple-value-bind (v p) (gethash node (context-input-values ctx))
      (if p v (or (get-attr node "value") ""))))
The input TYPE is (input-type NODE) if such a helper exists; otherwise
(string-downcase (or (get-attr NODE "type") "text")).  Only <input> elements
have these IDL members — for a non-input, match browser behaviour in the spec
(often: return default / throw / be a no-op).

## Hard rules
- PURE Common Lisp.  NO cl-ppcre / regex / external libs (not loaded; won't compile).
  Parse by hand: char, char=, digit-char-p, position, search, subseq,
  parse-integer (:junk-allowed t), loop.
- The file MUST compile: balanced parens (parens inside "..." don't count).  The
  oracle recompiles the file first; fix any READ/compile error before logic.
- Do the edit, run the oracle, read the FAILING subtest messages it can surface,
  fix, repeat — up to ~10 cycles.  Stop at 0 failed, or when no longer improving
  (then report the best count reached and one example still-failing subtest).

## Spec / behaviour for this unit
HDR
    cat "$TASKS/$unit.md"
  } > "$WAVE/$unit.task.md"
done
echo "[forms-wave] ${#units[@]} units: ${units[*]}  (PAR=$PAR)"
printf '%s\n' "${units[@]}" | xargs -P "$PAR" -I{} bash "$SELF/forms-worker.sh" {} "$WAVE"
echo "=== WAVE COMPLETE ==="
for u in "${units[@]}"; do
  printf '%-16s %s\n' "$u" "$(grep -hE '^UNIT ' "$WAVE/$u.result" 2>/dev/null | tail -1)"
done
