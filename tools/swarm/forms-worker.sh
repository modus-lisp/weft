#!/usr/bin/env bash
# Forms swarm worker. Args: <unit> <wave-dir> [model]
#
# Isolation contract (same as the CSS worker):
#   - edits ONLY $WD/src/script/forms-<unit>.lisp inside its own copy.
#   - the oracle loads "weft/script" via (:tree "$WD") :ignore-inherited-config,
#     so ASDF can NEVER fall through to the canonical weft checkout.
#   - a per-worker XDG_CACHE_HOME isolates fasls; the final re-check wipes it so a
#     stale fasl can't fake a low failure count.
set -u
WPT_ROOT="${WPT_ROOT:-$HOME/wpt}"
unit="$1"; WAVE="$2"; MODEL="${3:-deepseek/deepseek-v4-flash}"
OPERANDI_ROOT="${OPERANDI_ROOT:?set to the operandi checkout}"
WD="$WAVE/$unit"; CACHE="$WAVE/.cache-$unit"
REG="(:source-registry (:tree \"$WD\") :ignore-inherited-configuration)"

timeout "${WORKER_TIMEOUT:-1200}" sbcl --non-interactive --load "$OPERANDI_ROOT/bin/operandi.lisp" -- \
  --openrouter "$MODEL" \
  "Read $WAVE/$unit.task.md and carry it out fully and autonomously: edit the one file $WD/src/script/forms-$unit.lisp and loop its oracle, driving the failing-subtest count as low as you can (0 is the goal). No questions." \
  > "$WAVE/$unit.log" 2>&1

# Independent re-check: wiped cache, isolated registry.
rm -rf "$CACHE"
( cd "$WD" && XDG_CACHE_HOME="$CACHE" CL_SOURCE_REGISTRY="$REG" WPT_ROOT=$WPT_ROOT \
  sbcl --dynamic-space-size 4096 --non-interactive \
    --load inspect/forms-oracle.lisp \
    --eval "(weft.forms-oracle:run \"$unit\")" ) > "$WAVE/$unit.result" 2>&1
echo "$unit: $(grep -hE '^UNIT ' "$WAVE/$unit.result" 2>/dev/null | tail -1)"
