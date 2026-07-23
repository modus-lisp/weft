#!/usr/bin/env bash
# Round-2 forms worker (unit + variant). Args: <jobid> <unit> <wave-dir> [model]
# jobid = "<unit>-<variant>"; edits $WD/src/script/forms-<unit>.lisp; oracle runs <unit>.
set -u
jobid="$1"; unit="$2"; WAVE="$3"; MODEL="${4:-deepseek/deepseek-v4-flash}"
OPERANDI_ROOT="${OPERANDI_ROOT:?set to the operandi checkout}"
WD="$WAVE/$jobid"; CACHE="$WAVE/.cache-$jobid"
REG="(:source-registry (:tree \"$WD\") :ignore-inherited-configuration)"

timeout "${WORKER_TIMEOUT:-2400}" sbcl --non-interactive --load "$OPERANDI_ROOT/bin/operandi.lisp" -- \
  --openrouter "$MODEL" \
  "Read $WAVE/$jobid.task.md and carry it out fully and autonomously: edit the one file $WD/src/script/forms-$unit.lisp and loop its oracle, driving the failing-subtest count as low as you can. Whenever the oracle count drops, you are making progress — keep going. Do not stop while the count is still dropping. No questions." \
  > "$WAVE/$jobid.log" 2>&1

rm -rf "$CACHE"
( cd "$WD" && XDG_CACHE_HOME="$CACHE" CL_SOURCE_REGISTRY="$REG" WPT_ROOT=/home/claude/wpt \
  sbcl --dynamic-space-size 4096 --non-interactive \
    --load inspect/forms-oracle.lisp \
    --eval "(weft.forms-oracle:run \"$unit\")" ) > "$WAVE/$jobid.result" 2>&1
echo "$jobid: $(grep -hE '^UNIT ' "$WAVE/$jobid.result" 2>/dev/null | tail -1)"
