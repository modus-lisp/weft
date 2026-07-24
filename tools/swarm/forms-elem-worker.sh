#!/usr/bin/env bash
# Focused per-element worker (oracle = forms-oracle unit, NOT a whole subtree).
# Args: <jobid> <unit> <wave> [model].  Instrumented for the mechanism eval.
set -u
jobid="$1"; unit="$2"; WAVE="$3"; MODEL="${4:-deepseek/deepseek-v4-flash}"
OPERANDI_ROOT="${OPERANDI_ROOT:?set to the operandi checkout}"
WD="$WAVE/$jobid"; CACHE="$WAVE/.cache-$jobid"
REG="(:source-registry (:tree \"$WD\") :ignore-inherited-configuration)"
start=$(date +%s)
timeout "${WORKER_TIMEOUT:-1800}" sbcl --non-interactive --load "$OPERANDI_ROOT/bin/operandi.lisp" -- \
  --openrouter "$MODEL" \
  "Read $WAVE/$jobid.task.md and carry it out fully and autonomously: edit the one file and loop the oracle, driving the failing-subtest count as low as you can. Keep going while it drops. No questions." \
  > "$WAVE/$jobid.log" 2>&1
end=$(date +%s)
rm -rf "$CACHE"
( cd "$WD" && XDG_CACHE_HOME="$CACHE" CL_SOURCE_REGISTRY="$REG" WPT_ROOT=/home/claude/wpt \
  sbcl --dynamic-space-size 4096 --non-interactive --load inspect/forms-oracle.lisp \
    --eval "(weft.forms-oracle:run \"$unit\")" ) > "$WAVE/$jobid.result" 2>&1
cost=$(grep -aoE '[0-9.]+¢' "$WAVE/$jobid.log" | tail -1)
iters=$(grep -aoE '[0-9]+ iters' "$WAVE/$jobid.log" | tail -1 | grep -oE '^[0-9]+')
res=$(grep -haE '^UNIT ' "$WAVE/$jobid.result" | tail -1)
printf '%s\t%s\t%s\t%s\t%s\n' "$jobid" "${MODEL##*/}" "$((end-start))" "${iters:-?}" "${cost:-?¢}" >> "$WAVE/eval.tsv"
echo "$jobid: ${res:-<no result>}  [${MODEL##*/}, $((end-start))s, ${cost:-?}]"
