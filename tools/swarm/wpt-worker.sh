#!/usr/bin/env bash
# Generalized WPT swarm worker (any subtree).  Args: <jobid> <subdir> <wave> [model]
# jobid = "<unit>-<variant>".  Edits its isolated copy $WD, loops the WPT oracle for
# <subdir>, then an independent re-check.  Emits an instrumented result line
# (unit result + model + wall-seconds + cost) for the mechanism evaluation.
set -u
jobid="$1"; subdir="$2"; WAVE="$3"; MODEL="${4:-deepseek/deepseek-v4-flash}"
OPERANDI_ROOT="${OPERANDI_ROOT:?set to the operandi checkout}"
WD="$WAVE/$jobid"; CACHE="$WAVE/.cache-$jobid"
REG="(:source-registry (:tree \"$WD\") :ignore-inherited-configuration)"

start=$(date +%s)
timeout "${WORKER_TIMEOUT:-1800}" sbcl --non-interactive --load "$OPERANDI_ROOT/bin/operandi.lisp" -- \
  --openrouter "$MODEL" \
  "Read $WAVE/$jobid.task.md and carry it out fully and autonomously: edit weft source in your copy and loop the oracle, driving the failing-subtest count as low as you can. Keep going while it drops. No questions." \
  > "$WAVE/$jobid.log" 2>&1
end=$(date +%s)

# independent re-check: wiped cache, isolated registry
rm -rf "$CACHE"
( cd "$WD" && XDG_CACHE_HOME="$CACHE" CL_SOURCE_REGISTRY="$REG" WPT_ROOT=/home/claude/wpt WPT_SUBDIR="$subdir" \
  sbcl --dynamic-space-size 4096 --non-interactive --load inspect/wpt-oracle.lisp ) > "$WAVE/$jobid.result" 2>&1

cost=$(grep -aoE '[0-9.]+¢' "$WAVE/$jobid.log" | tail -1)
iters=$(grep -aoE '[0-9]+ iters' "$WAVE/$jobid.log" | tail -1 | grep -oE '^[0-9]+')
res=$(grep -haE '^UNIT ' "$WAVE/$jobid.result" | tail -1)
# record a machine-readable instrumentation line for the evaluation
printf '%s\t%s\t%s\t%s\t%s\n' "$jobid" "${MODEL##*/}" "$((end-start))" "${iters:-?}" "${cost:-?¢}" >> "$WAVE/eval.tsv"
echo "$jobid: ${res:-<no result>}  [${MODEL##*/}, $((end-start))s, ${cost:-?}]"
