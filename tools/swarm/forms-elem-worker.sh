#!/usr/bin/env bash
# Focused per-element worker (oracle = forms-oracle unit, NOT a whole subtree).
# Args: <jobid> <unit> <wave> [model].  Instrumented for the mechanism eval.
#
# OWNED is the one file the unit may edit.  It is exported as ORACLE_KEEP so the
# oracle snapshots it on every improvement, and the final scoring below restores
# that snapshot when the run ends worse than its own best — a worker that runs
# out of iterations mid-repair otherwise scores zero however good it once was
# (observed: 377 lines written, then a broken paren at the cap, whole run lost).
set -u
jobid="$1"; unit="$2"; WAVE="$3"; MODEL="${4:-deepseek/deepseek-v4-flash}"
OPERANDI_ROOT="${OPERANDI_ROOT:?set to the operandi checkout}"
WD="$WAVE/$jobid"; CACHE="$WAVE/.cache-$jobid"
REG="(:source-registry (:tree \"$WD\") :ignore-inherited-configuration)"
OWNED="$WD/src/script/forms-$unit.lisp"
export ORACLE_KEEP="$OWNED"

# The score is the TOTAL across every unit, not this unit's own tally: units
# share one element prototype, so a file can win locally by clobbering a
# sibling's method (measured: select 6->36 while costing 131 subtests elsewhere).
# maybe-keep-best in the oracle snapshots on the same number, so the restore
# below compares like with like.
score () {   # -> total passed across all units, for the tree as it stands
  rm -rf "$CACHE"
  ( cd "$WD" && XDG_CACHE_HOME="$CACHE" CL_SOURCE_REGISTRY="$REG" \
    WPT_ROOT=/home/claude/wpt ORACLE_KEEP="$OWNED" \
    sbcl --dynamic-space-size 4096 --non-interactive --load inspect/forms-oracle.lisp \
      --eval "(weft.forms-oracle:run \"$unit\")" ) > "$WAVE/$jobid.result" 2>&1
  grep -haE '^TOTAL ' "$WAVE/$jobid.result" | tail -1 \
    | sed -E 's/^TOTAL ([0-9]+) .*/\1/'
}

# operandi's 24k default context budget is sized for a small LOCAL model and
# makes these workers THRASH: the unit file plus a couple of WPT test files
# already exceed it, so compaction evicts what the agent just learned and it
# re-reads instead of writing.  Measured at the default: 23 compactions and ZERO
# Write calls in 40 minutes, across all six workers.  Both deepseek-v4 models
# carry a 1,048,576-token window, so 24k was 2.4% of what we were paying for.
export OPERANDI_CONTEXT_BUDGET="${OPERANDI_CONTEXT_BUDGET:-200000}"
export OPERANDI_MAX_ITERS="${OPERANDI_MAX_ITERS:-150}"

start=$(date +%s)
timeout "${WORKER_TIMEOUT:-1800}" sbcl --non-interactive --load "$OPERANDI_ROOT/bin/operandi.lisp" -- \
  --openrouter "$MODEL" --no-tools Fan,Task,Spawn \
  "Read $WAVE/$jobid.task.md and carry it out fully and autonomously: edit the one file and loop the oracle, driving TOTAL as high as you can. Write code early and often — do not spend the run reading. Keep going while TOTAL rises. No questions." \
  > "$WAVE/$jobid.log" 2>&1
end=$(date +%s)

fp=$(score)
best=$(cat "$OWNED.best-$unit.score" 2>/dev/null || echo "")
if [ -n "$best" ] && [ -f "$OWNED.best-$unit" ] && [ "${fp:-0}" -lt "$best" ]; then
  echo "$jobid: restoring best (TOTAL $best) over final (TOTAL ${fp:-0})" >&2
  cp "$OWNED.best-$unit" "$OWNED"
  fp=$(score)
fi

cost=$(grep -aoE '[0-9.]+¢' "$WAVE/$jobid.log" | tail -1)
iters=$(grep -aoE '[0-9]+ iters' "$WAVE/$jobid.log" | tail -1 | grep -oE '^[0-9]+')
res=$(grep -haE '^UNIT ' "$WAVE/$jobid.result" | tail -1)
tot=$(grep -haE '^TOTAL ' "$WAVE/$jobid.result" | tail -1)
printf '%s\t%s\t%s\t%s\t%s\t%s\n' "$jobid" "${MODEL##*/}" "$((end-start))" "${iters:-?}" "${cost:-?¢}" "${fp:-?}" >> "$WAVE/eval.tsv"
echo "$jobid: ${res:-<no result>} | ${tot:-<no total>}  [${MODEL##*/}, $((end-start))s, ${cost:-?}]"
