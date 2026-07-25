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

# ---- rounds ----------------------------------------------------------------
# One agent invocation is not one worker.  Both ways a single invocation can end
# are premature:  the textarea arms STOPPED VOLUNTARILY at 25/29 having used 27
# of 150 iterations, and the select arms were still climbing when the timeout
# cut them off.  So run rounds until the budget is gone or TOTAL stops moving,
# each round a FRESH context — which costs nothing now that the oracle prints
# the failing subtest names, so a new agent re-orients from one command instead
# of from the whole conversation it no longer has.
BUDGET="${WORKER_BUDGET:-5400}"          # total wall clock for this worker
ROUND_MAX="${WORKER_TIMEOUT:-1800}"      # per-round cap
deadline=$(( $(date +%s) + BUDGET ))
start=$(date +%s); round=0; dry=0
prev=$(score)
echo "$jobid: start TOTAL ${prev:-?}" >&2

while [ $dry -lt 2 ]; do
  left=$(( deadline - $(date +%s) ))
  [ "$left" -lt 420 ] && { echo "$jobid: out of budget after $round round(s)" >&2; break; }
  [ "$left" -gt "$ROUND_MAX" ] && left=$ROUND_MAX
  round=$((round+1))
  timeout "$left" sbcl --non-interactive --load "$OPERANDI_ROOT/bin/operandi.lisp" -- \
    --openrouter "$MODEL" --no-tools Fan,Task,Spawn \
    "Read $WAVE/$jobid.task.md and carry it out fully and autonomously. The tree already scores TOTAL ${prev:-?}; your job is to raise it. Run the oracle FIRST — it names every failing subtest and the assertion that failed, so you do not need to guess which ones are broken. Fix them in the one file you own, re-run, repeat. Write code early and often; do not spend the round reading. No questions." \
    >> "$WAVE/$jobid.log" 2>&1
  cur=$(score)
  # Keep whichever file scores best; the oracle snapshots on every improvement.
  best=$(cat "$OWNED.best-$unit.score" 2>/dev/null || echo "")
  if [ -n "$best" ] && [ -f "$OWNED.best-$unit" ] && [ "${cur:-0}" -lt "$best" ]; then
    cp "$OWNED.best-$unit" "$OWNED"; cur=$(score)
    echo "$jobid: round $round restored best (TOTAL $best)" >&2
  fi
  if [ "${cur:-0}" -gt "${prev:-0}" ]; then dry=0; else dry=$((dry+1)); fi
  echo "$jobid: round $round TOTAL ${prev:-?} -> ${cur:-?} (dry=$dry)" >&2
  prev=$cur
done
end=$(date +%s)
fp=$prev

# operandi prints its usage line ONCE per invocation, on exit (bin/operandi.lisp:
# "[N iters, X¢, ...]"), so summing the ¢ figures across the log = the worker's
# total.  Caveat: a round killed by `timeout` never reaches that print, so its
# spend is invisible here and the sum under-reports.
cost=$(grep -aoE '[0-9.]+¢' "$WAVE/$jobid.log" | awk '{gsub(/¢/,"");s+=$1} END{printf "%.3f¢", s}')
res=$(grep -haE '^UNIT ' "$WAVE/$jobid.result" | tail -1)
tot=$(grep -haE '^TOTAL ' "$WAVE/$jobid.result" | tail -1)
printf '%s\t%s\t%s\t%s\t%s\t%s\n' "$jobid" "${MODEL##*/}" "$((end-start))" "${round} rounds" "${cost:-?¢}" "${fp:-?}" >> "$WAVE/eval.tsv"
echo "$jobid: ${res:-<no result>} | ${tot:-<no total>}  [${MODEL##*/}, $((end-start))s, $round rounds, ${cost:-?}]"
