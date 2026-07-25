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

score () {   # -> "<passed> <failed>" for the tree as it stands
  rm -rf "$CACHE"
  ( cd "$WD" && XDG_CACHE_HOME="$CACHE" CL_SOURCE_REGISTRY="$REG" \
    WPT_ROOT=/home/claude/wpt ORACLE_KEEP="$OWNED" \
    sbcl --dynamic-space-size 4096 --non-interactive --load inspect/forms-oracle.lisp \
      --eval "(weft.forms-oracle:run \"$unit\")" ) > "$WAVE/$jobid.result" 2>&1
  grep -haE '^UNIT ' "$WAVE/$jobid.result" | tail -1 \
    | sed -E 's/.*: ([0-9]+) passed, ([0-9]+) failed.*/\1 \2/'
}

start=$(date +%s)
timeout "${WORKER_TIMEOUT:-1800}" sbcl --non-interactive --load "$OPERANDI_ROOT/bin/operandi.lisp" -- \
  --openrouter "$MODEL" --no-tools Fan,Task,Spawn \
  "Read $WAVE/$jobid.task.md and carry it out fully and autonomously: edit the one file and loop the oracle, driving the failing-subtest count as low as you can. Keep going while it drops. No questions." \
  > "$WAVE/$jobid.log" 2>&1
end=$(date +%s)

final=$(score); fp=${final%% *}
best=$(cat "$OWNED.best-$unit.score" 2>/dev/null || echo "")
if [ -n "$best" ] && [ -f "$OWNED.best-$unit" ] && [ "${fp:-0}" -lt "$best" ]; then
  echo "$jobid: restoring best ($best passed) over final (${fp:-0})" >&2
  cp "$OWNED.best-$unit" "$OWNED"
  final=$(score)
fi

cost=$(grep -aoE '[0-9.]+¢' "$WAVE/$jobid.log" | tail -1)
iters=$(grep -aoE '[0-9]+ iters' "$WAVE/$jobid.log" | tail -1 | grep -oE '^[0-9]+')
res=$(grep -haE '^UNIT ' "$WAVE/$jobid.result" | tail -1)
printf '%s\t%s\t%s\t%s\t%s\n' "$jobid" "${MODEL##*/}" "$((end-start))" "${iters:-?}" "${cost:-?¢}" >> "$WAVE/eval.tsv"
echo "$jobid: ${res:-<no result>}  [${MODEL##*/}, $((end-start))s, ${cost:-?}]"
