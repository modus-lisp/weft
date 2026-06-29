#!/usr/bin/env bash
# Generic swarm worker for CSS value-parser units. Args: <unit> <wave-dir> [model]
#
# Isolation contract (verified in tools/swarm/README.md):
#   - the worker edits ONLY files inside its own copy $WD = <wave-dir>/<unit>.
#   - the oracle loads "weft" via (:tree "$WD") :ignore-inherited-configuration,
#     so ASDF resolves ONLY the worker copy and can NEVER fall through to the
#     main /home/claude/weft. (A stub in $WD fails even if main is correct.)
#   - a fresh per-worker XDG_CACHE_HOME is wiped before every load, so a stale
#     fasl can never yield a false "0 failed".
set -u
unit="$1"; WAVE="$2"; MODEL="${3:-deepseek/deepseek-v4-flash}"
COMBAT=/home/claude/combat
WD="$WAVE/$unit"; CACHE="$WAVE/.cache-$unit"
REG="(:source-registry (:tree \"$WD\") :ignore-inherited-configuration)"
ORACLE="cd $WD && rm -rf $CACHE && XDG_CACHE_HOME=$CACHE CL_SOURCE_REGISTRY='$REG' sbcl --non-interactive --eval '(asdf:load-system \"weft\")' --load inspect/css-test.lisp --eval '(weft.css.test:run \"$unit\")' 2>&1 | tail -8"

timeout "${WORKER_TIMEOUT:-700}" sbcl --non-interactive --load "$COMBAT/bin/operandi.lisp" -- \
  --openrouter "$MODEL" \
  "Read $WAVE/$unit.task.md and carry it out fully and autonomously: edit the one file and loop the oracle until it prints '0 failed' (up to 8 cycles). No questions." \
  > "$WAVE/$unit.log" 2>&1

# Independent re-check, wiped cache, isolated registry.
rm -rf "$CACHE"
( cd "$WD" && XDG_CACHE_HOME="$CACHE" CL_SOURCE_REGISTRY="$REG" \
  sbcl --non-interactive --eval '(asdf:load-system "weft")' \
    --load inspect/css-test.lisp --eval "(weft.css.test:run \"$unit\")" ) > "$WAVE/$unit.result" 2>&1
if grep -q ", 0 failed" "$WAVE/$unit.result"; then echo "$unit PASS"; else echo "$unit FAIL"; fi
