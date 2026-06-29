#!/usr/bin/env bash
# Generic CSS value-parser swarm wave.
# Usage: wave.sh <wave-dir> <manifest-file> [parallelism]
# manifest lines: unit|<semantics text>   (oracle vectors must already exist as
# inspect/vectors/css/<unit>.json and a stub src/css/<unit>.lisp wired into weft.asd)
set -u
WAVE="$1"; MANIFEST="$2"; PAR="${3:-6}"
SRC=/home/claude/weft; SELF="$(cd "$(dirname "$0")" && pwd)"
rm -rf "$WAVE"; mkdir -p "$WAVE"
units=()
while IFS='|' read -r unit sem; do
  [ -z "$unit" ] && continue
  units+=("$unit")
  WD="$WAVE/$unit"; cp -r "$SRC" "$WD"; rm -rf "$WD/.git" "$WD"/src/*.fasl "$WD"/src/*/*.fasl
  cat > "$WAVE/$unit.task.md" <<TASK
# Implement the CSS <$unit> value parser

Edit ONLY: $WD/src/css/$unit.lisp (a stub). Write (in-package #:weft.css):
  (define-value-parser "$unit" (s) ...value or :invalid...)
S is the input string. Helpers: (css-trim s), (ascii-downcase s).

CRITICAL:
- PURE Common Lisp ONLY. NO cl-ppcre / regex / external libraries — they are not
  loaded and will not compile. Parse by hand (char, char=, digit-char-p,
  position, search, subseq, parse-integer, read-from-string guarded, loop).
- define-value-parser wraps your body in a LAMBDA. There is NO block named
  "$unit"; do NOT (return-from "$unit" ...). Use cond/if, or (block nil ...).
- The file MUST compile: balanced parens (parens inside "..." string literals do
  not count). The oracle compiles first; fix any READ error before logic.

Semantics: $sem

ORACLE (run after EVERY edit; loop until it prints "0 failed"):
  cd $WD && rm -rf $WAVE/.cache-$unit && XDG_CACHE_HOME=$WAVE/.cache-$unit CL_SOURCE_REGISTRY='(:source-registry (:tree "$WD") :ignore-inherited-configuration)' sbcl --non-interactive --eval '(asdf:load-system "weft")' --load inspect/css-test.lisp --eval '(weft.css.test:run "$unit")' 2>&1 | tail -8
Success = "0 failed". Stop then, or after 8 cycles (report closest + an example).
TASK
done < "$MANIFEST"
echo "[wave] ${#units[@]} units, $PAR concurrent"
printf '%s\n' "${units[@]}" | xargs -P "$PAR" -I{} bash "$SELF/worker.sh" {} "$WAVE"
echo "=== WAVE COMPLETE ==="
for u in "${units[@]}"; do printf '%-18s %s\n' "$u" "$(grep -hE 'passed,' "$WAVE/$u.result" 2>/dev/null | tail -1)"; done
