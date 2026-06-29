#!/usr/bin/env bash
# Collect passing worker files into main, verifying each is non-stub + balanced.
# Usage: collect.sh <wave-dir> <unit>...
set -u
WAVE="$1"; shift
for u in "$@"; do
  f="$WAVE/$u/src/css/$u.lisp"
  if ! grep -q ", 0 failed" "$WAVE/$u.result" 2>/dev/null; then echo "$u: SKIP (not 0-failed)"; continue; fi
  if grep -q "declare (ignore s)) :invalid" "$f"; then echo "$u: SKIP (still stub)"; continue; fi
  bal=$(python3 - "$f" <<'PY'
import sys
s=open(sys.argv[1]).read();d=i=0;n=len(s)
while i<n:
    c=s[i]
    if c==';':
        while i<n and s[i]!='\n': i+=1
        continue
    if c=='"':
        i+=1
        while i<n and s[i]!='"':
            if s[i]=='\\': i+=1
            i+=1
        i+=1; continue
    if c=='#' and i+1<n and s[i+1]=='\\': i+=3; continue
    if c=='(': d+=1
    elif c==')': d-=1
    i+=1
print(d)
PY
)
  if [ "$bal" != "0" ]; then echo "$u: SKIP (paren imbalance $bal)"; continue; fi
  cp "$f" "/home/claude/weft/src/css/$u.lisp"; echo "$u: COLLECTED"
done
