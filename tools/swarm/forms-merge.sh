#!/usr/bin/env bash
# Re-verify each swarm unit's file IN CANONICAL weft, then keep it only if it
# compiles and improves (or holds) the unit's oracle vs the current tree.
# Usage: forms-merge.sh <wave-dir> [unit ...]
set -u
WAVE="$1"; shift || true
CANON=/home/claude/weft
units=("$@"); [ ${#units[@]} -eq 0 ] && units=(valueasnumber valueasdate stepupdown selection validity labels)

canon_oracle() { # $1=unit  -> prints the UNIT line (uses canonical tree, its own cache)
  ( cd "$CANON" && WPT_ROOT=/home/claude/wpt sbcl --dynamic-space-size 4096 --non-interactive \
      --load inspect/forms-oracle.lisp --eval "(weft.forms-oracle:run \"$1\")" 2>&1 ) | grep -E '^UNIT ' | tail -1
}
failnum() { sed -n 's/.*, \([0-9]*\) failed.*/\1/p' <<<"$1"; }

for u in "${units[@]}"; do
  SRCF="$WAVE/$u/src/script/forms-$u.lisp"
  DSTF="$CANON/src/script/forms-$u.lisp"
  [ -f "$SRCF" ] || { echo "$u: no worker file"; continue; }
  before="$(canon_oracle "$u")"; bf="$(failnum "$before")"
  cp "$DSTF" "/tmp/forms-merge-backup-$u.lisp"
  cp "$SRCF" "$DSTF"
  after="$(canon_oracle "$u")"; af="$(failnum "$after")"
  if [ -z "$af" ]; then
    cp "/tmp/forms-merge-backup-$u.lisp" "$DSTF"
    echo "$u: REVERTED (worker file failed to load in canonical)   before=[$before]"
  elif [ "${af:-999}" -le "${bf:-999}" ]; then
    echo "$u: KEEP   $before  ->  $after"
  else
    cp "/tmp/forms-merge-backup-$u.lisp" "$DSTF"
    echo "$u: REVERTED (regressed $bf->$af failed)"
  fi
done
