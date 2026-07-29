#!/usr/bin/env bash
# cold-build.sh — does the tree still compile from an EMPTY fasl cache?
#
# Why this exists
# ---------------
# Wave 9 committed a tree that does not compile.  An arm re-defined `ns-arg' in
# src/script/dom.lisp, which already had it — and ASDF turns a full WARNING into
# a COMPILE-FILE-ERROR, so `(asdf:load-system :weft/script)' fails outright on a
# cold cache.  Nothing caught it:
#
#   - the forms oracle and the swarm arms all ran against a WARM ~/.cache, where
#     dom.lisp was never recompiled, so the duplicate was never seen;
#   - nonforms-gate.sh wraps its load in (handler-bind ((warning #'muffle-warning))),
#     which would have hidden it even on a cold cache.
#
# It only surfaced days later, when an unrelated weft.asd edit invalidated the
# cache.  A warm cache is not a build.
#
#   ./cold-build.sh [repo]        (default /home/claude/weft)
#
# Exits 0 iff every system compiles clean from scratch.  Run it before accepting
# a merge, alongside nonforms-gate.sh.
set -uo pipefail
REPO="${1:-/home/claude/weft}"
cd "$REPO" || exit 1

# A private XDG_CACHE_HOME is what makes this cold: ASDF derives its output
# translations from it, so we get an empty fasl tree without touching (or
# invalidating) the shared one every other run depends on.
CACHE="$(mktemp -d /tmp/weft-cold-XXXXXX)"
trap 'rm -rf "$CACHE"' EXIT

# Scope ASDF to this repo, or the global local-projects symlink resolves `weft'
# back to canon and a worktree silently builds the wrong source.
rc=0
for system in weft weft/render weft/script; do
  printf '=== %s ===\n' "$system"
  XDG_CACHE_HOME="$CACHE" \
  CL_SOURCE_REGISTRY="(:source-registry (:tree \"$REPO/\") :ignore-inherited-configuration)" \
    timeout 1800 sbcl --control-stack-size 128 --dynamic-space-size 8192 \
      --non-interactive --eval "(asdf:load-system :$system)" > "$CACHE/out" 2>&1
  if [ $? -ne 0 ]; then
    echo "FAIL: $system did not compile"
    # The condition is the last thing before the backtrace; the backtrace itself
    # is 30 frames of ASDF and says nothing about the source.
    grep -B2 -A6 -E "^; caught (ERROR|WARNING)|^Unhandled" "$CACHE/out" | head -40
    rc=1
  else
    echo "ok"
  fi
done

# ASDF only fails on a duplicate defun WITHIN one file.  Two files in the same
# package defining the same name is merely "WARNING: redefining X in DEFUN" at
# load time — nobody reads it, the later file silently wins, and the two copies
# drift.  weft.css::SPLIT-WS sat like that in selector.lisp and style.lisp.
# So group defuns by (package, name) and report any name defined in more than
# one file; same name in different packages is fine and must not trip this.
printf '=== duplicate defuns ===\n'
dupes=$(for f in $(find src -name '*.lisp'); do
          pkg=$(grep -m1 -oE '^\(in-package [^)]*' "$f" | sed 's/.*[#:]://')
          grep -oE '^\(defun [a-zA-Z0-9%*+/<>=_-]+' "$f" |
            sed "s/^(defun /$pkg /" | sort -u | sed "s|\$| $f|"
        done | awk '{k=$1" "$2; f[k]=f[k]" "$3; n[k]++} END{for(k in n) if(n[k]>1) print k":"f[k]}')
if [ -n "$dupes" ]; then
  echo "FAIL: same name defined in two files of the same package"
  echo "$dupes"
  rc=1
else
  echo "ok"
fi

[ $rc -eq 0 ] && echo "cold build clean"
exit $rc
