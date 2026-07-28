#!/usr/bin/env bash
# nonforms-gate.sh — the suites the forms oracle CANNOT see.
#
# The forms oracle scores html/semantics/forms only, so a change to shared code
# (the tree builder, the DOM, the event path) can pay for a forms gain out of
# something it never looks at, and the wave will happily keep it.  Wave 7 shipped
# a parser change that cost tree-test 332 -> 330; nothing in the oracle noticed.
# Run this against canon after every merge.
#
#   ./nonforms-gate.sh [repo]     (default /home/claude/weft)
#
# To baseline a clean tree instead, point it at a detached worktree and scope
# ASDF to it, or the global local-projects symlink resolves `weft' back to canon:
#   git -C /home/claude/weft worktree add --detach /tmp/clean HEAD
#   CL_SOURCE_REGISTRY='(:source-registry (:tree "/tmp/clean/") :ignore-inherited-configuration)' \
#     ./nonforms-gate.sh /tmp/clean
#
# Measured on weft 2ae34ae (wave-8 branch point):
#   tree-test      332 passed, 10 failed, 21 fragment-skipped
#   html-test      6677 passed, 0 failed
#   dom-test       116 passed, 0 failed
#   selector-test  35 passed, 0 failed
#   scripting-dom  28 passed, 0 failed
#   scripting-m1   PASS
# The 10 tree-test failures are long-standing (isindex, the </style> --> family,
# frameset trailing text); they are the baseline, not a regression.
set -uo pipefail
REPO="${1:-/home/claude/weft}"
cd "$REPO" || exit 1

# Two things that each cost a debugging round the first time:
#  - loading the .lisp alone leaves "The name WEFT.HTML does not designate any
#    package"; the system has to be loaded first and the runner called explicitly.
#  - the two scripting suites need :weft/script, not :weft.  Under
#    --disable-debugger the wrong system does not say "wrong system", it dumps a
#    backtrace and quits, which reads like a broken test file.
# The muffle-warning below is deliberate (a clean transcript), but it means this
# gate can NOT tell you the tree compiles — it hides exactly the duplicate-defun
# WARNING that ASDF treats as fatal.  cold-build.sh is the gate for that; run
# both.
run() {
  local label="$1" system="$2" file="$3" form="$4"
  printf '\n=== %s ===\n' "$label"
  timeout 1800 sbcl --control-stack-size 128 --dynamic-space-size 8192 \
    --non-interactive \
    --eval "(handler-bind ((warning (function muffle-warning))) (asdf:load-system :$system))" \
    --load "inspect/$file" \
    --eval "$form" 2>&1 | grep -Ei "passed|failed|PASS|FAIL|error" | tail -3
}

run "tree-test"     weft        tree-test.lisp     '(weft.html.tree-test:run)'
run "html-test"     weft        html-test.lisp     '(weft.html.test:run)'
run "dom-test"      weft        dom-test.lisp      '(weft.dom.test:run)'
run "selector-test" weft        selector-test.lisp '(weft.css.select-test:run)'
run "scripting-dom" weft/script scripting-dom.lisp '(weft.script.dom-test:run)'
run "scripting-m1"  weft/script scripting-m1.lisp  '(weft.script.m1:run)'
