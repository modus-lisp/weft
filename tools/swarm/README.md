# weft swarm harness

Parallel CSS value-parser construction by cheap-model agents (operandi on
DeepSeek-Flash), oracled against vendored vectors. The strong tier writes the
kernel + oracle + stubs; the swarm fills the parsers.

## Isolation (the hard-won invariant)

Each worker edits ONLY its own copy `$WD = <wave>/<unit>`, and its oracle loads
weft with:

    CL_SOURCE_REGISTRY='(:source-registry (:tree "$WD") :ignore-inherited-configuration)'

`:ignore-inherited-configuration` is load-bearing: with `:inherit-configuration`
(or a bare `(:tree ...)`, which ASDF rejects) the worker could resolve the MAIN
`/home/claude/weft` system instead of its copy, so edits to `$WD` had no effect
and "passes" were against main. Proof the isolation holds — a stub in the copy
fails even when main is correct:

    # main src/css/cursor.lisp = working parser; $WD copy = stub
    (:tree "$WD") :ignore-inherited-configuration  ->  cursor 1/5  (FAIL)  # tests the copy
    # copy made correct                            ->  cursor 5/5  (PASS)

A fresh `XDG_CACHE_HOME` is wiped before every load so a stale fasl can't fake a
pass. `collect.sh` re-verifies each file is non-stub + paren-balanced before
copying into main, and the strong tier always re-runs the full gate in main.

## Use

    tools/swarm/wave.sh <wave-dir> <manifest> [parallelism]   # manifest: unit|semantics
    tools/swarm/collect.sh <wave-dir> <unit>...               # pull passing winners into main

Prereqs per unit: `inspect/vectors/css/<unit>.json` (oracle) + a stub
`src/css/<unit>.lisp` wired into `weft.asd`.

## Worker contract reminders (in every task)
- Pure Common Lisp only (no cl-ppcre / external libs).
- `define-value-parser` wraps a lambda — no `return-from <unit>`.
- File must compile (balanced parens; parens in strings don't count).
