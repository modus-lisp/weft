# WPT reftest runner

Runs [web-platform-tests](https://github.com/web-platform-tests/wpt) **reftests**
against weft.  A reftest is an HTML page carrying `<link rel="match" href="…-ref.html">`
(must render identically to the reference) or `rel="mismatch"` (must differ).  Both
pages are rendered by weft, so font and anti-aliasing differences cancel — the
comparison purely exercises weft's layout of the tested feature against a reference
that produces the same result another way.  This covers a far larger and more
targeted slice of the platform than the Acid tests, and pinpoints gaps by feature.

There are two harnesses:

- **`wpt-run.py`** — *reftests* (rendering), described above.
- **`wpt-harness.py`** — *testharness.js* tests: the assert-based JS suite that
  covers the non-visual browser (DOM, HTML parsing, URL, encoding, events…).  weft
  runs them through shuttle; results come back through a completion callback we
  register in place of `testharnessreport.js`.

## Getting the tests

A partial + sparse checkout keeps it small (the full suite is ~2 GB):

```sh
git clone --no-checkout --depth 1 --filter=blob:none \
    https://github.com/web-platform-tests/wpt.git ../wpt
cd ../wpt
git sparse-checkout set css/css-flexbox css/css-text css/css-position \
    fonts common css/support resources dom url encoding
git checkout
```

## Running (reftests)

```sh
python3 inspect/wpt-run.py ../wpt css/css-position            # a whole category
python3 inspect/wpt-run.py ../wpt css/css-flexbox --limit 50  # a sample
python3 inspect/wpt-run.py ../wpt css/css-text --save-fails   # write failing paths
```

Every test+reference in the run is rendered in one weft process (`wpt-render.lisp`,
a file-backed subresource loader resolves the tests' relative / `/root-relative`
CSS and support files), then the PNG pairs are compared and pass/fail is reported
per category.  Requires Pillow.

## Running (testharness.js)

```sh
python3 inspect/wpt-harness.py ../wpt dom/nodes
python3 inspect/wpt-harness.py ../wpt url --limit 50
```

Reports subtest pass rate (each file has one or more `test(...)` subtests) and
file pass rate (a file all-passes when every subtest does).

## Baseline — full reftest run (2026-07-11)

Every reftest in the 34 checked-out CSS categories, run **unsampled** with the
full-page comparison (both renders padded to their common size and diffed whole,
not a top-left window), Ahem registered.  A reftest counts only when its reference
file is present locally (a sparse checkout omits some shared refs, which are
skipped, not failed).

**Aggregate: 4004 / 11847 = 33.8%**

| category | pass | | category | pass |
|---|---|---|---|---|
| css-ui | 89.0% (880/989) | | css-grid | 26.3% (387/1471) |
| css-images | 68.8% (298/433) | | css-text-decor | 25.0% (71/284) |
| css-variables | 59.6% (106/178) | | css-ruby | 24.8% (34/137) |
| css-conditional | 55.1% (59/107) | | css-position | 24.7% (56/227) †18 err |
| css-borders | 50.6% (39/77) | | css-display | 23.8% (19/80) |
| css-lists | 43.6% (71/163) | | css-overflow | 21.8% (108/496) |
| css-fonts | 39.4% (134/340) | | css-backgrounds | 21.5% (137/638) |
| css-transforms | 38.6% (297/770) | | css-tables | 21.2% (34/160) |
| CSS2 | 37.0% (133/359) | | css-align | 21.0% (13/62) |
| css-box | 37.0% (17/46) | | compositing | 20.0% (12/60) |
| css-pseudo | 33.9% (85/251) | | css-text | 19.2% (274/1424) |
| css-color | 32.8% (82/250) | | mediaqueries | 17.9% (10/56) |
| css-values | 31.3% (52/166) | | css-inline | 17.1% (28/164) |
| css-sizing | 31.1% (170/547) | | css-content | 16.9% (10/59) |
| css-cascade | 29.2% (14/48) | | css-multicol | 16.7% (56/336) |
| css-flexbox | 29.2% (248/848) | | css-writing-modes | 14.7% (56/380) |
| | | | css-counter-styles | 6.0% (14/235) |
| | | | css-logical | 0.0% (0/6) |

The runner registers WPT's **Ahem** font (every glyph a 1em square with fixed
metrics — the standard way CSS tests stay font-rendering-independent); without it
many text tests mismatch the fallback even when the layout is right.

Biggest pools of failing reftests (where a fix moves the most): **css-text**
(1150), **css-grid** (1084), **css-flexbox** (600), css-backgrounds (501),
css-overflow (388), css-sizing (377), css-writing-modes (324).  Whole-feature
gaps: css-logical (logical properties), css-counter-styles (`@counter-style`),
css-writing-modes (vertical writing).  18 css-position tests error during render
(a crash cluster, not a mismatch).

Excluded as out of scope for now: animations, transitions, scroll-timelines,
view-transitions, anchor-position, masking, filter-effects, the Houdini APIs,
forms, shapes; and the non-CSS suites (svg, html/rendering, mathml) — a
reasonable next expansion.  These are honest starting points for a from-scratch
engine; each failing category is a worklist of concrete, spec-referenced bugs.
