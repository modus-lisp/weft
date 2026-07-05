# WPT reftest runner

Runs [web-platform-tests](https://github.com/web-platform-tests/wpt) **reftests**
against weft.  A reftest is an HTML page carrying `<link rel="match" href="…-ref.html">`
(must render identically to the reference) or `rel="mismatch"` (must differ).  Both
pages are rendered by weft, so font and anti-aliasing differences cancel — the
comparison purely exercises weft's layout of the tested feature against a reference
that produces the same result another way.  This covers a far larger and more
targeted slice of the platform than the Acid tests, and pinpoints gaps by feature.

## Getting the tests

A partial + sparse checkout keeps it small (the full suite is ~2 GB):

```sh
git clone --no-checkout --depth 1 --filter=blob:none \
    https://github.com/web-platform-tests/wpt.git ../wpt
cd ../wpt
git sparse-checkout set css/css-flexbox css/css-text css/css-position fonts common css/support
git checkout
```

## Running

```sh
python3 inspect/wpt-run.py ../wpt css/css-position            # a whole category
python3 inspect/wpt-run.py ../wpt css/css-flexbox --limit 50  # a sample
python3 inspect/wpt-run.py ../wpt css/css-text --save-fails   # write failing paths
```

Every test+reference in the run is rendered in one weft process (`wpt-render.lisp`,
a file-backed subresource loader resolves the tests' relative / `/root-relative`
CSS and support files), then the PNG pairs are compared and pass/fail is reported
per category.  Requires Pillow.

## Baseline (indicative, sampled)

| category      | pass |
|---------------|------|
| css-position  | ~27% |
| css-text      | ~27% |
| css-flexbox   | ~10% (single-line/row-focused; no column shrink, wrap, align-content, baseline) |

These are honest starting points for a from-scratch engine; each failing category
is a worklist of concrete, spec-referenced layout bugs.
