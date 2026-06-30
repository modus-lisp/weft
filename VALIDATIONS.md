# weft — validations

*Why we believe weft does what we claim, and where the claims stop.*

This document exists to fight self-delusion. It is easy to look at a rendered
page, decide it "works," and ship a lie. So every claim below is tagged by the
**strength of its evidence**, and the honest gaps are listed as plainly as the
wins. If you are evaluating weft, read the **Weak** and **Not validated**
sections first — that is where a demo would mislead you.

The governing principle: **a claim is only as strong as the independent oracle
behind it.** weft is built clean-room against the web platform specs and tested
*differentially* against reference implementations we did not write — the Web
Platform Tests, html5lib, soupsieve, reference codecs, and Python's reference
parsers. Where there is no independent oracle (notably: "does the rendered page
look right?"), the claim is explicitly marked weak.

Run everything: `sbcl --eval '(asdf:test-system "weft")'`. All oracle vectors are
**vendored** under `inspect/vectors/` so the gates are offline and cannot be
gamed by a flaky network.

---

## Strongly validated — independent oracle, exact comparison

These compare weft's output **byte-for-byte or structure-for-structure** against
a reference implementation. Passing means weft agrees with an authority, not with
itself.

| Component | Oracle (independent) | Result |
|---|---|---|
| URL parser | **Web Platform Tests** `urltestdata.json` (the WHATWG conformance suite) — every serialized component compared, failures required to fail | **868 / 888 (97.7%)** |
| Encoding (36 charsets) | the reference codecs themselves (Python `codecs` + `webcolors`), `errors='replace'`; decode every byte sequence | **37,923 / 37,923** |
| HTML tokenizer | **html5lib-tests** tokenizer suite — token stream compared across every initial state | **6,677 / 6,677 (100%)** |
| HTML tree construction → DOM | **html5lib-tests** tree-construction `.dat` suite — serialized DOM tree compared | **338 / 342 (98.8%)** non-fragment |
| DOM query methods | **html5lib** (matched element sets by node path) | **116 / 116** |
| DOM traversal/attributes | **html5lib** | **540 / 540** |
| CSS selectors | **soupsieve** (the same selector run on the same HTML; matched sets compared) | **35 / 35** |
| CSS value parsers (38 types) | spec-precise Python references + `webcolors` | **369 / 369** |
| Content-Encoding + charset (fetch) | real gzip/deflate/brotli/zstd bodies produced by Python; round-tripped | **10 / 10** |
| Sibling codecs (used by fetch) | **libzstd** and the **reference brotli** (node/python): differential fuzz | zstd 150 cases, brotli 1,100 cases — byte-for-byte |
| PNG output | **Pillow (PIL)** opens and validates every PNG weft writes | every render verified loadable |

Why we trust these: the oracle is software we did not author, the comparison is
exact (not "looks similar"), the vectors are vendored, and the harness fails on
mismatch. The selector and DOM oracles were even regenerated with the **html5lib
tree builder** so node paths align with weft's conformant tree — a subtle trap we
caught and fixed rather than papered over.

## Medium — real inputs, weaker assertion

| Component | Evidence | Caveat |
|---|---|---|
| Resource loader | fetches and decodes **live pages** end-to-end (example.com over our own Brotli; Wikipedia 413 KB over gzip) | asserts "decodes to text + doesn't crash", not byte-correctness of every page |
| Layout robustness | renders live 400 KB+ pages and the vendored Acid tests **without crashing** (error-resilient: a bad subtree degrades to an empty box) | "doesn't crash" ≠ "lays out correctly" |

## Weak — self-asserted, NO independent oracle ⚠️

**This is the soft underbelly. Treat rendered-page screenshots with suspicion.**

- **Layout correctness** (block/inline/float/flex/table/position, sizing,
  margins). There is *no pixel oracle*. Correctness is judged by **a human (the
  author) eyeballing the output** against what a browser would do. The test
  pages were also written by the author, so they exercise what we already
  support. This is exactly the kind of evidence that produces self-delusion, and
  it is the single weakest claim in the project.
- **Paint** (colors, borders, gradients, text). Same: looks right to the eye,
  not diffed against a reference rendering. Text is now rendered by **scribe**
  (a pure-CL OpenType engine: real glyph outlines, shaping, anti-aliased
  gamma-correct compositing) with weft's 7×13 bitmap as fallback; scribe has its
  own differential oracle (FreeType/HarfBuzz) but weft's *use* of it — baseline
  placement, metric-driven wrapping/alignment — is still author-eyeballed, not
  pixel-diffed.

**This gap is now partly closed.** We built the reference-image diff harness we
said we lacked — two independent pixel/geometry oracles against a **real browser**
(Chromium via Playwright), for Acid2:
- `inspect/acid2-reftest.py` — colour-class agreement of weft's rendered face vs
  the canonical reference smiley, auto-aligned (slide-and-match).
- `inspect/acid2-layout-dump.lisp` + `inspect/acid2-layout-diff.py` — a
  **per-element** diff: every `.picture` descendant's box (x,y,w,h) vs Chromium's
  ground truth (`acid2-browser-layout.json`), so a regression names the element
  and the delta. This generalizes to a layout reftest for **any** page.
On Acid2 these reached **99.9% pixel match** (the 0.1% residual is the reference
browser's edge anti-aliasing, which weft's hard-edged fill can't bit-match — an
asymptote, not a defect). This is the first *independent, exact* validation of
weft's layout+paint. The caveat stands for everything ELSE: general layout
correctness on arbitrary pages is still author-eyeballed — but the tooling to
diff any page against a real browser now exists.

## Not validated / known limits — stated plainly

- **No JavaScript.** No JS engine exists (that is P4). Pages that render content
  client-side (SPAs, infinite scroll, most modern app UIs) render **blank**.
- **Acid3 cannot run** — it is ~99% JavaScript (183 `createElement`, 14
  `<script>` blocks). weft renders only its static pre-script state. See the Acid
  gate below.
- **Acid2 renders at 99.9% pixel-match vs a real browser** (up from ~0%). Driven
  to convergence by an objective oracle (the per-element + colour-class diffs
  above), the face assembles correctly: crown, two green-pupil eyes, black
  diamond nose, curved smile, chin — colour-class agreement **99.9%**, stray-red
  (Acid2's error colour) **0%**. Getting here built and verified a long list of
  *general* engine features, each oracle-gated and re-checked in the canonical
  tree: generated content, data-URI/element/**fixed**-attachment background
  images (+ PNG tRNS/Adam7), `<object>` images, the full positioning + viewport +
  scroll-to-anchor model, CSS2.1 margin collapsing, percentage sizing, a complete
  per-edge/mitered **border model**, appendix-E paint order, anonymous table rows
  + shrink-to-fit, line-box metrics from real fonts, **standards/quirks-mode
  DOCTYPE** determination, and several CSS parser/selector conformance fixes.
  Honest limits: the remaining 0.1% is edge anti-aliasing (asymptote); the match
  is against the reference *image* (colour classes), not a byte-identical
  framebuffer; and Acid2's interactive `:hover` sub-tests are not exercised
  (static render only). We do not call it an official "pass" — we call it
  **99.9% pixel-match, independently measured**.
- **No CSS grid, no `inline-block` baseline alignment.** Text is real now
  (scribe: anti-aliased, font-metric-driven advances/wrapping); remaining text
  gaps are **fake-bold** (stem-darkening, no bold font vendored) and **per-line
  mixed-font-size baseline sharing** (each inline size is centered in the line
  box independently rather than on a common baseline — fine for uniform lines).
- **URL: ~2.3% fail** — full UTS-46 IDNA mapping tables not implemented.
- **Tree construction: 4 fail** — the deepest adoption-agency × table
  foster-parenting cases; 21 fragment-parsing cases skipped.
- **The swarm** (DeepSeek-Flash via OpenRouter) wrote a number of CSS value
  parsers (most recently `object-fit`, `text-indent`, `background-repeat`,
  `background-position`), but **no swarm output is trusted on its word.** Every
  file is re-verified in the main tree (clean-cache compile + full gate) before
  it counts; see `tools/swarm/README.md` and `../combat/SWARM.md` for the
  isolation proof (a stub in a worker copy fails even when main is correct) and
  the stale-cache defenses. Several "passing" swarm results were caught as false
  by this re-verify and rewritten by hand.

## The Acid tests as a permanent gate

`inspect/acid-test.lisp` (in `weft/test`) renders the vendored `acid2.html` and
`acid3.html` on every run as a **robustness guard** — the build fails if these
real, gnarly pages ever *error*. Conformance is measured separately and honestly:

- **Acid2** — `inspect/acid2-reftest.py` (colour-class match vs the reference
  smiley) and `inspect/acid2-layout-diff.py` (per-element box diff vs Chromium
  ground truth). Currently **99.9% pixel-match, 0% stray red** (the 0.1% is edge
  AA). The grind from ~0% → 99.9% is recorded commit-by-commit (each subject
  carries its `face-ink`/`face-geom` delta). Re-run: render via
  `(weft.acid.test:run)`, then the two scripts.
- **Acid3** — still ~99% JavaScript; weft renders only its static pre-script
  state until the JS engine (P4) lands. Not runnable yet.

---

*Last updated alongside the Acid2 grind (→ 99.9% pixel-match). If a number here
disagrees with `asdf:test-system` or the `inspect/acid2-*` oracles, those are
right and this file is stale — fix this file.*
