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

What would make these strong: a reference-image diff harness (render the same
page in a real browser, compare pixels with tolerance — WPT reftests do this).
We do not have it. Until we do, **"the showcase looks like a web page" is not
proof the layout engine is correct** — only that it is plausible on inputs we
chose.

## Not validated / known limits — stated plainly

- **No JavaScript.** No JS engine exists (that is P4). Pages that render content
  client-side (SPAs, infinite scroll, most modern app UIs) render **blank**.
- **Acid3 cannot run** — it is ~99% JavaScript (183 `createElement`, 14
  `<script>` blocks). weft renders only its static pre-script state. See the Acid
  gate below.
- **Acid2 does not pass.** Two of its prerequisites now exist and are verified in
  isolation — **`::before`/`::after` generated content** (inline *and* empty-
  content `display:block` border boxes) and **data-URI image decoding** (a real
  PNG decoder: filters 0–4, colour types 0/2/3/4/6, alpha). With those plus a
  latent crash fix, the Acid gate's ink coverage went **~0% → ~16%**: the face
  *parts* now decode and paint. What remains is the hard, coupled part — pixel-
  exact `position:fixed`/absolute placement, float shrink-wrap, CSS2.1 margin
  collapsing, paint/stacking order, and `overflow:hidden` clipping — without
  which the parts do not assemble into the smiley. **16% ink is parts-on-canvas,
  not a face.** We will only claim a pass when the render matches the reference.
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
`acid3.html` on every run. It is **not** a pass/fail conformance claim — weft
fails Acid2 and cannot run Acid3. It is two honest things:

1. a **robustness guard** — the build fails if rendering these real, gnarly pages
   ever *errors*;
2. a **progress tracker** — it prints each render's **ink coverage** (fraction of
   painted pixels). Acid2 sits near 0% today; as we implement generated content
   and data-URI images, that number should climb toward the smiley. The PNGs are
   written next to the vendored sources so the visual progress is inspectable.

When Acid2's ink coverage climbs and the render starts to resemble the reference
smiley (`acid2-reference.html`), *that* will be real evidence — and only then will
we claim it.

---

*Last updated alongside the layout-hardening work. If a number here disagrees
with `asdf:test-system`, the test system is right and this file is stale — fix
this file.*
