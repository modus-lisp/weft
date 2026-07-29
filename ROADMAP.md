# weft roadmap

A self-sovereign web engine in pure Common Lisp, built clean-room against the
web platform specifications, with the Web Platform Tests (WPT, ~2M cases) and
reference-engine render dumps as the differential oracle — the same
spec + reference + fuzz method used for `zstd-pure` / `brotli-pure`.

Target ambition: an **independent engine** (Ladybird-class), no WebKit/Blink/
Gecko inside. This is a multi-year program; the discipline is to advance in
dependency order and keep every component pinned to its conformance oracle.

## Principle

> Don't reverse-engineer a browser's *code* — implement the *specs*, which are
> already the reverse-engineering of browser behaviour, and use WPT + a reference
> engine as the oracle. Stop where the oracle stops being honest (pixels), and
> lean on reftests there.

## Where we actually are

P0–P4 are all built and oracle-gated. The frontier is no longer "does this phase
exist" — it is **conformance depth inside phases that already work**, and the
active work is the Web IDL surface at the end of P4.

`VALIDATIONS.md` is the authoritative evidence document; this file is the map.
Where the two disagree, VALIDATIONS.md and the gates themselves are right.

### P0 — Foundation  *(built; residue is data tables)*
- [x] **URL** — WHATWG URL parser. Oracle: WPT `urltestdata.json`.
      **868/888 (97.7%)**
- [ ] URL: full UTS#46 IDNA mapping tables; opaque-path space edge cases.
      *(the whole of the remaining 2.3%)*
- [x] **Encoding** — 36 charset decoders. Oracle: the reference codecs
      themselves. **37,923/37,923**
- [x] **Fetch glue** — URL + transport + `br`/`zstd`/`gzip` content-decoders
      wired into a resource loader; fetches live pages end-to-end.
- [ ] Fetch: byte-correctness per page is not asserted, only "decodes + doesn't
      crash".

### P1 — DOM  *(built)*
- [x] **HTML tokenizer** — Oracle: html5lib-tests. **6,677/6,677 (100%)**
- [x] **Tree construction → DOM** — Oracle: html5lib-tests `.dat`.
      **338/342 (98.8%)** non-fragment
- [ ] Tree construction: the deepest adoption-agency × table foster-parenting
      cases (4 failing); 21 fragment-parsing cases skipped.
- [x] DOM core (nodes, traversal, mutation, ranges). **116/116** query,
      **540/540** traversal/attributes.

### P2 — CSSOM  *(built)*
- [x] CSS tokenizer + parser; **38 value-type parsers**, spec-precise Python
      references as oracle. **369/369**
- [x] Selectors + specificity, cascade, inheritance, computed values.
      Oracle: soupsieve. **35/35**
- [ ] CSS grid; `inline-block` baseline alignment.

### P3 — Layout & paint  *(built; the oracle is the weak point, not the code)*
- [x] Box tree; block + inline layout; floats, positioning, tables, flex.
- [x] Software rasterizer → PNG (validated loadable by Pillow).
- [x] *First "it renders a real page" milestone* — Hacker News renders
      faithfully; **CI-gated** at box error ≤ 5500 vs Chromium (currently 4487).
- [x] **Acid2 at 100% pixel-match** vs the reference — 0/21,160 face pixels
      mismatched, 0% stray red. Two independent oracles (colour-class + per-
      element box diff vs Chromium), both permanent CI gates.
- [ ] **General layout on arbitrary pages is still author-eyeballed.** Acid2 and
      HN are two pages. The reftest tooling generalizes to any page; pointing it
      at a corpus is the open work.
- [ ] Text: fake-bold (no bold face vendored); per-line mixed-font-size baseline
      sharing.

### P4 — JavaScript  *(engine built and wired; IDL surface is the frontier)*
- [x] **JS engine** — [shuttle](https://github.com/modus-lisp/shuttle), a
      clean-room Lisp-native engine (no FFI). Oracle: test262.
      **41,404/47,058 (87.99%)** — including full Temporal, Intl, RegExp `\p{}`,
      BigInt, Proxy, classes/generators/async.
- [x] **The scripting seam** — `weft/script`: inline `<script>` reads and mutates
      the live DOM, reflected on relayout. Events, timers, mutation observers,
      CSSOM, canvas, SVG, traversal, ranges, XML.
- [ ] **Web IDL breadth — this is where the work is.** HTML forms IDL is done
      (see below); everything past it is unclaimed surface, and picking the next
      front is itself an open question.
- [ ] test262 residue: intl402/Temporal (real calendars + IANA tz), the 112
      `$262.agent` Atomics tests (real threads), exotic-locale output.
- [x] Acid3 re-measured now that shuttle is wired in: **100/100 scripted**
      (`inspect/acid3.lisp`, the page's own `score` global).
- [ ] Acid3 *rendering* vs its reference. The scripted 100/100 is not a full
      pass — the official test also wants a pixel match and the title to read
      `Acid3`, and there is no Acid3 pixel oracle the way there is for Acid2.

### P5 — The long tail
- [x] Text shaping / fonts via **scribe** (real outlines, shaping, AA
      compositing; own FreeType/HarfBuzz oracle).
- [x] Image decoders: PNG (incl. tRNS/Adam7), JPEG, WebP.
- [ ] The breadth of the Web Platform API surface.
- [ ] External images and web-fonts are not fetched during render.

## The current front: HTML forms IDL

Driven by a DeepSeek swarm against WPT `html/semantics/forms`, with
`inspect/forms-oracle.lisp` as the ratchet.

**TOTAL 2,832 subtests across 28 units — this front is CLOSED.** Wave history:
447 → 705 → 1,262 → 1,925 → 2,098 → 2,692 → 2,832.

Wave 10 took the remainder and stopped exactly at the ceiling the corpus sweep
predicted (2,755 + 77). Of the ~640 subtests still unpassed, ~440 are walls we
are not trying to climb — real form submission and navigation, `test_driver`
user gestures, cross-origin iframes, `.tentative` files — and ~60 are hard
residue four waves have not moved. The estimate and the outcome agreeing is what
makes this a close rather than a plateau: there is no hidden headroom left to
widen the aperture into.

The oracle scores a named set of files (its *aperture*) and scores every other
unit as a **sentinel** against a pinned best, ratcheting on the SUM — a gain in
one unit paid for out of another is not a gain. `(weft.forms-oracle:sweep DIR)`
and `inspect/forms-sweep-all.lisp` rank the corpus by unreached subtests;
**the next wave's roster comes from that measurement, not from the last wave's
narrative** — which has been wrong three times running.

`inspect/nonforms-gate.sh` runs the six suites the forms oracle cannot see. Any
wave touching shared code must leave it unchanged.

Known-unreachable residue, deliberately not chased: tests needing
`test_driver`/WebDriver input, real form submission into an iframe, and
reftests — these are *unscorable*, which is not the same as failing, and mixing
them into a denominator aims arms at a wall.

## Notes
- CL strengths landed where predicted (P0–P2, P4 parser: compiler-heavy,
  live-buildable, macros for IDL binding boilerplate). The predicted soft spot,
  P3 rasterization performance, is real and accepted — correctness first.
- Each phase ships as its own ASDF system/module with an `inspect/` gate.
- **No swarm output is trusted on its word.** Every arm patch is re-verified in
  the canonical tree (clean-cache compile + full gate) before it counts; several
  "passing" swarm results were caught as false this way and rewritten by hand.
