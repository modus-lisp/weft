# weft

A **self-sovereign web engine in pure Common Lisp** — clean-room, no FFI, no
WebKit/Blink/Gecko. Built section by section against the web platform specs, with
the **Web Platform Tests** as the differential oracle (the same spec + reference +
fuzz discipline behind this project's siblings `zstd-pure` and `brotli-pure`).

This is the start of a long program, not a toy. The goal is an independent
engine in the spirit of [Ladybird](https://ladybird.org/): own your entire view
of the web, top to bottom, auditable.

## Status — P0–P3: fetch → DOM → CSS → layout → **pixels** ✅

weft now renders styled pages to PNG images, in pure Common Lisp, no browser
engine. The pipeline: **URL → fetch (TLS-less transport hook + pure-CL br/zstd/
gzip) → charset decode (36 decoders) → HTML parse (DOM, 98.8% html5lib) → CSS
(tokenize → parse → selectors vs soupsieve → cascade → computed style) → layout
(block, inline formatting, floats, flexbox, tables, positioning, width
constraints) → paint (text, borders, gradients) → PNG**.

- **P0** URL (97.7% WPT) · Encoding (36 charsets, 37923 cases) · Fetch — done.
- **P1** HTML tokenizer (6677/6677) · DOM + 10 interface methods · tree
  construction (338/342 = 98.8%).
- **P2** CSS: 37 value-type parsers (309/0) · tokenizer + rule/declaration parser
  · selector engine (35/35 vs soupsieve, specificity, querySelector) · cascade +
  computed style.
- **P3** layout + paint: normal flow, inline formatting (styled runs, bold,
  links, lists), floats + clear, flexbox (grow/justify/align/gap), tables,
  position (relative/absolute/fixed), max/min-width + margin:auto, linear-
  gradient backgrounds; a software rasterizer + own PNG encoder.

Run it: `sbcl --script demo/render-url.lisp <url> out.png 900`  (renders a live
page) or `demo/reader.lisp` / `demo/browse.lisp` for text views.

Not yet: JavaScript (P4), grid, inline-block, sub-pixel text/AA, the long tail of
CSS. Real JS-heavy pages render blank; server-rendered pages render.


## Honesty

See [VALIDATIONS.md](VALIDATIONS.md) — what is validated against independent
oracles (URL/encoding/tokenizer/tree/DOM/selectors vs WPT, html5lib, soupsieve),
what is only self-asserted (layout/paint correctness — no pixel oracle yet), and
the known limits (no JS; Acid3 cannot run; Acid2 does not pass). Read it before
trusting any screenshot.

## Roadmap

See [ROADMAP.md](ROADMAP.md). In dependency order, each phase backed by its
conformance oracle: **P0** URL + encoding + fetch glue (reusing the `br`/`zstd`/
`gzip` decoders) → **P1** HTML→DOM (html5lib-tests) → **P2** CSS→CSSOM (WPT) →
**P3** layout + software raster (WPT reftests) → **P4** JavaScript (test262) →
**P5** the long tail.

## Layout

```
src/
  packages   package definitions
  url        WHATWG URL parser + host/IPv4/IPv6/punycode + serialization
  encoding/  character decoders (WHATWG Encoding): kernel + label aliases +
             UTF-8/16, single-byte, and CJK (built in parallel by an agent swarm)
  fetch      resource loader: URL -> transport -> content-decode -> charset
inspect/
  vectors/           urltestdata.json + encoding/*.json (vendored oracles)
  offline-test.lisp  URL gate (self-contained JSON reader + WPT differential)
  encoding-test.lisp encoding gate (per-charset differential vs reference codec)
```

### P0 encoding decoders — built by a parallel agent swarm

**36 charsets pass** their differential suites (37,923 cases total) — effectively
the whole WHATWG Encoding decoder set (UTF-8/16, every single-byte family, and the
CJK multi-byte: Shift_JIS, EUC-JP, EUC-KR, Big5, GBK, GB18030).
The decoder kernel + vendored oracle were laid by hand; the decoders were then
built **in parallel by a fleet of cheap-model coding agents** (`operandi` on
DeepSeek-Flash, one worker per charset in an isolated copy of the tree, each
looping its per-charset gate until green). Three waves, **~$0.74 total**:

- **Wave 1** (10 workers, ~$0.55): the wide/uniform units landed first try —
  UTF-16 LE/BE, windows-1252/1251, ISO-8859-2, KOI8-R, Big5 — verified to
  generalize beyond the vectors. The three hardest multi-byte CJK encodings
  (Shift_JIS, EUC-JP, EUC-KR) failed: the cheap model stalled when asked to
  *both* generate a ~10k-entry table *and* hand-bake it as a literal.
- **Wave 2** (~$0.10): the fix was decomposition, not a bigger model — the strong
  tier pre-generates the byte→codepoint tables as vendored data and the worker
  writes only the ~15-line dispatch. Shift_JIS and EUC-KR then passed cleanly;
  EUC-JP reached 1291/1293, and the strong tier finished its 2-case SS3
  (`0x8F`) error-handling residue by hand.
- **Wave 3** (24 workers, ~$0.06): the rest of the single-byte families + GBK,
  all green first try (~$0.0025 each — single-byte converges in 1–2 iterations).
  Only GB18030's 4-byte algorithmic mapping was left to the strong tier (its
  207-entry linear-range table + the error rule, derived from the reference codec).

The lesson — and the operating model for the whole engine: a cheap swarm carries
the wide, oracle-pinned units for cents *once the strong tier carves the
data/logic boundary*; the strong tier owns the genuinely coupled residue.

## Use

```lisp
(asdf:load-system "weft")
(let ((u (weft.url:parse "../b?q#f" "http://example.org/a/c")))
  (weft.url:href u))        ; => "http://example.org/a/b?q#f"
```

Run the gate: `sbcl --eval '(asdf:test-system "weft")'`

## License

MIT.
