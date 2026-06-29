# weft

A **self-sovereign web engine in pure Common Lisp** — clean-room, no FFI, no
WebKit/Blink/Gecko. Built section by section against the web platform specs, with
the **Web Platform Tests** as the differential oracle (the same spec + reference +
fuzz discipline behind this project's siblings `zstd-pure` and `brotli-pure`).

This is the start of a long program, not a toy. The goal is an independent
engine in the spirit of [Ladybird](https://ladybird.org/): own your entire view
of the web, top to bottom, auditable.

## Status — P0 complete: URL + Encoding + Fetch ✅

`weft.fetch:fetch-text` already loads a real resource end-to-end — it decoded
`https://example.com`'s **live Brotli** with our own pure-CL decoder and pulled
the `<title>`, and `https://www.google.com` (gzip + ISO-8859-1, 81k chars).

### URL — WHATWG parser

`weft.url:parse` implements the [WHATWG URL Standard](https://url.spec.whatwg.org/)
basic URL parser: the full state machine, host parsing (domain / IPv4 / IPv6 /
opaque), IDNA ToASCII via punycode, percent-encoding sets, and URL serialization
(the `href` / `protocol` / `host` / `pathname` / `search` / `origin` … getters).

**Differential-tested against the Web Platform Tests url corpus**
(`inspect/vectors/urltestdata.json`, 888 cases): **868 pass (97.7%)**, comparing
every serialized component (or requiring a parse failure) against the reference.

Known tail (the remaining ~2.3%): **full UTS#46 IDNA mapping** — fullwidth→ASCII
folding, soft-hyphen removal, noncharacter/disallowed validation. Punycode is in;
the Unicode IDNA *mapping tables* are the scoped follow-up. (Plain ASCII,
punycode, and invalid-UTF-8 / U+FFFD domain rejection already work.)

### Fetch — resource loader (closes P0)

`weft.fetch` ties it together: parse URL → fetch via a **pluggable transport**
(default a curl backend; rebind `*http-transport*` for the real TLS/HTTP stack) →
strip **Content-Encoding** with the pure-CL codecs (`brotli-pure`, `zstd-pure`,
`chipz` for gzip/deflate) → **charset-decode** the body (Content-Type charset /
BOM sniff / UTF-8 default) through the 36 encoding decoders. The full WHATWG
**label alias** table (228 labels → 40 names, e.g. `latin1`/`iso-8859-1` →
windows-1252) is wired in. Offline gate: decodes real gzip/deflate/br/zstd bodies
and windows-1252 / Shift_JIS / UTF-16LE / UTF-8-BOM bodies, 10/10.

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
