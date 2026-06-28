# weft

A **self-sovereign web engine in pure Common Lisp** — clean-room, no FFI, no
WebKit/Blink/Gecko. Built section by section against the web platform specs, with
the **Web Platform Tests** as the differential oracle (the same spec + reference +
fuzz discipline behind this project's siblings `zstd-pure` and `brotli-pure`).

This is the start of a long program, not a toy. The goal is an independent
engine in the spirit of [Ladybird](https://ladybird.org/): own your entire view
of the web, top to bottom, auditable.

## Status — P0 begun: WHATWG URL parser ✅

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
  encoding/  character decoders (WHATWG Encoding): kernel + UTF-8/16, single-byte,
             and CJK (built in parallel by an agent swarm — see below)
inspect/
  vectors/           urltestdata.json + encoding/*.json (vendored oracles)
  offline-test.lisp  URL gate (self-contained JSON reader + WPT differential)
  encoding-test.lisp encoding gate (per-charset differential vs reference codec)
```

### P0 encoding decoders — built by a parallel agent swarm

The decoder kernel + oracle were laid by hand; the decoders themselves were then
built **in parallel by a fleet of cheap-model coding agents** (`operandi` on
DeepSeek-Flash, one worker per charset in an isolated tree, each looping the
per-charset gate until green). First wave: **8/10 charsets passing** — UTF-8 (ref),
UTF-16 LE/BE, windows-1252/1251, ISO-8859-2, KOI8-R, and Big5 — verified to
generalize beyond the test vectors. The three hardest multi-byte CJK encodings
(Shift_JIS, EUC-JP, EUC-KR) are deferred to a stronger model: the cheap swarm
nailed the wide/uniform units and stalled exactly on the tall-pole ones.

## Use

```lisp
(asdf:load-system "weft")
(let ((u (weft.url:parse "../b?q#f" "http://example.org/a/c")))
  (weft.url:href u))        ; => "http://example.org/a/b?q#f"
```

Run the gate: `sbcl --eval '(asdf:test-system "weft")'`

## License

MIT.
