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

## Phases (dependency order, each with its oracle)

### P0 — Foundation  *(in progress)*
- [x] **URL** — WHATWG URL parser. Oracle: WPT `urltestdata.json`. *(97.2%)*
- [ ] URL: full UTS#46 IDNA mapping tables; opaque-path space edge cases.
- [ ] **Encoding** — UTF-8 + legacy charset decoders. Oracle: WPT `encoding/`.
- [ ] **Fetch glue** — wire URL + the existing transport (TLS/HTTP on the other
      host) + the `br`/`zstd`/`gzip` content-decoders into a resource loader.

### P1 — DOM
- [ ] **HTML** — WHATWG tokenizer + tree construction → DOM. Oracle:
      `html5lib-tests` (thousands of broken-markup cases — HTML5 *is* the
      error-recovery spec, so conformant == realistic).
- [ ] DOM core (nodes, traversal, mutation) + ranges.

### P2 — CSSOM
- [ ] CSS tokenizer + parser (Syntax spec). Oracle: WPT `css/`.
- [ ] Selectors + specificity; the cascade, inheritance, computed values.

### P3 — Layout & paint  *(oracle weakens to reftests here)*
- [ ] Box tree; block + inline layout; then flexbox, grid, floats, positioning.
- [ ] Software rasterizer → PNG. Oracle: WPT reference tests + render-tree dumps.
- [ ] *First "it renders a real page" milestone.*

### P4 — JavaScript
- [ ] Lexer → parser (ESTree-ish) → bytecode → interpreter + GC. Oracle: test262.
- [ ] Web IDL bindings (DOM/Events/Fetch APIs) + the event loop.

### P5 — The long tail
- [ ] Text shaping / fonts; image decoders (PNG/JPEG/WebP — more codecs).
- [ ] The breadth of the Web Platform API surface.

## Notes
- CL strengths land in P0–P2 and P4-parser (compiler-heavy, live-buildable,
  CLOS for the node hierarchy, macros for IDL binding boilerplate). The known
  soft spot is P3 rasterization performance — acceptable for correctness-first.
- Each phase ships as its own ASDF system/module with an `inspect/` gate, like
  the sibling projects.
