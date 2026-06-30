#!/usr/bin/env python3
"""acid2-layout-diff — per-element layout oracle for the Acid2 face.

Diffs weft's element box geometry against a REAL BROWSER's (Chromium via
Playwright). Far more actionable than the pixel oracle: instead of "black where
yellow", it says exactly WHICH element is mis-positioned/mis-sized and by how
much, so a fix targets the responsible CSS behaviour.

Inputs (both walk the .picture subtree in document order, boxes relative to
.picture's top-left):
  inspect/vectors/acid/acid2-browser-layout.json  (ground truth, vendored)
  inspect/vectors/acid/acid2-weft-layout.json     (produced by acid2-layout-dump.lisp)

Usage:
  sbcl --non-interactive --load inspect/acid2-layout-dump.lisp   # refresh weft side
  python3 inspect/acid2-layout-diff.py [--all]
Prints per-element deltas sorted by error (worst first) and a TOTAL geometry
error (sum of |dx|+|dy|+|dw|+|dh| over boxed elements) — the number to drive to 0.
"""
import json, os, sys

HERE = os.path.dirname(os.path.abspath(__file__))
BROW = os.path.join(HERE, "vectors/acid/acid2-browser-layout.json")
WEFT = os.path.join(HERE, "vectors/acid/acid2-weft-layout.json")

def main():
    show_all = "--all" in sys.argv
    brow = json.load(open(BROW))
    bels = brow["els"]
    weft = json.load(open(WEFT))
    # Match by document order. Both lists are the .picture DFS element sequence.
    n = min(len(bels), len(weft))
    if len(bels) != len(weft):
        print(f"NOTE: element count differs (browser {len(bels)}, weft {len(weft)}) — "
              f"matching first {n} in order; a mismatch usually means weft dropped/added a box.")
    rows = []
    total = 0
    for i in range(n):
        b, w = bels[i], weft[i]
        name = w.get("el", b["el"])
        if "box" in w and w["box"] is None:
            # weft doesn't box this element (inline); skip from geometry error
            rows.append((0, name, "(weft: no box — inline)"))
            continue
        if b.get("disp") in (None,) :
            pass
        dx, dy = w["x"] - b["x"], w["y"] - b["y"]
        dw, dh = w["w"] - b["w"], w["h"] - b["h"]
        err = abs(dx) + abs(dy) + abs(dw) + abs(dh)
        total += err
        detail = (f"browser @({b['x']:>4},{b['y']:>4}) {b['w']:>3}x{b['h']:>3}   "
                  f"weft @({w['x']:>4},{w['y']:>4}) {w['w']:>3}x{w['h']:>3}   "
                  f"d=({dx:+d},{dy:+d},{dw:+d},{dh:+d})")
        rows.append((err, name, detail))
    rows.sort(key=lambda r: -r[0])
    print(f"TOTAL GEOMETRY ERROR: {total}  (sum |dx|+|dy|+|dw|+|dh| over boxed elements; drive to 0)\n")
    shown = 0
    for err, name, detail in rows:
        if err == 0 and not show_all:
            continue
        print(f"  [{err:>4}] {name:<26} {detail}")
        shown += 1
    if not show_all:
        print(f"\n({shown} elements with error shown; pass --all to see matches too)")

if __name__ == "__main__":
    main()
