#!/usr/bin/env python3
"""page-layout-diff — diff weft's element boxes against a real browser's, for any
page. Both JSONs are the <body>-subtree element list in document order
({els:[{d,el,x,y,w,h} | {d,el,box:null}]}). Matches by document order; prints a
TOTAL box error and the worst elements, plus a focused table-column view.
Usage: python3 inspect/page-layout-diff.py <weft.json> <browser.json> [--tables]
"""
import json, sys
weft = json.load(open(sys.argv[1]))["els"]
brow = json.load(open(sys.argv[2]))["els"]
tables_only = "--tables" in sys.argv
n = min(len(weft), len(brow))
if len(weft) != len(brow):
    print(f"NOTE: element counts differ (weft {len(weft)}, browser {len(brow)}) — matching first {n} in order")
rows, total = [], 0
for i in range(n):
    w, b = weft[i], brow[i]
    if w.get("box", 0) is None or "x" not in w:
        continue
    dx, dy, dw, dh = w["x"]-b["x"], w["y"]-b["y"], w["w"]-b["w"], w["h"]-b["h"]
    err = abs(dx)+abs(dy)+abs(dw)+abs(dh)
    total += err
    rows.append((err, b["el"], f"browser {b['w']:>4}x{b['h']:<3}@({b['x']:>3},{b['y']:>3})  "
                              f"weft {w['w']:>4}x{w['h']:<3}@({w['x']:>3},{w['y']:>3})  d=({dx:+d},{dy:+d},{dw:+d},{dh:+d})"))
print(f"TOTAL BOX ERROR: {total}\n")
shown = [r for r in rows if (('td' in r[1] or 'table' in r[1] or 'th' in r[1]) if tables_only else r[0] > 0)]
for err, el, detail in sorted(shown, key=lambda r: -r[0])[:30]:
    print(f"  [{err:>4}] {el:<20} {detail}")
