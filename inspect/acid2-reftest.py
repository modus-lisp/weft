#!/usr/bin/env python3
"""acid2-reftest — an OBJECTIVE pixel oracle for the Acid2 face.

weft has no other pixel oracle for layout (see VALIDATIONS.md: layout/paint are
the "soft underbelly", self-asserted). This scores weft's rendered Acid2 face
against the canonical reference smiley by colour-class agreement, auto-aligning
the reference over the render (slide-and-match) so it tolerates a few px of
positional error. It turns "does the face look right?" into a number that goes
up only when the render genuinely gets closer to the reference.

Usage:
    # (re)render first:  sbcl ... (weft.acid.test:run)   writes acid2-weft.png
    python3 inspect/acid2-reftest.py
    -> prints  FACE-INK MATCH: NN.N%   (the headline number to drive up)

Reports:
  - overall match   (incl. shared white bg; inflated, less useful)
  - FACE-INK match  (of the reference's NON-white pixels — the real signal)
  - stray red       (red is Acid2's "error" colour; should trend to ~0)
  - best offset     (where the face aligned)
Reference: inspect/vectors/acid/acid2-reference.png (canonical, vendored).
"""
import sys, os
try:
    from PIL import Image
    import numpy as np
except Exception as e:
    print("acid2-reftest needs Pillow+numpy:", e); sys.exit(2)

HERE = os.path.dirname(os.path.abspath(__file__))
REF = os.path.join(HERE, "vectors/acid/acid2-reference.png")
WEFT = os.path.join(HERE, "vectors/acid/acid2-weft.png")

def classify(arr):
    r, g, b = (arr[..., i].astype(int) for i in range(3))
    cls = np.full(r.shape, 4, np.uint8)          # 4 = other
    cls[(r > 200) & (g > 200) & (b > 200)] = 0   # white
    cls[(r < 80) & (g < 80) & (b < 80)] = 1      # black
    cls[(r > 200) & (g > 200) & (b < 100)] = 2   # yellow
    cls[(r > 180) & (g < 90) & (b < 90)] = 3     # red
    return cls

def main():
    if not (os.path.exists(REF) and os.path.exists(WEFT)):
        print("missing", REF if not os.path.exists(REF) else WEFT); sys.exit(2)
    rc = classify(np.asarray(Image.open(REF).convert("RGB"))); H, W = rc.shape
    wc = classify(np.asarray(Image.open(WEFT).convert("RGB"))); WH, WW = wc.shape
    nonwhite = rc != 0
    best = (-1.0, 0, 0)
    for oy in range(0, min(380, WH - H), 2):
        for ox in range(0, min(380, WW - W), 2):
            win = wc[oy:oy + H, ox:ox + W]
            ink = ((win == rc) & nonwhite).sum() / max(1, nonwhite.sum())
            if ink > best[0]:
                best = (ink, ox, oy)
    ink, ox, oy = best
    win = wc[oy:oy + H, ox:ox + W]
    overall = (win == rc).mean()
    red = (win == 3).mean()
    print(f"overall match : {overall*100:5.1f}%  (incl. shared white background)")
    print(f"FACE-INK MATCH: {ink*100:5.1f}%  (the number to drive up)")
    print(f"stray red     : {red*100:5.1f}%  (Acid2 error colour; -> 0)")
    print(f"best offset   : ({ox},{oy})")

if __name__ == "__main__":
    main()
