#!/usr/bin/env python3
"""wpt-run — run WPT reftests through weft and diff test vs reference.

A WPT reftest is an HTML page carrying <link rel="match" href="…-ref.html"> (must
render identically) or rel="mismatch" (must differ).  Both pages are rendered by
weft, so font/AA differences cancel — the comparison purely exercises weft's layout
of the tested feature against a reference that builds the same result another way.

  python3 inspect/wpt-run.py <wpt-root> <test-dir-under-wpt> [--limit N] [--save-fails]

Renders every reftest+reference in one weft process (batched), then compares the
PNG pairs and reports pass/fail per category.
"""
import sys, os, re, subprocess, tempfile, glob

def find_reftests(root, subdir):
    """Yield (test_html, ref_html, kind) for every reftest under root/subdir."""
    out = []
    link_re = re.compile(r'<link\s+[^>]*rel=["\']?(match|mismatch)["\']?[^>]*>', re.I)
    href_re = re.compile(r'href=["\']([^"\']+)["\']', re.I)
    for path in glob.glob(os.path.join(root, subdir, '**', '*.html'), recursive=True):
        if path.endswith('-ref.html') or '/reference/' in path:
            continue
        try:
            head = open(path, encoding='utf-8', errors='replace').read(4000)
        except Exception:
            continue
        m = link_re.search(head)
        if not m:
            continue
        kind = m.group(1).lower()
        h = href_re.search(m.group(0))
        if not h:
            continue
        ref = os.path.normpath(os.path.join(os.path.dirname(path), h.group(1).split('#')[0]))
        if os.path.exists(ref):
            out.append((path, ref, kind))
    return out

def load_full(png):
    from PIL import Image
    return Image.open(png).convert('RGB')

def pad_common(a, b):
    """Pad both images (white) to their common bounding size so the FULL rendered
    page is compared, not just a fixed top-left window — below-fold and right-of-
    fold divergences count.  Both are rendered by weft at the same width, so this
    just reconciles differing heights."""
    from PIL import Image
    W, H = max(a.width, b.width), max(a.height, b.height)
    ca = Image.new('RGB', (W, H), (255, 255, 255)); ca.paste(a, (0, 0))
    cb = Image.new('RGB', (W, H), (255, 255, 255)); cb.paste(b, (0, 0))
    return ca, cb

def main():
    root = os.path.abspath(sys.argv[1])
    subdir = sys.argv[2]
    limit = int(sys.argv[sys.argv.index('--limit') + 1]) if '--limit' in sys.argv else 0
    save_fails = '--save-fails' in sys.argv
    here = os.path.dirname(os.path.abspath(__file__))

    tests = find_reftests(root, subdir)
    if limit:
        tests = tests[:limit]
    print(f"{len(tests)} reftests under {subdir}")

    workdir = tempfile.mkdtemp(prefix='wpt-')
    jobs, meta = [], []
    for i, (t, r, kind) in enumerate(tests):
        tp = os.path.join(workdir, f'{i}-t.png'); rp = os.path.join(workdir, f'{i}-r.png')
        jobs.append(f'{t}\t{tp}\t{root}'); jobs.append(f'{r}\t{rp}\t{root}')
        meta.append((t, r, kind, tp, rp))
    manifest = os.path.join(workdir, 'jobs.txt')
    open(manifest, 'w').write('\n'.join(jobs) + '\n')

    print("rendering (one weft process)...")
    subprocess.run(['sbcl', '--dynamic-space-size', '4096', '--non-interactive',
                    '--load', os.path.join(here, 'wpt-render.lisp'), manifest],
                   stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, cwd=here)

    from PIL import ImageChops
    rank = '--rank' in sys.argv        # sort failures by how CLOSE weft got (diff pixels)
    npass = nfail = nerr = 0
    fails = []
    for (t, r, kind, tp, rp) in meta:
        if not (os.path.exists(tp) and os.path.exists(rp)):
            nerr += 1; continue
        try:
            a, b = pad_common(load_full(tp), load_full(rp))
            dimg = ImageChops.difference(a, b)
            bbox = dimg.getbbox()
            identical = bbox is None
        except Exception:
            nerr += 1; continue
        ok = identical if kind == 'match' else (not identical)
        if ok:
            npass += 1
        else:
            nfail += 1
            # diff magnitude: count of differing pixels (0 histogram bin = matching).
            # A near-miss (few differing pixels) is one small bug from a true pass.
            npix = (sum(dimg.convert('L').histogram()[1:]) if kind == 'match' else -1)
            fails.append((os.path.relpath(t, root), kind, npix, bbox))
    total = npass + nfail
    print(f"\n=== {subdir} ===")
    print(f"  PASS {npass}/{total}  ({100*npass/max(total,1):.1f}%)   FAIL {nfail}   render-err {nerr}")
    if fails:
        if rank:
            near = sorted((f for f in fails if f[2] >= 0), key=lambda f: f[2])
            print("  near-misses (fewest differing pixels first):")
            for f, k, npix, bbox in near[:40]:
                print(f"    {npix:>8} px  bbox={bbox}  {f}")
        else:
            print("  sample failures:")
            for f, k, npix, bbox in fails[:25]:
                print(f"    [{k}] {f}")
    if save_fails:
        open(os.path.join(here, 'wpt-fails.txt'), 'w').write('\n'.join(f[0] for f in fails))

if __name__ == '__main__':
    main()
