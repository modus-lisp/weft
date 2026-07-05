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

def load_gray(png, box=(800, 600)):
    from PIL import Image
    im = Image.open(png).convert('RGB')
    W, H = box
    c = Image.new('RGB', (W, H), (255, 255, 255))
    c.paste(im.crop((0, 0, min(W, im.width), min(H, im.height))), (0, 0))
    return c

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
    npass = nfail = nerr = 0
    fails = []
    for (t, r, kind, tp, rp) in meta:
        if not (os.path.exists(tp) and os.path.exists(rp)):
            nerr += 1; continue
        try:
            a, b = load_gray(tp), load_gray(rp)
            diff = ImageChops.difference(a, b).getbbox()
            identical = diff is None
        except Exception:
            nerr += 1; continue
        ok = identical if kind == 'match' else (not identical)
        if ok:
            npass += 1
        else:
            nfail += 1
            fails.append((os.path.relpath(t, root), kind))
    total = npass + nfail
    print(f"\n=== {subdir} ===")
    print(f"  PASS {npass}/{total}  ({100*npass/max(total,1):.1f}%)   FAIL {nfail}   render-err {nerr}")
    if fails:
        print("  sample failures:")
        for f, k in fails[:25]:
            print(f"    [{k}] {f}")
    if save_fails:
        open(os.path.join(here, 'wpt-fails.txt'), 'w').write('\n'.join(f for f, _ in fails))

if __name__ == '__main__':
    main()
