#!/usr/bin/env python3
"""wpt-harness — run WPT testharness.js tests through weft and tally subtests.

testharness.js tests are the assert-based JS suite covering the non-visual browser
(DOM, HTML parsing, URL, encoding, events…).  weft runs them via shuttle.  Each
test file contributes one or more subtests; this reports subtest pass rate and
file pass rate (a file passes when all its subtests pass).

  python3 inspect/wpt-harness.py <wpt-root> <dir-under-wpt> [--limit N] [--save-fails]
"""
import sys, os, re, subprocess, tempfile, glob, json

def find_tests(root, subdir):
    out = []
    for path in glob.glob(os.path.join(root, subdir, '**', '*.html'), recursive=True):
        try:
            head = open(path, encoding='utf-8', errors='replace').read(2000)
        except Exception:
            continue
        if 'testharness.js' in head and 'testharnessreport' in head:
            # skip tests that need infra weft doesn't have (fetch of test data, workers)
            if re.search(r'\.window\.js|\.worker\.js', path):
                continue
            out.append(path)
    return out

def main():
    root = os.path.abspath(sys.argv[1]); subdir = sys.argv[2]
    limit = int(sys.argv[sys.argv.index('--limit')+1]) if '--limit' in sys.argv else 0
    save = '--save-fails' in sys.argv
    here = os.path.dirname(os.path.abspath(__file__))

    tests = find_tests(root, subdir)
    if limit: tests = tests[:limit]
    print(f"{len(tests)} testharness files under {subdir}")

    wd = tempfile.mkdtemp(prefix='wpth-')
    jobs, meta = [], []
    for i, t in enumerate(tests):
        rj = os.path.join(wd, f'{i}.json')
        jobs.append(f'{t}\t{rj}\t{root}'); meta.append((t, rj))
    man = os.path.join(wd, 'jobs.txt'); open(man, 'w').write('\n'.join(jobs) + '\n')

    print("running (one weft process)...")
    subprocess.run(['sbcl', '--dynamic-space-size', '4096', '--non-interactive',
                    '--load', os.path.join(here, 'wpt-harness.lisp'), man],
                   stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, cwd=here)

    sub_pass = sub_total = 0
    file_pass = file_total = file_err = 0
    fails = []
    for (t, rj) in meta:
        try:
            res = json.load(open(rj))
        except Exception:
            file_err += 1; continue
        if isinstance(res, dict) and 'error' in res:
            file_err += 1; continue
        if not res:                      # ran but produced no subtests
            file_err += 1; continue
        file_total += 1
        allok = True
        for entry in res:
            name, status = entry[0], entry[1]
            sub_total += 1
            if status == 0: sub_pass += 1
            else: allok = False
        if allok: file_pass += 1
        else: fails.append(os.path.relpath(t, root))

    print(f"\n=== {subdir} ===")
    print(f"  subtests: {sub_pass}/{sub_total} pass ({100*sub_pass/max(sub_total,1):.1f}%)")
    print(f"  files:    {file_pass}/{file_total} all-pass ({100*file_pass/max(file_total,1):.1f}%)   no-result {file_err}")
    if fails:
        print("  sample failing files:")
        for f in fails[:20]: print(f"    {f}")
    if save:
        open(os.path.join(here, 'wpt-harness-fails.txt'), 'w').write('\n'.join(fails))

if __name__ == '__main__':
    main()
