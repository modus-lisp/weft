// gbcr-chrome.js — reference getBoundingClientRect dump, for grading weft's own.
//
// Loads each local HTML file in Chromium at a fixed width and prints, for every
// rendered element, a structural path plus the rect Chrome's getBoundingClientRect
// returns.  weft dumps the same rows through ITS getBoundingClientRect and the two
// are diffed row by row: the oracle is the very API being implemented, so a
// disagreement is a real difference in answer rather than a difference in method.
//
// pathOf is character-identical to inspect/hn-audit-chrome.js's, so the two
// harnesses name the same element the same way.
//
// Usage: node gbcr-chrome.js <width> <file.html> [file.html ...]
const { chromium } = require((process.env.PW_HOME || require('os').homedir() + '/pw') + '/node_modules/playwright');
const path = require('path');

const W = parseInt(process.argv[2] || '800', 10);
const files = process.argv.slice(3);

const DUMP = () => {
  function pathOf(el) {
    const parts = [];
    while (el && el.nodeType === 1 && el.tagName !== 'HTML') {
      const p = el.parentElement;
      if (!p) break;
      const same = [...p.children].filter(c => c.tagName === el.tagName);
      parts.unshift(el.tagName.toLowerCase() + ':' + (same.indexOf(el) + 1));
      el = p;
    }
    return parts.join('/');
  }
  const out = [];
  for (const el of document.querySelectorAll('*')) {
    if (el.tagName === 'SCRIPT' || el.tagName === 'STYLE') continue;
    const cs = getComputedStyle(el);
    if (cs.display === 'none' || cs.visibility === 'hidden') continue;
    const r = el.getBoundingClientRect();
    // Document coordinates: weft reports an unscrolled page, so add the scroll
    // back rather than comparing two different origins.
    out.push([pathOf(el), Math.round(r.x + window.scrollX), Math.round(r.y + window.scrollY),
              Math.round(r.width), Math.round(r.height),
              el.offsetLeft, el.offsetTop, el.offsetWidth, el.offsetHeight,
              el.clientLeft, el.clientTop, el.clientWidth, el.clientHeight,
              el.scrollWidth, el.scrollHeight,
              el.offsetParent ? pathOf(el.offsetParent) : '-'].join('\t'));
  }
  return out.join('\n');
};

(async () => {
  const browser = await chromium.launch();
  const page = await browser.newPage({ viewport: { width: W, height: 2000 } });
  for (const f of files) {
    await page.goto('file://' + path.resolve(f), { waitUntil: 'load' });
    const rows = await page.evaluate(DUMP);
    console.log('#FILE\t' + path.basename(f));
    console.log(rows);
  }
  await browser.close();
})();
