(handler-bind ((warning #'muffle-warning)) (asdf:load-system "weft/render"))
(weft.render:render-to-png
 "<body>
<div class=nav><div class=brand>weft</div><div class=sp></div><div class=nl>features</div><div class=nl>docs</div><div class=nl>source</div></div>
<div class=hero><h1>A web engine in Common Lisp</h1><p>Fetch, parse, cascade, lay out, and paint — no browser underneath.</p></div>
<div class=wrap>
<div class=tag>NEW</div>
<h2>What renders</h2>
<p>This centered column wraps its text around the floated tag on the right, then continues in normal flow. Inline runs keep their own <b>bold</b> weight and <a href=#>link</a> styling as lines wrap.</p>
<h2>Layout modes</h2>
<table><tr><th>Mode</th><th>State</th></tr>
<tr><td>Block + inline</td><td>working</td></tr>
<tr><td>Floats + clear</td><td>working</td></tr>
<tr><td>Flexbox</td><td>working</td></tr>
<tr><td>Tables</td><td>working</td></tr>
<tr><td>Positioning</td><td>working</td></tr></table>
<ul><li>width / max-width / margin auto</li><li>linear-gradient backgrounds</li><li>specificity-ordered cascade</li></ul>
</div></body>"
 "body{background:#eef1f7;font-size:14px;color:#1b2330;margin:0}
  .nav{display:flex;align-items:center;background:#0f1d3a;color:#fff;padding:12px 20px;gap:14px}
  .brand{font-weight:bold;color:#fff} .sp{flex-grow:1} .nl{color:#aac}
  .hero{background:linear-gradient(180deg,#2b5876,#4e4376);color:#fff;padding:34px 20px;text-align:center}
  .hero h1{color:#fff} .hero p{color:#dde}
  .wrap{max-width:640px;margin:0 auto;background:#fff;padding:26px}
  h2{color:#0f1d3a} a{color:#1457d6}
  .tag{float:right;background:linear-gradient(to right,#11998e,#38ef7d);color:#fff;width:74px;padding:12px;text-align:center;font-weight:bold;margin-left:14px}
  table{width:100%;border:1px #9aa;margin:8px 0} td{border:1px #ccd;padding:6px} th{border:1px #9aa;background:#dde;padding:6px}"
 900 "/tmp/claude-1001/-home-claude/52922185-82fd-4e4b-8d4f-166ff2de6022/scratchpad/out/final.png")
(format t "~%FINAL-DONE~%")
