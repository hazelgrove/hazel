#!/usr/bin/env python3
"""Serve a Hazel web build for local iteration, with two extra routes.

The plain `python3 -m http.server` loop has one bad property: visiting a
documentation slide saves a copy under `doc:<group> / <name>` in
IndexedDB, and that copy SHADOWS the shipped source from then on. So
after a rebuild you are still looking at your old copy, with nothing on
screen to say so. That turns "does the fix work?" into a private-window
ritual.

Two routes fix it:

  /fresh?slide=<id>   clear the saved copy, then open that slide.
                      Without ?slide, clears every saved doc slide.
                      This is the link to keep in a tab while iterating.

  /status             what is actually being served: bundle mtime, git
                      HEAD, and whether any source file is newer than
                      the bundle (i.e. you forgot to rebuild).

Usage:
    python3 scripts/dev_serve.py --port 8011
    # then open http://localhost:8011/fresh?slide=livelits-splices-mvp
"""

import argparse
import functools
import html
import http.server
import json
import os
import subprocess
import time
import urllib.parse

PORTS_PAGE = """<!doctype html>
<meta charset="utf-8">
<title>hazel dev ports</title>
<style>
  body { font: 13px/1.5 ui-monospace, SFMono-Regular, Menlo, monospace;
         padding: 1.5rem; color: #222; background: #fbfbf8; }
  h1 { font-size: 15px; margin: 0 0 1rem; font-weight: 600; }
  table { border-collapse: collapse; width: 100%; max-width: 1100px; }
  th, td { text-align: left; padding: .45rem .7rem; border-bottom: 1px solid #e4e4de; vertical-align: top; }
  th { font-weight: 600; color: #666; font-size: 11px; text-transform: uppercase; letter-spacing: .04em; }
  tr:hover td { background: #f3f3ec; }
  a { color: #1a5fb4; text-decoration: none; }
  a:hover { text-decoration: underline; }
  .stale { color: #b00; font-weight: 600; }
  .fresh { color: #070; }
  .muted { color: #999; }
  .branch { font-weight: 600; }
  #note { margin-top: 1rem; color: #777; }
  @media (prefers-color-scheme: dark) {
    body { background: #1c1c19; color: #ddd; }
    th { color: #999; } th, td { border-color: #333; }
    tr:hover td { background: #262622; } a { color: #7cb0f0; }
    .muted { color: #777; } .stale { color: #f77; } .fresh { color: #7c7; }
  }
</style>
<h1>hazel dev ports <span class=muted id=when></span></h1>
<table><thead><tr>
  <th>port</th><th>branch</th><th>worktree</th><th>built</th><th>state</th><th>open</th>
</tr></thead><tbody id=rows></tbody></table>
<p id=note>Scanning localhost 8000-8030 and 8100-8130. Refreshes every 10s.
   <b>open</b> clears saved slide state first, so you always get the shipped source.</p>
<script>
// Two ranges: the 80xx block and an 81xx block. A distinct port is a
// distinct ORIGIN, and IndexedDB is per-origin -- so serving an
// experiment on 8111 keeps its saved slides away from 8011's.
const RANGES = [[8000, 8030], [8100, 8130]];
async function probe(port) {
  const c = new AbortController();
  const t = setTimeout(() => c.abort(), 700);
  try {
    const r = await fetch(`http://localhost:${port}/status.json`,
                          {signal: c.signal, cache: 'no-store'});
    clearTimeout(t);
    if (!r.ok) return null;
    const j = await r.json();
    j.port = port;
    return j;
  } catch (e) { clearTimeout(t); return null; }
}
async function scan() {
  const ports = RANGES.flatMap(([lo, hi]) =>
    Array.from({length: hi - lo + 1}, (_, i) => lo + i));
  const found = (await Promise.all(ports.map(probe))).filter(Boolean);
  found.sort((a, b) => a.port - b.port);
  document.getElementById('rows').innerHTML = found.length ? found.map(p => `
    <tr>
      <td>${p.port}</td>
      <td class=branch>${p.branch || '<span class=muted>?</span>'}</td>
      <td class=muted>${p.worktree || ''}</td>
      <td>${p.bundle || '<span class=muted>no bundle</span>'}</td>
      <td class="${p.stale ? 'stale' : 'fresh'}">${
        p.stale ? p.stale + ' file(s) newer &mdash; REBUILD' : 'up to date'}</td>
      <td><a href="http://localhost:${p.port}/fresh">fresh</a>
          &middot; <a href="http://localhost:${p.port}/">plain</a></td>
    </tr>`).join('') :
    '<tr><td colspan=6 class=muted>no dev servers responding on 8000-8030</td></tr>';
  document.getElementById('when').textContent =
    '(' + new Date().toLocaleTimeString() + ')';
}
scan(); setInterval(scan, 10000);
</script>
"""

FRESH_PAGE = """<!doctype html>
<meta charset="utf-8">
<title>refreshing…</title>
<style>
  body {{ font: 14px/1.5 ui-monospace, monospace; padding: 2rem; color: #333; }}
  @media (prefers-color-scheme: dark) {{ body {{ background:#1c1c19; color:#ddd }} }}
</style>
<p id="msg">clearing local editor state…</p>
<script>
  // Delete the whole database rather than picking keys out of it.
  //
  // The surgical version was actively harmful: `indexedDB.open('hazel')`
  // CREATES the database when it is absent -- an empty v1 with no `kv`
  // object store -- and the app then hangs on "loading" forever against a
  // database it cannot use. Deleting cannot wedge anything, because the
  // app rebuilds the schema on boot. The cost is that other local state
  // (mode, settings, scratch) goes too, which for a "show me the shipped
  // source" route is the intent anyway.
  const target = {target};
  function go() {{ location.replace(target); }}
  let done = false;
  const bail = setTimeout(() => {{ if (!done) go(); }}, 3000);
  try {{
    const req = indexedDB.deleteDatabase('hazel');
    req.onsuccess = req.onerror = req.onblocked = () => {{
      done = true; clearTimeout(bail);
      document.getElementById('msg').textContent = 'cleared; opening…';
      setTimeout(go, 150);
    }};
  }} catch (e) {{ done = true; clearTimeout(bail); go(); }}
</script>
"""



def build_info(root):
    """What is being served, and is it stale?"""
    bundle = os.path.join(root, "hazel.js")
    info = {"bundle": None, "head": None, "stale_sources": [],
            "branch": None, "worktree": None}
    if os.path.exists(bundle):
        mt = os.path.getmtime(bundle)
        info["bundle"] = time.strftime("%Y-%m-%d %H:%M:%S", time.localtime(mt))
        # walk up to the repo root from _build/default/src/web/www
        repo = os.path.abspath(os.path.join(root, *([os.pardir] * 5)))
        src = os.path.join(repo, "src")
        if os.path.isdir(src):
            newer = []
            for dirpath, _, files in os.walk(src):
                for f in files:
                    if not f.endswith((".re", ".rei", ".ml", ".hz")):
                        continue
                    fp = os.path.join(dirpath, f)
                    try:
                        if os.path.getmtime(fp) > mt:
                            newer.append(os.path.relpath(fp, repo))
                    except OSError:
                        pass
            info["stale_sources"] = sorted(newer)[:20]
        info["worktree"] = os.path.basename(repo)
        for key, args in (("head", ["log", "--oneline", "-1"]),
                          ("branch", ["rev-parse", "--abbrev-ref", "HEAD"])):
            try:
                info[key] = subprocess.run(
                    ["git", "-C", repo] + args,
                    capture_output=True, text=True, timeout=5).stdout.strip()
            except Exception:
                pass
    return info


class Handler(http.server.SimpleHTTPRequestHandler):
    def _send(self, body, ctype="text/html; charset=utf-8", code=200):
        raw = body.encode()
        self.send_response(code)
        self.send_header("Content-Type", ctype)
        self.send_header("Content-Length", str(len(raw)))
        self.end_headers()      # adds Cache-Control below
        self.wfile.write(raw)

    def end_headers(self):
        """No caching, ever. A stale bundle that looks current is the whole
        problem this server exists to avoid."""
        self.send_header("Cache-Control", "no-store, must-revalidate")
        super().end_headers()

    def do_GET(self):
        parsed = urllib.parse.urlparse(self.path)
        if parsed.path.rstrip("/") == "/fresh":
            q = urllib.parse.parse_qs(parsed.query)
            slide = (q.get("slide") or [None])[0]
            target = "/" + ("?" + urllib.parse.urlencode(
                {k: v[0] for k, v in q.items()}) if q else "")
            _ = slide   # the redirect carries it; we clear everything
            self._send(FRESH_PAGE.format(target=json.dumps(target)))
            return
        if parsed.path.rstrip("/") == "/status.json":
            info = build_info(os.getcwd())
            info["port"] = self.server.server_address[1]
            info["stale"] = len(info["stale_sources"])
            raw = json.dumps(info)
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(raw.encode())))
            # Dev-only, localhost-only: lets the /ports dashboard on one
            # port read every other port's identity.
            self.send_header("Access-Control-Allow-Origin", "*")
            self.end_headers()
            self.wfile.write(raw.encode())
            return
        if parsed.path.rstrip("/") == "/ports":
            self._send(PORTS_PAGE)
            return
        if parsed.path.rstrip("/") == "/status":
            info = build_info(os.getcwd())
            stale = info["stale_sources"]
            body = (
                "<!doctype html><meta charset=utf-8>"
                "<style>body{font:14px/1.6 ui-monospace,monospace;padding:2rem}"
                ".bad{color:#b00}.ok{color:#070}</style>"
                f"<p>bundle built: <b>{html.escape(str(info['bundle']))}</b></p>"
                f"<p>git HEAD: {html.escape(str(info['head']))}</p>"
                + (f"<p class=bad>{len(stale)} source file(s) newer than the "
                   "bundle &mdash; you need to rebuild:</p><ul>"
                   + "".join(f"<li>{html.escape(s)}</li>" for s in stale)
                   + "</ul>"
                   if stale else "<p class=ok>bundle is up to date with src/</p>")
                + '<p><a href="/fresh">open a fresh deck</a></p>')
            self._send(body)
            return
        super().do_GET()

    def log_message(self, fmt, *args):
        pass   # quiet


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=8011)
    ap.add_argument("--dir", default=None,
                    help="www directory (default: _build/default/src/web/www "
                         "relative to the repo root)")
    args = ap.parse_args()

    root = args.dir
    if root is None:
        repo = subprocess.run(["git", "rev-parse", "--show-toplevel"],
                              capture_output=True, text=True).stdout.strip()
        root = os.path.join(repo, "_build/default/src/web/www")
    if not os.path.isfile(os.path.join(root, "index.html")):
        raise SystemExit(f"no index.html under {root} -- build first")
    os.chdir(root)

    info = build_info(root)
    print(f"serving {root}")
    print(f"  bundle: {info['bundle']}")
    print(f"  HEAD:   {info['head']}")
    if info["stale_sources"]:
        print(f"  WARNING: {len(info['stale_sources'])} source files are newer "
              "than the bundle; rebuild before trusting this")
    print(f"\n  http://localhost:{args.port}/fresh    <- always-current deck")
    print(f"  http://localhost:{args.port}/ports    <- every dev port, by branch")
    print(f"  http://localhost:{args.port}/status   <- what is being served\n")

    http.server.ThreadingHTTPServer(
        ("127.0.0.1", args.port),
        functools.partial(Handler, directory=root)).serve_forever()


if __name__ == "__main__":
    main()
