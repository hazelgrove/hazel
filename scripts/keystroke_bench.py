#!/usr/bin/env python3
"""Per-keystroke cost, read from Hazel's own Frame Timing panel.

The thing a merge of this size can silently cost, and the thing a green
test suite says nothing about -- it is what #2600 found after an agent
merged dev into modular-editors. Run it against two ports that differ
only by the merge and compare.

The Frame Timing panel keeps a per-frame history, so N keystrokes leave N
rows holding perform / statics / syntax / total. This types, then reads
the whole table at once.

    python3 scripts/keystroke_bench.py --port 8311 --keys 20
"""
import argparse, json, re, statistics, sys, time, os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from deck_regress import Driver

SECTIONS = ["Statics", "Frame Timing", "Editor & Memory"]


def ms(cell):
    """'182ms' / '1.2s' / '0ns' / '—' -> float ms, or None."""
    cell = (cell or "").strip()
    m = re.match(r"^([\d.]+)\s*(ms|s|ns|us|µs)$", cell)
    if not m:
        return None
    v, u = float(m.group(1)), m.group(2)
    return {"ms": v, "s": v * 1000, "ns": v / 1e6, "us": v / 1000, "µs": v / 1000}[u]


def open_panels(d):
    d.js("""
      const nk = document.getElementById('ninja-keys');
      const it = ((nk && nk.data) || []).find(a =>
        /toggle debug sidebar/i.test((a.title||'') + ' ' + (a.id||'')));
      if (it && typeof it.handler === 'function') it.handler();
      return 1;
    """)
    time.sleep(2.5)

    def click(box):
        d.pointer([{"type": "pointerMove", "x": box["x"], "y": box["y"]},
                   {"type": "pointerDown", "button": 0},
                   {"type": "pointerUp", "button": 0}])

    box = d.js("""
      const e = document.querySelector('.debug-info-button');
      if (!e) return null;
      const r = e.getBoundingClientRect();
      return {x: Math.round(r.x + r.width/2), y: Math.round(r.y + r.height/2)};
    """)
    if not box:
        return False
    click(box); time.sleep(3)

    for _ in range(2):          # expanding one shifts the others
        for want in SECTIONS:
            b = d.js("""
              const t = [...document.querySelectorAll('.debug-section-title')]
                .find(e => (e.innerText||'').indexOf(%s) >= 0);
              if (!t) return null;
              if ((t.innerText||'').trim()[0] !== '\\u25b8') return null;
              t.scrollIntoView({block: 'center'});
              const r = t.getBoundingClientRect();
              if (!(r.top >= 0 && r.bottom <= window.innerHeight)) return null;
              return {x: Math.round(r.x + r.width/2),
                      y: Math.round(r.y + r.height/2)};
            """ % json.dumps(want))
            if b:
                time.sleep(0.3); click(b); time.sleep(0.9)
    return True


FRAME_TABLE = """
  for (const t of document.querySelectorAll('.perf-table')) {
    const head = [...t.querySelectorAll('.perf-head')]
      .map(h => (h.innerText||'').trim());
    if (head.indexOf('perform') < 0) continue;
    const rows = [...t.querySelectorAll('tr')]
      .map(r => [...r.querySelectorAll('td')].map(c => (c.innerText||'').trim()))
      .filter(r => r.length === head.length);
    return {head: head, rows: rows};
  }
  return null;
"""


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, required=True)
    ap.add_argument("--wd-port", type=int, default=4800)
    ap.add_argument("--slide", default="livelits-tree-care")
    ap.add_argument("--keys", type=int, default=20)
    # A slide whose view crosses into Fumola is not up for ~54s: the wasm
    # has to load and the Fumola program has to run before a first frame
    # exists. 14s is enough for a plain Hazel slide and nothing else.
    ap.add_argument("--settle", type=int, default=14)
    args = ap.parse_args()

    d = Driver(args.port, args.wd_port)
    try:
        d.goto(f"/fresh?slide={args.slide}&panel=none")
        time.sleep(args.settle)
        if not open_panels(d):
            print("  could not open the telemetry panels"); return 1

        # caret into the program: click the last code row, then End.
        box = d.js("""
          /* .row does not exist in this markup; .token does. Click just
             inside the right edge of a token so the caret lands after it. */
          const ts = [...document.querySelectorAll('.code-container .token')]
            .filter(e => (e.innerText||'').trim().length);
          const t = ts[Math.floor(ts.length * 0.6)];
          if (!t) return null;
          t.scrollIntoView({block: 'center'});
          const b = t.getBoundingClientRect();
          if (b.width === 0) return null;
          return {x: Math.round(b.right - 1), y: Math.round(b.y + b.height/2)};
        """)
        if not box:
            print("  no code row to click"); return 1
        d.pointer([{"type": "pointerMove", "x": box["x"], "y": box["y"]},
                   {"type": "pointerDown", "button": 0},
                   {"type": "pointerUp", "button": 0}])
        time.sleep(2)
        d.keys([""])      # End
        time.sleep(1)

        # NEVER type blind. A first version reported 30 frames from 20
        # keystrokes with a perform timing on only 3: the click had not
        # placed a caret, so almost nothing was an edit and the numbers
        # were load-time frames wearing a keystroke's name.
        before = d.js("return document.querySelectorAll('.code-container .token').length;")
        # the caret is an ELEMENT ID, not a class (CaretDec ~id="caret")
        if not d.js("return !!document.getElementById('caret');"):
            print('  no caret in the editor after clicking; refusing to type')
            return 1

        for _ in range(args.keys):
            d.keys(["1"])
            time.sleep(0.9)
        time.sleep(3)

        after = d.js("return document.querySelectorAll('.code-container .token').length;")
        print(f'  caret ok; tokens {before} -> {after}')
        if after == before:
            print('  WARNING: program unchanged -- these are not keystrokes')

        t = d.js(FRAME_TABLE)
        if not t:
            print("  Frame Timing table not found"); return 1
        head = t["head"]
        idx = {k: head.index(k) for k in head}
        cols = [c for c in ("perform", "statics", "syntax", "total") if c in idx]
        series = {c: [] for c in cols}
        edits = 0
        for row in t["rows"][1:]:
            if ms(row[idx["perform"]]) is None:
                continue   # not an edit: a load-time frame
            edits += 1
            for c in cols:
                v = ms(row[idx[c]])
                if v is not None:
                    series[c].append(v)

        print(f"\n  port {args.port} — {len(t['rows'])-1} frames recorded, "
              f"{args.keys} keystrokes sent\n")
        print(f"    {'phase':<10}{'n':>4}{'median':>10}{'p90':>10}{'max':>10}")
        for c in cols:
            v = sorted(series[c])
            if not v:
                print(f"    {c:<10}{0:>4}{'—':>10}{'—':>10}{'—':>10}")
                continue
            p90 = v[min(len(v) - 1, int(len(v) * 0.9))]
            print(f"    {c:<10}{len(v):>4}{statistics.median(v):>9.1f}ms"
                  f"{p90:>9.1f}ms{max(v):>9.1f}ms")
    finally:
        d.quit()
    return 0


if __name__ == "__main__":
    sys.exit(main())
