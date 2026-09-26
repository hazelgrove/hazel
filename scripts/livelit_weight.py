#!/usr/bin/env python3
"""How heavy is each livelit's rendered view?

The claim to test: livelits that redraw interactively are slow because their
view output carries a large HTML payload (tree care, and the perf livelit).
That is a claim about SIZE, and size is measurable without any of the
editor's own instrumentation -- which in any case times the editor's frame
stages, not the building of one widget's view.

Reports, per deck slide: how many livelits, and for each the number of DOM
nodes and the innerHTML length of its rendered view.

    python3 scripts/livelit_weight.py --port 8311
"""
import argparse, json, sys, time, os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from deck_regress import Driver, DECK   # reuse the WebDriver plumbing

MEASURE = """
  const ws = [...document.querySelectorAll('.user-livelit')];
  return {
    count: ws.length,
    widgets: ws.map(w => ({
      nodes: w.querySelectorAll('*').length,
      html: w.innerHTML.length,
      text: (w.innerText || '').trim().slice(0, 28),
    })),
    pageNodes: document.querySelectorAll('*').length,
  };
"""


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=8311)
    ap.add_argument("--wd-port", type=int, default=4750)
    ap.add_argument("--settle", type=float, default=11)
    args = ap.parse_args()

    d = Driver(args.port, args.wd_port)
    rows = []
    try:
        for name in DECK:
            slug = name.lower().replace(",", "").replace("(", "").replace(")", "")
            slug = "livelits-" + slug.replace(" ", "-")
            d.goto(f"/fresh?slide={slug}&panel=none")
            time.sleep(args.settle)
            st = d.js(MEASURE)
            tot_nodes = sum(w["nodes"] for w in st["widgets"])
            tot_html = sum(w["html"] for w in st["widgets"])
            rows.append((name, st["count"], tot_nodes, tot_html,
                         st["pageNodes"], st["widgets"]))
            print(f"  {name:24} {st['count']:>2} livelits  "
                  f"{tot_nodes:>6} nodes  {tot_html:>8} B html  "
                  f"(page {st['pageNodes']} nodes)")
    finally:
        d.quit()

    print("\n  heaviest single widget per slide:")
    for name, _, _, _, _, widgets in sorted(
            rows, key=lambda r: -max((w["nodes"] for w in r[5]), default=0)):
        if not widgets:
            continue
        w = max(widgets, key=lambda w: w["nodes"])
        print(f"    {name:24} {w['nodes']:>6} nodes  {w['html']:>8} B  "
              f"{w['text']!r}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
