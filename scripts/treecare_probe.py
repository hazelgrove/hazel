#!/usr/bin/env python3
"""What does interacting with the tree care livelit actually cost?

At rest its view is 18 DOM nodes, so "a huge HTML payload" is not true of
the resting widget. But its model is a TREE, and the complaint is about
redraw while interacting -- so drive it and watch both the size and the
time grow (or not).

Reports, per click: wall time for the click to settle, the widget's node
count and innerHTML length, and the whole page's node count.
"""
import argparse, json, sys, time, os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from deck_regress import Driver

SHOT = """
  const w = document.querySelector('.user-livelit');
  if (!w) return null;
  return {nodes: w.querySelectorAll('*').length,
          html: w.innerHTML.length,
          page: document.querySelectorAll('*').length,
          text: (w.innerText||'').replace(/\\n/g,' ').slice(0, 60)};
"""

BUTTONS = """
  const w = document.querySelector('.user-livelit');
  if (!w) return [];
  return [...w.querySelectorAll('button, [role=button], svg, input')]
    .map((e, i) => ({i, tag: e.tagName,
                     text: (e.innerText||e.getAttribute('aria-label')||'').trim().slice(0,16),
                     x: Math.round(e.getBoundingClientRect().x + e.getBoundingClientRect().width/2),
                     y: Math.round(e.getBoundingClientRect().y + e.getBoundingClientRect().height/2)}));
"""


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=8311)
    ap.add_argument("--wd-port", type=int, default=4752)
    ap.add_argument("--clicks", type=int, default=8)
    ap.add_argument("--label", default="+", help="button text to click")
    args = ap.parse_args()

    d = Driver(args.port, args.wd_port)
    try:
        d.goto("/fresh?slide=livelits-tree-care&panel=none")
        time.sleep(13)
        d.js("const w=document.querySelector('.user-livelit');"
             "if(w) w.scrollIntoView({block:'center'}); return 1;")
        time.sleep(1.5)

        btns = json.loads(d.js("return JSON.stringify((() => {" + BUTTONS + "})());"))
        print("  controls found in the widget:")
        for b in btns:
            print(f"    [{b['i']}] {b['tag']:6} {b['text']!r:18} at ({b['x']},{b['y']})")

        target = next((b for b in btns if b["text"] == args.label), None)
        if target is None:
            print(f"\n  no control labelled {args.label!r}; nothing to drive")
            print("  baseline:", d.js(SHOT))
            return 1

        base = d.js(SHOT)
        print(f"\n  baseline: {base['nodes']} nodes, {base['html']} B, "
              f"page {base['page']}")
        print(f"\n  clicking {args.label!r} x{args.clicks}:")
        print(f"    {'#':>2}  {'settle ms':>9}  {'nodes':>6}  {'html B':>8}  {'page':>6}")
        for k in range(args.clicks):
            t0 = time.time()
            d.js(f"""
              const w = document.querySelector('.user-livelit');
              const b = [...w.querySelectorAll('button, [role=button], svg, input')][{target['i']}];
              if (b) b.dispatchEvent(new MouseEvent('click', {{bubbles: true}}));
              return 1;
            """)
            time.sleep(2.5)
            st = d.js(SHOT)
            dt = (time.time() - t0) * 1000
            print(f"    {k+1:>2}  {dt:>9.0f}  {st['nodes']:>6}  {st['html']:>8}  {st['page']:>6}")
    finally:
        d.quit()
    return 0


if __name__ == "__main__":
    sys.exit(main())
