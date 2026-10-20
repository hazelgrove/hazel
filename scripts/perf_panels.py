#!/usr/bin/env python3
"""Hazel's own per-frame telemetry, driven over tree care's `water + feed`.

PerfMetrics (src/web/util/PerfMetrics.re) records perform / statics /
syntax / cursor_info / color_map / total for each frame, and EvalMetrics
records evaluation. Both are GATED: `nothing is measured while every
per-frame panel is collapsed`. So this opens the Debug Sidebar, expands the
sections, and only then drives the widget -- otherwise every number is
absent and the run looks clean for the wrong reason.

    python3 scripts/perf_panels.py --port 8311 --clicks 8
"""
import argparse, json, re, sys, time, os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from deck_regress import Driver

SECTIONS = ["Statics", "Frame Timing", "Evaluation", "Editor & Memory"]

READ_FIELDS = """
  /* The perf sections render a TABLE (PerfFormat), not .debug-field rows:
     .perf-table inside each expanded section, with .perf-head headers. */
  const out = {};
  document.querySelectorAll('.perf-table').forEach((t, i) => {
    const head = [...t.querySelectorAll('.perf-head')]
      .map(h => (h.innerText||'').trim());
    const rows = [...t.querySelectorAll('tr')]
      .map(r => [...r.querySelectorAll('td')]
                  .map(c => (c.innerText||'').trim()))
      .filter(r => r.length);
    if (rows.length) out['table' + i] = {head: head, rows: rows.slice(0, 3)};
  });
  const empty = [...document.querySelectorAll('.perf-empty')]
    .map(e => (e.innerText||'').trim());
  if (empty.length) out._empty = empty;
  return out;
"""

WIDGET = """
  const w = document.querySelector('.user-livelit');
  if (!w) return null;
  return {nodes: w.querySelectorAll('*').length, html: w.innerHTML.length};
"""


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=8311)
    ap.add_argument("--wd-port", type=int, default=4764)
    ap.add_argument("--clicks", type=int, default=8)
    ap.add_argument("--settle", type=float, default=3.0)
    args = ap.parse_args()

    d = Driver(args.port, args.wd_port)
    try:
        d.goto("/fresh?slide=livelits-tree-care&panel=none")
        time.sleep(13)

        # 1. show_debug_panel, via the nut menu. Clicking the text node's
        #    wrapper does nothing -- the handler sits on a specific ancestor,
        #    so find the one that actually carries an onclick and click THAT,
        #    with a real pointer event at its coordinates.
        # The command palette (ninja-keys) holds every ShortcutAction with
        # its handler attached, so the toggle can be invoked directly. The
        # nut menu is a CSS hover menu that stays 0x0 under WebDriver, and
        # Hazel ignores synthetic clicks in places -- this sidesteps both.
        print("palette actions matching 'debug':", d.js("""
          const nk = document.getElementById('ninja-keys');
          if (!nk) return 'no ninja-keys element';
          const data = nk.data || [];
          return JSON.stringify(data
            .filter(a => /debug/i.test((a.title||'') + ' ' + (a.id||'')))
            .map(a => a.title || a.id));
        """))
        print("invoke toggle:", d.js("""
          const nk = document.getElementById('ninja-keys');
          const data = (nk && nk.data) || [];
          const it = data.find(a =>
            /toggle debug sidebar/i.test((a.title||'') + ' ' + (a.id||'')));
          if (!it) return 'action not found';
          if (typeof it.handler !== 'function') return 'no handler';
          it.handler();
          return 'invoked';
        """))
        time.sleep(2.5)

        # Real pointer clicks from here on. Hazel ignores synthetic ones in
        # places -- the same lesson as the projector drag bug -- and a
        # dispatched click on the debug tab leaves the panel unswitched.
        def click_sel(sel, what):
            box = d.js("""
              const e = document.querySelector(%s);
              if (!e) return null;
              const r = e.getBoundingClientRect();
              if (r.width === 0 || r.height === 0) return null;
              return {x: Math.round(r.x + r.width/2),
                      y: Math.round(r.y + r.height/2)};
            """ % json.dumps(sel))
            if not box:
                print(f"  {what}: not clickable ({sel})")
                return False
            d.pointer([{"type": "pointerMove", "x": box["x"], "y": box["y"]},
                       {"type": "pointerDown", "button": 0},
                       {"type": "pointerUp", "button": 0}])
            print(f"  {what}: clicked at {box['x']},{box['y']}")
            return True

        click_sel(".debug-info-button", "debug tab")
        time.sleep(3)

        print("panel now shows:", d.js("""
          return JSON.stringify(
            [...document.querySelectorAll('.debug-section-title')]
              .map(e => (e.innerText||'').trim()));
        """))

        # expand each gating section, by real click on its title
        # Scroll each title into view BEFORE clicking. The sections live in
        # a scrollable panel, so a title below the fold has a
        # getBoundingClientRect y outside the viewport, and a pointer click
        # there lands on whatever is actually at those coordinates -- which
        # is why "clicked" kept reporting success while nothing expanded.
        for want in SECTIONS:
            box = d.js("""
              const t = [...document.querySelectorAll('.debug-section-title')]
                .find(e => (e.innerText||'').indexOf(%s) >= 0);
              if (!t) return null;
              t.scrollIntoView({block: 'center'});
              const r = t.getBoundingClientRect();
              const inView = r.top >= 0 && r.bottom <= window.innerHeight &&
                             r.width > 0 && r.height > 0;
              return {x: Math.round(r.x + r.width/2),
                      y: Math.round(r.y + r.height/2),
                      inView: inView,
                      chev: (t.innerText||'').trim()[0]};
            """ % json.dumps(want))
            if not box:
                print(f"  {want!r}: no title")
                continue
            if not box["inView"]:
                print(f"  {want!r}: still off-screen at y={box['y']}, skipping")
                continue
            time.sleep(0.4)
            d.pointer([{"type": "pointerMove", "x": box["x"], "y": box["y"]},
                       {"type": "pointerDown", "button": 0},
                       {"type": "pointerUp", "button": 0}])
            time.sleep(1.2)
            now = d.js("""
              const t = [...document.querySelectorAll('.debug-section-title')]
                .find(e => (e.innerText||'').indexOf(%s) >= 0);
              return JSON.stringify({chev: t ? (t.innerText||'').trim()[0] : '?',
                docFields: document.querySelectorAll('.debug-field').length});
            """ % json.dumps(want))
            print(f"  {want!r}: {box['chev']} -> {now}")
        # A second pass: expanding one section shifts the others, so a title
        # measured before the shift can be clicked in the wrong place.
        for want in SECTIONS:
            box = d.js("""
              const t = [...document.querySelectorAll('.debug-section-title')]
                .find(e => (e.innerText||'').indexOf(%s) >= 0);
              if (!t) return null;
              if ((t.innerText||'').trim()[0] !== '\u25b8') return null;
              t.scrollIntoView({block: 'center'});
              const r = t.getBoundingClientRect();
              if (!(r.top >= 0 && r.bottom <= window.innerHeight)) return null;
              return {x: Math.round(r.x + r.width/2),
                      y: Math.round(r.y + r.height/2)};
            """ % json.dumps(want))
            if box:
                time.sleep(0.4)
                d.pointer([{"type": "pointerMove", "x": box["x"], "y": box["y"]},
                           {"type": "pointerDown", "button": 0},
                           {"type": "pointerUp", "button": 0}])
                time.sleep(1.0)
                print(f"  retry {want!r} -> " + d.js("""
                  const t = [...document.querySelectorAll('.debug-section-title')]
                    .find(e => (e.innerText||'').indexOf(%s) >= 0);
                  return t ? (t.innerText||'').trim()[0] : '?';
                """ % json.dumps(want)))
        time.sleep(2)

        d.js("const w=document.querySelector('.user-livelit');"
             "if(w) w.scrollIntoView({block:'center'}); return 1;")
        time.sleep(1.5)

        print("\n=== raw panel text ===")
        print(d.js("""
          const p = document.querySelector('[class*=sidebar], aside, .panel');
          const txt = [...document.querySelectorAll('.perf-table tr')]
            .map(e => (e.innerText||'').replace(/\\s+/g,' ').trim())
            .slice(0, 30);
          return JSON.stringify({tables:
            document.querySelectorAll('.perf-table').length, sample: txt.slice(0,6)});
        """))
        fields = d.js(READ_FIELDS)
        if not fields:
            print("\n  no fields yet -- driving anyway, PerfMetrics only has "
                  "rows once frames have run")
        print(f"\n  {len(fields)} telemetry fields visible; keys:")
        print("   ", ", ".join(sorted(fields)[:24]))

        print(f"\n  driving 'water + feed' x{args.clicks}\n")
        rows = []
        for k in range(args.clicks + 1):
            if k:
                d.js("""
                  const w = document.querySelector('.user-livelit');
                  const b = [...w.querySelectorAll('button')].find(
                    e => (e.innerText||'').trim() === 'water + feed');
                  if (b) b.dispatchEvent(new MouseEvent('click',{bubbles:true}));
                  return 1;
                """)
                time.sleep(args.settle)
            wid = d.js(WIDGET)
            f = d.js(READ_FIELDS)
            rows.append((k, wid, f))
            print(f"  grow {k}: {wid['nodes']:>5} nodes {wid['html']:>7} B")
            for key in sorted(k2 for k2 in f if k2.startswith("table")):
                t = f[key]
                head = " | ".join(t.get("head", []))
                print(f"        [{head}]")
                for r in t.get("rows", [])[:2]:
                    print(f"          {' | '.join(r)}")
        with open("/tmp/claude-1000/perf_rows.json", "w") as fh:
            json.dump([{"grow": k, "widget": w, "fields": f}
                       for k, w, f in rows], fh, indent=1)
        print("\n  raw rows -> /tmp/claude-1000/perf_rows.json")
    finally:
        d.quit()
    return 0


if __name__ == "__main__":
    sys.exit(main())
