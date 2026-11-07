#!/usr/bin/env python3
"""hazel-jit in the app, in a real Firefox (^ticket(716) and ^ticket(717): ^report(45)'s next steps 4 and 5).

For each polygon slide of the Livelit Playground (Polygons, Polygon Viewer, Full Body, the Witch, Hands, Scenes), loaded
fresh twice, with Compiled views on (?jit=on) and off (?jit=off):

- every livelit's drawing, as data (each element's tag, its attributes, its text; the ids, fresh on every load, made
  alike), the livelit errors on the slide, and the page's hazel-jit counts (globalThis.hazelJit: what was drawn from
  its JS, what was compiled, what ran on the interpreter, and why);
- then the same events both ways, through the projector's own event path (real pointer clicks on the first livelit's
  buttons, and on Polygons a drag on its drawing), the drawings after them, and the time from each event to the
  first change of the page under it (a MutationObserver on the livelit's projector, from the event's capture).

The drawings must be the same both ways, before the events and after them, and a livelit that errors one way must
error the other. Writes the readings as JSON (--out) and a screenshot each way (--shots). Exits non-zero on a
difference.

    dune build src --profile release
    python3 scripts/dev_serve.py --port 8111 &
    python3 scripts/jit_slides.py --port 8111 --out jit.json --shots jit-shots

Needs geckodriver and firefox on PATH (GitHub's Ubuntu runners have both), as scripts/deck_regress.py does, whose
driver this uses.
"""

import argparse
import json
import os
import re
import sys
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import deck_regress as D  # noqa: E402

SLIDES = ["Polygons", "Polygon Viewer", "Full Body", "Witch", "Hands", "Scenes"]

SNAPSHOT = r"""
  const uuid = /[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}/g;
  const ser = n => {
    if (n.nodeType === 3) return JSON.stringify(n.textContent.replace(uuid, '<id>'));
    if (n.nodeType !== 1) return '';
    // (an element's id is made fresh on every load: left out)
    const attrs = [...n.attributes].filter(a => a.name !== 'id').map(a => a.name + '=' + a.value.replace(uuid, '<id>')).sort().join(' ');
    return '<' + n.tagName.toLowerCase() + (attrs ? ' ' + attrs : '') + '>' + [...n.childNodes].map(ser).join('') + '</>';
  };
  const ws = [...document.querySelectorAll('.user-livelit')];
  return {
    widgets: ws.map(ser),
    errors: [...document.querySelectorAll('.livelit-user-error')].map(e => e.textContent.slice(0, 160)),
    jit: globalThis.hazelJit ? JSON.parse(JSON.stringify(globalThis.hazelJit)) : null,
  };
"""

# (from each event's capture to the first change under the livelit's projector: the update, the view, the drawing)
WATCH = r"""
  window.__lat = window.__lat || [];
  if (!window.__watching) {
    window.__watching = true;
    for (const kind of ['click', 'mousemove']) document.addEventListener(kind, e => {
      if (kind === 'mousemove' && !(e.buttons & 1)) return;
      const w = e.target.closest && e.target.closest('.user-livelit');
      if (!w) return;
      const root = w.closest('.projector') || w, t0 = performance.now();
      const mo = new MutationObserver(() => { window.__lat.push({ kind, ms: performance.now() - t0 }); mo.disconnect(); });
      mo.observe(root, { subtree: true, childList: true, attributes: true, characterData: true });
      setTimeout(() => mo.disconnect(), 5000);
    }, true);
  }
  window.__lat.length = 0;
  return 1;
"""

BUTTONS = r"""
  const w = [...document.querySelectorAll('.user-livelit')].find(w => w.querySelector('button'));
  if (!w) return [];
  w.scrollIntoView({ block: 'center' });
  return [...w.querySelectorAll('button')].filter(b => !b.disabled && b.offsetWidth)
    .map(b => { const r = b.getBoundingClientRect(); return [Math.round(r.left + r.width / 2), Math.round(r.top + r.height / 2), b.textContent.trim().slice(0, 30)]; });
"""

CANVAS = r"""
  const w = [...document.querySelectorAll('.user-livelit')].find(w => w.querySelector('svg'));
  if (!w) return null;
  const s = [...w.querySelectorAll('svg')].sort((a, b) => b.getBoundingClientRect().width - a.getBoundingClientRect().width)[0];
  s.scrollIntoView({ block: 'center' });
  const r = s.getBoundingClientRect();
  return [Math.round(r.left), Math.round(r.top), Math.round(r.width), Math.round(r.height)];
"""


def slide_ids():
    by_name = {}
    for name, sid in D.deck_slides():
        by_name[name.split(" / ")[-1]] = sid
    return [(n, by_name[n]) for n in SLIDES if n in by_name]


def load(d, sid, mode):
    t0 = time.time()
    d.goto(f"/fresh?slide={sid}&panel=none&jit={mode}")
    time.sleep(3)
    D.wait_for(d, "return document.querySelectorAll('.user-livelit').length > 0", 120)
    D.settled(d)
    return round(time.time() - t0, 1)


def events(d, slide):
    """The same events every time: the first livelit's buttons, each clicked in turn (twice round), and on Polygons a
    drag across its drawing. Their latencies, as WATCH measured them."""
    d.js(WATCH)
    clicked = []
    for i in range(6):
        bs = d.js(BUTTONS) or []
        if not bs:
            break
        x, y, label = bs[i % len(bs)]
        D.click_at(d, x, y)
        clicked.append(label)
        # (each click's own run landed before the next, both ways: the interpreter's view, slower, showed a button
        # half drawn and the next click found another one, in the first run of this check)
        time.sleep(0.5)
        D.settled(d)
    if slide == "Polygons":
        box = d.js(CANVAS)
        if box:
            x, y, w, h = box
            px, py = x + w // 3, y + h // 3
            acts = [{"type": "pointerMove", "x": px, "y": py}, {"type": "pointerDown", "button": 0}]
            for k in range(1, 9):
                acts += [{"type": "pause", "duration": 60}, {"type": "pointerMove", "x": px + 6 * k, "y": py + 4 * k}]
            acts += [{"type": "pointerUp", "button": 0}]
            d.pointer(acts)
            time.sleep(0.5)
            D.settled(d)
    D.settled(d)
    return clicked, d.js("return window.__lat || []")


def first_difference(a, b):
    i = 0
    while i < min(len(a), len(b)) and a[i] == b[i]:
        i += 1
    return f"at {i}: {a[max(0, i - 120):i + 120]!r} ||| {b[max(0, i - 120):i + 120]!r}"


def compare(label, on, off, problems):
    if len(on["widgets"]) != len(off["widgets"]):
        problems.append(f"{label}: {len(on['widgets'])} livelits compiled, {len(off['widgets'])} on the interpreter")
        return
    for k, (a, b) in enumerate(zip(on["widgets"], off["widgets"])):
        if a != b:
            problems.append(f"{label}: livelit {k} drawn differently: " + first_difference(a, b))
    if bool(on["errors"]) != bool(off["errors"]):
        problems.append(f"{label}: errors compiled {on['errors']}, on the interpreter {off['errors']}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=8111)
    ap.add_argument("--wd-port", type=int, default=4598)
    ap.add_argument("--only", help="substring filter on slide names")
    ap.add_argument("--out", default="jit.json")
    ap.add_argument("--shots")
    args = ap.parse_args()
    if args.shots:
        os.makedirs(args.shots, exist_ok=True)
    d = D.Driver(args.port, args.wd_port)
    report, problems = {"slides": []}, []
    try:
        for name, sid in slide_ids():
            if args.only and args.only.lower() not in name.lower():
                continue
            entry = {"slide": name, "id": sid}
            for mode in ("on", "off"):
                secs = load(d, sid, mode)
                before = d.js(SNAPSHOT)
                if args.shots:
                    d.shot(os.path.join(args.shots, re.sub(r"[^A-Za-z0-9]+", "_", name) + f"_{mode}.png"))
                clicked, lat = events(d, name)
                after = d.js(SNAPSHOT)
                if args.shots:
                    d.shot(os.path.join(args.shots, re.sub(r"[^A-Za-z0-9]+", "_", name) + f"_{mode}_after.png"))
                entry[mode] = {"load_s": secs, "livelits": len(before["widgets"]), "errors": before["errors"],
                               "errors_after": after["errors"], "clicked": clicked, "latency_ms": lat,
                               "jit": after["jit"], "drawn_chars": sum(len(w) for w in before["widgets"]),
                               "_before": before, "_after": after}
                print(f"  {name}, compiled {mode}: {len(before['widgets'])} livelits, loaded in {secs} s, "
                      f"{len(lat)} events timed, counts {json.dumps((after['jit'] or {}).get('counts'))}", flush=True)
                for why, n in sorted(((after["jit"] or {}).get("why") or {}).items(), key=lambda kv: -kv[1]):
                    print(f"      {n} x {why[:160]!r}", flush=True)
            compare(name + ", as loaded", entry["on"]["_before"], entry["off"]["_before"], problems)
            compare(name + ", after the events", entry["on"]["_after"], entry["off"]["_after"], problems)
            for mode in ("on", "off"):
                for k in ("_before", "_after"):
                    entry[mode].pop(k)
            report["slides"].append(entry)
    finally:
        d.quit()
    report["problems"] = problems
    with open(args.out, "w") as f:
        json.dump(report, f, indent=1)
    for p in problems:
        print("  DIFFERENT  " + p[:600])
    print(f"\n{len(report['slides'])} slides, {len(problems)} differences")
    return 1 if problems else 0


if __name__ == "__main__":
    sys.exit(main())
