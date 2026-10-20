#!/usr/bin/env python3
"""A table of slide timings (time_slides.mjs's JSON lines), for the job summary.

  python3 scripts/slide_timing/summary.py timing.jsonl [label]

Per slide: the time to open, and where it went, by the largest of: the page's
JavaScript, the eval worker's JavaScript, layout and style, garbage collection
(both threads), the browser's own work outside JS ((program)). A slide whose
largest share is under half of the busy time is "mixed"."""
import json, sys

rows = [json.loads(l) for l in open(sys.argv[1]) if l.strip()]
label = sys.argv[2] if len(sys.argv) > 2 else ""


def parts(r):
    m, ws, mt = r.get("main") or {}, r.get("workers") or [], r.get("metrics") or {}
    return {
        "page JS": m.get("js_ms", 0),
        "worker JS": sum(w.get("js_ms", 0) for w in ws),
        "layout+style": mt.get("layout_ms", 0) + mt.get("style_ms", 0),
        "GC": m.get("gc_ms", 0) + sum(w.get("gc_ms", 0) for w in ws),
        "browser": m.get("program_ms", 0),
    }


def verdict(p):
    busy = sum(p.values()) or 1
    k, v = max(p.items(), key=lambda kv: kv[1])
    return (k if v >= busy / 2 else f"mixed ({k} {100 * v // busy}%)"), busy


print(f"### Slide timing{(' — ' + label) if label else ''}\n")
print("| slide | open (s) | page JS | worker JS | layout+style | GC | browser | dominant | top function on the page |")
print("|---|---:|---:|---:|---:|---:|---:|---|---|")
for r in rows:
    p = parts(r)
    v, busy = verdict(p)
    top = ((r.get("main") or {}).get("top") or [["", 0]])[0]
    o = r.get("open_ms")
    open_s = "—" if o is None else f"{o / 1000:.1f}" + ("" if r.get("settled") else "+")
    print(f"| {r['name']} | {open_s} | {p['page JS']/1000:.1f} | {p['worker JS']/1000:.1f} | {p['layout+style']/1000:.1f} "
          f"| {p['GC']/1000:.1f} | {p['browser']/1000:.1f} | {v} | `{top[0][:50]}` {top[1]} ms |")
done = [r for r in rows if r.get("open_ms") is not None]
print(f"\n{len(rows)} slides; {sum(1 for r in rows if r.get('settled'))} settled; "
      f"total open time {sum(r['open_ms'] for r in done) / 1000:.0f} s. Times in seconds; \"+\": still changing at the limit.")
