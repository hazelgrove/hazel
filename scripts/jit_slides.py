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

On the Witch slide (--edit), also small edits, both ways: a slider stepped, and the time to its own livelit's change,
to the program's run, and to the picture's change, with how many of the picture's shapes changed.

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


# (^report(46)'s next step 3) each livelit view the page drew, run again on the same model 15 times both ways, bypassing
# the app's memo (globalThis.hazelJit.bench, in HazelJitValue.register_bench): as the app runs it compiled (model in,
# call, commands, drawing), the compiled view without its drawing, and on the interpreter (definition, view, command,
# HazelDOM's drawing). On a fresh load with compiled views on, so the picture is the slide's own.
BENCH_NAMES = "return (globalThis.hazelJit && globalThis.hazelJit.views) ? Object.keys(globalThis.hazelJit.views) : [];"
BENCH = "return globalThis.hazelJit.bench(arguments[0], arguments[1]);"


# (^report(46): the cost of a small edit) one step of a slider on the slide, as the browser's own input and change
# events, and from that moment: when the slider's own livelit changes (its update and view, before any run), when the
# program's run starts and ends (#dynamics's class), and when the picture changes (the livelit that draws the most
# shapes): the run, then every view on the slide drawn again. Watched on the whole page, since a projector's element
# may be made anew; each livelit told apart by what it holds, not by its element. Also how many of the picture's
# shapes the edit changed: what an incremental drawing of the picture would redo.
EDIT = r"""
  const [label, delta] = arguments;
  const lls = () => [...document.querySelectorAll('.user-livelit')];
  const count = w => w.querySelectorAll('polygon, polyline').length;
  const shapes = w => [...w.querySelectorAll('polygon, polyline')].map(e => e.outerHTML);
  const picture = () => lls().reduce((a, w) => (!a || count(w) > count(a)) ? w : a, null);
  // (a ^slider's view: its label in a span, then the range input)
  const slider = () => [...document.querySelectorAll('.user-livelit input[type=range]')]
    .find(i => i.parentElement && i.parentElement.firstElementChild
               && i.parentElement.firstElementChild.textContent.trim() === label);
  const inp = slider(), pic = picture();
  if (!inp || !pic) return null;
  inp.scrollIntoView({ block: 'center' });
  const st = { label, from: inp.value, own: [], pic: [], other: 0, dyn: [], before: shapes(pic) };
  st.sig0 = st.before.join('');
  window.__edit = st;
  const dyn = document.querySelector('#dynamics');
  const mo = new MutationObserver(recs => {
    const t = performance.now() - st.t0;
    const p = picture(), s = slider(), own = s && s.closest('.user-livelit');
    let sawOwn = false, sawPic = false, sawOther = false;
    for (const r of recs) {
      if (dyn && r.target === dyn) { st.dyn.push([Math.round(t), dyn.className]); continue; }
      const el = r.target.nodeType === 1 ? r.target : r.target.parentElement;
      const w = el && el.closest && el.closest('.user-livelit');
      if (!w) continue;
      if (w === own) sawOwn = true; else if (w === p) sawPic = true; else sawOther = true;
    }
    if (sawOwn && st.own.length < 50) st.own.push(t);
    if (sawPic && st.pic.length < 50) {
      st.pic.push(t);
      // (the new picture, not a blank or pending one in between)
      const now = shapes(p);
      if (st.picChanged === undefined && now.length > 0.8 * st.before.length && now.join('') !== st.sig0) st.picChanged = t;
    }
    if (sawOther) st.other++;
  });
  st.mo = mo;
  mo.observe(document.body, { subtree: true, childList: true, attributes: true, characterData: true });
  inp.value = String(+inp.value + delta);
  st.to = inp.value;
  // (the page's own timeline, Util.BenchMarks: each update, calculate, statics, view and worker message from here)
  window.hazelBenchMarks = [];
  // (^report(47), experiment 3: the ids the page mints for the edit, from Id.mk's own count)
  st.ids0 = globalThis.hazelIdCount ? globalThis.hazelIdCount() : null;
  st.t0 = performance.now();
  inp.dispatchEvent(new Event('input', { bubbles: true }));
  inp.dispatchEvent(new Event('change', { bubbles: true }));
  return [st.from, st.to, st.before.length];
"""

EDIT_DONE = r"""
  const st = window.__edit;
  if (!st || st.picChanged === undefined) return null;
  if (document.querySelector('.livelit-pending, #dynamics.running, #dynamics.pending')) return null;
  // (the worker's own timeline comes after its last result: wait for it, up to 20 s)
  const bm = window.hazelBenchMarks || [];
  const lastResult = Math.max(-1, ...bm.filter(m => m.kind === 'result').map(m => m.at));
  if (!bm.some(m => m.kind === 'timeline' && m.at >= lastResult)) {
    st.waitTl = st.waitTl || performance.now();
    if (performance.now() - st.waitTl < 20000) return null;
  }
  st.mo.disconnect();
  const count = w => w.querySelectorAll('polygon, polyline').length;
  const pic = [...document.querySelectorAll('.user-livelit')].reduce((a, w) => (!a || count(w) > count(a)) ? w : a, null);
  const after = [...pic.querySelectorAll('polygon, polyline')].map(e => e.outerHTML);
  let changed = Math.abs(after.length - st.before.length);
  for (let i = 0; i < Math.min(after.length, st.before.length); i++) if (after[i] !== st.before[i]) changed++;
  const r = x => Math.round(x * 10) / 10;
  const page_ids = (st.ids0 != null && globalThis.hazelIdCount) ? globalThis.hazelIdCount() - st.ids0 - 1 : null;
  const marks = (window.hazelBenchMarks || []).map(m => Object.assign({}, m, { at: r(m.at - st.t0) },
    m.ms === undefined ? {} : { ms: Math.round(m.ms * 100) / 100 })).sort((a, b) => a.at - b.at);
  delete window.hazelBenchMarks;
  return { marks, page_ids, label: st.label, from: st.from, to: st.to, own_first_ms: st.own.length ? r(st.own[0]) : null,
           picture_changed_ms: r(st.picChanged), picture_last_ms: r(st.pic[st.pic.length - 1]),
           dynamics: st.dyn, other_batches: st.other, shapes: after.length, shapes_changed: changed,
           jit: globalThis.hazelJit ? JSON.parse(JSON.stringify(globalThis.hazelJit.counts || {})) : null,
           jit_why: globalThis.hazelJit ? JSON.parse(JSON.stringify(globalThis.hazelJit.why || {})) : null };
"""

# (^report(46)) what each large answer from the worker held, by its parts' sizes as they crossed (AnswerParts): asked
# for after the edit's timing is done, since it re-reads every kept message
ANSWER_PARTS = "return globalThis.hazelAnswerParts ? globalThis.hazelAnswerParts() : null;"


def print_parts(parts):
    for m in parts:
        if "error" in m:
            print(f"      answer: error {m['error']}", flush=True)
            continue
        mb = lambda b: f"{b / 1e6:.2f} MB" if isinstance(b, (int, float)) else str(b)
        top = "; ".join(f"{p['part']} {mb(p['bytes'])}" + (f" x{p['n']}" if p["n"] > 1 else "") for p in m["parts"][:8])
        pr, tr = m["probes"], m["trace"]
        ev = "; ".join(f"{e['kind']} {e['n']} ({mb(e['bytes'])})" for e in tr["events"])
        print(f"      answer {m['kind']}: {mb(m['bytes'])} as it crossed, {mb(m['unshared_bytes'])} unshared, "
              f"{m['held_refs']} held references; {top}", flush=True)
        print(f"        probes {pr['count']}, samples {pr['samples']} (+{pr['samples_held']} held): value {mb(pr['value'])}, "
              f"env {mb(pr['env'])}, call stack {mb(pr['call_stack'])}, args {mb(pr['args'])}, frame {mb(pr['frame'])}; "
              f"trace: {ev or 'none'}; span close values {mb(tr['span_close_values'])}, envs {mb(tr['span_close_envs'])}", flush=True)
        for t in pr["top"][:4]:
            f = t.get("first")
            if isinstance(f, dict):
                env = "; ".join(f"{e['name']} {mb(e['bytes'])}" + (f" ({e['kind']})" if e["kind"] != "value" else "") for e in f["env"][:8])
                print(f"        probe {t['id'][-50:]}: {t['samples']} samples; its first sample's value {mb(f['value_bytes'])}, "
                      f"environment {mb(f['env_bytes'])} in {f['env_n']} bindings: {env}", flush=True)


# (^report(47), ^ticket(718): experiment 1) the Witch's picture, and every probe sample that converts, sent each way:
# as the term by Marshal (today), as first-order data by Marshal (with and without its sharing table), and as plain JS
# values by structuredClone and by JSON (DataExperiment); read from the last result the worker sent
DATA_EXPERIMENT = "return globalThis.hazelDataExperiment ? globalThis.hazelDataExperiment() : null;"


def print_data_experiment(x):
    if not x or "error" in x:
        print(f"    data experiment: {x}", flush=True)
        return
    print(f"    data experiment: {x['samples']} samples, {x['converted']} convert to data", flush=True)
    for name in ("picture", "all_converted"):
        m = x.get(name)
        if not m:
            continue
        kb = lambda b: f"{b / 1000:.1f} KB" if isinstance(b, (int, float)) else str(b)
        if name == "picture" and x.get("more"):
            mo = x["more"]
            print(f"      picture, more: an id {mo['id_ns']} ns; printed as a term {mo['print_term_chars']} chars in {mo['print_term_ms']} ms, "
                  f"as data {mo['print_data_chars']} chars in {mo['print_data_ms']} ms; into JS from the term {mo['into_js_term_ms']} ms, "
                  f"from data {mo['into_js_data_ms']} ms", flush=True)
            if "build_data_ms" in mo:
                print(f"      picture, built: {mo['build_data_nodes']} data nodes, {mo['build_term_nodes']} term nodes; "
                      f"as terms with fresh ids {mo['build_term_fresh_ms']} ms, as terms with no id minted "
                      f"{mo['build_term_temp_ms']} ms, as data {mo['build_data_ms']} ms", flush=True)
        print(f"      {name}: {m['nodes']} nodes; term {kb(m['term_bytes'])}, data {kb(m['data_bytes'])}, "
              f"data with no sharing table {kb(m['data_noshare_bytes'])}, JSON {kb(m['json_bytes'])}; ms: "
              + ", ".join(f"{k} {v}" for k, v in m["ms"].items()), flush=True)


# (two sliders: the neck moves the head, the hair and the hat with it; an arm moves only that arm and what it holds)
EDIT_STEPS = [("neck", 2), ("neck", -2), ("arms (up / down)", 10), ("arms (up / down)", -10)]

# (^ticket(722), Matthew 2026-11-08: screenshots of Hazel running, with the frame-rate HUD) a drag of a slider as a
# browser delivers one: at each frame the slider moves one step and fires `input` (a transient event: the program's
# text is not changed), sweeping lo to hi and back, and at the end one `change` (the commit). Every frame's time is
# kept, from requestAnimationFrame, as the HUD times them: a page busy for 300 ms is one 300 ms frame, and in it the
# slider moved once, as a real drag's pointer moves are folded into the next frame.
DRAG = r"""
  const [label, lo, hi, ms] = arguments;
  const slider = () => [...document.querySelectorAll('.user-livelit input[type=range]')]
    .find(i => i.parentElement && i.parentElement.firstElementChild
               && i.parentElement.firstElementChild.textContent.trim() === label);
  const inp = slider();
  if (!inp) return null;
  inp.scrollIntoView({ block: 'center' });
  const st = { label, from: +inp.value, frames: [], inputs: 0, done: false, t0: performance.now() };
  window.__drag = st;
  let v = +inp.value, dir = 1, last = 0;
  function step(t) {
    if (last) st.frames.push(Math.round((t - last) * 10) / 10);
    last = t;
    if (t - st.t0 >= ms) {
      const s = slider();
      if (s) s.dispatchEvent(new Event('change', { bubbles: true }));
      st.to = v; st.done = true; st.ms = Math.round(t - st.t0);
      return;
    }
    v += dir; if (v >= hi) dir = -1; if (v <= lo) dir = 1;
    const s = slider();
    if (s) { s.value = String(v); s.dispatchEvent(new Event('input', { bubbles: true })); st.inputs++; }
    requestAnimationFrame(step);
  }
  requestAnimationFrame(step);
  return [st.from];
"""
DRAG_DONE = "const st = window.__drag; return (st && st.done) ? {label: st.label, from: st.from, to: st.to, ms: st.ms, inputs: st.inputs, frames: st.frames} : null;"
DRAGS = [("neck", 0, 60)]


def drag_stats(frames):
    frames = sorted(frames)
    n, total = len(frames), sum(frames)
    q = lambda p: frames[min(n - 1, int(p * n))]
    return {"frames": n, "fps": round(1000 * n / total, 1) if total else 0, "median_ms": q(0.5), "p90_ms": q(0.9),
            "max_ms": frames[-1], "within_16_7": sum(1 for f in frames if f <= 1000 / 60 * 1.05)}


def drags(d, sid, shots):
    """Each slider in DRAGS dragged for 3 s on a fresh load, both ways, with the HUD on: once timed, then once more
    with a screenshot halfway (a screenshot can itself hold up a frame, so it is not taken in the timed drag)."""
    out = {}
    for mode in ("on", "off"):
        load(d, sid, mode, hud=True)
        rows = []
        for label, lo, hi in DRAGS:
            if not d.js(DRAG, [label, lo, hi, 3000]):
                rows.append({"label": label, "missing": True})
                print(f"    drag {label!r}: no such slider", flush=True)
                continue
            got = wait_for_value(d, DRAG_DONE, 120)
            if not got:
                rows.append({"label": label, "timed_out": True})
                print(f"    drag {label!r}: did not finish in 120 s", flush=True)
                continue
            got["stats"] = drag_stats(got["frames"])
            st = got["stats"]
            print(f"    drag, compiled {mode}: {label!r} for {got['ms']} ms: {st['frames']} frames, {st['fps']} fps, "
                  f"median {st['median_ms']} ms, 90th {st['p90_ms']} ms, longest {st['max_ms']} ms, "
                  f"{st['within_16_7']} within a 60 fps frame; {got['inputs']} inputs", flush=True)
            D.settled(d)
            time.sleep(1)
            if shots and d.js(DRAG, [label, lo, hi, 3000]):
                time.sleep(1.8)
                d.shot(os.path.join(shots, f"drag_{re.sub(r'[^A-Za-z0-9]+', '_', label)}_{mode}.png"))
                wait_for_value(d, DRAG_DONE, 120)
                D.settled(d)
            rows.append(got)
        out[mode] = rows
    return out


def wait_for_value(d, script, secs):
    t = time.time()
    while time.time() - t < secs:
        v = d.js(script)
        if v:
            return v
        time.sleep(0.25)
    return None


def print_marks(got):
    """where the edit's time went, from the page's timeline: before the first redraw, and until the picture changed"""
    marks = got.get("marks") or []
    for name, until in (("before the first redraw", got.get("own_first_ms") or 0), ("until the picture", got["picture_changed_ms"])):
        sums = {}
        for m in marks:
            if m["at"] <= until and "ms" in m:
                k = m["what"] + (f" ({m['kind']})" if m.get("kind") else "")
                n, t = sums.get(k, (0, 0.0))
                sums[k] = (n + 1, t + m["ms"])
        top = sorted(sums.items(), key=lambda kv: -kv[1][1])[:10]
        print(f"      {name} ({until} ms): " + "; ".join(f"{k} {t:.0f} ms x{n}" for k, (n, t) in top), flush=True)
    wids = sum(m.get("ids", 0) for m in marks if m["what"] == "worker: slices" and m["at"] > 0)
    print(f"      ids minted: by the page {got.get('page_ids')}, by the worker's runs of slices {wids:.0f}", flush=True)
    sums = {}
    for m in marks:
        if m["what"].startswith("worker: ") and "ms" in m:
            n, t, b, e = sums.get(m["what"], (0, 0.0, 0.0, 0.0))
            sums[m["what"]] = (n + 1, t + m["ms"], b + m.get("bytes", 0), e + m.get("eval_ms", 0))
    if sums:
        print("      the worker's own steps: " + "; ".join(
            f"{k[8:]} {t:.0f} ms x{n}" + (f" ({b / 1e6:.2f} MB)" if b else "") + (f" (evaluator {e:.0f} ms)" if e else "")
            for k, (n, t, b, e) in sorted(sums.items(), key=lambda kv: -kv[1][1])), flush=True)
    for m in marks:
        if m.get("kind") == "result" or m["what"] == "post to worker":
            print(f"      {m['at']} ms: {m['what']} {json.dumps({k: v for k, v in m.items() if k not in ('what', 'at')})}", flush=True)


def edits(d, sid):
    """Each small edit both ways, on a fresh load: the slider stepped, then wait for the run and the picture."""
    out = {}
    for mode in ("on", "off"):
        load(d, sid, mode)
        rows = []
        for label, delta in EDIT_STEPS:
            started = d.js(EDIT, [label, delta])
            if not started or not isinstance(started, list):
                rows.append({"label": label, "missing": True, "said": started})
                print(f"    edit {label!r}: no such slider, or no picture ({started!r})", flush=True)
                continue
            got = wait_for_value(d, EDIT_DONE, 240)
            if not got:
                rows.append({"label": label, "from": started[0], "to": started[1], "timed_out": True})
                print(f"    edit {label!r} {started[0]} -> {started[1]}: the picture did not change in 240 s", flush=True)
                continue
            # (the breakdown re-reads megabytes in the page: WebDriver's script limit raised from 30 s)
            d._req("POST", f"/session/{d.sid}/timeouts", {"script": 600000})
            t = time.time()
            raw = d.js(ANSWER_PARTS, timeout=600)
            got["answer_parts"] = json.loads(raw) if raw else None
            got["answer_parts_s"] = round(time.time() - t, 1)
            rows.append(got)
            print_marks(got)
            if got["answer_parts"]:
                print_parts(got["answer_parts"])
            print(f"    edit, compiled {mode}: {label!r} {got['from']} -> {got['to']}: the slider's own livelit "
                  f"{got['own_first_ms']} ms; the picture {got['picture_changed_ms']} ms "
                  f"({got['shapes_changed']} of {got['shapes']} shapes changed); the run {got['dynamics']}", flush=True)
            # (^ticket(722)) the slide's compiled run at this edit: events, answers, and why it was not used (since load)
            print(f"      the slide's run, since load: " + json.dumps({k: v for k, v in (got.get("jit") or {}).items()
                                                                  if k.startswith("the slide's run")}), flush=True)
            for why, n in sorted((got.get("jit_why") or {}).items(), key=lambda kv: -kv[1]):
                if why.startswith("the slide's run"):
                    print(f"        {n} x {why[:300]!r}", flush=True)
            D.settled(d)
            time.sleep(1)
        out[mode] = rows
        if mode == "on":
            d._req("POST", f"/session/{d.sid}/timeouts", {"script": 600000})
            raw = d.js(DATA_EXPERIMENT, timeout=600)
            out["data_experiment"] = json.loads(raw) if raw else None
            print_data_experiment(out["data_experiment"])
    return out


def fps(ms):
    return f"{1000 / ms:,.0f}" if ms > 0 else "-"


def bench(d, sid, n, views):
    load(d, sid, "on")
    # (the interpreter's runs of a big view take seconds: the script's own time limit raised from WebDriver's 30 s)
    d._req("POST", f"/session/{d.sid}/timeouts", {"script": 600000})
    out = {}
    for name in d.js(BENCH_NAMES) or []:
        if views and not re.search(views, name):
            continue
        t0 = time.time()
        r = d.js(BENCH, [name, n])
        if not r:
            continue
        out[name] = r
        c, v, i = r["compiled"]["median"], r["view_only"]["median"], r["interpreted"]["median"]
        print(f"    bench {name!r} (drawn {r['drawn']}x): as the app runs it {c:.3f} ms ({fps(c)} fps, {r['compiled']['said']}); "
              f"its view alone {v:.3f} ms ({r['view_only']['said'][:80]}); the interpreter {i:.3f} ms ({fps(i)} fps, "
              f"{r['interpreted']['said'][:60]}); {i / c if c else 0:.0f}x; {time.time() - t0:.0f} s", flush=True)
    return out


def slide_ids():
    by_name = {}
    for name, sid in D.deck_slides():
        by_name[name.split(" / ")[-1]] = sid
    return [(n, by_name[n]) for n in SLIDES if n in by_name]


def load(d, sid, mode, hud=False):
    t0 = time.time()
    d.goto(f"/fresh?slide={sid}&panel=none&jit={mode}" + ("&hud=on" if hud else ""))
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
    ap.add_argument("--bench", default="Witch", help="substring filter on the slides whose views are benched (empty: none)")
    ap.add_argument("--bench-n", type=int, default=15)
    ap.add_argument("--bench-views", default="picture|polygons", help="a regex on the views benched (empty: every one)")
    ap.add_argument("--edit", default="Witch", help="substring filter on the slides where small edits are timed (empty: none)")
    ap.add_argument("--drag", default="Witch", help="substring filter on the slides where a slider's drag is timed, with the HUD (empty: none)")
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
            if args.bench and args.bench.lower() in name.lower():
                entry["bench"] = bench(d, sid, args.bench_n, args.bench_views)
            if args.edit and args.edit.lower() in name.lower():
                entry["edits"] = edits(d, sid)
            if args.drag and args.drag.lower() in name.lower():
                entry["drags"] = drags(d, sid, args.shots)
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
