#!/usr/bin/env python3
"""Interaction regression tests for the livelits documentation deck.

The unit suite cannot reach these. Every bug this file encodes was found by
a human clicking on the running editor, and each one survived a green
`dune test` -- because they live in the projector's event path, which only
a real pointer interaction exercises. Synthetic `input`/`change` events are
NOT a substitute: they skip the optimistic-render path entirely, and that
path is where two of these bugs were.

Run against a locally served build:

    dune build src/web/www/hazel.js
    python3 scripts/dev_serve.py --port 8111     # in its own terminal
    python3 scripts/deck_regress.py --port 8111

Serve it with dev_serve.py, NOT `python3 -m http.server`: each case
resets saved editor state through that server's /fresh route, and
without it the suite silently goes order-dependent -- every case
inherits the previous one's edits and passes or fails for the wrong
reason. dev_serve.py also serves stylesheets from the source tree and
reports, at /status.json, whether the bundle is older than the sources.

    --only <substring>   run just the cases whose names match
    --shots <dir>        write a screenshot per case

Exits non-zero on any failure. Needs geckodriver and firefox on PATH.
Drives raw WebDriver over HTTP so there is no selenium dependency.
"""

import argparse
import base64
import json
import os
import subprocess
import sys
import time
import urllib.error
import urllib.request

# One place for the slide's identity. The deep-link id is derived from the
# NAME, so renaming the slide changes the URL too -- keep them together.
SLIDE_NAME = "Editable Parameters"
SLIDE_ID = "livelits-editable-parameters"
# /fresh forwards every query param to the page it opens, so the two
# URLs differ only in whether saved state is cleared on the way in.
SLIDE_QUERY = f"slide={SLIDE_ID}&panel=none"
SLIDE_URL = f"/?{SLIDE_QUERY}"
FRESH_URL = f"/fresh?{SLIDE_QUERY}"

# Every slide in the livelits deck, in the order the demo presents them.
DECK = [
    "Overview", "Define a Slider", "Higher-order, Functional Expansion", SLIDE_NAME, "Emotion",
    "Color Picker", "Tree Care", "Timings", "Advanced / JavaScript",
]


class Driver:
    def __init__(self, port, wd_port):
        self.base = f"http://127.0.0.1:{wd_port}"
        self.origin = f"http://localhost:{port}"
        self.proc = subprocess.Popen(
            ["geckodriver", "--port", str(wd_port)],
            stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        for _ in range(40):
            time.sleep(0.5)
            try:
                urllib.request.urlopen(self.base + "/status", timeout=2)
                break
            except Exception:
                pass
        else:
            raise SystemExit("geckodriver did not start")
        caps = {"capabilities": {"alwaysMatch": {
            "browserName": "firefox",
            "moz:firefoxOptions": {"args": ["-headless", "-width", "1600",
                                            "-height", "1100"]}}}}
        r = self._req("POST", "/session", caps)
        if "sessionId" not in r.get("value", {}):
            raise SystemExit("session failed: " + json.dumps(r)[:300])
        self.sid = r["value"]["sessionId"]

    def _req(self, method, path, body=None):
        data = json.dumps(body).encode() if body is not None else None
        req = urllib.request.Request(
            self.base + path, data=data, method=method,
            headers={"Content-Type": "application/json"})
        try:
            with urllib.request.urlopen(req, timeout=90) as f:
                return json.loads(f.read().decode())
        except urllib.error.HTTPError as e:
            return json.loads(e.read().decode())

    def js(self, script, args=None):
        return self._req("POST", f"/session/{self.sid}/execute/sync",
                         {"script": script, "args": args or []}).get("value")

    def goto(self, path):
        self._req("POST", f"/session/{self.sid}/url", {"url": self.origin + path})

    def pointer(self, actions):
        self._req("POST", f"/session/{self.sid}/actions", {"actions": [
            {"type": "pointer", "id": "m",
             "parameters": {"pointerType": "mouse"}, "actions": actions}]})

    def keys(self, names):
        acts = []
        for n in names:
            acts += [{"type": "keyDown", "value": n}, {"type": "keyUp", "value": n}]
        self._req("POST", f"/session/{self.sid}/actions",
                  {"actions": [{"type": "key", "id": "kb", "actions": acts}]})

    def shot(self, path):
        v = self._req("GET", f"/session/{self.sid}/screenshot").get("value")
        if isinstance(v, str):
            with open(path, "wb") as f:
                f.write(base64.b64decode(v))

    def quit(self):
        try:
            self._req("DELETE", f"/session/{self.sid}")
        except Exception:
            pass
        try:
            self.proc.terminate()
        except Exception:
            pass   # snap confinement can deny this; harmless


# The slide hosts several widgets now, so a probe names which one. Index 0
# is the `shown` example the original cases were written against; the
# composition examples are 1..6 in document order (see WIDGET below).
PROBE_TMPL = """
  const ws = [...document.querySelectorAll('.user-livelit')];
  const w = ws[%d];
  const own = w ? [...w.querySelectorAll('.livelit-splice')].filter(
                    e => e.closest('.user-livelit') === w) : [];
  return {
    widgetCount: ws.length,
    totalSplices: document.querySelectorAll('.livelit-splice').length,
    splices: own.length,
    spliceText: own.map(e => e.innerText.trim()),
    widget: w ? w.innerText.trim().replace(/\\n/g, ' | ') : null,
    value: (() => { if (!w) return null;
      const t = w.innerText.replace(/\\n/g, ' ');
      const i = t.lastIndexOf('= ');
      return i >= 0 ? t.slice(i + 2).trim().split(/\\s/)[0] : null; })(),
    errors: [...document.querySelectorAll('.livelit-user-error')].map(e => e.textContent.slice(0, 80)),
  };
"""

# Widgets in document order on the slide.
WIDGET = {"shown": 0, "cap": 1, "by_name": 2,
          "nested_outer": 3, "nested_inner": 4,
          "computed_outer": 5, "computed_inner": 6}

# Every splice on the slide: 2 for `shown`, and 2 per widget for the six
# composition widgets. A drop here means a commit ate someone's code.
TOTAL_SPLICES = 14


def probe(d, which="shown"):
    return d.js(PROBE_TMPL % WIDGET[which])


def reset_slide(d):
    """Drop the saved copy of the slide, via the server's own /fresh route.

    Without this the suite is order-dependent and quietly meaningless:
    each case inherits the previous case's splice edits, so a case can
    pass because of what ran before it. The tell is a digit that grows
    down the run -- `baseline * 2`, then `* 21`, then `* 211`.

    Two earlier attempts here deleted keys from the page itself, and both
    were wrong for the same two reasons:

    - The app was OPEN while they ran. Clearing rows underneath a live
      Hazel does not change what it holds in memory, and it writes that
      back out, so the delete is undone before the next navigation.
    - `indexedDB.open('hazel')` CREATES the database when it is absent --
      an empty v1 with no `kv` store -- and the app then hangs on
      "loading" forever. dev_serve.py's FRESH_PAGE carries this warning
      in a comment; the second attempt reintroduced the bug anyway.

    /fresh is served from the app's ORIGIN but is not the app, so nothing
    holds the database open: it can clear the store, wait, and only then
    navigate. It keeps the colour configuration, which is not slide state
    and so does not make the suite order-dependent -- but it does mean the
    suite runs under whatever theme is saved, so a geometry assertion that
    depends on the theme would be measuring the developer's preferences.
    None currently do.
    """
    d.goto(FRESH_URL)
    time.sleep(3)


def shown_value(st):
    """The `= N` the widget computes, or None.

    Asserting only that splices SURVIVE is not enough: a build that
    refused every commit would keep them perfectly and pass. The
    interaction has to actually do something, so cases check this too.
    Learned from a negative control that passed when it should not have.
    """
    w = st.get("widget") or ""
    if "=" not in w:
        return None
    tail = w.rsplit("=", 1)[1].strip()
    num = ""
    for ch in tail:
        if ch.isdigit() or (ch == "-" and not num):
            num += ch
        else:
            break
    return int(num) if num else None


def load_slide(d, settle=11):
    # reset_slide lands on the slide itself (/fresh redirects there), so
    # there is no second navigation -- one would race the app's first save.
    reset_slide(d)
    time.sleep(settle)
    d.js("const w=document.querySelector('.user-livelit');"
         "if(w) w.scrollIntoView({block:'center'}); return 1;")
    time.sleep(2)


def box(d, selector):
    v = d.js(f"""
      const e = document.querySelector({json.dumps(selector)});
      if (!e) return null;
      const r = e.getBoundingClientRect();
      return JSON.stringify({{x:Math.round(r.x), y:Math.round(r.y),
                              w:Math.round(r.width), h:Math.round(r.height)}});
    """)
    return json.loads(v) if v else None


def splice_box(d, i):
    v = d.js(f"""
      const s = document.querySelectorAll('.livelit-splice')[{i}];
      if (!s) return null;
      const r = s.getBoundingClientRect();
      return JSON.stringify({{x:Math.round(r.x), y:Math.round(r.y),
                              w:Math.round(r.width), h:Math.round(r.height)}});
    """)
    return json.loads(v) if v else None


def click_at(d, x, y):
    d.pointer([{"type": "pointerMove", "x": x, "y": y},
               {"type": "pointerDown", "button": 0},
               {"type": "pointerUp", "button": 0}])


def click_slider(d, frac):
    b = box(d, ".user-livelit input[type=range]")
    click_at(d, b["x"] + int(b["w"] * frac), b["y"] + b["h"] // 2)


def drag_slider(d, fracs):
    b = box(d, ".user-livelit input[type=range]")
    y = b["y"] + b["h"] // 2
    acts = [{"type": "pointerMove", "x": b["x"] + b["w"] // 2, "y": y},
            {"type": "pointerDown", "button": 0}]
    for f in fracs:
        acts.append({"type": "pointerMove", "x": b["x"] + int(b["w"] * f),
                     "y": y, "duration": 120})
    acts.append({"type": "pointerUp", "button": 0})
    d.pointer(acts)


def click_splice(d, i, frac=0.5):
    b = splice_box(d, i)
    click_at(d, b["x"] + int(b["w"] * frac), b["y"] + b["h"] // 2)


# --- invariants -------------------------------------------------------
# Checked after EVERY step of every case, not just at the end: the bugs
# below were all "one interaction too many", and a check that only runs
# at the end of a case would have missed the intermediate state.

def invariants(st, expect_own=2):
    """Return a list of violated invariants, empty when healthy."""
    bad = []
    if st["splices"] != expect_own:
        bad.append(f"this widget has {st['splices']} splices, expected {expect_own}")
    if st["totalSplices"] != TOTAL_SPLICES:
        bad.append(f"slide has {st['totalSplices']} splices, expected {TOTAL_SPLICES}")
    w = st["widget"] or ""
    # `Splice(0)` as literal text means splice_view returned None -- the
    # splice is gone from the projector's syntax. This is the exact
    # signature of the redex-gate bug.
    if "Splice(" in w:
        bad.append("widget renders a literal Splice(n): splices lost from syntax")
    if st["errors"]:
        bad.append(f"livelit errors: {st['errors']}")
    return bad


CASES = []


def case(name):
    def wrap(fn):
        CASES.append((name, fn))
        return fn
    return wrap


@case("loads with both bounds as splices")
def c_load(d, log):
    load_slide(d)
    st = json.loads(json.dumps(probe(d)))
    log(st)
    assert st["spliceText"] == ["baseline", "baseline * 2"], st["spliceText"]
    assert "= 15" in (st["widget"] or ""), st["widget"]
    return st


@case("a single slider click keeps the splices")
def c_click(d, log):
    # Regression: the update redex was committed instead of the merged
    # model, flattening both splices to literals on the first click.
    load_slide(d)
    before = shown_value(probe(d))
    seen = {before}
    for frac in (0.33, 0.8, 0.25):
        click_slider(d, frac)
        time.sleep(6)
        st = probe(d)
        log(st)
        bad = invariants(st)
        assert not bad, f"after click {frac}: {bad}"
        assert st["spliceText"] == ["baseline", "baseline * 2"], st["spliceText"]
        seen.add(shown_value(st))
    # The commit has to land, not merely be survivable.
    assert len(seen) > 1, f"slider never changed the value: {seen}"
    return st


@case("a real pointer drag keeps the splices")
def c_drag(d, log):
    # Regression: synthetic input events did NOT reproduce this. Only a
    # real drag populates the optimistic entry, and the optimistic entry
    # is what made the redex gate read the wrong model.
    load_slide(d)
    before = shown_value(probe(d))
    drag_slider(d, [0.7, 0.6, 0.5, 0.35, 0.25])
    time.sleep(8)
    st = probe(d)
    log(st)
    bad = invariants(st)
    assert not bad, bad
    assert st["spliceText"] == ["baseline", "baseline * 2"], st["spliceText"]
    after = shown_value(st)
    assert after is not None and after != before, \
        f"drag did not commit: {before} -> {after}"
    return st


@case("a splice can be edited, and the widget reads the edit")
def c_edit(d, log):
    load_slide(d)
    click_splice(d, 1, 0.95)
    time.sleep(3)
    d.keys([""])          # End
    time.sleep(1)
    d.keys(["1"])               # baseline * 2 -> baseline * 21
    time.sleep(7)
    st = probe(d)
    log(st)
    bad = invariants(st)
    assert not bad, bad
    assert st["spliceText"][1] == "baseline * 21", st["spliceText"]
    # lo=10, hi=210, pct=50 -> 10 + 200*50/100 = 110
    assert "= 110" in (st["widget"] or ""), st["widget"]
    return st


@case("drag then edit: both still work together")
def c_drag_then_edit(d, log):
    load_slide(d)
    drag_slider(d, [0.6, 0.7, 0.75])
    time.sleep(7)
    click_splice(d, 1, 0.95)
    time.sleep(3)
    d.keys([""]); time.sleep(1)
    d.keys(["1"]); time.sleep(7)
    st = probe(d)
    log(st)
    bad = invariants(st)
    assert not bad, bad
    assert st["spliceText"][1] == "baseline * 21", st["spliceText"]
    return st


@case("a hole in a bound keeps the splices (view degradation is KNOWN)")
def c_hole(d, log):
    # Deleting the `2` leaves `baseline * <hole>`, so `at(m)` does not
    # reduce and Html.text receives an indeterminate expression. The
    # widget then dumps the residual term inline -- ugly, and exactly
    # what the paper's eval_splice/Indet exists to prevent. What must
    # NOT happen is losing the splices, so that is what we assert.
    load_slide(d)
    click_splice(d, 1, 0.95)
    time.sleep(3)
    d.keys([""]); time.sleep(1)
    d.keys([""])          # BackSpace: delete the '2'
    time.sleep(7)
    st = probe(d)
    log(st)
    assert st["splices"] == 2, f"splices lost on hole: {st}"
    assert "Splice(" not in (st["widget"] or ""), st["widget"]
    return st


def drag_widget(d, which, to=0.9):
    """Drag a named widget's own slider. Scrolls it into view first:
    WebDriver pointer events at off-screen coordinates do nothing at all,
    silently, which cost an hour once."""
    idx = WIDGET[which]
    d.js(f"document.querySelectorAll('.user-livelit')[{idx}]"
         ".scrollIntoView({block:'center'}); return 1;")
    time.sleep(1.5)
    b = json.loads(d.js(f"""
      const r = [...document.querySelectorAll('.user-livelit')][{idx}]
                  .querySelector('input[type=range]');
      const rb = r.getBoundingClientRect();
      return JSON.stringify({{x0: Math.round(rb.x + rb.width * 0.5),
                              x1: Math.round(rb.x + rb.width * {to}),
                              y: Math.round(rb.y + rb.height / 2)}});"""))
    acts = [{"type": "pointerMove", "x": b["x0"], "y": b["y"]},
            {"type": "pointerDown", "button": 0}]
    for f in (0.5, 1.0):
        acts.append({"type": "pointerMove",
                     "x": int(b["x0"] + (b["x1"] - b["x0"]) * f),
                     "y": b["y"], "duration": 140})
    acts.append({"type": "pointerUp", "button": 0})
    d.pointer(acts)


@case("a ref in the model resolves to its splice")
def c_refs(d, log):
    # Increment 1: the view says Html.splice(m.lo.ref), not splice(0). A ref
    # that resolved to nothing would render the literal text `Splice(...)`.
    load_slide(d)
    st = probe(d)
    log(st)
    bad = invariants(st)
    assert not bad, bad
    assert "Splice(" not in (st["widget"] or ""), st["widget"]
    assert st["spliceText"] == ["baseline", "baseline * 2"], st["spliceText"]
    return st


@case("a nested widget's edit reaches the widget around it")
def c_nested(d, log):
    # Regression: a projector inside a splice could not commit at all.
    # SubEditor.confine_pre rejected the edit because the CARET was not in
    # the splice, and PerformConfined discards that silently -- so the inner
    # slider moved (rendered optimistically) while the outer, and the
    # program, kept a maximum that was no longer on screen.
    #
    # The assertion that matters is the OUTER's value changing. Asserting
    # only that the inner moved would have passed throughout the bug.
    load_slide(d)
    before_outer = probe(d, "nested_outer")["value"]
    before_inner = probe(d, "nested_inner")["value"]
    log({"before": {"outer": before_outer, "inner": before_inner}})
    drag_widget(d, "nested_inner")
    time.sleep(8)
    after_outer = probe(d, "nested_outer")["value"]
    after_inner = probe(d, "nested_inner")["value"]
    log({"after": {"outer": after_outer, "inner": after_inner}})
    assert after_inner != before_inner, f"inner did not move: {before_inner}"
    assert after_outer != before_outer, (
        f"inner moved {before_inner}->{after_inner} but the enclosing widget "
        f"stayed at {before_outer}: the commit never left the splice")
    bad = invariants(probe(d), 2)
    assert not bad, bad
    return {"outer": f"{before_outer}->{after_outer}",
            "inner": f"{before_inner}->{after_inner}"}


@case("arithmetic around a nested widget recomputes too")
def c_computed(d, log):
    # hi=((<slider>) * 2 + 10): a splice holds an EXPRESSION, so the
    # enclosing widget's maximum is arithmetic over the inner one. Same
    # propagation path as above, with a computation in between.
    load_slide(d)
    before = probe(d, "computed_outer")["value"]
    log({"before_outer": before})
    drag_widget(d, "computed_inner")
    time.sleep(8)
    after = probe(d, "computed_outer")["value"]
    log({"after_outer": after})
    assert after != before, (
        f"computed maximum did not recompute: stayed at {before}")
    return {"outer": f"{before}->{after}"}


@case("a livelit hosting a splice does not clip it vertically")
def c_splice_cell_does_not_clip(d, log):
    # He reported the error bar under a misspelled variable looking cut
    # off. The cause is geometric: an error ARM draws BELOW the row its
    # term sits on (Arms.simple_arm flips the path under the term and
    # adds a hook past that), while a livelit's content cell is exactly
    # one row tall and was `overflow: hidden`. Measured here, the splice
    # ends at y=519.8 and the editor's `.errors` containers sit at
    # exactly 519.8 -- the arm starts precisely where the cell stopped.
    #
    # This asserts the fix directly rather than by provoking an arm.
    # Trying to provoke one is what the first four versions of this case
    # did, and it does not work: an unbound variable inside a splice
    # leaves `.errors` EMPTY (height 0) and reports the error in the
    # status bar instead, so "no decoration found" said nothing about
    # clipping either way. The property that matters is a property of
    # the cell, and it is stable.
    load_slide(d)
    geo = d.js("""
      const w = document.querySelectorAll('.user-livelit')[%d];
      if (!w) return {err: 'no widget'};
      const sp = w.querySelector('.livelit-splice');
      if (!sp) return {err: 'no splice'};
      const cell = sp.closest('.livelit > *') ||
                   (() => { for (let e = sp; e; e = e.parentElement)
                              if (e.parentElement &&
                                  e.parentElement.classList.contains('livelit'))
                                return e;
                            return null; })();
      if (!cell) return {err: 'no livelit content cell above the splice'};
      const cs = getComputedStyle(cell);
      // Any OTHER ancestor BETWEEN the splice and the livelit that still
      // clips vertically would undo this just as effectively. The walk
      // stops at the livelit on purpose: above it is the documentation
      // page's own scrollport (`.Documentation`, overflow-y: auto), which
      // legitimately clips and SCROLLS rather than cutting a row, and
      // every editor on the page sits inside it.
      const clippers = [];
      for (let e = sp; e; e = e.parentElement) {
        const s2 = getComputedStyle(e);
        if (s2.overflowY !== 'visible') {
          clippers.push({cls: (e.className || '').toString().slice(0, 50),
                         overflowY: s2.overflowY});
        }
        if (e.classList.contains('livelit')) break;
      }
      return {overflowX: cs.overflowX, overflowY: cs.overflowY,
              clippers: clippers};
    """ % WIDGET["shown"])
    log(geo)
    assert not geo.get("err"), geo["err"]
    assert geo["overflowY"] == "visible", (
        f"the cell clips vertically again, so an error arm under a splice "
        f"will be cut off: overflow-y is {geo['overflowY']}")
    # The horizontal clip is the one doing real work -- it is what keeps a
    # wide GUI off the code beside it. Losing it would be a regression in
    # the other direction, so it is asserted too.
    assert geo["overflowX"] in ("clip", "hidden"), (
        f"the cell stopped clipping horizontally, so oversized widget "
        f"content can now overlap neighbouring code: {geo['overflowX']}")
    assert not geo["clippers"], (
        f"something between the splice and the page clips vertically: "
        f"{geo['clippers']}")
    return geo



# --- the eye: a livelit's own syntax, edited in place --------------------
# Kids' Choice's face reads its head color from a let-bound slider through
# a cell, `color = (head : Int)`. Opening the eye shows the use's syntax in
# a pane under the face; retyping that cell to name another slider must
# hand the face to that slider. The pane is an editor nested in the
# projector, so only real clicks and keys reach it.

KIDS_FRESH = "/fresh?slide=livelits-emotion-kids-choice&panel=none"
FACE_SVG = ("[...document.querySelectorAll('svg')].find(s => "
            "(s.getAttribute('viewBox') || '').startsWith('0 -80 200 '))")
FACE_FILL = ("const s = %s; if (!s) return null; const p = [...s.querySelectorAll('path')]"
             ".find(x => (x.getAttribute('d') || '').startsWith('M38 60'));"
             " return p && p.getAttribute('fill');" % FACE_SVG)
PANE_LINE = ("const p = document.querySelector('.livelit-syntax'); if (!p) return null;"
             " return p.innerText.split('\\n').filter(l => l.includes(arguments[0])).join(' / ');")


def wait_for(d, script, secs, args=None):
    t = time.time()
    while time.time() - t < secs:
        v = d.js(script, args)
        if v:
            return v
        time.sleep(0.5)
    return None


def slider_box(d, label):
    """The range input of the let-bound slider whose widget reads [label]."""
    return d.js("""
      const ins = [...document.querySelectorAll('.user-livelit input[type=range]')]
        .filter(i => (i.closest('.user-livelit').innerText || '').includes(arguments[0]));
      const i = ins[0]; if (!i) return null;
      i.scrollIntoView({block: 'center'});
      const r = i.getBoundingClientRect();
      return [Math.round(r.x), Math.round(r.y + r.height / 2), Math.round(r.width), i.value];
    """, [label])


def drag_labelled(d, label, frac):
    slider_box(d, label); time.sleep(0.5)   # scroll first, then measure
    b = slider_box(d, label)
    assert b, f"no slider labelled {label!r}"
    x, y, w = b[0], b[1], b[2]
    d.pointer([{"type": "pointerMove", "x": x + w // 2, "y": y},
               {"type": "pointerDown", "button": 0},
               {"type": "pointerMove", "x": x + int(w * frac), "y": y, "duration": 200},
               {"type": "pointerUp", "button": 0}])
    time.sleep(4)


def pane_token(d, word, off):
    """A point in the pane at character [off] of its first [word] token."""
    return d.js("""
      const [word, off] = arguments;
      const p = document.querySelector('.livelit-syntax'); if (!p) return null;
      const w = document.createTreeWalker(p, NodeFilter.SHOW_TEXT); let n;
      while (n = w.nextNode()) if (n.textContent === word) {
        const rg = document.createRange(); rg.setStart(n, off); rg.setEnd(n, off);
        const r = rg.getBoundingClientRect();
        return [Math.round(r.left), Math.round(r.top + r.height / 2)];
      }
      return null;
    """, [word, off])


def open_eye_of(d, target_js):
    """Click the syntax toggle of the livelit whose element [target_js] finds."""
    d.js("(%s).scrollIntoView({block: 'center'}); return 1;" % target_js)
    time.sleep(1)
    eye = d.js("""
      const g = (%s).closest('.projector').getBoundingClientRect(); let best = null, bd = 1e9;
      for (const t of document.querySelectorAll('.livelit-syntax-toggle')) {
        const r = t.getBoundingClientRect();
        const dd = Math.abs(r.top - g.top) + Math.abs(r.left - g.left);
        if (dd < bd) { bd = dd; best = t; } }
      const r = best.getBoundingClientRect();
      return [Math.round(r.left + r.width / 2), Math.round(r.top + r.height / 2)];
    """ % target_js)
    click_at(d, *eye)
    assert wait_for(d, "return !!document.querySelector('.livelit-syntax')", 20), "no pane"
    time.sleep(1)


PANE_TEXT = ("const p = document.querySelector('.livelit-syntax'); return p && "
             "p.innerText.replace(/\\s+/g, ' ').replace('params', '').replace('model', '').trim();")


@case("eye: the revealed model is read-only, and the face's GUI still drives it")
def c_eye_read_only(d, log):
    # Cyrus, docs/livelits.md "Revealing a use's syntax": the model shown
    # under an open eye is selectable but read-only; the GUI changes it.
    d.goto(KIDS_FRESH)
    assert wait_for(d, "return !!" + FACE_SVG, 120), "the face never drew"
    time.sleep(4)
    open_eye_of(d, FACE_SVG)
    before = d.js(PANE_TEXT)
    d.js("document.querySelector('.livelit-syntax').scrollIntoView({block: 'center'}); return 1;")
    time.sleep(1)
    at = pane_token(d, "85", 1)
    assert at, "no `85` in the pane"
    click_at(d, *at)
    time.sleep(1)
    # each key checked on its own: a Backspace after a typed 7 would undo
    # it, and an editable model would pass
    for key in ["7", "\ue003"]:
        d.keys([key])
        time.sleep(1.5)
        after = d.js(PANE_TEXT)
        log({"key": repr(key), "after": after[:60]})
        assert after == before, f"the revealed model took {key!r}: {after[:80]!r}"
    # the face's own smile slider, in its GUI, still commits
    smile = d.js("""
      const i = [...(%s).closest('.projector').querySelectorAll('input[type=range]')][0];
      i.scrollIntoView({block: 'center'});
      const r = i.getBoundingClientRect();
      return [Math.round(r.left), Math.round(r.top + r.height / 2), Math.round(r.width)];
    """ % FACE_SVG)
    time.sleep(1)
    smile = d.js("""
      const i = [...(%s).closest('.projector').querySelectorAll('input[type=range]')][0];
      const r = i.getBoundingClientRect();
      return [Math.round(r.left), Math.round(r.top + r.height / 2), Math.round(r.width)];
    """ % FACE_SVG)
    x, y, w = smile
    d.pointer([{"type": "pointerMove", "x": x + int(w * 0.85), "y": y},
               {"type": "pointerDown", "button": 0},
               {"type": "pointerMove", "x": x + int(w * 0.3), "y": y, "duration": 250},
               {"type": "pointerUp", "button": 0}])
    time.sleep(6)
    moved = d.js(PANE_TEXT)
    log({"moved": moved[:60]})
    assert "smile = 85" not in moved and "smile = " in moved, \
        f"the face's slider did not reach the revealed model: {moved[:80]!r}"
    return {}


@case("eye: a GUI cell takes typing while the syntax is revealed")
def c_eye_gui_cell(d, log):
    # Found on Color (Figure 3): with the syntax shown the use is one
    # splice, its cells nested in it, and a click in a GUI cell found no
    # way in -- the GUI looked unresponsive.
    color = ("[...document.querySelectorAll('.user-livelit')].find(w => "
             "/teal/.test(w.innerText) && w.querySelector('.livelit-splice'))")
    d.goto("/fresh?slide=livelits-color-figure-3&panel=none")
    assert wait_for(d, "return !!(" + color + ")", 120), "no ^color"
    time.sleep(3)
    open_eye_of(d, color)
    at = d.js("""
      const c = (%s).querySelectorAll('.livelit-splice')[0];
      c.scrollIntoView({block: 'center'});
      const t = [...c.querySelectorAll('*')].find(e => e.children.length === 0 && e.textContent === 'red');
      const r = (t || c).getBoundingClientRect();
      return [Math.round(r.right - 1), Math.round(r.top + r.height / 2)];
    """ % color)
    time.sleep(1)
    at = d.js("""
      const c = (%s).querySelectorAll('.livelit-splice')[0];
      const t = [...c.querySelectorAll('*')].find(e => e.children.length === 0 && e.textContent === 'red');
      const r = (t || c).getBoundingClientRect();
      return [Math.round(r.right - 1), Math.round(r.top + r.height / 2)];
    """ % color)
    click_at(d, *at)
    time.sleep(1)
    d.keys(list(" / 2"))
    time.sleep(6)
    pane = d.js(PANE_TEXT)
    log({"pane": pane})
    assert "(red / 2)" in pane, f"typing in the GUI cell did not land: {pane!r}"
    return {}


@case("eye: the params line edits live, and refuses what is not a value")
def c_eye_live_params(d, log):
    # Cyrus: params, where a livelit has them, are edited live. Emotion's
    # ^mood: its params are the mood. A pause in typing commits; text that
    # parses but is no value (a free `x`) commits nothing.
    d.goto("/fresh?slide=livelits-emotion&panel=none")
    assert wait_for(d, "return document.querySelectorAll('.livelit-syntax-toggle').length > 0", 120)
    time.sleep(3)
    last = "[...document.querySelectorAll('.livelit-syntax-toggle')].slice(-1)[0]"
    open_eye_of(d, last)
    state = """
      const i = document.querySelector('.livelit-params-input');
      const p = document.querySelector('.livelit-syntax');
      return [i && i.value, document.activeElement === i,
              p && p.innerText.replace(/\\s+/g, ' ')];
    """
    at = d.js("""
      const i = document.querySelector('.livelit-params-input');
      i.scrollIntoView({block: 'center'});
      const r = i.getBoundingClientRect();
      return [Math.round(r.right - 6), Math.round(r.top + r.height / 2)];
    """)
    time.sleep(1)
    at = d.js("""
      const r = document.querySelector('.livelit-params-input').getBoundingClientRect();
      return [Math.round(r.right - 6), Math.round(r.top + r.height / 2)];
    """)
    click_at(d, *at)
    time.sleep(0.5)
    d.keys(["\ue010", "\ue003", "\ue003", "4", "0"])
    time.sleep(3)
    v, focused, pane = d.js(state)
    log({"after 40": [v, focused, pane[:60]]})
    assert v == "40" and focused and "^mood(40" in pane, f"params 40 did not commit: {[v, focused, pane[:60]]}"
    d.keys(["\ue003", "\ue003", "x"])
    time.sleep(3)
    v, focused, pane = d.js(state)
    log({"after x": [v, focused, pane[:60]]})
    assert "^mood(40" in pane, f"a free x was committed: {pane[:80]!r}"
    return {}

@case("eye: a right-click menu in the open syntax is not clipped")
def c_eye_menu(d, log):
    # Found by hand: the pane under an open eye clipped the menu to its own
    # one row, so `Select term` showed cut off and could not be clicked.
    d.goto(KIDS_FRESH)
    star = "[...document.querySelectorAll('button')].find(b => b.innerText.includes('star eyes'))"
    assert wait_for(d, "return !!" + star, 120), "the star-eyes toggle never drew"
    time.sleep(4)
    d.js("const b = %s; b.scrollIntoView({block: 'center'}); return 1;" % star)
    time.sleep(1)
    eye = d.js("""
      const g = %s.closest('.projector').getBoundingClientRect(); let best = null, bd = 1e9;
      for (const t of document.querySelectorAll('.livelit-syntax-toggle')) {
        const r = t.getBoundingClientRect();
        const dd = Math.abs(r.top - g.top) + Math.abs(r.right - g.left);
        if (dd < bd) { bd = dd; best = t; } }
      const r = best.getBoundingClientRect();
      return [Math.round(r.left + r.width / 2), Math.round(r.top + r.height / 2)];
    """ % star)
    click_at(d, *eye)
    assert wait_for(d, "return !!document.querySelector('.livelit-syntax')", 20), "no pane"
    at = pane_token(d, "false", 2)
    assert at, "no `false` in the star toggle's pane"
    d.pointer([{"type": "pointerMove", "x": at[0], "y": at[1]},
               {"type": "pointerDown", "button": 2},
               {"type": "pointerUp", "button": 2}])
    time.sleep(1.5)
    item = d.js("""
      const m = document.querySelector('.context-menu'); if (!m) return null;
      const it = [...m.querySelectorAll('*')].find(e => /Select term/.test(e.textContent)
        && ![...e.children].some(c => /Select term/.test(c.textContent)));
      if (!it) return null;
      const r = it.getBoundingClientRect(), x = r.left + r.width / 2, y = r.top + r.height / 2;
      const top = document.elementFromPoint(x, y);
      return [Math.round(x), Math.round(y), !!(top && m.contains(top))];
    """)
    log({"select_term": item})
    assert item, "no context menu, or no Select term in it"
    assert item[2], "Select term is covered or clipped: a click there would miss it"
    click_at(d, item[0], item[1])
    time.sleep(1.5)
    assert not d.js("return !!document.querySelector('.context-menu')"), \
        "the menu stayed open after Select term"
    return {"select_term": item}


@case("eye: an open eye keeps the rest of its line on the line")
def c_eye_alignment(d, log):
    # Found by hand on Overview's `(^flag(true), ^flag(false))`: opening
    # the first flag's eye made it a Block, and the second flag dropped to
    # the pane's last row. An inline use now hangs its pane as a Tab.
    d.goto("/fresh?slide=livelits-overview&panel=none")
    flags = """
      return [...document.querySelectorAll('.livelit button')]
        .filter(b => /^(yes|no)$/.test(b.innerText.trim()))
        .map(b => Math.round(b.getBoundingClientRect().top));
    """
    assert wait_for(d, "const f = (() => {%s})(); return f.length == 2 ? f : null;" % flags, 120), \
        "Overview's two flags never drew"
    time.sleep(3)
    d.js("const b = [...document.querySelectorAll('.livelit button')]"
         ".find(b => b.innerText.trim() === 'yes'); b.scrollIntoView({block: 'center'}); return 1;")
    time.sleep(1)
    before = d.js(flags)
    eye = d.js("""
      const b = [...document.querySelectorAll('.livelit button')]
        .find(b => b.innerText.trim() === 'yes');
      const g = b.closest('.projector').getBoundingClientRect(); let best = null, bd = 1e9;
      for (const t of document.querySelectorAll('.livelit-syntax-toggle')) {
        const r = t.getBoundingClientRect();
        const dd = Math.abs(r.top - g.top) + Math.abs(r.right - g.left);
        if (dd < bd) { bd = dd; best = t; } }
      const r = best.getBoundingClientRect();
      return [Math.round(r.left + r.width / 2), Math.round(r.top + r.height / 2)];
    """)
    click_at(d, *eye)
    assert wait_for(d, "return !!document.querySelector('.livelit-syntax')", 20), "no pane"
    time.sleep(1)
    after = d.js(flags)
    log({"tops": [before, after]})
    assert abs(after[0] - after[1]) <= 2, f"the flags are no longer on one row: {after}"
    assert abs(after[1] - before[1]) <= 2, f"the second flag moved: {before} -> {after}"
    return {"tops": [before, after]}



@case("eye: a click with the eye shut adds no parens to the use")
def c_eye_no_parens(d, log):
    # Found by hand on Overview: a flag clicked while its eye was shut
    # came back as `(^flag(false))` -- SetSyntax wrapped every multi-piece
    # syntax in parens -- seen as soon as the eye opened.
    d.goto("/fresh?slide=livelits-overview&panel=none")
    flag = ("[...document.querySelectorAll('.livelit button')]"
            ".filter(b => /^(yes|no)$/.test(b.innerText.trim()))[0]")
    assert wait_for(d, "return !!" + flag, 120), "Overview's flags never drew"
    time.sleep(3)
    d.js("const b = %s; b.scrollIntoView({block: 'center'}); return 1;" % flag)
    time.sleep(1)
    at = d.js("const r = %s.getBoundingClientRect();"
              " return [Math.round(r.left + r.width / 2), Math.round(r.top + r.height / 2)];" % flag)
    click_at(d, *at)
    time.sleep(4)
    eye = d.js("""
      const g = %s.closest('.projector').getBoundingClientRect(); let best = null, bd = 1e9;
      for (const t of document.querySelectorAll('.livelit-syntax-toggle')) {
        const r = t.getBoundingClientRect();
        const dd = Math.abs(r.top - g.top) + Math.abs(r.right - g.left);
        if (dd < bd) { bd = dd; best = t; } }
      const r = best.getBoundingClientRect();
      return [Math.round(r.left + r.width / 2), Math.round(r.top + r.height / 2)];
    """ % flag)
    click_at(d, *eye)
    pane = wait_for(d, "const p = document.querySelector('.livelit-syntax');"
                       " return p && p.innerText.replace(/\\s+/g, ' ').replace('model', '').trim();", 20)
    log({"pane": pane})
    assert pane == "^flag(false)", f"the use came back as {pane!r}"
    return {"pane": pane}


TOGGLE_SPACING = """
  const out = [];
  for (const t of document.querySelectorAll('.livelit-syntax-toggle')) {
    t.scrollIntoView({block: 'center'});
    const T = t.getBoundingClientRect();
    if (T.width === 0) continue;
    // the livelit itself: the projector under the toggle's overlay
    const P = [...document.elementsFromPoint(T.left + 1, T.top + 1)]
      .find(e => e.classList && e.classList.contains('projector') && !e.contains(t));
    if (!P) { out.push('no livelit under a toggle'); continue; }
    const R = P.getBoundingClientRect();
    const gui = [...P.children].find(c => c.tagName !== 'svg');
    const G = gui && gui.getBoundingClientRect();
    const left = document.elementFromPoint(R.left - 3, T.top + T.height / 2);
    if (T.left < R.left - 0.5) out.push('hangs out of its livelit by ' + (R.left - T.left).toFixed(1));
    if (G && T.right > G.left + 0.5) out.push('overlaps the GUI by ' + (T.right - G.left).toFixed(1));
    if (left && left.closest('.livelit-syntax-toggle')) out.push('covers the code to its left');
  }
  return out;
"""


@case("eye: every toggle sits inside its livelit, clear of the code")
def c_eye_spacing(d, log):
    # Found by hand: hanging in the margin, the toggle covered the `=` of
    # `let x = ...` (and Overview's tuple `(`) by its own width.
    problems = []
    for slide in ["livelits-overview", "livelits-emotion-kids-choice"]:
        d.goto(f"/fresh?slide={slide}&panel=none")
        assert wait_for(d, "return document.querySelectorAll('.livelit-syntax-toggle').length > 1", 120)
        time.sleep(3)
        problems += [f"{slide}: {p}" for p in d.js(TOGGLE_SPACING)]
    log({"problems": problems[:5]})
    assert not problems, problems[:5]
    return {}


@case("eye: the toggle stays visible while the mouse is in the livelit")
def c_eye_hover(d, log):
    # Found by hand: hovering raises the livelit above the overlay that
    # holds its toggle, and its backing hid the glyph.
    d.goto("/fresh?slide=livelits-parameters&panel=none")
    assert wait_for(d, "return document.querySelectorAll('.livelit-syntax-toggle').length > 1", 120)
    time.sleep(3)
    d.js("document.querySelector('.livelit-syntax-toggle').scrollIntoView({block: 'center'}); return 1;")
    time.sleep(1)
    on_top = """
      const g = document.querySelector('.livelit-syntax-toggle .livelit-eye-glyph');
      const r = g.getBoundingClientRect();
      const top = document.elementFromPoint(r.left + r.width / 2, r.top + r.height / 2);
      return !!(top && top.closest('.livelit-syntax-toggle'));
    """
    assert d.js(on_top), "the toggle is covered even with the mouse away"
    inside = d.js("""
      const g = document.querySelector('.livelit-syntax-toggle').getBoundingClientRect();
      const P = [...document.elementsFromPoint(g.left + 1, g.top + 1)]
        .find(e => e.classList && e.classList.contains('projector')
                   && !e.querySelector('.livelit-syntax-toggle'));
      const r = P.getBoundingClientRect();
      return [Math.round(r.left + r.width * 0.6), Math.round(r.top + r.height / 2)];
    """)
    d.pointer([{"type": "pointerMove", "x": inside[0], "y": inside[1]}])
    time.sleep(1)
    assert d.js(on_top), "the toggle vanished with the mouse inside the livelit"
    return {}

@case("eye: the Colors slide's ^reveal turns the eyes into triangles")
def c_reveal_choice(d, log):
    # The Colors slide (Configuration mode) chooses the toggle's look with
    # its own livelit, ^reveal: one button per case. Pressing "triangle"
    # must reach every livelit's toggle. Pressed back to "eye" at the end,
    # since /fresh keeps the colour configuration for the cases after.
    setsel = """
      const [want, idx] = arguments;
      const s = [...document.querySelectorAll('select')][idx];
      if (!s || ![...s.options].some(o => o.value === want)) return 'no';
      s.value = want;
      s.dispatchEvent(new Event('input', {bubbles: true}));
      s.dispatchEvent(new Event('change', {bubbles: true}));
      return 'ok';
    """
    glyph = ("return getComputedStyle(document.documentElement)"
             ".getPropertyValue('--hazel-livelit-reveal').trim();")
    press = """
      const b = [...document.querySelectorAll('.choice button')]
        .find(b => b.innerText.trim() === arguments[0]);
      if (!b) return null;
      b.scrollIntoView({block: 'center'});
      const r = b.getBoundingClientRect();
      return [Math.round(r.left + r.width / 2), Math.round(r.top + r.height / 2)];
    """
    d.goto(KIDS_FRESH)
    assert wait_for(d, "return document.querySelectorAll('.livelit-syntax-toggle').length > 3", 120)
    assert d.js(setsel, ["Configuration", 0]) == "ok", "no Configuration mode"
    assert wait_for(d, "return !!document.querySelector('.choice')", 60), "no ^reveal on the Colors slide"
    time.sleep(1)
    d.js(press, ["triangle"]); time.sleep(1)
    click_at(d, *d.js(press, ["triangle"]))
    time.sleep(5)
    assert d.js(glyph) == "triangle", f"the choice did not reach the page: {d.js(glyph)!r}"
    d.goto("/?slide=livelits-emotion-kids-choice&panel=none")
    assert wait_for(d, "return document.querySelectorAll('.livelit-syntax-toggle').length > 3", 120)
    time.sleep(2)
    drawn = d.js("return [...document.querySelectorAll('.livelit-eye-glyph')]"
                 ".slice(0, 3).map(g => getComputedStyle(g, '::before').content);")
    log({"glyphs": drawn})
    assert drawn and all(g == '"\u25b8"' for g in drawn), f"toggles not triangles: {drawn}"
    # back to the eye
    assert d.js(setsel, ["Configuration", 0]) == "ok"
    assert wait_for(d, "return !!document.querySelector('.choice')", 60)
    time.sleep(1)
    d.js(press, ["eye"]); time.sleep(1)
    click_at(d, *d.js(press, ["eye"]))
    time.sleep(5)
    assert d.js(glyph) == "eye", "could not press the eye back"
    return {"glyphs": drawn}

@case("every deck slide renders its livelits")
def c_deck(d, log):
    setsel = """
      const [want, idx] = arguments;
      const s = [...document.querySelectorAll('select')][idx];
      if (!s) return 'nosel';
      if (![...s.options].some(o => o.value === want)) return 'noopt';
      s.value = want;
      s.dispatchEvent(new Event('input', {bubbles:true}));
      s.dispatchEvent(new Event('change', {bubbles:true}));
      return 'ok';
    """
    reset_slide(d)
    d.goto("/"); time.sleep(9)
    d.js(setsel, ["Documentation", 0]); time.sleep(4)
    d.js(setsel, ["Livelits", 1]); time.sleep(4)
    problems = []
    for name in DECK:
        # A slide in a folder, "Advanced / JavaScript", is a path: the
        # folder in the third picker, then the slide in the one it opens.
        parts = name.split(" / ")
        result = "ok"
        for i, part in enumerate(parts):
            result = d.js(setsel, [part, 2 + i])
            if result != "ok":
                break
            if i < len(parts) - 1:
                time.sleep(4)
        if result != "ok":
            problems.append(f"{name}: could not select ({result})"); continue
        time.sleep(7)
        shown = d.js("const s = [...document.querySelectorAll('select')][arguments[0]];"
                     " return s ? s.value : null;", [2 + len(parts) - 1])
        if shown != parts[-1]:
            problems.append(f"{name}: the picker shows {shown!r}"); continue
        st = probe(d)
        log({name: st})
        if st["errors"]:
            problems.append(f"{name}: {st['errors']}")
    assert not problems, problems
    return {"slides": len(DECK)}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=8011,
                    help="port serving the web build")
    ap.add_argument("--wd-port", type=int, default=4599)
    ap.add_argument("--only", help="substring filter on case names")
    ap.add_argument("--shots", help="directory to write a screenshot per case")
    args = ap.parse_args()

    try:
        urllib.request.urlopen(f"http://localhost:{args.port}/", timeout=3)
    except Exception:
        raise SystemExit(f"nothing serving on port {args.port}")

    if args.shots:
        os.makedirs(args.shots, exist_ok=True)

    d = Driver(args.port, args.wd_port)
    failures = []
    try:
        for name, fn in CASES:
            if args.only and args.only.lower() not in name.lower():
                continue
            detail = []
            try:
                fn(d, detail.append)
                print(f"  ok    {name}")
            except AssertionError as e:
                print(f"  FAIL  {name}\n          {e}")
                for line in detail[-2:]:
                    print(f"          seen: {json.dumps(line)[:220]}")
                failures.append(name)
            except Exception as e:
                print(f"  ERROR {name}: {type(e).__name__}: {e}")
                failures.append(name)
            if args.shots:
                d.shot(os.path.join(
                    args.shots, name.replace(" ", "_")[:50] + ".png"))
    finally:
        d.quit()

    total = len([c for c in CASES
                 if not args.only or args.only.lower() in c[0].lower()])
    print(f"\n{total - len(failures)}/{total} passed")
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
