#!/usr/bin/env python3
"""Interaction regression tests for the livelits documentation deck.

The unit suite cannot reach these. Every bug this file encodes was found by
a human clicking on the running editor, and each one survived a green
`dune test` -- because they live in the projector's event path, which only
a real pointer interaction exercises. Synthetic `input`/`change` events are
NOT a substitute: they skip the optimistic-render path entirely, and that
path is where two of these bugs were.

Run against a locally served build:

    dune build src/web src/web/www
    (cd _build/default/src/web/www && python3 -m http.server 8011)
    python3 scripts/deck_regress.py --port 8011

Needs geckodriver and firefox on PATH. Drives raw WebDriver over HTTP so
there is no selenium dependency.
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

SLIDE_URL = "/?slide=livelits-splices-mvp&panel=none"

# Every slide in the livelits deck, in the order the demo presents them.
DECK = [
    "Overview", "Define a Slider", "The Expansion", "Splices MVP", "Emotion",
    "Color Picker", "Tree Care", "Timings", "JavaScript (advanced)",
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


PROBE = """
  const w = document.querySelector('.user-livelit');
  return {
    splices: document.querySelectorAll('.livelit-splice').length,
    spliceText: [...document.querySelectorAll('.livelit-splice')].map(e => e.innerText.trim()),
    widget: w ? w.innerText.trim().replace(/\\n/g, ' | ') : null,
    errors: [...document.querySelectorAll('.livelit-user-error')].map(e => e.textContent.slice(0, 80)),
  };
"""


# Keys the editor saves a documentation slide under. Visiting a slide
# writes one; the saved copy then SHADOWS the shipped source for good.
SAVED_KEYS = ["doc:Livelits / Splices MVP", "doc:Livelits / Splices MVP:agent"]


def reset_slide(d):
    """Drop the saved copy of the slide.

    Without this the suite is order-dependent and quietly meaningless:
    each case inherits the previous case's slider position and splice
    edits, so a case can pass because of what ran before it. Found the
    hard way -- the first run of this file reported `baseline * 211`.
    """
    d.goto("/")
    time.sleep(3)
    d.js("""
      const keys = arguments[0];
      const req = indexedDB.open('hazel');
      req.onsuccess = e => {
        const db = e.target.result;
        try {
          const s = db.transaction('kv', 'readwrite').objectStore('kv');
          keys.forEach(k => s.delete(k));
        } catch (err) { /* store absent on a fresh profile: nothing to clear */ }
      };
      return 1;
    """, [SAVED_KEYS])
    time.sleep(2)


def load_slide(d, settle=11):
    reset_slide(d)
    d.goto(SLIDE_URL)
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

def invariants(st):
    """Return a list of violated invariants, empty when healthy."""
    bad = []
    if st["splices"] != 2:
        bad.append(f"splice count {st['splices']}, expected 2")
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
    st = json.loads(json.dumps(d.js(PROBE)))
    log(st)
    assert st["spliceText"] == ["baseline", "baseline * 2"], st["spliceText"]
    assert "= 15" in (st["widget"] or ""), st["widget"]
    return st


@case("a single slider click keeps the splices")
def c_click(d, log):
    # Regression: the update redex was committed instead of the merged
    # model, flattening both splices to literals on the first click.
    load_slide(d)
    for frac in (0.33, 0.8, 0.25):
        click_slider(d, frac)
        time.sleep(6)
        st = d.js(PROBE)
        log(st)
        bad = invariants(st)
        assert not bad, f"after click {frac}: {bad}"
        assert st["spliceText"] == ["baseline", "baseline * 2"], st["spliceText"]
    return st


@case("a real pointer drag keeps the splices")
def c_drag(d, log):
    # Regression: synthetic input events did NOT reproduce this. Only a
    # real drag populates the optimistic entry, and the optimistic entry
    # is what made the redex gate read the wrong model.
    load_slide(d)
    drag_slider(d, [0.7, 0.6, 0.5, 0.35, 0.25])
    time.sleep(8)
    st = d.js(PROBE)
    log(st)
    bad = invariants(st)
    assert not bad, bad
    assert st["spliceText"] == ["baseline", "baseline * 2"], st["spliceText"]
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
    st = d.js(PROBE)
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
    st = d.js(PROBE)
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
    st = d.js(PROBE)
    log(st)
    assert st["splices"] == 2, f"splices lost on hole: {st}"
    assert "Splice(" not in (st["widget"] or ""), st["widget"]
    return st


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
        if d.js(setsel, [name, 2]) != "ok":
            problems.append(f"{name}: could not select"); continue
        time.sleep(7)
        st = d.js(PROBE)
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
