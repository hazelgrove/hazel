#!/usr/bin/env python3
"""Checks the outline's deep-link parameters, `?outline=` and `?ux=`, in Firefox.

A script that measures a slide in UX mode with chosen GUI rows (the
performance series' "UX mode, the slider and the picture") used to reach it
by clicking the outline's UX button and then each row's chip until only the
wanted ones were on. These parameters land there directly:

    /?slide=livelit-playground-full-body&panel=none&outline=ux&ux=arm_length,drawn

Run against a locally served build:

    python3 scripts/dev_serve.py --port 8111     # in its own terminal
    python3 scripts/outline_link_check.py --port 8111

The cases share one browser profile, in order, because the later ones check
what the earlier ones leave saved: a link applies once, and is then ordinary
state. Exits non-zero on any failure. Uses deck_regress.Driver (raw
WebDriver, geckodriver and firefox on PATH).
"""

import argparse
import os
import subprocess
import sys
import time
import urllib.parse

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from deck_regress import Driver

SLIDE = "livelit-playground-full-body"
# every livelit row on the slide: UX mode's rows when none are chosen
ALL_ROWS = 43

STATE = """
  const name = c => (c.parentElement.innerText || '').replace(/[^A-Za-z0-9_]/g, '');
  const btns = [...document.querySelectorAll('.outline-mode-btn')];
  return { ux: btns.length > 1 && btns[1].classList.contains('outline-mode-on'),
           on: [...document.querySelectorAll('.outline-ux-chip.outline-ux-on')].map(name),
           sliders: [...document.querySelectorAll('input[type=range]')].filter(i => i.getClientRects().length).length,
           pictures: [...document.querySelectorAll('svg')].filter(s => s.getClientRects().length && s.querySelectorAll('*').length > 40).length };
"""
READY = "return document.querySelectorAll('.outline-mode-btn').length > 1 && document.querySelectorAll('svg').length > 3;"


def wait(d, script, limit):
    t0 = time.time()
    while time.time() - t0 < limit:
        try:
            if d.js(script):
                return time.time() - t0
        except Exception:
            pass
        time.sleep(0.5)
    return None


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=8111)
    ap.add_argument("--wd-port", type=int, default=4845)
    ap.add_argument("--settle", type=float, default=10, help="seconds after load before reading (Full Body is slow)")
    a = ap.parse_args()
    q = lambda **kw: urllib.parse.urlencode({"slide": SLIDE, "panel": "none", **kw})
    failures = []

    def visit(path):
        d.goto(path)
        if wait(d, READY, 300) is None:
            raise SystemExit(f"did not load: {path}")
        time.sleep(a.settle)
        return d.js(STATE)

    def check(name, st, ux, on=None, n_on=None, sliders=None, pictures=None):
        bad = []
        if st["ux"] != ux: bad.append(f"ux mode {st['ux']}, wanted {ux}")
        if on is not None and sorted(st["on"]) != sorted(on): bad.append(f"rows on {st['on']}, wanted {on}")
        if n_on is not None and len(st["on"]) != n_on: bad.append(f"{len(st['on'])} rows on, wanted {n_on}")
        if sliders is not None and st["sliders"] != sliders: bad.append(f"{st['sliders']} sliders shown, wanted {sliders}")
        if pictures is not None and st["pictures"] != pictures: bad.append(f"{st['pictures']} pictures shown, wanted {pictures}")
        print(("FAIL " if bad else "ok   ") + name + ("" if not bad else ": " + "; ".join(bad)), flush=True)
        if bad: failures.append(name)

    d = Driver(a.port, a.wd_port)
    try:
        # (the picture's row reads probe(drawn) while it is shown)
        check("a link lands in UX mode with only the rows it names",
              visit("/fresh?" + q(outline="ux", ux="arm_length,drawn")), True, on=["arm_length", "probedrawn"], sliders=1, pictures=1)
        check("a reload without the parameters keeps the linked view",
              visit("/?" + q()), True, on=["arm_length", "probedrawn"], sliders=1, pictures=1)
        check("outline=dev goes back to dev mode, UX's rows kept for later",
              visit("/?" + q(outline="dev")), False, on=["arm_length", "drawn"])
        check("ux= alone, from dev mode: labels loose, unknown ones dropped",
              visit("/?" + q(ux="Arm Length,nosuchrow")), False, on=["arm_length"])
        check("outline=ux alone, fresh: every livelit row, as the UX button gives",
              visit("/fresh?" + q(outline="ux")), True, n_on=ALL_ROWS)
    finally:
        d.quit()
        # geckodriver from the snap refuses a plain terminate(): signal it
        # from inside the snap
        subprocess.run(["snap", "run", "--shell", "firefox.geckodriver", "-c", f"kill {d.proc.pid}"],
                       timeout=20, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, check=False)
    if failures:
        print(f"{len(failures)} failed")
        sys.exit(1)
    print("all passed")


if __name__ == "__main__":
    main()
