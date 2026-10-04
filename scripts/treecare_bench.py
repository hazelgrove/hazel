#!/usr/bin/env python3
"""Attribute tree care's evaluation cost: view, or everything else?

`UserLivelit.instrument_view` folds `^tree.view(model)` into the MAIN
program evaluation, but only for a use that is projected and probed
(Statics.re, the `(Livelit, true, Some(...))` case). The CLI evaluates the
same program with no projector, so its eval is the program WITHOUT the
view. The editor's eval is the same program WITH it.

So: same tree, both ways, and the difference is what the view costs.

Generates tree-care programs with perfect binary trees of increasing depth
and runs `hazel bench-eval` on each.
"""
import argparse, os, re, subprocess, sys, time

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SLIDE = os.path.join(ROOT, "hazel-programs/docs/livelits/tree-care.hz")
CLI = os.path.join(ROOT, "_build/default/src/CLI/cli.bc.js")
STUB = os.path.join(ROOT, "test/idb_stub.js")

BARK = ["Smooth", "Rough", "Aspen"]
LEAF = ["Oak", "Maple", "Elm", "Aspen"]


def tree(depth, n=[0]):
    """A perfect binary tree of the given depth: 2**depth leaves."""
    if depth == 0:
        n[0] += 1
        return f"Leaf({LEAF[n[0] % len(LEAF)]})"
    n[0] += 1
    return (f"Branch({BARK[n[0] % len(BARK)]}, "
            f"{tree(depth-1)}, {tree(depth-1)})")


def program(depth, view_body=None):
    src = open(SLIDE).read()
    # swap the model literal for one of our own size
    old = re.search(r"\^\^livelit\(\^tree\(\(\s*tree=.*?season=Spring\s*\)\)\)",
                    src, re.S)
    assert old, "model literal not found"
    src = src.replace(old.group(0),
                      f"^^livelit(^tree((tree={tree(depth)}, season=Spring)))")
    if view_body is not None:
        # replace the whole `let view = ...;` member with a constant one
        m = re.search(r"\n  let view = fun m : Model ->.*?\n\n", src, re.S)
        assert m, "view member not found"
        src = src.replace(m.group(0), f"\n  let view = fun m : Model -> {view_body};\n\n")
    return src


def run(path, iters):
    t0 = time.time()
    p = subprocess.run(
        ["node", "--stack-size=8192", "--require", STUB, CLI,
         "bench-eval", "-n", str(iters), path],
        capture_output=True, text=True, timeout=900)
    wall = time.time() - t0
    out = (p.stdout or "") + (p.stderr or "")
    out = "\n".join(l for l in out.splitlines() if "SLOW PARSE" not in l)
    return wall, out.strip()


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--depths", default="1,2,3,4,5,6,7")
    ap.add_argument("--iterations", type=int, default=3)
    args = ap.parse_args()
    tmp = "/tmp/claude-1000/treecare"
    os.makedirs(tmp, exist_ok=True)

    print(f"  {'depth':>5} {'leaves':>7} {'bytes':>8}  eval (no view, CLI)")
    for d in [int(x) for x in args.depths.split(",")]:
        src = program(d)
        path = os.path.join(tmp, f"d{d}.hz")
        open(path, "w").write(src)
        wall, out = run(path, args.iterations)
        nums = re.findall(r"([\d.]+)\s*(ms|s)\b", out)
        summary = out.splitlines()[-1][:90] if out else "(no output)"
        print(f"  {d:>5} {2**d:>7} {len(src):>8}  wall {wall:6.1f}s  {summary}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
