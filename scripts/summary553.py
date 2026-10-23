"""MEASURING ONLY (^ticket(553)): one table from every job's World-rule rows.

Each job ran map553.mjs on two library builds, trial/r25-best (report 27's) and trial/r25-553 (with tickets 547, 548
and 551), in the order best, 553, 553, best. Per size, each build's better round in its job, then the median over the
jobs: ms a call for the rule as shipped (the cells passed with every way), the same with the cells passed as a handle
(Hazel.value, 553's build only), and the map rule (the squares held in a map, made once), plus the map's set-up. The
ratio is the map rule over the shipped one on 553's build, within each job, then the median.
"""

import json
import statistics
import sys
from pathlib import Path


def best_rounds(job, side):
    rows = {}
    for p in sorted(job.glob(f"{side}_*.json")):
        for r in json.load(open(p)):
            rows.setdefault(r["n"], []).append(r)
    return {n: {k: min(r[k] for r in rs if k in r) for k in rs[0] if k not in ("n", "cells")} for n, rs in rows.items()}


def main(root):
    jobs = sorted(p for p in Path(root).iterdir() if p.is_dir())
    cols = {}
    for job in jobs:
        for side in ("best", "553"):
            for n, r in best_rounds(job, side).items():
                for k, v in r.items():
                    cols.setdefault((n, side, k), []).append(v)
        b = best_rounds(job, "553")
        for n, r in b.items():
            cols.setdefault((n, "553", "ratio"), []).append(r["map"] / r["passed"])
    med = lambda n, side, k: statistics.median(cols[(n, side, k)]) if (n, side, k) in cols else float("nan")
    sizes = sorted({n for n, _, _ in cols})
    print(f"### The World rule with the squares in a map ({len(jobs)} jobs)\n")
    print("ms a call, each build's better round in a job, the median over jobs. best: trial/r25-best (report 27's); "
          "553: trial/r25-553 (tickets 547, 548, 551). passed: the rule as shipped, the cells passed with every way; "
          "handle: the same, the cells passed as a Hazel.value handle; map: the squares held in a map, made once (set-up "
          "in its own column); ratio: map over passed on 553's build, within a job.\n")
    print("| map | cells | best: passed | best: map | 553: passed | 553: handle | 553: map | 553: map set-up | map / passed |")
    print("|---|---|---|---|---|---|---|---|---|")
    for n in sizes:
        print(f"| {n}x{n} | {n * n} | {med(n, 'best', 'passed'):.3f} | {med(n, 'best', 'map'):.3f} | {med(n, '553', 'passed'):.3f} | "
              f"{med(n, '553', 'passed_handle'):.3f} | {med(n, '553', 'map'):.3f} | {med(n, '553', 'map_setup'):.1f} | "
              f"{med(n, '553', 'ratio'):.4f} |")
    for job in jobs:
        runner = job / "runner.txt"
        if runner.exists():
            print(f"\n{job.name}: " + " ".join(runner.read_text().split()))


if __name__ == "__main__":
    main(sys.argv[1] if len(sys.argv) > 1 else "all")
