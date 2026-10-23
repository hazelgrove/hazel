"""MEASURING ONLY (^ticket(553)): Hazel's map against Fumola's, from every job's rows (the second run).

Each job ran, on one runner, Hazel's World rule on trial/r25-553 (map553.mjs: the cells passed, as a handle, and the
squares held in a map) and report 27's Fumola driver (measure.mjs: the map passed as text, the neighbours, held as a
list, held as a HashMap), in the order hazel, fumola, fumola, hazel. Per size, each side's better round in a job, then
the median over the jobs; the ratio, Hazel's map over Fumola's HashMap, is taken within each job, then the median, with
each job's ratio listed.
"""

import json
import statistics
import sys
from pathlib import Path


def best(job, side):
    rows = {}
    for p in sorted(job.glob(f"{side}_*.json")):
        for r in json.load(open(p)):
            rows.setdefault(r["n"], []).append(r)
    return {n: {k: min(r[k] for r in rs) for k in rs[0] if isinstance(rs[0][k], (int, float)) and k not in ("n", "cells")} for n, rs in rows.items()}


def main(root):
    jobs = sorted(p for p in Path(root).iterdir() if p.is_dir())
    cols, ratio = {}, {}
    for job in jobs:
        h, f = best(job, "hazel"), best(job, "fumola")
        for n in h:
            for k, v in h[n].items():
                cols.setdefault((n, "h", k), []).append(v)
            for k, v in f.get(n, {}).items():
                cols.setdefault((n, "f", k), []).append(v)
            if n in f:
                ratio.setdefault(n, []).append(h[n]["map"] / f[n]["held_map"])
    m = lambda n, s, k: statistics.median(cols[(n, s, k)]) if (n, s, k) in cols else float("nan")
    print(f"### The World rule held in a map: Hazel against Fumola, on the same runners ({len(jobs)} jobs)\n")
    print("ms a call, each side's better round in a job, the median over jobs. Hazel: trial/r25-553 (tickets 547, 548, "
          "551), the game's rule texts (hazel-map-553). Fumola: wasm from 667a34d, report 27's driver. Hazel map over "
          "Fumola HashMap is taken within each job.\n")
    print("| map | cells | Hazel passed | Hazel handle | Hazel map | Hazel map set-up | Fumola passed | Fumola neighbours | Fumola held list | Fumola HashMap | Fumola set-up | Hazel map / Fumola HashMap | per job |")
    print("|---|---|---|---|---|---|---|---|---|---|---|---|---|")
    for n in sorted(ratio):
        print(f"| {n}x{n} | {n * n} | {m(n, 'h', 'passed'):.3f} | {m(n, 'h', 'passed_handle'):.3f} | {m(n, 'h', 'map'):.3f} | "
              f"{m(n, 'h', 'map_setup'):.1f} | {m(n, 'f', 'passed'):.3f} | {m(n, 'f', 'near'):.3f} | {m(n, 'f', 'held_list'):.3f} | "
              f"{m(n, 'f', 'held_map'):.3f} | {m(n, 'f', 'setup_ms'):.1f} | {statistics.median(ratio[n]):.2f} | "
              + " ".join(f"{r:.2f}" for r in ratio[n]) + " |")
    for job in jobs:
        runner = job / "runner.txt"
        if runner.exists():
            print(f"\n{job.name}: " + " ".join(runner.read_text().split()))


if __name__ == "__main__":
    main(sys.argv[1] if len(sys.argv) > 1 else "all")
