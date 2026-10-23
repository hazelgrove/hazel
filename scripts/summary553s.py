"""MEASURING ONLY (^ticket(553)): the map's set-up in parts (setup553.mjs), the better of two rounds a job, the median
over jobs, ms: squares whole, then its parts (order check, count, balanced build of a list already in order), and id (a
call that ignores the cells: the conversion from JS alone), as written and unannotated."""
import json, statistics, sys
from pathlib import Path
jobs = sorted(p for p in Path(sys.argv[1] if len(sys.argv) > 1 else "all").iterdir() if p.is_dir())
cols = {}
for job in jobs:
    acc = {}
    for f in sorted(job.glob("setup_*.json")):
        for r in json.load(open(f)): acc.setdefault(r["n"], []).append(r)
    for n, rs in acc.items():
        for k in rs[0]:
            if k not in ("n", "cells"): cols.setdefault((n, k), []).append(min(r[k] for r in rs))
parts = ["squares", "in_order", "count", "take", "id"]
print(f"### The map's set-up, in parts ({len(jobs)} jobs)\n")
print("| map | " + " | ".join(f"{v}: {p}" for v in ("annotated", "unannotated") for p in parts) + " |")
print("|---|" + "---|" * 10)
for n in sorted({n for n, _ in cols}):
    print(f"| {n}x{n} | " + " | ".join(f"{statistics.median(cols[(n, f'{v}_{p}')]):.1f}" for v in ("annotated", "unannotated") for p in parts) + " |")
for job in jobs:
    r = job / "runner.txt"
    if r.exists(): print(f"\n{job.name}: " + " ".join(r.read_text().split()))
