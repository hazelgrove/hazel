"""MEASURING ONLY (^ticket(553)): the map's set-up and lookup, Common.squares with three walks (dac7bc6) against two
(order_count), alternating on each runner; each side's better round a job, the median over jobs, ms; ratio within a job."""
import json, statistics, sys
from pathlib import Path
jobs = sorted(p for p in Path(sys.argv[1] if len(sys.argv) > 1 else "all").iterdir() if p.is_dir())
def best(job, side):
    acc = {}
    for f in sorted(job.glob(f"{side}_*.json")):
        for r in json.load(open(f)): acc.setdefault(r["n"], []).append(r)
    return {n: {k: min(r[k] for r in rs) for k in ("map_setup", "map", "passed")} for n, rs in acc.items()}
rows = {}
for job in jobs:
    a, b = best(job, "three"), best(job, "two")
    for n in a:
        if n in b: rows.setdefault(n, []).append((a[n], b[n]))
print(f"### The map's set-up: three walks against two ({len(jobs)} jobs)\n")
print("| map | set-up, three walks | set-up, two | two / three | per job | map, three | map, two |")
print("|---|---|---|---|---|---|---|")
for n in sorted(rows):
    rs = rows[n]; rat = [b["map_setup"] / a["map_setup"] for a, b in rs]
    m = lambda i, k: statistics.median(r[i][k] for r in rs)
    print(f"| {n}x{n} | {m(0, 'map_setup'):.1f} | {m(1, 'map_setup'):.1f} | {statistics.median(rat):.3f} | " + " ".join(f"{r:.2f}" for r in rat) + f" | {m(0, 'map'):.3f} | {m(1, 'map'):.3f} |")
