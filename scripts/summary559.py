"""MEASURING ONLY (^ticket(559)): one table from every job's kernel rows.

Each job ran report 29's kernels (kernels.mjs) on four of GitHub's builds: the counter build (trial/r25-stats) and
the fast path on it (trial/r25-fastpath) once each, for passes; then the timing build (trial/r25-best) and the fast
path on it (trial/r25-fastpath-best) in the order best, fast, fast, best, for times. Runners differ in speed, so a
time is compared only with the other build on the same runner: each job gives a ratio, fast over best, from each
side's better round, and the table gives the median ratio over the jobs. The noise floor is the timing build's own
spread over its two rounds in the same job, the median over the jobs.
"""

import json
import statistics
import sys
from pathlib import Path


def load(job, side):
    rows = {}
    for p in sorted(job.glob(f"{side}_*.json")):
        for r in json.load(open(p)):
            rows.setdefault((r["k"], r["n"]), []).append(r)
    return rows


def main(root):
    jobs = sorted(p for p in Path(root).iterdir() if p.is_dir())
    passes, ratios, noise, ms = {}, {}, {}, {}
    for job in jobs:
        stats, fast = load(job, "stats"), load(job, "fastpath")
        for key, rs in stats.items():
            if key in fast:
                b, f = rs[0]["by_kind"], fast[key][0]["by_kind"]
                passes.setdefault(key, set()).add((b.get("evaluate", 0), f.get("evaluate", 0), f.get("passes skipped", 0)))
        best, fbest = load(job, "best"), load(job, "fastbest")
        for key, rs in best.items():
            if key not in fbest:
                continue
            bt, ft = [r["ms"] for r in rs], [r["ms"] for r in fbest[key]]
            ratios.setdefault(key, []).append(min(ft) / min(bt))
            noise.setdefault(key, []).append(max(bt) / min(bt) - 1)
            ms.setdefault(key, []).append((min(bt), min(ft)))
    print(f"### Report 29's kernels, the fast path against the builds it is on ({len(jobs)} jobs)\n")
    print("Passes: calls through the evaluator's stages on the counter build and on the fast path; skipped: the passes "
          "the fast path stood in for (exact when the first column equals the other two summed). Times: ms a call, the "
          "median over jobs of each side's better round; ratio: fast over best on the same runner, the median over jobs; "
          "noise: the timing build's spread over its own two rounds.\n")
    print("| kernel | n | passes | fast path | skipped | exact | best (ms) | fast (ms) | ratio | per job | noise |")
    print("|---|---|---|---|---|---|---|---|---|---|---|")
    for key in ratios:
        p = passes.get(key, set())
        bp, fp, sk = sorted(p)[0] if p else (0, 0, 0)
        exact = "yes" if p and bp == fp + sk and len(p) == 1 else ("varies" if len(p) > 1 else "no")
        b = statistics.median(x for x, _ in ms[key])
        f = statistics.median(y for _, y in ms[key])
        per = " ".join(f"{r:.2f}" for r in ratios[key])
        print(f"| {key[0]} | {key[1]} | {bp} | {fp} | {sk} | {exact} | {b:.4f} | {f:.4f} | "
              f"{statistics.median(ratios[key]):.3f} | {per} | {statistics.median(noise[key]):.0%} |")
    for job in jobs:
        runner = job / "runner.txt"
        if runner.exists():
            print(f"\n{job.name}: " + " ".join(runner.read_text().split()))


if __name__ == "__main__":
    main(sys.argv[1] if len(sys.argv) > 1 else "all")
