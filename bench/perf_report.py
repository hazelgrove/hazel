#!/usr/bin/env python3
"""Compare two builds of Hazel's performance measurements and render the
report (docs/performance.md, .github/workflows/perf-report.yml).

Each side is a checkout in which bench/perf-compare.sh has built
_build/default/bench/perf_work.bc.js and hazel_bench.bc.js; either may be
missing, which makes that measurement unavailable rather than an error.

  Work counts: perf_work.bc.js once per side, on the same programs (the
  head's corpus). Deterministic, so any change is real; significant when a
  program's count moves by at least WORK_PCT percent and WORK_MIN_STEPS
  steps, or a program that finished on the base no longer does.

  Timings: hazel_bench.bc.js for --rounds rounds per side, alternating
  sides (base, head, head, base, ...) so drift on the runner falls on both
  equally. Each round reports the median of its reps; a benchmark's change
  is significant when the median of the round medians moves by more than
  TIME_PCT percent AND the two sides' ranges (min to max) of round medians
  do not overlap. min-max rather than the interquartile range on purpose:
  one stray round can widen a range and hide a change, but cannot invent
  one, and false alarms are what would get an advisory report ignored.

Writes OUT/report.json, OUT/report.md (for the job summary), OUT/comment.md
(the same without the per-program list, for the PR comment) and
OUT/significant ("true" or "false"). Standard library only.
"""

import argparse
import json
import os
import statistics
import subprocess
import sys

WORK_PCT = 1.0
WORK_MIN_STEPS = 20
TIME_PCT = 10.0

NODE = ["node", "--stack-size=8192"]


def bundle(side_dir, name):
    path = os.path.join(side_dir, "_build", "default", "bench", name)
    return os.path.abspath(path) if os.path.exists(path) else None


def run_json(cmd, out_path):
    """Run a harness that writes JSON to out_path; None if it fails."""
    print("+ " + " ".join(cmd), file=sys.stderr, flush=True)
    result = subprocess.run(cmd, stdout=subprocess.DEVNULL, stderr=subprocess.PIPE, text=True)
    if result.returncode != 0:
        print(result.stderr[-4000:], file=sys.stderr)
        return None
    with open(out_path) as f:
        return json.load(f)


# --- Work counts ---


def measure_work(js, programs, scratch, side):
    if js is None:
        return None
    out = os.path.join(scratch, f"work-{side}.json")
    data = run_json(NODE + [js, "--out", out] + programs, out)
    if data is None:
        return None
    return {p["program"]: p for p in data["programs"]}


def compare_work(base, head):
    if base is None or head is None:
        return {"available": False, "base_available": base is not None,
                "head_available": head is not None}
    rows = []
    total_base = total_head = 0
    compared = 0
    for program in sorted(set(base) | set(head)):
        b = base.get(program, {"status": "absent", "steps": None})
        h = head.get(program, {"status": "absent", "steps": None})
        row = {"program": program, "base": b["steps"], "head": h["steps"],
               "base_status": b["status"], "head_status": h["status"],
               "significant": False}
        if b["status"] == "ok" and h["status"] == "ok":
            compared += 1
            total_base += b["steps"]
            total_head += h["steps"]
            delta = h["steps"] - b["steps"]
            row["delta"] = delta
            row["pct"] = 100.0 * delta / b["steps"] if b["steps"] else 0.0
            row["changed"] = delta != 0
            row["significant"] = (abs(delta) >= WORK_MIN_STEPS
                                  and abs(row["pct"]) >= WORK_PCT)
        else:
            row["delta"] = row["pct"] = None
            row["changed"] = b["status"] != h["status"]
            # Finishing on the base and not on the head is a regression
            # (over the step limit, or a crash). The other way round is
            # usually a program the base cannot parse.
            row["significant"] = b["status"] == "ok"
        rows.append(row)
    return {
        "available": True,
        "programs": rows,
        "compared": compared,
        "total_base": total_base,
        "total_head": total_head,
        "significant": any(r["significant"] for r in rows),
    }


# --- Timings ---


def measure_timings(js_by_side, rounds, reps, eval_bench, scratch):
    samples = {"base": {}, "head": {}}
    for r in range(rounds):
        order = ["base", "head"] if r % 2 == 0 else ["head", "base"]
        for side in order:
            out = os.path.join(scratch, f"time-{side}-{r}.json")
            cmd = (["node", "--expose-gc", "--stack-size=8192", js_by_side[side],
                    "--reps", str(reps), "--out", out]
                   + (["--eval-bench", eval_bench] if eval_bench else []))
            data = run_json(cmd, out)
            if data is None:
                return None, side
            for name, ms in data["results"].items():
                samples[side].setdefault(name, []).append(ms)
    return samples, None


def compare_timings(samples, rounds, reps):
    rows = []
    for name in samples["base"]:
        b, h = samples["base"][name], samples["head"].get(name)
        if not h:
            continue
        mb, mh = statistics.median(b), statistics.median(h)
        (b_lo, b_hi), (h_lo, h_hi) = (min(b), max(b)), (min(h), max(h))
        pct = 100.0 * (mh - mb) / mb if mb else 0.0
        disjoint = b_hi < h_lo or h_hi < b_lo
        rows.append({
            "benchmark": name,
            "base_median": mb, "head_median": mh,
            "base_range": [b_lo, b_hi], "head_range": [h_lo, h_hi],
            "base_rounds": b, "head_rounds": h,
            "pct": pct,
            "ranges_disjoint": disjoint,
            "significant": abs(pct) > TIME_PCT and disjoint,
        })
    return {
        "available": True,
        "rounds": rounds,
        "reps": reps,
        "benchmarks": rows,
        "significant": any(r["significant"] for r in rows),
    }


# --- Rendering ---


def fmt_int(n):
    return "–" if n is None else f"{n:,}"


def fmt_pct(p):
    return "–" if p is None else f"{p:+.1f}%"


def fmt_ms(x):
    return f"{x:.1f}" if x >= 10 else f"{x:.2f}"


def unavailable_reason(side_available):
    base_ok, head_ok = side_available
    if not head_ok:
        return "unavailable: the harness did not build or run on the head."
    return ("unavailable for this base: the head's harness does not build or "
            "run against it (e.g. the base predates an API it uses).")


def render_work(work, lines, full):
    lines.append("### Evaluator work (steps)")
    lines.append("")
    if not work["available"]:
        lines.append("Work counts " + unavailable_reason(
            (work["base_available"], work["head_available"])))
        lines.append("")
        return
    rows = work["programs"]
    changed = [r for r in rows if r["changed"]]
    tb, th = work["total_base"], work["total_head"]
    tpct = 100.0 * (th - tb) / tb if tb else 0.0
    lines.append(
        f"Steps the evaluator's trampoline takes on a cold run "
        f"(`Evaluator.evaluate_counting_work`), over {work['compared']} programs "
        f"(`hazel-programs/` and `bench/eval-bench.hz`). Deterministic: any "
        f"difference is a real one. Significant: a change of at least "
        f"{WORK_PCT:g}% and {WORK_MIN_STEPS} steps.")
    lines.append("")
    lines.append("| | Base | Head | Δ | Δ% |")
    lines.append("|:--|--:|--:|--:|--:|")
    lines.append(f"| **Total** | {fmt_int(tb)} | {fmt_int(th)} | "
                 f"{th - tb:+,} | {fmt_pct(tpct)} |")
    lines.append("")
    if not changed:
        lines.append("No program's step count changed.")
        lines.append("")
    else:
        lines.append(f"{len(changed)} program(s) changed:")
        lines.append("")
        lines.append("| Program | Base | Head | Δ | Δ% | |")
        lines.append("|:--|--:|--:|--:|--:|:-:|")
        for r in sorted(changed, key=lambda r: (not r["significant"],
                                                -abs(r["pct"] or 0))):
            if r["delta"] is None:
                base = fmt_int(r["base"]) if r["base_status"] == "ok" else r["base_status"]
                head = fmt_int(r["head"]) if r["head_status"] == "ok" else r["head_status"]
                delta = pct = "–"
            else:
                base, head = fmt_int(r["base"]), fmt_int(r["head"])
                delta, pct = f"{r['delta']:+,}", fmt_pct(r["pct"])
            flag = "⚠️" if r["significant"] else ""
            lines.append(f"| `{r['program']}` | {base} | {head} | {delta} | {pct} | {flag} |")
        lines.append("")
    if not full:
        return
    lines.append("<details><summary>All programs</summary>")
    lines.append("")
    lines.append("| Program | Base | Head |")
    lines.append("|:--|--:|--:|")
    for r in rows:
        base = fmt_int(r["base"]) if r["base_status"] == "ok" else r["base_status"]
        head = fmt_int(r["head"]) if r["head_status"] == "ok" else r["head_status"]
        lines.append(f"| `{r['program']}` | {base} | {head} |")
    lines.append("")
    lines.append("</details>")
    lines.append("")


def render_timings(timings, lines):
    lines.append("### Timings (ms)")
    lines.append("")
    if not timings["available"]:
        lines.append("Timings " + unavailable_reason(
            (timings["base_available"], timings["head_available"])))
        lines.append("")
        return
    lines.append(
        f"js_of_ocaml under node, release profile. {timings['rounds']} rounds per "
        f"side, alternating base and head; each round is the median of "
        f"{timings['reps']} runs. Shown: the median of the round medians, with "
        f"their range (min–max). Wall time on a shared runner is noisy, so a "
        f"change is significant only when it exceeds {TIME_PCT:g}% **and** the "
        f"two ranges do not overlap. `letN` is a chain of N `let`s through the "
        f"editor's `CachedStatics` and `CachedSyntax` passes: `cold` a program "
        f"no cache has seen, `warm` the same zipper again, `move` and `edit` "
        f"one action after priming. `eval-bench` is `bench/eval-bench.hz` "
        f"evaluated as `hazel bench-eval` times it.")
    lines.append("")
    lines.append("| Benchmark | Base | Head | Δ% | |")
    lines.append("|:--|--:|--:|--:|:-:|")
    for r in timings["benchmarks"]:
        b = f"{fmt_ms(r['base_median'])} ({fmt_ms(r['base_range'][0])}–{fmt_ms(r['base_range'][1])})"
        h = f"{fmt_ms(r['head_median'])} ({fmt_ms(r['head_range'][0])}–{fmt_ms(r['head_range'][1])})"
        flag = "⚠️" if r["significant"] else ""
        lines.append(f"| `{r['benchmark']}` | {b} | {h} | {fmt_pct(r['pct'])} | {flag} |")
    lines.append("")


def render(report, full=True):
    """The report; full=False leaves out the per-program list, for the PR
    comment (the job summary has it)."""
    lines = []
    sig = []
    if report["work"].get("significant"):
        n = sum(1 for r in report["work"]["programs"] if r["significant"])
        sig.append(f"evaluator work ({n} program{'s' if n != 1 else ''})")
    if report["timings"].get("significant"):
        n = sum(1 for r in report["timings"]["benchmarks"] if r["significant"])
        sig.append(f"timings ({n} benchmark{'s' if n != 1 else ''})")
    lines.append(f"Head `{report['head_label']}` against its merge base with "
                 f"`dev`, `{report['base_label']}`.")
    lines.append("")
    if sig:
        lines.append("**⚠️ Significant change: " + "; ".join(sig) + ".** "
                     "Advisory only: this check never blocks a merge. The "
                     "deterministic budgets in `test/evaluator/Test_PerfBudgets.re` "
                     "are the gate.")
    else:
        lines.append("No significant change. Advisory only: this check never "
                     "blocks a merge.")
    lines.append("")
    render_work(report["work"], lines, full)
    render_timings(report["timings"], lines)
    lines.append("What this measures and how to run it locally: "
                 "[docs/performance.md](https://github.com/hazelgrove/hazel/blob/dev/docs/performance.md).")
    return "\n".join(lines) + "\n"


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--base", required=True, help="base checkout, built")
    ap.add_argument("--head", required=True, help="head checkout, built")
    ap.add_argument("--programs", nargs="+", required=True,
                    help="programs or directories of .hz files to count work on")
    ap.add_argument("--eval-bench", help="program to time evaluation of")
    ap.add_argument("--rounds", type=int, default=5)
    ap.add_argument("--reps", type=int, default=5)
    ap.add_argument("--base-label", default="base")
    ap.add_argument("--head-label", default="head")
    ap.add_argument("--out", required=True)
    args = ap.parse_args()

    os.makedirs(args.out, exist_ok=True)
    scratch = os.path.join(args.out, "raw")
    os.makedirs(scratch, exist_ok=True)

    work_js = {s: bundle(d, "perf_work.bc.js") for s, d in
               (("base", args.base), ("head", args.head))}
    work = compare_work(
        measure_work(work_js["base"], args.programs, scratch, "base"),
        measure_work(work_js["head"], args.programs, scratch, "head"))

    time_js = {s: bundle(d, "hazel_bench.bc.js") for s, d in
               (("base", args.base), ("head", args.head))}
    samples, failed = None, None
    if time_js["base"] and time_js["head"]:
        samples, failed = measure_timings(time_js, args.rounds, args.reps,
                                          args.eval_bench, scratch)
    if samples is None:
        timings = {"available": False,
                   "base_available": bool(time_js["base"]) and failed != "base",
                   "head_available": bool(time_js["head"]) and failed != "head"}
    else:
        timings = compare_timings(samples, args.rounds, args.reps)

    report = {
        "base_label": args.base_label,
        "head_label": args.head_label,
        "work": work,
        "timings": timings,
        "significant": bool(work.get("significant") or timings.get("significant")),
    }
    with open(os.path.join(args.out, "report.json"), "w") as f:
        json.dump(report, f, indent=2)
    with open(os.path.join(args.out, "report.md"), "w") as f:
        f.write(render(report))
    with open(os.path.join(args.out, "comment.md"), "w") as f:
        f.write(render(report, full=False))
    with open(os.path.join(args.out, "significant"), "w") as f:
        f.write("true\n" if report["significant"] else "false\n")
    print(render(report))


if __name__ == "__main__":
    main()
