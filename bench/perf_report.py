#!/usr/bin/env python3
"""Compare two builds of Hazel's performance measurements and render the
report (docs/performance.md, .github/workflows/perf-report.yml).

Each side is a directory into which bench/perf-compare.sh copied the
perf_work.bc.js and hazel_bench.bc.js it built; either may be missing,
which makes that measurement unavailable rather than an error.

  Work counts: perf_work.bc.js once per side, on the same programs (the
  head's corpus). Deterministic, so any change is real; significant when a
  program's count moves by at least WORK_PCT percent and WORK_MIN_STEPS
  steps, or a program that finished on the base no longer does.

  Timings: hazel_bench.bc.js for --rounds rounds per side, alternating
  sides (base, head, head, base, ...) so drift on the runner falls on both
  equally. Each round reports the median of its reps; a benchmark's change
  is significant when the median of the round medians moves by more than
  TIME_PCT percent and by at least TIME_MIN_MS, AND the two sides' ranges
  (min to max) of round medians do not overlap. min-max rather than the
  interquartile range on purpose: one stray round can widen a range and
  hide a change, but cannot invent one, and false alarms are what would get
  an advisory report ignored. A `total` (statics plus syntax) is shown but
  never flagged on its own: it moves only when one of its parts does.

  A harness that ran on the base but crashed or timed out (RUN_TIMEOUT_S)
  on the head is significant too: a head that hangs or dies is the worst
  regression there is. So is a benchmark the head's harness had to skip
  (it raised) when the base's ran it. The other way round (the base fails,
  the head does not) makes the measurement unavailable for this base.

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
# The smallest benchmarks take 1-6 ms, where a 10% move is well inside
# what a shared runner's jitter can do even with disjoint ranges.
TIME_MIN_MS = 1.0
# Per harness invocation. A full work-count run takes about a minute and a
# timing round well under one, so this is only hit by a hang; two of them
# (work and timings) still leave the job inside its 60-minute limit.
RUN_TIMEOUT_S = 15 * 60

NODE = ["node", "--stack-size=8192"]


def bundle(bundles_dir, name):
    path = os.path.join(bundles_dir, name)
    return os.path.abspath(path) if os.path.exists(path) else None


def last_line(text):
    lines = [l for l in (text or "").splitlines() if l.strip()]
    return lines[-1][-300:] if lines else ""


def run_json(cmd, out_path, timeout):
    """Run a harness that writes JSON to out_path. Returns (data, None), or
    (None, failure) if it exits non-zero or outlives the timeout; failure
    says which, with the last line it wrote to stderr (the harnesses name
    the program or benchmark they start on there)."""
    print("+ " + " ".join(cmd), file=sys.stderr, flush=True)
    try:
        result = subprocess.run(cmd, stdout=subprocess.DEVNULL, stderr=subprocess.PIPE,
                                text=True, timeout=timeout)
    except subprocess.TimeoutExpired as e:
        err = e.stderr.decode(errors="replace") if isinstance(e.stderr, bytes) else e.stderr
        print((err or "")[-4000:], file=sys.stderr)
        print(f"timed out after {timeout:g} s", file=sys.stderr)
        return None, {"kind": "timeout", "timeout": timeout, "last_line": last_line(err)}
    if result.returncode != 0:
        print(result.stderr[-4000:], file=sys.stderr)
        return None, {"kind": "crash", "exit": result.returncode,
                      "last_line": last_line(result.stderr)}
    with open(out_path) as f:
        return json.load(f), None


# A side's run of one harness: "unbuilt" (no bundle), "failed" (it crashed or
# timed out; "failure" says how) or "ok".


def measure_work(js, programs, scratch, side, timeout):
    if js is None:
        return {"status": "unbuilt"}
    out = os.path.join(scratch, f"work-{side}.json")
    data, failure = run_json(NODE + [js, "--out", out] + programs, out, timeout)
    if data is None:
        return {"status": "failed", "failure": failure}
    return {"status": "ok", "programs": {p["program"]: p for p in data["programs"]}}


def head_failed_where_base_ran(base, head):
    return base["status"] == "ok" and head["status"] == "failed"


def compare_work(base, head):
    if base["status"] != "ok" or head["status"] != "ok":
        return {"available": False, "base": base, "head": head,
                "significant": head_failed_where_base_ran(base, head)}
    base, head = base["programs"], head["programs"]
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


def measure_timings(js_by_side, rounds, reps, eval_bench, scratch, timeout):
    """Alternating rounds. Stops at the first round that fails: a harness
    that crashed or hung once will again, and each hang costs a timeout."""
    sides = {s: {"status": "ok", "samples": {}, "skipped": {}, "rounds": 0}
             for s in ("base", "head")}
    for r in range(rounds):
        order = ["base", "head"] if r % 2 == 0 else ["head", "base"]
        for side in order:
            out = os.path.join(scratch, f"time-{side}-{r}.json")
            cmd = (["node", "--expose-gc", "--stack-size=8192", js_by_side[side],
                    "--reps", str(reps), "--out", out]
                   + (["--eval-bench", eval_bench] if eval_bench else []))
            data, failure = run_json(cmd, out, timeout)
            if data is None:
                sides[side]["status"] = "failed"
                sides[side]["failure"] = failure
                return sides
            sides[side]["rounds"] += 1
            for name, ms in data["results"].items():
                sides[side]["samples"].setdefault(name, []).append(ms)
            # Older harnesses do not report skips.
            for name, msg in data.get("skipped", {}).items():
                sides[side]["skipped"].setdefault(name, msg)
    return sides


def skip_key(skipped, name):
    """The skipped benchmark that result `name` belongs to, if any: a
    skipped benchmark is named by a prefix of its results (`eval-bench` for
    `eval-bench/plain`, `let100` for all of `let100/...` if the chain could
    not be set up)."""
    for key in skipped:
        if name == key or name.startswith(key + "/"):
            return key
    return None


def compare_timings(base, head, rounds, reps):
    if base["status"] != "ok" or head["status"] != "ok" or not base["rounds"]:
        return {"available": False, "base": base, "head": head,
                "significant": head_failed_where_base_ran(base, head)
                and base["rounds"] > 0}
    rows = []
    names = list(base["samples"]) + [n for n in head["samples"]
                                     if n not in base["samples"]]
    grouped = {}  # (side, skipped benchmark) -> its row
    for name in names:
        b, h = base["samples"].get(name), head["samples"].get(name)
        if not b or not h:
            # Ran on one side only: skipped (it raised) on the other, or not
            # in the other's harness at all (the base fell back to its own).
            # A skipped benchmark gets one row, not one per result.
            missing = "head" if b else "base"
            other = head if b else base
            key = skip_key(other["skipped"], name)
            if key is not None and (missing, key) in grouped:
                grouped[(missing, key)]["results"] += 1
                continue
            row = {
                "benchmark": name if key is None else key, "missing": missing,
                "skipped": key is not None,
                "reason": other["skipped"][key] if key else "not in its harness",
                "results": 1,
                # the side that has it: its result, or for a skipped
                # benchmark's several results, none
                "value": statistics.median(b or h) if key is None else None,
                # A benchmark the base ran and the head could not.
                "significant": missing == "head" and key is not None,
            }
            if key is not None:
                grouped[(missing, key)] = row
            rows.append(row)
            continue
        mb, mh = statistics.median(b), statistics.median(h)
        (b_lo, b_hi), (h_lo, h_hi) = (min(b), max(b)), (min(h), max(h))
        pct = 100.0 * (mh - mb) / mb if mb else 0.0
        disjoint = b_hi < h_lo or h_hi < b_lo
        is_sum = name.endswith("/total")
        rows.append({
            "benchmark": name,
            "base_median": mb, "head_median": mh,
            "base_range": [b_lo, b_hi], "head_range": [h_lo, h_hi],
            "base_rounds": b, "head_rounds": h,
            "pct": pct,
            "ranges_disjoint": disjoint,
            "sum": is_sum,
            "significant": (not is_sum and abs(pct) > TIME_PCT
                            and abs(mh - mb) >= TIME_MIN_MS and disjoint),
        })
    # Skipped on both sides: no row above, but still worth a line.
    seen = set(base["samples"]) | set(head["samples"])
    neither = sorted(k for k in set(base["skipped"]) | set(head["skipped"])
                     if not any(n == k or n.startswith(k + "/") for n in seen))
    return {
        "available": True,
        "rounds": rounds,
        "reps": reps,
        "benchmarks": rows,
        "skipped_both": [{"benchmark": k, "base": base["skipped"].get(k),
                          "head": head["skipped"].get(k)} for k in neither],
        "significant": any(r["significant"] for r in rows),
    }


# --- Rendering ---


def md(text):
    """Harness output (an exception, a status) made safe for a markdown table
    cell or line: one line, no column breaks, no HTML (the PR comment's
    hidden markers are HTML comments)."""
    text = " ".join(str(text).split())
    text = text.replace("\\", "\\\\").replace("|", "\\|").replace("`", "\\`")
    text = text.replace("<", "&lt;").replace(">", "&gt;")
    return text if len(text) <= 300 else text[:297] + "..."


def fmt_int(n):
    return "–" if n is None else f"{n:,}"


def fmt_pct(p):
    return "–" if p is None else f"{p:+.1f}%"


def fmt_ms(x):
    return f"{x:.1f}" if x >= 10 else f"{x:.2f}"


def fmt_failure(f):
    return (f"timed out after {f['timeout'] / 60:g} min" if f["kind"] == "timeout"
            else f"crashed (exit code {f['exit']})")


def fmt_last_line(f):
    """The harness's last line on stderr: the program or benchmark it had
    started on."""
    if not f.get("last_line"):
        return ""
    return f" The last line it wrote: “{md(f['last_line'])}”."


HARNESS_FILE = {"work": "bench/perf_work.re", "time": "bench/hazel_bench.re"}


TITLE = {"work": "Work counts", "time": "Timings"}
NOUN = {"work": "work counter", "time": "timing harness"}


def unavailable(m, harness):
    """Why measurement m ({"base": .., "head": ..}) is unavailable."""
    key = harness["key"]
    base, head = m["base"], m["head"]
    if head_failed_where_base_ran(base, head):
        return (f"**⚠️ The head's {NOUN[key]} {fmt_failure(head['failure'])}**, "
                f"where the base's ran to the end.{fmt_last_line(head['failure'])}")
    if head["status"] == "unbuilt":
        return f"{TITLE[key]} unavailable: the harness did not build on the head."
    if base["status"] == "failed":
        return (f"{TITLE[key]} unavailable for this base: the {NOUN[key]} "
                f"{fmt_failure(base['failure'])} on the base."
                f"{fmt_last_line(base['failure'])}")
    if base["status"] == "unbuilt" and harness["own"]:
        return (f"{TITLE[key]} unavailable for this base: neither the head's "
                f"`{HARNESS_FILE[key]}` nor the base's own builds against it.")
    if base["status"] == "unbuilt":
        return (f"{TITLE[key]} unavailable for this base: the head's "
                f"`{HARNESS_FILE[key]}` does not build against it (e.g. the base "
                f"predates an API it uses), and the base has no harness of its "
                f"own.")
    return f"{TITLE[key]} unavailable."


def harness_note(harness):
    if harness["used"] != "base":
        return None
    return (f"The base was measured with its own `{HARNESS_FILE[harness['key']]}`: "
            f"the head's does not build against it (an API it calls changed). "
            f"Where the two harnesses differ, their numbers may not be comparable.")


def render_work(work, lines, full, harness):
    lines.append("### Evaluator work (steps)")
    lines.append("")
    if not work["available"]:
        lines.append(unavailable(work, harness))
        lines.append("")
        return
    note = harness_note(harness)
    if note:
        lines.append(note)
        lines.append("")
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

    def cell(r, side):
        return fmt_int(r[side]) if r[side + "_status"] == "ok" else md(r[side + "_status"])

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
                delta = pct = "–"
            else:
                delta, pct = f"{r['delta']:+,}", fmt_pct(r["pct"])
            flag = "⚠️" if r["significant"] else ""
            lines.append(f"| `{md(r['program'])}` | {cell(r, 'base')} | "
                         f"{cell(r, 'head')} | {delta} | {pct} | {flag} |")
        lines.append("")
    if not full:
        return
    lines.append("<details><summary>All programs</summary>")
    lines.append("")
    lines.append("| Program | Base | Head |")
    lines.append("|:--|--:|--:|")
    for r in rows:
        lines.append(f"| `{md(r['program'])}` | {cell(r, 'base')} | {cell(r, 'head')} |")
    lines.append("")
    lines.append("</details>")
    lines.append("")


def render_timings(timings, lines, harness):
    lines.append("### Timings (ms)")
    lines.append("")
    if not timings["available"]:
        lines.append(unavailable(timings, harness))
        lines.append("")
        return
    note = harness_note(harness)
    if note:
        lines.append(note)
        lines.append("")
    lines.append(
        f"js_of_ocaml under node, release profile. {timings['rounds']} rounds per "
        f"side, alternating base and head; each round is the median of "
        f"{timings['reps']} runs. Shown: the median of the round medians, with "
        f"their range (min–max). Wall time on a shared runner is noisy, so a "
        f"change is significant only when it exceeds {TIME_PCT:g}% and "
        f"{TIME_MIN_MS:g} ms **and** the two ranges do not overlap; a `total` "
        f"is never flagged on its own. `letN` is a chain of N `let`s through "
        f"the editor's `CachedStatics` and `CachedSyntax` passes: `cold` a "
        f"program no cache has seen, `warm` the same zipper again, `move` and "
        f"`edit` one action after priming (whole passes, not the editor's "
        f"incremental update path). `eval-bench` is `bench/eval-bench.hz` "
        f"evaluated as `hazel bench-eval` times it.")
    lines.append("")
    lines.append("| Benchmark | Base | Head | Δ% | |")
    lines.append("|:--|--:|--:|--:|:-:|")
    for r in timings["benchmarks"]:
        flag = "⚠️" if r["significant"] else ""
        if "missing" in r:
            gone = ("skipped: " if r["skipped"] else "") + md(r["reason"])
            ran = (fmt_ms(r["value"]) if r["value"] is not None
                   else f"ran ({r['results']} results)")
            b = gone if r["missing"] == "base" else ran
            h = gone if r["missing"] == "head" else ran
            name = r["benchmark"] + ("/…" if r["results"] > 1 else "")
            lines.append(f"| `{name}` | {b} | {h} | – | {flag} |")
            continue
        b = f"{fmt_ms(r['base_median'])} ({fmt_ms(r['base_range'][0])}–{fmt_ms(r['base_range'][1])})"
        h = f"{fmt_ms(r['head_median'])} ({fmt_ms(r['head_range'][0])}–{fmt_ms(r['head_range'][1])})"
        lines.append(f"| `{r['benchmark']}` | {b} | {h} | {fmt_pct(r['pct'])} | {flag} |")
    lines.append("")
    for s in timings["skipped_both"]:
        lines.append(f"`{s['benchmark']}` was skipped on both sides "
                     f"(head: {md(s['head'] or 'not in its harness')}; "
                     f"base: {md(s['base'] or 'not in its harness')}).")
        lines.append("")


def summary(report):
    """The significant findings, one phrase each."""
    sig = []
    work, timings = report["work"], report["timings"]
    for key, m in (("work", work), ("time", timings)):
        if not m["available"] and m["significant"]:
            how = ("timed out" if m["head"]["failure"]["kind"] == "timeout"
                   else "crashed")
            sig.append(f"the head's {NOUN[key]} {how} where the base's ran")
    if work["available"] and work["significant"]:
        n = sum(1 for r in work["programs"] if r["significant"])
        sig.append(f"evaluator work ({n} program{'s' if n != 1 else ''})")
    if timings["available"] and timings["significant"]:
        n = sum(1 for r in timings["benchmarks"] if r["significant"])
        sig.append(f"timings ({n} benchmark{'s' if n != 1 else ''})")
    return sig


def render(report, full=True):
    """The report; full=False leaves out the per-program list, for the PR
    comment (the job summary has it)."""
    lines = []
    sig = summary(report)
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
    render_work(report["work"], lines, full, report["harness"]["work"])
    render_timings(report["timings"], lines, report["harness"]["time"])
    lines.append("What this measures and how to run it locally: "
                 "[docs/performance.md](https://github.com/hazelgrove/hazel/blob/dev/docs/performance.md).")
    return "\n".join(lines) + "\n"


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--base-bundles", required=True,
                    help="directory with the base's built bundles")
    ap.add_argument("--head-bundles", required=True,
                    help="directory with the head's built bundles")
    ap.add_argument("--base-work-harness", choices=["head", "base", "none"],
                    default="head", help="whose perf_work.re the base's bundle is")
    ap.add_argument("--base-time-harness", choices=["head", "base", "none"],
                    default="head", help="whose hazel_bench.re the base's bundle is")
    ap.add_argument("--base-has-harness", action="store_true",
                    help="the base has bench/ harness files of its own")
    ap.add_argument("--programs", nargs="+", required=True,
                    help="programs or directories of .hz files to count work on")
    ap.add_argument("--eval-bench", help="program to time evaluation of")
    ap.add_argument("--rounds", type=int, default=5)
    ap.add_argument("--reps", type=int, default=5)
    ap.add_argument("--timeout", type=float, default=RUN_TIMEOUT_S,
                    help="seconds per harness run before it counts as hung")
    ap.add_argument("--base-label", default="base")
    ap.add_argument("--head-label", default="head")
    ap.add_argument("--out", required=True)
    args = ap.parse_args()

    os.makedirs(args.out, exist_ok=True)
    scratch = os.path.join(args.out, "raw")
    os.makedirs(scratch, exist_ok=True)
    dirs = {"base": args.base_bundles, "head": args.head_bundles}

    # A side is run only if the other has a bundle too: alone, it has
    # nothing to be compared with.
    work_js = {s: bundle(d, "perf_work.bc.js") for s, d in dirs.items()}
    if work_js["base"] and work_js["head"]:
        work = compare_work(
            measure_work(work_js["base"], args.programs, scratch, "base", args.timeout),
            measure_work(work_js["head"], args.programs, scratch, "head", args.timeout))
    else:
        work = compare_work(*({"status": "ok" if work_js[s] else "unbuilt"}
                              for s in ("base", "head")))

    time_js = {s: bundle(d, "hazel_bench.bc.js") for s, d in dirs.items()}
    if time_js["base"] and time_js["head"]:
        sides = measure_timings(time_js, args.rounds, args.reps,
                                args.eval_bench, scratch, args.timeout)
    else:
        sides = {s: {"status": "ok" if time_js[s] else "unbuilt", "samples": {},
                     "skipped": {}, "rounds": 0} for s in dirs}
    timings = compare_timings(sides["base"], sides["head"], args.rounds, args.reps)

    report = {
        "base_label": args.base_label,
        "head_label": args.head_label,
        "harness": {
            "work": {"key": "work", "used": args.base_work_harness,
                     "own": args.base_has_harness},
            "time": {"key": "time", "used": args.base_time_harness,
                     "own": args.base_has_harness},
        },
        "work": work,
        "timings": timings,
        "significant": bool(work["significant"] or timings["significant"]),
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
