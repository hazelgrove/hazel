# Performance

Two things watch Hazel's performance in CI:

- **The budgets** in `test/evaluator/Test_PerfBudgets.re`, part of the quick
  suite and so of the required `Fast checks`. They hold the evaluator's step
  counts on a benchmark program to a ratchet. A count is the same on every run,
  so it can fail a build without false alarms. **They are the merge gate.** See
  the [test README](../test/README.md#performance-budgets).
- **The performance report**, below: wider and noisier, and advisory only.

## The performance report

`.github/workflows/perf-report.yml` runs on every ready-for-review pull request
into `dev` (drafts are skipped until marked ready). It compares the PR head
against its merge base with `dev` and shows up as the **Performance report**
check. It is not a required check and never blocks a merge: it succeeds
whatever it measures, and goes red only if the tooling itself breaks.

Click into the check (its job summary) for the tables. A **PR comment** appears
only when something changed significantly. It is one comment, edited on later
pushes: updated while the change persists, and marked resolved, with the old
report collapsed, once a push no longer shows it. No comment means no
significant change. Forks get the check but not the comment.

### What it measures

**Evaluator work** (deterministic). The steps the evaluator's trampoline takes
(`Evaluator.evaluate_counting_work`, the count the budgets use) on a cold run of
every program in `hazel-programs/` and of `bench/eval-bench.hz`, after statics
and elaboration. Programs that do not finish within 20 million steps are
recorded as over the limit rather than counted. Since a count does not vary
from run to run, any difference is a real one.

*Significant:* a program's count moves by at least 1% and at least 20 steps,
or a program that finished on the base no longer does (over the limit, or an
exception).

**Timings** (noisy). Wall time of:

- the editor's derived-data pipeline, a whole `CachedStatics` pass followed by
  a whole `CachedSyntax` pass, on generated chains of 100 and 500 `let`s, in
  four scenarios: `cold` (a copy with fresh ids, which no memo table has seen,
  as when a program is loaded), `warm` (the same zipper again, as on a
  redraw), `move` (after one cursor move) and `edit` (after editing one
  literal); and
- evaluating `bench/eval-bench.hz` the way `hazel bench-eval` times it: plain
  and incremental (seeding the incremental cache).

Each round runs every benchmark 5 times after an untimed run and keeps the
median. The job runs 5 rounds per side, alternating base and head (base, head,
head, base, ...) on the same runner, so drift lands on both.

*Significant:* the median of a benchmark's round medians changes by more than
10% **and** the base's and head's ranges of round medians (min to max) do not
overlap. Both conditions, because wall time on GitHub's shared runners varies
by 10-40% between runs, and an advisory report that cries wolf gets ignored.

The timings run as js_of_ocaml under node, release profile: what ships. A native
build is not an option, as the core libraries call JavaScript (for instance
`Util.StringUtil`'s regular expressions).

### How the comparison is built

The measurement code is always the PR's. `bench/perf-compare.sh` copies the
head's `bench/dune`, `bench/perf_work.re` and `bench/hazel_bench.re` into the
base checkout and builds each executable there and in the head; both sides run
on the head's programs. If the head's harness does not compile against the base
(say, a base older than an API it calls), that measurement is reported as
unavailable for this base, and the rest of the report still runs.

Because the harness is compiled by `make ci-check` (`dune build @check
--profile release` covers `bench/`), a change that breaks it fails `Static
checks` and has to fix it in the same PR.

### Running it locally

Compare your checkout against `dev` the way CI does (a few minutes, mostly the
two builds):

```sh
bench/perf-compare.sh origin/dev
```

It checks the base out as a worktree under `_perf/base` (git-ignored), prints
the report and leaves it in `_perf/report/`. `--rounds` and `--reps` trade time
for precision; `--out DIR` puts the report elsewhere. An existing checkout can
stand in for the ref.

Or run one harness on your checkout alone:

```sh
dune build --profile release ./bench/perf_work.bc.js ./bench/hazel_bench.bc.js
node --stack-size=8192 _build/default/bench/perf_work.bc.js hazel-programs bench/eval-bench.hz
node --expose-gc --stack-size=8192 _build/default/bench/hazel_bench.bc.js \
  --reps 10 --eval-bench bench/eval-bench.hz [--filter let500/edit]
```

`perf_work` logs each program's step count and writes them all as JSON to
stdout, or to `--out FILE`; `hazel_bench` writes median milliseconds per
benchmark as JSON the same way.
`bench/perf_report.py` is the comparison and rendering, standard-library Python.

### When the report flags something

- **Work counts went up:** the evaluator does more steps on those programs. If
  it is intended (a semantics change, say), say so in the PR. If the budget
  programs moved too, `Test_PerfBudgets` will already have asked you to raise
  a budget with the reason in the commit.
- **A timing went up:** check it locally with more rounds
  (`bench/perf-compare.sh --rounds 10 origin/dev`) before chasing it, then
  iterate on that one benchmark with `hazel_bench`'s `--filter`.
- **Something got faster:** good. If it is a budget program, lower the budget
  (`Test_PerfBudgets` will insist).
