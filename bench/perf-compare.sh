#!/usr/bin/env bash
# The performance report, locally or in CI (.github/workflows/perf-report.yml;
# docs/performance.md): build this checkout's measurements against a base and
# against this checkout, run both, and compare.
#
#   bench/perf-compare.sh [--rounds N] [--reps N] [--out DIR] BASE
#
# BASE is a git ref (e.g. origin/dev), checked out as a worktree under
# _perf/base, or an existing checkout directory (CI checks the base out
# itself). The measurement code -- bench/dune, perf_work.re, hazel_bench.re --
# is always this checkout's, copied into the base before building it, so both
# sides are measured the same way. The programs are this checkout's too.
#
# A side whose harness does not build is reported as unavailable, not as a
# failure: e.g. a base older than Evaluator.evaluate_counting_work.
#
# Run from anywhere, with the opam switch Hazel builds with. Leaves the report
# in OUT (default _perf/report): report.md, report.json, significant.
set -euo pipefail

ROUNDS=5
REPS=5
HEAD="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
OUT="$HEAD/_perf/report"
while [ $# -gt 0 ]; do
  case "$1" in
    --rounds) ROUNDS="$2"; shift 2 ;;
    --reps) REPS="$2"; shift 2 ;;
    --out) OUT="$2"; shift 2 ;;
    -*) echo "unknown option $1" >&2; exit 2 ;;
    *) break ;;
  esac
done
if [ $# -ne 1 ]; then
  echo "usage: $0 [--rounds N] [--reps N] [--out DIR] BASE" >&2
  exit 2
fi

# _perf, like _build, starts with an underscore, so dune does not treat a base
# checkout inside it as part of this workspace.
if [ -d "$1" ]; then
  BASE="$(cd "$1" && pwd)"
else
  BASE="$HEAD/_perf/base"
  if [ -e "$BASE" ]; then
    git -C "$HEAD" worktree remove --force "$BASE" 2>/dev/null || rm -rf "$BASE"
  fi
  git -C "$HEAD" worktree add --detach "$BASE" "$1"
fi

cp "$HEAD/bench/dune" "$HEAD/bench/perf_work.re" "$HEAD/bench/hazel_bench.re" \
  "$BASE/bench/"

# Each bundle on its own, so one that does not compile leaves the other. Stale
# bundles are removed first so a failed build cannot report an old one.
# opam runs from the head checkout, where its switch is found (CI's is local
# to the workspace); --root keeps dune from climbing out of the base checkout
# into an enclosing workspace.
build() {
  local side="$1" dir="$2" target="$3"
  local log="$OUT/build-$side-${target%.bc.js}.log"
  rm -f "$dir/_build/default/bench/$target"
  if ! (cd "$HEAD" && opam exec -- dune build --root "$dir" --profile release \
          "./bench/$target" >"$log" 2>&1); then
    echo "::warning::bench/$target did not build on the $side; the report says so"
    tail -40 "$log" >&2
  fi
}
mkdir -p "$OUT"
for target in perf_work.bc.js hazel_bench.bc.js; do
  build base "$BASE" "$target"
  build head "$HEAD" "$target"
done

label() { git -C "$1" rev-parse --short HEAD 2>/dev/null || basename "$1"; }

# Programs as paths relative to the head checkout, so they read the same in
# both sides' results.
cd "$HEAD"
python3 bench/perf_report.py \
  --base "$BASE" --head "$HEAD" \
  --programs hazel-programs bench/eval-bench.hz \
  --eval-bench bench/eval-bench.hz \
  --rounds "$ROUNDS" --reps "$REPS" \
  --base-label "$(label "$BASE")" --head-label "$(label "$HEAD")" \
  --out "$OUT"
