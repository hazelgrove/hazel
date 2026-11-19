#!/usr/bin/env bash
# The performance report, locally or in CI (.github/workflows/perf-report.yml;
# docs/performance.md): build this checkout's measurements against a base and
# against this checkout, run both, and compare.
#
#   bench/perf-compare.sh [--rounds N] [--reps N] [--timeout SECONDS]
#                         [--out DIR] BASE
#
# BASE is a git ref (e.g. origin/dev), checked out as a worktree under
# _perf/base, or an existing checkout directory (CI checks the base out
# itself). The measurement code -- bench/dune, perf_work.re, hazel_bench.re --
# is this checkout's, copied into the base before building it, so both sides
# are measured the same way. The programs are this checkout's too.
#
# If the head's harness does not build against the base (a PR that changes an
# API the harness calls, and updates the harness to match), the base's own
# harness is built instead, when it has one, and the report says so. If
# neither builds, or the base predates bench/ altogether, that measurement is
# reported as unavailable for this base; not being able to measure the base
# is never a failure of this script.
#
# A BASE directory's own bench/ files are put back as they were on exit, so
# an existing checkout can be used as the base (its _build gains the
# release-profile bundles).
#
# Run from anywhere, with the opam switch Hazel builds with. Leaves the report
# in OUT (default _perf/report): report.md, report.json, significant, and the
# build logs.
set -euo pipefail

ROUNDS=5
REPS=5
TIMEOUT=900
HEAD="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
OUT="$HEAD/_perf/report"
while [ $# -gt 0 ]; do
  case "$1" in
    --rounds) ROUNDS="$2"; shift 2 ;;
    --reps) REPS="$2"; shift 2 ;;
    --timeout) TIMEOUT="$2"; shift 2 ;;
    --out) OUT="$2"; shift 2 ;;
    -*) echo "unknown option $1" >&2; exit 2 ;;
    *) break ;;
  esac
done
if [ $# -ne 1 ]; then
  echo "usage: $0 [--rounds N] [--reps N] [--timeout SECONDS] [--out DIR] BASE" >&2
  exit 2
fi
mkdir -p "$OUT"
OUT="$(cd "$OUT" && pwd)"

# _perf, like _build, starts with an underscore, so dune does not treat a base
# checkout inside it as part of this workspace.
if [ -d "$1" ]; then
  BASE="$(cd "$1" && pwd)"
  if [ "$BASE" = "$HEAD" ]; then
    echo "$0: BASE is this checkout; give the base as a ref or another directory" >&2
    exit 2
  fi
else
  BASE="$HEAD/_perf/base"
  if [ -e "$BASE" ]; then
    git -C "$HEAD" worktree remove --force "$BASE" 2>/dev/null || rm -rf "$BASE"
  fi
  git -C "$HEAD" worktree add --detach "$BASE" "$1"
fi

HARNESS="dune perf_work.re hazel_bench.re"

# The base's own harness, if it has one (none before this report existed):
# kept to fall back on, and to put back on exit.
OWN="$OUT/base-own-harness"
rm -rf "$OWN"
mkdir -p "$OWN"
CREATED_BENCH=false
if [ ! -d "$BASE/bench" ]; then
  # A base from before bench/ existed: the head's harness still goes in.
  mkdir -p "$BASE/bench"
  CREATED_BENCH=true
fi
for f in $HARNESS; do
  if [ -f "$BASE/bench/$f" ]; then cp -p "$BASE/bench/$f" "$OWN/"; fi
done
own_harness() { [ -f "$OWN/dune" ] && [ -f "$OWN/$1.re" ]; }
BASE_HAS_HARNESS=false
if own_harness perf_work || own_harness hazel_bench; then BASE_HAS_HARNESS=true; fi

use_own_harness() {
  for f in $HARNESS; do
    if [ -f "$OWN/$f" ]; then cp -p "$OWN/$f" "$BASE/bench/$f"; else rm -f "$BASE/bench/$f"; fi
  done
}
restore_base() {
  use_own_harness
  if $CREATED_BENCH; then rmdir "$BASE/bench" 2>/dev/null || true; fi
}
trap restore_base EXIT

# Each bundle on its own, so one that does not compile leaves the other, and
# copied out of _build as soon as it is built, so a later build in the same
# checkout (the base's own harness) cannot replace or remove it. Stale
# bundles are removed first so a failed build cannot report an old one.
# opam runs from the head checkout, where its switch is found (CI's is local
# to the workspace); --root keeps dune from climbing out of the base checkout
# into an enclosing workspace.
rm -rf "$OUT/bundles"
mkdir -p "$OUT/bundles/base" "$OUT/bundles/head"
build() {
  local side="$1" dir="$2" target="$3" log_name="$4"
  local log="$OUT/build-$log_name-$target.log"
  rm -f "$dir/_build/default/bench/$target.bc.js"
  if (cd "$HEAD" && opam exec -- dune build --root "$dir" --profile release \
        "./bench/$target.bc.js" >"$log" 2>&1); then
    cp "$dir/_build/default/bench/$target.bc.js" "$OUT/bundles/$side/"
  else
    echo "bench/$target.bc.js did not build on the $log_name:" >&2
    tail -40 "$log" >&2
    return 1
  fi
}

# Whose harness each base bundle is: head, base (its own) or none.
WORK_HARNESS=none
TIME_HARNESS=none
set_harness() {
  case "$1" in
    perf_work) WORK_HARNESS="$2" ;;
    hazel_bench) TIME_HARNESS="$2" ;;
  esac
}

for f in $HARNESS; do cp "$HEAD/bench/$f" "$BASE/bench/"; done
FAILED=""
for target in perf_work hazel_bench; do
  if build base "$BASE" "$target" base; then
    set_harness "$target" head
  else
    FAILED="$FAILED $target"
  fi
  build head "$HEAD" "$target" head ||
    echo "::warning::bench/$target did not build on the head; the report says so"
done

if [ -n "$FAILED" ]; then
  use_own_harness
  for target in $FAILED; do
    if own_harness "$target" && build base "$BASE" "$target" base-own; then
      echo "::notice::the head's bench/$target.re does not build on the base; measured the base with its own"
      set_harness "$target" base
    else
      echo "::warning::bench/$target could not be built for the base; the report says so"
    fi
  done
fi

label() { git -C "$1" rev-parse --short HEAD 2>/dev/null || basename "$1"; }

# Programs as paths relative to the head checkout, so they read the same in
# both sides' results.
cd "$HEAD"
own_flag=()
if $BASE_HAS_HARNESS; then own_flag=(--base-has-harness); fi
python3 bench/perf_report.py \
  --base-bundles "$OUT/bundles/base" --head-bundles "$OUT/bundles/head" \
  --base-work-harness "$WORK_HARNESS" --base-time-harness "$TIME_HARNESS" \
  ${own_flag[@]+"${own_flag[@]}"} \
  --programs hazel-programs bench/eval-bench.hz \
  --eval-bench bench/eval-bench.hz \
  --rounds "$ROUNDS" --reps "$REPS" --timeout "$TIMEOUT" \
  --base-label "$(label "$BASE")" --head-label "$(label "$HEAD")" \
  --out "$OUT"
