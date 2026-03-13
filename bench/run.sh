#!/usr/bin/env bash
# Build, run benchmarks, store results as a git note, and display a table.
#
# Usage:
#   bench/run.sh                    # run, store, and display table
#   bench/run.sh --quiet            # run and store only (no table)
#   bench/run.sh --filter let100    # filtered run
#
# Results are stored as git notes under refs/notes/benchmarks.
# Retrieve with: git notes --ref=benchmarks show <sha>

set -euo pipefail

QUIET=false
BENCH_ARGS=()
while [[ $# -gt 0 ]]; do
  case "$1" in
    --quiet) QUIET=true; shift ;;
    *) BENCH_ARGS+=("$1"); shift ;;
  esac
done

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SHA=$(git rev-parse HEAD)
SHORT_SHA=$(git rev-parse --short HEAD)

echo "==> Running benchmarks for $SHORT_SHA" >&2
RESULTS=$("$SCRIPT_DIR/build-and-run.sh" ${BENCH_ARGS[@]+"${BENCH_ARGS[@]}"})

if [ -z "$RESULTS" ] || [ "$RESULTS" = "[]" ]; then
  echo "==> No benchmark results to store" >&2
  exit 1
fi

echo "$RESULTS" | git notes --ref=benchmarks add -f -F - "$SHA"
echo "==> Stored benchmark results for $SHORT_SHA" >&2

if [ "$QUIET" != true ]; then
  echo "$RESULTS" | node "$SCRIPT_DIR/format-table.js"
fi
