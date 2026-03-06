#!/usr/bin/env bash
# Run benchmarks on current branch and a base branch, then compare.
#
# Usage:
#   bench/run-comparison.sh              # compares against dev
#   bench/run-comparison.sh main         # compares against main
#   bench/run-comparison.sh abc123       # compares against a specific commit
#
# The current branch's benchmark code (bench/) is used for both branches,
# matching the GitHub Actions /perf workflow behavior.

set -euo pipefail

BASE_REF="${1:-dev}"
RESULTS_DIR=$(mktemp -d)
BENCH_DIR=$(mktemp -d)
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
COPIED_BENCH=false

# Clean up on exit (normal or error) to always restore head branch state
cleanup() {
  echo "==> Cleaning up"
  # Remove copied bench/ from worktree if we added it
  if [ "$COPIED_BENCH" = true ]; then
    rm -rf bench/
  fi
  # Restore original branch
  if [ -n "${CURRENT_BRANCH:-}" ]; then
    git checkout -q "$CURRENT_BRANCH" 2>/dev/null || git checkout -q "$CURRENT_SHA"
  elif [ -n "${CURRENT_SHA:-}" ]; then
    git checkout -q "$CURRENT_SHA"
  fi
  git stash pop -q 2>/dev/null || true
  # Ensure head branch deps are current
  if [ -n "${CURRENT_SHA:-}" ]; then
    echo "==> Ensuring head branch dependencies are up to date"
    opam install . --deps-only --locked -q 2>/dev/null || true
  fi
  rm -rf "$BENCH_DIR" "$RESULTS_DIR"
}
trap cleanup EXIT

echo "==> Saving benchmark harness from current branch"
cp -r "$SCRIPT_DIR" "$BENCH_DIR/bench"

echo "==> Building and running benchmarks on current branch (head)"
opam install . --deps-only --locked -q
dune build bench/hazel_bench.bc.js
node --stack-size=8192 _build/default/bench/hazel_bench.bc.js --json 2>&1 \
  | sed -n '/^\[/,/^\]/p' > "$RESULTS_DIR/head.json"

HEAD_SHA=$(git rev-parse --short HEAD)
CURRENT_BRANCH=$(git branch --show-current)
CURRENT_REF="${CURRENT_BRANCH:-HEAD}"
CURRENT_SHA=$(git rev-parse HEAD)

# Resolve to SHA so we can checkout as detached HEAD.
# This avoids conflicts with branches checked out in other worktrees.
BASE_SHA_FULL=$(git rev-parse "$BASE_REF")

echo "==> Checking out base ($BASE_REF @ ${BASE_SHA_FULL:0:7}) as detached HEAD"
git stash --include-untracked -q 2>/dev/null || true
git checkout -q "$BASE_SHA_FULL"

# Copy benchmark harness if base doesn't have one
if [ ! -f bench/hazel_bench.re ]; then
  echo "==> Base doesn't have bench/ — copying from head"
  cp -r "$BENCH_DIR/bench" bench
  COPIED_BENCH=true
fi

echo "==> Installing base branch dependencies"
opam install . --deps-only --locked -q

echo "==> Building and running benchmarks on base ($BASE_REF)"
dune build bench/hazel_bench.bc.js
node --stack-size=8192 _build/default/bench/hazel_bench.bc.js --json 2>&1 \
  | sed -n '/^\[/,/^\]/p' > "$RESULTS_DIR/base.json"

BASE_SHA=$(git rev-parse --short HEAD)

# cleanup trap handles: removing copied bench/, restoring branch,
# popping stash, reinstalling head deps

echo ""
echo "Base: $BASE_REF @ $BASE_SHA"
echo "Head: $CURRENT_REF @ $HEAD_SHA"
node bench/compare.js "$RESULTS_DIR/base.json" "$RESULTS_DIR/head.json"
