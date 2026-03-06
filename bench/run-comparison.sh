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
#
# NOTE: This script stashes uncommitted changes before switching branches.
# If cleanup encounters conflicts (e.g., files deleted on one branch but
# modified on another), it falls back to a hard reset to restore the
# original branch cleanly.

set -euo pipefail

BASE_REF="${1:-dev}"
RESULTS_DIR=$(mktemp -d)
BENCH_DIR=$(mktemp -d)
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
STASHED=false

# Clean up on exit (normal or error) to always restore head branch state
cleanup() {
  echo "==> Cleaning up"

  # Restore original branch
  if [ -n "${CURRENT_BRANCH:-}" ]; then
    git checkout -f -q "$CURRENT_BRANCH" 2>/dev/null || git checkout -f -q "$CURRENT_SHA"
  elif [ -n "${CURRENT_SHA:-}" ]; then
    git checkout -f -q "$CURRENT_SHA"
  fi

  # Try to restore stashed changes; on conflict, hard reset and clean
  if [ "$STASHED" = true ]; then
    if ! git stash pop -q 2>/dev/null; then
      echo "==> Stash pop had conflicts, resetting to clean state" >&2
      git reset --hard -q 2>/dev/null || true
      git clean -fd -q 2>/dev/null || true
      git stash drop -q 2>/dev/null || true
    fi
  fi

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

echo "==> Benchmarking current branch (head)"
"$SCRIPT_DIR/build-and-run.sh" > "$RESULTS_DIR/head.json"

HEAD_SHA=$(git rev-parse --short HEAD)
CURRENT_BRANCH=$(git branch --show-current)
CURRENT_REF="${CURRENT_BRANCH:-HEAD}"
CURRENT_SHA=$(git rev-parse HEAD)

# Resolve to SHA so we can checkout as detached HEAD.
# This avoids conflicts with branches checked out in other worktrees.
BASE_SHA_FULL=$(git rev-parse "$BASE_REF")

echo "==> Checking out base ($BASE_REF @ ${BASE_SHA_FULL:0:7}) as detached HEAD"
if git stash --include-untracked -q 2>/dev/null; then
  # Check if stash actually created an entry (git stash exits 0 even if nothing to stash)
  STASH_SHA=$(git stash list -1 --format='%H' 2>/dev/null || true)
  if [ -n "$STASH_SHA" ]; then
    STASHED=true
  fi
fi
git checkout -q "$BASE_SHA_FULL"

# Copy benchmark harness if base doesn't have one
COPIED_BENCH=false
if [ ! -f bench/hazel_bench.re ]; then
  echo "==> Base doesn't have bench/ — copying from head"
  cp -r "$BENCH_DIR/bench" bench
  COPIED_BENCH=true
fi

echo "==> Benchmarking base ($BASE_REF)"
bench/build-and-run.sh > "$RESULTS_DIR/base.json"

BASE_SHA=$(git rev-parse --short HEAD)

# Remove copied bench/ before switching back to avoid untracked file conflicts
if [ "$COPIED_BENCH" = true ]; then
  rm -rf bench/
fi

# cleanup trap handles: restoring branch, popping stash, reinstalling head deps

echo ""
echo "Base: $BASE_REF @ $BASE_SHA"
echo "Head: $CURRENT_REF @ $HEAD_SHA"
node "$BENCH_DIR/bench/compare.js" "$RESULTS_DIR/base.json" "$RESULTS_DIR/head.json"
