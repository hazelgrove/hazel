#!/usr/bin/env bash
# Run benchmarks on two commits and compare the results.
#
# Usage:
#   bench/run-and-compare.sh                          # HEAD vs dev
#   bench/run-and-compare.sh --base main              # HEAD vs main
#   bench/run-and-compare.sh --base abc123            # HEAD vs specific commit
#   bench/run-and-compare.sh --head my-branch --base dev
#   bench/run-and-compare.sh --filter let100          # pass filter to benchmarks
#
# For non-HEAD commits, uses a git worktree to build and run benchmarks
# without touching the current working tree.

set -euo pipefail

usage() {
  cat <<'HELP'
Usage: bench/run-and-compare.sh [OPTIONS]

Run benchmarks on two commits and compare the results.
Uses git worktrees for non-HEAD commits.

Options:
  --head REF           Head commit ref (default: HEAD)
  --base REF           Base commit ref (default: dev)
  --filter PATTERN     Filter benchmarks by name substring (repeatable)
  -h, --help           Show this help message
HELP
  exit 0
}

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
HEAD_REF="HEAD"
BASE_REF="dev"
FILTER_ARGS=()

while [[ $# -gt 0 ]]; do
  case "$1" in
    -h|--help) usage ;;
    --head)  HEAD_REF="${2:?--head requires a ref}"; shift 2 ;;
    --base)  BASE_REF="${2:?--base requires a ref}"; shift 2 ;;
    --filter) FILTER_ARGS+=("--filter" "${2:?--filter requires a pattern}"); shift 2 ;;
    --*) echo "Unknown option: $1" >&2; exit 1 ;;
    *) echo "Unknown argument: $1" >&2; exit 1 ;;
  esac
done

HEAD_SHA=$(git rev-parse "$HEAD_REF" 2>/dev/null) || {
  echo "Error: cannot resolve '$HEAD_REF' to a commit." >&2
  exit 1
}
BASE_SHA=$(git rev-parse "$BASE_REF" 2>/dev/null) || {
  echo "Error: cannot resolve '$BASE_REF' to a commit." >&2
  echo "  Try: git fetch origin $BASE_REF" >&2
  exit 1
}

HEAD_SHORT=$(git rev-parse --short "$HEAD_SHA")
BASE_SHORT=$(git rev-parse --short "$BASE_SHA")

run_in_worktree() {
  local sha="$1"
  local short="$2"
  local label="$3"
  shift 3

  local wt_dir
  wt_dir=$(mktemp -d)
  echo "==> Creating worktree for $label ($short) at $wt_dir" >&2
  git worktree add "$wt_dir" "$sha" --detach --quiet

  # Copy bench/ into the worktree if it doesn't have one
  if [ ! -d "$wt_dir/bench" ]; then
    cp -r "$SCRIPT_DIR" "$wt_dir/bench"
  fi

  (
    cd "$wt_dir"
    bash bench/run.sh --quiet ${FILTER_ARGS[@]+"${FILTER_ARGS[@]}"}
  )

  git worktree remove "$wt_dir" --force 2>/dev/null || true
}

# --- Run benchmarks on HEAD ---
CURRENT_SHA=$(git rev-parse HEAD)
if [ "$HEAD_SHA" = "$CURRENT_SHA" ]; then
  echo "==> Running benchmarks on HEAD ($HEAD_SHORT)" >&2
  "$SCRIPT_DIR/run.sh" --quiet ${FILTER_ARGS[@]+"${FILTER_ARGS[@]}"}
else
  run_in_worktree "$HEAD_SHA" "$HEAD_SHORT" "head"
fi

# --- Run benchmarks on base ---
run_in_worktree "$BASE_SHA" "$BASE_SHORT" "base"

# --- Compare stored results ---
echo "" >&2
"$SCRIPT_DIR/compare.sh" "$BASE_REF" "$HEAD_REF"
