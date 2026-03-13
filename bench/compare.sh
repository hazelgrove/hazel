#!/usr/bin/env bash
# Compare stored benchmark results for two commits.
#
# Usage:
#   bench/compare.sh                          # compare dev vs HEAD
#   bench/compare.sh abc123 def456            # compare two specific commits
#   bench/compare.sh dev HEAD --markdown      # GitHub markdown output
#
# Looks up git notes (refs/notes/benchmarks) for each commit.
# If no stored results exist for a commit, exits with instructions.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
BASE_REF="dev"
HEAD_REF="HEAD"
COMPARE_ARGS=()

positional=()
while [[ $# -gt 0 ]]; do
  case "$1" in
    --markdown) COMPARE_ARGS+=("--markdown"); shift ;;
    --*) echo "Unknown option: $1" >&2; exit 1 ;;
    *) positional+=("$1"); shift ;;
  esac
done

if [[ ${#positional[@]} -ge 2 ]]; then
  BASE_REF="${positional[0]}"
  HEAD_REF="${positional[1]}"
elif [[ ${#positional[@]} -eq 1 ]]; then
  BASE_REF="${positional[0]}"
fi

# Resolve refs to SHAs
BASE_SHA=$(git rev-parse "$BASE_REF" 2>/dev/null) || {
  echo "Error: cannot resolve '$BASE_REF' to a commit." >&2
  echo "  Try: git fetch origin $BASE_REF" >&2
  exit 1
}
HEAD_SHA=$(git rev-parse "$HEAD_REF" 2>/dev/null) || {
  echo "Error: cannot resolve '$HEAD_REF' to a commit." >&2
  exit 1
}

BASE_SHORT=$(git rev-parse --short "$BASE_SHA")
HEAD_SHORT=$(git rev-parse --short "$HEAD_SHA")

RESULTS_DIR=$(mktemp -d)
trap 'rm -rf "$RESULTS_DIR"' EXIT

# Look up stored results
if ! git notes --ref=benchmarks show "$BASE_SHA" > "$RESULTS_DIR/base.json" 2>/dev/null; then
  echo "No stored benchmark results for $BASE_REF ($BASE_SHORT)." >&2
  echo "" >&2
  echo "To generate them:" >&2
  echo "  git checkout $BASE_SHORT && bench/run.sh && git checkout -" >&2
  exit 1
fi

if ! git notes --ref=benchmarks show "$HEAD_SHA" > "$RESULTS_DIR/head.json" 2>/dev/null; then
  echo "No stored benchmark results for $HEAD_REF ($HEAD_SHORT)." >&2
  echo "" >&2
  echo "To generate them:" >&2
  echo "  bench/run.sh    # (if $HEAD_REF is HEAD)" >&2
  exit 1
fi

echo "Base: $BASE_REF @ $BASE_SHORT" >&2
echo "Head: $HEAD_REF @ $HEAD_SHORT" >&2
echo "" >&2

node "$SCRIPT_DIR/compare.js" "$RESULTS_DIR/base.json" "$RESULTS_DIR/head.json" ${COMPARE_ARGS[@]+"${COMPARE_ARGS[@]}"}
