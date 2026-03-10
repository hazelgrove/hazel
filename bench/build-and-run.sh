#!/usr/bin/env bash
# Build and run benchmarks, outputting JSON to stdout.
# Shared by both the local comparison script and the CI workflow.
#
# Usage:
#   bench/build-and-run.sh                              # all benchmarks
#   bench/build-and-run.sh --filter Insert+Full         # filtered
#   bench/build-and-run.sh --filter memo --filter let500

set -euo pipefail

eval $(opam env) 2>/dev/null || true
export OPAMYES=1

echo "==> Installing dependencies" >&2
opam install . --deps-only --locked -q >&2

echo "==> Building benchmarks" >&2
dune build bench/hazel_bench.bc.js >&2

echo "==> Running benchmarks" >&2
# The harness writes JSON directly to stdout; progress goes to stderr.
if ! node --stack-size=8192 _build/default/bench/hazel_bench.bc.js --json "$@"; then
  echo "==> Warning: benchmark process failed" >&2
  echo "[]"
fi
