#!/usr/bin/env bash
# Build and run benchmarks, outputting JSON to stdout.
# Shared by both the local comparison script and the CI workflow.
#
# Usage:
#   bench/build-and-run.sh                    # from repo root
#   cd some-dir && path/to/build-and-run.sh   # from any checkout

set -euo pipefail

eval $(opam env) 2>/dev/null || true
export OPAMYES=1

echo "==> Installing dependencies" >&2
opam install . --deps-only --locked -q

echo "==> Building benchmarks" >&2
dune build bench/hazel_bench.bc.js

echo "==> Running benchmarks" >&2
node --stack-size=8192 _build/default/bench/hazel_bench.bc.js --json 2>&1 \
  | sed -n '/^\[/,/^\]/p'
