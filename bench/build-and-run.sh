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
dune build bench/hazel_bench.bc.js >&2

echo "==> Running benchmarks" >&2
# core_bench prints "Estimated testing time..." to stdout before the JSON.
# JS runtime may also print warnings to stdout or stderr.
# Capture all stdout to a temp file, then extract just the JSON array.
# stderr passes through normally for warnings/errors.
TMPOUT=$(mktemp)
if node --stack-size=8192 _build/default/bench/hazel_bench.bc.js --json > "$TMPOUT"; then
  JSON=$(sed -n '/^\[/,/^\]/p' "$TMPOUT")
  if [ -n "$JSON" ]; then
    echo "$JSON"
  else
    echo "==> Warning: no JSON found in benchmark output" >&2
    echo "[]"
  fi
else
  echo "==> Warning: benchmark process failed" >&2
  echo "[]"
fi
rm -f "$TMPOUT"
