#!/usr/bin/env bash

# Deterministic QCheck runs by default so random-draw findings don't
# red the suite mid-work; hunt new counterexamples explicitly with
# QCHECK_SEED=<n> sweeps (see Test_RoundtripFuzz header).
export QCHECK_SEED="${QCHECK_SEED:-42}"
# Shared wrapper for running the test JS with the correct node flags.
# Extra arguments are forwarded to the test runner.
#
# When IDB_STUB and TEST_JS env vars are set (by dune), uses those paths.
# Otherwise resolves paths from _build/default/test/.
#
# Node.js flags:
#  --stack-size=8192: increase stack to 8MB for deeply recursive tests
#    (e.g., Pattern Coverage Checker)
#  --require idb_stub.js: provide minimal IndexedDB globals for Node.js
#    (Ezjs_idb captures IDBKeyRange at module init time)
#
# What --stack-size hides
#
# A browser gives the page about 1MB of JS stack, and a Web Worker (where
# the evaluator runs, see src/web/util/WorkerServer.re) less; neither is
# adjustable. So 8MB here hides any recursion that fits in 8MB but
# overflows in the editor.
#
# It can't be lowered to browser size: alcotest walks the test list with
# a non-tail-recursive bind, so the runner itself overflows below about
# 2048 (it fails at 1536), whatever NAME_REGEX you pass. Pattern Coverage
# passes at 2048, so 2048 is the lowest this could go, but that leaves no
# headroom for new tests; trying it needs a full suite run.
#
# To check browser-stack behaviour, use the CLI, which runs the same
# parse -> statics -> elaborate -> evaluate pipeline without alcotest:
#
#   dune build src/CLI/cli.bc.js --profile dev
#   node --stack-size=984 --require src/CLI/polyfill.js \
#     _build/default/src/CLI/cli.bc.js run docs/stlc-progress-example.hazel
#
# Walk --stack-size down to find the headroom; node prints the top frames
# of the RangeError, which name the recursion.

if [ -z "$IDB_STUB" ] || [ -z "$TEST_JS" ]; then
  BUILD_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../_build/default/test" && pwd)"
  : "${IDB_STUB:=$BUILD_DIR/idb_stub.js}"
  : "${TEST_JS:=$BUILD_DIR/haz3ltest.bc.js}"
fi

exec node --stack-size=8192 --require "$IDB_STUB" "$TEST_JS" "$@"
