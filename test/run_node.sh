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

if [ -z "$IDB_STUB" ] || [ -z "$TEST_JS" ]; then
  BUILD_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../_build/default/test" && pwd)"
  : "${IDB_STUB:=$BUILD_DIR/idb_stub.js}"
  : "${TEST_JS:=$BUILD_DIR/haz3ltest.bc.js}"
fi

exec node --stack-size=8192 --require "$IDB_STUB" "$TEST_JS" "$@"
