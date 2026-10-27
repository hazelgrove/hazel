#!/usr/bin/env bash
# Shared wrapper for running the test JS with the correct node flags.
# Extra arguments are forwarded to the test runner.
#
# When IDB_STUB and TEST_JS env vars are set (by dune), uses those paths.
# Otherwise resolves paths from _build/default/test/. Dune's `test/dune`
# uses `(copy_files idb_stub.js)` so the stub is always mirrored to the
# build directory alongside the compiled JS.
#
# Node.js flags:
#  --stack-size=32768: increase stack to 32MB for deeply recursive tests
#    (e.g., Pattern Coverage Checker)
#  --require idb_stub.js: provide minimal IndexedDB globals for Node.js
#    (Ezjs_idb captures IDBKeyRange at module init time)

if [ -z "$IDB_STUB" ] || [ -z "$TEST_JS" ]; then
  BUILD_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../_build/default/test" && pwd)"
  : "${IDB_STUB:=$BUILD_DIR/idb_stub.js}"
  : "${TEST_JS:=$BUILD_DIR/haz3ltest.bc.js}"
fi

ulimit -s 65536 2>/dev/null || true

exec node --stack-size=32768 --require "$IDB_STUB" "$TEST_JS" "$@"
