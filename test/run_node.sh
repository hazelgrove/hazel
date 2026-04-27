#!/usr/bin/env bash
# Shared wrapper for running the test JS with the correct node flags.
# Extra arguments are forwarded to the test runner.
#
# When IDB_STUB and TEST_JS env vars are set (by dune), uses those paths.
# Otherwise resolves paths from _build/default/test/ and the source test
# directory. Dune's alias actions provide idb_stub.js via %{dep:...}, but
# `./run_tests` invokes this wrapper outside that sandbox, where the stub is
# not copied into _build/default/test.
#
# Node.js flags:
#  --stack-size=8192: increase stack to 8MB for deeply recursive tests
#    (e.g., Pattern Coverage Checker)
#  --require idb_stub.js: provide minimal IndexedDB globals for Node.js
#    (Ezjs_idb captures IDBKeyRange at module init time)

if [ -z "$IDB_STUB" ] || [ -z "$TEST_JS" ]; then
  SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
  BUILD_DIR="$(cd "$SCRIPT_DIR/../_build/default/test" && pwd)"
  : "${IDB_STUB:=$SCRIPT_DIR/idb_stub.js}"
  : "${TEST_JS:=$BUILD_DIR/haz3ltest.bc.js}"
fi

exec node --stack-size=8192 --require "$IDB_STUB" "$TEST_JS" "$@"
