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
# WHAT --stack-size MASKS, AND WHY IT STAYS
#
# A browser gives the page ~1MB of JS stack and a Web Worker (where the
# evaluator actually runs, see src/web/util/WorkerServer.re) less than
# that, and neither is adjustable. So 8MB here hides an entire bug class:
# a pipeline stage whose recursion depth is fine at 8MB and overflows in
# the editor. docs/stlc-progress-example.hazel was exactly that — its
# evaluation needed ~500KB of stack, all of it a full `Statics.mk` run
# nested inside proof checking (fixed in ProofRule.mentions_any).
#
# The flag cannot simply be lowered to browser size to expose that class,
# because this binary needs a big stack for reasons of its own: alcotest
# walks the test list with a non-tail-recursive monad bind, so the runner
# overflows inside its own iteration — before any Hazel code runs — no
# matter which NAME_REGEX you pass. Measured floor for the harness alone:
# overflows at 1536, passes at 2048. So 2048 is the lowest this could go,
# it sits directly on that floor (adding tests would red the suite for a
# harness reason), and it is still twice a browser worker. Pattern
# Coverage, the other reason 8192 was chosen, does pass at 2048 — if you
# want to tighten the mask, that is the experiment, and it needs a FULL
# suite run, not a targeted one.
#
# TO CHECK BROWSER-STACK BEHAVIOR, use the CLI instead of the suite — it
# runs the same parse -> statics -> elaborate -> evaluate pipeline with no
# alcotest harness underneath, so its stack demand is the program's:
#
#   dune build src/CLI/cli.bc.js --profile dev
#   node --stack-size=984 --require src/CLI/polyfill.js \
#     _build/default/src/CLI/cli.bc.js run docs/stlc-progress-example.hazel
#
# Bisect the headroom by walking --stack-size down; node prints the top
# frames of the RangeError, which name the guilty recursion.

if [ -z "$IDB_STUB" ] || [ -z "$TEST_JS" ]; then
  BUILD_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../_build/default/test" && pwd)"
  : "${IDB_STUB:=$BUILD_DIR/idb_stub.js}"
  : "${TEST_JS:=$BUILD_DIR/haz3ltest.bc.js}"
fi

exec node --stack-size=8192 --require "$IDB_STUB" "$TEST_JS" "$@"
