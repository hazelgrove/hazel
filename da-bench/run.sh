#!/usr/bin/env bash
# Run a DA-Bench solution by concatenating the shared prelude in front of it
# (the `hazel run` path has no import mechanism, so we splice helpers in here).
#
# Usage: da-bench/run.sh daN-foo.hz [extra ./hazel run args]
#   TABLES=/path/to/da-dev-tables overrides the default data dir.
set -euo pipefail
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$DIR/.."
TABLES="${TABLES:-$HOME/Projects/InfiAgent/examples/DA-Agent/data/da-dev-tables}"
sol="$1"; shift || true
tmp="$(mktemp)"
trap 'rm -f "$tmp"' EXIT
cat "$DIR/prelude.hz" "$DIR/$sol" >"$tmp"
# Invoke node directly (instead of ./hazel) so we can raise V8's stack: the now-large
# prelude makes the statics pass recurse deeply enough to overflow node's default ~1 MB
# stack. --stack-size=8192 (the value test/run_node.sh uses) gives it room.
# --max-old-space-size=8192 raises the heap so the big regression tasks (e.g. da363,
# n~16.6k, builds several thousand-leaf gather trees) don't OOM ("Abort trap: 6").
(cd "$ROOT" && dune build ./src/CLI/cli.bc.js)
node --stack-size=8192 --max-old-space-size=8192 -r "$ROOT/src/CLI/polyfill.js" \
  "$ROOT/_build/default/src/CLI/cli.bc.js" run "$tmp" --data-dir "$TABLES" --yes "$@"
