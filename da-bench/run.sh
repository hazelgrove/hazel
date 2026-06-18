#!/usr/bin/env bash
# Run a DA-Bench solution by concatenating the shared prelude in front of it
# (the `hazel run` path has no import mechanism, so we splice helpers in here).
#
# Usage: da-bench/run.sh daN-foo.hz [extra ./hazel run args]
#   TABLES=/path/to/da-dev-tables overrides the default data dir.
set -euo pipefail
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TABLES="${TABLES:-$HOME/Projects/InfiAgent/examples/DA-Agent/data/da-dev-tables}"
sol="$1"; shift || true
tmp="$(mktemp)"
trap 'rm -f "$tmp"' EXIT
cat "$DIR/prelude.hz" "$DIR/$sol" >"$tmp"
"$DIR/../hazel" run "$tmp" --data-dir "$TABLES" --yes "$@"
