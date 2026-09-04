#!/usr/bin/env bash
# SPIKE (wasm-eval-bench): build the evaluator benchmark with BOTH backends
# from the SAME compiler version, run each on node 22, print both results.
#
# Isolation: this uses its own build dir (_build-wasm) so it does not force
# the shared _build to be rebuilt under a different opam switch, and it
# temporarily rewrites bench/wasm/dune. Prefer running it from a dedicated
# git worktree -- see bench/wasm/README.md -- so concurrent work on other
# branches in the primary checkout is unaffected.
set -euo pipefail

SWITCH="${1:-hazel-wasm}"
ITERS="${2:-20}"
BUILD_DIR="${BUILD_DIR:-_build-wasm}"

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$ROOT"

if [ "$(git rev-parse --git-dir)" = "$(git rev-parse --git-common-dir)" ]; then
  echo "WARNING: running in the primary checkout, not a linked worktree." >&2
  echo "         bench/wasm/dune is edited in place and restored on exit;" >&2
  echo "         switching branches here mid-run will confuse the build." >&2
fi

export NVM_DIR="$HOME/.nvm"
# shellcheck disable=SC1091
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"
NODE="$(nvm which 22)"
echo "node: $("$NODE" --version)"

eval "$(opam env --switch="$SWITCH" --set-switch)"
echo "switch: $SWITCH"
echo "js_of_ocaml:   $(js_of_ocaml --version 2>/dev/null || echo MISSING)"
echo "wasm_of_ocaml: $(wasm_of_ocaml --version 2>/dev/null || echo MISSING)"

# Enable the wasm mode only for this run; restored on exit so the committed
# dune stays green in the ordinary Hazel switch.
# Three in-place edits, all restored on exit:
#   1. enable the wasm mode;
#   2. swap BigIntWasmStub over BigInt, so Bigint no longer needs bignum;
#   3. drop bignum from util's libraries, which takes zarith and the Jane
#      Street Core C-stub surface out of the link entirely.
cleanup() {
  sed -i 's/^ (modes js wasm)$/ (modes js)/' bench/wasm/dune
  [ -f /tmp/hazel-bigint.bak ] && mv /tmp/hazel-bigint.bak src/util/BigInt.re
  [ -f /tmp/hazel-utildune.bak ] && mv /tmp/hazel-utildune.bak src/util/dune
}
trap cleanup EXIT
sed -i 's/^ (modes js)$/ (modes js wasm)/' bench/wasm/dune
cp src/util/BigInt.re /tmp/hazel-bigint.bak
cp src/util/dune /tmp/hazel-utildune.bak
cp src/util/BigIntWasmStub.re src/util/BigInt.re
# bignum also supplied Sexplib transitively (via core), so substitute
# sexplib rather than simply dropping bignum.
sed -i 's/ (libraries ptmap bignum / (libraries ptmap sexplib base /' src/util/dune

dune build --build-dir="$BUILD_DIR" bench/wasm/eval_bench.bc.js      --profile release
dune build --build-dir="$BUILD_DIR" bench/wasm/eval_bench.bc.wasm.js --profile release

echo
ls -l "$BUILD_DIR"/default/bench/wasm/eval_bench.bc.js \
      "$BUILD_DIR"/default/bench/wasm/eval_bench.bc.wasm.js 2>/dev/null \
  | awk '{printf "%12d  %s\n", $5, $9}'

echo
echo "=== js_of_ocaml ==="
"$NODE" "$BUILD_DIR/default/bench/wasm/eval_bench.bc.js" "$ITERS"
echo "=== wasm_of_ocaml ==="
"$NODE" "$BUILD_DIR/default/bench/wasm/eval_bench.bc.wasm.js" "$ITERS"
