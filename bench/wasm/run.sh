#!/usr/bin/env bash
# SPIKE (wasm-eval-bench): build the evaluator benchmark with BOTH backends
# from the SAME compiler version, run each on node 22, print both results.
set -euo pipefail

SWITCH="${1:-hazel-wasm}"
ITERS="${2:-20}"
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$ROOT"

export NVM_DIR="$HOME/.nvm"
# shellcheck disable=SC1091
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"
NODE="$(nvm which 22)"
"$NODE" --version

eval "$(opam env --switch="$SWITCH" --set-switch)"

# Enable the wasm mode only inside this switch; restored on exit so the
# committed dune stays green in the ordinary Hazel switch.
cleanup() { sed -i 's/^ (modes js wasm)$/ (modes js)/' bench/wasm/dune; }
trap cleanup EXIT
sed -i 's/^ (modes js)$/ (modes js wasm)/' bench/wasm/dune

dune build bench/wasm/eval_bench.bc.js      --profile release
dune build bench/wasm/eval_bench.bc.wasm.js --profile release

echo "=== js_of_ocaml ==="
"$NODE" _build/default/bench/wasm/eval_bench.bc.js "$ITERS"
echo "=== wasm_of_ocaml ==="
"$NODE" _build/default/bench/wasm/eval_bench.bc.wasm.js "$ITERS"
