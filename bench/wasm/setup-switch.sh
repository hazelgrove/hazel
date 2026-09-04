#!/usr/bin/env bash
# SPIKE (wasm-eval-bench): build the opam switch that can compile the
# evaluator benchmark to Wasm.
#
# Why a separate switch: virtual_dom v0.16 requires js_of_ocaml < 6.0.0, and
# wasm_of_ocaml is only released from 6.0.1 onward. The two cannot coexist.
# This switch therefore has NO Bonsai/virtual_dom in it, which is exactly why
# bench/wasm links only [language] + [menhirParser].
#
# Your default Hazel switch is untouched (--no-switch).
#
# PREREQUISITE (system packages, needs sudo):
#
#     sudo apt-get install cmake ninja-build
#
# wasm_of_ocaml depends on binaryen-bin, which compiles binaryen from source
# and needs cmake, ninja and a C++ compiler. Expect that build to dominate
# the wall-clock time here.
#
# Notes on the version pins below, all of them load-bearing:
#
#   * js_of_ocaml 6.2.0, not the latest 6.4.1: js_of_ocaml-compiler >= 6.3.0
#     requires cmdliner >= 2.0, while Hazel pins uuidm = 0.9.8 (0.9.9 has
#     breaking deprecated changes) and uuidm 0.9.8 builds with topkg, which
#     caps cmdliner < 2.0. So uuidm transitively bounds how new a
#     js_of_ocaml Hazel can use.
#   * The archive opam repo is required (as in `make deps`) for
#     ppx_deriving_qcheck.0.6, the only version compatible with ppxlib 0.35.
#   * Versions are pinned to whatever the main Hazel switch resolved to.
#     Left to float, the solver walks the Jane Street packages backwards
#     toward OCaml 4.10 and reports a spurious conflict.
#   * bisect_ppx is deliberately absent: it is only needed under
#     `--instrument-with`, and unconstrained it drags in a pre-2.x release
#     that caps ocaml < 5.0.
set -euo pipefail

SWITCH="${1:-hazel-wasm}"
JSOO=6.2.0

for tool in cmake ninja; do
  command -v "$tool" >/dev/null 2>&1 || {
    echo "ERROR: '$tool' not found. Run: sudo apt-get install cmake ninja-build" >&2
    exit 1
  }
done

if ! opam switch list --short | grep -qx "$SWITCH"; then
  opam switch create "$SWITCH" 5.2.0 --no-switch
fi

opam repo list --switch="$SWITCH" | grep -q '^ *[0-9]* archive ' || \
  opam repo add archive git+https://github.com/ocaml/opam-repository-archive \
    --switch="$SWITCH"

opam install --switch="$SWITCH" -y \
  dune.3.19.1 menhir.20260209 reason.3.15.0 yojson.2.2.2 \
  ppx_yojson_conv.v0.16.0 ppx_yojson_conv_lib.v0.16.0 ppx_blob.0.9.0 \
  ppx_deriving.6.0.3 ppx_sexp_conv.v0.16.0 ppx_enumerate.v0.16.0 \
  ppx_variants_conv.v0.16.0 variantslib.v0.16.0 ppxlib.0.35.0 \
  ptmap.2.0.5 uuidm.0.9.8 unionFind.20250818 csv.2.4 bignum.v0.16.0 \
  qcheck.0.25 qcheck-alcotest.0.25 ppx_deriving_qcheck.0.6 \
  "js_of_ocaml.$JSOO" "js_of_ocaml-compiler.$JSOO" "js_of_ocaml-ppx.$JSOO" \
  "wasm_of_ocaml-compiler.$JSOO"

echo
echo "Switch '$SWITCH' ready. Now run:  bench/wasm/run.sh $SWITCH"
