#!/usr/bin/env bash
# SPIKE (wasm-eval-bench): build the opam switch that can compile the
# evaluator benchmark to Wasm.
#
# Why a separate switch: virtual_dom v0.16 requires js_of_ocaml < 6.0.0,
# and wasm_of_ocaml is only released from 6.0.1 onward. The two cannot
# coexist. This switch therefore has NO Bonsai/virtual_dom in it, which is
# exactly why bench/wasm links only [language] + [menhirParser].
#
# Your default Hazel switch is untouched.
set -euo pipefail

SWITCH="${1:-hazel-wasm}"

# Why 6.2.0 and not the latest 6.4.1: js_of_ocaml-compiler >= 6.3.0 requires
# cmdliner >= 2.0, while Hazel pins uuidm = 0.9.8 (0.9.9 has breaking
# deprecated changes), and uuidm 0.9.8 builds with topkg, which caps
# cmdliner < 2.0. 6.2.0 needs only cmdliner >= 1.1.0, so it keeps the uuidm
# pin intact. Both backends still come from one compiler version, which is
# what the comparison actually requires.
JSOO=6.2.0

if ! opam switch list --short | grep -qx "$SWITCH"; then
  opam switch create "$SWITCH" 5.2.0 --no-switch
fi

opam install --switch="$SWITCH" -y \
  dune menhir reason \
  yojson ppx_yojson_conv ppx_yojson_conv_lib ppx_blob ppx_deriving \
  ppx_sexp_conv ppx_enumerate ppx_variants_conv variantslib bisect_ppx \
  ptmap uuidm.0.9.8 unionFind bignum csv \
  qcheck qcheck-alcotest ppx_deriving_qcheck \
  "js_of_ocaml.$JSOO" "js_of_ocaml-compiler.$JSOO" "js_of_ocaml-ppx.$JSOO" \
  "wasm_of_ocaml-compiler.$JSOO"

echo
echo "Switch '$SWITCH' ready. Now run:  bench/wasm/run.sh $SWITCH"
