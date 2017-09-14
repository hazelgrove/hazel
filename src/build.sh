#!/bin/bash

set -eou pipefail

SCRIPTPATH="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPTPATH"

../core/extract-coq.sh
refmt ../core/ml/hazel_core.ml > hazel_semantics.re

for f in `find . -name '*.re'` ; \
do ( \
refmt --in-place $f; \
); \
done

rebuild -use-ocamlfind \
  -pkgs js_of_ocaml,js_of_ocaml.ppx,js_of_ocaml.tyxml,tyxml,react,reactiveData,camomile \
   hazel.byte;

js_of_ocaml +weak.js -o www/hazel.js hazel.byte

