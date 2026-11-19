#!/bin/sh
# Create (or top up) the opam switch for the js_of_ocaml 6 / Jane Street v0.18
# preview port (hazelgrove/hazel#1131). Leaves every other switch alone: the
# extra repositories are selected for this switch only.
#
#   scripts/jsoo6-switch.sh            # switch name defaults to hazel-jsoo6
#   SWITCH=foo scripts/jsoo6-switch.sh
#
# Then build with: opam exec --switch=hazel-jsoo6 -- dune build ...
set -eu

SWITCH="${SWITCH:-hazel-jsoo6}"
OCAML="${OCAML:-ocaml-base-compiler.5.4.1}"
ROOT="$(cd "$(dirname "$0")/.." && pwd)"

if ! opam switch list --short | grep -qx "$SWITCH"; then
  # Repository ranks, highest first:
  #  - hazel-jsoo6-overlay: patches Hazel needs on top of the preview packages
  #    (opam-jsoo6/ in this repo; see opam-jsoo6/README.md)
  #  - opam-fresh: a second copy of opam.ocaml.org, so this switch sees current
  #    metadata (alcotest 1.10) without `opam update`-ing the shared default repo
  #  - default: the usual opam.ocaml.org
  #  - janestreet-bleeding: Jane Street's v0.18~preview packages, which as of
  #    v0.18~preview.133.03+642 build on mainline OCaml >= 5.1 with jsoo >= 6
  opam switch create "$SWITCH" "$OCAML" --no-switch -y \
    --repos="hazel-jsoo6-overlay=$ROOT/opam-jsoo6,opam-fresh=https://opam.ocaml.org,default,janestreet-bleeding=https://github.com/janestreet/opam-repository.git"
fi

# bisect_ppx 2.8.3 (the latest release) caps ppxlib < 0.36, and its master
# lifts the cap but does not build against ppxlib's 5.2 AST. Use the branch of
# the open upstream PR ocaml-ppx/bisect_ppx#448 ("Support ppxlib.0.36.0").
opam pin add --switch="$SWITCH" -n -y bisect_ppx \
  "https://github.com/patricoferris/bisect_ppx.git#7061d643ff492b0045796357ee6917ded21fb1f0"

# LC_ALL=C: incremental's build seds over non-UTF-8 sources (opam-jsoo6/README.md)
LC_ALL=C opam install --switch="$SWITCH" -y "$ROOT/hazel.opam" --deps-only --with-test
