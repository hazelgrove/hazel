#!/usr/bin/env bash
# Generate a starter .rei for a Reason module, from the signature the compiler
# already inferred.
#
#   make rei FILE=src/util/Tree.re      # writes src/util/Tree.rei
#   scripts/gen_rei.sh src/util/Tree.re # same
#
# Why you want a .rei at all: OCaml's warning 32 (unused value) is
# per-compilation-unit, so a module with no interface exports everything and
# the compiler can never tell you a value is dead. Add a .rei and warning 32
# starts working -- and it is a hard error in the release profile that CI
# gates on, so it keeps working.
#
# How this works. dune has already written a ppx-expanded <M>.re.pp.ml into
# _build, so `ocamlc -i` on that prints the inferred signature with every
# [@deriving] value already expanded. refmt converts it to Reason syntax. The
# output is already refmt-formatted, so `dune build @src/fmt` accepts it.
#
# The output is a STARTER, not a finished file. Two things to do by hand:
#
#   1. `ocamlc -i` prints [@deriving]-generated values individually (pp_foo,
#      show_foo, foo_of_sexp, ...). Collapse them back into the [@deriving]
#      attribute on the type -- the file gets shorter and stays in sync.
#      Careful: a .rei may declare FEWER derivers than the .re and still
#      compile, because the ppx wraps generated values in
#      [@@@ocaml.warning "-32"]. Drift shows up only as Unbound value at a
#      consumer, so keep the deriver lists identical.
#
#   2. Delete what you do not want exported. That is the point: whatever you
#      leave out, warning 32 will report as unused in the .re.
#
# Known non-starters, all of which produce a .rei larger than the source and
# break on the dependency's next release: any module whose body is
# `include <third-party>`. BigInt is 14 source lines and 184 extracted (refmt
# cannot even parse it); Sets is 29 -> 235; Result re-arms Base's deprecation
# alerts, fatal in release. Util.re is also excluded -- dune treats it as the
# library interface module and its extracted signature names Util__, which you
# cannot write by hand.

set -euo pipefail

SRC="${1:?usage: gen_rei.sh <path/to/Module.re>}"
[[ "$SRC" == *.re ]] || { echo "expected a .re file, got $SRC" >&2; exit 1; }

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO"

DIR="$(dirname "$SRC")"
BASE="$(basename "$SRC" .re)"
OUT="$DIR/$BASE.rei"

# Library name from the dune file next to the source, e.g. (name util).
LIB="$(grep -oE '\(name +[a-zA-Z0-9_]+\)' "$DIR/dune" 2>/dev/null \
  | head -1 | grep -oE '[a-zA-Z0-9_]+\)$' | tr -d ')')"
if [[ -z "$LIB" ]]; then
  echo "could not find a (name ...) in $DIR/dune; is this a library directory?" >&2
  exit 1
fi
# Dune capitalises the first letter for the module alias prefix.
ALIAS="$(tr '[:lower:]' '[:upper:]' <<<"${LIB:0:1}")${LIB:1}"
OBJS="$DIR/.$LIB.objs/byte"

echo "building $LIB ..." >&2
dune build @ocaml-index --profile dev

PP="_build/default/$DIR/$BASE.re.pp.ml"
[[ -f "$PP" ]] || { echo "no ppx-expanded source at $PP" >&2; exit 1; }

# Recover the exact flags dune compiles this library with, minus the bits that
# name the output and the input. -open <Alias>__ is load-bearing: without it
# every sibling module resolves to the wrong thing.
FLAGS="$(dune rules "_build/default/$OBJS/${LIB}__${BASE}.cmo" 2>/dev/null \
  | sed -n '/(action/,$p' | tr -d '\n' \
  | python3 -c '
import re, sys
s = re.sub(r"\s+", " ", sys.stdin.read())
m = re.search(r"ocamlc\.opt(.*)$", s)
if not m:
    sys.exit("could not find the ocamlc invocation in the dune rule")
toks = [t for t in m.group(1).replace(")", " ) ").split() if t not in ("(", ")")]
out, i = [], 0
while i < len(toks):
    t = toks[i]
    if t in ("-o", "-impl"):
        i += 2
    elif t == "-c":
        i += 1
    else:
        out.append(t)
        i += 1
print(" ".join(out))
')"
[[ -n "$FLAGS" ]] || { echo "no compile flags recovered for $BASE" >&2; exit 1; }

TMP="$(mktemp -t "$BASE.XXXXXX").mli"
trap 'rm -f "$TMP"' EXIT

( cd _build/default && ocamlc $FLAGS -i -impl "$DIR/$BASE.re.pp.ml" ) > "$TMP"
refmt --parse ml --print re --interface true "$TMP" > "$OUT"

# -short-paths renders sibling modules through the library's own interface
# module, e.g. Util.Either.t inside src/util. Compiled as a .rei in the same
# library that is a dependency cycle ("Module Aba ... depends on Util"), so
# strip the library's own prefix and let -open <Alias>__ resolve it.
python3 - "$OUT" "$ALIAS" <<'PY'
import re, sys
path, alias = sys.argv[1], sys.argv[2]
s = open(path, encoding="latin-1").read()
open(path, "w", encoding="latin-1").write(
    re.sub(rf"\b{re.escape(alias)}\.(?=[A-Z])", "", s))
PY

echo "wrote $OUT ($(wc -l < "$OUT" | tr -d ' ') lines)" >&2
echo "  now: collapse the [@deriving] values, delete what should stay private," >&2
echo "  then \`dune build @src/fmt src --profile release\` to see what is dead." >&2
