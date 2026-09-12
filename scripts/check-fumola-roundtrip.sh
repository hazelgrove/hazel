#!/usr/bin/env bash
# Check the printer's contract against the real Fumola parser.
#
# FumolaPrint claims that everything it prints is accepted by
# crates/fumola_parser and means there what it means here. The Hazel test
# suite cannot check that claim -- it can only check that the printer agrees
# with our own expectations, which is exactly the agreement a misread grammar
# would preserve. So this runs the corpus through the Fumola binary.
#
# Two checks, because acceptance is not meaning:
#
#   1. `fumola check` accepts every line. This catches the printer emitting
#      something ungrammatical -- dropping parentheses around an adapton form,
#      say, which Fumola rejects outright rather than misparsing.
#
#   2. `fumola eval` agrees between the printed form and the same source with
#      every operator's grouping made explicit by the shell. This catches the
#      errors the first check cannot see -- ones where both spellings parse.
#      The sharpest is not precedence but `{ … }`: in a nest position it is a
#      block, anywhere else an object literal, so `{ x }` evaluates to the
#      record {x = 5} rather than to x, and `fumola check` is happy either way.
#      Lines that do not reduce to a value sit this one out; the summary says
#      how many took part.
#
# Needs a built Fumola binary. Set FUMOLA_BIN, or it looks in the usual place.
set -uo pipefail

HAZEL_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
FUMOLA_REPO="${FUMOLA_REPO:-$HOME/fumola}"
FUMOLA_BIN="${FUMOLA_BIN:-$FUMOLA_REPO/target/debug/fumola}"
CORPUS="${1:-$HAZEL_ROOT/fumola-corpus.txt}"

if [ ! -x "$FUMOLA_BIN" ]; then
  echo "Fumola binary not found at $FUMOLA_BIN." >&2
  echo "Build it with: cd $FUMOLA_REPO && cargo build -p fumola" >&2
  exit 2
fi

if [ ! -f "$CORPUS" ]; then
  echo "Corpus not found at $CORPUS." >&2
  echo "It is written by the FumolaPrint test group:" >&2
  echo "  dune build test/haz3ltest.bc.js && bash test/run_node.sh test FumolaPrint" >&2
  exit 2
fi

pass=0
fail=0
failed_lines=()

while IFS= read -r src; do
  [ -z "$src" ] && continue
  if "$FUMOLA_BIN" check -- "$src" >/dev/null 2>&1; then
    pass=$((pass + 1))
  else
    fail=$((fail + 1))
    failed_lines+=("$src")
  fi
done < "$CORPUS"

echo "fumola check: $pass accepted, $fail rejected (of $((pass + fail)))"

if [ "$fail" -gt 0 ]; then
  echo
  echo "Rejected by the Fumola parser:" >&2
  for l in "${failed_lines[@]}"; do
    echo "  $l" >&2
    "$FUMOLA_BIN" check -- "$l" 2>&1 | grep -v '^\[' | sed 's/^/      /' >&2
  done
  exit 1
fi

echo "Every printed program is accepted by crates/fumola_parser."

# --- check 2: the minimal and explicit spellings of a term agree ---
#
# The test suite prints each corpus term twice: once with only the parentheses
# the grammar requires, and once with every grouping made explicit. Those two
# strings encode the same belief about precedence. If the belief is wrong they
# are still both grammatical -- which is why check 1 cannot see it -- but they
# evaluate to different values.
EXPLICIT="${2:-${CORPUS%.txt}-explicit.txt}"

if [ ! -f "$EXPLICIT" ]; then
  echo "Explicit corpus not found at $EXPLICIT; skipping the meaning check." >&2
  exit 0
fi

agree=0
differ=0
skipped=0
differed_lines=()

value_of() {
  # The last line of eval output, or nothing if it did not reduce to a value.
  "$FUMOLA_BIN" eval -- "$1" 2>/dev/null | tail -1
}

exec 3< "$CORPUS"
exec 4< "$EXPLICIT"
while IFS= read -r src <&3 && IFS= read -r exp_src <&4; do
  [ -z "$src" ] && continue
  if [ "$src" = "$exp_src" ]; then
    # Nothing to compare: the term needed no parentheses either way.
    skipped=$((skipped + 1))
    continue
  fi
  bare=$(value_of "$src")
  explicit=$(value_of "$exp_src")
  if [ -z "$bare" ] || [ -z "$explicit" ]; then
    skipped=$((skipped + 1))
  elif [ "$bare" = "$explicit" ]; then
    agree=$((agree + 1))
  else
    differ=$((differ + 1))
    differed_lines+=("$src"$'\t'"$exp_src"$'\t'"$bare"$'\t'"$explicit")
  fi
done
exec 3<&-
exec 4<&-

echo "fumola eval: $agree agree, $differ differ, $skipped not compared"

if [ "$differ" -gt 0 ]; then
  echo
  echo "The printer groups these differently from what it claims:" >&2
  for l in "${differed_lines[@]}"; do
    IFS=$'\t' read -r src exp_src bare explicit <<< "$l"
    echo "  printed:  $src" >&2
    echo "     => $bare" >&2
    echo "  explicit: $exp_src" >&2
    echo "     => $explicit" >&2
  done
  exit 1
fi

if [ "$agree" -eq 0 ]; then
  echo "No term was compared -- the meaning check proved nothing." >&2
  exit 1
fi

echo "Minimal and explicit spellings agree on all $agree comparable terms."
