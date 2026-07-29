#!/usr/bin/env bash
# Regenerate the MVU documentation slides in src/mvu from the .hz sources here.
#
# Run this after editing any .hz in this directory. Nothing checks that the
# encodings are current, so an un-regenerated edit silently leaves the old
# program shipping in the slide.
#
# Three things happen beyond a plain `hazel slide-encode`:
#
#   1. Leading whitespace is stripped from every line. Hazel computes
#      indentation at layout time (Measured.add_secondary places a linebreak
#      at the level from Indentation.level_map) and renders any literal
#      leading spaces on top of it, so baked-in indentation shows up doubled
#      and drifting. Flat input renders correctly indented.
#
#   2. The final (init, update, view, subs) tuple is wrapped in
#      ^^html_sidebar(...) so the slide opens with the app already running and
#      docked in the projector panel, leaving a chip at the code site. The
#      `_sidebar` suffix is part of the invoke token, so the placement lives in
#      the TEXT — a slide stores both a zipper and its backup_text, and if
#      placement were patched into the zipper afterwards the two would disagree
#      and DocSlides.ReparseBackuptext would (rightly) fail.
#
#      Nothing else is patched into the encoded slide. A slide stores a zipper
#      AND a backup_text, and only what the TEXT can express survives a reparse
#      (DocSlides.ReparseBackuptext checks exactly this), so a slide can only
#      carry projectors in their default model state. Docked apps size to their
#      content (proj-html.css), so they no longer need a baked-in size.

set -euo pipefail

cd "$(dirname "$0")/../.."
HAZEL=./hazel
OUT=src/mvu
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

# <source .hz>:<generated module>:<slide title>
PROGRAMS=(
  "mvu-counter:MvuCounter:Counter"
  "timer:MvuTimer:Watering Timer"
  "todo-list:MvuTodoList:Planting List"
  "keyboard-game:MvuFirefly:Firefly"
  "crop-plotter:MvuCropPlotter:Crop Plotter"
  "tictactoe:MvuTicTacToe:Sprouts and Shrooms"
  "gameoflife:MvuGameOfLife:Garden of Life"
  "seed-catalog:MvuSeedCatalog:Seed Catalog"
  "harvest-streak:MvuHarvestStreak:Harvest Ledger"
  "nutrient-rotation:MvuNutrientRotation:Nutrient Tracker"
)

# The trailing app tuple is usually `(init, update, view, subs)`, but a program
# whose update never issues a command wraps it — `(init, noCmd(update), view,
# subs)` — so match the shape rather than one exact string.
TUPLE_SHAPE='^\(.*\bview\b.*\bsubs\b.*\)$'

# With no argument, regenerate everything. With one, regenerate only the
# programs whose source name contains it (encoding one is much faster than
# encoding all ten).
FILTER="${1:-}"
matched=0

for entry in "${PROGRAMS[@]}"; do
  src="${entry%%:*}"
  if [ -n "$FILTER" ] && [[ "$src" != *"$FILTER"* ]]; then
    continue
  fi
  matched=$((matched + 1))
  rest="${entry#*:}"
  mod="${rest%%:*}"
  title="${rest#*:}"

  hz="hazel-programs/mvu/$src.hz"
  flat="$TMP/$src.hz"

  # Hazel comments are single-line `# ... #`. A comment spanning two lines
  # silently reparses as code rather than erroring at the delimiter, so catch
  # an odd number of `#` on any line (ignoring string literals, which hold
  # hex colours). Cheaper than reading it out of a wall of static errors.
  python3 - "$hz" <<'PY' || exit 1
import re, sys
path = sys.argv[1]
bad = [
    (n, line.rstrip())
    for n, line in enumerate(open(path), 1)
    if re.sub(r'"(?:[^"\\]|\\.)*"', '""', line).count('#') % 2
]
for n, line in bad:
    print(f"{path}:{n}: unterminated comment: {line}", file=sys.stderr)
sys.exit(1 if bad else 0)
PY

  # Strip indentation, then dock the trailing tuple under an html projector.
  sed 's/^[[:space:]]*//' "$hz" > "$flat"
  tuple="$(grep -v '^[[:space:]]*$' "$flat" | tail -1)"
  if ! printf '%s' "$tuple" | grep -qE "$TUPLE_SHAPE"; then
    echo "$hz: expected to end in an app tuple, found: $tuple" >&2
    exit 1
  fi
  perl -0pi -e "s/\Q$tuple\E(\s*)\z/^^html_sidebar($tuple)\$1/" "$flat"

  $HAZEL slide-encode --title="$title" -o "$OUT/$mod.ml" "$flat"

  # The ^^html_sidebar trigger already docks it; just confirm one encoded.
  if ! grep -q 'placement Sidebar' "$OUT/$mod.ml"; then
    echo "$OUT/$mod.ml: no docked projector encoded" >&2
    exit 1
  fi

  echo "$mod.ml  <-  $src.hz  ($title)"
done

if [ "$matched" -eq 0 ]; then
  echo "No program matched '$FILTER'." >&2
  exit 1
fi

# slide-encode's output is not ocamlformat-clean, and the repo formats .ml.
# Left unformatted, the next `make test-quick` (and CI) fails on the promotion
# alone, with 3000+ passing tests buried above it. Format here instead.
#
# ocamlformat breaks the long persisted string with `\`-continuations, which
# preserves the string's value but splits it across lines — so any grep-based
# inspection of a formatted slide must join continuations first.
dune build @src/mvu/fmt --auto-promote 2>/dev/null || true

echo
echo "Regenerated $matched slide(s). Rebuild to pick them up."
