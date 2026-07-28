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
#   2. The final (init, update, view, subs) tuple is wrapped in ^^html(...)
#      so the slide opens with the app already running.
#
#   3. The generated projector is switched to `placement Sidebar`, so the app
#      docks in the projector panel and leaves a chip at the code site, and
#      is given a size that fits its content. HTMLProj defaults to 40x12
#      character cells, which clips all but the smallest of these. Docked,
#      the panel owns the width, so `rows` is what matters; `cols` applies
#      when a reader undocks the app back inline.

set -euo pipefail

cd "$(dirname "$0")/../.."
HAZEL=./hazel
OUT=src/mvu
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

# <source .hz>:<generated module>:<cols>x<rows>:<slide title>
#
# Rows are measured, not guessed: a row is ~24.9px, and these come from each
# app's rendered content height plus a row of slack. The three whose content
# grows as you use them carry extra headroom on purpose — the planting list
# (todo items), the harvest ledger (table rows) and the seed catalog (its
# collection list). Docked, `cols` has no effect since the panel owns the
# width; it applies when a reader undocks the app back inline.
PROGRAMS=(
  "mvu-counter:MvuCounter:40x9:Counter"
  "timer:MvuTimer:44x9:Watering Timer"
  "todo-list:MvuTodoList:48x13:Planting List"
  "keyboard-game:MvuFirefly:56x17:Firefly"
  "crop-plotter:MvuCropPlotter:52x17:Crop Plotter"
  "tictactoe:MvuTicTacToe:52x16:Sprouts and Shrooms"
  "gameoflife:MvuGameOfLife:56x16:Garden of Life"
  "seed-catalog:MvuSeedCatalog:60x35:Seed Catalog"
  "harvest-streak:MvuHarvestStreak:60x22:Harvest Ledger"
  "nutrient-rotation:MvuNutrientRotation:56x24:Nutrient Tracker"
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
  rest="${rest#*:}"
  size="${rest%%:*}"
  title="${rest#*:}"
  cols="${size%x*}"
  rows="${size#*x}"

  hz="hazel-programs/html-examples/$src.hz"
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
  perl -0pi -e "s/\Q$tuple\E(\s*)\z/^^html($tuple)\$1/" "$flat"

  $HAZEL slide-encode --title="$title" -o "$OUT/$mod.ml" "$flat"

  # slide-encode always emits Inline; these apps ship docked.
  if ! grep -q 'placement Inline' "$OUT/$mod.ml"; then
    echo "$OUT/$mod.ml: no projector encoded" >&2
    exit 1
  fi
  perl -pi -e 's/placement Inline/placement Sidebar/g' "$OUT/$mod.ml"
  perl -pi -e "s/\(cols \d+\)\(rows \d+\)/(cols $cols)(rows $rows)/g" "$OUT/$mod.ml"

  echo "$mod.ml  <-  $src.hz  ($title, ${cols}x${rows})"
done

if [ "$matched" -eq 0 ]; then
  echo "No program matched '$FILTER'." >&2
  exit 1
fi

# slide-encode's output is not ocamlformat-clean, and the repo formats .ml.
# Left unformatted, the next `make test-quick` (and CI) fails on the promotion
# alone, with 3000+ passing tests buried above it. Format here instead.
#
# Do this AFTER the size/placement rewrites above, not before: ocamlformat
# breaks the long persisted string with `\`-continuations, which preserves the
# string's value but splits `(cols N)(rows M)` across lines so it no longer
# matches. The rewrites need the unformatted, single-line form.
dune build @src/mvu/fmt --auto-promote 2>/dev/null || true

echo
echo "Regenerated $matched slide(s). Rebuild to pick them up."
