#!/usr/bin/env bash
# Regenerate the user-defined-livelit documentation slides in src/livelitdemos
# from the .hz sources here.
#
# Run this after editing any .hz in this directory. Nothing checks that the
# encodings are current, so an un-regenerated edit silently leaves the old
# program shipping in the slide.
#
# Unlike the MVU pipeline (hazel-programs/mvu/regen-slides.sh), nothing is
# rewritten: livelit uses are wrapped in ^^livelit(...) directly in the .hz
# text, which both materializes the projector on parse and keeps the file
# runnable with `./hazel run`. Leading whitespace is still stripped — Hazel
# computes indentation at layout time, so baked-in indentation renders
# doubled.

set -euo pipefail

cd "$(dirname "$0")/../.."
HAZEL=./hazel
OUT=src/livelitdemos
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

# <source .hz>:<generated module>:<slide title>
PROGRAMS=(
  "defined-slider:LivelitSlider:Define a Slider"
  "color-picker:LivelitColor:Color Picker"
)

# With no argument, regenerate everything. With one, regenerate only the
# programs whose source name contains it.
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

  hz="hazel-programs/livelits/$src.hz"
  flat="$TMP/$src.hz"

  # Hazel comments are single-line `# ... #`. A comment spanning two lines
  # silently reparses as code rather than erroring at the delimiter, so catch
  # an odd number of `#` on any line (ignoring string literals).
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

  sed 's/^[[:space:]]*//' "$hz" > "$flat"

  $HAZEL slide-encode --title="$title" -o "$OUT/$mod.ml" "$flat"

  # The ^^livelit(...) invokes in the text should have encoded projectors.
  if ! grep -q 'kind Livelit' "$OUT/$mod.ml"; then
    echo "$OUT/$mod.ml: no livelit projector encoded" >&2
    exit 1
  fi

  echo "$mod.ml  <-  $src.hz  ($title)"
done

if [ "$matched" -eq 0 ]; then
  echo "No program matched '$FILTER'." >&2
  exit 1
fi

# slide-encode's output is not ocamlformat-clean, and the repo formats .ml.
dune build @src/livelitdemos/fmt --auto-promote 2>/dev/null || true

echo
echo "Regenerated $matched slide(s). Rebuild to pick them up."
