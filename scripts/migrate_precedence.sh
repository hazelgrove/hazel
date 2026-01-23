#!/bin/bash
#
# migrate_precedence.sh
#
# Migrates serialized Hazel precedence values in .ml slide files.
# See README_migrate_precedence.md for details.
#
# Usage: ./migrate_precedence.sh [--dry-run] <file.ml>
#
# Tested on macOS. Linux support included but untested.

set -e

# Parse arguments
DRY_RUN=false
FILE=""

while [[ $# -gt 0 ]]; do
  case $1 in
    --dry-run)
      DRY_RUN=true
      shift
      ;;
    -*)
      echo "Unknown option: $1" >&2
      echo "Usage: $0 [--dry-run] <file.ml>" >&2
      exit 1
      ;;
    *)
      if [[ -n "$FILE" ]]; then
        echo "Error: Only one file argument supported" >&2
        echo "Usage: $0 [--dry-run] <file.ml>" >&2
        exit 1
      fi
      FILE="$1"
      shift
      ;;
  esac
done

if [[ -z "$FILE" ]]; then
  echo "Error: No file specified" >&2
  echo "Usage: $0 [--dry-run] <file.ml>" >&2
  exit 1
fi

if [[ ! -f "$FILE" ]]; then
  echo "Error: File not found: $FILE" >&2
  exit 1
fi

# Check if file needs migration (has old precedence values)
needs_migration() {
  grep -qE '(\(Concave (40|43|47)\)|^         (40|43|47)\)\))' "$1"
}

if ! needs_migration "$FILE"; then
  echo "File already migrated or has no precedence values to migrate: $FILE"
  exit 0
fi

# Detect platform for sed in-place syntax
if [[ "$(uname)" == "Darwin" ]]; then
  SED_INPLACE=(-i '')
else
  # Linux - note: untested
  SED_INPLACE=(-i)
fi

# The migration patterns:
#   comma:    47 -> 44  (tighter, so tuples bind inside let/case)
#   let_:     40 -> 45  (looser than comma)
#   rule_sep: 43 -> 46  (looser than comma)
#
# Two pattern types:
#   1. Inline: (Concave N) -> (Concave M)
#   2. Line-wrapped: ^         N)) -> ^         M))  (9 spaces before number)

if [[ "$DRY_RUN" == "true" ]]; then
  echo "Dry run - changes that would be made to: $FILE"
  echo "---"
  sed \
    -e 's/(Concave 40)/(Concave 45)/g' \
    -e 's/(Concave 43)/(Concave 46)/g' \
    -e 's/(Concave 47)/(Concave 44)/g' \
    -e 's/^         40))/         45))/g' \
    -e 's/^         43))/         46))/g' \
    -e 's/^         47))/         44))/g' \
    "$FILE" | diff "$FILE" - || true
  echo "---"
  echo "Run without --dry-run to apply changes."
else
  sed "${SED_INPLACE[@]}" \
    -e 's/(Concave 40)/(Concave 45)/g' \
    -e 's/(Concave 43)/(Concave 46)/g' \
    -e 's/(Concave 47)/(Concave 44)/g' \
    -e 's/^         40))/         45))/g' \
    -e 's/^         43))/         46))/g' \
    -e 's/^         47))/         44))/g' \
    "$FILE"
  echo "Migrated: $FILE"
fi
