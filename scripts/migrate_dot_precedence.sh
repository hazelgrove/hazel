#!/bin/bash
#
# migrate_dot_precedence.sh
#
# Migrates the serialized `dot` precedence value (22 → 10) in `.ml` slide
# files. Needed after Precedence.dot was moved tighter than the type-level
# postfix `T(Int)` to fix qualified-type-access parsing (`M.T(Int)`).
#
# See README_migrate_precedence.md for the general migration mechanism;
# this script is single-valued so doesn't need the two-phase cycle handling
# of the original.
#
# Usage: ./migrate_dot_precedence.sh [--dry-run] <file.ml>
#
# Tested on macOS. Linux support included but untested.

set -e

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

# Skip if no `(Concave 22)` references remain
if ! grep -qE '(\(Concave 22\)|^         22\)\))' "$FILE"; then
  echo "File already migrated or has no dot precedence values: $FILE"
  exit 0
fi

# Detect platform for sed in-place syntax
if [[ "$(uname)" == "Darwin" ]]; then
  SED_INPLACE=(-i '')
else
  # Linux - note: untested
  SED_INPLACE=(-i)
fi

if [[ "$DRY_RUN" == "true" ]]; then
  echo "Dry run - would migrate: $FILE"
  echo "Inline matches:"
  grep -c '(Concave 22)' "$FILE" || true
  echo "Line-wrapped matches:"
  grep -cE '^         22\)\)' "$FILE" || true
  exit 0
fi

# Single-pass migration: 22 → 10 (no cycle since target is unused).
sed "${SED_INPLACE[@]}" \
  -e 's/(Concave 22)/(Concave 10)/g' \
  -e 's/^         22))/         10))/g' \
  "$FILE"

echo "Migrated: $FILE"
