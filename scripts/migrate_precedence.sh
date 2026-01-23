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

# Check if file needs migration (has old dev precedence values)
# Dev values that need migration: 35, 36, 37, 38, 40, 43, 44, 47
needs_migration() {
  grep -qE '(\(Concave (35|36|37|38|40|43|44|47)\)|^         (35|36|37|38|40|43|44|47)\)\))' "$1"
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

# Migration mappings from dev baseline to new values:
#
#   Dev Value -> New Value   (Identifier)
#   ---------    ---------   ------------
#   35        -> 36          (if_)
#   36        -> 37          (fun_)
#   37        -> 44          (prod)
#   38        -> 35          (semi)
#   40        -> 45          (let_)
#   43        -> 46          (rule_sep)
#   44        -> 42          (case_)
#   47        -> 44          (comma)
#
# IMPORTANT: There are cycles (35->36->37->44, and 38->35, and 44->42)
# so we use a two-phase approach with temporary values (100+).
#
# Two pattern types in serialized files:
#   1. Inline: (Concave N) -> (Concave M)
#   2. Line-wrapped: ^         N)) -> ^         M))  (9 spaces before number)

if [[ "$DRY_RUN" == "true" ]]; then
  echo "Dry run - would migrate: $FILE"
  echo "(Actual diff not shown for two-phase migration)"
  exit 0
fi

# Phase 1: Convert source values to temp values (100+)
sed "${SED_INPLACE[@]}" \
  -e 's/(Concave 35)/(Concave 135)/g' \
  -e 's/(Concave 36)/(Concave 136)/g' \
  -e 's/(Concave 37)/(Concave 137)/g' \
  -e 's/(Concave 38)/(Concave 138)/g' \
  -e 's/(Concave 40)/(Concave 140)/g' \
  -e 's/(Concave 43)/(Concave 143)/g' \
  -e 's/(Concave 44)/(Concave 144)/g' \
  -e 's/(Concave 47)/(Concave 147)/g' \
  -e 's/^         35))/         135))/g' \
  -e 's/^         36))/         136))/g' \
  -e 's/^         37))/         137))/g' \
  -e 's/^         38))/         138))/g' \
  -e 's/^         40))/         140))/g' \
  -e 's/^         43))/         143))/g' \
  -e 's/^         44))/         144))/g' \
  -e 's/^         47))/         147))/g' \
  "$FILE"

# Phase 2: Convert temp values to final values
sed "${SED_INPLACE[@]}" \
  -e 's/(Concave 135)/(Concave 36)/g' \
  -e 's/(Concave 136)/(Concave 37)/g' \
  -e 's/(Concave 137)/(Concave 44)/g' \
  -e 's/(Concave 138)/(Concave 35)/g' \
  -e 's/(Concave 140)/(Concave 45)/g' \
  -e 's/(Concave 143)/(Concave 46)/g' \
  -e 's/(Concave 144)/(Concave 42)/g' \
  -e 's/(Concave 147)/(Concave 44)/g' \
  -e 's/^         135))/         36))/g' \
  -e 's/^         136))/         37))/g' \
  -e 's/^         137))/         44))/g' \
  -e 's/^         138))/         35))/g' \
  -e 's/^         140))/         45))/g' \
  -e 's/^         143))/         46))/g' \
  -e 's/^         144))/         42))/g' \
  -e 's/^         147))/         44))/g' \
  "$FILE"

echo "Migrated: $FILE"
