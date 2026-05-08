#!/bin/bash
#
# migrate_typfun_to_abs.sh
#
# Renames serialized expression-level `typfun` references in `.ml` slide
# files to `abs`. Needed after the surface keyword for the value-level
# type abstraction was renamed (`typfun a -> e` → `abs a -> e`) so the
# `typfun` keyword could be reused for the new type-level type-function
# syntax (`type Option = typfun a -> + None + Some(a)`).
#
# Two kinds of edits are needed in slide files:
#
# 1. Serialized tile labels:
#      (label(typfun ->))(mold((out Exp)…))
#    → (label(abs ->))(mold((out Exp)…))
#    Only the Exp-mold tiles are renamed — `typfun` tiles whose mold is
#    `(out Typ)` are the new type-level form and must keep the keyword.
#
# 2. Backup-text source code: any `typfun a -> …` inside the
#    `backup_text` string. A simple word-boundary rename suffices since
#    the backup text is plain Hazel surface syntax.
#
# Slides wrap long string literals with `\`-newline-leading-whitespace,
# which makes plain-line sed awkward. Use Python so we can match across
# the line wrapping.
#
# Usage: ./migrate_typfun_to_abs.sh [--dry-run] <file.ml>

set -e

DRY_RUN=false
FILE=""

while [[ $# -gt 0 ]]; do
  case $1 in
    --dry-run) DRY_RUN=true; shift ;;
    -*) echo "Unknown option: $1" >&2; exit 1 ;;
    *)
      if [[ -n "$FILE" ]]; then
        echo "Error: Only one file argument supported" >&2
        exit 1
      fi
      FILE="$1"; shift ;;
  esac
done

if [[ -z "$FILE" || ! -f "$FILE" ]]; then
  echo "Usage: $0 [--dry-run] <file.ml>" >&2
  exit 1
fi

if ! grep -q 'typfun' "$FILE"; then
  echo "File has no typfun references: $FILE"
  exit 0
fi

DRY="$DRY_RUN" python3 - "$FILE" <<'PY'
import os, re, sys
path = sys.argv[1]
with open(path, "r") as f:
    content = f.read()

# 1. Tile-label rename. The serialized substring
#    `(label(typfun ->))(mold((out Exp)`
#    can be split across `\`-continuation breaks: any whitespace (incl.
#    `\` + newline + leading spaces) may sit between any two tokens.
#    Match `typfun` followed by any run of whitespace/backslashes followed
#    by `->))(mold((out Exp)` and replace `typfun` with `abs`.
tile_label_pattern = re.compile(
    r'\(label\(typfun([\s\\]+)->\)\)\(mold\(\(out Exp\)'
)
new_content, tile_subs = tile_label_pattern.subn(
    lambda m: '(label(abs' + m.group(1) + '->))(mold((out Exp)',
    content,
)

# 2. Backup-text rename. The backup_text is a regular OCaml string
#    literal that contains plain Hazel surface source. Replace
#    `typfun ` (the keyword as a word) with `abs ` everywhere it
#    appears as a free token in the file. We're conservative: only
#    rewrite occurrences NOT inside a tile-label substring (those were
#    already handled above).
backup_text_pattern = re.compile(
    r'(?<![A-Za-z0-9_])typfun(?=[\s\\])'
)
new_content2, text_subs = backup_text_pattern.subn('abs', new_content)

if os.environ.get("DRY") == "true":
    print(f"Dry run - would migrate: {path}")
    print(f"Tile-label matches: {tile_subs}")
    # Subtract tile_subs because that pattern's `typfun` would have been
    # double-counted by the simpler backup-text pattern.
    print(f"Backup-text matches: {text_subs - tile_subs}")
else:
    with open(path, "w") as f:
        f.write(new_content2)
    print(f"Migrated: {path}")
PY
