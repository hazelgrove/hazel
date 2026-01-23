# Precedence Migration Script

## Why This Exists

Hazel serializes syntax trees into `.ml` files (in `src/web/init/docs/` and `src/b2t2/slides/`). These serialized representations include numeric precedence values that are baked into the syntax.

In the `comma-precedence-fix` branch, we changed the relative precedences of comma, let, and case rule separators so that:

- `let x = 1 in x, y` parses as `let x = 1 in (x, y)` (tuple inside let body)
- Previously it parsed as `(let x = 1 in x), y` (let only bound first element)

This required updating the precedence values in `Precedence.re`:

| Operator   | Old Value | New Value |
|------------|-----------|-----------|
| `comma`    | 47        | 44        |
| `let_`     | 40        | 45        |
| `rule_sep` | 43        | 46        |

Any `.ml` slide files created or modified before this change need to have their serialized precedence values updated to match.

## Usage

```bash
# Preview changes without modifying the file
./scripts/migrate_precedence.sh --dry-run path/to/file.ml

# Apply the migration
./scripts/migrate_precedence.sh path/to/file.ml
```

The script will:
- Skip files that are already migrated (no old precedence values found)
- Show a diff in dry-run mode
- Report success after migration

## What It Changes

The script performs these substitutions:

1. **Inline patterns**: `(Concave N)` where N is 40, 43, or 47
2. **Line-wrapped patterns**: When the number appears at the start of a line (after 9 spaces) due to OCaml string wrapping

## Platform Support

- **macOS**: Tested and working
- **Linux**: Included but untested (uses different `sed -i` syntax)

## When to Use

Run this script if you:
- Created new slide files based on old templates
- Modified existing slides that were serialized before the precedence change
- See test failures in `DocSlides.ReparseBackuptext` related to precedence mismatches

## Verification

This script was verified by successfully reproducing the migration of all 49 `.ml` files in commit `440a08eb1` ("Migrate serialized precedence values for comma/let/rule_sep changes").
