# Precedence Migration Script

## Why This Exists

Hazel serializes syntax trees into `.ml` files (in `src/web/init/docs/` and `src/b2t2/slides/`). These serialized representations include numeric precedence values baked into nib shapes like `(Concave N)`.

This branch reorganizes precedence values with the following semantic changes:

1. **Move `semi` to be the tightest structural form** — sequences naturally extend into bodies
2. **Consolidate `prod` with `comma`** — both now use the same precedence value
3. **Make `comma` tighter than `let`** — tuples can appear as let bodies without parens
4. **Make `comma` tighter than `rule_sep`** — tuples can appear as case scrutinees without parens

### Parsing examples

| Expression | Dev parsing | New parsing |
|------------|-------------|-------------|
| `fun x -> a; b` | `(fun x -> a); b` | `fun x -> (a; b)` ← changed |
| `if c then a; b else d` | `(if c then a); b else d` | `if c then (a; b) else d` ← changed |
| `let x = 1 in a; b` | `let x = 1 in (a; b)` | `let x = 1 in (a; b)` |
| `fun x -> a, b` | `(fun x -> a), b` | `(fun x -> a), b` |
| `let x = 1 in x, y` | `(let x = 1 in x), y` | `let x = 1 in (x, y)` ← changed |
| `case a, b \| ...` | (comma outside case) | `case (a, b) \| ...` ← changed |

## Precedence Changes

### Old Values (dev baseline)

```
concave_grout = 34
if_ = 35
fun_ = 36
prod = 37
semi = 38
lab = 39
let_ = 40
rule_arr = 41
rule_pre = 42
rule_sep = 43
case_ = 44
comma = 47
min = 48
```

### New Values (this branch)

```
concave_grout = 34   (unchanged)

// ===== SEMICOLON (tightest structural form) =====
semi = 35            (moved up from 38)

// ===== STRUCTURAL FORMS =====
if_ = 36             (shifted from 35)
fun_ = 37            (shifted from 36)
lab = 39             (unchanged)
case_ = 42           (moved from 44)
comma = 44           (moved from 47, prod consolidated here)
let_ = 45            (moved from 40)
rule_sep = 46        (moved from 43)

min = 48             (unchanged)
```

### Migration Mapping

| Dev Value | New Value | Identifier |
|-----------|-----------|------------|
| 35        | 36        | if_        |
| 36        | 37        | fun_       |
| 37        | 44        | prod (→ comma) |
| 38        | 35        | semi       |
| 40        | 45        | let_       |
| 43        | 46        | rule_sep   |
| 44        | 42        | case_      |
| 47        | 44        | comma      |

**Note:** Values 39 (lab), 41 (rule_arr), 42 (rule_pre) are unchanged or unused in current slide files.

**Note:** There are cycles in the migration (35→36→37→44, 38→35, 44→42), so the script uses a two-phase approach with temporary values (100+) to avoid collisions.

## Usage

```bash
# Preview changes without modifying the file
./scripts/migrate_precedence.sh --dry-run path/to/file.ml

# Apply the migration
./scripts/migrate_precedence.sh path/to/file.ml

# Migrate all slide files
find src/web/init/docs src/b2t2/slides -name "*.ml" -exec ./scripts/migrate_precedence.sh {} \;
```

The script will:
- Skip files already migrated (no old dev values found)
- Show a diff in dry-run mode
- Report success after migration

## What It Changes

The script handles two pattern types in serialized files:

1. **Inline patterns**: `(Concave N)` → `(Concave M)`
2. **Line-wrapped patterns**: When the number appears at the start of a line (after 9 spaces) due to OCaml string wrapping

## Platform Support

- **macOS**: Tested and working
- **Linux**: Included but untested (uses different `sed -i` syntax)

## When to Use

Run this script if you:
- Have slide files from the dev branch that need updating
- Created new slide files based on dev templates
- See test failures in `DocSlides.ReparseBackuptext` related to precedence mismatches

## Technical Notes

### Two-phase migration

Because some source and target values overlap (creating cycles like 35→36→37→44), the script uses temporary values (100+) in phase 1, then converts to final values in phase 2. This prevents sed from double-transforming values.

### Segment structure vs precedence

The segment structure (serialized in `.ml` files) does **not** do precedence parsing. It's a tree based on matching delimiters. Precedence numbers are metadata baked into tiles that only affect how the structure is later interpreted. This migration only updates those metadata numbers.
