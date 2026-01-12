# Hazel Scripts

Utility scripts for the Hazel project.

## extract-docs.py

Extracts Hazel programs from OCaml documentation files. These files contain a `backup_text` field with the Hazel source as an escaped string.

### Usage

```bash
# Extract all sources (docs + b2t2 slides)
python3 scripts/extract-docs.py

# Extract only docs (src/web/init/docs/)
python3 scripts/extract-docs.py --docs

# Extract only b2t2 slides (src/b2t2/slides/)
python3 scripts/extract-docs.py --b2t2
```

### Output

- `hazel-programs/docs/` - Programs from `src/web/init/docs/`
- `hazel-programs/b2t2/` - Programs from `src/b2t2/slides/` (preserves subdirectory structure)

### When to Re-run

Run this script after modifying documentation slides in:
- `src/web/init/docs/*.ml`
- `src/b2t2/slides/**/*.ml`

The script is idempotent and will overwrite existing files.

### Requirements

- Python 3.10+ (for `str | None` type hints)
- No external dependencies
