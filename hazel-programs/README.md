# Hazel Programs

This directory contains Hazel programs in plain text (`.hz` files) for various purposes.

## Directory Structure

```
hazel-programs/
├── docs/       # Programs from Hazel documentation slides (9 files)
├── b2t2/       # Programs from B2T2 table API slides (39 files)
│   ├── errors/
│   ├── example_programs/
│   └── table_api/
└── study/      # Programs and docs for user studies (probe mechanism evaluation)
    ├── debugging/   # Programs with intentional bugs for debugging tasks
    └── writing/     # Sketch/solution pairs for program writing tasks
```

## File Format

Hazel programs use the `.hz` extension and contain plain text Hazel syntax.

```hazel
# Example: recursive function (requires arrow type annotation)
let length : [Int] -> Int =
fun xs ->
  case xs
  | [] => 0
  | hd::tl => 1 + length(tl)
  end
in length([1, 2, 3])
```

**Note**: Hazel has no `let rec` keyword. Recursion works automatically with arrow type annotations.

## Running Programs

```bash
# From repository root:
./hazel run hazel-programs/study/basic-functions.hz

# Or from stdin:
cat hazel-programs/docs/BasicReference.hz | ./hazel run -

# Check for type errors:
./hazel analyze hazel-programs/docs/BasicReference.hz
```

## Regenerating Extracted Programs

Programs in `docs/` and `b2t2/` are extracted from ML source files. To regenerate after source changes:

```bash
python3 scripts/extract-docs.py
```

See `scripts/README.md` for details.

## Directory Details

### `docs/`
Programs from the main Hazel documentation slides (`src/web/init/docs/`). Includes:
- BasicReference.hz - Language quick reference
- ADTs.hz - Algebraic data types example
- Probes.hz - Probe projector documentation
- And more...

### `b2t2/`
Programs from B2T2 (Bootstrap to Table Types) slides (`src/b2t2/slides/`). Examples of table processing in Hazel.

### `study/`
Programs and documentation for user studies evaluating Hazel's probe debugging mechanism.
- `debugging/` - Programs with intentional bugs for debugging tasks
- `writing/` - Sketch/solution pairs for program writing tasks
- Planning docs, tutorials, and guides for the study
