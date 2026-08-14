# Hazel Programs

This directory contains Hazel programs in plain text (`.hz` files) for various purposes.

## Directory Structure

```
hazel-programs/
├── docs/
│   ├── reference/   # THE documentation-mode slides (compiled into src/docslides)
│   └── b2t2/        # THE B2T2 table-API slides (compiled into src/b2t2)
├── study/           # Programs for user studies (probe mechanism evaluation)
│   ├── debugging/   # Programs with intentional bugs for debugging tasks
│   ├── writing/     # Sketch/solution pairs for program writing tasks
│   └── tutorial/    # Study tutorial slides
├── study-old/       # Earlier study programs, kept for reference (not shipped)
└── tutorial/        # Tutorial-mode lesson sources, .hzt (see tutorial/README.md)
```

## File Format

Hazel programs use the `.hz` extension and contain plain text Hazel syntax.
Every `.hz` file here must parse (the corpus tests check that). Tutorial
lesson sources, which mix prose with `@`-marker sections, use `.hzt`.

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
cat hazel-programs/docs/reference/basic-reference.hz | ./hazel run -

# Check for type errors:
./hazel analyze hazel-programs/docs/reference/basic-reference.hz
```

## Directory Details

### `docs/`
The committed text of the shipped slides: `reference/` is embedded by
`src/docslides`, `b2t2/` by `src/b2t2`. Editing a file here changes the slide.

### `study/`
Programs and documentation for user studies evaluating Hazel's probe debugging mechanism.
- `debugging/` - Programs with intentional bugs for debugging tasks
- `writing/` - Sketch/solution pairs for program writing tasks
- Planning docs, tutorials, and guides for the study
