# Hazel Programs

This directory contains Hazel programs in plain text (`.hz` files) for various purposes.

## Directory Structure

```
hazel-programs/
├── world/           # THE world tree: the standard library and every shipped
│   │                #   slide, as one program (World mode); generated into
│   │                #   src/world (WorldSlides) and baked into src/stdlib
│   ├── order        # the top level's order: the library, then the folders
│   ├── List.hz …    # the standard library's modules
│   ├── documentation/   # Documentation's slides (reference, livelits,
│   │                    #   perf corpora, b2t2), one folder per module
│   └── scratch/         # Scratch's starting slide
├── config/          # Configuration-mode programs
└── tutorial/        # Tutorial-mode lesson sources, .hzt (see tutorial/README.md)
```

A folder is a module and a `.hz` file a slide (at the top level, a library
module); a `.md` file is a slide of prose. Each folder's `order` file lists
its entries, one a line: `<name without extension>  <display name>  [exp]`
(two or more spaces between fields; `#` comment lines). `exp` marks an
expression program (the perf corpora); every other slide is module members
(`let x = 1;`). Unlisted entries follow by name.

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

The CLI reads expression programs: the perf corpora, the config programs,
or your own. (World slides are module members; the CLI does not read those
yet.)

```bash
# From repository root:
./hazel run hazel-programs/world/documentation/perf/mega-1k.hz

# Or from stdin:
cat hazel-programs/world/documentation/perf/mega-1k.hz | ./hazel run -

# Check for type errors:
./hazel analyze hazel-programs/world/documentation/perf/mega-1k.hz
```

## Directory Details

### `world/`
The committed text of the library and the shipped slides. Editing a file
here changes the slide; adding one (and a line in its folder's `order`)
adds a slide.
