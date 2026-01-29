# Segments in Hazel

This document explains the Segment data structure, which serves as an intermediate representation between raw text and the fully-parsed AST (Term) in Hazel.

## Overview

A **Segment** is a partially-parsed representation of code where:
- Delimiters are matched (e.g., parentheses, `let`/`=`/`in`)
- Operators remain flat (precedence parsing happens later)
- Tree structure exists only where delimiters create nesting

Segments are the output of tile-based editing and the input to term construction.

## Key Types

The core types are defined in `src/haz3lcore/tiles/Base.re`:

```reason
type segment = list(piece)

type piece =
  | Tile(tile)
  | Grout(Grout.t)
  | Secondary(Secondary.t)
  | Projector(projector)

type tile = {
  id: Id.t,
  label: Label.t,        /* e.g., ["let", "=", "in"] */
  mold: Mold.t,          /* shape and sort information */
  shards: list(int),     /* which delimiters are present */
  children: list(segment), /* bi-delimited content between shards */
}
```

### Pieces

A segment is a flat list of **pieces**:

| Piece Type | Purpose |
|------------|---------|
| `Tile` | Syntactic construct (operators, keywords, delimiters) |
| `Grout` | Placeholder for missing content (holes) |
| `Secondary` | Whitespace and comments |
| `Projector` | Visual projectors wrapping syntax |

### Tiles

A **tile** represents a syntactic form. The key fields are:

- **`label`**: The list of delimiter tokens. Defined in `src/haz3lcore/tiles/Label.re` as `list(Token.t)`.
  - Single-token: `["+"]`, `["x"]`, `["123"]`
  - Multi-token: `["let", "=", "in"]`, `["(", ")"]`, `["if", "then", "else"]`

- **`shards`**: Indices into `label` indicating which delimiters are actually present.
  - Complete tile: `shards = [0, 1, 2]` for `["let", "=", "in"]`
  - Incomplete tile: `shards = [0, 1]` means only `let` and `=` are present (missing `in`)

- **`children`**: Segments between consecutive shards (bi-delimited content).
  - For `["let", "=", "in"]` with all shards: 2 children (pattern and definition)
  - Invariant: `length(children) == length(shards) - 1`

- **`mold`**: Shape information from `src/haz3lcore/tiles/Mold.re`:
  ```reason
  type t = {
    out: Sort.t,           /* output sort (Exp, Pat, Typ, etc.) */
    in_: list(Sort.t),     /* sorts of bi-delimited children */
    nibs: (Nib.t, Nib.t),  /* left and right edge shapes */
  }
  ```

### Nib Shapes

Nibs describe how pieces connect. From `src/haz3lcore/tiles/Nib.re`:

```reason
type Shape.t =
  | Convex                  /* operand-like: can be an argument */
  | Concave(Precedence.t)   /* operator-like: expects operands */
```

- **Convex** shapes point outward (like a variable `x` or literal `42`)
- **Concave** shapes create "sockets" for operands (like `+` or `let`)
- Adjacent pieces must have fitting shapes: Convex-Concave or Concave-Convex

### Grout

Grout fills gaps where content is missing. From `src/haz3lcore/tiles/Grout.re`:

```reason
type shape = Convex | Concave
type t = { id: Id.t, shape }
```

- **Convex grout**: Missing operand (empty hole)
- **Concave grout**: Missing operator

## The Aba Pattern

Many structures use the **Aba** (Alternating B-A) pattern from `src/util/Aba.re`:

```reason
/* Invariant: length(as_) == length(bs) + 1 */
type t('a, 'b) = (list('a), list('b))
```

This represents alternating sequences like:
- `[a0, b0, a1, b1, a2]` stored as `([a0, a1, a2], [b0, b1])`
- For tiles: shards alternate with children

## Worked Examples

### Example A: Simple Infix Expression

**Code**: `1 + 2 * 3`

**Segment structure** (simplified, omitting whitespace):

```
[ Tile("1"), Tile("+"), Tile("2"), Tile("*"), Tile("3") ]
```

The segment is **flat** - there is no tree structure yet. The `+` and `*` tiles are siblings at the same level, not nested.

**ASCII diagram**:
```
Segment: [ 1 ] [ + ] [ 2 ] [ * ] [ 3 ]
              ^       ^       ^
         all tiles are siblings
```

Precedence parsing happens later in `MakeTerm.re`, which uses the **skeleton** (computed by `Skel.re`) to determine that `*` binds tighter than `+`.

### Example B: Parenthesized Expression

**Code**: `(1 + 2) * 3`

**Segment structure**:

```
[
  Tile({
    label: ["(", ")"],
    shards: [0, 1],
    children: [
      [ Tile("1"), Tile("+"), Tile("2") ]  /* child segment */
    ]
  }),
  Tile("*"),
  Tile("3")
]
```

**ASCII diagram**:
```
Segment: [ (~~~) ] [ * ] [ 3 ]
            |
            +---> child: [ 1 ] [ + ] [ 2 ]
```

The parentheses tile has **one child** containing the inner segment `1 + 2`. This is tree structure created by delimiter matching - the content between `(` and `)` is a child segment of the parentheses tile.

### Example C: Let Expression (Complete)

**Code**: `let x = 1 + 2 in x * 3`

**Segment structure**:

```
[
  Tile({
    label: ["let", "=", "in"],
    shards: [0, 1, 2],      /* all three delimiters present */
    children: [
      [ Tile("x") ],                        /* pattern: between "let" and "=" */
      [ Tile("1"), Tile("+"), Tile("2") ]   /* definition: between "=" and "in" */
    ]
  }),
  Tile("x"),
  Tile("*"),
  Tile("3")
]
```

**ASCII diagram**:
```
Segment: [ let~~~=~~~in ] [ x ] [ * ] [ 3 ]
              |     |
              |     +---> child 1: [ 1 ] [ + ] [ 2 ]  (definition)
              |
              +---> child 0: [ x ]  (pattern)
```

Key observations:
- The `let` tile has 3 delimiters and 2 children (between consecutive delimiters)
- Children are **bi-delimited**: each sits between two shards
- The body `x * 3` after `in` is **not** a child - it's a sibling of the `let` tile
- This is because `in` is uni-delimited on the right (nothing follows within the tile)

### Example D: Incomplete Let Expression

**Code**: `let x = 1 + 2`

**Segment structure**:

```
[
  Tile({
    label: ["let", "=", "in"],
    shards: [0, 1],         /* only "let" and "=" present, missing "in" */
    children: [
      [ Tile("x") ]         /* only one child: the pattern */
    ]
  }),
  Tile("1"),
  Tile("+"),
  Tile("2")
]
```

**ASCII diagram**:
```
Segment: [ let~~~= ] [ 1 ] [ + ] [ 2 ]
              |
              +---> child 0: [ x ]  (pattern only)
```

Key observations:
- `shards = [0, 1]` means indices 0 (`let`) and 1 (`=`) are present
- Index 2 (`in`) is missing, so the tile is **incomplete**
- With only 2 shards, there's only 1 child (between shard 0 and shard 1)
- The content `1 + 2` after `=` becomes **siblings** of the `let` tile, not children
- This is the key difference from the complete case!

### Example E: Grout (Holes)

**Code**: `1 + _ * 3` (where `_` represents an empty hole)

**Segment structure**:

```
[ Tile("1"), Tile("+"), Grout({shape: Convex}), Tile("*"), Tile("3") ]
```

**Convex grout** fills the position where an operand is expected. If an operator were missing instead:

**Code**: `1 _ 2` (missing operator)

```
[ Tile("1"), Grout({shape: Concave}), Tile("2") ]
```

**Concave grout** fills operator positions.

## Segment vs Term

| Aspect | Segment | Term |
|--------|---------|------|
| Operators | Flat list | Tree by precedence |
| Delimiters | Matched into tiles | Implicit in term structure |
| Holes | Explicit Grout pieces | EmptyHole / MultiHole nodes |
| Whitespace | Secondary pieces | Discarded (except in annotations) |

### Conversion Flow

```
Text -> Segment -> Skeleton -> Term
        ^             ^
        |             |
   tile-based     precedence
    editing        parsing
```

1. **Text to Segment**: Tile-based editing in the zipper
2. **Segment to Skeleton**: `Skel.mk` builds precedence structure
3. **Skeleton + Segment to Term**: `MakeTerm.go` produces the AST

## Key Files

| File | Purpose |
|------|---------|
| `src/haz3lcore/tiles/Base.re` | Core type definitions |
| `src/haz3lcore/tiles/Segment.re` | Segment operations (remold, regrout, skel, etc.) |
| `src/haz3lcore/tiles/Tile.re` | Tile operations, `is_complete`, `disassemble` |
| `src/haz3lcore/tiles/Piece.re` | Piece operations, `shapes`, `id` |
| `src/haz3lcore/tiles/Skel.re` | Skeleton construction (precedence parsing) |
| `src/haz3lcore/tiles/Grout.re` | Grout types and operations |
| `src/haz3lcore/tiles/Mold.re` | Mold types (shapes, sorts) |
| `src/haz3lcore/tiles/Label.re` | Label type (`list(Token.t)`) |
| `src/haz3lcore/lang/Form.re` | Language form definitions |
| `src/haz3lcore/lang/MakeTerm.re` | Segment-to-Term conversion |
| `src/util/Aba.re` | Alternating list utilities |

## Invariants

From the tile definition in `Base.re`:

```reason
type tile = {
  // invariants:
  // - length(mold.in_) + 1 == length(label)
  // - length(shards) <= length(label)
  // - length(shards) == length(children) + 1
  // - sort(shards) == shards
  ...
}
```

1. A label with N delimiters has N-1 bi-delimited regions (and thus N-1 child sorts)
2. Shards are a subset of label indices (incomplete tiles have fewer shards)
3. Children fill the gaps between shards (always one fewer than shards)
4. Shards are sorted (ordered left-to-right)

## Complete vs Incomplete Tiles

A tile is **complete** when `length(shards) == length(label)`:

```reason
/* From Tile.re */
let is_complete = (t: t) => List.length(t.label) == List.length(t.shards);
```

Incomplete tiles arise during editing when:
- User has typed `let x =` but not yet `in`
- User has typed `if cond then` but not yet `else`
- Delimiter matching is in progress

The `Segment.reassemble` function handles merging incomplete tile shards when they can be combined.
