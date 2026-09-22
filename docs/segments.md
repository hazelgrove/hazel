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
  form: Form.t,          /* which syntactic form: Compound(Let), Tok("x"), ... */
  sort: Sort.t,          /* the editor's local sort guess for this tile */
  shards: list(int),     /* which delimiters are present */
  children: list(segment), /* bi-delimited content between shards */
}
```

A tile stores its form and a sort; its label (the delimiter tokens) and
mold (shape and sort information) are *derived* from those two fields
(`Tile.label`, `Tile.mold`) rather than stored.

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

- **`form`**: Which form the tile is, from `src/language/grammar/FormId.re`:
  ```reason
  type t =
    | Compound(family)   /* a registered compound form: Let, Parens, Plus, ... */
    | Tok(Token.t)       /* a single free-text token: a variable, literal, hole, ... */
    | TokInfix(Token.t); /* a keyword prefix (`i` of `in`) in operator position */
  ```
  A `family` is one constructor per compound form up to sort (`Let`,
  `If`, `Parens`, `Comma`, `Cons`, ...); `Cons` at Exp and at Pat are
  the same family. Two families may share a spelling when they differ
  in shape: `["(", ")"]` is `Parens` (an operand) or `Ap` (a postfix
  application), `["-"]` is `Minus` or `UnaryMinus`.

- **`sort`**: The sort the editor guessed for the tile when it was
  classified or last remolded (`Exp`, `Pat`, `Typ`, ...). It is a
  local guess, not the parse's global answer; see
  [Why mold sorts are quotiented](#why-mold-sorts-are-quotiented).

- **`label`** (derived, `Tile.label`): The list of delimiter tokens, `Form.label_of(form)`.
  `Label.t` is `list(Token.t)`, defined in `src/language/grammar/Label.re`.
  - Single-token: `["+"]`, `["x"]`, `["123"]` (a `Tok` or a one-delimiter `Compound`)
  - Multi-token: `["let", "=", "in"]`, `["(", ")"]`, `["if", "then", "else"]`
  - `Tile.arity` is the label's length; `Tile.token(t, i)` is its `i`th delimiter.

- **`shards`**: Indices into the label indicating which delimiters are actually present.
  - Complete tile: `shards = [0, 1, 2]` for `["let", "=", "in"]`
  - Incomplete tile: `shards = [0, 1]` means only `let` and `=` are present (missing `in`)

- **`children`**: Segments between consecutive shards (bi-delimited content).
  - For `["let", "=", "in"]` with all shards: 2 children (pattern and definition)
  - Invariant: `length(children) == length(shards) - 1`

- **`mold`** (derived, `Tile.mold`): Shape information, `Form.mold_of(form, sort)`,
  from `src/haz3lcore/tiles/Mold.re`:
  ```reason
  type t = {
    out: Sort.t,           /* output sort (Exp, Pat, Typ, etc.) */
    in_: list(Sort.t),     /* sorts of bi-delimited children */
    nibs: (Nib.t, Nib.t),  /* left and right edge shapes */
  }
  ```
  Each family has one definition row per sort it inhabits
  (`Form.rows_of`), so `(form, sort)` picks a row. A tile is
  *well-sorted* when `mold.out == sort`; a family with no row at the
  stored sort (or a token no atomic class recognizes) gets a
  sort-`Any` fallback mold with no children.

### Nib Shapes

Nibs describe how pieces connect. From `src/haz3lcore/tiles/Nib.re`:

```reason
/* Nib.Shape.t */
type t =
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

(`Tile("1")` abbreviates a tile with `form: Tok("1")`; `Tile("+")` one
with `form: Compound(Plus)`.)

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
    form: Compound(Parens),   /* label ["(", ")"] */
    sort: Exp,
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
    form: Compound(Let),    /* label ["let", "=", "in"] */
    sort: Exp,
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
    form: Compound(Let),    /* label ["let", "=", "in"] */
    sort: Exp,
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

**Code**: `1 + · * 3` (where `·` marks a position where nothing was typed)

**Segment structure**:

```
[ Tile("1"), Tile("+"), Grout({shape: Convex}), Tile("*"), Tile("3") ]
```

**Convex grout** fills the position where an operand is expected. If an operator were missing instead:

**Code**: `1 · 2` (missing operator)

```
[ Tile("1"), Grout({shape: Concave}), Tile("2") ]
```

**Concave grout** fills operator positions.

Note that grout is distinct from the explicit hole token `?`: typing `?`
produces an ordinary single-token *tile*, while grout is synthesized by
`Segment.regrout` wherever adjacent nib shapes conflict and is never
typed. Both display as holes, but only grout is ephemeral (regrout
freely re-derives, moves, or removes it).

## Segment vs Term

| Aspect | Segment | Term |
|--------|---------|------|
| Operators | Flat list | Tree by precedence |
| Delimiters | Matched into tiles | Implicit in term structure |
| Holes | Explicit Grout pieces | EmptyHole / MultiHole nodes |
| Whitespace | Secondary pieces | Preserved in annotations (`IdTag.secondary`), restored on print |

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
| `src/language/grammar/FormId.re` | Form identities: `family` constructors and their labels |
| `src/language/grammar/Label.re` | Label type (`list(Token.t)`) |
| `src/language/grammar/Token.re` | Token classes (variables, literals, operators, holes) |
| `src/haz3lcore/lang/Form.re` | Per-family mold rows, classification (`classify_label`, `mold_of`) |
| `src/haz3lcore/lang/MakeTerm.re` | Segment-to-Term conversion |
| `src/util/Aba.re` | Alternating list utilities |

## Invariants

From the tile definition in `Base.re`:

```reason
type tile = {
  // invariants (arity = length(Form.label_of(form))):
  // - length(shards) <= arity
  // - length(shards) == length(children) + 1
  // - sort(shards) == shards
  ...
}
```

1. Shards are a subset of label indices (incomplete tiles have fewer shards)
2. Children fill the gaps between shards (always one fewer than shards)
3. Shards are sorted (ordered left-to-right)

A label with N delimiters has N-1 bi-delimited regions, so a
registered mold has N-1 child sorts; this holds by construction
(`Form.defs_of_rows` fails fast on a row whose `in_` disagrees with
its family's label) rather than per tile.

## Complete vs Incomplete Tiles

A tile is **complete** when every label index has a shard:

```reason
/* From Tile.re */
let is_complete = (t: t) => arity(t) == List.length(t.shards);
```

Incomplete tiles arise during editing when:
- User has typed `let x =` but not yet `in`
- User has typed `if cond then` but not yet `else`
- Delimiter matching is in progress

The `Segment.reassemble` function handles merging incomplete tile shards when they can be combined.

Before term construction, incomplete segments are given a *canonical
completion* (`CanonicalCompletion`, used by `MakeTerm.from_zip_for_sem`):
missing closers are appended, missing openers spliced at their skeleton
position, and missing middle delimiters filled in place, so every edit
state gets a well-formed term with full statics.

## Roundtripping Segments and Terms

Terms record enough provenance to print back the segment they came
from (`ExpToSegment`, `PreserveExact` settings). Two annotation fields
carry this (`IdTagged.IdTag.t`):

- `incomplete: list((Id.t, list(int)))` — for each canonically
  completed tile, which shards the user had actually typed; the
  printer strips the synthesized shards on the way out, so incomplete
  tiles survive the trip.
- `lexeme: option(string)` — surface spellings the term normalizes
  (`007`, `1e3`, `?` vs `??` hole flavor, quoted labels, unknown
  operators).

The roundtrip is the identity on text and on the piece sequence
(tile ids, labels, shards, secondary) modulo grout and re-derived mold
choices; it is enforced by a corpus plus an editor-action fuzzer in
`test/Test_ExpToSegment.re` and `test/Test_RoundtripFuzz.re`.

### Why grout is quotiented

Not for ids: measured on the action fuzzer (5 seeds, 290 passing
states, 2026-07-07), when grout placement matches, ids almost always
match too — synthesized completion grout derives its ids from the
tile id, and printed holes carry the term's rep id, which descends
from the buffer grout. The 4/290 id-only diffs traced to `pad_ids`
minting fresh ids when a multihole's recorded op ids came up one
short of its printed gap count; padding is now derived from the rep
id, so printing is a pure function of the term (the buffer grout id
still isn't recovered — nothing anchors to it). The quotient earns
its keep on *placement*: ~23% of
reachable states park grout where the parsed term doesn't reproduce
it (dangling boundary grout around linebreaks, stray convex grout
glommed through MakeTerm — `?()` prints back as `()` — typ-side
postfix parens, multihole gap counts). Grout placement is
re-derivable state, and deriving it for real (the virtual-grout
branch, #2165) removes it from the stored segment entirely, which
retires this quotient by construction rather than by tolerance.

### Why mold sorts are quotiented

A tile's stored `sort` — and so the mold derived from it — is the
editor's *local* guess at typing time; a parse derives sort *globally*. For incomplete tiles orphaned across a
sort boundary the two can legitimately disagree with no observable
difference. Minimal case (two keystrokes): type `:` then `]`. The
editor molds the orphan `]` at Typ — the caret sits on the typ side
of the `:` — but canonical completion splices the missing `[` at the
start, reading the state as `[? : ?]` (an Exp list whose *element* is
the ascription, not `? : [?]`):

```
term:     ListLit([Asc(EmptyHole, Unknown(Hole(EmptyHole)))])
stored:   tile "]"  out=Typ, in_=[Typ]
reprint:  tile "]"  out=Exp, in_=[Exp]
```

Same token, same convex shape, same visible text (the synthesized
opener is stripped on print); only the sort component differs, so
`Segment.equiv_mod_grout(~mold_sorts=false)` forgives sorts while
staying strict on nib shapes and precedences, which are observable (a
stranded `:` must reprint at Concave 24, not the Any fallback's
Concave 0). Wrong-sort *operands* need no forgiveness — `?:1` parses
to `Asc(EmptyHole, Unknown(Hole(Invalid "1")))` and the printer's
host-sort→Exp mold fallback reproduces the stored mold exactly.
Flipping the fuzzer to `~mold_sorts=true` enumerates the remaining
disagreement states: the work-list for eventually deriving sorts
instead of storing them.
