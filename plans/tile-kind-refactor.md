# Tile Kind Refactor: Replacing label+mold with Form Identifiers

## Problem Statement

The current `tile` record in `Base.re` stores both `label` (list of delimiter tokens) and `mold` (structural/precedence info) directly on every tile:

```ocaml
type tile = {
  id: Id.t,
  label: Label.t,    // e.g., ["let", "=", "in"]
  mold: Mold.t,      // contains precedence in Concave(p) shapes
  shards: list(int),
  children: list(segment),
}
```

**Issues with this design:**

1. **Migration burden**: Changing operator precedence requires migrating all serialized syntax because `mold` is stored on tiles. This recently came up with the comma precedence fix.

2. **Redundancy**: For compound forms, `label` and `mold` are fully determined by which form the tile represents. Storing both is redundant.

3. **Illegal states**: Nothing prevents a tile from having a label/mold mismatch.

4. **Verbose serialization**: Every tile serializes full label strings and mold records.

5. **String pattern matching**: Code throughout the codebase pattern matches on label strings (e.g., `| Tile({label: ["case", "end"], _})`), which is slower and less readable than enum matching.

6. **Structural redundancy for atomic forms**: Atomic tiles always have `shards = [0]` and `children = []`, yet we store these fields.

## Proposed Solution

Replace the homogeneous `tile` record with a discriminated union that separates atomic and compound forms:

```ocaml
type tile =
  | Atomic({id: Id.t, form: atomic_form, token: Token.t})
  | Compound({id: Id.t, form: compound_form, shards: list(int), children: list(segment)})
```

**Key changes:**
- Atomic forms carry only what they need: id, form variant, and token text
- Compound forms carry structural data: shards and children
- Both `label` and `mold` are derived from the form variant
- No separate `Unknown` variant at tile level - folded into `atomic_form`

## Design Details

### Atomic Form Type (Expanded)

The `atomic_form` type expands to include sort information in each variant, paralleling how `compound_form` works (e.g., `ConsExp` vs `ConsPat`). This is necessary because many atomic forms can appear in multiple sorts (Exp, Pat, Typ, TPat), and remolding selects which one.

```ocaml
type atomic_form =
  // Var - Exp or Pat
  | VarExp | VarPat
  // Literals - Exp or Pat
  | IntLitExp | IntLitPat
  | FloatLitExp | FloatLitPat
  | BoolLitExp | BoolLitPat
  | StringExp | StringPat
  | UndefinedExp | UndefinedPat
  | LivelitNameExp | LivelitNamePat
  // Quoted labels - Exp, Pat, or Typ
  | QuotedLabelExp | QuotedLabelPat | QuotedLabelTyp
  // Constructors - Exp or Pat
  | CtrExp | CtrPat
  // Empty collections - varying sorts
  | EmptyListExp | EmptyListPat
  | EmptyTupleExp | EmptyTuplePat | EmptyTupleTyp
  // Holes - all sorts
  | ExplicitHoleExp | ExplicitHolePat | ExplicitHoleTyp | ExplicitHoleTPat
  | LLMHoleExp | LLMHolePat | LLMHoleTyp | LLMHoleTPat
  // Projector invoke - all sorts
  | ProjectorInvokeExp | ProjectorInvokePat | ProjectorInvokeTyp | ProjectorInvokeTPat
  // Infix delimiter prefix - all sorts
  | InfixDelimiterPrefixExp | InfixDelimiterPrefixPat | InfixDelimiterPrefixTyp | InfixDelimiterPrefixTPat
  // Single-sort forms
  | Wild              // Pat only
  | Deferral          // Exp only
  | ExplicitNonlabel  // Typ only
  | TyVar             // Typ only
  | TyVarP            // TPat only
  | BaseType          // Typ only
  // Unknown tokens (fallback for unrecognized tokens)
  | UnknownOperator   // Concave nibs, Any sort
  | UnknownOperand    // Convex nibs, Any sort
```

This expands from ~21 variants to ~49 variants, but each variant now fully determines its mold (including sort).

### Compound Form Type

The existing `compound_form` (85 variants) remains unchanged. Each variant already fully determines its label and mold.

### Derived Accessors

```ocaml
let label_of = (t: tile): Label.t =>
  switch (t) {
  | Atomic({form, token, _}) =>
      // For atomic forms, label is always [token]
      [token]
  | Compound({form, _}) =>
      (Form.get(form)).label
  }

let mold_of = (t: tile): Mold.t =>
  switch (t) {
  | Atomic({form, _}) =>
      Form.get_atomic_mold(form)  // new function mapping variant to mold
  | Compound({form, _}) =>
      (Form.get(form)).mold
  }

let id_of = (t: tile): Id.t =>
  switch (t) {
  | Atomic({id, _}) => id
  | Compound({id, _}) => id
  }
```

### Shards and Children

For compound forms, shards and children work as before:
- `shards`: list of delimiter indices that are present
- `children`: segments between present delimiters
- Invariant: `len(children) == len(shards) - 1`

For atomic forms, these concepts don't exist - atomic tiles are always "complete" with their single token.

### Backpack

Shards (incomplete multi-delimiter tiles) are always compound forms:

```ocaml
// A shard in the backpack
Compound({
  id: parent_tile_id,
  form: Let,           // same form as parent
  shards: [0],         // just the "let" delimiter
  children: []
})
```

The `backpack_find` function searches for shards whose effective label matches a token, which still works since we can derive the label from the form.

## Example: `let x = 1 + 2 in x`

### Current Representation (verbose)

```ocaml
[(Tile
    { id = Option.get(Haz3lcore.Id.of_string("09a28dbf-..."));
      label = ["let"; "="; "in"];
      mold = { out = Exp; in_ = [Pat; Exp]; nibs = ... };
      shards = [0; 1; 2];
      children = [[...pattern...]; [...definition...]] });
  ...
  (Tile
     { id = Option.get(Haz3lcore.Id.of_string("ede5360c-..."));
       label = ["x"];
       mold = { out = Exp; in_ = []; nibs = ({shape = Convex; ...}, ...) };
       shards = [0]; children = [] })
]
```

### New Representation (compact)

```ocaml
[(Compound
    { id = Id.v("09a28dbf-84f1-4be0-8643-e66a7d88900f");
      form = Let;
      shards = [0; 1; 2];
      children =
      [[(Secondary { id = Id.v("6da5cc3a-..."); content = Whitespace(" ") });
         (Atomic { id = Id.v("89be0dce-..."); form = VarPat; token = "x" });
         (Secondary { id = Id.v("a668602b-..."); content = Whitespace(" ") })
        ];
        [(Secondary { id = Id.v("ce1b077d-..."); content = Whitespace(" ") });
          (Atomic { id = Id.v("847b2d6f-..."); form = IntLitExp; token = "1" });
          (Secondary { id = Id.v("4aea21fa-..."); content = Whitespace(" ") });
          (Compound { id = Id.v("b077d730-..."); form = Plus; shards = [0]; children = [] });
          (Secondary { id = Id.v("e8a3e252-..."); content = Whitespace(" ") });
          (Atomic { id = Id.v("ecc64329-..."); form = IntLitExp; token = "2" });
          (Secondary { id = Id.v("5673cdec-..."); content = Whitespace(" ") })
        ]
      ]
    });
  (Secondary { id = Id.v("278969fd-..."); content = Whitespace(" ") });
  (Atomic { id = Id.v("ede5360c-..."); form = VarExp; token = "x" })
]
```

**Improvements visible:**
- `VarPat` vs `VarExp` distinguishes pattern vs expression context
- No `shards`/`children` on atomic tiles
- `label` and `mold` fields eliminated entirely
- `Id.v(...)` much shorter than `Option.get(Haz3lcore.Id.of_string(...))`

## Benefits

### 1. No Migration for Precedence Changes

When precedence changes (e.g., comma precedence fix), only `Form.get` needs updating. Serialized tiles store `Compound(CommaExp)`, not the precedence value. The mold is derived at runtime.

### 2. Smaller Serialization

Significant size reduction per tile:
- Atomic: ~60% smaller (no label list, no mold record, no shards/children)
- Compound: ~40% smaller (no label list, no mold record)

### 3. Illegal States Unrepresentable

- Can't have label/mold mismatch (both derived from form)
- Can't have atomic tile with children
- Can't have wrong sort for a form (sort encoded in variant)

### 4. Faster Pattern Matching

Current (string list comparison):
```ocaml
| Tile({label: ["case", "end"], _}) => ...
```

Proposed (enum tag comparison):
```ocaml
| Compound({form: Case, _}) => ...
```

### 5. MakeTerm Performance

Form classification (regex matching for atomic forms) moves from MakeTerm (runs on whole segment every edit) to edit time (runs only on affected tokens).

### 6. Clearer Semantics

The tile explicitly declares what form it is. No ambiguity about sort or structure.

## Implementation Considerations

### Code Changes Guided by Type System

The discriminated union will cause compile errors at every `tile` access site:

1. **Base.re** - Type definition
2. **Tile.re** - All operations need case analysis
3. **Piece.re** - Tile wrapping/unwrapping
4. **Insert.re** - Tile creation
5. **Segment.re** - Remolding
6. **MakeTerm.re** - Big switch becomes form-based
7. **Code.re, Highlight.re, Arms.re** - Rendering
8. **~30 pattern match sites** - Label matches → form matches

### Pattern Match Conversion

```ocaml
// Before
| Tile({label: ["case", "end"], shards: [0], _}) => ...
| Tile({label: ["let" | "type", ..._], _}) => ...

// After
| Compound({form: Case, shards: [0], _}) => ...
| Compound({form: Let | TypeAlias, _}) => ...
```

### String-Encoded Hazel Syntax

The .ml files in `src/web/init/docs/` and `src/b2t2/slides/` contain s-expression serialized segments. These need migration.

**Migration approach:**
1. Parse old s-expression format
2. Transform each tile: read `label`/`mold`, determine form variant
3. Output new format, preserving IDs

### UUID Preservation

Critical: The refractors map uses tile IDs for probe positions. Migration must preserve IDs.

## Migration Phases

### Phase 1: Core Type Changes
1. Expand `atomic_form` with per-sort variants
2. Define new `tile` discriminated union
3. Add derived accessors (`label_of`, `mold_of`, `id_of`)

### Phase 2: Update Tile Operations
1. Update Tile.re with case analysis
2. Update Insert.re tile creation
3. Update Segment.re remolding

### Phase 3: Update Pattern Matches
1. Convert ~30 label pattern matches to form matches
2. Update MakeTerm.re

### Phase 4: Update Serialization
1. Add sexp/yojson derivers for new types
2. Write and test migration script
3. Migrate .ml files

### Phase 5: Cleanup
1. Remove any compatibility code
2. Update tests

## References

- `src/haz3lcore/tiles/Base.re` - Current tile type
- `src/haz3lcore/lang/Form.re` - Form definitions
- `src/haz3lcore/tiles/Mold.re` - Mold type
- `src/haz3lcore/lang/MakeTerm.re` - Term construction
- `src/haz3lcore/zipper/action/Insert.re` - Tile creation
- `src/haz3lcore/tiles/Segment.re` - Remolding logic

---

## Appendix A: ID Serialization Improvement

### Problem

The current `Id.pp` and `Id.show` functions output verbose compilable OCaml:

```ocaml
Option.get(Haz3lcore.Id.of_string("09a28dbf-84f1-4be0-8643-e66a7d88900f"))
```

This is 70+ characters per ID.

### Solution

Add a shorter constructor function:

```ocaml
// In Id.re
let v: string => t = s =>
  Uuidm.of_string(s) |> OptUtil.get(_ => failwith("Id.v: invalid UUID: " ++ s));

let pp = (f, id) => Format.fprintf(f, "Id.v(\"%s\")", to_string(id));
let show = id => Format.sprintf("Id.v(\"%s\")", to_string(id));
```

Output becomes:
```ocaml
Id.v("09a28dbf-84f1-4be0-8643-e66a7d88900f")
```

47 characters - about 33% shorter. In contexts with `open Haz3lcore`, this works directly. Otherwise `Haz3lcore.Id.v(...)` is still much shorter than the current format.

### Implementation

1. Add `Id.v` function to `src/util/Id.re`
2. Update `pp` and `show` to use the shorter format
3. Regenerate any files that use `show` output

---

## Appendix B: Potential PPX for ID Literals

For even cleaner ID literals, a PPX extension could provide compile-time validated UUIDs:

```ocaml
[%id "09a28dbf-84f1-4be0-8643-e66a7d88900f"]
```

### Benefits
- Validates UUID format at compile time
- Cleanest possible syntax
- No runtime parsing overhead

### Drawbacks
- Requires writing and maintaining a PPX
- Adds build complexity
- May not be worth it for this use case alone

### Recommendation

The `Id.v` approach (Appendix A) provides most of the benefit with minimal effort. PPX could be a future enhancement if we find more use cases for compile-time validation.

---

## Appendix C: Future Considerations

### Infix Operators as Mono-Tiles

Infix operators (Plus, Minus, etc.) are classified as `Compound` because they're defined in `compound_form`, but structurally they always have `shards = [0]` and `children = []`. They're "compound" in the term sense (binary operators with two children) but "atomic" in the segment sense (single token, no segment-level children).

**Possible refinement:** A third tile category for infix operators:
```ocaml
type tile =
  | Atomic({id: Id.t, form: atomic_form, token: Token.t})
  | Infix({id: Id.t, form: infix_form})
  | Delimited({id: Id.t, form: delimited_form, shards: list(int), children: list(segment)})
```

**Current recommendation:** Not worth the complexity. The slight awkwardness of compound tiles with empty children is acceptable, and the current categorization reflects term-level structure which is meaningful.

### Shards/Children Coupling

The `shards` and `children` fields have tight invariants:
- `len(children) == len(shards) - 1`
- `shards` is sorted, unique, elements in `[0, len(form.label) - 1]`

The codebase already uses `Aba.t` (alternating sequence) internally to represent this coupling. We could surface it in the type:

```ocaml
type compound_content = Aba.t(int, segment)  // shard, child, shard, child, ..., shard

type tile =
  | Atomic({id: Id.t, form: atomic_form, token: Token.t})
  | Compound({id: Id.t, form: compound_form, content: compound_content})
```

**Benefits:** Eliminates length-mismatch illegal states.

**Current recommendation:** Nice-to-have but not essential. The shard index validity (in range, sorted) would still need runtime checking regardless. Could be a follow-up refinement.
