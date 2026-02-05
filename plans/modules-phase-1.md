# Hazel Modules: Phase 1 Implementation Plan

## Overview

This plan describes the implementation of a basic module system for Hazel. Phase 1 provides a **syntactic gloss over labeled tuples** - modules use ML-style syntax but are semantically equivalent to labeled tuples with nested `let`/`type` definitions.

### Goals
- Enable module syntax: `{ let a = 1; let b = 2; type T = Int }`
- Modules expand to nested definitions + labeled tuple: `let a = 1 in let b = 2 in type T = Int in (a=a, b=b)`
- Field access via existing dot notation: `M.a`
- No new type-checking or evaluation logic - piggyback on labeled tuples

### Non-Goals (Phase 1)
- Module signatures/types (Phase 1.5)
- Accessing type members via dot notation (Phase 2)
- Abstract types (Phase 2+)
- Custom evaluator logic for modules (Phase 2+)

---

## Background: Sorted Insertion

This work builds on the sorted-insertion branch which implements **sort-dependent expansion**. Multi-delimiter forms now only expand when inserted in an appropriate sort context. This enables:
- Same delimiter (`{` `}`) to mean different things in different sort contexts
- `let` and `type` keywords to expand differently in Mod sort vs Exp sort

The target syntax (from PR notes):
```
Exp ::= ...
  | "{" Mod "}"
Mod ::=
  | Mod ";" Mod
  | "type" TPat "=" Typ
  | "let" Pat "=" Exp
```

---

## Architecture Overview

### Key Files and Their Roles

| File | Purpose | Changes Needed |
|------|---------|----------------|
| `src/language/term/Sort.re` | Defines sort enum | Add `Mod` variant |
| `src/language/term/Grammar.re` | Term type definitions | Add `mod_t`, `mod_term`, update `exp_term` and `any_t` |
| `src/haz3lcore/lang/Form.re` | Syntactic form definitions | Add module forms, remove TEST_* forms |
| `src/haz3lcore/tiles/Segment.re` | Remolding (sort-aware restructuring) | Add `remold_mod`, `remold_mod_uni` |
| `src/haz3lcore/lang/MakeTerm.re` | Tile → Term parsing | Add module parsing logic |
| `src/language/statics/Statics.re` | Type checking | Add Module case with on-demand expansion |
| `src/language/statics/Elaborator.re` | Pre-dynamics transformation | Add Module case with expansion |
| (new) `src/language/statics/ExpandModule.re` | Module expansion function | New file |

### Data Flow

```
User Input → Tiles → Segment (remold) → MakeTerm → Term
                                                      ↓
                                              Statics (expand on-demand)
                                                      ↓
                                              Elaborator (expand for dynamics)
                                                      ↓
                                              Evaluator (standard evaluation)
```

---

## Term Structure

### Sort.re

```ocaml
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t =
  | Any
  | Pat
  | Typ
  | TPat
  | Rul
  | Exp
  | Mod;  // NEW

let root = Exp;

let to_string_verbose =
  fun
  | Any => "any"
  | Pat => "pattern"
  | TPat => "type pattern"
  | Typ => "type"
  | Rul => "rule"
  | Exp => "expression"
  | Mod => "module";  // NEW
```

### Grammar.re

Add to `any_t`:
```ocaml
and any_t('a) =
  | Exp(exp_t('a))
  | Pat(pat_t('a))
  | Typ(typ_t('a))
  | TPat(tpat_t('a))
  | Rul(rul_t('a))
  | Mod(mod_t('a))  // NEW
  | Any(unit)
```

Add new module types:
```ocaml
and mod_term('a) =
  | Invalid(string)
  | EmptyHole
  | MultiHole(list(any_t('a)))
  | ModLet(pat_t('a), exp_t('a))      // let binding: let pat = exp
  | ModType(tpat_t('a), typ_t('a))    // type alias: type T = typ
  | ModExp(exp_t('a))                  // bare expression (tests, side effects)
and mod_t('a) = Annotated.t(mod_term('a), 'a)
```

Add to `exp_term`:
```ocaml
| Module(list(mod_t('a)))  // { mod_item; mod_item; ... }
```

### Rationale

- **Flat list in Module**: Each `mod_t` is annotated with its own ID (from the `let`/`type` tile). The Module expression's annotation holds curly brace IDs + semicolon IDs.
- **ModExp for bare expressions**: Allows `{ let a = 1; test a == 1 end; let b = 2 }` - useful for inline tests.
- **No ModSeq**: We flatten semicolon chains in MakeTerm (like tuples flatten commas). The binary structure from parsing doesn't persist to terms.

---

## Form Definitions

### Form.re Changes

Remove TEST_* forms (they were for demonstrating sort-dependent expansion):
```ocaml
// REMOVE:
| TEST_Let
| TEST_TypeAlias
| TEST_Seq
| TEST_Curly
```

Add module forms to `compound_form`:
```ocaml
// ADD:
| ModBody      // { } wrapping Mod sort, producing Exp
| ModSeq       // ; infix in Mod sort
| ModLet       // let = in Mod sort
| ModType      // type = in Mod sort
| EmptyMod     // {} empty module (atomic)
```

Add to `get` function:
```ocaml
| ModBody => mk_op_c(LT, ["{", "}"], Exp, [Mod])
| ModSeq => mk_infix(";", Mod, P.mod_seq)
| ModLet => mk_pre_c'(L, ["let", "="], P.let_, Mod, [Pat], Exp)   // Note: uses mk_pre_c'
| ModType => mk_pre_c'(L, ["type", "="], P.let_, Mod, [TPat], Typ) // Note: uses mk_pre_c'
```

**Important**: ModLet and ModType use `mk_pre_c'` (not `mk_pre_c`) because they need heterogeneous sorts - the form's out sort is Mod, but the body (after `=`) is Exp/Typ. See "Implementation Notes" section for details.

Add to `atomic_form` and `get_atomic_form`:
```ocaml
// In atomic_form:
| EmptyMod

// In get_atomic_form:
| EmptyMod => (Token.is_empty_mod, [op(Exp)])  // need to add Token.is_empty_mod
```

### Form Structure Explanation

Reference: `src/haz3lcore/lang/Form.re` lines 38-73

```ocaml
type t = {
  label: Label.t,      // List of tokens, e.g. ["let", "="]
  mold: Mold.t,        // Structure: input sorts, output sort, nib shapes
  expansion,           // Non | L | LT (which delimiters trigger expansion)
}
```

For `ModLet = mk_pre_c'(L, ["let", "="], P.let_, Mod, [Pat], Exp)`:
- `L` = Leading delimiter triggers expansion
- `["let", "="]` = Two tokens
- `P.let_` = Precedence
- `Mod` = Output sort (the form IS a Mod item)
- `[Pat]` = Inner sorts (just the pattern between "let" and "=")
- `Exp` = Body sort (what comes after "=" is an expression)

**Note**: This uses `mk_pre_c'` which creates a heterogeneous prefix form where the body sort differs from the out sort. See "Implementation Notes" for why this is necessary.

---

## Remold Implementation

Reference: `src/haz3lcore/tiles/Segment.re` lines 98-449

The remold system handles sort-aware restructuring of tile segments. Each sort needs:
- `remold_[sort]`: Main function for that sort
- `remold_[sort]_uni`: "Unified" version that tracks shape and returns rest

### Pattern Analysis

From analyzing existing remold functions:

1. **Common structure**: All follow the same skeleton - switch on segment, handle Secondary/Grout/Projector/Tile
2. **Sort-specific**: The key difference is right-nib handling - which child sorts can be spawned
3. **Exp right nibs can spawn**: Pat, TPat, Typ, Rul
4. **Pat right nibs can spawn**: Typ
5. **Mod right nibs can spawn**: Pat (for let), TPat (for type), Exp (for definition bodies), Typ (for annotations)

### remold_mod Implementation

```ocaml
// Add to main remold switch:
let rec remold = (~shape=Nib.Shape.concave(), seg: t, s: Sort.t) =>
  switch (s) {
  | Any => seg
  | Typ => remold_typ(shape, seg)
  | Pat => remold_pat(shape, seg)
  | Exp => remold_exp(shape, seg)
  | Rul => remold_rul(shape, seg)
  | TPat => remold_tpat(shape, seg)
  | Mod => remold_mod(shape, seg)  // NEW
  }

// New function (~50-70 lines):
and remold_mod = (shape, seg: t): t =>
  switch (seg) {
  | [] => []
  | [hd, ...tl] =>
    switch (hd) {
    | Secondary(_) | Grout(_) => [hd, ...remold_mod(shape, tl)]
    | Projector(p) => [hd, ...remold_mod(snd(ProjectorCore.shapes(p)), tl)]
    | Tile(t) =>
      switch (remold_tile(Mod, shape, t)) {
      | None => [Tile(t), ...remold_mod(snd(Tile.shapes(t)), tl)]
      | Some(t) when !Tile.has_end(Right, t) =>
        let (_, r) = Tile.nibs(t);
        let remolded = remold(~shape=r.shape, tl, r.sort);
        [Tile(t), ...remolded];
      | Some(t) =>
        switch (Tile.nibs(t)) {
        | (_, {shape, sort: Pat}) =>
          let (remolded, shape, rest) = remold_pat_uni(shape, tl, [Mod]);
          [Piece.Tile(t), ...remolded] @ remold_mod(shape, rest);
        | (_, {shape, sort: TPat}) =>
          let (remolded, shape, rest) = remold_tpat_uni(shape, tl, [Mod]);
          [Piece.Tile(t), ...remolded] @ remold_mod(shape, rest);
        | (_, {shape, sort: Typ}) =>
          let (remolded, shape, rest) = remold_typ_uni(shape, tl, [Mod]);
          [Piece.Tile(t), ...remolded] @ remold_mod(shape, rest);
        | (_, {shape, sort: Exp}) =>
          let (remolded, shape, rest) = remold_exp_uni(shape, tl, [Mod]);
          [Piece.Tile(t), ...remolded] @ remold_mod(shape, rest);
        | _ => [Tile(t), ...remold_mod(snd(Tile.shapes(t)), tl)]
        }
      }
    }
  }

// Also need remold_mod_uni for when Mod appears as child of another sort
and remold_mod_uni = (shape, seg: t, parent_sorts): (t, Nib.Shape.t, t) =>
  // Similar structure to remold_mod but returns (remolded, shape, rest) triple
  ...
```

---

## MakeTerm Implementation

Reference: `src/haz3lcore/lang/MakeTerm.re`

### N-ary Semicolon Handling

Following the tuple pattern (lines 58-60, 474-490):

```ocaml
// Add near line 60:
let is_mod_seq = is_nary(Any.is_mod, ";");

// Add Any.is_mod to Any.re:
let is_mod = fun
  | Mod(m) => Some(m)
  | _ => None;
```

### Module Parsing

Add a `mod` function following the pattern of `exp`, `pat`, etc.:

```ocaml
and mod_ = unsorted => {
  let (term, inner_ids) = mod_term(unsorted);
  let ids = ids(unsorted) @ inner_ids;
  return(m => Mod(m), ids, IdTagged.mk(ids, get_secondary(ids), term));
}

and mod_term: unsorted => (Mod.term, list(Id.t)) = {
  let ret = (term: Mod.term) => (term, []);
  let hole = unsorted => Mod.hole(kids_of_unsorted(unsorted));

  fun
  | Op(tiles) as tm =>
    switch (tiles) {
    | ([(_, ([t], []))], []) when is_hole_label(t) => ret(hole(tm))
    | _ => ret(hole(tm))
    }

  | Pre(tiles, Mod(r)) as tm =>
    switch (tiles) {
    | ([(_id, t)], []) =>
      ret(
        switch (t) {
        | (["let", "="], [Pat(pat), Exp(def)]) => ModLet(pat, def)
        | (["type", "="], [TPat(tpat), Typ(typ)]) => ModType(tpat, typ)
        | _ => hole(tm)
        }
      )
    | _ => ret(hole(tm))
    }

  | Bin(Mod(l), tiles, Mod(r)) as tm =>
    switch (is_mod_seq(tiles)) {
    | Some(between_kids) =>
      // Flatten into list, collecting all items
      let items: list(Mod.t) = [l] @ between_kids @ [r];
      // Return as a "virtual" term - the Module wrapper comes from exp parsing
      // Actually, we need to think about this more carefully...
      ret(hole(tm))  // placeholder - see discussion below
    | None => ret(hole(tm))
    }

  | tm => ret(hole(tm))
}
```

### Expression Parsing for Module

In the `exp_term` function, add handling for `ModBody`:

```ocaml
// In Op(tiles) case, around line 307:
| (["{", "}"], [Mod(body)]) =>
  // Handle non-empty module
  // Need to flatten if body is a sequence
  let items = flatten_mod(body);
  ret(Module(items))

// For empty module (if we add EmptyMod atomic form):
| (["{}"], []) => ret(Module([]))
```

### Flattening Logic

```ocaml
// Helper to flatten ModSeq into list
let rec flatten_mod = (m: Mod.t): list(Mod.t) =>
  switch (m.term) {
  | ModLet(_, _) | ModType(_, _) | ModExp(_) => [m]
  | Invalid(_) | EmptyHole | MultiHole(_) => [m]
  // If we had ModSeq, we'd flatten here, but we're flattening at parse time
  };
```

### ID Handling

Reference: `src/haz3lcore/lang/MakeTerm.re` lines 112-115

```ocaml
let return = (wrap, ids, tm) => {
  map := TermMap.add_all(ids, wrap(tm), map^);
  tm;
};
```

For modules:
- **Curly brace IDs**: Go into the `Module` expression's annotation
- **Semicolon IDs**: Also collected into the `Module` expression's annotation (via `ids(unsorted)`)
- **Individual item IDs**: Each `mod_t` has its own annotation from the `let`/`type` tile

When expanding, the **curly brace ID becomes the parentheses ID** around the final tuple.

---

## Capitalized Names in Patterns

### Problem

Module names should be capitalized (like `M`), but capitalized identifiers in patterns are currently parsed as constructors.

### Solution (Phase 1)

**Allow standalone capitalized identifiers as pattern variables.**

Rule:
- `Ctr(args)` in pattern → constructor application
- `Ctr` standalone in pattern → variable binding

This means `let M = { ... }` works, but you lose the ability to match nullary constructors in let patterns. This is acceptable because:
1. Matching nullary constructors in `let` is marginal
2. `case` patterns still work correctly
3. We can refine later

### Implementation

In MakeTerm, when parsing patterns, don't turn standalone capitalized identifiers into `Constructor`:

```ocaml
// In pat_term, around the Ctr handling:
| ([t], []) when Token.is_ctr(t) =>
  // Only treat as constructor if it's applied, otherwise it's a variable
  // This case is for standalone - just treat as Var
  ret(Var(t))

// The applied case is handled by the Ap pattern matching
```

### Future Work (Phase 2+)

Consider:
- `module` keyword with dedicated sort for module binding names
- Context-aware parsing that knows about in-scope constructors

---

## Module Expansion

### Overview

The expansion transforms module syntax into standard Hazel expressions. It happens **on-demand** in:
1. **Statics**: When type-checking, expand and recurse into the expansion
2. **Elaborator**: When preparing for dynamics, expand permanently

### Expansion Algorithm

```ocaml
// New file: src/language/statics/ExpandModule.re

let expand_module = (items: list(Mod.t), curly_brace_ids: list(Id.t)): Exp.t => {
  // 1. Collect non-shadowed let bindings for the final tuple
  let non_shadowed = compute_non_shadowed_bindings(items);

  // 2. Build the labeled tuple body
  let tuple_body = build_labeled_tuple(non_shadowed, curly_brace_ids);

  // 3. Wrap with definitions from bottom to top (preserving order)
  List.fold_right(wrap_item, items, tuple_body);
}

and wrap_item = (item: Mod.t, body: Exp.t): Exp.t => {
  let item_ids = IdTagged.ids(item);
  switch (item.term) {
  | ModLet(pat, def) =>
    Let(pat, def, body) |> Exp.mk(~ids=item_ids)
  | ModType(tpat, typ) =>
    TyAlias(tpat, typ, body) |> Exp.mk(~ids=item_ids)
  | ModExp(e) =>
    // Bare expression becomes: let _ = e in body
    Let(Wild |> Pat.fresh, e, body) |> Exp.mk(~ids=item_ids)
  | Invalid(_) | EmptyHole | MultiHole(_) =>
    // Error cases - wrap in a way that preserves the error
    body  // or handle specially
  }
}

and compute_non_shadowed_bindings = (items: list(Mod.t)): list((string, Id.t)) => {
  // Walk items, tracking which names are bound
  // Return only the final binding for each name
  let rec go = (seen, items) =>
    switch (items) {
    | [] => []
    | [item, ...rest] =>
      switch (item.term) {
      | ModLet(pat, _) =>
        let names = Pat.bound_vars(pat);
        let seen' = List.fold_left((s, n) => StringSet.add(n, s), seen, names);
        // Add to result only if not shadowed later
        let later_names = collect_later_names(rest);
        let non_shadowed = List.filter(n => !StringSet.mem(n, later_names), names);
        let entries = List.map(n => (n, Pat.rep_id(pat)), non_shadowed);
        entries @ go(seen', rest)
      | ModType(_, _) | ModExp(_) => go(seen, rest)
      | _ => go(seen, rest)
      }
    };
  go(StringSet.empty, items)
}

and build_labeled_tuple = (bindings: list((string, Id.t)), curly_ids: list(Id.t)): Exp.t => {
  // (a=a, b=b, ...)
  let fields = List.map(((name, _id)) => {
    let label = Label(name) |> Exp.fresh;
    let value = Var(name) |> Exp.fresh;
    TupLabel(label, value) |> Exp.fresh
  }, bindings);

  switch (fields) {
  | [] => Tuple([]) |> Exp.mk(~ids=curly_ids)  // empty tuple
  | _ => Tuple(fields) |> Exp.mk(~ids=curly_ids)  // use curly brace IDs for parens
  }
}
```

### Statics Integration

In `Statics.re`, add to `uexp_to_info_map`:

```ocaml
| Module(items) =>
  // Expand and recurse
  let curly_ids = IdTagged.ids(uexp);
  let expanded = ExpandModule.expand_module(items, curly_ids);
  go(~ana, expanded, m)
```

### Elaborator Integration

In `Elaborator.re`, add to `elaborate`:

```ocaml
| Module(items) =>
  let curly_ids = IdTagged.ids(uexp);
  let expanded = ExpandModule.expand_module(items, curly_ids);
  elaborate(m, expanded)
```

---

## Expansion Examples

### Basic Module

```
{ let a = 1; let b = 2 }
```
Expands to:
```
let a = 1 in let b = 2 in (a=a, b=b)
```

Type: `(a=Int, b=Int)`

### With Type Definition

```
{ type T = Int; let x: T = 5 }
```
Expands to:
```
type T = Int in let x: T = 5 in (x=x)
```

Type: `(x=Int)` (or `(x=T)` before normalization)

### Shadowing

```
{ let a = 1; let a = 2; let b = a }
```
Expands to:
```
let a = 1 in let a = 2 in let b = a in (a=a, b=b)
```

Only the final `a` and `b` appear in the tuple (non-shadowed bindings).

### Bare Expression (Test)

```
{ let a = 1; test a == 1 end; let b = 2 }
```
Expands to:
```
let a = 1 in let _ = test a == 1 end in let b = 2 in (a=a, b=b)
```

### Nested Module

```
{ let M = { let x = 1 }; let y = M.x }
```
Expands to:
```
let M = (let x = 1 in (x=x)) in let y = M.x in (M=M, y=y)
```

### Empty Module

```
{ }
```
Expands to:
```
()
```

Type: `()`

### ID Mapping Diagram

For `{ let a = 1; let b = 2 }`:

```
Source:     { let a = 1 ;  let b = 2 }
IDs:        ^1           ^2          ^1 (paired)

Expansion:  let a = 1 in let b = 2 in (a=a, b=b)
                                      ^---------^
                                      Parens ID: 1 (from curly braces)

The tuple's ids field: [2] (semicolon IDs)
"let a = 1" tile ID → "let a = 1 in ..." term
"let b = 2" tile ID → "let b = 2 in ..." term
```

---

## Implementation Phases

### Phase 1.1: Syntax Foundation

**Goal**: Module syntax can be entered and displays correctly in the editor.

**Files**:
- `Sort.re`: Add `Mod` variant, update `to_string_verbose`
- `Grammar.re`: Add `mod_t`, `mod_term`, `Module` in exp_term, `Mod` in any_t
- `Form.re`: Remove TEST_* forms, add `ModBody`, `ModSeq`, `ModLet`, `ModType`
- `Segment.re`: Add `remold_mod`, `remold_mod_uni`, update main `remold` switch
- `TermBase.re`: Add `Mod` module with `map_term`, equals, fresh, etc.
- `Any.re`: Add `is_mod` function
- Various other files that enumerate sorts (grep for `| Exp =>` patterns)

**Verification**: Can type `{ let a = 1; let b = 2 }` and see proper tile structure.

### Phase 1.2: MakeTerm

**Goal**: Module syntax parses into proper term structure.

**Files**:
- `MakeTerm.re`: Add `is_mod_seq`, `mod_` function, `mod_term` function, handle ModBody in exp parsing
- `Any.re`: Add `is_mod`
- Pattern handling: Allow standalone capitalized identifiers as variables

**Verification**: Module terms appear correctly in term structure, IDs are properly assigned.

### Phase 1.3: Statics + Elaborator

**Goal**: Modules type-check correctly and evaluate.

**Files**:
- (new) `ExpandModule.re`: Module expansion function
- `Statics.re`: Add Module case with on-demand expansion
- `Elaborator.re`: Add Module case with expansion

**Verification**:
- Cursor inspector shows expected types
- `{ let a = 1 }.a` evaluates to `1`
- Nested modules work

### Phase 1.4: Testing

**Statics tests**:
- Basic module with lets
- Module with type definitions
- Shadowing behavior
- Empty module
- Nested modules
- Error cases (invalid syntax inside module)

**Dynamics tests**:
- Module evaluation
- Field access via dot notation
- Tests inside modules
- Nested module field access

---

## Phase 1.5: Module Signatures (Future)

Add signature sort for module types:

```
Typ ::= ...
  | "{" Sig "}"
Sig ::=
  | Sig ";" Sig
  | "type" TPat "=" Typ
  | "let" Pat              // just the pattern, no body
```

For Phase 1, module types are just labeled tuple types. Users can annotate:
```
let M: (a=Int, b=Int) = { let a = 1; let b = 2 }
```

---

## Phase 2: Full Module System (Future)

### Type Member Access

Allow `M.T` to access type members defined in module `M`.

Requires:
- Type-level dot accessor
- Module types that track type members
- More sophisticated expansion that preserves type information

### Custom Evaluator

Instead of expanding to labeled tuples, evaluate modules directly:
- `Module(items)` as a value
- Dot accessor that projects fields from module values
- Potentially more efficient for large modules

### Abstract Types

The full ML module system feature:
- Signatures can hide type implementations
- `type T` in signature without `= ...`
- Sealing: `M :> Sig` hides internals

---

## Notes and Considerations

### Why Labeled Tuples?

Hazel already has labeled tuples with:
- Field access via dot notation (`t.field`)
- Type-level support (`(a=Int, b=String)`)
- Sophisticated rearrangement logic (`LabeledTuple.re`)

Modules as labeled tuple gloss gives us field access "for free".

Reference: `src/language/term/LabeledTuple.re`

### Remold Complexity

The remold code has significant duplication across sorts. Each sort needs ~70-100 lines for `remold_*` and `remold_*_uni`. A table-driven approach is possible but adds complexity without much clarity gain. For now, we follow the existing pattern.

Reference: Analysis of `Segment.re` lines 98-449 showed:
- Common skeleton: piece-type dispatch loop
- Sort-specific: right-nib handling (which child sorts can spawn)
- Recommendation: Keep explicit form, document the nib behavior matrix

### Empty Module Form

Like `()` (empty tuple) and `[]` (empty list), we should have `{}` as an atomic form for empty modules. Without it, `{ }` (with hole) would be "a module with one unknown item" rather than "an empty module."

### Future: `module` Keyword

For readability, we may want to add:
```
module M = { ... } in ...
```

as an alias for `let M = { ... } in ...`. This doesn't require any semantic changes - just a new form that parses to the same term structure.

---

## Appendix: File Locations

Key files referenced in this plan:

```
src/language/term/Sort.re           - Sort enum
src/language/term/Grammar.re        - Term type definitions
src/language/term/TermBase.re       - Term utilities per sort
src/language/term/LabeledTuple.re   - Labeled tuple algorithms
src/haz3lcore/lang/Form.re          - Syntactic form definitions
src/haz3lcore/tiles/Segment.re      - Remold implementation
src/haz3lcore/lang/MakeTerm.re      - Tile to term parsing
src/language/statics/Statics.re     - Type checking
src/language/statics/Elaborator.re  - Pre-dynamics transformation
```

---

## Appendix: Grammar Reference

### Current Hazel Grammar (Relevant Excerpts)

From `Grammar.re`:

```ocaml
type exp_term('a) =
  | Let(pat_t('a), exp_t('a), exp_t('a))     // let p = e1 in e2
  | TyAlias(tpat_t('a), typ_t('a), exp_t('a)) // type T = t in e
  | Tuple(list(exp_t('a)))                    // (e1, e2, ...)
  | TupLabel(exp_t('a), exp_t('a))           // label = value
  | Dot(exp_t('a), exp_t('a))                // e.field
  ...
```

### Proposed Module Grammar

```
Mod ::=
  | ModLet(Pat, Exp)     // let p = e
  | ModType(TPat, Typ)   // type T = t
  | ModExp(Exp)          // e (bare expression)
  | Invalid | EmptyHole | MultiHole

Exp ::= ...
  | Module(list(Mod))    // { items }
```

### Expansion Transformation

```
Module([ModLet(p1,e1), ModType(tp,t), ModLet(p2,e2)])
  ↓ expand_module
Let(p1, e1,
  TyAlias(tp, t,
    Let(p2, e2,
      Tuple([TupLabel(Label(x1), Var(x1)), TupLabel(Label(x2), Var(x2))]))))
```

Where `x1`, `x2` are the non-shadowed variables bound by `p1`, `p2`.

---

## Implementation Notes (Phase 1.1 Completed)

This section documents what was actually implemented, key insights discovered, and deviations from the original plan.

### Status Summary

**Completed (Phase 1.1 - Syntax Foundation)**:
- ✅ Mod sort added to Sort.re
- ✅ Module forms defined in Form.re (ModBody, ModSeq, ModLet, ModType)
- ✅ Remold functions added to Segment.re (remold_mod, remold_mod_uni)
- ✅ MakeTerm parsing for modules with flattening
- ✅ CSS styling for Mod sort (greenish color)
- ✅ Stubs in dynamics/statics for Module expression

**Completed (Phase 1.2 - Module Expansion & Semantics)**:
- ✅ ExpandModule.re: Transform module syntax to nested let/type + labeled tuple
- ✅ Statics.re: Type-check modules by expanding and checking expanded form
- ✅ Elaborator.re: Elaborate modules directly to expanded form

**Completed (Phase 1.3 - Menhir Parser)**:
- ✅ AST.re: Added Module and mod_item types
- ✅ Lexer.mll: Added OPEN_CURLY and CLOSE_CURLY tokens
- ✅ Parser.mly: Added modItem rule and module expression grammar
- ✅ Conversion.re: Added ModItem conversion functions
- ✅ Grammar.re: Added mod_ type alias for Factory module

**Completed (Phase 1.4 - Testing)**:
- ✅ Test_Statics_Modules.re: 9 statics tests (1 skipped for nested modules)
- ✅ Test_Evaluator_Modules.re: 11 evaluator tests (2 skipped)
- ✅ All 1446 tests pass

**Not Implemented (Deferred)**:
- ❌ Empty module atomic form ({}) - not critical for Phase 1
- ❌ Nested module full support - marked as skip test

### Key Insight: Heterogeneous Prefix Forms

**Problem Discovered**: The original plan specified:
```ocaml
| ModLet => mk_pre_c(L, ["let", "="], P.let_, Mod, [Pat, Exp])
```

This is incorrect. For a 2-token prefix form, `inner_sorts` specifies:
- Sorts of children **between** tokens (1 slot for 2 tokens)
- The **body** sort (what comes after the prefix) is controlled by the right nib

With `mk_pre(p, out, in_)`, both nibs get sort `out`. But ModLet needs:
- Left nib: Mod (the form produces a Mod item)
- Right nib: Exp (the body after `=` is an expression)

**Solution**: Created `mk_pre'` and `mk_pre_c'` for heterogeneous prefix forms:

```ocaml
// In Mold.re:
let mk_pre' = (p, out, in_, sort_r) => {
  let l = Nib.{shape: Convex, sort: out};
  let r = Nib.{shape: Concave(p), sort: sort_r};  // Different sort!
  {out, in_, nibs: (l, r)};
};

// In Form.re:
let mk_pre_c' = (exp, label, prec, sort, inner_sorts, body_sort) =>
  mk(exp, label, Mold.mk_pre'(prec, sort, inner_sorts, body_sort));

// Correct form definitions:
| ModLet => mk_pre_c'(L, ["let", "="], P.let_, Mod, [Pat], Exp)
| ModType => mk_pre_c'(L, ["type", "="], P.let_, Mod, [TPat], Typ)
```

This follows the precedent of `mk_bin'` which already allows heterogeneous left/right sorts for binary operators.

### Key Insight: Sort Disambiguation at Heterogeneous Boundaries

**Problem**: At heterogeneous form boundaries, multiple sorts are simultaneously valid. For example, after `{ type t = Int`:
- **Local sort** (right nib of `Int`) = `Typ`
- **Parent sort** (bi-delimited region `{ }`) = `Mod`

When typing `;`, which sort should it use? The current design assumed one sort per insertion point, which breaks at these boundaries.

**Key Insight**: At heterogeneous boundaries, **two sorts are relevant**:
- **Local sort**: for extending the current content (e.g., `Int -> Bool`)
- **Parent sort**: for completing content and adding siblings (e.g., `Int; let x = 1`)

The token being inserted disambiguates which context applies.

**Important**: Only local and parent (bi-delimited region) sorts matter. Grandparents would be outside the bi-delimiter and can't affect inner sort resolution.

### Solution: Mod→Exp Fallback Pattern

We established a general **Mod→Exp fallback pattern**: when in Mod context and no Mod form exists, try Exp.

This handles two categories:
1. **Exp forms in Mod context**: `if`, `test`, function literals, etc. should expand inside `{ }`
2. **Bare expressions in Mod context**: `2+2` should remold with Exp molds

Additionally, there's a **semicolon special case**: when `;` is typed and parent sort is Mod, prefer Mod (for ModSeq over CellJoin).

**Why this is principled**:
- Mod is unique: it's the only sort where standalone items of another sort (Exp) are valid
- Priority is preserved: ModLet takes precedence over exp Let (checked first)
- It's explicit: only Mod→Exp, not Mod→Pat or Mod→Typ
- Semicolon special case matches intuition that `;` means "this item is done, next one coming"

**Decision Table**:

| Scenario | Local | Parent | Token | Result |
|----------|-------|--------|-------|--------|
| `{ type t = Int;` | Typ | Mod | `;` | Mod (semicolon special case) |
| `{ let x = 1;` | Exp | Mod | `;` | Mod (semicolon special case) |
| `{ if ...` | Mod | - | `if` | Exp (Mod→Exp fallback) |
| `1 + 2` (top level) | Exp | Exp | `+` | Exp (local first) |

**In Segment.re (remold_mod)**:
```ocaml
| Tile(t) =>
  switch (remold_tile(Mod, shape, t)) {
  | None =>
    /* No Mod form - try Exp since bare expressions are valid module items */
    switch (remold_tile(Exp, shape, t)) {
    | None => [Tile(t), ...remold_mod(snd(Tile.shapes(t)), tl)]
    | Some(t) =>
      let (remolded, shape, rest) = remold_exp_uni(snd(Tile.shapes(t)), tl, [Mod]);
      [Piece.Tile(t), ...remolded] @ remold_mod(shape, rest)
    }
  | Some(t) => ...
  }
```

**In Insert.re (effective_sort)**:
```ocaml
switch (Form.Expansion.try_get(local_sort, t)) {
| Some(_) => local_sort
| None =>
  /* In Mod context, try Exp since bare expressions are valid module items */
  if (local_sort == Sort.Mod) {
    switch (Form.Expansion.try_get(Exp, t)) {
    | Some(_) => Exp
    | None => parent_sort
    };
  } else {
    parent_sort;
  }
};
```

### Key Insight: MakeTerm Pattern Matching

For prefix forms, the tile's children list contains only what's **between** tokens. The body is a separate argument to the Pre constructor.

**Wrong pattern** (expected 2 children for 2-token form):
```ocaml
| Pre(([(_id, (["let", "="], [Pat(p), Exp(e)]))], []), Mod(_)) =>
```

**Correct pattern** (1 child between tokens, body separate):
```ocaml
| Pre(([(_id, (["let", "="], [Pat(p)]))], []), Exp(e)) =>
  ret(ModLet(p, e))
```

### Key Insight: Handling Expression-Level Structures in Mod

When parsing in Mod context, expression-level structures like `2+2` (which is `Bin(Exp, tiles, Exp)`) need to be wrapped as ModExp:

```ocaml
/* Expression-level structures - wrap as ModExp */
| Bin(Exp(_), _, Exp(_)) as tm => ret(ModExp(exp(tm)))
| Pre(_, Exp(_)) as tm => ret(ModExp(exp(tm)))
| Post(Exp(_), _) as tm => ret(ModExp(exp(tm)))
```

### Precedent: Rul Sort Fallback

The Mod→Exp fallback pattern has precedent in Form.Expansion.get for Rul sort:

```ocaml
| Rul =>
  /* Rul context: fall back to any expansion since rules contain
     Exp/Pat operands but have no direct operand forms. */
  let any_match = sorted_expansions |> List.find_opt(((tok, _, _, _)) => tok == t);
  ...
```

Both Rul and Mod are "container" sorts that hold items of other sorts, requiring fallback logic.

### Files Modified (Phase 1.1)

| File | Changes |
|------|---------|
| `src/language/term/Sort.re` | Added `Mod` variant |
| `src/language/term/Grammar.re` | Added `mod_term`, `mod_t`, `Module` in exp_term |
| `src/language/term/TermBase.re` | Added Mod module with map_term, etc. |
| `src/language/term/Mod.re` | New file with Mod utilities |
| `src/language/term/Any.re` | Added `is_mod` function |
| `src/language/term/Equality.re` | Added mod equality |
| `src/haz3lcore/lang/Form.re` | Added module forms, `mk_pre_c'` helper |
| `src/haz3lcore/tiles/Mold.re` | Added `mk_pre'` for heterogeneous prefix |
| `src/haz3lcore/tiles/Segment.re` | Added `remold_mod`, `remold_mod_uni`, Exp fallback |
| `src/haz3lcore/lang/MakeTerm.re` | Added module parsing with flattening |
| `src/haz3lcore/lang/Precedence.re` | Added `mod_seq` precedence |
| `src/haz3lcore/zipper/action/Insert.re` | Added Mod→Exp expansion fallback |
| `src/web/www/style/variables.css` | Added `--MOD` color variable |
| `src/web/www/style/editor.css` | Added mod sort styling |
| Various statics/dynamics files | Added stub cases for Module |

### Remaining Work: Detailed Phase Breakdown

#### Phase 1.2: Module Expansion & Semantics

1. **Module Expansion** (`ExpandModule.re`):
   - Transform `{ let a = 1; let b = 2 }` → `let a = 1 in let b = 2 in (a=a, b=b)`
   - Handle shadowing (only non-shadowed bindings in final tuple)
   - Handle bare expressions (wrap in `let _ = ...`)
   - Handle type definitions (expand to TyAlias)

2. **Statics Integration** (`Statics.re`):
   - Add Module case that expands and recurses
   - Modules should type-check via their expansion

3. **Elaborator Integration** (`Elaborator.re`):
   - Add Module case that expands for evaluation
   - Ensure proper ID mapping for cursor info

#### Phase 1.3: Menhir Parser

1. **Parser Updates** (`src/menhirParser/`):
   - Add module syntax to Menhir grammar
   - Add conversion for module terms in `Conversion.re`
   - Ensure Menhir ↔ MakeTerm equivalence

#### Phase 1.4: Testing

Testing should cover multiple layers:

1. **Parsing Tests** (`test/Test_Module.re` or extend `Test_Menhir.re`):
   - String → Term parsing for module syntax
   - Menhir ↔ MakeTerm equivalence for modules
   - Roundtrip tests (code → parse → serialize → parse)

2. **Statics Tests** (`test/statics/Test_Statics_Modules.re`):
   - Module type inference (`{ let a = 1 }` has type `(a=Int)`)
   - Module field access typing (`M.x`)
   - Nested module typing
   - Error cases (unbound variables in modules, etc.)

3. **Dynamics Tests** (`test/evaluator/Test_Evaluator_Modules.re`):
   - Module evaluation (`{ let a = 1 }.a` evaluates to `1`)
   - Nested module evaluation
   - Bare expressions in modules (side effects, tests)
   - Shadowing behavior

4. **Grammar Factory** (`test/Test_Grammar.re`):
   - Add samples for all Mod.cls variants
   - Ensure module factory functions work

5. **Update Test Runner** (`test/haz3ltest.re`):
   - Include new module test suites

#### Phase 1.5: Polish & Exploration

1. **Empty Module Atomic Form**:
   - Add `{}` as atomic form for empty modules
   - Currently `{ }` creates module with hole; need true empty

2. **ExplainThis Documentation** (if appropriate):
   - Add explanations for module syntax forms
   - Check `src/web/app/explainthis/` for patterns

3. **Other System Integration**:
   - Check if any other systems need module support
   - Look for exhaustiveness warnings or stub cases that need filling

#### Phase 1.6: Final Report

After implementation, document:
1. **What was implemented**: Summary of all changes
2. **What works**: Verified functionality with test results
3. **What doesn't work**: Any known issues or skipped tests
4. **What was skipped**: Features deemed unnecessary or deferred
5. **Recommendations**: Suggestions for future work

### Future Considerations

**Universal Semicolon**: An alternative approach would be a single sort-polymorphic `;` operator that works at whatever level makes sense contextually. This would eliminate the CellJoin/ModSeq ambiguity entirely. Challenges:
- Hazel's tiles have concrete sorts; `Any` sort behavior isn't truly polymorphic
- Would need sort-dispatching at MakeTerm phase
- Larger architectural change

Could be revisited if we move toward more semicolon-separated syntax throughout Hazel.

**Removing CellJoin**: If expression-level `;` (CellJoin) isn't needed, removing it eliminates the semicolon ambiguity entirely. Worth considering as module syntax matures.
