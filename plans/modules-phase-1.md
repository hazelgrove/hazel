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

## Capitalized Module Names: Design Considerations

**Status: NOT IMPLEMENTED - Decision Required**

This section documents the design space for allowing capitalized module names (e.g., `let M = { ... }` and `M.x`). The original plan proposed a partial solution that was never implemented, and upon further analysis, that solution was incomplete.

### The Problem: Two Sides

Capitalized identifiers are currently parsed as constructors at the token/form level. This affects TWO places:

**1. Pattern/Binding Side:**
```
let M = { let x = 1 }
    ^-- M is parsed as Constructor("M", None), not Var("M")
        Statics rejects this because constructors can't be bound
```

**2. Expression/Reference Side:**
```
M.x
^-- M is parsed as Constructor("M", None), not Var("M")
    Statics tries to look up M as a constructor, fails
```

Both sides must be addressed for capitalized modules to work.

### Current Implementation State

In `Token.re`:
```reason
let is_var = match("^[a-z_][A-Za-z0-9_']*$" | qualified)  // lowercase
let is_ctr = match("^[A-Z][A-Za-z0-9_]*$")                 // capitalized
```

In `Form.re`:
```reason
| Var => (Token.is_var, [op(Exp), op(Pat)])  // lowercase → Var form
| Ctr => (Token.is_ctr, [op(Exp), op(Pat)])  // capitalized → Ctr form
```

In `MakeTerm.re` (both exp and pat):
```reason
| ([t], []) when Token.is_var(t) => ret(Var(t))
| ([t], []) when Token.is_ctr(t) => ret(Constructor(t, None))
```

The distinction happens at **token level** and propagates through forms and terms.

### Why the Original Proposal Was Incomplete

The original plan proposed changing only patterns:
```reason
// Proposed: standalone Ctr in patterns → Var
| ([t], []) when Token.is_ctr(t) => ret(Var(t))
```

This would fix `let M = { ... }` but NOT `M.x` in expressions. The expression side was overlooked.

Additionally, this change would break nullary constructor matching in ALL patterns:
```reason
case x | None => 1    // None would become Var("None"), not Constructor
```

### Design Options

#### Option 1: Status Quo (Lowercase Modules)

Use lowercase names: `let m = { let x = 1 } in m.x`

**Pros:**
- No implementation changes needed
- Consistent with "modules are first-class values"
- Avoids constructor ambiguity entirely

**Cons:**
- Doesn't match OCaml/ML convention
- Less visual distinction for organizational structures
- May feel inconsistent to users from ML background

#### Option 2: Unify Var/Constructor at Term Level

Replace separate `Var(string)` and `Constructor(string, ...)` with unified `Name(string)`. Statics resolves meaning from context.

**Implementation:**
1. Change `Grammar.re`: Replace `Var`/`Constructor` with `Name` in exp_term and pat_term
2. Change `MakeTerm.re`: Both `is_var` and `is_ctr` tokens create `Name(t)`
3. Change `Statics.re`: When encountering `Name(n)`:
   - Look up in variable context
   - Look up in constructor context
   - Use type information to disambiguate

**Concrete behavior:**
```
let M = { let x = 1 }   // M is Name, statics sees module type → binding
M.x                      // M is Name, statics resolves to bound module
case x | None => 1       // None is Name, statics sees sum type → constructor
case x | Some(v) => v    // Some(v) is Ap, statics resolves Some as constructor
```

**Pros:**
- Maximum flexibility
- Context determines meaning (more principled)
- Single identifier representation

**Cons:**
- Significant refactor (touches Grammar, MakeTerm, Statics, tests)
- Cursor inspector can't show "Constructor" vs "Variable" until after statics
- Potential for confusing error messages when context is ambiguous

#### Option 3: Extend `is_var` to Include Capitalized

Simpler variant of Option 2: just change the token regex so capitalized names are also "variables."

**Implementation:**
1. Change `Token.re`: `is_var` matches both lowercase and capitalized
2. Remove or repurpose `Ctr` form
3. MakeTerm creates `Var(t)` for all identifiers
4. Statics determines if a `Var` is actually a constructor reference

**Pros:**
- Smaller change than full unification
- Keeps `Var` term variant

**Cons:**
- `Var` becomes a misnomer (it's really "Name")
- Still need statics changes for constructor lookup
- Cursor inspector shows "Variable" for constructors (confusing)

#### Option 4: Bidirectional Resolution with Capitalization Hints

Parse as unified names, but statics uses capitalization + scope as disambiguation hints:

1. If capitalized AND in scope as constructor → treat as constructor
2. If capitalized AND NOT in scope as constructor → treat as variable
3. If lowercase → always variable (can warn if shadows constructor)

**Pros:**
- Liberal parsing, stricter semantics
- Can provide helpful warnings about naming conventions

**Cons:**
- Complex resolution rules
- Behavior depends on what's in scope (potentially confusing)

#### Option 5: Keep Distinction, Add Module-Specific Handling

Keep Var/Constructor separate but add special cases:

1. In patterns: Context-aware parsing (let patterns vs case patterns)
2. In expressions: Allow Constructor to resolve to variable if not found as constructor

**Pros:**
- Minimal structural changes
- Targeted fixes

**Cons:**
- Ad-hoc, multiple special cases
- Harder to reason about

### Trade-offs Summary

| Concern | Option 1 | Option 2 | Option 3 | Option 4 | Option 5 |
|---------|----------|----------|----------|----------|----------|
| Implementation effort | None | High | Medium | High | Medium |
| Conceptual cleanliness | High | High | Medium | Medium | Low |
| OCaml convention | No | Yes | Yes | Yes | Partial |
| Constructor handling | Clean | Context-based | Context-based | Heuristic | Special-cased |
| Cursor inspector accuracy | Accurate | Post-statics | Misleading | Post-statics | Mixed |

### Considerations

**Aesthetic/Convention:**
- OCaml uses capitalized module names - familiar to ML users
- Capitalization visually distinguishes "big" organizational units
- But: if modules are first-class, why treat them specially?

**Information Design:**
- Currently: cursor inspector shows "Constructor" or "Variable" based on capitalization (pre-statics)
- With unification: would show generic "Identifier" pre-statics, resolved info post-statics
- Question: Is pre-statics classification useful or misleading?

**Scope of Change:**
- Option 1 requires no changes but limits naming
- Options 2-4 require touching multiple layers (Token, Form, MakeTerm, Statics, Cursor Inspector)
- All options except 1 need careful handling of existing constructor semantics

### Recommendation

**For Phase 1:** Use lowercase module names (Option 1). This lets us ship working modules without resolving the naming question.

**For Phase 1.5 or 2:** Revisit with Option 2 (full unification) or Option 3 (extend is_var). These are the cleanest solutions that address both sides of the problem.

The key insight is that any solution supporting capitalized modules MUST address both the pattern/binding side AND the expression/reference side. Partial solutions that only fix patterns are incomplete.

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
- ✅ Test_Statics_Modules.re: 14 statics tests (all pass)
- ✅ Test_Evaluator_Modules.re: 11 evaluator tests (2 skipped for Menhir grammar conflict)
- ✅ Test_MakeTerm.re: Module parsing tests including nested modules
- ✅ All 1461 tests pass

**Not Implemented (Deferred)**:
- ❌ Empty module atomic form (`{}`) - compound form `["{", "}"]` used instead
- ❌ Menhir multi-item modules with semicolons - grammar conflict with Seq in exp (see Known Limitations)

**Bugs Fixed**:
- ✅ Singleton labeled tuple elaboration for patterns with Unknown synth type (see "Singleton Labeled Tuple Bug Fix" section)

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

---

## Singleton Labeled Tuple Bug Fix

### The Bug

When a Var pattern is analyzed against a singleton labeled tuple type (e.g., `(y=Int)`), the pattern was incorrectly elaborated even when it shouldn't be, causing the variable to have the inner type (`Int`) instead of the full tuple type (`(y=Int)`).

**Reproducer (no module syntax needed):**
```
let m = (y=1) in m
```
- Expected type of `m`: `(y=Int)`
- Actual type before fix: `Int`

This bug was discovered during module testing because modules with single bindings produce singleton labeled tuple types.

### Root Cause

In `src/language/statics/Statics.re`, lines 1941-1954 (pattern singleton tuple handling):

```reason
switch (Typ.weak_head_normalize(ctx, ana).term) {
| Prod([{term: TupLabel({term: Label(l1), _}, ana_ty), _}]) =>
  let (e, m) = go(~ana=syn, ~ctx, upat, m);

  switch (Typ.weak_head_normalize(ctx, e.ty).term) {
  | Prod([{term: TupLabel({term: Label(l2), _}, _), _}]) when l1 == l2 =>
    default_case()
  | _ => elaborate_singleton_tuple(upat, ana_ty, l1, m)   // <-- PROBLEM
  };
| _ => default_case()
};
```

The logic:
1. If `ana` (expected type) is a singleton labeled tuple `(l1=T)`
2. Synth the pattern to get `e.ty`
3. If `e.ty` is a singleton labeled tuple with matching label, don't elaborate
4. **Otherwise, elaborate**

For Var patterns, `e.ty` is always `Unknown(Internal)`. Since `Unknown` doesn't match the `Prod([TupLabel(...)])` case, it always elaborated - even when the pattern name didn't match the label.

### The Fix

Added handling for `Unknown` synth types that checks if the pattern name matches the label:

```reason
switch (Typ.weak_head_normalize(ctx, e.ty).term) {
| Prod([{term: TupLabel({term: Label(l2), _}, _), _}]) when l1 == l2 =>
  default_case()
| Unknown(_) =>
  /* Only elaborate if pattern is a Var whose name matches the label */
  switch (upat.term) {
  | Var(name) when name == l1 =>
    elaborate_singleton_tuple(upat, ana_ty, l1, m)  // Destructuring
  | _ =>
    default_case()  // Pattern should have full tuple type
  }
| _ => elaborate_singleton_tuple(upat, ana_ty, l1, m)
};
```

**Behavior after fix:**
- `let a = (a=1) in a` → pattern `a` matches label `a` → elaborate → `a : Int` ✓
- `let m = (y=1) in m` → pattern `m` ≠ label `y` → don't elaborate → `m : (y=Int)` ✓

All existing singleton labeled tuple tests still pass.

---

## Known Limitations

### Empty Module Atomic Form

~~The empty module `{}` was not an atomic form like `()` (empty tuple) or `[]` (empty list).~~

**Fixed:** Added `t == "{}"` to `Token.is_potential_token` in `Token.re:163`. Now typing `{}` creates an atomic `EmptyModule` token, matching the behavior of `()` and `[]`.

---

## Menhir Parser: Semicolon Ambiguity

### The Problem

The Menhir parser cannot parse modules with multiple semicolon-separated items:
```
{ let x = 1; let y = 2 }  // Menhir parse error
```

Single-item modules work fine:
```
{ let x = 1 }  // OK
```

### Technical Analysis

The Menhir grammar (`src/menhirParser/Parser.mly`) has:

```
// Line 367: Seq in expressions uses semicolon
| e1 = exp; SEMI_COLON; e2 = exp { Seq(e1, e2) }

// Line 377: Module uses semicolon as item separator
| OPEN_CURLY; items = separated_list(SEMI_COLON, modItem); CLOSE_CURLY { Module(items) }

// Lines 380-386: Module items can contain expressions
modItemExp:
    | e = exp { e } %prec LET_EXP

modItem:
    | LET; i = pat; SINGLE_EQUAL; e = modItemExp { ModItemLet(i, e) }
    | e = modItemExp { ModItemExp(e) }
```

When parsing `{ let x = 1; let y = 2 }`, after reading `{ let x = 1`, the parser sees `;` and faces a **shift-reduce conflict**:

- **Reduce**: Treat `1` as the complete `modItemExp`, reduce to `modItem`, use `;` as list separator
- **Shift**: Push `;` onto the stack to build `Seq(1, ...)` as part of a larger expression

The `%prec LET_EXP` on `modItemExp` attempts to force early reduction, but precedence only resolves conflicts between the **same operator** in different positions. Here `;` plays **two different grammatical roles** (expression operator vs. list separator), which is fundamentally ambiguous in LR parsing.

### Why Hazel's Tile-Based Approach Works

Hazel's tile-based system has **sort-aware insertion**. In `Insert.re:54-77`:

```reason
let effective_sort = (t: Token.t, z: t): Sort.t => {
  let local_sort = Relatives.sort(z.relatives);
  let parent_sort = Ancestors.sort(z.relatives.ancestors);

  /* Special case: semicolon inside module context should be ModSeq */
  if (t == ";" && parent_sort == Sort.Mod) {
    parent_sort;  // Use Mod sort, which gives ModSeq form
  } else {
    ...
  }
};
```

When typing `;` inside `{ }`:
1. The system asks: "What's my parent sort?"
2. Parent is `Mod` → use `ModSeq` form (module item separator)
3. Parent is `Exp` → use `Seq` form (expression sequencing)

This is **context-sensitive** disambiguation that LR parsing fundamentally cannot express - LR parsers make decisions based on **lookahead tokens and stack state**, not "what construct am I inside."

### Potential Menhir Fixes

**Option 1: Grammar Duplication (Clean but Verbose)**

Create a separate expression grammar without Seq for use inside modules:

```
expNoSeq:
    | ... all exp rules EXCEPT the Seq rule ...

modItemExp:
    | e = expNoSeq { e }
```

This is unambiguous but requires duplicating ~50 lines of grammar rules. Any future changes to `exp` would need to be mirrored in `expNoSeq`.

**Option 2: GLR Parsing**

Menhir supports GLR (Generalized LR) mode which can handle ambiguous grammars by exploring both parses simultaneously. Disambiguation happens via semantic actions or post-processing.

```bash
menhir --lalr ...  # Current: fails on ambiguity
menhir --glr ...   # GLR: explores both parses
```

Adds complexity and potentially slower parsing, but avoids grammar duplication.

**Option 3: Lexer Hack**

Track brace depth in the lexer and emit different semicolon tokens inside braces:

```
SEMI_COLON       // Outside braces
SEMI_COLON_MOD   // Inside { }
```

This is ugly but has precedent - C compilers use a similar "typedef hack" to distinguish type names from identifiers. Requires lexer state management.

**Option 4: Post-Processing**

Accept whichever parse Menhir produces and fix it up afterward. For example, if Menhir parses `{ let x = 1; let y = 2 }` as a single item with nested Seq, transform it to the correct multi-item structure in a post-pass.

Fragile and requires understanding all the ways the "wrong" parse can manifest.

**Option 5: Remove Expression-Level Semicolon**

If expression-level `Seq` (`;` for sequencing expressions like `print("hi"); 42`) isn't essential, removing it eliminates the ambiguity entirely. Modules would be the only place semicolons appear.

This is a language design decision with broader implications.

### Current Status

**Deferred.** The tile-based editor handles multi-item modules correctly. Only the Menhir parser is affected, which is used for:
- Test input parsing (2 evaluator tests skipped)
- Potential future uses (CLI tools, batch processing)

For Phase 1, the workaround is using single-item modules in Menhir-parsed tests, or writing tests that use the tile-based editor directly.

---

## Cursor Inspector: ID Preservation in Module Expansion

### The Problem

The cursor inspector doesn't show type information for:
1. Module items (ModLet, ModType) - shows "whitespace or comment"
2. Semicolons inside modules - shows no info

### Surface Syntax IDs

For `{ let a = 1; let b = 2 }`, the surface syntax has these IDs:

```
{ let a = 1 ; let b = 2 }
^           ^           ^
CB          S          CB    (curly braces and semicolon tiles)
  ^-------^   ^-------^
     L1          L2          (ModLet tiles)
```

- Curly braces `{ }` tile has ID = CB
- Semicolon `;` tile has ID = S
- First ModLet `let a = 1` tile has ID = L1
- Second ModLet `let b = 2` tile has ID = L2

### Root Causes

**Issue 1: Semicolon IDs are lost during parsing**

In `MakeTerm.re`, the Module case doesn't absorb semicolon IDs from the body:
```reason
| (["{", "}"], [Mod(body)]) =>
  ret(Module(flatten_mod(body)))  // body's IDs (including semicolons) are lost!
```

Compare to ListLit which properly absorbs comma IDs:
```reason
| {annotation: {ids, _}, term: Tuple(es)} =>
  adopted_ids := ids @ adopted_ids^;  // Absorb comma IDs
```

**Issue 2: Expanded expressions use fresh IDs**

`ExpandModule.wrap_item` uses `Exp.fresh` which creates random IDs:
```reason
| ModLet(pat, def) => Exp.fresh(Let(pat, def, body))  // Fresh ID, not L1!
```

### The Solution: Complete ID Mapping

**Step 1: MakeTerm absorbs semicolon IDs**

Fix the Module case to absorb IDs like ListLit does:
```reason
| (["{", "}"], [Mod(body)]) =>
  switch (body) {
  | {annotation: {ids, _}, term: EmptyHole} => ret(Module([]))
  | {annotation: {ids, _}, term: _} =>
    adopted_ids := ids @ adopted_ids^;  // Absorb semicolon IDs
    ret(Module(flatten_mod(body)))
  }
```

Result: Module expression gets IDs = [CB, S] (curly braces + semicolons)

**Step 2: ExpandModule preserves Mod item IDs**

```reason
let wrap_item = (item: Mod.t, body: Exp.t): Exp.t => {
  let item_id = Mod.rep_id(item);
  switch (item.term) {
  | ModLet(pat, def) =>
    IdTagged.fast_copy(item_id, Exp.fresh(Let(pat, def, body)))
  | ModType(tpat, typ) =>
    IdTagged.fast_copy(item_id, Exp.fresh(TyAlias(tpat, typ, body)))
  ...
```

**Step 3: Synthetic tuple gets fresh IDs (NOT Module's IDs)**

The tuple `(a=a, b=b)` has no surface syntax counterpart and MUST use fresh IDs.

**CRITICAL**: The tuple cannot use the Module's IDs (CB, S) because:
1. Statics processes the expanded expression, storing Tuple info for those IDs
2. Then Statics' Module case calls `add()`, storing Module info for those same IDs
3. This overwrites the Tuple info with Module info
4. When Elaborator later processes the tuple and looks up its ID, it gets Module info
5. Elaborator sees Module term, tries to expand again → **infinite loop**

### Complete ID Mapping

```
Surface syntax:
{ let a = 1 ; let b = 2 }
  ^-------^ ^ ^-------^
     L1     S    L2

Module expression IDs = [CB, S]  (curly braces + semicolons)
ModLet item IDs = [L1], [L2]

Expanded expression:
Let(pat, def,              ID = L1  ← from first ModLet
  Let(pat, def,            ID = L2  ← from second ModLet
    Tuple([...])           ID = fresh  ← synthetic, no surface counterpart
  )
)
```

### Cursor Inspector Results

After these fixes:
- Click on `{` or `}` → looks up CB → Module info (tuple type) ✓
- Click on `;` → looks up S → Module info (tuple type) ✓
- Click on `let a = 1` → looks up L1 → Let info (binding type) ✓
- Click on `let b = 2` → looks up L2 → Let info (binding type) ✓

### Why Elaborator Keeps Inline Expansion

The Elaborator cannot simply call `ExpandModule.expand` and then `elaborate` on the result because:
1. `elaborate` calls `elaborated_type(m, uexp)` at the start of every call
2. This looks up the expression's ID in the statics map
3. For the expanded Let expressions, this works (Statics stored info for L1, L2)
4. But Elaborator needs to elaborate the **inner** expressions (pat, def) differently

So Elaborator keeps its inline expansion approach but with ID preservation:
```reason
| Module(items) =>
  let elaborate_mod_item = (item: Mod.t, body: Exp.t): Exp.t => {
    let item_id = Mod.rep_id(item);
    switch (item.term) {
    | ModLet(pat, def) =>
      let (pat', _) = elaborate_pattern(m, pat, false);
      let (def', _) = elaborate(m, def);
      IdTagged.fast_copy(item_id, Exp.fresh(Let(pat', def', body)));
    ...
```

This approach:
- Only calls `elaborate` on inner expressions (which have IDs in the statics map)
- Constructs wrapper Let/TyAlias directly with preserved Mod item IDs
- Constructs tuple with fresh IDs
- Avoids ID lookup issues

### Implementation Checklist

1. **MakeTerm.re**: Add semicolon ID absorption to Module case
2. **ExpandModule.wrap_item**: Use `IdTagged.fast_copy(item_id, ...)` to preserve Mod item IDs
3. **ExpandModule.build_labeled_tuple**: Use fresh IDs (NOT Module's IDs)
4. **Elaborator.re**: Keep inline expansion with ID preservation
5. **Statics.re**: No changes needed (already works correctly)

### ID Duplication Avoidance

The key insight is that IDs are partitioned:
- **CB, S** (Module's IDs): Used only by the Module expression, stored once by Statics' Module case
- **L1, L2** (Mod item IDs): Used by expanded Let/TyAlias, stored by Statics when processing expansion
- **fresh** (tuple ID): Not in surface syntax, not looked up by anyone

No ID appears in multiple places, so no overwrites occur.

### CRITICAL: ModExp is Different from ModLet/ModType

**ModLet and ModType** have corresponding surface syntax tiles (`let a = 1`, `type T = Int`). Users can click on these tiles, so preserving their IDs on the expanded Let/TyAlias makes sense.

**ModExp is synthetic** - it's a wrapper MakeTerm creates around a bare expression inside a module. For `{ 1 + 1; let x = 2 }`:
- `1 + 1` is an expression with its own IDs (from the `+` tile, etc.)
- `ModExp(1 + 1)` is a synthetic wrapper with IDs that may overlap with the inner expression
- The expanded `Let(_, 1+1, body)` has no surface syntax counterpart

**Therefore**: ModExp should use **fresh IDs** for its wrapper Let, NOT preserve the ModExp's ID. The inner expression already has its own IDs for cursor inspector.

```reason
| ModLet(pat, def) => IdTagged.fast_copy(item_id, ...)  // Preserve ID
| ModType(tpat, typ) => IdTagged.fast_copy(item_id, ...) // Preserve ID
| ModExp(e) => Exp.fresh(Let(wild_pat, e, body))  // Fresh ID - synthetic!
```

This fixes the stack overflow that occurred when ModExp's ID overlapped with the inner expression's IDs.

---

## Known Issue: Nested Semicolon IDs Not Collected

### The Problem

For modules with 3+ items like `{ let x = 1; let y = 2; let z = 3 }`, only the **first** semicolon has cursor inspector info. Subsequent semicolons show "whitespace or comment".

### Root Cause

The Skel (skeleton) system produces **nested** binary structures for same-precedence operators:

```
Bin(Bin(a_skel, [;1], b_skel), [;2], c_skel)
```

When `unsorted` in MakeTerm processes this:
1. Outer level sees `Bin(l_skel, [;2], c_skel)`
2. Recursively processes `l_skel` → produces `Mod(inner_result)` with `annotation.ids = [;1]`
3. Outer `tiles` only contains `[;2]`
4. `is_mod_seq(tiles)` returns `Some([])` - no between_kids at this level
5. Result: `all_items = [Mod(inner_result), Mod(c)]`
6. `ids(unsorted)` at outer level = `[;2]` only

The inner `;1` ID is buried inside `inner_result.annotation.ids` and never collected.

### Current State

- `flatten_mod` correctly flattens the **terms** (extracts all Mod items from nested MultiHole)
- But it does NOT collect the **IDs** from nested structures
- Only the outermost semicolon ID ends up in the Module expression's annotation
- Statics adds cursor info only for those IDs

### Potential Solutions

**Option A: Modify Skel to produce flat structures for semicolons**

Make semicolons behave like commas in tuples (if tuples are indeed flat). This would require changes to `Skel.re` to handle ModSeq specially. However, it's unclear if tuples actually produce flat structures or just happen to work for other reasons.

**Option B: Collect IDs during flattening**

Modify `flatten_mod` (or create a new function) to collect IDs from nested MultiHole annotations while flattening:

```reason
let rec flatten_mod_with_ids = (m: Mod.t): (list(Mod.t), list(Id.t)) =>
  switch (m.term) {
  | MultiHole(kids) =>
    let results = kids |> List.filter_map(...) |> List.map(flatten_mod_with_ids);
    let items = results |> List.map(fst) |> List.flatten;
    let ids = m.annotation.ids @ (results |> List.map(snd) |> List.flatten);
    (items, ids)
  | _ => ([m], [])
  };
```

Then use these collected IDs in the Module case of exp_term.

**Option C: Accept the limitation**

Document that cursor inspector works for curly braces and the first semicolon, but not subsequent semicolons. This is a minor UX issue - the type information is still correct, just not accessible via clicking all semicolons.

### Decision

Deferred. The current implementation works correctly for evaluation and type-checking. Cursor inspector works for:
- Curly braces (`{` and `}`)
- First semicolon
- All ModLet/ModType items

This is acceptable for Phase 1. A complete fix can be addressed in a future iteration.
