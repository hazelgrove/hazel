# Projector Term Constructor Plan

## Goal

Add `Projector` constructors to the term grammar (`exp_term`, `pat_term`, `typ_term`) to enable round-tripping of projector data through the segment → term → segment cycle.

Currently, when segments containing projectors are parsed via MakeTerm, the projector metadata (`kind`, `model`) is discarded—only the inner syntax is preserved. This prevents faithful round-tripping.

## Current State

### Segment-Level Projectors

In `ProjectorCore.re`:
```reason
type t('syntax) = {
  id: Id.t,
  kind: Kind.t,
  syntax: 'syntax,  // Always a parenthesized Piece
  model: string,
};
```

In `Base.re`:
```reason
type projector = ProjectorCore.t(piece);
```

### Projector Syntax Structure

**Key invariant**: The `syntax` field is always a parenthesized `Piece`:
- Created via `Segment.parenthesize(seg)` in `ProjectorPerform.init`
- The inner segment is the actual payload
- `Piece.unparenthesize(syntax)` extracts this payload

```
Projector
  ├─ id: Id.t
  ├─ kind: ProjectorCore.Kind.t
  ├─ model: string
  └─ syntax: Piece (always parenthesized tile)
       ├─ label: ["(", ")"]
       └─ children: [Segment]  ← actual payload
```

### Current MakeTerm Handling

In `tile_kids`:
```reason
| Projector({syntax, _} as pr) =>
  let sort = Piece.sort(syntax) |> fst;
  let seg = Piece.unparenthesize(syntax);  // Extract from parens
  [go_s(sort, Segment.skel(seg), seg)];
```

The `PROJ_WRAP` hack in exp/pat/typ pattern matching:
```reason
| (["PROJ_WRAP", "PROJ_WRAP"], [Exp(body)]) => ret(body.term)
```

This discards all projector metadata.

## Proposed Design

### 1. Projector Data Type

Define in `Grammar.re` (or a shared location):

```reason
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type projector_data = {
  kind: ProjectorCore.Kind.t,
  model: string,
};
```

**Dependency resolution**: Create `src/language/ProjectorKind.re` containing just the `Kind.t` enum. Both `Grammar.re` and `ProjectorCore.re` will import from this shared location.

**Current Kind.t values** (from ProjectorCore.re):
```reason
type t =
  | Fold
  | Probe
  | Statics
  | Checkbox
  | Slider
  | SliderF
  | Card
  | Livelit
  | TextArea
  | Csv;
```

Also copy over the `name` and `of_name` functions, and any other Kind-related utilities that Grammar might need.

### 2. Term Constructors

Add to `exp_term`:
```reason
| Projector(projector_data, exp_t('a))
```

Add to `pat_term`:
```reason
| Projector(projector_data, pat_t('a))
```

Add to `typ_term`:
```reason
| Projector(projector_data, typ_t('a))
```

**TPat**: Omit for now given its minimal scope.

### 3. ID Handling

The projector's ID uses the standard annotation system:
- The `Projector(data, inner)` term has its own annotation containing `ids`
- `rep_id` extracts the primary ID
- This parallels how `Parens(inner)` works

## Implementation Changes Required

### MakeTerm.re

**Key change in `tile_kids`:** Construct the Projector term directly when processing projector pieces:

```reason
| Projector({id, kind, model, syntax}) =>
  let _ = log_projector({id, kind, model, syntax});
  let sort = Piece.sort(syntax) |> fst;
  let seg = Piece.unparenthesize(syntax);
  let inner = go_s(sort, Segment.skel(seg), seg);

  // Construct Projector term with proper annotation
  let wrapped = switch (inner) {
    | Exp(e) => Exp({
        term: Projector({kind, model}, e),
        annotation: IdTagged.IdTag.mk([id], get_secondary([id])),
      })
    | Pat(p) => Pat({
        term: Projector({kind, model}, p),
        annotation: IdTagged.IdTag.mk([id], get_secondary([id])),
      })
    | Typ(t) => Typ({
        term: Projector({kind, model}, t),
        annotation: IdTagged.IdTag.mk([id], get_secondary([id])),
      })
    | _ => inner
  };
  [wrapped];
```

**PROJ_WRAP pattern matching stays unchanged:**
```reason
| (["PROJ_WRAP", "PROJ_WRAP"], [Exp(body)]) => ret(body.term)
```

Now `body.term` is already `Projector(...)`, so this just passes it through.

### ExpToSegment.re

Add `Projector` cases that serialize back to `Piece.Projector`:

```reason
| Projector({kind, model}, e) =>
  let id = exp |> Exp.rep_id;
  let+ inner_seg = go(e);
  // Create a parenthesized piece from inner_seg
  let syntax = Segment.parenthesize(inner_seg);
  [Piece.Projector(ProjectorCore.mk(~id, kind, syntax, model))]
```

### Grammar.re: map_*_annotation Functions

Add Projector cases (straightforward pass-through):

```reason
| Projector(data, e) => Projector(data, map_exp_annotation(f, e))
```

### TermBase.re: map_term Functions

```reason
| Projector(data, e) => Projector(data, exp_map_term(e))
```

### Statics.re

Follow the Parens pattern—bidirectional type propagation. Looking at actual Parens handling:

**Expressions** (uexp_to_info_map, ~line 373):
```reason
| DynamicErrorHole(e, _)
| Parens(e) =>
  let (e, m) = go(~ana, e, m);
  add'(~self=e.self, ~co_ctx=e.co_ctx, m);
```

So for Projector:
```reason
| Projector(_, e) =>
  let (e, m) = go(~ana, e, m);
  add'(~self=e.self, ~co_ctx=e.co_ctx, m);
```

**Patterns** (upat_to_info_map, ~line 1867):
```reason
| Parens(p) =>
  let (p, m) = go(~ctx, ~ana, p, m);
  add'(~self=p.self, ~ctx=p.ctx, ~constraint_=p.constraint_, m);
```

**Types** (utyp_to_info_map, ~line 1958):
```reason
| List(t)
| Parens(t) => add(go(t, m) |> snd)
```

All are simple pass-through—the Projector wrapper is transparent to typechecking.

### Dynamics / Transition.re

Follow the Parens pattern—`RemoveParens` step removes the wrapper:

```reason
| Projector(_, d') =>
  let. _ = otherwise(d');
  Step({
    expr: d',
    state_update,
    kind: RemoveParens,  // or new RemoveProjector kind
    is_value: false,
  });
```

### Pattern Matching (PatternMatch.re)

```reason
| Projector(_, p) => recur(p, d)
```

### Substitution.re

```reason
| Projector(data, e) => Projector(data, subst_exp(x, v, e))
```

### EvalCtx.re

Add `Projector` to the evaluation context type:
```reason
| Projector(projector_data, t)
```

And the compose function.

### Coverage.re

Treat as degenerate (like Parens):
```reason
| Projector(_, p) => check_coverage(p, ...)
```

### Exp.re, Pat.re, Typ.re Utility Functions

Many functions that recurse through terms need Projector cases:
- `is_fun`, `is_var`, `get_var`, etc.
- Generally just recurse into the inner term

### Elaborator.re

```reason
| Projector(data, e) =>
  let (e', ty) = elaborate(~ctx, e);
  (Projector(data, e') |> Exp.fresh, ty)
```

### Form.re

Consider whether a distinct `ProjectorExp`/`ProjectorPat`/`ProjectorTyp` form is needed, or if the `PROJ_WRAP` approach can be refined.

### Abbreviate.re

Add cost for projector (similar to Parens):
```reason
| Projector(_, e) => cost_exp(e) + 2  // or appropriate cost
```

### ProofHacks.re

Handle in exp_to_pat, pat_to_exp, and inductive hypothesis extraction.

### Grammar.re: Factory Module

Grammar.re has a `Factory` module with helper constructors like `Exp.parens`. Add corresponding helpers:
```reason
let projector = (~ann=?, data, e): exp_t(DefaultAnnotation.t) => {
  term: Projector(data, e),
  annotation: default_annotation(ann),
};
```

Similarly for Pat and Typ.

### ExpToSegment.re: Precedence Functions

The `external_precedence` and `internal_precedence` functions handle Parens by returning `Precedence.max` (highest precedence, no wrapping needed). Projector should do the same:
```reason
| Projector(_) => Precedence.max
```

### Equality.re

Term equality checking may need Projector cases. Check if there's explicit pattern matching on term constructors.

## Files Requiring Changes (Comprehensive List)

Based on Parens handling analysis:

**Core Grammar & Types:**
- `Grammar.re` - Type definitions
- `TermBase.re` - map_term functions
- `Exp.re`, `Pat.re`, `Typ.re` - Utility functions

**Parsing:**
- `MakeTerm.re` - Term construction
- `Form.re` - Form definitions (if needed)

**Pretty Printing:**
- `ExpToSegment.re` - Serialization

**Statics:**
- `Statics.re` - Type checking
- `Elaborator.re` - Elaboration
- `Coverage.re` - Coverage checking

**Dynamics:**
- `Transition.re` - Evaluation steps
- `PatternMatch.re` - Pattern matching
- `Substitution.re` - Substitution
- `EvalCtx.re` - Evaluation contexts
- `EvaluatorStep.re` - Stepper
- `DHExp.re` - ty_subst

**Proof:**
- `ProofHacks.re` - Proof utilities

**Display:**
- `Abbreviate.re` - Abbreviation

**Projectors:**
- `CardProj.re` - Card projector (if it pattern matches on terms)

## Resolved Design Questions

### 1. MakeTerm Threading (RESOLVED)

**Problem:** The `PROJ_WRAP` pattern matching loses projector metadata (kind, model) because by that point we only have tokens and kids.

**Solution:** Construct Projector term directly in `tile_kids` where we have all the metadata. The PROJ_WRAP pattern match then just passes through the already-constructed term.

See "MakeTerm.re" section above for implementation details.

### 2. Kind.t Dependency (RESOLVED)

Create `src/language/ProjectorKind.re` with just the enum. Both Grammar.re and ProjectorCore.re import from there.

### 3. RemoveProjector vs RemoveParens (RESOLVED)

Use `RemoveParens` for now. Add a comment noting we may want `RemoveProjector` later for stepper clarity.

## Remaining Considerations

### Round-Trip Test Structure

The tests in `Test_ExpToSegment.re` currently explicitly exclude projectors (noted as "out of scope" around line 652). With projectors in terms, we can add:
- Simple: `^^fold 1`
- With model data: checkbox, slider projectors
- Nested: projector containing projector

### Projector ID vs Inner Term ID

When a projector wraps a term:
- Projector term has its own ID (from its annotation)
- Inner term has its own ID (from its annotation)
- Both are distinct—this matches segment-level behavior where `pr.id` differs from inner piece IDs

## Build Configuration

Creating `src/language/ProjectorKind.re` may require updating `src/language/dune` to include the new module. Check the dune file structure and add the module if needed.

## Implementation Order

Suggested order to minimize breakage:

1. **Create `src/language/ProjectorKind.re`** with `Kind.t` enum (check dune file)
2. **Update `ProjectorCore.re`** to use `ProjectorKind.Kind.t`
3. **Add `projector_data` type and `Projector` constructors** to `Grammar.re`
4. **Add `map_*_annotation` cases** in `Grammar.re`
5. **Add `map_term` cases** in `TermBase.re`
6. **Update `MakeTerm.re`** - construct Projector terms in `tile_kids`
7. **Update `ExpToSegment.re`** - serialize Projector terms back to `Piece.Projector`
8. **Update `Statics.re`** - add pass-through cases for Projector
9. **Update dynamics** - `Transition.re` (with RemoveParens + comment), `PatternMatch.re`, etc.
10. **Update utilities** - `Exp.re`, `Pat.re`, `Typ.re` recursive functions
11. **Add round-trip tests** in `Test_ExpToSegment.re`
12. **Fix remaining compilation errors** - they'll guide us to any missed files

## Risk Assessment

**Low risk:**
- Adding type definitions
- Adding pass-through cases in most files

**Medium risk:**
- MakeTerm changes (threading projector data)
- ExpToSegment changes (serialization must be exact inverse)
- Ensuring ID handling is consistent

**High risk:**
- Dynamics changes (evaluation semantics must be neutral)
- Any changes that affect existing projector behavior

## Decision Points

1. [x] **Kind.t dependency approach**: Create `src/language/ProjectorKind.re` with just the enum. Both Grammar.re and ProjectorCore.re will use this shared definition.

2. [x] **MakeTerm threading approach**: Construct Projector term directly in `tile_kids`.

3. [x] **RemoveProjector vs RemoveParens step kind**: Use `RemoveParens` for now. Add a comment at the implementation noting we may want a distinct `RemoveProjector` step kind later for stepper clarity.

4. [x] **All projector kinds neutral at term level**: Yes. All projector kinds behave the same at the term level—just wrap. Projector-specific behavior stays at segment/UI level.

## Implementation Notes

### Dynamics Flag on Projectors

Projectors have a `dynamics: bool` flag that was used to determine whether to collect dynamics / create sample targets in the evaluator. The only projector that used this (Probe) is now a refractor, so this may be dormant.

**Verified**: The dynamics flag is NOT checked in MakeTerm—it's only referenced in the UI layer (RefractorView.re, ProjectorView.re) and ProjectorBase.re definition. This is currently a UI-level concern, not a term-level concern. If we later need projectors that require dynamics collection at the term level, that machinery would need to be introduced. For now, this doesn't affect implementation.

### Secondary Handling for Projectors

Projectors should act like any other term for secondary (whitespace/comments) handling. In `tile_kids`, when constructing the Projector term annotation, use `get_secondary([id])` where `id` is the projector's ID.

**TODO**: Verify during implementation that projector IDs are present in the `secondary_map`. If the projector piece's secondary isn't being collected, we may need to ensure it's added to the map, or fall back to empty secondary `([], [])`.
