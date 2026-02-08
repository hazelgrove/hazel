# Type Flow Investigation: Elaboration -> Evaluation -> Post-Eval Statics

## Executive Summary

The post-eval statics is 100x slower because **every type object in the evaluated expression tree is a fresh allocation**, defeating `===` (physical equality) fast paths in `Typ.meet`. The damage comes from three sources, each of which destroys physical equality independently:

1. **Ascriptions.re** takes types apart and reassembles them using `Typ.temp`, `Typ.fresh`, `DHExp.fresh`, `Typ.subst`, and `Typ.unroll` -- creating many new type allocations during evaluation.
2. **Transition.re** creates new `Asc` nodes with `Typ.temp` types during evaluation steps (function application, short-circuit booleans, etc.).
3. **`Exp.replace_all_ids`** at the end of evaluation (Evaluator.re:249) traverses the *entire* expression tree including all embedded types, creating a new record for every single node -- including type nodes. This alone would destroy all physical equality.

Additionally, **`Substitution.in_exp`** (also at Evaluator.re:249) traverses all types via `in_typ`, and while it preserves term structure for types without expression children (most types), the `map_term` infrastructure creates new `{...exp, term: ...}` records regardless.

---

## Part 1: Types the Elaborator Writes

File: `/Users/andrewblinn/Dropbox/projects/hazel-projector-html/src/language/statics/Elaborator.re`

### 1.1 `fresh_ascription` (lines 36-54) -- Primary type insertion point

This is the main function that inserts `Asc` nodes into the elaborated AST. It takes:
- `d`: the expression
- `t`: the expression's self type
- `t'`: the expected/ana type (optional)

Logic:
- If `t'` is `Unknown(Internal)`, skip ascription (line 39)
- If `t'` and `t` are `fast_equal` (unnormalized), skip (line 40)
- Otherwise, **normalize both** (`Typ.normalize(ctx, ty)` and `Typ.normalize(ctx, t)`) and compare again (lines 44-46)
- If still not equal after normalization, insert `asc(d, ty_n)` -- the ascription contains the **normalized** expected type (line 49)

**Key insight**: The types written into `Asc` nodes by the elaborator are **normalized**. For a recursive type like `type color = Red + Green + Blue`, the normalized form expands `Rec(tp, ...)` into the full sum type structure. This is what creates the expensive Rec types that later defeat the `Var-Var` fast path in `meet`.

### 1.2 Explicit `Asc` nodes from user source (line 258-259)

```reason
| Asc(e, t) =>
  Asc(elaborate(m, e) |> fst, Typ.normalize(ctx, t)) |> rewrap
```

User-written type annotations are **normalized** before embedding. The `rewrap` preserves the original node's ID.

### 1.3 `FixF` with ascription (line 394)

```reason
| FixF(p, e, env) =>
  FixF(p', Asc(e', pty) |> Exp.fresh, env) |> rewrap
```

Creates a fresh `Asc` node wrapping the body with the pattern type `pty` (from `elaborate_pattern`). The type is the elaborated pattern type (which comes from `match_synswitch`).

### 1.4 `Cons` with ascription (lines 461-466)

```reason
| Cons(e1, e2) =>
  Cons(e1', e2')
  |> rewrap
  |> IdTagged.FreshGrammar.Exp.asc(_, elaborated_type);
```

List cons expressions get ascribed with the `elaborated_type`.

### 1.5 `If` branches (lines 424-433)

```reason
| If(c, t, f) =>
  If(c',
    fresh_ascription(ctx, t', t_ty, Some(elaborated_type)),
    fresh_ascription(ctx, f', f_ty, Some(elaborated_type)),
  ) |> rewrap;
```

Both branches of `If` get ascriptions via `fresh_ascription`.

### 1.6 `ListLit` elements (lines 274-281)

```reason
| ListLit(es) =>
  let meet_ty = Typ.meet_all(~empty=Unknown(Internal) |> Typ.temp, ctx, tys);
  let ds' = List.map2((d, t) => fresh_ascription(ctx, d, t, meet_ty), ds, tys);
```

List literal elements get ascribed with the meet of all element types.

### 1.7 `Match` case bodies (lines 496-509)

```reason
| Match(e, cases) =>
  let es' = List.map(e => {
    let (e', ty) = elaborate(m, e);
    fresh_ascription(ctx, e', ty, Some(elaborated_type));
  }, es);
```

Each match case body gets ascribed with the overall match type.

### 1.8 `Constructor` with type annotation (lines 283-294)

```reason
| Constructor(c, _) =>
  let t = ...Some(Typ.normalize(ctx, ty))...;
  Constructor(c, t) |> rewrap;
```

Constructors embed a **normalized** type annotation.

### 1.9 `TypAp` type argument (lines 421-423)

```reason
| TypAp(e, ut) =>
  let ut' = Typ.normalize(ctx, ut);
  TypAp(e', ut') |> rewrap;
```

Type application arguments are **normalized**.

### 1.10 Pattern `Asc` (line 181-183)

```reason
| Asc(p, t) =>
  Asc(p', Typ.normalize(ctx, t)) |> rewrap;
```

Pattern type annotations are **normalized**.

### 1.11 Pattern `Constructor` (lines 183-196)

```reason
| Constructor(c, _) =>
  let t = ...Some(Typ.normalize(ctx, ana_ty))...;
  Constructor(c, Some(t)) |> rewrap;
```

Pattern constructors embed **normalized** types.

### 1.12 `Fun` with closure type (lines 295-298)

```reason
| Fun(p, e, _, n) =>
  let (p', typ) = elaborate_pattern(m, p, false);
  Fun(p', e', Some(typ), n) |> rewrap;
```

Functions embed the pattern's elaborated type as the closure type. This comes from `elaborated_pat_type` which calls `Typ.match_synswitch`.

### Summary: Type Entry Points Table

| Location | Node Type | Type Form | Normalized? | Line |
|----------|-----------|-----------|-------------|------|
| `fresh_ascription` | `Asc(e, ty_n)` | Normalized expected type | Yes | 49 |
| User `Asc` | `Asc(e, t)` | Normalized user annotation | Yes | 259 |
| `FixF` body | `Asc(e, pty)` | Pattern elab type | Via match_synswitch | 394 |
| `Cons` | `Asc(cons, elab_type)` | Elaborated type | Via match_synswitch | 466 |
| `If` branches | `Asc(branch, ty)` | Via fresh_ascription | Yes (if inserted) | 430-431 |
| `ListLit` elements | `Asc(elem, meet_ty)` | Via fresh_ascription | Yes (if inserted) | 280 |
| `Match` bodies | `Asc(body, elab_type)` | Via fresh_ascription | Yes (if inserted) | 505 |
| `Constructor` | `Constructor(c, Some(norm_ty))` | Normalized self/ana type | Yes | 292-293 |
| `TypAp` arg | `TypAp(e, norm_ty)` | Normalized type arg | Yes | 422 |
| `Fun` closure type | `Fun(p, e, Some(typ), n)` | match_synswitch type | Partially | 298 |
| Pattern `Asc` | `Asc(p, norm_ty)` | Normalized annotation | Yes | 183 |
| Pattern `Constructor` | `Constructor(c, Some(norm_ty))` | Normalized type | Yes | 196 |

---

## Part 2: Ascriptions.re -- Detailed Type Operations

File: `/Users/andrewblinn/Dropbox/projects/hazel-projector-html/src/language/dynamics/transition/Ascriptions.re`

### 2.1 Overall Logic

`Ascriptions.transition` takes a `DHExp.t` and, if it's an `Asc(e, t)` node, tries to "push" the ascription inward. For example, `[1, 2] : [Int]` becomes `[1 : Int, 2 : Int]`. It also removes ascriptions from values whose types are consistent.

`Ascriptions.transition_multiple` repeatedly calls `transition` until no more transitions are possible.

### 2.2 Every `Typ.*` Function Call

#### Line 29: `Typ.term_of(Typ.unroll(t))`
```reason
switch (DHExp.term_of(e), Typ.term_of(Typ.unroll(t))) {
```
- `Typ.unroll(t)` on the ascription's type. If `t` is `Rec(tp, body)`, this calls `Typ.subst(t, tp, body)` which traverses and recreates the entire body type.
- **Creates new types**: Yes, `subst` uses `rewrap` which creates new records for every node.

#### Lines 33-36: `Typ.is_consistent` with `Typ.unroll`
```reason
Typ.is_consistent(Ctx.empty, Typ.unroll(t |> Typ.temp), Typ.unroll(t'))
```
- Creates a `Typ.temp` wrapping `t` (the inner unrolled type term)
- Calls `Typ.unroll` on both
- `is_consistent` calls `meet` internally
- **Creates new types**: `Typ.temp` creates a new record. `Typ.unroll` creates new records via `subst` if Rec. `meet` creates `|> temp` results.

#### Lines 39: `Typ.meet` with `Typ.unroll`
```reason
Typ.meet(Ctx.empty, Typ.unroll(t |> Typ.temp), Typ.unroll(t'))
```
- Same as above, plus `meet` itself creates new type objects using `|> temp` for every structural case (Arrow, Prod, Sum, List, TupLabel, Rec, Poly, Unknown).
- **Creates new types**: Yes, extensively. Every successful `meet` creates new `temp` types.

#### Line 48: `Typ.fresh`
```reason
transition(~recursive, Asc(d, t |> Typ.fresh) |> DHExp.fresh)
```
- Wrapping a Parens type term with `Typ.fresh` (fresh ID allocation).
- **Creates new types**: Yes, `Typ.fresh` allocates a new `Id.t` via `Id.mk()`.

#### Lines 122-123: `Typ.temp` (TypFun/Poly case)
```reason
| Some(tyvar) => Var(tyvar) |> Typ.temp
| None => Unknown(Internal) |> Typ.temp
```
- Creating type variables for type substitution in Poly ascriptions.
- **Creates new types**: Yes, two new `Typ.temp` allocations.

#### Line 130: `Typ.subst`
```reason
recur(Asc(body, Typ.subst(new_ty, tp', t')) |> DHExp.fresh)
```
- Substituting into the Poly body type. `subst` traverses and recreates every node.
- **Creates new types**: Yes, extensively. Every node in the type tree gets a new `rewrap` allocation.

#### Lines 142-143: `Typ.temp` (If case)
```reason
recur(Asc(e1, t |> Typ.temp) |> DHExp.fresh),
recur(Asc(e2, t |> Typ.temp) |> DHExp.fresh),
```
- Pushing ascription into If branches. Creates new `Typ.temp` wrappers for the type term.
- **Creates new types**: Yes, two new allocations per If ascription.

#### Line 155: `Typ.temp` (Match case)
```reason
((p, body)) => (p, Asc(body, t |> Typ.temp) |> DHExp.fresh)
```
- Pushing ascription into each Match branch. One new `Typ.temp` per branch.
- **Creates new types**: Yes, one per match branch.

#### Lines 171, 183: `Typ.is_consistent` + `Typ.unroll` + `Typ.temp`
```reason
Typ.is_consistent(Ctx.empty, Typ.unroll(sumt), sumt' |> Typ.temp)
Typ.is_consistent(Ctx.empty, Typ.unroll(t), t' |> Typ.temp)
```
- Constructor consistency checks. Creates temp wrappers and calls meet internally.
- **Creates new types**: Yes, temp wrappers and meet internals.

#### Lines 192, 196-199, 210-213: `Typ.is_consistent` + `Typ.temp`
```reason
Typ.is_consistent(Ctx.empty, t, Atom(Bool) |> Typ.temp)
Typ.is_consistent(Ctx.empty, t, Atom(Atom.cls_of_kind(ty_out)) |> Typ.temp)
```
- BinOp/UnOp consistency checks. Creates temp wrappers for comparison.
- **Creates new types**: Yes, temp wrappers. But these are just for comparison and discarded.

### 2.3 Every Place New Type Objects are Created

| Line | Operation | What's Created |
|------|-----------|----------------|
| 29 | `Typ.unroll(t)` | New type tree if Rec (via subst) |
| 35 | `t \|> Typ.temp` | New Typ.t record |
| 36 | `Typ.unroll(t')` | New type tree if Rec |
| 39 | `Typ.meet(...)` | New type tree via `\|> temp` at every node |
| 41 | `Asc(e, t) \|> DHExp.fresh` | New Exp with fresh ID |
| 48 | `t \|> Typ.fresh` | New Typ.t with fresh ID |
| 66 | `DHExp.fresh` | New Exp with fresh ID |
| 76 | `DHExp.fresh` (x5) | New Exp records in Tuple case |
| 104 | `DHExp.fresh` (x2) | New Exp records in ListLit case |
| 113-114 | `DHExp.fresh` (x3) | New Exp records in Cons case |
| 122-123 | `Typ.temp` | New Typ.t for Var or Unknown |
| 130 | `Typ.subst(...)` | Entire new type tree |
| 130 | `DHExp.fresh` | New Exp record |
| 142-143 | `Typ.temp` (x2) | Two new Typ.t for If branches |
| 155 | `Typ.temp` (per branch) | One new Typ.t per Match branch |
| 171 | `Typ.temp`, `Typ.unroll` | New types for consistency check |
| 183 | `Typ.temp`, `Typ.unroll` | New types for consistency check |
| 192, 199, 213 | `Typ.temp` | New types for consistency checks |

### 2.4 Key Observation: `Typ.unroll` is the Main Damage Source

When a type like `Rec("t", Sum([("Red", None), ("Green", None), ("Blue", None)]))` is unrolled, `subst` recreates every node in the sum type body with `rewrap`. For a large sum type with N constructors, this creates O(N) new type objects. If this unrolled type is then used in a `meet` call, the meet creates *another* O(N) type objects (one `temp` per constructor entry).

For the `full_app` benchmark with large sum types, a single `Typ.unroll` + `Typ.meet` in Ascriptions can create hundreds of fresh type objects.

---

## Part 3: Transition.re -- Type Involvement

File: `/Users/andrewblinn/Dropbox/projects/hazel-projector-html/src/language/dynamics/transition/Transition.re`

### 3.1 Ascription Handling (lines 921-944)

Transition delegates to `Ascriptions.transition`:
```reason
| Asc(d', t) =>
  switch (Ascriptions.transition(d)) {
  | Some(d') => ...Step({expr: d', ...})
  | None => ...
    switch (Ascriptions.transition(Asc(d', t) |> rewrap)) {
    | Some(d) => ...Step({expr: d, ...})
    | None => Constructor
    };
  }
```

Transition itself does NOT manipulate types here -- it delegates entirely to Ascriptions.re.

### 3.2 Function Application with Ascribed Function (lines 460-468)

```reason
| Asc(d1'', {term: Arrow(t1, t2), _}) =>
  Step({
    expr: Asc(Ap(Forward, d1'', Asc(d2', t1) |> fresh) |> fresh, t2) |> fresh,
    ...
  })
```

When applying an ascribed function `(f : t1 -> t2)(arg)`, creates:
- `Asc(d2', t1) |> fresh` -- ascribing the argument with `t1` (creates new Exp)
- `Ap(Forward, d1'', ...) |> fresh` -- creates new App expression
- `Asc(..., t2) |> fresh` -- ascribing the result with `t2` (creates new Exp)

**Types reused**: `t1` and `t2` are extracted from the existing Arrow type and **reused directly** (no new type allocation). But the Exp wrappers are fresh.

### 3.3 Short-Circuit Boolean Operations (lines 649, 665)

```reason
expr: b1 ? asc(d2, IdTagged.FreshGrammar.Typ.bool()) : bool(false)
expr: b1 ? bool(true) : asc(d2, IdTagged.FreshGrammar.Typ.bool())
```

Creates `Bool` type via `FreshGrammar.Typ.bool()` which generates a fresh ID. Two new type allocations per short-circuit boolean.

### 3.4 Type Application (lines 421-442)

```reason
| TypAp(d, tau) =>
  ...
  | TypFun(utpat, tfbody, name) =>
    Step({expr: DHExp.ty_subst(tau, utpat, tfbody), ...})
```

`DHExp.ty_subst` traverses the entire expression tree, calling `Typ.subst` on every type within. This creates new type allocations for every type node encountered.

### 3.5 Summary

Transition.re touches types in exactly these cases:
1. **Asc handling** -- delegates to Ascriptions.re (heavy type operations)
2. **Ascribed function application** -- reuses existing type components from Arrow, but creates fresh Exp wrappers
3. **Short-circuit booleans** -- creates fresh Bool types
4. **TypAp** -- calls `DHExp.ty_subst` which creates many new type allocations via `Typ.subst`
5. All other cases do NOT touch types

---

## Part 4: Evaluator.re -- Type Involvement

File: `/Users/andrewblinn/Dropbox/projects/hazel-projector-html/src/language/dynamics/Evaluator.re`

### 4.1 The Evaluator Itself

The evaluator (`evaluate` function, lines 128-234) is a trampoline-based big-step evaluator that drives `Transition.transition`. It does **NOT** directly manipulate types. It only:
- Calls `Eval.transition` (the `Transition.Transition` functor instantiated with `EvaluatorEVMode`)
- Manages probe recording and side effects
- Returns the final expression

### 4.2 The Critical Post-Processing (line 249)

```reason
Completed((x |> Substitution.in_exp(env) |> Exp.replace_all_ids, state^))
```

This is where the most damage happens. After evaluation completes:

1. **`Substitution.in_exp(env)`**: Traverses the entire result expression. For types, calls `in_typ` which uses `Typ.map_term`. The `map_term` infrastructure creates `{...exp, term: ...}` for every type node, even when nothing changes. This creates a new allocation for every type node in the tree.

2. **`Exp.replace_all_ids`**: Traverses the entire expression tree (including all types, patterns, etc.) and creates a new record for every node with a fresh `Id.mk()`. This is defined at `Exp.re:398-413`:
   ```reason
   let f = (continue, exp) =>
     {...exp, annotation: IdTagged.IdTag.mk_internal([Id.mk()])}
     |> continue;
   ```
   This **unconditionally** creates a new record and a new ID for every single node in the tree, including every type node.

**This alone guarantees that no type in the evaluated result shares physical identity with any other type.**

---

## Part 5: PatternMatch.re -- Type Involvement

File: `/Users/andrewblinn/Dropbox/projects/hazel-projector-html/src/language/dynamics/transition/PatternMatch.re`

### 5.1 Pattern Ascription Handling (line 72)

```reason
| Asc(p, t1) =>
  recur(p, Ascriptions.transition_multiple(Asc(d, t1) |> DHExp.fresh))
```

When a pattern has a type annotation, `PatternMatch` creates a new `Asc(d, t1)` expression with `DHExp.fresh`, then runs `Ascriptions.transition_multiple` on it. This triggers all the type manipulation in Ascriptions.re.

### 5.2 Pre-matching Ascription Transition (line 113)

```reason
let d = Ascriptions.transition_multiple(d);
```

Before matching, the scrutinee is run through `Ascriptions.transition_multiple`, which strips/pushes ascriptions. This can trigger extensive type operations.

### 5.3 Otherwise Pure Structural Matching

The rest of `match_pattern` (lines 23-73) is purely structural -- it calls `Unboxing.unbox` to destructure values and recurs on sub-patterns. No direct type creation.

---

## Part 6: `Typ.temp` and `Typ.fresh` Usage in Elaborator and Dynamics

### In Elaborator.re

| Line | Usage | Context |
|------|-------|---------|
| 104-105 | `Typ.temp` (x3) | Creating singleton labeled tuple type for pattern autolabelling |
| 277 | `Typ.temp` | Empty type for `meet_all` in ListLit |
| 475, 478 | `Typ.fresh` (x2) | `Unknown(Internal)` for Unquote constructors |

### In Ascriptions.re

| Line | Usage | Context |
|------|-------|---------|
| 35 | `Typ.temp` | Wrapping unrolled type for consistency check |
| 39 | `Typ.temp` | Wrapping unrolled type for meet |
| 48 | `Typ.fresh` | Wrapping Parens type with fresh ID |
| 122 | `Typ.temp` | Creating `Var(tyvar)` for Poly substitution |
| 123 | `Typ.temp` | Creating `Unknown(Internal)` fallback |
| 142, 143 | `Typ.temp` (x2) | Pushing ascription into If branches |
| 155 | `Typ.temp` | Pushing ascription into Match branches |
| 171, 183 | `Typ.temp` (x2) | Constructor consistency checks |
| 192, 199, 213 | `Typ.temp` (x3) | BinOp/UnOp consistency checks |

### In Transition.re

| Line | Usage | Context |
|------|-------|---------|
| 649 | `FreshGrammar.Typ.bool()` | Short-circuit AND |
| 665 | `FreshGrammar.Typ.bool()` | Short-circuit OR |

### In PatternMatch.re

No direct `Typ.temp` or `Typ.fresh` calls. Types enter via `Ascriptions.transition_multiple`.

---

## Damage Assessment

### Source 1: Elaborator -- Normalized Types in Asc Nodes (MODERATE)

The elaborator writes **normalized** types into Asc nodes via `fresh_ascription`. For recursive types (like sum types defined with `type`), normalization:
- Expands `Var("color")` into `Rec("color_t", Sum([("Red", None), ("Green", None), ("Blue", None)]))`
- Creates new type allocations for the entire expanded tree

However, these types are created once during elaboration and could theoretically maintain physical equality through evaluation if nothing disturbed them. The real damage comes from steps 2 and 3.

### Source 2: Ascriptions.re During Evaluation (MODERATE)

During evaluation, `Ascriptions.transition` and `transition_multiple` create many new type objects:
- **`Typ.unroll`**: Every Rec type gets fully substituted, creating N new objects for N-constructor sums
- **`Typ.meet`**: Creates fresh `temp` types for every structural case
- **`Typ.subst`**: Rewraps every node in the type tree
- **`Typ.temp`/`Typ.fresh`**: Direct allocations for pushing ascriptions inward

For a program with many ascriptions on large sum types, this creates thousands of fresh type objects during evaluation. But the types that survive in the final result (inside remaining Asc nodes) would be the *output* of these operations.

### Source 3: Post-Evaluation Processing (CATASTROPHIC)

**`Exp.replace_all_ids`** at Evaluator.re:249 is the single most damaging operation. It:
- Traverses the ENTIRE expression tree
- Creates a new `{...exp, annotation: IdTagged.IdTag.mk_internal([Id.mk()])}` record for EVERY node, including every type
- This means every type node in the result has a unique, freshly-allocated record
- **No two types can ever be `===` after this pass**

This is the root cause of the 2M meet calls with 17K sum meets. When post-eval statics runs `Typ.meet` on two types from the evaluated result:
- The `===` check at line 703 of Typ.re **always fails** because every type is a fresh allocation
- The `Var-Var` fast path at lines 718-720 can still catch some cases (string equality on names), but for normalized Rec types, the Var names have been expanded away
- Every structural comparison must recurse into the full type tree

### Source 4: Substitution (MODERATE)

`Substitution.in_exp` at Evaluator.re:249 also creates new records via `map_term`, but this happens before `replace_all_ids` which would destroy the physical equality anyway.

### Source 5: Web Worker postMessage (ADDITIONAL)

Not investigated in this document, but the MEMORY.md notes that `postMessage` structured clone destroys all physical equality. This is an additional layer on top of `replace_all_ids`.

---

## Recommendations

### Priority 1: Stop `replace_all_ids` from touching types

`Exp.replace_all_ids` uses `map_term` with `~f_typ=f` which traverses into types. But types don't need fresh IDs for the purpose of result display -- their IDs were already temp/invalid from elaboration. Options:
- **Skip types entirely**: Pass `~f_typ=(_, t) => t` (identity for types) to avoid creating new type allocations
- **Or**: Use a type-preserving variant that only refreshes expression/pattern IDs

This would preserve type physical equality through the post-evaluation boundary, potentially enabling `===` fast paths in post-eval statics.

### Priority 2: Type interning/deduplication after evaluation

Create a type interning table that deduplicates structurally-equal types to share physical identity. Run this on the evaluation result before post-eval statics. This would restore the `===` fast path.

### Priority 3: Reduce type allocations in Ascriptions.re

- Cache `Typ.unroll` results (same Rec type gets unrolled multiple times)
- Use physical equality checks in `Typ.subst` to avoid creating new records when the substitution is a no-op (similar to how `normalize` already does `t === t' ? ty : ...`)
- Consider memoizing `Typ.meet` results

### Priority 4: Avoid unnecessary meet calls in post-eval statics

Investigate why post-eval statics needs 2M meet calls vs 8.9K for pre-eval. The evaluated result should be simpler (values only), so fewer meet calls should be needed. The issue may be that normalized Rec types in Asc nodes force the statics to do deep structural comparisons that wouldn't be needed if types used Var references.

### Priority 5: Consider not normalizing types in the elaborator

If the elaborator kept types in their Var form (e.g., `color` instead of `Rec("color_t", Sum(...))`) in Asc nodes, then:
- Asc nodes would contain compact Var types
- Post-eval statics could use the Var-Var fast path in meet
- Normalization would only happen when actually needed (e.g., pattern matching)

This would require changes to the dynamics (Ascriptions.re, etc.) which currently expect to find structural types in Asc nodes.
