# Large Sum Type Performance Investigation

## Summary

HTML (47 constructors), Attr (~35), Sub (~10), Cmd (~10) are large sum types
defined as builtins. They cause performance problems in statics and elaboration
because `Typ.meet` on Sum types is expensive (O(n²) via `ConstructorMap.meet`'s
`venn_regions`), and `Typ.normalize` on Sum types traverses all constructors.

## Empirical Data

### Profiling Results (full_app benchmark, cache OFF)

```
Statics (252ms total):
  meet calls:     238,098
  Sum-Sum meets:  1,167  (all from Rec-Rec unwrapping, all size 47 = HTML)
  Sum meet time:  ~150ms (53% of statics)
  Var==Var fast:  89,588  (working correctly)
  Rec-Rec meets:  1,167  (exactly equals Sum-Sum count)
  Normalize:      10,659 calls, ~3ms (negligible)

Elaboration (1,404ms total):
  meet calls:     89,631
  Sum-Sum meets:  786    (775 from Rec-Rec, 11 from elsewhere)
  Sum meet time:  ~170ms (12% of elab)
  Rec-Rec meets:  19     (each triggers many nested Sum meets)
  Normalize:      1,543,186 calls, ~520ms (37% of elab)
  Unaccounted:    ~700ms (52%)
```

### Key Finding: ALL Sum-Sum meets come from Rec-Rec unwrapping

For statics: `sum_from_rec == sum_calls` (1167 == 1167). Every single Sum-Sum
meet happens because `meet` unwraps two Rec types and then meets their Sum bodies.

Zero physical equality hits on Sum types — the compared constructor maps are
never the same allocation.

## How Types Flow Through the System

### Type Definitions (BuiltinsADT.re)

Builtin types are defined using `Var` references for cross-references:
```reason
// HTML constructor argument types use Var form:
let elem_body = () => prod([list(var("Attr")), list(var("HTML"))]);
// HTML type wraps in Rec for recursion:
let t = Rec(TPat.Var("HTML"), Sum([Variant("Div", _, Some(elem_body())), ...]))
// Attr type is a bare Sum (no Rec needed — not recursive):
let attr = Sum([Variant("Id", _, Some(string())), ...])
```

### Context Storage (Ctx.re)

Constructor entries use `Var(name)` for the return type (line 169-172):
```reason
| None => Var(name) |> fresh           // e.g., Var("HTML")
| Some(typ) => Arrow(typ, Var(name) |> fresh) |> fresh
```

The carried types (`typ`) come directly from the constructor map, preserving
the original `Var("Attr")`, `Var("HTML")` references.

Type aliases stored as `Singleton(ty)` where `ty` is the full structural form
(e.g., `Rec("HTML", Sum(...))` or bare `Sum(...)` for non-recursive types).

### The Var-Var Fast Path in meet (WORKS)

```reason
| (Var(n1), Var(n2)) =>
    if (n1 == n2) { Some(ty1) }  // O(1) — fires 89,588 times for full_app
```

This correctly handles `meet(Var("HTML"), Var("HTML"))` → instant return.

### Where Types Get Expanded

1. **`Typ.normalize`**: Expands `Var(x)` → looks up alias → recursively normalizes.
   Used in elaboration (1.5M calls for full_app). Not used much in statics.

2. **`Typ.weak_head_normalize`**: Expands only the outermost `Var`. Used in statics
   for pattern matching on the outer type constructor (e.g., `matched_arrow`,
   `matched_list`, `matched_prod`). Preserves inner Vars.

3. **`Ctx.lookup_alias`**: Returns the full structural form. Used by `meet` itself
   when it encounters a `Var` and expands it.

4. **`meet`'s own Var expansion**: When `meet(Var("HTML"), something_not_Var)`,
   it looks up the alias and recurses with `meet(Rec("HTML", Sum(...)), something)`.

### The Rec-Rec Problem (ROOT CAUSE)

When `meet` expands a Var, it gets `Rec("HTML", Sum(...))` from `lookup_alias`.
If both sides are the same Var, Var-Var catches it. But when the types arrive
as `Rec(...)` directly (not as Vars), the Rec-Rec case fires:

```reason
| (Rec(tp1, ty1), Rec(tp2, ty2)) =>
    // Alpha-rename tp1 to tp2 (no-op when same name)
    let ty1' = subst(Var(tp2), tp1, ty1);
    // Meet the bodies — THIS TRIGGERS SUM-SUM:
    let+ ty_body = meet(ctx, ty1', ty2);
    Rec(tp1, ty_body) |> temp;
```

For `Rec("HTML", Sum(47 ctrs))` vs `Rec("HTML", Sum(47 ctrs))`:
- Alpha rename is a no-op (same name)
- Then `meet(Sum(...), Sum(...))` traverses all 47 constructors via `venn_regions`
- Each constructor's carried type may contain Attr, Sub, Cmd references (as Vars)
- Inner Vars hit the Var-Var fast path, but the outer Sum traversal is O(n²)

**The Rec-Rec case has no fast path for same-type-alias Recs.**

### Open Question: Who Passes Rec Types to meet?

We confirmed that Sum-Sum meets come from Rec-Rec (100% in statics). But we
haven't traced exactly WHO calls `meet` with Rec-type arguments. Candidates:

1. **`meet` itself** from the `(Var(name), _)` expansion — if the other side
   is not a Var but a Rec or some other form, it expands the Var to Rec and
   recurses, hitting Rec-Rec.

2. **`meet_all`** (in Self.re, CoCtx.re) — the result of one meet feeds into
   the next. If `meet(Var("HTML"), Unknown(SynSwitch))` goes through the
   `(Unknown, _)` → `Some(ty2) = Some(Var("HTML"))` path, that's fine. But
   if the Var gets expanded during meet and the result is Rec, subsequent
   meets would receive Rec types.

3. **External callers** — `Info.status_common` passes `ana` and `syn` to meet.
   If either is already a Rec (from being stored as a Rec somewhere), that
   triggers the Rec-Rec path.

**NEXT STEP**: Add logging at `Info.status_common` (line 431) to check if `ana`
or `syn` arrive as Rec types. This was attempted but not completed before
context consolidation.

## The Normalize Problem in Elaboration

Elaboration calls `normalize` 1.54M times for full_app, taking 520ms (37%).
With the normalize cache enabled, this drops to ~31ms (60x speedup), but the
cache is unsound (keyed by type ID, ignores context — causes 57 test failures).

The normalize cache bug: two different `Var("HTML")` nodes have different IDs
but normalize to the same result. The cache correctly caches per-node. But the
issue is that the cache is also used across different contexts where the same
type ID might normalize differently (though for builtins this doesn't happen).

## Results: Physical Equality Short-Circuit in meet (IMPLEMENTED)

Added `if (ty1 === ty2) { Some(ty1) }` at the top of `Typ.meet`. Results:

```
Statics speedup (full_app):
  meet calls:  238,098 → 8,906  (96% fewer)
  Sum meets:   1,167 → 0        (eliminated)
  Rec-Rec:     1,167 → 0        (eliminated)
  Time:        252ms → 21ms     (12x speedup)

Elaboration: UNCHANGED
  meet calls:  89,631 (same)
  Sum meets:   786 (same)
  Time:        1,285ms (same)
```

The types compared during statics ARE physically equal — they come from the
same context lookup. During elaboration, they are NOT — the elaborator produces
different type allocations (through normalize, which creates new types).

**This optimization is unconditionally correct**: `meet(x, x) = Some(x)` always
holds (meet is idempotent on a lattice). No shadowing or scoping concerns.

## Survey of Physical Equality (`===`) Across the Type System

### Already Have `===` Short-Circuits

- **`Typ.meet`** (line 780): Top-level `ty1 === ty2 → Some(ty1)`. IMPLEMENTED.
- **`Equality.re` — `exp`** (line 120): Top-level `e1 === e2 → true`. Already present.
- **`Equality.re` — `typ`** (line 534): Top-level `t1 === t2 → true`. Already present.
- **`ConstructorMap.meet`** (line 149): `m1 === m2 → Some(m1)`. Already present.
- **`ConstructorMap.match_synswitch`** (line 181): `m1 === m2 → m1`. Already present.
- **`ConstructorMap.equal`** (line 198): `m1 === m2 → true`. Already present.
- **`Typ.normalize`** (internal): Uses `===` to preserve physical identity when
  children are unchanged (e.g., `t === t' ? ty : List(t') |> rewrap`). This is
  the identity-preserving pattern that feeds the `===` check in `meet`.

### Not Candidates for `===`

- **`Typ.subst`** (line 437): Substitutes one type into another. Inputs are
  structurally different by design. No benefit from `===`.
- **`Typ.normalize`** (top-level): Single-argument function, no second argument
  to compare against. Not applicable.
- **`Typ.weak_head_normalize`**: Same — single argument.
- **`Typ.equal`** (line 481): Delegates to `Equality.syntactic.typ`, which
  already has `===`. Already covered.
- **`Typ.match_synswitch`** (line 928): Could add `t1 === t2` at the top (valid
  since physically equal types have no synswitches to replace). But this function
  is called once per expression during elaboration, and already delegates to
  `ConstructorMap.match_synswitch` which has `===`. Marginal benefit.

### Key Insight: normalize Destroys Physical Equality

`normalize` creates new type wrappers via `rewrap` whenever any child changes
(even just expanding a Var). Types that were physically equal before normalization
are no longer physically equal after. This is why the `===` check in `meet`
helps statics (types stay as Vars, same context lookup → same object) but NOT
elaboration (types go through normalize, producing new allocations).

## Elaboration Breakdown (PROFILED)

Instrumented the elaborator to decompose elaboration time. For each expression
node, `elaborated_type()` does: info map lookup → `match_synswitch` → `normalize`
→ `all_ids_temp`. After all elaboration, `fix_typ_ids` gives real IDs to all types.

### full_app Results

```
Total elab:          1,273ms
├── elaborated_type:  1,000ms (79%)
│   ├── all_ids_temp:   698ms (55% of total elab)
│   ├── normalize:      421ms (33%)
│   ├── match_synswitch:  1ms (~0%)
│   └── info map lookup:  ~0ms
├── fix_typ_ids:       19ms (1.5%)
└── other (traversal): 273ms (21%)
    (recursive elaborate calls, pattern matching, allocation)
```

### All Programs Breakdown

```
Program           elab_type   norm    synswitch  all_ids_temp  fix_ids  other
simple_let          0.0ms     0.0ms    0.0ms       0.0ms       0.1ms    0.0ms
fibonacci           0.1ms     0.0ms    0.0ms       0.0ms       0.1ms    0.1ms
counter           199.5ms    83.8ms    0.2ms     137.4ms       0.2ms   60.3ms
mvu_counter       212.4ms    94.9ms    0.2ms     151.4ms      35.4ms  119.2ms
keyboard_game     144.6ms    63.8ms    0.2ms     103.2ms      16.8ms   72.1ms
animation         152.3ms    70.0ms    0.2ms     100.5ms       0.2ms   50.2ms
full_app          999.9ms   421.3ms    0.7ms     697.6ms      19.1ms  272.9ms
```

### Key Finding: `all_ids_temp` Is the Biggest Cost

`all_ids_temp` alone accounts for 55% of elaboration time (698ms for full_app).
It unconditionally traverses and rewrites EVERY node in the type tree, replacing
all IDs with temp IDs. This is called once per expression/pattern node via
`elaborated_type()` and `elaborated_pat_type()`.

This also explains why `===` in meet doesn't help elaboration: even if `normalize`
perfectly preserved physical identity, `all_ids_temp` destroys it immediately
after. The flow is:

```
Info map → match_synswitch → normalize → all_ids_temp (destroys ALL identity)
                                                ↓
                                        new type allocation
                                                ↓
                                  meet sees different objects → full O(n²)
```

After elaboration, `fix_typ_ids` does yet another full traversal to assign
real IDs, but this is only 19ms (1.5%) — not a significant cost.

`match_synswitch` is essentially free (0.7ms total). Not a factor.

### Why `all_ids_temp` Exists

Comment in Elaborator.re (line 499-502):
```
/* This function gives a new id to all the types
   in the expression. It does this to get rid of
   all the invalid ids we added to prevent generating
   too many new ids */
```

During elaboration, types get temp IDs (via `Typ.temp`) to avoid generating
many fresh UUIDs. Then `fix_typ_ids` assigns real IDs in one pass at the end.
The question is whether this two-pass approach (temp IDs → real IDs) is still
necessary, or whether it could be eliminated or made cheaper.

## all_ids_temp + fix_typ_ids: DISABLED (Feb 7, 2026)

Both `all_ids_temp` (in `elaborated_type`/`elaborated_pat_type`) and `fix_typ_ids`
(post-pass in `uexp_elab`) have been disabled. Results:

```
full_app elab: 1,273ms → 703ms (1.8x speedup, 45% reduction)
  - all_ids_temp: 698ms → 0ms (eliminated)
  - normalize:    421ms → 469ms (unchanged)
  - fix_typ_ids:  19ms  → 0ms  (eliminated)
```

Quick tests pass. Full test suite pending.

### Why this is safe (probably)

The two-pass scheme (temp IDs during elaboration → real IDs at end) was introduced
to avoid UUID generation cost. But the traversal cost of all_ids_temp itself became
the bottleneck (55% of elab time). The types coming out of normalize already have
real IDs from the statics map — all_ids_temp was destroying them, then fix_typ_ids
was assigning new ones. Skipping both means types keep their original IDs.

### Potential downstream issues (INVESTIGATE)

Duplicate IDs in elaborated/evaluated code could cause display decoration association
problems in:
1. **Results display panel** — sometimes shows elaborated code, normally shows
   evaluated code
2. **Probes** — display evaluated code
3. **Any UI that uses type IDs for decoration/cursor tracking**

If issues arise, the fix is a one-time UUID dedup pass before display (much cheaper
than doing it during elaboration for every expression node). This is strictly better:
only pay the traversal cost when actually displaying, not during elaboration.

### Historical context

- `Typ.mk_fast`/`Typ.temp` introduced 2024-04-24 by Matt Keenan ("statics + elab
  performance") to avoid UUID generation cost during elaboration.
- `all_ids_temp` + `fix_typ_ids` wiring introduced 2024-08-07 ("Speed up elaborator")
  to replace `DHExp.replace_all_ids_typ` which was even more expensive.
- The original optimization target was UUID generation, but now the traversal itself
  is the bottleneck, making the whole scheme counterproductive.

## Normalize Cache: REMOVED

The normalize cache (memoize by type ID) was attempted but removed because:
1. **Unsound**: Keyed by type ID only, ignoring context. 57 test failures.
2. **Inconsistent perf**: Some programs 10x faster, others actually slower.
   full_app was 0.9x (slower) with cache ON despite 1.54M→1207 call reduction.
3. **Wouldn't help anyway**: Even with the cache, `all_ids_temp` destroys
   physical identity, so `===` in meet still can't fire for elaboration types.

The `normalize_cache_enabled` ref and all cache machinery have been removed
from `Typ.re`. Profiling counters for normalize calls/timing remain.

## Potential Further Fixes (Ordered by Impact/Feasibility)

### 1. Fix Normalize Cache Soundness (HIGHEST IMPACT for elab)

The cache is keyed by type ID. The bug is that the same ID in different contexts
could normalize differently. Options:
- Only cache for builtin types (they never change across contexts) — **safest**
- Key on `(id, context_hash)` — expensive to hash contexts
- Key on `(id, scope_depth)` — simpler but less precise
- Clear cache when context changes (scope entry/exit)

For builtins, the context is always the same, so ID-keying is safe. The failures
come from user-defined types where the same ID appears in different scopes.

Combined with the `===` check in meet, a sound normalize cache would make
elaboration types physically equal again, enabling the meet short-circuit.

### 2. Type Identity Tag (MEDIUM IMPACT, MEDIUM RISK)

Add an optional origin identifier to types. When normalizing/expanding a type
alias, tag the result with the alias's UUID. In meet, same origin → same type.

**Pro**: O(1) identity check, semantically correct (UUIDs are unique).
**Con**: Changes the type representation, touches many files.

### 3. Prevent Rec Types from Reaching meet (LOWER PRIORITY)

Instead of fixing meet, ensure callers always pass Var types (not Rec). This
means not expanding type aliases until structurally necessary. The Var-Var fast
path already handles same-alias cases in O(1). Less important now that `===`
handles statics, but could help elaboration.

### 4. Hash-Based Constructor Maps (LONG TERM)

Replace the association-list constructor maps with hash maps. This makes
`venn_regions` O(n) instead of O(n²) and `meet_entry` lookups O(1).

## Key Files

- `src/language/term/Typ.re` — normalize (~line 625), meet (~line 761), cache toggle (~line 606)
- `src/language/statics/ConstructorMap.re` — meet (line 140), venn_regions (line 105)
- `src/language/statics/Info.re` — status_common (line 423), meet call (line 431)
- `src/language/statics/Self.re` — of_ctr (line 211), meet_all calls
- `src/language/statics/CoCtx.re` — meet (line 71)
- `src/language/statics/Ctx.re` — add_ctrs (line 157), lookup_alias (line 147)
- `src/language/builtins/BuiltinsADT.re` — HTML/Attr/Sub/Cmd definitions
- `src/language/statics/Statics.re` — main traversal, normalize calls
- `src/language/statics/Elaborator.re` — elaborate, normalize calls
- `bench/bench.re` — benchmark executable with profiling counters
- `bench/dune` — build config with source maps

## Current Instrumentation

`Typ.re` has these profiling counters (currently active):
- `meet_calls`, `meet_sum_calls`, `meet_sum_time_ms`
- `meet_var_eq`, `meet_rec_rec`, `meet_in_rec` (bool), `meet_sum_from_rec`
- `normalize_calls`, `normalize_cache_hits`, `normalize_total_ms`, `normalize_depth`
- `reset_meet_stats()`, `reset_normalize_cache()`

`bench.re` prints `[PROF]` lines with these counters per program.

## Running the Benchmark

```bash
cd /Users/andrewblinn/Dropbox/projects/hazel-projector-html
dune build bench/bench.bc.js && node _build/default/bench/bench.bc.js
# Filter for profiling:
... | grep PROF
```

## Lazy Normalize: IMPLEMENTED (Feb 7, 2026)

Removed `Typ.normalize` from `elaborated_type`/`elaborated_pat_type`. Normalize
now only happens at specific use sites:

- `uexp_elab` return: normalize final type once
- `fresh_ascription`: internalized normalize (try fast_equal first, normalize fallback)
- Case-specific: Asc, Constructor, TypAp, Pat Asc, Pat Constructor (kept as-is)
- Let label rearrangement: changed from normalize to weak_head_normalize
- `get_labels`: already does weak_head_normalize internally, no change needed

Results for full_app:
```
Original elab:           1,273ms
After all_ids_temp off:    703ms  (1.8x)
After lazy normalize:      151ms  (8.4x from original)

Normalize calls: 1,543,190 → 438,452 (71% reduction)
Meet calls in elab: 89,048 → 184 (99.8% reduction!)
Sum meets in elab: 786 → 0 (eliminated)
```

The `===` in meet now fires for elaboration too, because types aren't being
destroyed by all_ids_temp or redundant normalize.

## Post-Eval Statics: THE NEW BOTTLENECK (Feb 7, 2026)

After evaluation, the result is displayed in the footer. This involves:
1. `ExpToSegment.exp_to_segment(exp)` — convert eval result to displayable code
2. `Statics.mk` on the eval result — full type checking
3. `Elaborator.uexp_elab` on the eval result — full elaboration (via CachedStatics.init)

### Post-eval full pipeline benchmark (Feb 8, 2026):

| Program | Statics | Elab | Eval | Post Statics | Post Elab | E2S | Total Post-Eval |
|---------|---------|------|------|-------------|-----------|-----|-----------------|
| simple_let | 0.8 | 0.0 | 0.2 | 0.1 | 0.2 | 0.3 | 0.6 |
| fibonacci | 0.6 | 0.1 | 3.7 | 0.0 | 0.2 | 0.0 | 0.2 |
| counter | 7.3 | 29.8 | 0.6 | 390.8 | 111.9 | 3.0 | 505.7 |
| mvu_counter | 9.3 | 33.1 | 23.4 | 548.6 | 16.7 | 41.0 | 606.3 |
| keyboard_game | 10.9 | 20.3 | 21.1 | 263.8 | 9.9 | 46.1 | 319.8 |
| animation | 14.4 | 17.8 | 1.6 | 148.6 | 8.1 | 1.8 | 158.5 |
| full_app | 19.2 | 191.6 | 29.1 | 2133.1 | 85.3 | 138.1 | 2356.5 |

(All times in ms. Post Statics = `Statics.mk` on eval result. Post Elab = `uexp_elab`
on eval result. E2S = `ExpToSegment.exp_to_segment` on eval result.)

### Post-eval meet stats (full_app):
- Pre-eval: 8,906 meet calls, 0 sum meets, 0 rec-rec
- Post-eval: 2,021,652 meet calls, 17,733 sum meets, 420 rec-rec

The `===` optimization is completely ineffective on the evaluated result.

### Post-eval meet stats (all programs):

| Program | Meet Calls | Sum Meets | from_rec | var_eq | rec_rec | sum_ms |
|---------|-----------|-----------|----------|--------|---------|--------|
| counter | 360,923 | 3,165 | 3,139 | 2,482 | 73 | 522ms |
| mvu_counter | 469,145 | 4,113 | 4,044 | 3,198 | 96 | 870ms |
| keyboard_game | 251,621 | 2,199 | 2,154 | 1,704 | 54 | 403ms |
| animation | 162,048 | 1,419 | 1,376 | 1,088 | 32 | 205ms |
| full_app | 2,021,652 | 17,733 | 17,514 | 13,851 | 420 | 3,361ms |

### Notable: counter post-elab anomaly

Counter's post-eval elaboration is 111.9ms vs 8-17ms for other MVU programs.
The counter's eval result has unevaluated case expressions with sum type
constructors, triggering 79K normalize calls during post-eval elaboration.

### Why post-eval statics is slow

The primary cause is **normalized types in ascriptions**: The elaborator writes
normalized types (e.g., `Rec("HTML", Sum(47 ctrs))`) into Asc nodes. When
post-eval statics encounters `Asc(expr, Rec(...))`, it uses the expanded Rec
form, defeating the Var-Var fast path in meet. (Hypothesis — needs verification
by inspecting actual types in ascription nodes of the eval result.)

### Web worker boundary: NOT the main problem

`postMessage` uses the structured clone algorithm. Per the WhatWG spec, structured
clone maintains a "memory" map during cloning: "The purpose of the memory map is
to avoid serializing objects twice. This ends up preserving cycles and the identity
of duplicate objects in graphs."

This means:
- **Within a single `postMessage`, shared references ARE preserved.** If k elements
  of a list all point to the same type object of size w, after cloning they still
  all point to the same (cloned) object. Space stays ~w, not k*w.
- **`===` between cloned objects and objects outside the clone graph will fail.**
  E.g., comparing an eval-result type with a type from the statics context.
- **`===` between independently-allocated but structurally-equal objects still fails.**

So the web worker boundary does NOT destroy sharing within the eval result itself.
The real question is whether the evaluator/elaborator creates shared type references
across Asc nodes in the first place. If every Asc node gets its own independently
allocated `Rec("HTML", Sum(47 ctrs))`, then there's no sharing to preserve and
structured clone is irrelevant. Factor (1) — types being in expanded Rec form
instead of Var form — is the primary problem.

### Post-eval rendering pipeline (EvalResult.re)

When eval results come back:
1. `CodeSelectable.Model.mk_from_exp(~settings, exp)` (EvalResult.re:257)
   - `ExpToSegment.exp_to_segment(term)` — convert to displayable segments
   - `Zipper.unzip` — create zipper
   - `Editor.Model.mk` — create editor model
2. `CodeSelectable.Update.calculate(...)` (EvalResult.re:267)
   - `CachedStatics.init` (CodeWithStatics.re:117)
     - `Statics.mk` — full statics on eval result
     - `Elaborator.uexp_elab` — full elaboration on eval result

## Typ.meet Optimizations (Feb 8, 2026)

Implemented incrementally with benchmarks between each. All optimizations target
the `meet` function and its supporting code.

### Benchmark results (full_app post_statics):

| Optimization | post_statics | sum_ms | meet calls | Change |
|---|---|---|---|---|
| Baseline (pre-opt) | 2,249ms | 5,104ms | 2,021,652 | — |
| 1. Hash venn_regions | 1,812ms | 3,406ms | 2,021,652 | -19% |
| 2. Rec-Rec skip subst | 1,289ms | 2,238ms | 2,003,506 | -43% cumulative |
| 3. Alloc-preserving subst | ~1,289ms | ~2,200ms | 1,940,538 | -63K meets, ~same time |
| 4. Unknown-Unknown skip | ~1,270ms | ~2,150ms | 1,940,538 | minor |
| 5. Arrow/List meet skip | ~1,270ms | ~2,150ms | 1,940,538 | minor |
| 6. ConstructorMap.map phys eq | ~1,270ms | ~2,150ms | 1,940,538 | minor |
| **Final (all 6)** | **~1,270ms** | **~2,150ms** | **1,940,538** | **-44% total** |

### 1. Hash-based venn_regions (DONE)
`ConstructorMap.re`: Replaced O(n*m) `List.partition`-per-element with O(n+m)
`Hashtbl`-based lookup. For 47-constructor HTML type: ~47x speedup in inner loop.

### 2. Skip subst in Rec-Rec when var names match (DONE)
`Typ.re`: When both Rec types use the same type variable name (the common case
for `Rec("HTML", ...)` meeting `Rec("HTML", ...)`), skip the no-op substitution
that still traverses the entire body. Eliminated 420 full-body traversals, 18K
fewer meet calls via improved `===` hit rate.

### 3. Allocation-preserving subst (DONE)
`Typ.re`: `subst` always creates new allocations via `rewrap` even when the
variable doesn't appear. Fix: check `===` on recursive results, return original
if unchanged. Also fix leaf cases (Unknown, Label, ExplicitNonlabel) that
unnecessarily reconstruct via `rewrap`.

### 4. Unknown-Unknown allocation skip (DONE)
`Typ.re`: `Unknown(p1), Unknown(p2)` always allocates `temp`. When `p1 == p2`,
return `Some(ty1)` instead.

### 5. Arrow/List/TupLabel meet allocation skip (DONE)
`Typ.re`: These cases always allocate via `|> temp` even when children are
unchanged. Check if meet results are `===` to inputs and return original.

### 6. ConstructorMap.map preserve physical equality (DONE)
`ConstructorMap.re`: `map` always creates new list via `List.map`, breaking `===`
checks in `normalize` for Sum types. Track whether anything changed, return
original map `m` if not.

## Root Cause Analysis: "The Body of the Hydra" (Feb 8, 2026)

The single core problem: **types lose their compact identity as they flow through
the system, and the consumers that need to compare them don't have the information
to resolve them back.**

### The chain

1. **Builtins start as `Var("HTML")`** in the context. Statics uses these, and
   `meet` hits the Var-Var fast path. Pre-eval statics is fast (19ms, 0 sum meets).

2. **The elaborator normalizes them**: `fresh_ascription` turns `Var("HTML")` into
   `Rec("HTML", Sum(47 ctrs))` in Asc nodes. This is forced because...

3. **Ascriptions.re uses `Ctx.empty`**: Every `meet`/`is_consistent` call in
   `Ascriptions.re` passes `Ctx.empty`. With no context, it can't resolve
   `Var("HTML")`. So the elaborator **must** pre-normalize types into structural
   form before handing them to evaluation.

4. **Post-eval statics sees expanded types**: When `Statics.mk` runs on the eval
   result, it encounters `Rec("HTML", Sum(47 ctrs))` in every Asc node. Can't use
   the Var-Var fast path. Does full O(N^2) constructor comparison. 2M meets, 17K
   sum meets.

**Root cause: Ascriptions.re has no type resolution capability, which forces
upstream normalization, which poisons downstream statics.**

### Additional damage sources

- **`Exp.replace_all_ids`** (Evaluator.re:249): Traverses the entire expression tree
  including all types, giving every node a fresh `Id.mk()`. Unconditionally destroys
  all physical equality. The concern is purely view-level (duplicate IDs confuse
  display). Should potentially be moved to the view layer.

- **Post-eval elaboration**: `CachedStatics.init` runs both `Statics.mk` AND
  `Elaborator.uexp_elab` on the eval result. The elaboration costs 85-120ms and
  may be unnecessary — likely an artifact of the caching infrastructure.

## Remediation Options (Feb 8, 2026)

### Option A: Builtin-specific — Give Ascriptions.re a real context (SELECTED)

Replace `Typ.meet(Ctx.empty, ...)` with `Typ.meet(builtin_ctx, ...)` in
Ascriptions.re. This lets types stay as `Var("HTML")` through evaluation, hitting
the Var-Var fast path. Zero changes to the AST representation, zero postMessage
concerns. Implementation: thread the builtin alias map (or full initial context)
through the evaluator to Ascriptions.re.

**Key architectural change**: Ascriptions.re currently assumes all types are
pre-normalized (no unresolved Vars). After this change, it MUST NOT assume
normalization — it resolves types lazily via `weak_head_normalize(ctx, t)` before
structural pattern matching. This is documented prominently in Ascriptions.re.

**Incremental toward type closures (Option B)**: The Ascriptions.re changes
(removing normalization assumption, adding context-based lazy resolution) are
identical to what Option B requires. The only difference is where the context
comes from: Option A threads a single `builtin_ctx`, Option B would use per-type
context closures. So this work is NOT throwaway — it's the first step toward
the principled long-term architecture.

**Display benefit**: Types in the eval result stay as compact `Var("HTML")` instead
of expanded `Rec("HTML", Sum(47 ctrs))`. This also addresses large-output display
problems — no need for post-hoc alias replacement in the view layer.

Concretely:
- Store `Builtins.ctx_init` (or the alias subset) on `EvaluatorState.t`
- Thread it to `Ascriptions.transition` (5 call sites in Transition.re)
- In Ascriptions.re: add `weak_head_normalize(ctx, t)` before structural matching,
  replace 6 `Ctx.empty` calls with the threaded context
- In the elaborator, modify `fresh_ascription` — keep types as `Var(x)` when `x`
  is an unshadowed builtin alias (check via context lookup + `===` against known
  builtin type objects)
- Shadowed builtins still get normalized as before (correct behavior)

**Shadowing check cost**: To determine if `Var("HTML")` refers to an unshadowed
builtin, `fresh_ascription` does `Ctx.lookup_alias(ctx, "HTML")` (O(context_depth)
linear scan) then `===` against the builtin type object (O(1)). Context depth is
typically 10-50 entries. This replaces a `Typ.normalize` call that traverses the
entire 47-constructor sum type tree — strictly cheaper. If context depth ever
becomes a concern, a `Set<string>` of currently-shadowed builtin names could be
maintained incrementally (O(1) check), but this is unlikely to be needed.

**Option C (interning) was considered but rejected**: Interning relies on physical
equality (`===`), which is destroyed by `Exp.replace_all_ids` (Evaluator.re:249).
Option A uses the Var-Var string comparison fast path in `meet`, which is robust
to any pass that clones or re-IDs types.

**Estimated impact**: Should eliminate nearly all 17K sum-sum meets in post-eval
statics, since builtins are the overwhelming source. Post-eval statics could drop
from ~1.3s to something close to pre-eval statics (19ms).

### Option B: Type closures — Pair types with contexts (PRINCIPLED, LONG-TERM)

Instead of storing bare `Typ.t` in the elaborated AST, store `(Typ.t, Ctx.t)` —
a "type closure." Types stay in compact Var form, with enough context to resolve
them lazily on demand.

**Full context approach** (recommended over alias-only):
- The `ctx` already exists at every point in the elaborator. Just reference it.
- Contexts are immutable lists in Reason, sharing tails. Structured clone preserves
  shared references within a single `postMessage`. Total serialized data is O(N)
  where N is unique bindings — each binding entry appears exactly once.
- The builtin type definitions (the big sum types) appear once in the base context
  and are shared by every deeper context. Structured clone serializes them once.

**Alias-only approach** (analyzed, NOT recommended):
- Per attachment cost: O(N) per type attachment point to filter ctx for aliases.
  M attachment points gives O(N*M). Expensive.
- Incremental alternative: Maintain separate alias map, add on `type` definitions
  entering scope. O(1) per scope change. But requires threading extra data.
- Savings are modest since sharing already handles the serialization concern.

**Conclusion**: "The alias-only approach is NOT obviously cheaper overall. The full
context approach is simpler and probably equivalent in practice. Just pair (type, ctx)
— zero extra elaborator work, structured clone handles the sharing, done."

This is the right long-term architecture for user-defined types, not just builtins.
Option A is the first incremental step — its Ascriptions.re changes (removing the
normalization assumption, adding lazy resolution via context) are shared with this
approach. Moving from A to B later requires only changing the context source, not
the consumer code.

### Option C: Type interning

Create a canonical table of builtin `Typ.t` objects. Whenever the elaborator writes
a type that normalizes to a builtin, substitute the canonical interned object. All
Asc nodes referring to `HTML` then share the same physical object; `===` fires
immediately. ~20-30 lines of code.

### Other potential wins (lower priority)

- **Skip post-eval elaboration**: Check if `Elaborator.uexp_elab` on the eval result
  is actually needed by the display code, or if it's just an artifact of CachedStatics
- **Move `replace_all_ids` to view layer**: Instead of running it at end of evaluation,
  run it in `ExpToSegment` or `CodeSelectable.Model.mk_from_exp`
- **Dedicated `is_consistent_bool`**: Avoid all allocation in consistency checks by
  returning bool directly instead of building a result type via `meet` and discarding it
- **Investigate duplicate ID downstream effects**: Check results panel, probes, UI
  decoration for issues with duplicate type IDs

## Completed Optimizations

- **Benchmark additions**: DONE (`[POST]`, `[PELAB]`, `[E2S]` lines)
- **IdTag.temp unthunked** (Feb 8, 2026): Changed from thunked function to constant.
  No algorithmic change but saves unnecessary allocations.
- **Statics `===` in meet**: Pre-eval statics 252ms → 21ms (12x)
- **Lazy normalize + disable all_ids_temp**: Elab 1,273ms → 151ms (8.4x)
- **Hash venn_regions**: sum_ms -33%
- **Rec-Rec skip subst**: sum_ms -34% more, 43% cumulative on post_statics
- **Alloc-preserving subst**: 63K fewer meet calls (2M→1.94M)
- **Unknown-Unknown skip**: Minor allocation savings
- **Arrow/List/TupLabel meet skip**: Minor allocation savings
- **ConstructorMap.map phys eq**: Minor allocation savings
- **All 6 meet optimizations combined**: post_statics 2,249ms→~1,270ms (-44%)

## Post-Eval Statics: SOLVED (Feb 8, 2026)

Implemented **Option A** (compact builtin types + lazy resolution in Ascriptions.re).

### Root cause (confirmed empirically)

Shape diagnostics in bench.re revealed the source:
- Constructor annotations had `has_rec=49` after elaboration (49 of 79 constructors)
- Asc nodes had `has_rec=0` — Asc nodes were NOT the source
- The Rec types came from `normalize_ctr_type` falling through to full normalize

The chain:
1. `Self.ctr_ana_typ` → `get_sum_constructors` unrolls Rec → `ana = Arrow(_, Rec(...))`
2. `Typ.meet(ana, syn)` where syn has `Var("HTML")` → var_expand → result has Rec
3. `Info.InfoExp.ty` = meet result with Rec (from `status_common`)
4. `normalize_ctr_type` gets `Arrow(_, Rec(...))`, doesn't match `Arrow(_, Var(name))`
5. Falls through to `Typ.normalize` → fully expands everything
6. Post-eval statics hits these → 2M meet calls, 17K sum meets

### Fix: `compact_builtin_recs` in Elaborator.re

New function that recursively replaces `Rec(tp, _)` → `Var(name)` for unshadowed
builtin type aliases. Called from `normalize_ctr_type` when the fast path
(`Arrow(_, Var(name))`) doesn't match. Combined with:
- `fresh_ascription`: stores unnormalized type `ty` (not `ty_n`) in Asc nodes
- `Ascriptions.re`: resolves lazily via `weak_head_normalize(builtin_ctx, t)`
- `Evaluator.re`: calls `Ascriptions.set_ctx(Builtins.ctx_init(None))` at start

### Results

| Program | Post Statics (before) | Post Statics (after) | Speedup |
|---------|----------------------|---------------------|---------|
| counter | 391ms | 1.0ms | 391x |
| mvu_counter | 549ms | 1.0ms | 549x |
| keyboard_game | 264ms | 2.1ms | 126x |
| animation | 149ms | 1.3ms | 115x |
| full_app | 2,133ms | 3.7ms | **577x** |

Meet stats (full_app post_statics):
- Meet calls: 2,021,652 → 2,578
- Sum meets: 17,733 → **0**
- Rec-Rec: 420 → **0**
- Var-Var eq: 4 → **96**

Shape data after fix: `has_rec=1` in elab/eval (down from 49). One remaining
has_rec is a non-builtin edge case, not performance-critical.

### Investigation of `replace_all_ids` (NOT the cause)

`Exp.replace_all_ids` (Evaluator.re:252) was initially suspected as a source
of expanded types, but investigation showed `Constructor(_) => term` in
`TermBase.re:map_term` — Constructor is ATOMIC, so neither `replace_all_ids`
nor `Substitution.in_exp` traverses constructor type annotations. The expanded
Rec types are introduced during ELABORATION, not evaluation. `replace_all_ids`
was restored with a TODO note for future removal (redundant with per-substitution
freshening in Substitution.re:34).

### Compatibility with type closures (Option B)

`compact_builtin_recs` is a step TOWARD type closures:
- Same pattern: keep types compact (Var refs), resolve lazily via context
- Ascriptions.re changes (lazy resolution via `weak_head_normalize`) are shared
- When type closures arrive, `compact_builtin_recs` becomes unnecessary
- Easy to unwind: remove function, revert `fresh_ascription` (1 line),
  remove `Ascriptions.set_ctx` (1 line)

## Cumulative Performance Summary

All measurements on `full_app` benchmark (HTML app with 47-constructor sum types).

| Stage | Original | After all fixes | Speedup |
|-------|----------|----------------|---------|
| Statics | 252ms | 16ms | 16x |
| Elaboration | 1,273ms | 2.7ms | 471x |
| Evaluation | 29ms | 2.6ms | 11x |
| Post-eval statics | 2,133ms | 3.7ms | 577x |
| Post-eval elab | 85ms | 3.4ms | 25x |
| ExpToSegment | 138ms | 2.5ms | 55x |
| **Total pipeline** | **~3,910ms** | **~31ms** | **~126x** |

## Type Flow Investigation (Feb 8, 2026)

See `docs/type-flow-investigation.md` for detailed analysis of how types flow through
elaboration → evaluation → post-eval statics, focusing on Ascriptions.re.
