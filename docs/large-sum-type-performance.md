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

### Post-eval statics benchmark results:

| Program | Pre-eval statics | Post-eval statics | Ratio |
|---------|-----------------|-------------------|-------|
| counter | 7.5ms | 408ms | 54x |
| mvu_counter | 12ms | 595ms | 50x |
| keyboard_game | 14ms | 360ms | 26x |
| full_app | 23ms | 2,353ms | 102x |

### Post-eval meet stats (full_app):
- Pre-eval: 8,906 meet calls, 0 sum meets, 0 rec-rec
- Post-eval: 2,021,652 meet calls, 17,733 sum meets, 420 rec-rec

The `===` optimization is completely ineffective on the evaluated result.

### Why post-eval statics is slow

Two factors destroy physical equality:

1. **Normalized types in ascriptions**: The elaborator writes normalized types
   (e.g., `Rec("HTML", Sum(47 ctrs))`) into Asc nodes. When post-eval statics
   encounters `Asc(expr, Rec(...))`, it uses the expanded Rec form, defeating
   the Var-Var fast path in meet. (Hypothesis — needs verification.)

2. **Web worker boundary**: `postMessage` uses the structured clone algorithm
   which does NOT preserve reference identity. All physical equality is destroyed
   when the evaluated result crosses the web worker boundary. Even if the evaluator
   preserved type sharing, structured cloning would break it.

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

## What to Do Next

1. **Fix post-eval statics** (HIGHEST PRIORITY, 2.3s for full_app):
   - Investigate what types appear in ascriptions in the evaluated result
   - Check Ascriptions.re and Transition.re for type handling in evaluator
   - Options: type dedup pass before statics, keep types as Vars in ascriptions,
     or intern types after web worker crossing
   - Add ExpToSegment and elaborate-on-eval-result to benchmark

2. **Web worker physical equality**: Structured cloning breaks all `===`.
   After results cross the web worker boundary, need a dedup/intern pass
   to restore physical equality. This single fix could solve post-eval statics.

3. **Investigate duplicate ID downstream effects**: Check these call sites for
   issues with duplicate type IDs in elaborated/evaluated code:
   - Results display panel, probes, UI decoration mapping
   - If needed, add one-time UUID dedup pass before display

4. **Benchmark additions needed**:
   - Post-eval statics: DONE (added to bench.re)
   - Post-eval elaboration: TODO
   - ExpToSegment.exp_to_segment on eval result: TODO
   - Full editor chain timing: TODO (lower priority)
