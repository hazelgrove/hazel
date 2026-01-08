# Progressive Sample Accumulation System

## Problem Statement

When exploring a **static program** (not actively editing), moving around—changing cursor position, placing new probes, or navigating to different functions—currently triggers fresh evaluation work. This feels slow and unresponsive even though the program hasn't changed.

The core insight is: **if the program is static, we should be able to pre-compute and cache sample data progressively**, so that when you navigate somewhere, the data is already there (or appears quickly with minimal additional work).

### Target Use Cases

**Primary: Debugging tasks on static programs**
- Programs of a few hundred lines (a few screens)
- Typically <100 samples per expression, occasionally more but usually significantly less
- User study involving debugging tasks where users explore existing code
- Example: "Emoji paint" - representative of debugging task complexity

**Secondary: Writing small programs with auto-probe**
- Live development with auto-probe following cursor
- As lines are written, intermediate values appear
- Currently not experiencing significant performance issues here, but the system should support this well

### Current Pain Points

1. **Auto-probe latency**: When cursor moves to a new function, auto-probe instruments that function, triggering re-evaluation. There's a noticeable delay before samples appear.

2. **Manual probe placement**: Adding a new probe requires re-evaluation even if the program hasn't changed.

3. **No data reuse**: Moving away from a probed region and back discards and recomputes the same data.

### Goals

- Make probe exploration feel instant on static programs (cache hit)
- Show stale data immediately on program edit, refresh when ready
- Enable future features like dependency-driven probing by having broader sample coverage

---

## Current System Overview

### How Probes Work

1. Probes wrap expressions in the ASG as `Probe(expr, {refs})` where `refs` is statically determined
2. During evaluation, when a probe is hit, it captures: value, filtered environment (only the `refs`), call stack, step counts, timestamp
3. Samples are collected into `EvaluatorState.probes: Sample.Map.t`
4. Everything is serialized as S-expressions with structure sharing and sent back from the worker

### Worker Architecture

- Single web worker (`Worker.re`, `WorkerServer.re`, `WorkerClient.re`)
- On each evaluation request: serialize expression list → postMessage → worker evaluates → serialize response → postMessage back
- If a new request comes while one is in progress, the worker is terminated and restarted
- 20-second timeout

### Auto-Probe Heuristics

Located in `AutoProbe.re`. Selects expressions to probe on each line:
- Rightmost-ending term per line, preferring larger terms
- Avoids holes, function-typed expressions, redundant variable references
- Special handling for let bindings, if expressions, multiline containers

---

## Design

### Key Concepts

**Program Identity**: A program is "the same" if its expanded term hasn't changed. Any syntax change invalidates everything (but we keep stale data visible).

**Sample Cache**: Maps `(expr_id, call_stack_hash) -> Sample`. When samples arrive for an expression, they replace existing samples for that expression.

**Priority**: What expressions to instrument, in what order. On-screen status is an orthogonal filter across categories.

**Staleness**: Samples collected before the most recent program change are "stale." Shown with visual indicator until fresh samples replace them.

---

## Implementation Tiers

### Tier 0: Benchmarking ✓ COMPLETE

Before optimizing, measure where time actually goes.

**What to measure:**
1. Instrumentation time: `Dynamics.instrument_exp` duration
2. Evaluation time: Pure computation in worker
3. Serialization time: `Response.serialize` duration
4. Transfer time: postMessage to onmessage delta
5. Deserialization time: `Response.deserialize` duration
6. Total round-trip: Request initiated to UI updated

**Approach:**
- Add `Performance.now()` calls at key points
- Console logging gated by a debug flag
- Test with representative programs (e.g., Emoji paint)

**Findings**: Initial profiling revealed serialization was the dominant cost:
- Serialize (worker): ~200ms
- Evaluate: ~150ms
- Deserialize (client): ~560ms
- Total: ~910ms (serialization was 5x more expensive than evaluation!)

**Solution**: Replaced S-expression string serialization with browser's structured clone algorithm. This required:
1. Changing `Environment.t` to use `Maps.StringMap.t` instead of `Core.Map.t` (Core.Map embeds a comparator function that can't be cloned)
2. Removing serialize/deserialize calls from WorkerClient and WorkerServer
3. Passing OCaml values directly through postMessage

The browser's structured clone handles js_of_ocaml's runtime representation natively, preserving object identity/sharing without manual structure-sharing code.

### Tier 1: Sample Cache + Staleness

**Changes:**
- Add `SampleCache.t` to model
- On evaluation result: merge into cache
- On program change: bump version (marks samples stale)
- When rendering probes: read from cache, attach staleness info
- Add `.sample-stale` CSS styling (reduced opacity or similar)

### Tier 2: Viewport Tracking

**Changes:**
- Track scroll position in model (throttled action on scroll)
- Implement `visible_expr_ids` using Measured system
- Expose for instrumentation priority decisions

### Tier 3: Priority-Based Immediate Evaluation

**Changes:**
- Modify instrumentation to accept set of expression IDs to probe
- Compute immediate_ids = (manual ∪ auto) ∩ visible
- First evaluation pass only instruments immediate_ids

### Tier 4: Background Progressive Evaluation

**Changes:**
- After immediate completes, schedule background pass
- Background instruments broader set (potentially everything)
- Merge results into cache
- Handle cancellation on program change

---

## Alternative Approaches Considered

**"Just probe everything always"**: Skip priority system, always instrument all expressions. If total time for ~100-line programs is acceptable (<100ms?), this is simpler. Benchmarking will tell us if viable.

**"Lazy evaluation on hover"**: Don't evaluate until user focuses an expression. Simpler caching but worse UX. Not preferred.

**"Streaming within evaluation"**: Worker sends samples incrementally as produced. More invasive changes. Future consideration.

---

## Open Questions

1. **Multiple programs/cells**: Cache keying for exercise mode with multiple cells
2. **Background evaluation granularity**: Probe everything in one pass, or chunk it?
3. **Worker pool**: Single worker with queuing vs. multiple workers. Start simple.

---

## Implementation Status

### Phase A: Probe Refactor ✓ COMPLETE

**Goal**: Replace Probe AST nodes with a `probe_map: Id.Map.t(Probe.t)` passed to evaluator. Preserve exact current behavior (only manual/auto probes).

**Rationale**: This refactor was necessary before implementing progressive sample accumulation because probes are evaluation-time concerns, not syntax concerns. Moving probe tracking from AST nodes to metadata passed through the evaluation pipeline enables future features like "probe everything" mode.

**Architecture Implemented**:
```
MakeTerm.from_zip_for_sem(zipper)
  → Collect probe IDs from refractors
  → Return (term WITHOUT Probe nodes, probe_ids: Id.Map.t(unit))

CachedStatics.init_from_term(term, probe_ids)
  → Elaborate term
  → Compute refs for each probe_id from info_map
  → Return CachedStatics.t {elaborated, probe_map: Id.Map.t(Probe.t), ...}

WorkerServer.work({expr, probe_map})
  → Pass probe_map to evaluator

Evaluator.evaluate(~probe_map, ~env, expr)
  → Check probe_map at start of evaluation
  → Emit RecordExpProbe effect when probed expression finishes
```

**Key Implementation Details**:

1. **MakeTerm.re** - Collects probe IDs instead of creating Probe AST nodes
   - Uses `Id.Map.t(unit)` as map-as-set for O(log n) membership
   - Stores probe IDs for both expressions and patterns
   - **Note**: This collection step can be optimized away in the future by extracting probe IDs directly from `z.refractors.manuals` in CachedStatics

2. **CachedStatics.re** - Computes probe_map from probe_ids using static analysis
   - Added `probe_map: Id.Map.t(Probe.t)` field to type `t`
   - Uses `Statics.Map.refs_in` for expression probes (captures referenced variables)
   - Uses `Statics.Map.bound_in` for pattern probes (captures bound variables)

3. **MkRefractor.re** - Critical fix for sample lookup
   - Changed `add_single` to use original ID instead of `Id.transform_variant(id)`
   - Fixed mismatch where projectors looked up samples with transformed IDs but new system stores them with original IDs
   - **This was the key fix that made probes work correctly**

4. **WorkerServer.re** - Updated request/response structure
   - Changed `Request.value` from `Language.Exp.t` to `{expr, probe_map}`
   - Worker now receives probe_map alongside expression
   - Updated all callsites in view files (ScratchMode, ExerciseMode, etc.)

5. **Evaluator.re** - Uses probe_map to emit probe effects
   - Added `~probe_map` parameter to `evaluate` and `evaluate_and_limit`
   - Checks probe_map at start of evaluation to record probe start time
   - Emits `RecordExpProbe` effect when probed expression finishes evaluating

6. **EvaluatorState.re** - Stores probe_map in evaluation state
   - Added `probe_map: Id.Map.t(Probe.t)` field
   - State initialized with probe_map from evaluator

7. **EvalResult.re** - Critical cache invalidation fix
   - Added `cached_probe_map: Calc.saved(Id.Map.t(Probe.t))` field to Model.t
   - Added probe_map as calculation dependency using `and.calc probe_map = probe_map`
   - **Ensures cache invalidates when probes are added/removed**
   - Fixed issue where first probe placement didn't trigger evaluation until next program edit

**Backward Compatibility**:
- Kept Probe AST node handling in Elaborator.re and Transition.re for test compatibility
- Both old (AST-based) and new (probe_map-based) systems run in parallel
- Tests use old system (construct Probe nodes directly)
- UI uses new system (MakeTerm collects IDs, no Probe nodes created)

**Testing Status**:
- Expression probes (UI): **WORKING** ✓
- Manual probes show samples correctly
- Auto-probes work correctly
- Cache invalidation fixed - probes trigger immediate re-evaluation
- Pattern probes: **NOT YET IMPLEMENTED** (limitation documented below)

**Known Limitations**:

**Pattern Probes Not Yet Supported**

Pattern probes are not yet working with the new probe_map system. Current behavior uses `Probe(p, pr)` AST nodes in `PatternMatch.re`. When a pattern with a Probe wrapper is matched, it calls `capture(pr, dp, d, inner_match)` to generate sample closures.

To implement pattern probes with the new system:
- Pass `probe_map` to `PatternMatch.matches` function
- Check if the pattern ID is in `probe_map` during pattern matching
- Call `capture` with probe metadata from the map instead of from AST
- Thread probe_map through all Transition.re call sites (Let, FunAp, Match)

Unlike expression probes (which only needed changes in Evaluator), pattern probes require modifying `PatternMatch.matches` signature and threading probe_map through multiple call sites. This can be implemented after the old AST-based system is retired.

**Technical Decisions Made**:
1. **Map-as-set pattern**: Used `Id.Map.t(unit)` instead of separate Set type for consistency
2. **ID transformation eliminated**: No longer use `Id.transform_variant` for probes
3. **Minimal worker payload**: Pass only `{expr, probe_map}` to avoid deep cloning info_map
4. **Dual systems**: Keep both old and new probe systems running for backward compatibility
5. **Cache dependency tracking**: Use Calc.t system to trigger re-evaluation on probe_map changes

**Files Modified**:

Core implementation:
- `src/haz3lcore/lang/MakeTerm.re` - Collect probe IDs
- `src/haz3lcore/derived/CachedStatics.re` - Compute probe_map
- `src/haz3lcore/MkRefractor.re` - Fix sample ID lookup (key fix)
- `src/language/dynamics/Evaluator.re` + `.rei` - Accept and use probe_map
- `src/language/dynamics/state/EvaluatorState.re` - Store probe_map
- `src/web/util/WorkerServer.re` - Updated request structure
- `src/web/app/editors/result/EvalResult.re` - Cache invalidation fix

View files:
- `src/web/view/ScratchMode.re`
- `src/web/view/ExerciseMode.re`
- `src/web/view/TutorialMode.re`
- `src/web/view/TheoremExerciseMode.re`

Backward compatibility:
- `src/language/statics/Elaborator.re` - Kept Probe AST handling
- `src/language/dynamics/transition/Transition.re` - Kept Probe case

**Cleanup Tasks for Future**:

1. **Remove MakeTerm probe collection** (optional optimization)
   - Currently MakeTerm collects probe IDs, but this is unnecessary
   - Could extract probe IDs directly from `z.refractors.manuals` in CachedStatics
   - TODO comments added in MakeTerm.re marking this

2. **Implement pattern probes**
   - Pass probe_map to PatternMatch.matches
   - Check probe_map during pattern matching
   - Thread through all Transition.re call sites

3. **Retire old AST-based probe system**
   - Remove Probe cases from Elaborator and Transition
   - Update tests to use new system
   - Remove Probe AST constructors

---

### Phase B: Extend to "Probe Everything"

**Goal**: After pure refactor works, easily extend probe_map to include all "probeable" expressions.

**Approach**: Now that probes are metadata rather than AST nodes, we can generate comprehensive probe_map without modifying the term structure.

#### B1: Create probeable expression filter

- Add utility to identify probeable expressions
- Reuse AutoProbe filtering logic (skip holes when better options exist, skip function-typed exprs)
- Should NOT require line/cursor context
- Returns set of IDs worth probing
- Keep this modular - policy might evolve

#### B2: Generate comprehensive probe_map

- Add `comprehensive_probing: bool` to CoreSettings.t
- Default to false initially (explicit opt-in)
- In CachedStatics, if comprehensive_probing enabled:
  - Compute probeable IDs from AST/info_map
  - Union with manual/auto probe IDs
  - Compute refs for all
- If disabled, only use manual/auto (current behavior preserved)

#### B3: Add sample caching and staleness

- Add program version tracking
- Tag samples with version
- Display stale samples immediately
- Skip re-evaluation if fresh samples exist

(Details to be fleshed out when Phase B begins)

---

## Commit History

- **1ffb52268** - Consolidate probe refactor planning documents
- **d3bedd0cc** - Phase A: Refactor probes from AST nodes to probe_map metadata
  - Pure refactor moving probe tracking from AST wrapper nodes to probe_map passed through evaluation pipeline
  - Expression probes now work correctly with immediate sample display
  - Cache invalidation fixed - probes trigger re-evaluation when added/removed
  - Known limitation: Pattern probes not yet implemented (old system still used)
  - Both old and new probe systems running in parallel for test compatibility
