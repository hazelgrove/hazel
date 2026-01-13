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

1. A `probe_map: Id.Map.t(Probe.t)` maps expression/pattern IDs to probe metadata (refs to capture)
2. During evaluation, when an expression ID is in probe_map, it captures: value, filtered environment (only the `refs`), call stack, step counts, timestamp
3. Samples are collected into `EvaluatorState.probes: Sample.Map.t`
4. Results are sent back via structured clone (browser handles js_of_ocaml representation natively)

### Worker Architecture

- Single web worker (`Worker.re`, `WorkerServer.re`, `WorkerClient.re`)
- Request/response via postMessage with structured clone (no sexp serialization)
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

## Implementation Status

### Completed

- **Tier 0: Benchmarking** - Profiling revealed sexp serialization was the dominant cost; fixed by switching to structured clone
- **Phase A: Probe Refactor** - Moved probes from AST nodes to probe_map metadata
- **Phase B: Probe Everything** - Added `probe_all` setting to probe all expressions

### Future Work (not blocking user study)

- **Sample Cache + Staleness**: Show stale data immediately on program edit, refresh when ready
- **Viewport Tracking**: Prioritize visible expressions
- **Background Progressive Evaluation**: Evaluate broader set after immediate pass completes

---

## Status / Progress

### Phase A: Probe Refactor - COMPLETE ✓

**Goal**: Replace Probe AST nodes with a `probe_map: Id.Map.t(Probe.t)` passed to evaluator. Preserve exact current behavior (only manual/auto probes).

#### What Was Implemented

**Architecture**:

```
MakeTerm.from_zip_for_sem(zipper)
  → Collect probe IDs from refractors
  → Return (term WITHOUT Probe nodes, probe_ids: Id.Map.t(unit))

CachedStatics.init_from_term(term, probe_ids)
  → Elaborate term
  → Compute refs for each probe_id from info_map
  → Return CachedStatics.t {elaborated, probe_map: Id.Map.t(Probe.t), ...}

WorkerClient → WorkerServer
  → Send {expr, probe_map}

Evaluator.evaluate
  → For each expression, check if ID in probe_map
  → If yes, record step_start, emit RecordExpProbe on completion
```

#### Core Changes

- **MakeTerm.re** - Collects probe IDs instead of creating Probe AST nodes
- **CachedStatics.re** - Computes `probe_map: Id.Map.t(Probe.t)` from probe_ids
- **MkRefractor.re** - Uses original ID (removed `Id.transform_variant`)
- **WorkerServer.re** - Request now includes `{expr, probe_map}`
- **Evaluator.re** - Added `~probe_map` parameter, emits `RecordExpProbe` effects
- **EvaluatorState.re** - Added `probe_map` field
- **EvalResult.re** - Added `cached_probe_map` for cache invalidation (ensures probes trigger re-eval immediately)

**Backward Compatibility**: Old AST-based Probe system still runs in parallel for test compatibility. Tests construct Probe nodes directly; UI uses new probe_map system.

#### Testing Status

- Expression probes (UI): **WORKING** ✓
- Manual probes show samples correctly
- Auto-probes work correctly
- Cache invalidation fixed - probes trigger immediate re-evaluation
- Pattern probes: **WORKING** ✓
- Tests: All Probes (51) and ProbeSteps (10) tests passing

#### Notes

**Ascription ID Preservation**: Value expressions inside type ascriptions need their IDs preserved when the ascription transition fires. Without this, probes on expressions like `^^probe([1,2]) : [Int]` fail because the ascription transition creates fresh IDs.

See detailed comment at top of `Ascriptions.re`. Cases using `IdTagged.fast_copy`: Tuple, Cons, ListLit, Fun, TypFun.

Non-value expressions (If, Let, Seq, etc.) don't need this because the probe fires during sub-expression evaluation before the ascription transition.

#### Key Technical Decisions

- **Map-as-set pattern**: `Id.Map.t(unit)` instead of separate Set type
- **ID transformation eliminated**: Removed `Id.transform_variant` and `Id.recover_original` - probes use original syntax IDs
- **Minimal worker payload**: Pass only `{expr, probe_map}` to avoid deep cloning info_map
- **Cache dependency tracking**: Use Calc.t system to trigger re-evaluation on probe_map changes

#### Cleanup Tasks Completed

- Removed `transform_variant`/`recover_original` from Id.re
- Removed Test_Id_Transform.re test file
- Simplified Refractors.re and ProjectorPerform.re
- Fixed saved editor data with transformed IDs (normalized to original IDs)

---

### Phase B: Probe Everything - COMPLETE ✓

**Goal**: Extend probe_map to include all "probeable" expressions with a UI toggle.

#### What was implemented

1. **CoreSettings.re** - Added `probe_all: bool` setting (default false)
2. **Settings.re** - Added `ProbeAll` action to toggle the setting
3. **NutMenu.re** - Added "∀ Probe All" toggle in Semantics group
4. **CachedStatics.re** - Added probe_all logic:
   - `should_probe(info)`: Returns true for InfoExp and InfoPat
   - `all_probeable_ids(info_map)`: Collects all IDs passing should_probe
   - When `probe_all` enabled, uses all_probeable_ids instead of manual probe IDs
5. **CodeEditable.re** - Suppresses re-evaluation for Refractor actions when probe_all is on

**Profiling infrastructure:**

- **ScratchMode.re** - Round-trip timing logged to console
- **WorkerServer.re** - Eval-only timing logged via Printf

#### Benchmarks (Emoji Paint)

Tested with the "Emoji paint" example (~100 lines, representative debugging task):

| Mode                          | Eval Time | Round-Trip | Notes                      |
| ----------------------------- | --------- | ---------- | -------------------------- |
| probe_all OFF, 1 manual probe | ~35ms     | ~38ms      | Baseline                   |
| probe_all ON                  | ~99ms     | ~130ms     | ~3x eval, ~3.5x round-trip |

After structured clone fix, performance is acceptable for user study on fast machines. Future optimization opportunities remain (selective return, viewport awareness) but are not blocking.

#### Bug Fix: Post-Edit UI Unresponsiveness - FIXED ✓

**Previous symptom**: After an edit with probe_all enabled, the edit itself felt instant, but the UI became unresponsive for ~1 second afterward due to sexp deserialization on the main thread.

**Solution**: Replaced sexp serialization with browser's structured clone algorithm:

1. **WorkerClient.re** - Changed worker type from `Worker.worker(string, string)` to `Worker.worker(Request.t, Response.t)`, removed serialize/deserialize calls
2. **WorkerServer.re** - Removed sexp serialization, receives/sends OCaml values directly
3. **Environment.re** - Changed `cached_search_tree` from `Core.Map.t` to `Maps.StringMap.t` (Core.Map embeds a comparator function that can't be structured-cloned)

The browser's structured clone handles js_of_ocaml's runtime representation natively, eliminating the ~900ms sexp parsing overhead

#### Bug Fix: Mouse Click Re-evaluation - FIXED ✓

**Problem**: Mouse clicks were triggering unnecessary re-evaluation when probe_all was on. The action log showed:

```
Action: (MakeActive (Scratch (Cell MainEditor)))
Action: (Editors (Scratch (CellAction (MainEditor (Perform (Move (Point { row = 62; col = 5 })))))))
Action: Save
```

**Root cause**: In Page.re, `MakeActive` was using `Updated.return(~scroll_active=false)` without specifying `~is_edit=false`. Since `Updated.return` defaults to `~is_edit=true`, every mouse click was inadvertently marking the action as an edit, triggering the autosave alarm.

**Fix**: Added explicit `~is_edit=false` to `MakeActive` handler in Page.re.

---

## Known Limitations

These are documented limitations of the current probe system:

### Probe on Parens Bug

Probing a parenthesized expression like `^^probe((1 + 2))` doesn't work. The paren tile ID is added to refractors, but elaboration removes the Parens wrapper, so the ID doesn't match during evaluation. Preserving Parens in elaboration was attempted but broke evaluator/stepper consistency tests.

**Test**: `Test_Evaluator_Probes.re` - "Probe on parens (known issue: ID lost during elaboration)"

### Untestable Value Types

Some value expressions have ID preservation in Ascriptions.re but are hard to test:

- **TupLabel**: Precedence issue - `^^probe(l=1) : (l=Int)` parses as `l=(1:(l=Int))`, not `(l=1):(l=Int)`. Would need parens which hit the parens bug.
- **Fun**: Function values get Fold projectors in output, complicating test expectations.
- **TypFun**: Hits unrelated bug: `[failure] patterns should be handled separately in substitution`

The ID preservation is consistent with other value types so we keep it without explicit tests.

---

## Future Work: Probes Test Coverage

### TODO: Improve test coverage for probe edge cases

The current Probes tests cover many cases but should be expanded for regression protection.

#### Approach for Adding Tests

1. **Use Ascriptions.re as a guide**: Each case that handles `Asc(inner, type)` is a potential test case. Value expressions need ID preservation via `fast_copy`; non-value expressions don't.

2. **Only use `^^probe` syntax**: Don't use other `^^` forms (like `^^fold`) in tests - they add projectors that complicate output expectations.

3. **Watch for precedence**: The probe syntax doesn't create AST nodes, so `^^probe(x) : T` parses based on the precedence of `x : T`. To avoid precedence issues, wrap the entire probe annotation in parens: `(^^probe(l=1)) : (l=Int)` instead of `^^probe(l=1) : (l=Int)`.

4. **Check the grammar**: Look at `Exp.re` term variants to identify expression types that might need probe coverage.

#### Cases NOT to pursue

- **TypFun**: Hits `[failure] patterns should be handled separately in substitution` - a polymorphism evaluation bug, not a probe issue.

- **Fun with ascription**: Function values get Fold projectors automatically applied in output. The ID preservation is in place but untested.

- **Parens inside probe**: `^^probe((expr))` loses the ID due to parens stripping. (Parens _outside_ probe are fine.)

- **Floats**: Printed representation varies (`4.` vs `4.0`) - not worth the hassle for probe testing.

- **Test expressions**: Always return unit, no value to probe.

#### Test Coverage Plan - COMPLETE ✓

**1. More Operators** (skip floats)

- [x] Int arithmetic: +, -, \*, /, \*\*
- [x] Int comparisons: <, >, <=, >=, ==, !=
- [x] String operations: ++, $==
- [x] Boolean operations: &&, ||, !
- [x] Unary minus: -5

**2. More Literals**

- [x] Empty list: `[]`, `[] : [Int]`

**3. Dot Projection**

- [x] Labeled tuple access: `let t = (a=1, b=2) in ^^probe(t.a)`

**4. More Pattern Probes**

- [x] Cons pattern: `case [1,2,3] | ^^probe(x) :: xs => x | [] => 0 end`
- [x] List literal pattern: `case [1,2,3] | [^^probe(a), b, c] => a | _ => 0 end`
- [x] Constructor pattern: `case Some(42) | Some(^^probe(x)) => x | None => 0 end`
- [x] Nested tuple pattern: `case ((1,2),3) | ((^^probe(a),b),c) => a end`
- [x] Multiple pattern probes in same case

**5. Nested Probes (probes within probes)**

- [x] Nested probes via multi-line list literals
- [x] Nested probes via let bindings on separate lines
- [x] Deeply nested probes

**6. ADT Tests with Recursive Functions**

- [x] User-defined list ADT with recursive function (length)
- [x] Tree ADT (Leaf/Node) with recursive traversal (sum)
- [x] Multiple probes inside recursive ADT function (depth)

**7. More Recursive Patterns**

- [x] List operations returning lists: reverse, map
- [x] Multiple probes at different recursion depths (fib)

**8. More Compound Nesting**

- [x] Case inside let inside if
- [x] Multiple nested lets with probes

**Note on nested probes**: Only one probed term can end on a given line. Tests use multi-line list literals and let expressions to ensure each probed term ends on a different line.

---

## Commit History

- **d3bedd0cc** - Phase A: Refactor probes from AST nodes to probe_map metadata

  - Pure refactor moving probe tracking from AST wrapper nodes to probe_map passed through evaluation pipeline
  - Expression probes now work correctly with immediate sample display
  - Cache invalidation fixed - probes trigger re-evaluation when added/removed
  - Both old and new probe systems running in parallel for test compatibility

- **TBD** - Add pattern probe support to probe_map system
  - PatternMatch.re: Added probe_map parameter, sample collection during pattern matching
  - Transition.re: Threading probe_map to PatternMatch.matches calls
  - Evaluator.re: Pass state.probe_map to transition
  - Test infrastructure: Probes and ProbeSteps tests updated/relaxed for new system
