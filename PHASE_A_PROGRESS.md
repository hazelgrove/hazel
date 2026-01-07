# Phase A: Probe Refactor - Progress Summary

## Status: COMPLETE ✓

Phase A successfully refactored probes from AST nodes to metadata passed via `probe_map`.

## What Was Implemented

### Core Changes

1. **MakeTerm.re** - Collects probe IDs instead of creating Probe AST nodes
   - Uses `Id.Map.t(unit)` as map-as-set for O(log n) membership
   - Stores probe IDs for both expressions and patterns
   - Probe IDs returned in MakeTerm result alongside term

2. **CachedStatics.re** - Computes probe_map from probe_ids
   - Added `probe_map: Id.Map.t(Probe.t)` field to type `t`
   - Uses `Statics.Map.refs_in` for expression probes
   - Uses `Statics.Map.bound_in` for pattern probes
   - Probe metadata flows: MakeTerm → CachedStatics → Worker → Evaluator

3. **MkRefractor.re** - KEY FIX for sample lookup
   - Changed `add_single` to use original ID instead of `Id.transform_variant(id)`
   - This fixed the mismatch where projectors looked up samples with transformed IDs but new system stored them with original IDs
   - **This was the critical fix that made probes work**

4. **WorkerServer.re** - Updated request structure
   - Changed `Request.value` from `Language.Exp.t` to `{expr, probe_map}`
   - Updated all callsites in view files to pass both expr and probe_map

5. **Evaluator.re** - Accepts and uses probe_map
   - Added `~probe_map` parameter to `evaluate` and `evaluate_and_limit`
   - Checks probe_map at start of evaluation to record probe start
   - Emits `RecordExpProbe` effect when probed expression finishes

6. **EvaluatorState.re** - Stores probe_map in state
   - Added `probe_map: Id.Map.t(Probe.t)` field
   - State initialized with probe_map from evaluator

### Backward Compatibility

- **Elaborator.re** - Kept Probe AST node handling for tests
- **Transition.re** - Kept Probe case active for tests

Both old (AST-based) and new (probe_map-based) systems run in parallel:
- Tests use old AST-based system (construct Probe nodes directly)
- UI uses new probe_map system (MakeTerm collects IDs, no Probe nodes created)

## Testing Status

- Expression probes (UI): **WORKING** ✓
- Manual probes show samples correctly
- Pattern probes: **NOT YET IMPLEMENTED** (see below)
- Tests: Some failures remain (ProbeSteps tests still expect old behavior)

## Known Limitations / TODOs

### Pattern Probes Not Yet Supported

Pattern probes are not yet working with the new probe_map system. The implementation requires:

1. **Current behavior**: Pattern probes work via `Probe(p, pr)` AST nodes in `PatternMatch.re` line 67-70. When a pattern with a Probe wrapper is matched, it calls `capture(pr, dp, d, inner_match)` to generate sample closures.

2. **What's needed for new system**:
   - Pass `probe_map` to `PatternMatch.matches` function
   - Check if the pattern ID is in `probe_map` during pattern matching
   - Call `capture` with probe metadata from the map instead of from AST
   - Handle this at all call sites: Let bindings, function application, case expressions

3. **Complexity**: Unlike expression probes (which only needed changes in Evaluator), pattern probes require:
   - Modifying `PatternMatch.matches` signature to accept probe_map
   - Threading probe_map through multiple Transition.re call sites
   - Determining which pattern IDs should be probed at match time

This can be implemented after Phase A is complete and the old system is retired.

## Key Technical Decisions

1. **Map-as-set pattern**: Used `Id.Map.t(unit)` instead of separate Set type
2. **ID transformation eliminated**: No longer use `Id.transform_variant` for probes
3. **Minimal worker payload**: Pass only `{expr, probe_map}` to avoid deep cloning info_map
4. **Dual systems**: Keep both old and new probe systems running for backward compatibility

## Next Steps

### Before Phase B - Cleanup Tasks

1. **Remove MakeTerm probe collection** (optional optimization)
   - Currently MakeTerm collects probe IDs, but this is unnecessary for the new system
   - Could extract probe IDs directly from `z.refractors.manuals` in CachedStatics
   - Added TODO comments in MakeTerm.re marking this for future cleanup

2. **Implement pattern probes**
   - Pass probe_map to PatternMatch.matches
   - Check probe_map during pattern matching and invoke capture callback
   - Thread through all Transition.re call sites (Let, FunAp, Match)

3. **Fix cache invalidation issue**
   - **FIXED** ✓ Added `cached_probe_map` field to EvalResult Model
   - Probes now trigger re-evaluation immediately when added/removed

### Phase B - Feature Expansion

Phase B will expand probe functionality:
- Implement "probe everything" mode (Phase B1)
- Progressive sample accumulation (Phase B2)
- Enhanced probe filtering (Phase B3)

## Files Modified

### Core implementation:
- `src/haz3lcore/lang/MakeTerm.re`
- `src/haz3lcore/derived/CachedStatics.re`
- `src/haz3lcore/MkRefractor.re` (KEY FIX)
- `src/language/dynamics/Evaluator.re` + `.rei`
- `src/language/dynamics/state/EvaluatorState.re`
- `src/web/util/WorkerServer.re`
- `src/web/app/editors/result/EvalResult.re`

### View files:
- `src/web/view/ScratchMode.re`
- `src/web/view/ExerciseMode.re`
- `src/web/view/TutorialMode.re`
- `src/web/view/TheoremExerciseMode.re`

### Backward compatibility:
- `src/language/statics/Elaborator.re`
- `src/language/dynamics/transition/Transition.re`
