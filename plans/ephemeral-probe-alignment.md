# Ephemeral Probe Auto-Alignment

## Problem

When editing code in auto-def mode, new ephemeral probes appear on new
lines of code (e.g., writing a new case branch). But the dynamic cursor
isn't aligned to these new probes, so they show ⊖ (NotAligned) even when
samples exist. The user gets no dynamic feedback while writing.

## Existing Infrastructure

### `pending_probe_cursor`

- **Set by**: `add_manual`, `add_auto` — when a probe is explicitly created
- **Stored in**: `refractors.pending_probe_cursor: option(list(Id.t))`
- **Resolved by**: `resolve_pending_probe_cursor` in `editor_effects`
- **Resolution logic**:
  1. Find first target ID that has samples in dynamics
  2. Call `closest_to_cursor` to pick best sample (preservative: prefers
     related samples, falls back to first)
  3. Call `capture` to set sample cursor (preservative: keeps deeper
     call_stack when new one is a suffix)
  4. Clear pending flag
  5. If no samples yet → keep pending for next cycle
- **Two-pass trigger**: `CellEditor.Update.calculate` checks for pending
  flag and forces a second `Editor.calculate` pass

### `add_ids_from_auto_term`

- Runs every `editor_effects` cycle
- Rebuilds ephemeral map from scratch based on `autos.ids`
- Does NOT set `pending_probe_cursor`
- Does NOT track which ephemerals are new

## The Manual/Auto Case (Already Working)

For manual and auto probes, `pending_probe_cursor` already does roughly
the right thing:
- `closest_to_cursor` finds a compatible sample when possible
- `capture` preserves deeper call stack context
- Only overrides when no compatible sample exists
- This is preservative enough — no changes needed here.

## The Ephemeral Case (The Gap)

When user edits cause `add_ids_from_auto_term` to produce new ephemeral
IDs, nothing triggers alignment. No pending flag, no check, nothing.

## Approach: Use `pending_probe_cursor` for New Ephemerals

### Why `pending_probe_cursor` works here

The deferred mechanism is needed because when a new ephemeral appears,
the evaluator hasn't run yet with the new probe map. Samples for the new
expression don't exist yet in dynamics. So we can't align immediately.

Timeline:
1. User types → code changes → `add_ids_from_auto_term` rebuilds ephemerals
2. Dynamics are stale (pre-edit) → new ephemeral has no samples
3. Set `pending_probe_cursor` with new ephemeral IDs
4. `resolve_pending_probe_cursor` tries → no samples → keeps pending
5. Worker re-evaluates → new dynamics arrive with samples
6. Next cycle → `resolve_pending_probe_cursor` finds samples → aligns

### Concern: Extra second pass on every keystroke

`pending_probe_cursor` triggers a second `Editor.calculate` pass in
`CellEditor`. During typing, each keystroke could produce new ephemerals,
setting pending each time. This means a second pass on every keystroke.

**But**: the second pass is cheap when it can't resolve (no samples yet).
`resolve_pending_probe_cursor` just returns `z` unchanged. The actual
alignment only happens when dynamics catch up (after worker returns).

During rapid typing, the worker is probably still evaluating, so the
second pass is a no-op. Only when typing pauses and the worker returns
does the alignment fire. This is actually desirable behavior — align
when things settle, not mid-keystroke.

### Concern: Churn — different ephemeral IDs each keystroke

Each keystroke changes the AST, which changes which expressions exist,
which changes ephemeral IDs. So the pending list changes each keystroke.

**But**: only the last pending list matters. Resolution happens when
samples arrive, by which point the pending list reflects the latest code
state. Intermediate lists are overwritten and never resolved.

### Alternative: Inline check in `editor_effects`

Instead of pending, do the check inline after `add_ids_from_auto_term`:
for each ephemeral, check if it has samples but cursor isn't aligned.

**Problem**: This can't work for the NEW ephemeral case because dynamics
are stale. The new expression doesn't have samples yet. We'd need to wait
for the evaluator. So we'd end up needing pending anyway.

This approach COULD work for a different scenario: "existing ephemeral has
samples but cursor shifted away from them." But that's not the main problem.

## Implementation Plan

### Step 1: Detect new ephemerals in `add_ids_from_auto_term`

In `add_ids_from_auto_term`, compare old ephemerals with newly computed
ones. If there are IDs in the new map that weren't in the old map, set
`pending_probe_cursor` with those new IDs.

```
let add_ids_from_auto_term = (~syntax, ~info_map, z) => {
  let auto_ids = Id.Map.bindings(z.refractors.autos.ids) |> List.map(fst);
  let ids = List.concat_map(ids_from_term(~syntax, ~info_map), auto_ids);
  let old_ephemerals = z.refractors.autos.ephemerals;
  let new_ephemeral_map = <build map from ids>;
  let z = Zipper.update_ephemerals(_ => new_ephemeral_map, z);

  (* Detect new ephemeral IDs *)
  let new_ids = ids |> List.filter(id => !Id.Map.mem(id, old_ephemerals));
  if (new_ids != []) {
    let sorted = sort_ids_lexically(~syntax, new_ids);
    Zipper.update_refractors(z, r => {...r, pending_probe_cursor: Some(sorted)})
  } else {
    z
  }
};
```

### Step 2: Avoid overwriting existing pending

If `pending_probe_cursor` is already set (from a manual/auto probe add),
don't overwrite it. The existing pending is more intentional.

Or: merge — append new IDs to existing pending list. This way both the
explicit probe and the new ephemerals get considered.

### Step 3: Conditional resolution (optional enhancement)

Currently `resolve_pending_probe_cursor` always calls `capture`. We could
make it conditional: only capture if the current cursor would show ⊖ for
the new probe. But `closest_to_cursor` + `capture` are already
preservative, so this might not be necessary.

## Open Questions

1. **Should we set pending only when there are genuinely NEW IDs, or also
   when existing IDs are "refreshed"?** Probably only new — refreshed IDs
   already had (or didn't have) samples, nothing changed.

2. **What about the case where dynamics arrive but the expression existed
   before (just wasn't probed)?** E.g., user moves cursor to a different
   def in auto-def mode, new ephemerals appear on existing expressions.
   The dynamics map might already have samples from a previous evaluation
   cycle. In that case, alignment could happen immediately in the first
   pass (pending is set, second pass finds samples). This is fine.

3. **Should `pending_probe_cursor` be a `Some(list)` or something richer?**
   E.g., could carry a flag like `only_if_not_aligned: bool` to distinguish
   "user explicitly placed probe, always align" from "ephemeral appeared,
   align only if needed." For now, the preservative behavior of
   `closest_to_cursor` + `capture` makes this unnecessary.

## Status

- [x] Implementation complete (uncommitted)
- [x] Analysis complete
- [x] Approach agreed

## What was implemented

In `add_ids_from_auto_term` (ProbePerform.re): diff old vs new ephemeral
maps. If there are IDs in the new map that weren't in the old map, set
`pending_probe_cursor` with those new IDs (sorted lexically). This causes
`resolve_pending_probe_cursor` to align the sample cursor when evaluation
results arrive for the new probes.
