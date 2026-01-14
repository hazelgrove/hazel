# Probe System Cleanup Plan

This document tracks the cleanup and reorganization of probe-related code before merging `probemoar` into `dev`.

## Branch Status

- **Branch**: `probemoar`
- **Commits ahead of dev**: ~392
- **Files changed**: ~287 (+22k/-7k lines)

## Current Architecture Overview

### Key Files and Their Roles

| File                     | Current Role                                      | Status/Issues                                                           |
| ------------------------ | ------------------------------------------------- | ----------------------------------------------------------------------- |
| `Sample.re`              | Core sample types, Cursor module, Selection logic | Cursor contains probe-specific fields (`pinned_stack`, `pending_focus`) |
| `DynCursor.re`           | Just `include Sample.Cursor`                      | ✅ **DELETED** - all refs now use `Sample.Cursor`                       |
| `SampleCursorPerform.re` | Zipper wrappers for cursor updates                | ✅ **RENAMED** from `DynCursorPerform.re` (func `perform` → `go`)       |
| `ProbePerform.re`        | Probe operations (add/remove/toggle/step-into)    | ✅ **RENAMED** from `Refractors.re` (func `update` → `go`)              |
| `ProbeProj.re`           | Probe UI/view, stateful settings                  | Contains `Settings` module with global refs                             |
| `ProbeSidebar.re`        | Sidebar panel showing probes/prints               | Name is vague - should be `ProbeSidebar.re` or similar                  |
| `ZipperBase.re`          | `Refractor.t` record in zipper                    | Mixes probe config with cursor state                                    |

### Action Type Structure (Updated)

```reason
// In Action.re (CURRENT STATE after Phase 1D + 2A cleanup)
type sample_cursor =  // was dyn_cursor
  | Capture(Sample.t, option(Id.t))
  | TogglePinCall(call_stack)  // was TogglePin
  | Reset;

type probe =  // was refractor
  | ToggleManual       // was ToggleProbeManual
  | ToggleAuto         // was ToggleProbeREPL
  | StepInto(Sample.t, Id.t);  // was StepIntoSample

type project =
  | SampleCursor(sample_cursor)  // was DynCursor(dyn_cursor)
  | ...

type t =
  | ...
  | Probe(probe)   // was Refractor(refractor)
  // DynCursor removed - was vestigial
```

**Resolved**: Vestigial top-level `Action.t.DynCursor` has been removed.

- All cursor action dispatches now flow through `Project(DynCursor(...))` only
- This matches the actual dispatch path from ProbeProj → ProjectorView.handle

### ProbePerform Module Usage (was Refractors)

Currently exported and used:

- `FocusEffect.execute()` - Main.re
- `go()` - Perform.re (was `update`)
- `add_ids_from_auto_term()` - Editor.re
- `resolve_pending_focus()` - Editor.re
- `ids_from_term()` - ProbeSidebar.re
- `probe_status()` - ContextMenu.re
- `can_probe()` - ContextMenu.re
- `has_probe()` - Arms.re

Note: `Arms.Refractors` in `Arms.re` is a DIFFERENT module (drawing code for probe arms).

---

## Phase 1: DynCursor Consolidation

### 1A: Delete DynCursor.re ✅ COMPLETED

- [x] Replace all `DynCursor.*` with `Sample.Cursor.*`
- [x] Delete `src/language/dynamics/DynCursor.re`

**Files updated**:

- Dynamics.re
- ZipperBase.re
- DynCursorPerform.re
- Refractors.re
- ProbeProj.re
- ProjectorInfo.re
- ProjectorView.re
- ProbeSidebar.re

### 1B: Merge DynCursorPerform.re into Refractors.re ⚠️ BLOCKED → N/A

**Issue**: Dependency cycle prevents this merge.

- Refractors.re depends on CachedSyntax, CachedStatics
- These indirectly depend on ProjectorPerform, Printer, etc.
- ProjectorPerform would need to call Refractors.Cursor.perform
- This creates: Refractors → CachedSyntax → ... → ProjectorPerform → Refractors

**Resolution**: Keep as separate files. The cursor update operations are conceptually related to probes but must stay isolated due to the dependency graph. Both modules were renamed in Phase 2A (see below).

### 1C: Consolidate Action.DynCursor Duplication ✅ COMPLETED

Removed vestigial top-level `Action.t.DynCursor`:

- [x] Removed from `Action.t` type definition
- [x] Removed from `is_edit`, `is_historic`, `prevent_in_read_only_editor`, `should_animate`
- [x] Removed from `Perform.go`
- [x] Removed from `CodeEditable.re` and `CodeSelectable.re`

All DynCursor dispatches now correctly flow through `Project(DynCursor(...))` only.

### 1D: Rename Action Types ✅ COMPLETED

Completed renames:

- `Action.dyn_cursor` → `Action.sample_cursor`
- `Action.refractor` → `Action.probe`
- `Action.project.DynCursor` → `Action.project.SampleCursor`
- `Action.t.Refractor` → `Action.t.Probe`

**Files updated** (~15 files):

- Action.re (type definitions and pattern matches)
- ProjectorBase.re, ProjectorPerform.re, ProjectorView.re
- Perform.re, DynCursorPerform.re (now SampleCursorPerform.re)
- ProbeProj.re, Keyboard.re, CodeEditable.re, CodeSelectable.re
- ContextMenu.re, Arms.re, and others

---

## Phase 2: Module Renaming (Does NOT touch ZipperBase types)

### 2A: Rename Module Files ✅ COMPLETED

**Completed renames**:

1. **`DynCursorPerform.re` → `SampleCursorPerform.re`**

   - Function: `perform` → `go`
   - Old file deleted
   - Follows "Perform" suffix pattern

2. **`Refractors.re` → `ProbePerform.re`**
   - Function: `update` → `go`
   - Git tracked rename
   - Chose `ProbePerform` over `Probes` to avoid conflict with docs `Probes.ml`

Note: `Arms.Refractors` in `Arms.re` is a DIFFERENT module (drawing code for probe arms) - unchanged.

### 2B: Rename ProbeSidebar.re 📋 DEFERRED

`ProbeSidebar.re` is actually a sidebar panel view. Options:

- `ProbeSidebar.re`
- `ProbePanel.re`

**Status**: Confirmed keeping as `ProbeSidebar.re` - name is clear enough

### 2C: Update External References ✅ COMPLETED

Updated all import sites after module renames (~27 files):

- Perform.re, Editor.re, Main.re
- ContextMenu.re, ProbeSidebar.re, Arms.re
- And others

---

## Phase 3: Structural Changes (REQUIRES data migration)

**Warning**: Changes to `ZipperBase.Refractor.t` require migrating serialized zipper data. Should be done holistically after deciding on final structure, with a migration script.

### 3A: Consider Splitting Cursor State

`Sample.Cursor.t` contains probe-specific fields:

```reason
type t = {
  stack: call_stack,           // Generic cursor position
  index: int,                  // Generic depth
  pinned_stack: option(...),   // PROBE-SPECIFIC: pin feature
  pending_focus: option(...),  // PROBE-SPECIFIC: step-into state
  indicated_call: option(Id.t),// Context
  time, iter, step_range,      // Context
};
```

Could potentially split into core cursor + probe-specific extensions.

### 3B: Rename ZipperBase.Refractor.t

Options:

- `ZipperBase.Probes.t`
- Split into `probe_config` and `sample_cursor`

### 3C: Write Migration Script

For any ZipperBase changes, need script to update serialized data files.

---

## Phase 4: TypeProj Conversion (Optional, Low Priority)

If we want TypeProj to become a refractor:

1. Extract `RefractorBase.re` with trimmed API
2. Convert TypeProj to use RefractorBase
3. Update ProjectorView to handle multiple refractor types

**Current assessment**: Probably not worth it unless there's a concrete benefit.

---

## Phase 5: TODO Cleanup

### Investigation Process

For each TODO, gather context by:

1. Read the local code context around the TODO
2. Consider the broader module/file purpose
3. Check git history (`git log -p` on the file) to see when the TODO was added and what else changed at the same time
4. Use this context to determine: fix it, delete it (if resolved/stale), or document why it stays

### TODO Inventory

#### Deferred

| Location       | TODO                      | Notes                                          |
| -------------- | ------------------------- | ---------------------------------------------- |
| `style.css:93` | "fix backpack visibility" | Deeper issue - not probe-related, keep for now |

#### Needs Investigation

(None remaining)

#### Resolved

| Location                | TODO                                      | Resolution                                                                                     |
| ----------------------- | ----------------------------------------- | ---------------------------------------------------------------------------------------------- |
| `ChatLSP.re:384`        | empty refractors                          | Extracted `orphan_to_string` helper with comment explaining why empty/identity is safe         |
| `Test_AutoProbe.re:297` | "probably this should probe body instead" | Kept current behavior - semantically equivalent, simpler to use default. Updated test comment. |

---

## Phase 5B: Remove Probe from ProjectorPanel ✅ COMPLETED

Probes don't fit well in the ProjectorPanel because:

- Other projectors are mutually exclusive on syntax (one projector per term)
- Probes are additive (can put probes on anything, multiple probes)
- The "select a projector" UX doesn't match probe workflow

### Changes Made

1. **Remove Probe from `ProjectorCore.Kind.projectors` list** - Probe no longer appears in dropdown
2. **Update dispatch sites** to use `Probe(ToggleManual)` directly:
   - `Keyboard.re` (Option-V shortcut)
   - `Shortcut.re` (menu shortcut)
3. **Remove special-case intercept from `Perform.re`** - No longer needed (resolved `Perform.re:57` TODO)
4. **Remove Probe case from `ProjectorPanel.keyboard_shortcut_of`** - Won't be shown
5. **Extract CSS z-index to variable** - `--context-menu-z` in variables.css (resolved `editor.css:362` TODO)

### Notes

- `ProjectorCore.Kind.t` still has `Probe` variant (for rendering, etc.)
- Context menu probe options unchanged
- This is partial work toward full probe/projector separation

---

## Phase 5C: Replace ProjectorPanel with Context Menu (Future)

**Status**: 📋 PROPOSED - Not yet decided

Consider removing the ProjectorPanel entirely and moving projector options to the syntax context menu.

**Rationale**:

- Context menu is already used for probe actions
- Projector options are contextual to the indicated term anyway
- Simplifies the inspector UI

**Implementation**:

1. Add projector options to ContextMenu.re (similar to probe options)
2. Remove or repurpose ProjectorPanel.re
3. Update inspector layout

**Decision**: TBD - may do as part of merge or defer to future work.

---

## Phase 5D: Extract RefractorView Module ✅ COMPLETED

The `ProjectorView.Model.mk` function had a confusing fallback path: it tried `Measured.find_pr_opt` first (which works for projectors) and fell back to `TermData.extreme_measures` (for refractors). This was a roundabout way of distinguishing the two since:

- **Projectors** have measurements stored in `measured.projectors`
- **Refractors** derive position from the underlying term's rightmost point

### Changes Made

1. **Created `RefractorView.re`** with:

   - `measurement_of_term`: Computes refractor position from term extremes
   - `mk_data`: Builds refractor data (analogous to `ProjectorView.Model.mk`)
   - `all`: Renders refractors (moved from `ProjectorView.all_refractors`)

2. **Simplified `ProjectorView.Model.mk`**:

   - Removed the fallback path to `TermData.extreme_measures`
   - Now only handles projectors via `Measured.find_pr_opt`
   - Added comment directing to `RefractorView` for refractors

3. **Cleaned up function signatures**:

   - Both `mk` functions now take `~syntax: CachedSyntax.t` instead of 4 separate fields
   - Reduces argument count from 9 to 6
   - Call sites are cleaner: `~syntax=model.editor.syntax`

4. **Updated call sites** in `CodeEditable.re` and `ProbeSidebar.re`

### Files Changed

- **Created**: `src/web/app/common/RefractorView.re`
- **Modified**: `src/web/app/common/ProjectorView.re` (removed `all_refractors`, simplified `Model.mk`)
- **Modified**: `src/web/app/editors/code/CodeEditable.re`
- **Modified**: `src/web/app/probesystem/ProbeSidebar.re`

---

## Phase 5E: Streamline ProjectorView/RefractorView APIs 📋 PROSPECTIVE

**Status**: Not planned for initial merge, but worth doing eventually.

### Current Issues

1. **Two-step API is unnecessarily complex**: Both ProjectorView and RefractorView have `Model.mk` / `mk_data` (compute data) and `all` (render) as separate steps. Most call sites immediately chain them.

2. **ProbeSidebar's pattern is wasteful**: It calls `mk_data` to compute ALL probe data, then looks up individual probes by id via `fancy`. Should compute single-probe data on-demand instead.

3. **Intermediate `projector_data` type is heavyweight**: Pre-computes info, measurement, status, offside_base for every projector/refractor upfront, even though viewport culling might skip most of them.

### Proposed Changes

1. **Collapse `mk_data` + `all` into single `view` function** for both RefractorView and ProjectorView:

   ```reason
   // Instead of:
   let data = RefractorView.mk_data(...);
   let views = RefractorView.all(..., data);

   // Just:
   let views = RefractorView.view(...);
   ```

2. **Add `view_single` for on-demand rendering** (if ProbeSidebar needs it):

   ```reason
   // Compute and render a single probe by id
   let view = RefractorView.view_single(~id, ...);
   ```

3. **Consider lazy computation**: Instead of pre-computing all fields in `projector_data`, compute on-demand during rendering. Viewport culling already filters early, so this may not matter much for performance.

4. **Clean up ProbeSidebar**: The module is acknowledged as hacky. Its `fancy` function pattern (batch compute → lookup) should be replaced with direct single-probe rendering.

### Why Defer

- Current API works fine, just not elegant
- Performance is acceptable (measurement computation is cheap)
- Would touch many files for modest benefit
- Better to stabilize probe system first, then streamline

---

## Phase 6: Prior to Merge Decisions

Items to address before merging `probemoar` into `dev`.

### 6A: Remove Debug/Profiling Code ✅ (Mark when done)

- [ ] `WorkerServer.re:54` - Remove debug logging
- [ ] `ScratchMode.re:382` - Remove profiling code

### 6B: Feature Decisions (UI Toggle Visibility)

Consider hiding incomplete/experimental features from the merge PR while retaining code on a separate branch for future work.

**probe_all Setting Toggle**

- Location: Settings UI (probe settings panel)
- Decision: Comment out the toggle in UI? Keep setting in model but hide toggle?
- Rationale: May want to hide experimental "probe all" feature from initial merge

**Sidebar / ProbeSidebar**

- Location: `ProbeSidebar.re`, sidebar integration
- Decision options:
  - Keep sidebar as-is
  - Comment out sidebar panel from merge
  - Comment out specific sidebar sections (e.g., print statements list)
- Rationale: Sidebar may be incomplete or experimental for initial merge

**Implementation approach**: Create a feature branch from `probemoar` before making these changes, so experimental features are preserved for future development.

---

## Open Questions

1. **Should `Action.refractor` and `Action.dyn_cursor` merge?**

   They're both probe-related but conceptually different:

   - `refractor`: Config changes (add/remove probes, step-into)
   - `dyn_cursor`: Navigation state (capture position, pin, reset)

   Recommendation: Keep separate but rename to `Action.probe` and `Action.sample_cursor`

2. **Where should cursor operations live long-term?**

   Options:

   - In `Sample.re` (current - cursor is about sample navigation)
   - In renamed `Probes.re` (cursor is probe-specific feature)
   - Separate `SampleCursor.re` module

   Recommendation: Keep in `Sample.re` - it's pure logic about sample relationships

3. **Is "refractor" worth keeping as a concept?**

   Currently only probes are refractors. TypeProj could theoretically become one.

   Recommendation: Rename to probes for now, can always generalize later if needed.

---

## Execution Order

1. **Phase 1A** ✅ COMPLETED: DynCursor.re deleted, refs updated to Sample.Cursor
2. **Phase 1B** ⚠️ N/A: DynCursorPerform merge blocked by dependency cycle (kept separate)
3. **Phase 1C** ✅ COMPLETED: Vestigial Action.t.DynCursor removed
4. **Phase 1D** ✅ COMPLETED: Action type renaming (sample_cursor, probe)
5. **Phase 2A** ✅ COMPLETED: Module renaming (SampleCursorPerform, ProbePerform)
6. **Phase 2B** ✅ RESOLVED: Keeping as `ProbeSidebar.re` - name is clear
7. **Phase 2C** ✅ COMPLETED: Updated all external references
8. **Phase 3** 📋 DEFERRED: Requires data migration, do last and holistically
9. **Phase 4** 📋 DEFERRED: TypeProj conversion (optional, low priority)
10. **Phase 5** 📋 IN PROGRESS: TODO cleanup (see inventory above)
11. **Phase 5B** ✅ COMPLETED: Remove Probe from ProjectorPanel
12. **Phase 5C** 📋 PROPOSED: Replace ProjectorPanel with context menu (future)
13. **Phase 5D** ✅ COMPLETED: Extract RefractorView module from ProjectorView
14. **Phase 5E** 📋 PROSPECTIVE: Streamline ProjectorView/RefractorView APIs (see below)
15. **Phase 6** 📋 PENDING: Prior-to-merge decisions (profiling removal, feature toggles)

Each completed phase is part of one cohesive commit.

---

## Summary of Completed Work

### Session 1: Phase 1A-C (Earlier)

**Changes made**:

1. Deleted `src/language/dynamics/DynCursor.re`
2. Updated 8 files to use `Sample.Cursor` instead of `DynCursor`
3. Removed vestigial `Action.t.DynCursor` variant and all pattern matches
4. Documented why DynCursorPerform merge is blocked (dependency cycle)

### Session 2: Phase 1D + 2A+2C (2026-01-13)

**Changes made**:

1. **Action Type Renames** (Phase 1D):

   - `Action.dyn_cursor` → `Action.sample_cursor`
   - `Action.refractor` → `Action.probe`
   - `Action.project.DynCursor` → `Action.project.SampleCursor`
   - `Action.t.Refractor` → `Action.t.Probe`
   - Updated pattern matches in ~15 files

2. **Module File Renames** (Phase 2A):

   - `DynCursorPerform.re` → `SampleCursorPerform.re` (function `perform` → `go`)
   - `Refractors.re` → `ProbePerform.re` (function `update` → `go`)
   - Updated ~27 files referencing these modules

3. **Action Variant Simplifications** (manual cleanup):
   - `ToggleProbeManual` → `ToggleManual` (shortened since already in `Action.probe` type)
   - `ToggleProbeREPL` → `ToggleAuto` (clearer name, avoids "REPL" confusion)
   - `StepIntoSample` → `StepInto` (shortened, context is clear)

**Files modified** (~30 files total):

- Action.re, Perform.re, Editor.re, Main.re
- ProjectorBase.re, ProjectorPerform.re, ProjectorView.re
- ProbeProj.re, Keyboard.re, CodeEditable.re, CodeSelectable.re
- ContextMenu.re, Arms.re, ProbeSidebar.re
- And others

**Files renamed**:

- `src/haz3lcore/Refractors.re` → `src/haz3lcore/ProbePerform.re`

**Files deleted**:

- `src/language/dynamics/DynCursor.re` (Session 1)
- `src/haz3lcore/zipper/action/DynCursorPerform.re` (replaced by SampleCursorPerform.re)

**Files created**:

- `src/haz3lcore/zipper/action/SampleCursorPerform.re`
