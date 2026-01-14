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

### 3A: Simplify Refractor Entry Type 📋 PLANNED

**Current state**: `Refractor.Map.t = Id.Map.t(Base.projector)` where `Base.projector = ProjectorCore.t(piece)`:

```reason
// ProjectorCore.t
type t('syntax) = {
  id: Id.t,       // Redundant - it's the map key
  kind: Kind.t,   // Currently always Probe
  syntax: 'syntax,// Dummy parens with randomly-generated inner ID
  model: string,  // Currently always "()" for probes
};
```

**Target state**: Simplified entry type without redundancy:

```reason
// New entry type for manuals/ephemerals maps
type entry = {
  kind: Kind.t,
  model: string,  // Keep for potential future non-unit models
};

module Map = {
  type t = Id.Map.t(entry);
};
```

**Changes needed**:

1. Define `entry` type with just `kind` and `model`
2. Update `manuals` and `ephemerals` to use `Id.Map.t(entry)`
3. When ProjectorCore.t is needed (for projector API), construct on demand:
   - Use `Id.invalid` for any dummy IDs in syntax
   - Create consistent dummy parenthesis (not randomly generated)
4. Update `persist` and `restore_refractors` in PersistentSegment.re

**Result**: Serialized format becomes much cleaner - essentially a list of `(id, kind, model)` tuples where model is typically `()`.

### 3B: Restructure Refractor.t Record 📋 PLANNED

**Current state**:

```reason
type t = {
  manuals: Map.t,                    // Manually added probes
  autos: list(Id.t),                 // Auto-generated probes
  ephemerals: Map.t,                 // Implementation detail of autos
  dyn_cursor: Sample.Cursor.t,       // Sample navigation state
};
```

**Issues**:

- `autos` as list is semantically weird (order doesn't matter, should be set-like)
- `ephemerals` is tightly coupled to `autos` (implementation detail)
- `dyn_cursor` is probe-specific, mixing config with navigation state

**Target structure**:

```reason
type t = {
  manuals: Map.t,    // User-placed refractors
  autos: auto_state, // Auto-generated refractors (grouped)
  sample_cursor: Sample.Cursor.t,  // Renamed from dyn_cursor
};

// Group auto-related state together
type auto_state = {
  ids: Id.Map.t(unit),  // Set-like semantics (no Id.Set available)
  ephemerals: Map.t,    // Implementation detail - projector instances for autos
};
```

**Decision**: Use `Id.Map.t(unit)` instead of `list(Id.t)` for set-like semantics (order doesn't matter, membership queries).

**Naming considerations**:

- `dyn_cursor` → `sample_cursor` (consistent with type rename)
- Keep `Refractor` module name (vs `Probes`) since TypeProj may become a refractor

### 3C: Document and Clean Up Sample.Cursor.t 📋 PLANNED

**Decision**: Keep `Sample.t` and `Sample.Cursor.t` as separate types. They look superficially similar but have different semantics:

- **Sample.t**: A captured sample during evaluation — properties of the sample
- **Cursor.t**: "Coordinates to find a sample" + navigation state — search criteria

**Why NOT combine them**:

1. **Different semantics for `call_stack`**: Cursor's `stack` is for intent preservation — it can be *longer* than the selected sample's `call_stack`. The `index` field controls effective depth. This is navigation, not sample data.

2. **Optionality reflects "no selection"**: Cursor's `time: option(float)` means "do we have a selection?" not "missing data". Sample's `time: float` is always present.

3. **Coordinates vs object**: Cursor stores coordinates to *re-identify* the same sample after re-evaluation. Storing the actual `Sample.t` would give stale data.

4. **Different purposes**: In Sample, `time`/`iter`/`step_range` are properties. In Cursor, they're search criteria.

**Changes to make**:

1. **Rename `stack` → `call_stack`** for consistency (even though semantics differ)
2. **Rename `iter` → `seq`** (sequence number, not iteration count)
3. **Add documentation** explaining:
   - Why Cursor stores coordinates, not a Sample reference
   - The relationship between the two types
   - The intent preservation mechanism (`stack`/`index`)
4. **Fix typo**: "evaluatation" → "evaluation" in Sample.re:116

### 3D: Naming Improvements 📋 PLANNED

| Current | Proposed | Rationale |
|---------|----------|-----------|
| `Cursor.stack` | `Cursor.call_stack` | Consistency with Sample.t (type is same, semantics differ) |
| `Sample.iter`, `Cursor.iter` | `seq` | "iter" misleadingly suggests loop iteration; `seq` = sequence number |
| `dyn_cursor` (field names) | `sample_cursor` | Consistent with type rename |
| `Sample.iter` (module-level ref) | `Sample.seq_counter` | Clarify it's the counter for generating seq values |

### 3E: Document State Locations 📋 PLANNED

Add documentation noting where probe-related state lives:

| State | Location | Scope | Persisted? |
|-------|----------|-------|------------|
| Manual probes | ZipperBase.Refractor.manuals | Per-editor | Yes |
| Auto probe IDs | ZipperBase.Refractor.autos | Per-editor | No |
| Sample cursor | ZipperBase.Refractor.sample_cursor | Per-editor | No |
| Display settings | ProbeProj.Settings.s | Global | No |
| Window offsets | ProbeProj.Settings.offset | Per-probe | No |
| Sample lengths | ProbeProj.SampleLength.lengths | Per-sample | No |

### 3F: Migration Script 📋 PLANNED

Script to update serialized `.ml` files:

**Input format** (current):
```
((id1((id id1)(kind Probe)(syntax(Tile...))(model"()")))
 (id2((id id2)(kind Probe)(syntax(Tile...))(model"()")))...)
```

**Output format** (new):
```
((id1((kind Probe)(model"()")))
 (id2((kind Probe)(model"()")))...)
```

**Files affected** (~9 docs files):
- src/web/init/docs/*.ml (any with `refractors = "..."` containing data)

**Script approach**:
1. Parse old s-expression format
2. Extract id (map key) and kind/model
3. Emit new format without redundant id/syntax fields

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

| Location | Issue | Notes |
|----------|-------|-------|
| `Triggers.re:26` | `^^probe` hardcoded special case | The `expand_projector` function has a special case `when name == "^^probe"` that bypasses the normal projector invocation path. This exists because probes use `MkRefractor.add_single` instead of `invoked_projector`. Should either: (a) make probe creation go through the normal path, or (b) document why the special case is necessary (likely because probes don't fit the "replace syntax" projector model). |

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
8. **Phase 3** 📋 READY: Structural changes (do holistically with migration script)
   - **3A**: Simplify entry type (remove id/syntax redundancy) ← SERIALIZATION CHANGE
   - **3F**: Migration script for .ml files ← RUN IMMEDIATELY AFTER 3A
   - **⏸️ CHECKPOINT**: Pause here to test editor works with new serialization
   - **3B**: Restructure Refractor.t (group autos/ephemerals, rename dyn_cursor)
   - **3C**: Document Sample.Cursor.t (comments explaining coordinates vs sample)
   - **3D**: Naming improvements (call_stack, seq, sample_cursor)
   - **3E**: Document state locations
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

### Session 3: MkRefractor.re Cleanup (2026-01-13)

**Changes made**:

1. Updated `MkRefractor.re` to use `Id.invalid` consistently for dummy syntax:
   - Grout piece uses `Id.invalid`
   - Tile record constructed directly with `Id.invalid` (can't pass id to `Piece.mk_tile`)
   - Added documentation explaining the dependency cycle preventing merge into ProbePerform

2. Investigated moving MkRefractor to ProbePerform - blocked by dependency cycle:
   - `Triggers → ProbePerform` would create cycle through `Printer → ProjectorInfo → CachedSyntax`
   - MkRefractor remains separate with clear documentation

3. Added TODO for `^^probe` special case in Triggers.re to investigate

**Files modified**:
- `src/haz3lcore/MkRefractor.re` (Id.invalid usage, documentation)
- `plans/probe-cleanup.md` (this file)

---

## Phase 3 Implementation Reference

This section provides detailed implementation guidance for a fresh context window.

### Key Files to Modify

| File | Changes |
|------|---------|
| `src/haz3lcore/zipper/ZipperBase.re` | Define new `entry` type, update `Refractor.Map.t`, restructure `Refractor.t` |
| `src/haz3lcore/zipper/PersistentSegment.re` | Update `persist` and `restore_refractors` for new format |
| `src/language/dynamics/Sample.re` | Rename `iter` → `seq`, fix typo, add documentation |
| `src/haz3lcore/zipper/action/SampleCursorPerform.re` | Update field names (`iter` → `seq`, etc.) |
| `src/haz3lcore/projectors/implementations/ProbeProj.re` | Update any `iter` references |
| `src/web/init/docs/*.ml` | Migration script updates serialized `refractors` |

### MkRefractor.re - Dummy Syntax Creation ✅ COMPLETED

**Location**: `src/haz3lcore/MkRefractor.re`

**Purpose**: Creates `Base.projector` records for refractors with dummy syntax. Now uses `Id.invalid` consistently for all generated IDs.

**Why it exists separately**: Cannot be moved to `ProbePerform.re` due to dependency cycle:
```
Triggers → ProbePerform → CachedSyntax → ... → Printer → ProjectorInfo → CachedSyntax
```
`Triggers.re` needs to call refractor creation (in `expand_projector`), but ProbePerform's other functions depend on CachedSyntax which transitively depends on Triggers through the printer/projector chain.

**Changes made**:
- Uses `Id.invalid` for grout piece
- Constructs Tile record directly with `Id.invalid` (since `Piece.mk_tile` doesn't accept an id parameter)
- Added documentation explaining the dependency cycle

**Future simplification**: Since refractors use `skip_inline=true`, the dummy syntax structure doesn't matter for display. Could simplify from grout+parens to just Secondary (empty whitespace).

**Future rename**: Consider renaming `MkRefractor.re` → `Refractor.re`. The "Mk" prefix is unnecessarily verbose. Could also become a home for other refractor-related utilities if needed.

### 3A: Entry Type Change - Detailed

**Current** (`ZipperBase.re:8-13`):
```reason
module Map = {
  type t = Id.Map.t(Base.projector);  // Base.projector = ProjectorCore.t(piece)
  let empty = Id.Map.empty;
};
```

**Target**:
```reason
type entry = {
  kind: ProjectorCore.Kind.t,
  model: string,
};

module Map = {
  type t = Id.Map.t(entry);
  let empty = Id.Map.empty;
};
```

**Where ProjectorCore.t is still needed**: When interfacing with projector API (e.g., in `RefractorView.re`), construct on demand using `MkRefractor.mk` which already handles dummy syntax with `Id.invalid`.

**Note**: `MkRefractor.re` already creates consistent dummy syntax with `Id.invalid`. After the entry type change, it will construct from `entry` instead of storing the full `Base.projector` in maps.

### 3A: Serialization Change - Detailed

**Current serialized format** (in .ml files):
```
((uuid1((id uuid1)(kind Probe)(syntax(Tile((id random-id)...)))(model"()")))
 (uuid2((id uuid2)(kind Probe)(syntax(Tile((id random-id)...)))(model"()"))))
```

**Target serialized format**:
```
((uuid1((kind Probe)(model"()")))
 (uuid2((kind Probe)(model"()"))))
```

**PersistentSegment.re changes**:
- `persist`: Serialize just `{kind, model}` per entry
- `restore_refractors`: Parse new format, construct `entry` records

### 3F: Migration Script

**Input**: Old s-expression with `(id ...)(kind ...)(syntax ...)(model ...)`
**Output**: New s-expression with just `(kind ...)(model ...)`

**Algorithm**:
1. Parse the outer list of `(uuid entry)` pairs
2. For each entry, extract just `kind` and `model` fields
3. Re-emit in new format

**Files to process**: Any `.ml` file in `src/web/init/docs/` where `refractors = "..."` contains actual probe data (not just empty `"()"`).

### 3B: Refractor.t Restructure - Detailed

**Current** (`ZipperBase.re:15-27`):
```reason
type t = {
  manuals: Map.t,
  autos: list(Id.t),
  ephemerals: Map.t,
  dyn_cursor: Language.Sample.Cursor.t,
};
```

**Target**:
```reason
type auto_state = {
  ids: Id.Map.t(unit),
  ephemerals: Map.t,
};

type t = {
  manuals: Map.t,
  autos: auto_state,
  sample_cursor: Language.Sample.Cursor.t,
};

let init = {
  manuals: Id.Map.empty,
  autos: {ids: Id.Map.empty, ephemerals: Id.Map.empty},
  sample_cursor: Language.Sample.Cursor.init,
};
```

**Update sites**: Grep for `refractors.autos` and `refractors.dyn_cursor` to find all access points.

### 3C/3D: Sample.re Changes - Detailed

**Renames**:
- `Sample.t.iter` → `Sample.t.seq`
- `Sample.Cursor.t.iter` → `Sample.Cursor.t.seq`
- `Sample.Cursor.t.stack` → `Sample.Cursor.t.call_stack`
- `Sample.iter` (ref) → `Sample.seq_counter`
- All field accesses site-wide

**Typo fix** (line 116):
```reason
time: float, /* Time of evaluatation */
```
→
```reason
time: float, /* Time of evaluation */
```

**Documentation to add** (in `Sample.Cursor` module):
```reason
/* The sample cursor stores "coordinates" to identify a selected sample,
 * NOT a reference to the sample itself. This is because:
 *
 * 1. Samples are recomputed each evaluation - storing Sample.t would be stale
 * 2. The cursor's call_stack may be LONGER than the selected sample's
 *    (for intent preservation - see doc comment above)
 * 3. Optional fields (time, step_range) represent "no selection" state
 *
 * Fields like `seq`, `time`, `step_range` are search criteria used to
 * re-identify the "same" sample in fresh evaluation results. */
```

### Testing Checkpoint

After completing 3A + 3F:
1. Build the project (`dune build`)
2. Run the editor and verify:
   - Documentation slides with probes load correctly
   - Adding/removing probes works
   - Probe samples display correctly
3. Check that serialized format in localStorage matches new format
