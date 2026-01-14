# Probe System Cleanup Plan

Cleanup and reorganization of probe-related code before merging `probemoar` into `dev`.

---

## Completed Work

### Phase 6: Pre-Merge Cleanup ✅

- Removed debug/profiling code from `WorkerServer.re`, `ScratchMode.re`, `CodeEditable.re`
- Commented out `probe_all` toggle in `NutMenu.re` (underlying code retained)
- Simplified `ProbeSidebar.re`:
  - Removed Probearium/Printarium mode toggle
  - Removed `sketch_view` settings panel
  - Kept only "One or Many" toggle, moved inline
  - Truncated Dynamic Cursor to show only trimmed stack (∅ if empty)
  - Removed sample displays from probe list (syntax only)
  - Removed `indicated-call` red outline

### Phase 1-2: Naming & Consolidation ✅

- Deleted `DynCursor.re` - all uses now `Sample.Cursor`
- Renamed `DynCursorPerform.re` → `SampleCursorPerform.re`
- Renamed `Refractors.re` → `ProbePerform.re`
- Renamed action types: `dyn_cursor` → `sample_cursor`, `refractor` → `probe`
- Removed vestigial `Action.t.DynCursor`

### Phase 3: Structural Changes ✅

- Simplified `Refractor.entry` type (removed redundant id/syntax)
- Restructured `Refractor.t` (grouped autos/ephemerals, renamed dyn_cursor → sample_cursor)
- Renamed `iter` → `seq`, `stack` → `call_stack` in Sample types
- Added documentation to `Sample.Cursor.t` and state location tables
- Migrated 8 .ml files to new serialization format

### Phase 5: Module Extraction ✅

- **5B**: Removed Probe from ProjectorPanel (probes use context menu)
- **5D**: Extracted `RefractorView.re` from `ProjectorView.re`
- **5F**: Extracted `Refractors.re` from `ZipperBase.re`

---

## Current Architecture

### Key Files

| File                     | Role                                               |
| ------------------------ | -------------------------------------------------- |
| `Sample.re`              | Core sample types, Cursor module, selection logic  |
| `SampleCursorPerform.re` | Zipper wrappers for cursor updates                 |
| `ProbePerform.re`        | Probe operations (add/remove/toggle/step-into)     |
| `ProbeProj.re`           | Probe UI/view, stateful settings                   |
| `ProbeSidebar.re`        | Sidebar panel showing probes                       |
| `Refractors.re`          | Refractor state types + `mk_entry`, `to_projector` |
| `RefractorView.re`       | Refractor rendering                                |

### State Locations

| State            | Location                   | Persisted? |
| ---------------- | -------------------------- | ---------- |
| Manual probes    | `Refractors.manuals`       | Yes        |
| Auto probe IDs   | `Refractors.autos.ids`     | No         |
| Sample cursor    | `Refractors.sample_cursor` | No         |
| Display settings | `ProbeProj.Settings.s`     | No         |
