# Probe System Cleanup Plan

Cleanup and reorganization of probe-related code before merging `probemoar` into `dev`.

---

## Remaining Work

### Pre-Merge (Phase 6)

**6A: Remove Debug/Profiling Code**

- [ ] `WorkerServer.re:54` - Remove debug logging
- [ ] `ScratchMode.re:382` - Remove profiling code

**6B: Feature Decisions**

Decide whether to hide experimental features:

- `probe_all` setting toggle - hide from UI?
- ProbeSidebar - keep, hide, or remove sections?

### TypeProj → TypeRefractor Conversion

Convert TypeProj from a projector to a refractor. This tests whether the refractor abstraction is robust.

**Why convert:**
- Type annotations are additive (like probes), not syntax-replacing (like other projectors)
- Removes TypeProj from ProjectorPanel (already awkward fit)
- Validates refractor abstraction works for multiple kinds

**Implementation steps:**

1. **Triggers.re**: Handle `^^type` alongside `^^probe`
   - Add predicate `ProjectorCore.Kind.is_refractor` for `[Probe, Type]`
   - Use predicate instead of hardcoded `"^^probe"` check

2. **ContextMenu.re**: Add "Add type annotation" option below probe options

3. **ProjectorPanel**: Remove Type from `Kind.projectors` list (like Probe)

4. **Arms.re**: Add class based on kind for purple vs green styling

5. **TypeProj.re**: Remove dead code
   - `syntax_str`, `syntax_view` - only used for inline (refractors skip inline)
   - Keep offside view (mode_view, typ_view)

6. **Extract shared predicate** (optional): `is_typeable_term(id, info_map)`
   - Returns true for expressions/patterns, false for types/labels/deferrals
   - Used by both `ProbePerform.can_probe` and type refractor gating

**No changes needed:**
- `Refractors.mk_entry(Type)` already works
- `Zipper.add_manual(id, Type, z)` already works
- RefractorView renders with `skip_inline=true` automatically
- TypeProj offside view doesn't need `info.syntax`

### Other Deferred / Future

**Phase 5C: Move projector options to context menu** - Would simplify inspector UI

**Phase 5E: Streamline View APIs** - Collapse `mk_data` + `all` into single `view` functions

### Minor TODOs

| Location         | Issue                                                 |
| ---------------- | ----------------------------------------------------- |
| `style.css:93`   | "fix backpack visibility" - not probe-related         |
| `Triggers.re:26` | `^^probe` hardcoded - generalize with is_refractor    |

---

## Completed Work

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

| File                     | Role                                              |
| ------------------------ | ------------------------------------------------- |
| `Sample.re`              | Core sample types, Cursor module, selection logic |
| `SampleCursorPerform.re` | Zipper wrappers for cursor updates                |
| `ProbePerform.re`        | Probe operations (add/remove/toggle/step-into)    |
| `ProbeProj.re`           | Probe UI/view, stateful settings                  |
| `ProbeSidebar.re`        | Sidebar panel showing probes                      |
| `Refractors.re`          | Refractor state types + `mk_entry`, `to_projector` |
| `RefractorView.re`       | Refractor rendering                               |

### State Locations

| State            | Location                   | Persisted? |
| ---------------- | -------------------------- | ---------- |
| Manual probes    | `Refractors.manuals`       | Yes        |
| Auto probe IDs   | `Refractors.autos.ids`     | No         |
| Sample cursor    | `Refractors.sample_cursor` | No         |
| Display settings | `ProbeProj.Settings.s`     | No         |
