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

Convert TypeProj from a projector to a refractor. Tests whether the refractor abstraction generalizes.

**Why convert:**
- Type annotations are additive (like probes), not syntax-replacing
- Removes TypeProj from ProjectorPanel (awkward fit)
- Validates refractor abstraction works for multiple kinds

**Already works (no changes needed):**
- `Refractors.mk_entry(Type)` - creates entry with Type kind
- `Zipper.add_manual(id, Type, z)` - adds to manuals map
- RefractorView renders with `skip_inline=true` automatically
- TypeProj offside view doesn't use `info.syntax`

---

#### Step 1: ProjectorCore.re - Add `is_refractor` predicate

Location: `src/haz3lcore/projectors/ProjectorCore.re:43`

Current:
```reason
let projectors: list(t) = livelit_projectors @ [Fold, Info];
```

Add:
```reason
let refractors: list(t) = [Probe, Type];
let is_refractor = (kind: t) => List.mem(kind, refractors);
```

Type is already not in `projectors` list, so it won't appear in ProjectorPanel.

---

#### Step 2: Triggers.re - Handle `^^type` alongside `^^probe`

Location: `src/haz3lcore/zipper/action/Triggers.re:19-36`

Current (hardcoded `^^probe`):
```reason
| [Tile({...}), Tile({label: [name], _}), ...rest]
    when name == "^^probe" =>
  ...
  |> Zipper.add_manual(Segment.root_id(...), Probe)
```

Change to:
```reason
| [Tile({...}), Tile({label: [name], _}), ...rest]
    when ProjectorCore.Kind.is_refractor_name(name) =>
  let kind = ProjectorCore.Kind.of_refractor_name(name);
  ...
  |> Zipper.add_manual(Segment.root_id(...), kind)
```

Add to ProjectorCore.Kind:
```reason
let is_refractor_name = (s: string) =>
  String.length(s) > 2 && String.sub(s, 0, 2) == "^^"
  && is_refractor(of_name(String.sub(s, 2, String.length(s) - 2)));

let of_refractor_name = (s: string) =>
  of_name(String.sub(s, 2, String.length(s) - 2));
```

---

#### Step 3: Action.re - Add ToggleType action

Location: `src/haz3lcore/zipper/action/Action.re:86`

Extend probe type:
```reason
type probe =
  | ToggleManual
  | ToggleAuto
  | ToggleType  // NEW
  | StepInto(Language.Sample.t, Id.t);
```

---

#### Step 4: ProbePerform.re - Add toggle_type logic

Add simple toggle (no auto, no pins, no step-into):
```reason
let toggle_type = (id: Id.t, info_map: Statics.Map.t, z: Zipper.t): Zipper.t => {
  if (!can_type(id, info_map)) {
    z;
  } else {
    switch (Id.Map.find_opt(id, z.refractors.manuals)) {
    | Some({kind: Type, _}) =>
      Zipper.update_manuals(Id.Map.remove(id), z)
    | _ =>
      Zipper.add_manual(id, Type, z)
    };
  };
};

let can_type = (id: Id.t, info_map: Statics.Map.t): bool =>
  switch (Statics.Map.lookup(id, info_map)) {
  | Some(InfoExp({term: {term: Deferral(_) | Label(_) | TyAlias(_), _}, _})) => false
  | Some(InfoTyp(_) | InfoTPat(_)) => false
  | Some(InfoExp(_) | InfoPat(_)) => true
  | _ => false
  };
```

Update `go` to handle `ToggleType`:
```reason
| ToggleType =>
  switch (Indicated.index(z)) {
  | Some(id) => toggle_type(id, info_map, z)
  | None => z
  }
```

---

#### Step 5: ContextMenu.re - Add "Add type" option

Location: `src/web/app/editors/code/ContextMenu.re`

Add below `manual_probe` (~line 68):
```reason
let type_annotation =
    (~inject, ~can_type: bool, has_type: bool, ci: option(Language.Info.t)) =>
  switch (ci) {
  | Some(InfoExp(_) | InfoPat(_)) when can_type => [
      menu_item(
        ~shortcut=Shortcuts.type_annotation(),
        has_type ? "Remove type" : "Add type",
        inject,
        Probe(ToggleType),
      ),
    ]
  | _ => []
  };
```

Update `probes_items` to compute `has_type` and include it:
```reason
let has_type =
  Id.Map.find_opt(id, z.refractors.manuals)
  |> Option.map(e => e.kind == Type)
  |> Option.value(~default=false);
...
@ type_annotation(~inject, ~can_type=can_probe, has_type, ci)
```

---

#### Step 6: Arms.re - Pass kind for CSS styling

Location: `src/web/app/editors/decoration/Arms.re` in `Refractors` module

Update `refractor_arms` to accept and use kind:
```reason
let refractor_arms =
    (~id, ~kind: ProjectorCore.Kind.t, ~syntax, ...) => {
  ...
  div(~attrs=[Attr.classes(["refractor-arm", ProjectorCore.Kind.name(kind)])], ...)
}
```

Update call sites in `of_zipper` to pass `entry.kind`:
```reason
z.refractors.manuals
|> Id.Map.to_list
|> List.concat_map(((id, entry)) =>
     refractor_arms(~id, ~kind=entry.kind, ~syntax, ...))
```

---

#### Step 7: CSS - Color differentiation

Location: `src/web/www/style/projectors/proj-probe.css`

Add kind-based selectors (probe green = `#73e1a3`, type purple = `#bd92cf`):
```css
.refractor-arm.probe { --bottom-border-clr: #73e1a3; }
.refractor-arm.type { --bottom-border-clr: #bd92cf; }
```

May need to add `.proj-type` styles for offside display in refractor context.

---

#### Step 8: TypeProj.re - Remove dead inline code

Location: `src/haz3lcore/projectors/implementations/TypeProj.re`

Remove (only used for inline, refractors skip):
- `syntax_str` function (lines 85-91)
- `syntax_view` function (line 97)
- Reference in `view.inline` (line 109)

Keep:
- `expected_ty`, `self_ty`, `totalize_ty`, `display_ty`, `display_mode`
- `mode_view`, `typ_view`
- `view.offside`

Simplify `placeholder` to return constant (value doesn't matter for refractors).

---

#### Step 9: Keyboard shortcut

Check `Keyboard.re` for existing Type shortcut (likely Option-T).
Ensure it dispatches `Probe(ToggleType)`.

---

#### Testing checklist

1. `^^type(expr)` creates type refractor on expr
2. Option-T toggles type refractor on indicated term
3. Context menu shows "Add type" / "Remove type"
4. Type refractor appears with purple arm/underline
5. Probe and type can coexist on different terms
6. Type on same term as probe: decide behavior (replace? coexist?)
7. Type display shows expected/self correctly
8. Double-click toggles Expected/Self mode
9. Type refractor persists across edits
10. ProjectorPanel doesn't show Type option

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
