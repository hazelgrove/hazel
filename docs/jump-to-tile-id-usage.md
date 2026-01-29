# Jump-to-Tile-ID Usage in Hazel

This document analyzes how "jump to tile by ID" functionality is used throughout Hazel, including direct and indirect uses.

---

## Summary

Jump-to-tile-ID is **heavily load-bearing** in Hazel. It powers:

1. **Test result clicking** - Click on test bar segments to jump to test definitions
2. **Grading report navigation** - Click on test/implementation reports to jump to relevant code
3. **Context inspector navigation** - Click on variables in scope to jump to their definitions
4. **ExplainThis documentation** - Click on highlighted terms to jump to them in code
5. **Go-to-definition (F12)** - Jump from variable usage to binding site
6. **Probe sidebar navigation** - Click on probe entries to jump to their locations
7. **LLM assistant navigation** - AI composition tool uses it to navigate to definitions
8. **Result-to-source linking** - Click on evaluation results to jump back to source

---

## Core Implementation

### Low-Level: `Move.jump_to_id_indicated`

```reasonml
// src/haz3lcore/zipper/action/Move.re
let jump_to_id_indicated = (z: t, id: Id.t): option(t) => {
  let* z_l = jump_to_side_of_id(Left, z, id);
  let* indicated_id = Indicated.index(z_l);
  if (id == indicated_id) {
    Some(z_l);
  } else {
    let* z_r = jump_to_side_of_id(Right, z, id);
    let* indicated_id = Indicated.index(z_r);
    id == indicated_id ? Some(z_r) : None;
  };
};
```

This is the core function that actually moves the zipper to a tile by ID. It:
1. Moves to the left edge of the tile with the given ID
2. Checks if that tile is now "indicated" (under cursor)
3. If not, tries the right edge
4. Returns `None` if the tile cannot be found

### Action Level: `Action.goal`

```reasonml
// src/haz3lcore/zipper/action/Action.re
type goal =
  | Hole(Direction.t)
  | TileId(Id.t)                    // Direct tile navigation
  | BindingSiteOfIndicatedVar       // Indirect: uses statics to find binding
```

### Move Dispatch

```reasonml
// src/haz3lcore/zipper/action/Move.re
| Goal(TileId(id)) => jump_to_id_indicated(z, id)
| Goal(BindingSiteOfIndicatedVar) =>
    let* ci = Indicated.ci_of(z, statics);
    let* binding_id = Language.Info.get_binding_site(ci);
    jump_to_id_indicated(z, binding_id);  // Uses TileId internally!
```

Note: `BindingSiteOfIndicatedVar` **also uses** `jump_to_id_indicated` under the hood.

---

## Entry Points

### 1. `Globals.Update.JumpToTile(Id.t)`

A top-level action that wraps the jump functionality for use from global UI.

**Used by:**
- ExplainThis documentation (clicking on highlighted terms)
- Context Inspector (clicking on variables in scope)

```reasonml
// src/web/app/globals/Globals.re
type t =
  | JumpToTile(Haz3lcore.Id.t)
  // ...
```

**Implementation in Page.re:**
```reasonml
| JumpToTile(id) =>
    let jump = Editors.Selection.jump_to_tile(~settings, id, model.editors);
    // Performs Move(Goal(TileId(id))) on appropriate editor
```

### 2. `Action.Move(Goal(TileId(id)))`

The core editor action for jumping to a tile.

### 3. `Action.Move(Goal(BindingSiteOfIndicatedVar))`

Go-to-definition: uses statics to find the binding site, then jumps to it.

---

## UI Features Using Jump-to-Tile

### 1. Test Bar Segments (Click to Jump)

**Location:** `src/web/view/TestView.re`

When you click on a colored segment in the test bar (showing pass/fail status), it jumps to that test in the code.

```reasonml
let test_bar_segment = (~inject_jump, (id, reports)) => {
  let status = reports |> TestMap.joint_status |> TestStatus.to_string;
  div(
    ~attrs=[clss(["segment", status]), Attr.on_click(_ => inject_jump(id))],
    [],
  );
};
```

**Used in:**
- `EvalResult.re` (line 637): Result panel test summary
- `Grading.re` (line 249): Test validation report
- `TutorialGrading.re` (line 124): Tutorial grading

### 2. Grading Reports (Click on Test Numbers)

**Location:** `src/web/exercises/Grading.re` (lines 1000-1075)

In exercise mode, clicking on test numbers/reports jumps to those tests.

```reasonml
div(
  ~attrs=[
    Attr.classes(["test-report"]),
    Attr.on_click(_ => signal_jump(id)),
  ],
  // ...
)
```

**Features using this:**
- Test Validation Report (`signal_jump` at line 989)
- Implementation Grading Report (`signal_jump` at line 1121)
- Mutation Testing Report

### 3. Tutorial Grading Reports

**Location:** `src/web/exercises/TutorialGrading.re`

Similar to exercise grading - clicking on test items jumps to them.

```reasonml
let individual_report = (i, ~signal_jump, ~hint: string, ~status, (id, _)) =>
  div(
    ~attrs=[
      Attr.classes(["test-report"]),
      Attr.on_click(_ => signal_jump(id)),
    ],
    // ...
  );
```

### 4. Context Inspector (Click on Variables)

**Location:** `src/web/view/ContextInspector.re`

Clicking on a variable in the context inspector jumps to its definition.

```reasonml
let jump_to = entry => Globals.Update.JumpToTile(Language.Ctx.get_id(entry));

let context_entry_view = (~globals, entry: Language.Ctx.entry): Node.t => {
  // ...
  div(
    ~attrs=[
      Attr.on_click(_ => globals.inject_global(jump_to(entry))),
      clss(["context-entry", "code"]),
    ],
    // ...
  )
};
```

### 5. ExplainThis Documentation (Click on Highlighted Terms)

**Location:** `src/web/app/explainthis/ExplainThis.re`

When the ExplainThis panel shows documentation with highlighted syntax, clicking those highlights jumps to the corresponding code.

```reasonml
let attrs = [
  // ...
  Attr.on_click(_ => globals.inject_global(JumpToTile(id))),
];
```

### 6. Go-to-Definition (F12 / Context Menu)

**Locations:**
- `src/web/Keyboard.re` (F12 key)
- `src/web/app/editors/code/ContextMenu.re`
- `src/web/app/editors/code/CodeEditable.re`

F12 or "Go to Definition" in the context menu jumps to the binding site of the variable under cursor.

```reasonml
// Keyboard.re
| (Up, "F12") => now(Move(Goal(BindingSiteOfIndicatedVar)))

// ContextMenu.re
{
  name: "Go to Definition",
  action: Move(Goal(BindingSiteOfIndicatedVar)),
  predicate: ...
}

// CodeEditable.re (Cmd/Ctrl+Click)
| {button: Left, sys: PC, ctrl: Down, _}
| {button: Left, sys: Mac, meta: Down, _} =>
    Effect.Many([
      signal(MakeActive),
      inject(Perform(Move(Point(loc(mouse))))),
      inject(Perform(Move(Goal(BindingSiteOfIndicatedVar)))),
    ])
```

### 7. Probe Sidebar (Click on Probes)

**Location:** `src/web/app/probesystem/ProbeSidebar.re`

Clicking on entries in the probe sidebar jumps to those probes in the code.

```reasonml
let jump_to = (~globals: Globals.t, id: Id.t, _) =>
  globals.inject_global(ActiveEditor(Move(Goal(TileId(id)))));

let basic = (~globals: Globals.t, id: Id.t) =>
  div(
    ~attrs=[
      Attr.create("style", "cursor: pointer;"),
      Attr.on_pointerdown(jump_to(~globals, id)),
    ],
    [text(Id.str3(id))],
  );
```

### 8. LLM Assistant / ChatLSP (AI Navigation)

**Location:** `src/web/app/helpful-assistant/ChatLSP.re`

The LLM composition tool uses jump-to-tile to navigate when performing code actions.

```reasonml
let actions =
  switch (matching_id) {
  | Some(id) => [
      Action.Move(Goal(TileId(id))),
      Action.Move(Local(Left, ByToken)),
      // ...
    ]
  | None => [Action.Select(Term(Id(Id.invalid, Direction.Left)))]
  };
```

### 9. Result Panel → Source Navigation

**Location:** `src/web/app/editors/cell/CellEditor.re`

When viewing evaluation results, clicking can jump back to the source code.

```reasonml
| JumpTo(id) =>
    Effect.Many([
      signal(MakeActive(MainEditor)),
      inject(MainEditor(Perform(Move(Goal(TileId(id)))))),
    ])
```

### 10. Selection Jump (Editor Selection)

**Location:** `src/web/app/editors/code/CodeEditable.re`

The `jump_to_tile` function in Selection module allows programmatic navigation.

```reasonml
let jump_to_tile = (id: Id.t, model: Model.t): option(Update.t) => {
  switch (TermData.root_tile(id, model.editor.syntax.term_data)) {
  | Some(_) => Some(Perform(Move(Goal(TileId(id)))))
  | None => None
  };
};
```

---

## Call Graph Summary

```
User Actions
    │
    ├── Click Test Bar Segment
    │   └── inject_jump(id)
    │       └── Editors → Scratch/Exercise/Tutorial
    │           └── CellEditor.Update.MainEditor(Perform(Move(Goal(TileId(id)))))
    │
    ├── Click Grading Report Item
    │   └── signal_jump(id)
    │       └── Exercise/Tutorial Mode
    │           └── Editor(pos, MainEditor(Perform(Move(Goal(TileId(id))))))
    │
    ├── Click Context Inspector Entry
    │   └── globals.inject_global(JumpToTile(id))
    │       └── Page.Update → Editors.Selection.jump_to_tile
    │           └── Move(Goal(TileId(id)))
    │
    ├── Click ExplainThis Highlight
    │   └── globals.inject_global(JumpToTile(id))
    │       └── (same as above)
    │
    ├── F12 / Go to Definition
    │   └── Move(Goal(BindingSiteOfIndicatedVar))
    │       └── (internally) jump_to_id_indicated(z, binding_id)
    │
    ├── Cmd/Ctrl+Click Variable
    │   └── Move(Goal(BindingSiteOfIndicatedVar))
    │       └── (same as above)
    │
    ├── Click Probe Sidebar Entry
    │   └── ActiveEditor(Move(Goal(TileId(id))))
    │       └── Move(Goal(TileId(id)))
    │
    └── Click Result Panel Item
        └── JumpTo(id)
            └── MainEditor(Perform(Move(Goal(TileId(id)))))

All paths eventually reach:
    Move.jump_to_id_indicated(z, id)
```

---

## Implications for Logging/Replay

### Why This Is Problematic for Replay

1. **ID Instability**: If a user edits code, tile IDs change. Replaying a `Move(Goal(TileId(old_id)))` action will fail because the old ID no longer exists.

2. **Frequent Use**: This functionality is used constantly during normal interaction:
   - Every F12 press
   - Every Cmd+click on a variable
   - Every test bar click
   - Every grading report click
   
3. **UI-Initiated**: Most of these are view-layer interactions (clicks), not core editing - but they still appear in the action stream.

### Potential Approaches

1. **Log structural location instead of ID**: Instead of `TileId(uuid)`, log something like "the 3rd test expression" or "the binding site of variable 'x'".

2. **Don't log navigation-only actions**: Since `Move(Goal(TileId(_)))` doesn't modify the document, you could exclude it from logged history for replay purposes.

3. **ID remapping during replay**: Build a mapping from old IDs to new IDs based on structural correspondence.

4. **BindingSiteOfIndicatedVar is more stable**: This action uses semantic information (variable name + scope) rather than raw IDs, so it may replay better if cursor position can be established.

---

## Statistics

| Feature | Location | Occurrences |
|---------|----------|-------------|
| `TileId(` pattern | All of src | 15 |
| `Goal(TileId` pattern | All of src | 14 |
| `JumpToTile` pattern | src/web | 5 |
| `inject_jump` pattern | src/web | 11 |
| `signal_jump` pattern | src/web | 23 |
| `BindingSiteOfIndicatedVar` | All of src | 6 |

**Total UI features using jump-to-tile**: ~10 distinct features
