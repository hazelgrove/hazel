# Hazel Action Hierarchy

This document captures the complete hierarchy of action/update types in Hazel's MVU architecture, starting from the root `History.Update.t` down through all nested action types.

> **Purpose**: This document is intended to support analysis of:
> - What actions are being logged
> - Which actions might have large payloads (serialization concerns)
> - Which actions contain IDs (affects replayability)

---

## Legend

- 🆔 = Contains `Id.t` directly or indirectly (affects replayability)
- 📦 = Potentially large payload (serialization concern)
- 🔄 = Marked as historic (affects undo/redo)

---

## Root: `History.Update.t`

```
History.Update.t = Page.Update.t
```

---

## Level 1: `Page.Update.t`

```reasonml
type t =
  | Globals(Globals.Update.t)
  | Editors(Editors.Update.t)
  | ExplainThis(ExplainThisUpdate.update)
  | Assistant(AssistantUpdate.t)
  | MakeActive(selection)                    // 🆔 selection = Editors.Selection.t
  | Benchmark(benchmark_action)
  | Start
  | Save
```

### `benchmark_action`
```reasonml
type benchmark_action =
  | Start
  | Finish
```

---

## Level 2: `Globals.Update.t` (alias: `Globals.Action.t`)

```reasonml
type t =
  | SetFontMetrics(FontMetrics.t)
  | Set(Settings.Update.t)
  | JumpToTile(Id.t)                         // 🆔 Direct Id.t
  | InitImportAll(File.file)                 // 📦 Opaque file handle
  | FinishImportAll(option(string))          // 📦 Potentially large string (import data)
  | ExportForInit
  | ActiveEditor(Action.t)                   // 🆔📦 See Action.t below
  | Undo
  | Redo
  | SetMetaDown(bool)
  | UpdateVisibleRows(VisibleRows.t)
```

---

## Level 2: `Editors.Update.t`

```reasonml
type t =
  | SwitchMode(Model.mode)
  | Scratch(ScratchMode.Update.t)
  | Tutorial(TutorialsMode.Update.t)
  | Exercises(ExercisesMode.Update.t)
```

### `Model.mode`
```reasonml
type mode =
  | Scratch
  | Documentation
  | Tutorial
  | Exercises
```

---

## Level 2: `ExplainThisUpdate.update`

```reasonml
type update =
  | SpecificityOpen(bool)
  | ToggleExplanationFeedback(group_id, form_id, feedback_option)
  | ToggleExampleFeedback(group_id, form_id, example_id, feedback_option)
  | UpdateGroupSelection(group_id, form_id)
```

*Note*: `group_id`, `form_id`, `example_id` are strings, not `Id.t`.

---

## Level 2: `AssistantUpdate.t`

```reasonml
type t =
  | SendMessage(send_message, CodeModel.t, Id.t)     // 🆔📦 CodeModel.t is large
  | HandleResponse(handle_response, OpenRouter.reply, Id.t)  // 🆔📦
  | EmployLLMAction(employ_llm_action)               // 🆔
  | ChatAction(chat_action)                          // 🆔
  | InternalError(string, AssistantSettings.mode, Id.t)  // 🆔
  | ExternalAPIAction(external_api_action)
  | InitializeAssistant
```

### `send_message`
```reasonml
type send_message =
  | Tutor(string)
  | Completion(completion)
  | Composition(composition)
```

### `completion`
```reasonml
type completion =
  | Request(Id.t, bool)                      // 🆔
  | Query(string)
  | Loop(string, Id.t, int)                  // 🆔
```

### `composition`
```reasonml
type composition =
  | Request(string)
  | Loop(int, OpenRouter.tool_contents)
```

### `handle_response`
```reasonml
type handle_response =
  | Tutor
  | CompletionErrorRound(CodeModel.t, int, Id.t)  // 🆔📦
  | CompletionQueryResponse
  | CompositionLoopRound(CodeModel.t, int)        // 📦
```

### `employ_llm_action`
```reasonml
type employ_llm_action =
  | RemoveAndSuggest(string, Id.t)           // 🆔
  | Describe(string, AssistantSettings.mode, Id.t)  // 🆔
  | SetLoop(bool)
```

### `chat_action`
```reasonml
type chat_action =
  | NewChat
  | DeleteChat(Id.t)                         // 🆔
  | SwitchChat(Id.t)                         // 🆔
  | CollapseMessage(int)
  | FilterLoadingMessages
```

### `external_api_action`
```reasonml
type external_api_action =
  | SetLLM(string)
  | SetAPIKey(string)
  | SetListOfLLMs(list(OpenRouter.model_info))  // 📦
```

---

## Level 3: `Settings.Update.t`

```reasonml
type t =
  | Captions
  | SecondaryIcons
  | Statics
  | Dynamics
  | ProbeAll
  | Assist
  | Elaborate
  | Benchmark
  | ContextInspector
  | InstructorMode
  | Evaluation(evaluation)
  | Sidebar(SidebarModel.Settings.action)
  | ExplainThis(ExplainThisModel.Settings.action)
  | Assistant(AssistantSettings.action)
  | FlipAnimations
```

### `evaluation`
```reasonml
type evaluation =
  | ShowRecord
  | ForceShowRecord
  | EnableProof
  | ShowCaseClauses
  | ShowFnBodies
  | ShowAscriptionSteps
  | ShowFixpoints
  | ShowLookups
  | ShowFilters
  | ShowSettings
  | ShowHiddenSteps
```

### `SidebarModel.Settings.action`
```reasonml
type action =
  | ToggleShow
  | SwitchPanel(panel)
```

### `AssistantSettings.action`
```reasonml
type action =
  | UpdateChatStatus
  | SwitchMode(mode)
  | ToggleHistory
  | ToggleAPIKeyVisibility
```

---

## Level 3: `ScratchMode.Update.t`

```reasonml
type t =
  | CellAction(CellEditor.Update.t)
  | SwitchSlide(int)
  | ResetCurrent
  | InitImportScratchpad(File.file)          // 📦 Opaque
  | FinishImportScratchpad(option(string))   // 📦
  | Export
  | Encode
  | AddSlide
  | RenameSlide
  | DeleteSlide
```

---

## Level 3: `TutorialsMode.Update.t`

```reasonml
type t =
  | SwitchExercise(int)
  | Tutorial(TutorialMode.Update.t)
  | ExportModule
  | ExportSubmission
  | ExportTransitionary
```

---

## Level 3: `ExercisesMode.Update.t`

```reasonml
type t =
  | SwitchExercise(int)
  | Exercise(ExerciseMode.Update.t)
  | TheoremExercise(TheoremExerciseMode.Update.t)
  | ExportModule
  | ExportSubmission
  | ExportTransitionary
```

---

## Level 4: `CellEditor.Update.t`

```reasonml
type t =
  | MainEditor(CodeEditable.Update.t)
  | ResultAction(EvalResult.Update.t)
```

---

## Level 4: `TutorialMode.Update.t`

```reasonml
type t =
  | Editor(Tutorial.pos, CellEditor.Update.t)
  | ResetEditor(Tutorial.pos)
  | ResetTutorial
  | MoveToNextExercise
  | MoveToPrevExercise
  | Change_report_view
```

*Note*: `Tutorial.pos` is a variant type indicating editor position within exercise.

---

## Level 4: `ExerciseMode.Update.t`

```reasonml
type t =
  | Editor(Exercise.pos, CellEditor.Update.t)
  | ResetEditor(Exercise.pos)
  | ResetExercise
  | Instructor(instructor)
```

### `instructor`
```reasonml
type instructor =
  | EditingTitle
  | EditingPrompt
  | EditingTestValRep
  | EditingMutTestRep
  | EditingImplGrdRep
  | EditingModuleName
  | EditingSyntaxRep
  | UpdateTitle(string)
  | AddBuggyImplementation
  | DeleteBuggyImplementation(int)
  | UpdatePrompt(string)
  | UpdateTestValRep(int, int)
  | UpdateMutTestRep(int, list(string))
  | UpdateImplGrdRep(int, list(string))
  | UpdateSyntaxRep(list(string))
  | UpdateModuleName(string)
```

---

## Level 5: `CodeEditable.Update.t`

```reasonml
type t =
  | Perform(Action.t)                        // 🆔📦 Core editor action
  | TAB
  | ContextMenu(ContextMenu.Model.action)
  | DebugConsole(string)
```

### `ContextMenu.Model.action`
```reasonml
type action =
  | Open
  | Close
  | Toggle
  | Navigate(int)
  | Select
```

---

## Level 5: `EvalResult.Update.t`

```reasonml
type t =
  | ToggleStepper
  | StepperAction(StepperView.Update.t)
  | EvalEditorAction(CodeSelectable.Update.t)
  | UpdateResult(ProgramResult.t(ProgramResult.inner))  // 📦 Evaluation result
  | TheoremsAction(Theorems.Update.t)
```

---

## Level 6: `Action.t` (Core Editor Actions) ⭐

This is the most substantive action type - the core editing operations.

```reasonml
type t =
  | Reparse                                  // 🔄
  | Buffer(buffer)                           // 🔄
  | Paste(paste)                             // 🔄🆔📦 Can contain Segment.t
  | Copy
  | Cut                                      // 🔄
  | Project(project)                         // 🆔 Various ID-containing variants
  | Move(move)                               // 🆔 TileId contains Id.t
  | Select(select)                           // 🆔 Term/Tile rel contains Id.t
  | Unselect(option(Direction.t))
  | Destruct(Direction.t, chunkiness)        // 🔄
  | Insert(string)                           // 🔄
  | Put_down                                 // 🔄
  | Introduce                                // 🔄
  | Probe(probe)                             // 🆔🔄
  | Format                                   // 🔄
  | Dump                                     // 🔄
```

### `move`
```reasonml
type move =
  | Start
  | End
  | Line(Direction.t)
  | Local(Direction.t, chunkiness)
  | Vertical(vertical)
  | Point(Point.t)
  | Goal(goal)                               // 🆔 TileId contains Id.t
```

### `goal`
```reasonml
type goal =
  | Hole(Direction.t)
  | TileId(Id.t)                             // 🆔 Direct
  | BindingSiteOfIndicatedVar
```

### `select`
```reasonml
type select =
  | All
  | Resize(move)                             // 🆔 If move uses TileId
  | Smart(int)
  | Tile(rel)                                // 🆔
  | Term(rel)                                // 🆔
  | ToggleFocus
  | SetFocus(Direction.t)
```

### `rel`
```reasonml
type rel =
  | Current
  | Id(Id.t, Direction.t)                    // 🆔 Direct
```

### `buffer`
```reasonml
type buffer =
  | Set(agent)
  | Clear
  | Accept
```

### `agent`
```reasonml
type agent =
  | TyDi
  | LLM(string)
```

### `paste` 📦🆔
```reasonml
type paste =
  | String(string)
  | Segment(Segment.t)                       // 🆔📦 Segments contain Pieces with IDs
```

### `project` 🆔
```reasonml
type project =
  | SampleCursor(sample_cursor)              // 🆔
  | SetIndicated(chooser)
  | RemoveIndicated
  | SetSyntax(Id.t, Base.segment)            // 🆔📦 Direct Id.t + segment
  | SetModel(Id.t, ProjectorCore.Kind.t, string)  // 🆔 Direct
  | Focus(Id.t, ProjectorCore.Kind.t, option(Direction.t))  // 🆔 Direct
  | Escape(Id.t, Direction.t)                // 🆔 Direct
```

### `sample_cursor` 🆔
```reasonml
type sample_cursor =
  | Capture(Language.Sample.t, option(Id.t))  // 🆔
  | TogglePin(Language.Sample.call_stack)     // 🆔 call_stack likely contains IDs
  | Reset
```

### `probe` 🆔
```reasonml
type probe =
  | ToggleManual
  | ToggleAuto
  | ToggleStatics
  | StepInto(Language.Sample.t, Id.t)        // 🆔 Direct
```

### `chunkiness`
```reasonml
type chunkiness =
  | ByChar
  | ByToken
```

### `vertical`
```reasonml
type vertical =
  | Up
  | Down
```

---

## Level 6: `StepperView.Update.t` (alias: `StepperBase.step_action`)

See `StepperBase` for the actual action type definition.

---

## Level 6: `CodeSelectable.Update.t`

Similar to `CodeEditable.Update.t` but for read-only selection contexts.

---

## Types That Contain IDs (Summary)

### Direct `Id.t` References

| Action Type | Variant | Field |
|-------------|---------|-------|
| `Globals.Update.t` | `JumpToTile` | `Id.t` |
| `AssistantUpdate.t` | `SendMessage` | 3rd param |
| `AssistantUpdate.t` | `HandleResponse` | 3rd param |
| `AssistantUpdate.t` | `InternalError` | 3rd param |
| `completion` | `Request` | `Id.t` |
| `completion` | `Loop` | 2nd param |
| `employ_llm_action` | `RemoveAndSuggest` | 2nd param |
| `employ_llm_action` | `Describe` | 3rd param |
| `chat_action` | `DeleteChat` | `Id.t` |
| `chat_action` | `SwitchChat` | `Id.t` |
| `Action.goal` | `TileId` | `Id.t` |
| `Action.rel` | `Id` | 1st param |
| `Action.project` | `SetSyntax` | 1st param |
| `Action.project` | `SetModel` | 1st param |
| `Action.project` | `Focus` | 1st param |
| `Action.project` | `Escape` | 1st param |
| `Action.sample_cursor` | `Capture` | 2nd param |
| `Action.probe` | `StepInto` | 2nd param |

### Types That Transitively Contain IDs

| Type | Contains IDs Because |
|------|---------------------|
| `Segment.t` | Segments are `list(Piece.t)`, pieces contain tiles, tiles have IDs |
| `Base.segment` | Alias for `Segment.t` |
| `paste.Segment` | Contains `Segment.t` |
| `project.SetSyntax` | Contains `Base.segment` |
| `CodeModel.t` / `CodeEditable.Model.t` | Contains editor state with `Zipper.t` which has segments |
| `Language.Sample.t` | Sample structures contain probe data with IDs |
| `Language.Sample.call_stack` | Call stack contains IDs for evaluation tracking |

---

## Potentially Large Payloads (Summary)

| Action Type | Variant | Reason |
|-------------|---------|--------|
| `Globals.Update.t` | `FinishImportAll` | Full export data as string |
| `ScratchMode.Update.t` | `FinishImportScratchpad` | Full scratchpad data |
| `AssistantUpdate.t` | `SendMessage` | `CodeModel.t` is full editor state |
| `AssistantUpdate.t` | `HandleResponse` | Can contain `CodeModel.t` |
| `external_api_action` | `SetListOfLLMs` | List of all available models |
| `EvalResult.Update.t` | `UpdateResult` | Full evaluation result |
| `Action.paste` | `Segment` | Can be arbitrarily large |
| `Action.project` | `SetSyntax` | Contains segment |

---

## Undo/Redo Behavior Notes

Actions marked with `is_historic = true` will:
1. Push the previous state onto the undo stack
2. Clear the redo stack

Key non-historic actions (skip in undo history):
- `Move`, `Select`, `Unselect`, `Copy`
- `Project.Focus`, `Project.Escape`, `Project.SampleCursor`
- `SetModel` (projector model changes)

---

## Replayability Concerns

For action replay to work, IDs must be stable. Current issues:

1. **Fresh IDs on edit**: Many operations generate new IDs (e.g., `Insert`, paste operations)
2. **ID references**: Actions like `JumpToTile`, `TileId`, `Focus` reference IDs that may not exist in replayed state
3. **Segment payloads**: `Paste(Segment(...))` carries IDs within the segment structure

### Recommendations for Replay

1. Consider logging `Paste(String(...))` instead of `Paste(Segment(...))` for replayability
2. Actions with direct ID references (`JumpToTile`, `Focus`, etc.) need ID remapping logic
3. Consider a deterministic ID generation scheme for replay scenarios
