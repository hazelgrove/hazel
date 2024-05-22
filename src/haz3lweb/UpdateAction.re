open Sexplib.Std;
open Util;
open Haz3lcore;

[@deriving (show({with_path: false}), sexp, yojson)]
type stepper_action =
  | StepForward(int)
  | StepBackward
  | HideStepper;

[@deriving (show({with_path: false}), sexp, yojson)]
type agent =
  | TyDi;

[@deriving (show({with_path: false}), sexp, yojson)]
type agent_action =
  | Prompt(agent)
  | AcceptSuggestion;

[@deriving (show({with_path: false}), sexp, yojson)]
type benchmark_action =
  | Start
  | Finish;

[@deriving (show({with_path: false}), sexp, yojson)]
type editor_action =
  | Perform(Action.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type cell_action =
  | ToggleStepper
  | Stepper(stepper_action)
  | Editor(editor_action);

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  /* meta */
  | Globals(Globals.Update.t)
  | Reset
  | UpdateExplainThisModel(ExplainThisUpdate.update)
  | ExportPersistentData
  | DebugConsole(string)
  /* editors */
  | ResetCurrentEditor
  | InitImportAll([@opaque] Js_of_ocaml.Js.t(Js_of_ocaml.File.file))
  | FinishImportAll(option(string))
  | MakeActive(Editors.Selection.t)
  | SwitchDocumentationSlide(string) //examplemode only
  // editors: scratchmode only
  | InitImportScratchpad([@opaque] Js_of_ocaml.Js.t(Js_of_ocaml.File.file))
  | FinishImportScratchpad(option(string))
  | SwitchScratchSlide(int)
  /* editor */
  | TAB
  | Save
  | PerformAction(Action.t)
  | ReparseCurrentEditor
  | Cut
  | Copy
  | Undo
  | Redo
  | MoveToNextHole(Direction.t)
  | Benchmark(benchmark_action)
  | Assistant(agent_action)
  | ToggleStepper(ModelResults.Key.t)
  | StepperAction(ModelResults.Key.t, stepper_action)
  | UpdateEvals(WorkerServer.Response.t)
  | UpdateResult(ModelResults.t);

module Failure = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | CantReset
    | CantSuggest
    | FailedToLoad
    | FailedToSwitch
    | FailedToPerform(Action.Failure.t)
    | Exception(string);
};

module Result = {
  include Result;
  type t('success) = Result.t('success, Failure.t);
};

let is_edit: t => bool =
  fun
  | PerformAction(a) => Action.is_edit(a)
  | Globals(Set(s_action)) =>
    switch (s_action) {
    | Mode(_) => true
    | Captions
    | SecondaryIcons
    | Statics
    | Assist
    | Elaborate
    | ExplainThis(_)
    | Dynamics
    | Benchmark
    | ContextInspector
    | InstructorMode
    | Evaluation(_) => false
    }
  | Globals(
      SetMousedown(_) | SetShowBackpackTargets(_) | SetFontMetrics(_) |
      JumpToTile(_),
    ) =>
    false
  | Cut
  | Undo
  | Redo
  | SwitchScratchSlide(_)
  | SwitchDocumentationSlide(_)
  | ToggleStepper(_)
  | StepperAction(_)
  | ReparseCurrentEditor
  | FinishImportAll(_)
  | FinishImportScratchpad(_)
  | ResetCurrentEditor
  | Assistant(AcceptSuggestion)
  | Reset => true
  | UpdateResult(_)
  | UpdateEvals(_)
  | MakeActive(_)
  | ExportPersistentData
  | Save
  | Copy
  | UpdateExplainThisModel(_)
  | DebugConsole(_)
  | InitImportAll(_)
  | InitImportScratchpad(_)
  | MoveToNextHole(_)
  | Benchmark(_)
  | TAB
  | Assistant(Prompt(_)) => false;

let reevaluate_post_update: t => bool =
  fun
  | PerformAction(a) => Action.is_edit(a)
  | Globals(Set(s_action)) =>
    switch (s_action) {
    | Assist
    | Captions
    | SecondaryIcons
    | Statics
    | ContextInspector
    | Benchmark
    | ExplainThis(_)
    | Evaluation(
        ShowCaseClauses | ShowFnBodies | ShowCasts | ShowRecord | ShowFixpoints |
        ShowLookups |
        ShowFilters |
        ShowSettings |
        ShowHiddenSteps,
      ) =>
      false
    | Elaborate
    | Dynamics
    | InstructorMode
    | Mode(_) => true
    }
  | Globals(
      SetMousedown(_) | SetShowBackpackTargets(_) | SetFontMetrics(_) |
      JumpToTile(_),
    ) =>
    false
  | Assistant(AcceptSuggestion) => true
  | Assistant(Prompt(_)) => false
  | MoveToNextHole(_)
  | Save
  | Copy
  | InitImportAll(_)
  | InitImportScratchpad(_)
  | UpdateExplainThisModel(_)
  | ExportPersistentData
  | UpdateResult(_)
  | UpdateEvals(_)
  | MakeActive(_)
  | DebugConsole(_)
  | TAB
  | Benchmark(_) => false
  | StepperAction(_, StepForward(_) | StepBackward | HideStepper)
  | ToggleStepper(_)
  | ReparseCurrentEditor
  | FinishImportAll(_)
  | FinishImportScratchpad(_)
  | ResetCurrentEditor
  | SwitchScratchSlide(_)
  | SwitchDocumentationSlide(_)
  | Reset
  | Cut
  | Undo
  | Redo => true;

let should_scroll_to_caret =
  fun
  | Globals(Set(s_action)) =>
    switch (s_action) {
    | Mode(_) => true
    | Captions
    | SecondaryIcons
    | Statics
    | Assist
    | Elaborate
    | ExplainThis(_)
    | Dynamics
    | Benchmark
    | ContextInspector
    | InstructorMode
    | Evaluation(_) => false
    }
  | Globals(SetFontMetrics(_)) => true
  | Globals(SetMousedown(_) | SetShowBackpackTargets(_) | JumpToTile(_)) =>
    false
  | Assistant(Prompt(_))
  | UpdateResult(_)
  | UpdateEvals(_)
  | ToggleStepper(_)
  | StepperAction(_, StepBackward | StepForward(_) | HideStepper) => false
  | Assistant(AcceptSuggestion) => true
  | FinishImportScratchpad(_)
  | FinishImportAll(_)
  | ResetCurrentEditor
  | MakeActive(_)
  | SwitchScratchSlide(_)
  | SwitchDocumentationSlide(_)
  | ReparseCurrentEditor
  | Reset
  | Copy
  | Cut
  | Undo
  | Redo
  | MoveToNextHole(_)
  | TAB => true
  | PerformAction(a) =>
    switch (a) {
    | Move(_)
    | MoveToNextHole(_)
    | Jump(_)
    | Select(Resize(_) | Term(_) | Smart | Tile(_))
    | Destruct(_)
    | Insert(_)
    | Pick_up
    | Put_down
    | RotateBackpack
    | MoveToBackpackTarget(_)
    | Paste(_) => true
    | Unselect(_)
    | Select(All)
    | Suggest(_)
    | ResetSuggestion => false
    }
  | Save
  | InitImportAll(_)
  | InitImportScratchpad(_)
  | UpdateExplainThisModel(_)
  | ExportPersistentData
  | DebugConsole(_)
  | Benchmark(_) => false;
