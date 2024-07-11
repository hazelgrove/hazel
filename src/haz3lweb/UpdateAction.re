open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Util;
open Haz3lcore;

[@deriving (show({with_path: false}), sexp, yojson)]
type evaluation_settings_action =
  | ShowRecord
  | ShowCaseClauses
  | ShowFnBodies
  | ShowCasts
  | ShowFixpoints
  | ShowLookups
  | ShowFilters
  | ShowSettings
  | ShowHiddenSteps;

[@deriving (show({with_path: false}), sexp, yojson)]
type settings_action =
  | Captions
  | SecondaryIcons
  | Statics
  | Dynamics
  | Assist
  | Elaborate
  | Benchmark
  | ContextInspector
  | InstructorMode
  | Evaluation(evaluation_settings_action)
  | ExplainThis(ExplainThisModel.Settings.action)
  | Mode(Settings.mode);

[@deriving (show({with_path: false}), sexp, yojson)]
type stepper_action =
  | StepForward(int)
  | StepBackward;

[@deriving (show({with_path: false}), sexp, yojson)]
type agent_action =
  | AcceptSuggestion;

[@deriving (show({with_path: false}), sexp, yojson)]
type set_meta =
  | Mousedown
  | Mouseup
  | ShowBackpackTargets(bool)
  | FontMetrics(FontMetrics.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type benchmark_action =
  | Start
  | Finish;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  /* meta */
  | Reset
  | Set(settings_action)
  | SetMeta(set_meta)
  | UpdateExplainThisModel(ExplainThisUpdate.update)
  | ExportPersistentData
  | DebugConsole(string)
  /* editors */
  | ResetCurrentEditor
  | InitImportAll([@opaque] Js_of_ocaml.Js.t(Js_of_ocaml.File.file))
  | FinishImportAll(option(string))
  | SwitchEditor(Exercise.pos) //exercisemode only
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
  | Paste(string)
  | Undo
  | Redo
  | MoveToNextHole(Direction.t)
  | Benchmark(benchmark_action)
  | Assistant(agent_action)
  | ToggleStepper(ModelResults.Key.t)
  | StepperAction(ModelResults.Key.t, stepper_action)
  | UpdateResult(ModelResults.t);

module Failure = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | CantUndo
    | CantRedo
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
  | Set(s_action) =>
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
  | SetMeta(meta_action) =>
    switch (meta_action) {
    | Mousedown
    | Mouseup
    | ShowBackpackTargets(_)
    | FontMetrics(_) => false
    }
  | Cut
  | Undo
  | Redo
  | Paste(_)
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
  | SwitchEditor(_)
  | ExportPersistentData
  | Save
  | Copy
  | UpdateExplainThisModel(_)
  | DebugConsole(_)
  | InitImportAll(_)
  | InitImportScratchpad(_)
  | MoveToNextHole(_)
  | Benchmark(_)
  | TAB => false;

let reevaluate_post_update: t => bool =
  fun
  | PerformAction(a) => Action.is_edit(a)
  | Set(s_action) =>
    switch (s_action) {
    | Assist
    | Captions
    | SecondaryIcons
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
    | Statics
    | Dynamics
    | InstructorMode
    | Mode(_) => true
    }
  | SetMeta(meta_action) =>
    switch (meta_action) {
    | Mousedown
    | Mouseup
    | ShowBackpackTargets(_)
    | FontMetrics(_) => false
    }
  | Assistant(AcceptSuggestion) => true
  | MoveToNextHole(_)
  | Save
  | Copy
  | InitImportAll(_)
  | InitImportScratchpad(_)
  | UpdateExplainThisModel(_)
  | ExportPersistentData
  | UpdateResult(_)
  | SwitchEditor(_)
  | DebugConsole(_)
  | TAB
  | Benchmark(_) => false
  | StepperAction(_, StepForward(_) | StepBackward)
  | ToggleStepper(_)
  | ReparseCurrentEditor
  | FinishImportAll(_)
  | FinishImportScratchpad(_)
  | ResetCurrentEditor
  | SwitchScratchSlide(_)
  | SwitchDocumentationSlide(_)
  | Reset
  | Cut
  | Paste(_)
  | Undo
  | Redo => true;

let should_scroll_to_caret =
  fun
  | Set(s_action) =>
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
  | SetMeta(meta_action) =>
    switch (meta_action) {
    | FontMetrics(_) => true
    | Mousedown
    | Mouseup
    | ShowBackpackTargets(_) => false
    }
  | UpdateResult(_)
  | ToggleStepper(_)
  | StepperAction(_, StepBackward | StepForward(_)) => false
  | Assistant(AcceptSuggestion) => true
  | FinishImportScratchpad(_)
  | FinishImportAll(_)
  | ResetCurrentEditor
  | SwitchEditor(_)
  | SwitchScratchSlide(_)
  | SwitchDocumentationSlide(_)
  | ReparseCurrentEditor
  | Reset
  | Copy
  | Paste(_)
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
    | SetBuffer(_)
    | ResetBuffer
    | Paste(_)
    | Reparse => true
    | RecalcStatics
    | Project(_)
    | Unselect(_)
    | Select(All) => false
    }
  | Save
  | InitImportAll(_)
  | InitImportScratchpad(_)
  | UpdateExplainThisModel(_)
  | ExportPersistentData
  | DebugConsole(_)
  | Benchmark(_) => false;

let perform_action = (model: Model.t, a: Action.t): Result.t(Model.t) => {
  let ed = Editors.get_editor(model.editors);
  switch (Haz3lcore.Perform.go(~settings=model.settings.core, a, ed)) {
  | Error(err) => Error(FailedToPerform(err))
  | Ok(ed) => Ok({...model, editors: Editors.put_editor(ed, model.editors)})
  };
};
