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
  | EditingPrompt
  | EditingTestValRep
  | EditingMutTestRep
  | EditingImplGrdRep
  | Evaluation(evaluation_settings_action)
  | ExplainThis(ExplainThisModel.Settings.action)
  | Mode(Settings.mode);

[@deriving (show({with_path: false}), sexp, yojson)]
type stepper_action =
  | StepForward(int)
  | StepBackward;

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
type export_action =
  | ExportScratchSlide
  | ExportPersistentData
  | ExerciseModule
  | Submission
  | TransitionaryExerciseModule
  | GradingExerciseModule;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  /* meta */
  | Reset
  | Set(settings_action)
  | SetMeta(set_meta)
  | UpdateExplainThisModel(ExplainThisUpdate.update)
  | Export(export_action)
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
  | Undo
  | Redo
  | Benchmark(benchmark_action)
  | ToggleStepper(ModelResults.Key.t)
  | StepperAction(ModelResults.Key.t, stepper_action)
  | UpdateResult(ModelResults.t)
  | UpdatePrompt(string)
  | UpdateTestValRep(int, int, int)
  | UpdateMutTestRep(int)
  | UpdateImplGrdRep(int);

module Failure = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | CantUndo
    | CantRedo
    | FailedToSwitch
    | FailedToPerform(Action.Failure.t)
    | InstructorOnly
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
    | EditingPrompt
    | EditingTestValRep
    | EditingMutTestRep
    | EditingImplGrdRep
    | Evaluation(_) => false
    }
  | SetMeta(meta_action) =>
    switch (meta_action) {
    | Mousedown
    | Mouseup
    | ShowBackpackTargets(_)
    | FontMetrics(_) => false
    }
  | Undo
  | Redo
  | SwitchScratchSlide(_)
  | SwitchDocumentationSlide(_)
  | ToggleStepper(_)
  | StepperAction(_)
  | FinishImportAll(_)
  | FinishImportScratchpad(_)
  | ResetCurrentEditor
  | UpdatePrompt(_)
  | UpdateTestValRep(_)
  | UpdateMutTestRep(_)
  | UpdateImplGrdRep(_)
  | Reset
  | TAB => true
  | UpdateResult(_)
  | SwitchEditor(_)
  | Export(_)
  | Save
  | UpdateExplainThisModel(_)
  | DebugConsole(_)
  | InitImportAll(_)
  | InitImportScratchpad(_)
  | Benchmark(_) => false;

let reevaluate_post_update: t => bool =
  fun
  | PerformAction(a) => Action.is_edit(a)
  | Set(s_action) =>
    switch (s_action) {
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
    | Assist
    | Dynamics
    | InstructorMode
    | EditingPrompt
    | EditingTestValRep
    | EditingMutTestRep
    | EditingImplGrdRep
    | Mode(_) => true
    }
  | SetMeta(meta_action) =>
    switch (meta_action) {
    | Mousedown
    | Mouseup
    | ShowBackpackTargets(_)
    | FontMetrics(_) => false
    }
  | Save
  | InitImportAll(_)
  | InitImportScratchpad(_)
  | UpdateExplainThisModel(_)
  | Export(_)
  | UpdateResult(_)
  | UpdatePrompt(_)
  | UpdateTestValRep(_)
  | UpdateMutTestRep(_)
  | UpdateImplGrdRep(_)
  | SwitchEditor(_)
  | DebugConsole(_)
  | Benchmark(_) => false
  | TAB
  | StepperAction(_, StepForward(_) | StepBackward)
  | ToggleStepper(_)
  | FinishImportAll(_)
  | FinishImportScratchpad(_)
  | ResetCurrentEditor
  | SwitchScratchSlide(_)
  | SwitchDocumentationSlide(_)
  | Reset
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
    | EditingPrompt
    | EditingTestValRep
    | EditingMutTestRep
    | EditingImplGrdRep
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
  | UpdatePrompt(_)
  | UpdateTestValRep(_)
  | UpdateMutTestRep(_)
  | UpdateImplGrdRep(_)
  | StepperAction(_, StepBackward | StepForward(_)) => false
  | FinishImportScratchpad(_)
  | FinishImportAll(_)
  | ResetCurrentEditor
  | SwitchEditor(_)
  | SwitchScratchSlide(_)
  | SwitchDocumentationSlide(_)
  | Reset
  | Undo
  | Redo
  | TAB => true
  | PerformAction(a) =>
    switch (a) {
    | Move(_)
    | Jump(_)
    | Select(Resize(_) | Term(_) | Smart(_) | Tile(_))
    | Destruct(_)
    | Insert(_)
    | Pick_up
    | Put_down
    | RotateBackpack
    | MoveToBackpackTarget(_)
    | Buffer(Set(_) | Accept | Clear)
    | Paste(_)
    | Copy
    | Cut
    | Reparse => true
    | Project(_)
    | Unselect(_)
    | Select(All) => false
    }
  | Save
  | InitImportAll(_)
  | InitImportScratchpad(_)
  | UpdateExplainThisModel(_)
  | Export(_)
  | DebugConsole(_)
  | Benchmark(_) => false;
