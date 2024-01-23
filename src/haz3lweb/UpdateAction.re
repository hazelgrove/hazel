open Sexplib.Std;
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
  | ShowFilters;

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
  | Mode(Settings.mode);

[@deriving (show({with_path: false}), sexp, yojson)]
type stepper_action =
  | StepForward(EvaluatorStep.EvalObj.t)
  | StepBackward;

[@deriving (show({with_path: false}), sexp, yojson)]
type agent =
  | TyDi;

[@deriving (show({with_path: false}), sexp, yojson)]
type agent_action =
  | Prompt(agent)
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
  | UpdateLangDocMessages(LangDocMessages.update)
  | ExportPersistentData
  | DebugConsole(string)
  /* editors */
  | ResetCurrentEditor
  | InitImportAll([@opaque] Js_of_ocaml.Js.t(Js_of_ocaml.File.file))
  | FinishImportAll(option(string))
  | SwitchEditor(Exercise.pos) //exercisemode only
  | SwitchExampleSlide(string) //examplemode only
  // editors: scratchmode only
  | InitImportScratchpad([@opaque] Js_of_ocaml.Js.t(Js_of_ocaml.File.file))
  | FinishImportScratchpad(option(string))
  | SwitchScratchSlide(int)
  /* editor */
  | DoTheThing
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
  | UpdateResult(ModelResults.Key.t, option(ModelResult.t));

module Failure = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | CantUndo
    | CantRedo
    | CantPaste
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
  | Cut
  | Undo
  | Redo
  | DoTheThing
  | Set(_)
  | Paste(_)
  | SwitchScratchSlide(_)
  | SwitchExampleSlide(_)
  | ToggleStepper(_)
  | StepperAction(_, _)
  | UpdateResult(_, _) => true
  | PerformAction(a) => Action.is_edit(a)
  | SwitchEditor(_)
  | Reset
  | ExportPersistentData
  | ResetCurrentEditor
  | Save
  | ReparseCurrentEditor
  | Copy
  | SetMeta(_)
  | UpdateLangDocMessages(_)
  | DebugConsole(_)
  | InitImportAll(_)
  | FinishImportAll(_)
  | InitImportScratchpad(_)
  | FinishImportScratchpad(_)
  | MoveToNextHole(_)
  | Benchmark(_)
  | Assistant(_) => false;
