open Sexplib.Std;
open Util;
open Haz3lcore;

[@deriving (show({with_path: false}), sexp, yojson)]
type settings_action =
  | Captions
  | SecondaryIcons
  | Statics
  | Elaborate
  | Dynamics
  | Benchmark
  | ContextInspector
  | InstructorMode
  | Mode(Settings.mode);

[@deriving (show({with_path: false}), sexp, yojson)]
type agent =
  | TyDi
  | Weather
  | Oracle
  | Filler(FillerOptions.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type agent_action =
  | Prompt(agent)
  | SetBuffer(string)
  | AcceptSuggestion;

[@deriving (show({with_path: false}), yojson, sexp)]
type focus =
  | Editor
  | MVU;

[@deriving (show({with_path: false}), sexp, yojson)]
type set_meta =
  | Mousedown
  | Mouseup
  | ShowBackpackTargets(bool)
  | FontMetrics(FontMetrics.t)
  | MVU(string, DHExp.t)
  | Focus(focus)
  | Result(ModelResults.Key.t, ModelResult.current)
  | Auto(Auto.action(Auto.llm_report));

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
  | DebugAction(DebugAction.t)
  | StoreKey(string, string)
  | Execute /* Attempt to parse selection as sexp UpdateAction */
  | ExportPersistentData
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
  | Assistant(agent_action);

[@deriving (show({with_path: false}), sexp, yojson)]
type auto_llm = Auto.t(t, Auto.llm_report);

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
  | Redo => true
  | PerformAction(a) => Action.is_edit(a)
  | _ => false;
