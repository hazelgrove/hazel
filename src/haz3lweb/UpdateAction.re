open Sexplib.Std;
open Util;
open Haz3lcore;

[@deriving (show({with_path: false}), sexp, yojson)]
type settings_action =
  | Captions
  | SecondaryIcons
  | Statics
  | Dynamics
  | Benchmark
  | ContextInspector
  | InstructorMode
  | Mode(Editors.mode);

[@deriving (show({with_path: false}), sexp, yojson)]
type filler_options = {
  llm: OpenAI.chat_models,
  prompt_builder: Editor.t => option(string),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type agent =
  | TyDi
  | Weather
  | Oracle
  | Filler(option(filler_options));

[@deriving (show({with_path: false}), sexp, yojson)]
type agent_action =
  | Prompt(agent)
  | SetBuffer(string)
  | AcceptSuggestion;

[@deriving (show({with_path: false}), sexp, yojson)]
type set_meta =
  | DoubleTap(option(float))
  | Mousedown
  | Mouseup
  | ShowBackpackTargets(bool)
  | FontMetrics(FontMetrics.t)
  | MVU(string, DHExp.t)
  | Result(ModelResults.Key.t, ModelResult.current)
  | Auto(Auto.action(Auto.llm_report));

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  /* meta */
  | Set(settings_action)
  | SetMeta(set_meta)
  | UpdateLangDocMessages(LangDocMessages.update)
  | DebugAction(DebugAction.t)
  | Execute(string)
  /* editors */
  | Save
  | ToggleMode
  | ResetCurrentEditor
  | InitImportAll([@opaque] Js_of_ocaml.Js.t(Js_of_ocaml.File.file))
  | FinishImportAll(option(string))
  | SwitchEditor(SchoolExercise.pos) //schoolmode only
  // editors: scratchmode only
  | SwitchSlide(int) // scratchmode only
  | InitImportScratchpad([@opaque] Js_of_ocaml.Js.t(Js_of_ocaml.File.file))
  | FinishImportScratchpad(option(string))
  /* editor */
  | PerformAction(Action.t)
  | ReparseCurrentEditor
  | Cut
  | Copy
  | Paste(string)
  | Undo
  | Redo
  | MoveToNextHole(Direction.t)
  | Agent(agent_action);

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
