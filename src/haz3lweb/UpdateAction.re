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

[@deriving (show({with_path: false}), yojson, sexp)]
type script_result = {
  time_start: option(float),
  time_end: option(float),
  completed_sketch: option(string),
  static_errors: option(list((Id.t, Info.error))),
  syntax_errors: option(list(string)),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type script_action =
  | StartRun(unit)
  | StartTest(unit)
  | UpdateResult(string, script_result => script_result)
  | LogTest(unit)
  | EndTest(unit);

let empty_script_result = {
  time_start: None,
  time_end: None,
  completed_sketch: None,
  static_errors: None,
  syntax_errors: None,
};

let initialize_results = _r => {
  time_start: Some(Sys.time()),
  time_end: None,
  completed_sketch: None,
  static_errors: None,
  syntax_errors: None,
};

let finalize_results =
    (syntax_errors, static_errors, completed_sketch, result: script_result) => {
  ...result,
  syntax_errors: Some(syntax_errors),
  static_errors: Some(static_errors),
  completed_sketch: Some(completed_sketch),
  time_end: Some(Sys.time()),
};

/*
 To start a run:
 1. reset model to init
 (assume for now list stdlib not required; otherwise will need to populate more slides)
 2. go to sketch slide (can be first slide for now)
 3. select all and delete


 start test:
 -1: add a new entry to test_results
 0. go to sketch slide (can be first slide for now)
 1. select all and delete
 2. paste in sketch (or insert segment as zipper manually to save time)
 3. move caret to immediately after first ??
 4. schedule_action(PerformAction(Select(Term(Current))));
     schedule_action(Agent(Prompt(Filler)));
 NOTE: will want to parameterize Filler action
 5. after 5, other actions will get scheduled; we
 need the last to trigger End test

 end test:
 1. Accept Completion
 2. collate errors
 3. add errors to test_results



 */

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Set(settings_action)
  | Execute(string)
  | UpdateDoubleTap(option(float))
  | Mousedown
  | Mouseup
  | InitImportAll([@opaque] Js_of_ocaml.Js.t(Js_of_ocaml.File.file))
  | FinishImportAll(option(string))
  | InitImportScratchpad([@opaque] Js_of_ocaml.Js.t(Js_of_ocaml.File.file))
  | FinishImportScratchpad(option(string))
  | ResetSlide
  | Save
  | StoreKey(LocalStorage.Generic.t, string)
  | MVUSet(string, DHExp.t)
  | ToggleMode
  | SwitchSlide(int)
  | SwitchEditor(SchoolExercise.pos)
  | SetFontMetrics(FontMetrics.t)
  | SetLogoFontMetrics(FontMetrics.t)
  | PerformAction(Action.t)
  | FailedInput(FailedInput.reason) //TODO(andrew): refactor as failure?
  | ResetCurrentEditor
  | Cut
  | Copy
  | Paste(string)
  | Agent(agent_action)
  | Undo
  | Redo
  | SetShowBackpackTargets(bool)
  | MoveToNextHole(Direction.t)
  | UpdateResult(ModelResults.Key.t, ModelResult.current)
  | UpdateLangDocMessages(LangDocMessages.update)
  | DebugAction(DebugAction.t)
  | Script(script_action);

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
    | UnrecognizedInput(FailedInput.reason)
    | FailedToPerform(Action.Failure.t)
    | Exception(string);
};

module Result = {
  include Result;
  type t('success) = Result.t('success, Failure.t);
};
