open Sexplib.Std;
open Util;
open Haz3lcore;

[@deriving (show({with_path: false}), sexp, yojson)]
type live_inspector_action =
  | ToggleOn
  | ToggleCursor
  | ToggleShowFnsInEnv
  | UpdateIds(list(Id.t) => list(Id.t))
  | SetCurrentEnvIdx(int)
  | UpdateCurrentEnv(ProbeMap.dhexp_env => ProbeMap.dhexp_env);

[@deriving (show({with_path: false}), sexp, yojson)]
type settings_action =
  | Captions
  | SecondaryIcons
  | Statics
  | Dynamics
  | Benchmark
  | ContextInspector
  | InstructorMode
  | LiveInspector(live_inspector_action)
  | Mode(Editors.mode);

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Set(settings_action)
  | UpdateDoubleTap(option(float))
  | Mousedown
  | Mouseup
  | InitImportAll([@opaque] Js_of_ocaml.Js.t(Js_of_ocaml.File.file))
  | FinishImportAll(option(string))
  | InitImportScratchpad([@opaque] Js_of_ocaml.Js.t(Js_of_ocaml.File.file))
  | FinishImportScratchpad(option(string))
  | ResetSlide
  | Save
  | ToggleMode
  | SwitchSlide(int)
  | SwitchEditor(int)
  | SetFontMetrics(FontMetrics.t)
  | SetLogoFontMetrics(FontMetrics.t)
  | PerformAction(Action.t)
  | FailedInput(FailedInput.reason) //TODO(andrew): refactor as failure?
  | ResetCurrentEditor
  | Cut
  | Copy
  | Paste(string)
  | Undo
  | Redo
  | SetShowBackpackTargets(bool)
  | MoveToNextHole(Direction.t)
  | UpdateResult(ModelResults.Key.t, ModelResult.current)
  | UpdateLangDocMessages(LangDocMessages.update)
  | DebugAction(DebugAction.t);

module Failure = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | CantUndo
    | CantRedo
    | CantPaste
    | CantReset
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
