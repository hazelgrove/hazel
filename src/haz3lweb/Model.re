open Sexplib.Std;
open Haz3lcore;

[@deriving (show({with_path: false}), yojson, sexp)]
type timestamp = float;

[@deriving (show({with_path: false}), yojson, sexp)]
type result = {
  completed_sketch: string,
  errors: string,
  time: string,
};

[@deriving (show({with_path: false}), yojson, sexp)]
type results = VarMap.t_(result);

[@deriving (show({with_path: false}), yojson, sexp)]
type script = {
  current_script: option(string),
  to_run: list((string, list(UpdateAction.t))),
  results: VarMap.t_(result),
};

let script_init: script = {
  current_script: None,
  to_run: [],
  results: VarMap.empty,
};

type t = {
  editors: Editors.t,
  results: ModelResults.t,
  mvu_states: VarMap.t_(DHExp.t),
  settings: ModelSettings.t,
  font_metrics: FontMetrics.t,
  logo_font_metrics: FontMetrics.t,
  show_backpack_targets: bool,
  double_tap: option(timestamp),
  mousedown: bool,
  langDocMessages: LangDocMessages.t,
  script,
};

let cutoff = (===);

let mk = editors => {
  editors,
  results: ModelResults.empty,
  mvu_states: VarMap.empty,
  settings: ModelSettings.init,
  // TODO: move below to 'io_state'?
  font_metrics: FontMetrics.init,
  logo_font_metrics: FontMetrics.init,
  show_backpack_targets: false,
  double_tap: None,
  mousedown: false,
  langDocMessages: LangDocMessages.init,
  script: script_init,
};

let blank = mk(Editors.Scratch(0, []));
let debug = mk(Editors.DebugLoad);
