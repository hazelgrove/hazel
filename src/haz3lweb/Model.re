open Haz3lcore;
open Sexplib.Std;

[@deriving (show({with_path: false}), yojson)]
type timestamp = float;

[@deriving (show({with_path: false}), sexp, yojson)]
type settings = {
  captions: bool,
  whitespace_icons: bool,
  statics: bool,
  dynamics: bool,
  async_evaluation: bool,
  context_inspector: bool,
  instructor_mode: bool,
  mode: Editors.mode,
};

let settings_init = {
  captions: true,
  whitespace_icons: false,
  statics: true,
  dynamics: true,
  async_evaluation: false,
  context_inspector: false,
  instructor_mode: true,
  mode: Editors.Scratch,
};

type t = {
  editors: Editors.t,
  id_gen: IdGen.state,
  results: ModelResults.t,
  settings,
  font_metrics: FontMetrics.t,
  logo_font_metrics: FontMetrics.t,
  show_backpack_targets: bool,
  double_tap: option(timestamp),
  clipboard: string,
  mousedown: bool,
};

let cutoff = (===);

let mk = editors => {
  id_gen: 1,
  editors,
  results: ModelResults.empty,
  settings: settings_init,
  // TODO: move below to 'io_state'?
  font_metrics: FontMetrics.init,
  logo_font_metrics: FontMetrics.init,
  show_backpack_targets: false,
  double_tap: None,
  clipboard: ",",
  mousedown: false,
};

let blank = mk(Editors.Scratch(0, []));
