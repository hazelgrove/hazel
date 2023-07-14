open Sexplib.Std;
open Haz3lcore;

[@deriving (show({with_path: false}), yojson, sexp)]
type timestamp = float;

type t = {
  editors: Editors.t,
  results: ModelResults.t,
  settings: ModelSettings.t,
  font_metrics: FontMetrics.t,
  logo_font_metrics: FontMetrics.t,
  show_backpack_targets: bool,
  double_tap: option(timestamp),
  mousedown: bool,
  langDocMessages: LangDocMessages.t,
};

let cutoff = (===);

let mk = editors => {
  editors,
  results: ModelResults.empty,
  settings: ModelSettings.default,
  font_metrics: FontMetrics.init,
  logo_font_metrics: FontMetrics.init,
  show_backpack_targets: false,
  double_tap: None,
  mousedown: false,
  langDocMessages: LangDocMessages.default,
};

let default = mk(Editors.default);
let debug = {
  ...default,
  settings: {
    ...default.settings,
    mode: DebugLoad,
  },
};

let load = (init_model: t): t => {
  let settings = Store.Settings.load();
  let editors = Store.Editors.load();
  let langDocMessages = Store.LangDocMessages.load();
  let results =
    ModelResults.init(
      settings.dynamics
        ? Editors.get_spliced_elabs(settings.mode, editors) : [],
    );
  {...init_model, editors, settings, langDocMessages, results};
};

let save = ({settings, editors, langDocMessages, _}: t) => {
  Store.Settings.save(settings);
  Store.Editors.save(editors);
  Store.LangDocMessages.save(langDocMessages);
};

let save_and_return = (model: t) => {
  save(model);
  Ok(model);
};
