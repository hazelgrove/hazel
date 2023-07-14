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
let debug = mk(Editors.DebugLoad);

let load_editors = (~mode: Editors.mode, ~instructor_mode: bool): Editors.t =>
  switch (mode) {
  | DebugLoad => DebugLoad
  | Scratch =>
    let (idx, slides) = Store.Scratch.load();
    Scratch({idx, slides});
  | Examples =>
    let (current, slides) = Store.Examples.load();
    Examples({current, slides});
  | Exercise =>
    let (idx, specs, state) =
      Store.Exercise.load(
        ~specs=ExerciseSettings.exercises,
        ~instructor_mode,
      );
    Exercise({idx, specs, state});
  };

let save_editors = (editors: Editors.t, ~instructor_mode: bool): unit =>
  switch (editors) {
  | DebugLoad => failwith("no editors in debug load mode")
  | Scratch({idx, slides}) => Store.Scratch.save((idx, slides))
  | Examples({current, slides}) => Store.Examples.save((current, slides))
  | Exercise({idx, specs, state}) =>
    Store.Exercise.save((idx, specs, state), ~instructor_mode)
  };

let load = (init_model: t): t => {
  let settings = Store.Settings.load();
  let langDocMessages = Store.LangDocMessages.load();
  let editors =
    load_editors(
      ~mode=settings.mode,
      ~instructor_mode=settings.instructor_mode,
    );
  let results =
    ModelResults.init(
      settings.dynamics ? Editors.get_spliced_elabs(editors) : [],
    );
  {...init_model, editors, settings, langDocMessages, results};
};

let save = (model: t) => {
  save_editors(
    model.editors,
    ~instructor_mode=model.settings.instructor_mode,
  );
  Store.LangDocMessages.save(model.langDocMessages);
  Store.Settings.save(model.settings);
};

let save_and_return = (model: t) => {
  save(model);
  Ok(model);
};
