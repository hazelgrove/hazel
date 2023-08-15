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
  explainThisModel: ExplainThisModel.t,
};

let cutoff = (===);

let mk = editors => {
  editors,
  results: ModelResults.empty,
  settings: Init.startup.settings,
  // TODO: move below to 'io_state'?
  font_metrics: FontMetrics.init,
  logo_font_metrics: FontMetrics.init,
  show_backpack_targets: false,
  double_tap: None,
  mousedown: false,
  explainThisModel: ExplainThisModel.init,
};

let blank = mk(Editors.Scratch(0, []));
let debug = mk(Editors.DebugLoad);

let load_editors =
    (~mode: ModelSettings.mode, ~instructor_mode: bool): Editors.t =>
  switch (mode) {
  | DebugLoad => DebugLoad
  | Scratch =>
    let (idx, slides) = Store.Scratch.load();
    Scratch(idx, slides);
  | Examples =>
    let (name, slides) = Store.Examples.load();
    Examples(name, slides);
  | Exercise =>
    let (n, specs, exercise) =
      Store.Exercise.load(
        ~specs=ExerciseSettings.exercises,
        ~instructor_mode,
      );
    Exercise(n, specs, exercise);
  };

let save_editors = (editors: Editors.t, ~instructor_mode: bool): unit =>
  switch (editors) {
  | DebugLoad => failwith("no editors in debug load mode")
  | Scratch(n, slides) => Store.Scratch.save((n, slides))
  | Examples(name, slides) => Store.Examples.save((name, slides))
  | Exercise(n, specs, exercise) =>
    Store.Exercise.save((n, specs, exercise), ~instructor_mode)
  };

let load = (init_model: t): t => {
  let settings = Store.Settings.load();
  let explainThisModel = Store.ExplainThisModel.load();
  let editors =
    load_editors(
      ~mode=settings.mode,
      ~instructor_mode=settings.instructor_mode,
    );
  let results =
    ModelResults.init(
      settings.dynamics ? Editors.get_spliced_elabs(editors) : [],
    );
  {...init_model, editors, settings, explainThisModel, results};
};

let save = (model: t) => {
  save_editors(
    model.editors,
    ~instructor_mode=model.settings.instructor_mode,
  );
  Store.ExplainThisModel.save(model.explainThisModel);
  Store.Settings.save(model.settings);
};

let save_and_return = (model: t) => {
  save(model);
  Ok(model);
};
