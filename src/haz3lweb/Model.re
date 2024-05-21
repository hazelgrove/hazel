open Sexplib.Std;
open Haz3lcore;

/* MODEL:

      The model consists of three broad categories. Editors is the meat,
      containing the code content and cursor/selection/buffer state for all
      active editors. Settings are user-selectable preferences. Together,
      these two comprise the persistent state of the application which is
      saved to localstore.

      Meta on the other hand consists of everything which is not
      peristant, including transitory ui_state such as whether the mouse
      is held down.

   */

[@deriving (show({with_path: false}), yojson, sexp)]
type timestamp = float;

/* Non-persistent application state */
[@deriving (show({with_path: false}), yojson, sexp)]
type ui_state = {
  font_metrics: FontMetrics.t,
  show_backpack_targets: bool,
  mousedown: bool,
};

let ui_state_init = {
  font_metrics: FontMetrics.init,
  show_backpack_targets: false,
  mousedown: false,
};

type t = {
  editors: Editors.t,
  settings: Settings.t,
  results: ModelResults.t,
  statics: CachedStatics.t,
  explainThisModel: ExplainThisModel.t,
  ui_state,
};

let cutoff = (===);

let mk = (editors, results, statics) => {
  editors,
  settings: Init.startup.settings,
  results,
  statics,
  explainThisModel: ExplainThisModel.init,
  ui_state: ui_state_init,
};

let blank =
  mk(Editors.Scratch(0, []), ModelResults.empty, CachedStatics.empty);

let load_editors =
    (~mode: Settings.mode, ~instructor_mode: bool)
    : (Editors.t, ModelResults.t) =>
  switch (mode) {
  | Scratch =>
    let (idx, slides, results) = Store.Scratch.load();
    (Scratch(idx, slides), results);
  | Documentation =>
    let (name, slides, results) = Store.Documentation.load();
    (Documentation(name, slides), results);
  | Exercises =>
    let (n, specs, exercise) =
      Store.Exercise.load(
        ~specs=ExerciseSettings.exercises,
        ~instructor_mode,
      );
    (Exercises(n, specs, exercise), ModelResults.empty);
  };

let save_editors =
    (editors: Editors.t, results: ModelResults.t, ~instructor_mode: bool)
    : unit =>
  switch (editors) {
  | Scratch(n, slides) => Store.Scratch.save((n, slides, results))
  | Documentation(name, slides) =>
    Store.Documentation.save((name, slides, results))
  | Exercises(n, specs, exercise) =>
    Store.Exercise.save((n, specs, exercise), ~instructor_mode)
  };

let load = (init_model: t): t => {
  let settings = Store.Settings.load();
  let explainThisModel = Store.ExplainThisModel.load();
  let (editors, results) =
    load_editors(
      ~mode=settings.mode,
      ~instructor_mode=settings.instructor_mode,
    );
  let ui_state = init_model.ui_state;
  let statics = Editors.mk_statics(~settings, editors);
  {editors, settings, results, statics, explainThisModel, ui_state};
};

let save = ({editors, settings, explainThisModel, results, _}: t) => {
  save_editors(editors, results, ~instructor_mode=settings.instructor_mode);
  Store.ExplainThisModel.save(explainThisModel);
  Store.Settings.save(settings);
};

let save_and_return = (model: t) => {
  save(model);
  Ok(model);
};

let reset = (model: t): t => {
  /* Reset model to default, including in localstorage,
     but don't otherwise erase localstorage, allowing
     e.g. api keys to persist */
  ignore(Store.Settings.init());
  ignore(Store.ExplainThisModel.init());
  ignore(Store.Scratch.init());
  ignore(Store.Documentation.init());
  ignore(Store.Exercise.init(~instructor_mode=true));
  let new_model = load(blank);
  {
    ...new_model,
    ui_state: {
      ...model.ui_state,
      font_metrics: model.ui_state.font_metrics,
    },
  };
};

let current_statics =
    ({settings, editors, statics, _}: t): CachedStatics.statics =>
  Editors.lookup_statics(~settings, ~statics, editors);
