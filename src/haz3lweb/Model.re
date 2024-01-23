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
      is held down, and cached evaluation results.

   */

[@deriving (show({with_path: false}), yojson, sexp)]
type timestamp = float;

/* Non-persistent UI state */
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

/* Non-persistent application state */
[@deriving (show({with_path: false}), yojson, sexp)]
type meta = {
  ui_state,
  results: ModelResults.t,
};

let meta_init = {ui_state: ui_state_init, results: ModelResults.empty};

type t = {
  editors: Editors.t,
  settings: Settings.t,
  explainThisModel: ExplainThisModel.t,
  meta,
};

let cutoff = (===);

let mk = editors => {
  editors,
  settings: Init.startup.settings,
  explainThisModel: ExplainThisModel.init,
  meta: meta_init,
};

let blank = mk(Editors.Scratch(0, []));
let debug = mk(Editors.DebugLoad);

let load_editors = (~mode: Settings.mode, ~instructor_mode: bool): Editors.t =>
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
      ~settings=settings.core,
      Editors.get_spliced_elabs(~settings, editors),
    );
  let meta = {...init_model.meta, results};
  {editors, settings, explainThisModel, meta};
};

let save = ({editors, settings, explainThisModel, _}: t) => {
  save_editors(editors, ~instructor_mode=settings.instructor_mode);
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
  ignore(Store.Examples.init());
  ignore(Store.Exercise.init(~instructor_mode=true));
  let new_model = load(blank);
  {
    ...new_model,
    meta: {
      ...model.meta,
      ui_state: {
        ...model.meta.ui_state,
        font_metrics: model.meta.ui_state.font_metrics,
      },
    },
  };
};
