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
type meta = {ui_state};

let meta_init = {ui_state: ui_state_init};

type t = {
  editors: Editors.t,
  settings: Settings.t,
  results: ModelResults.t,
  explainThisModel: ExplainThisModel.t,
  meta,
};

let cutoff = (===);

let mk = (editors, results) => {
  editors,
  settings: Init.startup.settings,
  results,
  explainThisModel: ExplainThisModel.init,
  meta: meta_init,
};

let blank = mk(Editors.Scratch(0, []), ModelResults.empty);
let debug = mk(Editors.DebugLoad, ModelResults.empty);

let load_editors =
    (~mode: Settings.mode, ~instructor_mode: bool)
    : (Editors.t, ModelResults.t) =>
  switch (mode) {
  | DebugLoad => (DebugLoad, ModelResults.empty)
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
  | DebugLoad => failwith("no editors in debug load mode")
  | Scratch(n, slides) => Store.Scratch.save((n, slides, results))
  | Documentation(name, slides) =>
    Store.Documentation.save((name, slides, results))
  | Exercises(n, specs, exercise) =>
    Store.Exercise.save((n, specs, exercise), ~instructor_mode)
  };

// let update_elabs = (model: t): t => {
//   let model = {
//     ...model,
//     results:
//       Util.TimeUtil.measure_time(
//         "ModelResults.init", model.settings.benchmark, ()
//         //ModelResults.init performs evaluation on the DHExp value.
//         =>
//           ModelResults.update_elabs(
//             //Editors.get_spliced_elabs generates the DHExp.t of the editor.
//             Editors.get_spliced_elabs(
//               ~settings=model.settings,
//               model.editors,
//             ),
//             model.results,
//           )
//         ),
//   };
//   model;
// };

let load = (init_model: t): t => {
  let settings = Store.Settings.load();
  let explainThisModel = Store.ExplainThisModel.load();
  let (editors, results) =
    load_editors(
      ~mode=settings.mode,
      ~instructor_mode=settings.instructor_mode,
    );
  let meta = init_model.meta;
  let m = {editors, settings, results, explainThisModel, meta};
  //let m = update_elabs(m);
  // {
  //   ...m,
  //   results: ModelResults.run_pending(~settings=m.settings.core, m.results),
  // };
  m;
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
    meta: {
      ui_state: {
        ...model.meta.ui_state,
        font_metrics: model.meta.ui_state.font_metrics,
      },
    },
  };
};
