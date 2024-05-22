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

type t = {
  editors: Editors.t,
  results: ModelResults.t,
  statics: CachedStatics.t,
  explainThisModel: ExplainThisModel.t,
  globals: Globals.t,
  active_editor: option(Editors.Selection.t),
};

let cutoff = (===);

let load_editors = (~settings: Settings.t): (Editors.t, ModelResults.t) =>
  switch (settings.mode) {
  | Scratch =>
    let (idx, slides, results) = Store.Scratch.load(~settings=settings.core);
    (Scratch(idx, slides), results);
  | Documentation =>
    let (name, slides, results) =
      Store.Documentation.load(~settings=settings.core);
    (Documentation(name, slides), results);
  | Exercises =>
    let (n, specs, exercise) =
      Store.Exercise.load(
        ~specs=ExerciseSettings.exercises,
        ~instructor_mode=settings.instructor_mode,
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

let load = (): t => {
  let globals = Globals.Model.load();
  let settings = globals.settings;
  let (editors, results) = load_editors(~settings);
  let statics = Editors.mk_statics(~settings, editors);
  let explainThisModel = Store.ExplainThisModel.load();
  {editors, results, statics, globals, explainThisModel, active_editor: None};
};

let save = ({editors, globals, results, explainThisModel, _}: t) => {
  save_editors(
    editors,
    results,
    ~instructor_mode=globals.settings.instructor_mode,
  );
  Globals.Model.save(globals);
  Store.ExplainThisModel.save(explainThisModel);
};

let save_and_return = (model: t) => {
  save(model);
  Ok(model);
};
let reset = (model: t): t => {
  /* Reset model to default, including in localstorage,
     but don't otherwise erase localstorage, allowing
     e.g. api keys to persist */
  let settings = Store.Settings.init().core;
  ignore(Store.ExplainThisModel.init());
  ignore(Store.Scratch.init(~settings));
  ignore(Store.Documentation.init(~settings));
  ignore(Store.Exercise.init(~instructor_mode=true));
  let new_model = load();
  {
    ...new_model,
    globals: {
      ...model.globals,
      font_metrics: model.globals.font_metrics,
    },
  };
};
