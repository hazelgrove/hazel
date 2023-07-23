open Sexplib.Std;
open Haz3lcore;

/* MODEL.re

   The model consists of three broad categories. EDITORS is the 'meat',
   containing the code content and cursor/selection/buffer state for all
   active editors. SETTINGS are user-selectable preferences. Together,
   these two comprise the 'persistant' state of the application which is
   saved to localstore.

   META on the other hand consists of everything which is not
   peristant, including transitory UI_STATE such as whether the mouse
   is held down, and cached evaluation RESULTS.

    */

[@deriving (show({with_path: false}), yojson, sexp)]
type timestamp = float;

/* Non-persistant UI state */
[@deriving (show({with_path: false}), yojson, sexp)]
type ui_state = {
  font_metrics: FontMetrics.t,
  show_backpack_targets: bool,
  double_tap: option(timestamp),
  mousedown: bool,
};

let ui_state_init = {
  font_metrics: FontMetrics.init,
  show_backpack_targets: false,
  double_tap: None,
  mousedown: false,
};

/* Non-persistant application state */
[@deriving (show({with_path: false}), yojson, sexp)]
type meta = {
  ui_state,
  results: ModelResults.t,
  auto: UpdateAction.auto_llm,
  mvu_states: VarMap.t_(DHExp.t),
};

let meta_init = {
  ui_state: ui_state_init,
  results: ModelResults.empty,
  mvu_states: VarMap.empty,
  auto: Auto.init,
};

type t = {
  editors: Editors.t,
  settings: ModelSettings.t,
  langDocMessages: LangDocMessages.t,
  meta,
};

let cutoff = (===);

let mk = editors => {
  editors,
  settings: ModelSettings.init,
  langDocMessages: LangDocMessages.init,
  meta: meta_init,
};

let blank = mk(Editors.Scratch(0, []));
let debug = mk(Editors.DebugLoad);

let load_editors = (~mode: Editors.mode, ~instructor_mode: bool): Editors.t =>
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
  let meta = {...init_model.meta, results};
  {editors, settings, langDocMessages, meta};
};

let save = ({editors, settings, langDocMessages, _}: t) => {
  save_editors(editors, ~instructor_mode=settings.instructor_mode);
  Store.LangDocMessages.save(langDocMessages);
  Store.Settings.save(settings);
};

let save_and_return = (model: t) => {
  save(model);
  Ok(model);
};
let reset = (): t => {
  /* Reset model to default, including in localstorage,
     but don't otherwise erase localstorage,, allowing
     e.g. api keys to persist */
  ignore(Store.Settings.init());
  ignore(Store.LangDocMessages.init());
  ignore(Store.Scratch.init());
  ignore(Store.Examples.init());
  ignore(Store.Exercise.init(~instructor_mode=true));
  load(blank);
};
