open Sexplib.Std;

/* This single data structure collects together all the app-wide values
   that might be of interest to view functions. Most view functions then
   take ~globals as an argument.*/

module Action = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | SetMousedown(bool)
    | SetShowBackpackTargets(bool)
    | SetFontMetrics(FontMetrics.t)
    | Set(Settings.Update.t)
    | JumpToTile(Haz3lcore.Id.t);
};

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    // Persistent:
    settings: Settings.t,
    // State:
    font_metrics: FontMetrics.t,
    show_backpack_targets: bool,
    mousedown: bool,
    // Calculated:
    color_highlights: option(ColorSteps.colorMap),
    // Other:
    inject_global: Action.t => Ui_effect.t(unit),
    /* inject_global is not really part of the model, but added here for
       convenience to avoid having to pass it around everywhere. Can only
       be used in view functions. */
  };

  let load = () => {
    let settings = Store.Settings.load();
    {
      font_metrics: FontMetrics.init,
      show_backpack_targets: false,
      mousedown: false,
      settings,
      color_highlights: None,
      inject_global: _ =>
        failwith(
          "Cannot use inject_global outside of the main view function!",
        ),
    };
  };

  let save = model => {
    Store.Settings.save(model.settings);
  };
};

module Update = {
  include Action;

  // Update is handled by the top-level update function

  let calculate = (color_highlights, model: Model.t): Model.t => {
    ...model,
    color_highlights,
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = Model.t;
