open Util;

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
    | JumpToTile(Haz3lcore.Id.t) // Perform(Select(Term(Id(id, Left))))
    | InitImportAll([@opaque] Js_of_ocaml.Js.t(Js_of_ocaml.File.file))
    | FinishImportAll(option(string))
    | ExportPersistentData
    | ActiveEditor(Haz3lcore.Action.t)
    | Undo // These two currently happen at the editor level, and are just
    | Redo; // global actions so they can be accessed by the command palette
};

module Model = {
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
    get_log_and: (string => unit) => unit,
    export_all:
      (
        ~settings: Haz3lcore.CoreSettings.t,
        ~instructor_mode: bool,
        ~log: string
      ) =>
      Yojson.Safe.t,
    export_persistent: unit => unit,
  };

  let load = () => {
    let settings = Settings.Store.load();
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
      get_log_and: _ =>
        failwith(
          "Cannot use get_log_and outside of the main view or update functions!",
        ),
      export_all: (~settings as _, ~instructor_mode as _, ~log as _) =>
        failwith(
          "Cannot use export_all outside of the main view or update functions!",
        ),
      export_persistent: () =>
        failwith(
          "Cannot use export_persistent outside of the main view function!",
        ),
    };
  };

  let save = model => {
    Settings.Store.save(model.settings);
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

type t = Model.t;
