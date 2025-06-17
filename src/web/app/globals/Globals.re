open Util;

/* This single data structure collects together all the app-wide values
   that might be of interest to view functions. Most view functions then
   take ~globals as an argument.*/

module Action = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | SetMousedown(bool)
    | SetShowBackpackTargets(bool)
    | SetFontMetrics(Haz3lcorep.FontMetrics.t)
    | Set(Settings.Update.t)
    | JumpToTile(Haz3lcore.Id.t) // Perform(Select(Term(Id(id, Left))))
    | InitImportAll([@opaque] Js_of_ocaml.Js.t(Js_of_ocaml.File.file))
    | FinishImportAll(option(string))
    | ExportForInit
    | Undo
    | Redo;
};

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    // Persistent:
    settings: Settings.t,
    // State:
    font_metrics: Haz3lcorep.FontMetrics.t,
    show_backpack_targets: bool,
    mousedown: bool,
    // Calculated:
    color_highlights: option(Haz3lcorep.ColorSteps.colorMap),
    // Other:
    inject_global: Action.t => Ui_effect.t(unit),
    /* inject_global is not really part of the model, but added here for
       convenience to avoid having to pass it around everywhere. Can only
       be used in view functions. */
    get_log_and: (string => unit) => unit,
    export_all:
      (
        ~settings: Language.CoreSettings.t,
        ~instructor_mode: bool,
        ~log: string
      ) =>
      Yojson.Safe.t,
    export_persistent: unit => unit,
  };

  let load = () => {
    let settings = Settings.Store.load();
    {
      font_metrics: Haz3lcorep.FontMetrics.init,
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

  let can_undo = (action: t) => {
    switch (action) {
    | SetMousedown(_) => false
    | SetShowBackpackTargets(_) => false
    | SetFontMetrics(_) => false
    | Set(action) => Settings.Update.can_undo(action)
    | JumpToTile(_) => false
    | InitImportAll(_) => true
    | FinishImportAll(_) => true
    | ExportForInit => false
    | Undo => false
    | Redo => false
    };
  };
};

module ContextualAction = Haz3lcorep.ContextualAction;

let contextual_actions = (~inject: Action.t => Ui_effect.t(unit)) => [
  ContextualAction.mk(
    ~section="Settings",
    ~mdIcon="tune",
    "Toggle Statics",
    inject(Set(Statics)),
  ),
  ContextualAction.mk(
    ~section="Settings",
    ~mdIcon="tune",
    "Toggle Completion",
    inject(Set(Assist)),
  ),
  ContextualAction.mk(
    ~section="Settings",
    ~mdIcon="tune",
    "Toggle Show Whitespace",
    inject(Set(SecondaryIcons)),
  ),
  ContextualAction.mk(
    ~section="Settings",
    ~mdIcon="tune",
    "Toggle Print Benchmarks",
    inject(Set(Benchmark)),
  ),
  ContextualAction.mk(
    ~section="Settings",
    ~mdIcon="tune",
    "Toggle Toggle Dynamics",
    inject(Set(Dynamics)),
  ),
  ContextualAction.mk(
    ~section="Settings",
    ~mdIcon="tune",
    "Toggle Show Elaboration",
    inject(Set(Elaborate)),
  ),
  ContextualAction.mk(
    ~section="Settings",
    ~mdIcon="tune",
    "Toggle Show Function Bodies",
    inject(Set(Evaluation(ShowFnBodies))),
  ),
  ContextualAction.mk(
    ~section="Settings",
    ~mdIcon="tune",
    "Toggle Show Case Clauses",
    inject(Set(Evaluation(ShowCaseClauses))),
  ),
  ContextualAction.mk(
    ~section="Settings",
    ~mdIcon="tune",
    "Toggle Show fixpoints",
    inject(Set(Evaluation(ShowFixpoints))),
  ),
  ContextualAction.mk(
    ~section="Settings",
    ~mdIcon="tune",
    "Toggle Show Cast Steps",
    inject(Set(Evaluation(ShowCastSteps))),
  ),
  ContextualAction.mk(
    ~section="Settings",
    ~mdIcon="tune",
    "Toggle Show Lookup Steps",
    inject(Set(Evaluation(ShowLookups))),
  ),
  ContextualAction.mk(
    ~section="Settings",
    ~mdIcon="tune",
    "Toggle Show Stepper Filters",
    inject(Set(Evaluation(ShowFilters))),
  ),
  ContextualAction.mk(
    ~section="Settings",
    ~mdIcon="tune",
    "Toggle Show Hidden Steps",
    inject(Set(Evaluation(ShowHiddenSteps))),
  ),
  ContextualAction.mk(
    ~section="Settings",
    ~mdIcon="tune",
    "Toggle Show Docs Sidebar",
    inject(Set(ExplainThis(ToggleShow))),
  ),
  ContextualAction.mk(
    ~section="Settings",
    ~mdIcon="tune",
    "Toggle Show Docs Feedback",
    inject(Set(ExplainThis(ToggleShowFeedback))),
  ),
  ContextualAction.mk(
    ~mdIcon="download",
    ~section="Export",
    "Export For Init",
    inject(ExportForInit),
  ),
];

type t = Model.t;
