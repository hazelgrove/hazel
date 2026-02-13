open Util;

/* This single data structure collects together all the app-wide values
   that might be of interest to view functions. Most view functions then
   take ~globals as an argument.*/

/* Viewport culling for projectors/refractors.
 * None = no culling (all visible), Some(range) = only show in range */
module VisibleRows = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    first: int,
    last: int,
  };

  /* Compute visible row range from scroll container properties.
   * buffer: extra rows above/below to prevent popping */
  let compute =
      (
        ~scroll_top: float,
        ~client_height: float,
        ~row_height: float,
        ~buffer=5,
        (),
      )
      : t => {
    let first = max(0, int_of_float(scroll_top /. row_height) - buffer);
    let visible_count = int_of_float(client_height /. row_height);
    let last = first + visible_count + 2 * buffer;
    {
      first,
      last,
    };
  };

  /* Check if visible_rows changed significantly (threshold of 2 rows) */
  let changed = (old: option(t), new_rows: t): bool =>
    switch (old) {
    | None => true
    | Some(old) =>
      abs(old.first - new_rows.first) > 2
      || abs(old.last - new_rows.last) > 2
    };
};

// MVU App state for the sidebar
// App = (init_model, view: model -> Html, subs: model -> Sub)
// We store pre-evaluated html and subs to avoid re-evaluating in view
module AppViewState = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    source_result: Language.DHExp.t, // Eval result this state was derived from
    model: Language.DHExp.t, // Current model state
    update_fn: option(Language.DHExp.t), // Some = Elm update fn, None = legacy
    view_fn: Language.DHExp.t, // view: model -> Html
    subs_fn: Language.DHExp.t, // subscriptions: model -> Sub
    html: Language.DHExp.t, // Pre-evaluated: view_fn(model)
    subs: Language.DHExp.t // Pre-evaluated: subs_fn(model)
  };
};

module Action = {
  [@deriving (show({with_path: false}), yojson, sexp)]
  type log =
    | InitImport([@opaque] Js_of_ocaml.Js.t(Js_of_ocaml.File.file))
    | FinishImport(option(string))
    | NextLog
    | SkipLog
    | ToggleReplay
    | ClearLog;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | SetFontMetrics(FontMetrics.t)
    | Set(Settings.Update.t)
    | JumpToTile(Haz3lcore.Id.t) // Perform(Select(Term(Id(id, Left))))
    | InitImportAll([@opaque] Js_of_ocaml.Js.t(Js_of_ocaml.File.file))
    | FinishImportAll(option(string))
    | ExportForInit
    | ActiveEditor(Haz3lcore.Action.t)
    | Undo // These two currently happen at the editor level, and are just
    | Redo // global actions so they can be accessed by the command palette
    | Log(log)
    | SetMetaDown(bool)
    | UpdateVisibleRows(VisibleRows.t)
    | SetAppViewModel(Language.DHExp.t) // Update the MVU model state
    | AppViewMsg(Language.DHExp.t) // Elm mode: route msg through update_fn
    // InitAppView takes (source_result, model, update_fn option, view_fn, subs_fn)
    | InitAppView(
        Language.DHExp.t,
        Language.DHExp.t,
        option(Language.DHExp.t),
        Language.DHExp.t,
        Language.DHExp.t,
      )
    // RefreshAppView: code changed, try to preserve model state
    | RefreshAppView(
        Language.DHExp.t,
        Language.DHExp.t,
        option(Language.DHExp.t),
        Language.DHExp.t,
        Language.DHExp.t,
      )
    | ResetAppView // Reset App View to show evaluation result
    | RethrowException
    | ClearException;
};

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    // Persistent:
    settings: Settings.t,
    // State:
    font_metrics: FontMetrics.t,
    meta_down: bool,
    visible_rows: option(VisibleRows.t),
    // MVU App View sidebar state
    app_view_state: option(AppViewState.t),
    // Calculated:
    color_highlights: option(ColorSteps.colorMap),
    // Other:
    inject_global: Action.t => Ui_effect.t(unit),
    /* inject_global is not really part of the model, but added here for
       convenience to avoid having to pass it around everywhere. Can only
       be used in view functions. */
    get_log_and: (string => unit) => unit,
    get_log_count: (int => unit) => unit,
    export_all:
      (
        ~settings: Language.CoreSettings.t,
        ~instructor_mode: bool,
        ~log: string
      ) =>
      Yojson.Safe.t,
    export_persistent: unit => unit,
  };

  let init =
      (~settings=Settings.Model.init, ~font_metrics=FontMetrics.init, ()) => {
    settings,
    font_metrics,
    meta_down: false,
    visible_rows: None,
    app_view_state: None,
    color_highlights: None,
    inject_global: _ =>
      failwith("Cannot use inject_global outside of the main view function!"),
    get_log_and: _ =>
      failwith(
        "Cannot use get_log_and outside of the main view or update functions!",
      ),
    get_log_count: _ =>
      failwith(
        "Cannot use get_log_count outside of the main view or update functions!",
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

  let load = () => {
    let settings = Settings.Store.load();
    init(~settings, ());
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
    | SetFontMetrics(_) => false
    | Set(action) => Settings.Update.can_undo(action)
    | JumpToTile(_) => false
    | InitImportAll(_) => true
    | FinishImportAll(_) => true
    | ExportForInit => false
    | ActiveEditor(_) => false
    | Undo => false
    | Redo => false
    | SetMetaDown(_) => false
    | UpdateVisibleRows(_) => false
    | SetAppViewModel(_) => false
    | AppViewMsg(_) => false
    | InitAppView(_) => false
    | RefreshAppView(_) => false
    | ResetAppView => false
    | Log(_) => false
    | RethrowException => false
    | ClearException => false
    };
  };
};

type t = Model.t;
