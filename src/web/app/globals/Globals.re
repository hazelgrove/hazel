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
    | SetAgentGlobals(AgentGlobals.Update.action)
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
    /* Which rows the Fumola panel is showing in full. Focus replaces what is
       open IN ITS OWN NAMESPACE, which is what following a pointer out of
       the event list means: that node at that moment, and not the same node
       at another -- while leaving every edge as the reader left it. */
    | FumolaToggleOpen(string)
    | FumolaFocus(string)
    /* Put an instance back to nothing and run the program again, so the graph
       is rebuilt rather than remembered. */
    | FumolaReset(string, Language.FumolaRun.mode)
    | AppViewMsg(Haz3lcore.Id.t, Language.DHExp.t) // route msg through update_fn
    // InitAppView takes (id, source_result, model, update_fn, view_fn, subs_fn)
    | InitAppView(
        Haz3lcore.Id.t,
        Language.DHExp.t,
        Language.DHExp.t,
        Language.DHExp.t,
        Language.DHExp.t,
        Language.DHExp.t,
      )
    | RethrowException
    | ClearException
    | RestoreLastKnownGood;
};

/* What a `fumola_open` key names: "n" a node revision, "e" an edge.

   The namespace is a dimension of the panel in its own right. A reader
   expands some nodes and then follows an edge id out of one of them; the
   edge is what they asked to see, and the nodes are the context they built
   to ask it in. Replacing the whole set threw that context away, so the one
   click cost every expansion on the other tab. Focus now replaces only
   within the namespace it names.

   The moment is a third dimension and is deliberately inside the node key:
   two revisions of one space are two rows in the list, and a reader
   comparing them wants both open at once. */
let fumola_namespace = (key: string): string =>
  switch (String.index_opt(key, ':')) {
  | Some(i) => String.sub(key, 0, i)
  | None => ""
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
    /* Fumola panel rows shown in full. Each key carries the namespace of
       what it names -- see `fumola_namespace`. Not persisted, and
       deliberately: the runtime mints these ids afresh on every page load,
       so a persisted set would accumulate dead keys forever. */
    fumola_open: list(string),
    /* The row a pointer was just followed to, if any. Separate from the open
       set because being open and being arrived at are different things: a
       reader opens several rows and is looking at one of them. Cleared when a
       row is opened by hand, so only following scrolls. */
    fumola_focused: option(string),
    /* The documentation slide showing, when one is: what a copied link names,
       and the only deck `?slide=` can address. Assembled per frame in
       `Page.main_view`, which is the first place that knows both the mode and
       the slide -- an editor does not know it is on a slide at all. */
    slide_name: option(string),
    // MVU apps, keyed by app-projector syntax id; not persisted
    apps: AppStore.t,
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
    fumola_open: [],
    fumola_focused: None,
    slide_name: None,
    apps: AppStore.empty,
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
    /* A link's `?panel=` and the rest sit over the stored settings: the
       reader arriving is shown what the link is about, and everything they
       have not been sent to stays as they left it. */
    let settings = Settings.Store.load() |> DeepLink.settings;
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
};

type t = Model.t;
