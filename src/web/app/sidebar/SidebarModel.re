open Util;

module Settings = {
  /* Which of the Fumola panel's three views is showing. They are three
     indices on one history -- the same fetch answers all three -- so this is
     a view choice and not three panels. */
  /* What to do with the editor's own edges and events -- the ones sourced at
     a node whose space is Here, at whatever time. They are here to be
     inspected, and they get verbose, so they can be dimmed or dropped
     without being forgotten.

     "The editor" is meant literally: these are the puts Hazel itself made on
     the program's behalf, as against what the program computed. */
  [@deriving (show({with_path: false}), sexp, yojson, enumerate)]
  type fumola_editor =
    | Show
    | Dim
    | Hide;

  [@deriving (show({with_path: false}), sexp, yojson, enumerate)]
  type fumola_tab =
    | Events
    | Nodes
    | Edges;

  /* `enumerate` so a link can name a panel without a second list of names to
     keep in step: DeepLink matches `?panel=` against these constructors. */
  [@deriving (show({with_path: false}), sexp, yojson, enumerate)]
  type panel =
    | LanguageDocumentation
    | HelpfulAssistant
    | Probes
    | Projectors
    | LogControl
    | Problems
    | Fumola
    | TaskReference
    | DebugInfo;

  [@deriving (show({with_path: false}), sexp, yojson, enumerate)]
  type problem_category =
    Haz3lcore.ProblemCollection.problem_category =
      | Syntax | Hole | Static | Warning | Projector;

  /* Base CSS class for a category */
  let category_cls = cat =>
    switch (cat) {
    | Syntax => "syntax"
    | Hole => "hole"
    | Static => "static"
    | Warning => "warning"
    | Projector => "projector-error"
    };

  /* Human-readable label */
  let category_label = cat =>
    switch (cat) {
    | Syntax => "Syntax Errors"
    | Hole => "Holes"
    | Static => "Static Errors"
    | Warning => "Warnings"
    | Projector => "Projector Errors"
    };

  /* Short label for legend */
  let category_short_label = cat =>
    switch (cat) {
    | Syntax => "Syntax"
    | Hole => "Hole"
    | Static => "Static"
    | Warning => "Warning"
    | Projector => "Projector"
    };

  /* Badge severity: categories with higher values take priority in the tab icon.
     Categories that share a badge group should share severity. */
  let category_badge_severity = cat =>
    switch (cat) {
    | Syntax
    | Static => 2
    | Projector
    | Warning => 1
    | Hole => 0
    };

  /* CSS class for the tab badge indicator */
  let category_badge_cls = cat =>
    switch (cat) {
    | Syntax
    | Static => "has-errors"
    | Projector
    | Warning => "has-warnings"
    | Hole => "has-holes"
    };

  /* Singular label for the badge tooltip */
  let category_badge_label = cat =>
    switch (cat) {
    | Syntax
    | Static => "error"
    | Projector
    | Warning => "warning"
    | Hole => "hole"
    };

  /* Derived CSS helpers */
  let category_row_cls = category_cls;
  let category_section_cls = category_cls;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type problems_settings = {
    /* Collapsed category sections keyed by `(editor_label, category)` so
       each editor group has its own per-category collapse state.
       Single-editor modes pass `""` as the label. */
    collapsed: list((string, problem_category)),
    /* Collapsed editor groups keyed by editor label. Only meaningful when
       there is more than one group shown. */
    collapsed_editors: list(string),
    flat: bool,
    expanded: list(Id.t),
  };

  let is_collapsed = (label, cat, settings) =>
    List.mem((label, cat), settings.collapsed);

  let toggle_collapsed = (label, cat, settings) =>
    if (is_collapsed(label, cat, settings)) {
      {
        ...settings,
        collapsed:
          List.filter(pair => pair != (label, cat), settings.collapsed),
      };
    } else {
      {
        ...settings,
        collapsed: [(label, cat), ...settings.collapsed],
      };
    };

  let is_editor_collapsed = (label, settings) =>
    List.mem(label, settings.collapsed_editors);

  let toggle_editor_collapsed = (label, settings) =>
    if (is_editor_collapsed(label, settings)) {
      {
        ...settings,
        collapsed_editors:
          List.filter(l => l != label, settings.collapsed_editors),
      };
    } else {
      {
        ...settings,
        collapsed_editors: [label, ...settings.collapsed_editors],
      };
    };

  let is_expanded = (id, settings) => List.mem(id, settings.expanded);

  let toggle_expanded = (id, settings) =>
    if (is_expanded(id, settings)) {
      {
        ...settings,
        expanded: List.filter(i => !Id.equal(i, id), settings.expanded),
      };
    } else {
      {
        ...settings,
        expanded: [id, ...settings.expanded],
      };
    };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type problems_action =
    | ToggleCollapsed(string, problem_category)
    | ToggleEditorCollapsed(string)
    | ToggleFlat
    | ToggleExpanded(Id.t);

  /* A field removed from this record must not cost a reader every other
     setting they have.

     `Store.deserialize` catches whatever `t_of_sexp` raises, prints a line to
     the console and hands back the WHOLE default record; there is no
     versioning and no migration. ppx_sexp_conv raises on a field it does not
     recognise, so renaming `fumola_prime_mover` to `fumola_editor` meant that
     every browser which had run an earlier build of this branch -- including
     the ones the demo runs on -- would silently lose instructor mode, the
     dynamics toggles, the worker encodings, the line numbers and the rest,
     on the next load. `debug_collapsed`, which dev replaced with the inverse
     `debug_expanded`, is the same case. The `[@sexp.default]` attributes below
     do not help: they cover a field that is MISSING, not one left over.

     So extra fields are ignored here. A stale key is a key nothing reads,
     which is what it should have been all along. */
  [@deriving (show({with_path: false}), sexp, yojson)]
  [@sexp.allow_extra_fields]
  [@yojson.allow_extra_fields]
  type t = {
    show: bool,
    panel,
    problems: problems_settings,
    debug_show_raw: bool,
    /* Debug sidebar sections/fields the user has expanded, keyed by section
       title or field label. Everything starts collapsed: the panel is deep, and
       the instrumented sections only collect while expanded. Persists across
       cursor moves, so expanding e.g. "ctx" keeps it open regardless of the term
       under the cursor. */
    [@sexp.default []] [@yojson.default []]
    debug_expanded: list(string),
    /* Encodings (WorkerServer.encoding) enabled in the Worker Messaging panel;
       only these are benchmarked. Defaults to just the active encoding
       (Marshal) — Direct and Sexp start off — and is defaulted on load so
       existing persisted settings (which lack this field) still load. */
    [@sexp.default [WorkerServer.Marshal]] [@yojson.default
                                              [WorkerServer.Marshal]
                                            ]
    worker_encodings: list(WorkerServer.encoding),
    /* Defaulted on load for the same reason as worker_encodings: settings
       persisted before this field existed still have to load. A tab is a
       fixed name, so it is safe to persist -- unlike a node or edge id,
       which the runtime mints afresh on every page load and which would
       accumulate here forever. */
    [@sexp.default Events] [@yojson.default Events]
    fumola_tab,
    /* Dim by default: it answers "they get verbose" without anything
       disappearing before the reader knows it was ever there. */
    [@sexp.default Dim] [@yojson.default Dim]
    fumola_editor,
  };

  let is_debug_expanded = (key: string, settings: t) =>
    List.mem(key, settings.debug_expanded);

  let toggle_debug_expanded = (key: string, settings: t): t =>
    if (is_debug_expanded(key, settings)) {
      {
        ...settings,
        debug_expanded: List.filter(k => k != key, settings.debug_expanded),
      };
    } else {
      {
        ...settings,
        debug_expanded: [key, ...settings.debug_expanded],
      };
    };

  let is_encoding_enabled = (e: WorkerServer.encoding, settings: t) =>
    List.mem(e, settings.worker_encodings);

  let toggle_encoding = (e: WorkerServer.encoding, settings: t): t =>
    if (is_encoding_enabled(e, settings)) {
      {
        ...settings,
        worker_encodings: List.filter(x => x != e, settings.worker_encodings),
      };
    } else {
      {
        ...settings,
        worker_encodings: [e, ...settings.worker_encodings],
      };
    };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | ToggleShow
    | SwitchPanel(panel)
    | Problems(problems_action)
    | ToggleDebugRaw
    | ToggleDebugExpanded(string)
    | SwitchFumolaTab(fumola_tab)
    | SwitchFumolaEditor(fumola_editor)
    | ToggleWorkerEncoding(WorkerServer.encoding);
};
