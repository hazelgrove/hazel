open Util_web;

module Settings = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type panel =
    | LanguageDocumentation
    | HelpfulAssistant
    | Probes
    | LogControl
    | Problems
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

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    show: bool,
    panel,
    problems: problems_settings,
    debug_show_raw: bool,
    /* Collapsed debug sidebar sections/fields, keyed by section title or
       field label. Persists across cursor moves so collapsing e.g. "ctx"
       keeps it collapsed regardless of the term under the cursor. */
    debug_collapsed: list(string),
    /* Encodings (WorkerServer.encoding) enabled in the Worker Messaging panel;
       only these are benchmarked. Defaults to just the active encoding
       (Marshal) — Direct and Sexp start off — and is defaulted on load so
       existing persisted settings (which lack this field) still load. */
    [@sexp.default [WorkerServer.Marshal]] [@yojson.default
                                              [WorkerServer.Marshal]
                                            ]
    worker_encodings: list(WorkerServer.encoding),
  };

  let is_debug_collapsed = (key: string, settings: t) =>
    List.mem(key, settings.debug_collapsed);

  let toggle_debug_collapsed = (key: string, settings: t): t =>
    if (is_debug_collapsed(key, settings)) {
      {
        ...settings,
        debug_collapsed: List.filter(k => k != key, settings.debug_collapsed),
      };
    } else {
      {
        ...settings,
        debug_collapsed: [key, ...settings.debug_collapsed],
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
    | ToggleDebugCollapsed(string)
    | ToggleWorkerEncoding(WorkerServer.encoding);
};
