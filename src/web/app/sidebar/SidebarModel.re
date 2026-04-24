open Util;

module Settings = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type panel =
    | LanguageDocumentation
    | HelpfulAssistant
    | Probes
    | LogControl
    | Problems;

  [@deriving (show({with_path: false}), sexp, yojson, enumerate)]
  type problem_category =
    Haz3lcore.ProblemCollection.problem_category =
      | Syntax | Hole | Static | Warning;

  /* Base CSS class for a category */
  let category_cls = cat =>
    switch (cat) {
    | Syntax => "syntax"
    | Hole => "hole"
    | Static => "static"
    | Warning => "warning"
    };

  /* Human-readable label */
  let category_label = cat =>
    switch (cat) {
    | Syntax => "Syntax Errors"
    | Hole => "Holes"
    | Static => "Static Errors"
    | Warning => "Warnings"
    };

  /* Short label for legend */
  let category_short_label = cat =>
    switch (cat) {
    | Syntax => "Syntax"
    | Hole => "Hole"
    | Static => "Static"
    | Warning => "Warning"
    };

  /* Badge severity: categories with higher values take priority in the tab icon.
     Categories that share a badge group should share severity. */
  let category_badge_severity = cat =>
    switch (cat) {
    | Syntax
    | Static => 2
    | Warning => 1
    | Hole => 0
    };

  /* CSS class for the tab badge indicator */
  let category_badge_cls = cat =>
    switch (cat) {
    | Syntax
    | Static => "has-errors"
    | Warning => "has-warnings"
    | Hole => "has-holes"
    };

  /* Singular label for the badge tooltip */
  let category_badge_label = cat =>
    switch (cat) {
    | Syntax
    | Static => "error"
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
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | ToggleShow
    | SwitchPanel(panel)
    | Problems(problems_action);
};
