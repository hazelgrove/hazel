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
    | Syntax
    | Hole
    | Static
    | Warning;

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
    collapsed: list(problem_category),
    flat: bool,
    expanded: list(Id.t),
  };

  let is_collapsed = (cat, settings) => List.mem(cat, settings.collapsed);

  let toggle_collapsed = (cat, settings) =>
    if (is_collapsed(cat, settings)) {
      {
        ...settings,
        collapsed: List.filter(c => c != cat, settings.collapsed),
      };
    } else {
      {
        ...settings,
        collapsed: [cat, ...settings.collapsed],
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
    | ToggleCollapsed(problem_category)
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
