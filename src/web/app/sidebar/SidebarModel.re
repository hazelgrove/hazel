open Util;

module Settings = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type panel =
    | LanguageDocumentation
    | HelpfulAssistant
    | Probes
    | LogControl
    | Errors;

  [@deriving (show({with_path: false}), sexp, yojson, enumerate)]
  type error_category =
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

  /* Derived CSS helpers */
  let category_row_cls = cat => "error-" ++ category_cls(cat);
  let category_section_cls = cat => category_cls(cat) ++ "-errors";

  [@deriving (show({with_path: false}), sexp, yojson)]
  type errors_settings = {
    collapsed: list(error_category),
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
  type errors_action =
    | ToggleCollapsed(error_category)
    | ToggleFlat
    | ToggleExpanded(Id.t);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    show: bool,
    panel,
    errors: errors_settings,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | ToggleShow
    | SwitchPanel(panel)
    | Errors(errors_action);
};
