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
    | Static
    | Warning;

  /* Base CSS class for a category */
  let category_cls = cat =>
    switch (cat) {
    | Syntax => "syntax"
    | Static => "static"
    | Warning => "warning"
    };

  /* Human-readable label */
  let category_label = cat =>
    switch (cat) {
    | Syntax => "Syntax Errors"
    | Static => "Static Errors"
    | Warning => "Warnings"
    };

  /* Short label for legend */
  let category_short_label = cat =>
    switch (cat) {
    | Syntax => "Syntax"
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

  [@deriving (show({with_path: false}), sexp, yojson)]
  type errors_action =
    | ToggleCollapsed(error_category)
    | ToggleFlat;

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
