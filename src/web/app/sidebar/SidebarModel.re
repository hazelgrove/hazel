open Util;

module Settings = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type panel =
    | LanguageDocumentation
    | HelpfulAssistant
    | Probes
    | LogControl
    | Errors;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type errors_settings = {
    syntax_collapsed: bool,
    static_collapsed: bool,
    warnings_collapsed: bool,
    flat: bool,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type errors_action =
    | ToggleSyntaxCollapsed
    | ToggleStaticCollapsed
    | ToggleWarningsCollapsed
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
