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
  type t = {
    show: bool,
    panel,
    syntax_collapsed: bool,
    static_collapsed: bool,
    warnings_collapsed: bool,
    errors_flat: bool,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | ToggleShow
    | SwitchPanel(panel)
    | ToggleSyntaxCollapsed
    | ToggleStaticCollapsed
    | ToggleWarningsCollapsed
    | ToggleErrorsFlat;
};
