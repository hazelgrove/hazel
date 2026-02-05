open Util;

module Settings = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type panel =
    | LanguageDocumentation
    | HelpfulAssistant
    | Probes
    | AppView;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    show: bool,
    panel,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | ToggleShow
    | SwitchPanel(panel);
};
