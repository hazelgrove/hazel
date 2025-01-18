module Sexp = Sexplib.Sexp;
open Haz3lcore;
open Util;

module Settings = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type window =
    | LanguageDocumentation
    | HelpfulAssistant;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    show: bool,
    window,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | ToggleShow
    | SwitchWindow(window);
};
