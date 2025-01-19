module Sexp = Sexplib.Sexp;
open Haz3lcore;
open Util;

module Settings = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type manual_llm =
    | Agent
    | Human;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type manual_lsp =
    | LanguageServer
    | Human;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    llm: bool,
    lsp: bool,
    ongoing_chat: bool,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | ToggleLLM
    | ToggleLSP
    | UpdateChatStatus;
};
