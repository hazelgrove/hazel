module Sexp = Sexplib.Sexp;
open Haz3lcore;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type mode =
  | CodeSuggestion
  | TaskCompletion
  | SimpleChat;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  llm: bool,
  lsp: bool,
  ongoing_chat: bool,
  mode,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type action =
  | ToggleLLM
  | ToggleLSP
  | UpdateChatStatus
  | SwitchMode(mode);
