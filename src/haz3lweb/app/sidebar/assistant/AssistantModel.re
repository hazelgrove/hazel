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

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {chat: list(string)};

  [@deriving (show({with_path: false}), sexp, yojson)]
  let init: t = {chat: []};
};

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | SendMessage(string);

  let update =
      (~settings: Settings.t, action, model: Model.t): Updated.t(Model.t) => {
    switch (action) {
    | SendMessage(message) =>
      print_endline(message);
      Model.{chat: ["updated", "and testing"]} |> Updated.return_quiet;
    };
  };
};

module Store =
  Store.F({
    [@deriving (show({with_path: false}), yojson, sexp)]
    type t = Model.t;
    let default = () => Model.init;
    let key = Store.Assistant;
  });
