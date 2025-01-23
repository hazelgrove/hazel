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
  type party =
    | Prompt
    | Task
    | LLM
    | LS;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type message = {
    party,
    content: string,
    // This id is to help group LLM/LS chats together... helpful for knowing what to send to LLM
    pass_id: int,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    chat: list(message) /*To-do: Add chat ids for saving past chats*/,
    currSender: party,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  let init: t = {chat: [], currSender: LS};
};

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | SendMessage(Model.message)
    | NewChat;

  let update =
      (~settings: Settings.t, ~action, ~model: Model.t): Updated.t(Model.t) => {
    switch (action) {
    | SendMessage(message) =>
      {
        Model.{
          chat: model.chat @ [message],
          currSender: model.currSender == LLM ? LS : LLM,
        };
      }
      |> Updated.return_quiet
    | NewChat => Model.{chat: [], currSender: LS} |> Updated.return_quiet
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
