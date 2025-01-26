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
    | NewChat
    | Respond(Model.message);

  let react = (response: string): t => {
    // let response = response |> sanitize_response |> quote;
    let response: Model.message = {party: LLM, content: response};
    Respond(response);
  };

  let update =
      (~settings: Settings.t, ~action, ~model: Model.t, ~schedule_action)
      : Updated.t(Model.t) => {
    switch (action) {
    | SendMessage(message) =>
      // todo: send API Call here
      switch (message.party) {
      | LS =>
        switch (Oracle.ask(message.content)) {
        | None => print_endline("Oracle: prompt generation failed")
        | Some(prompt) =>
          let llm = OpenAI.Azure_GPT4_0613;
          let key = OpenAI.lookup_key(llm);
          let params: OpenAI.params = {llm, temperature: 1.0, top_p: 1.0};
          OpenAI.start_chat(~params, ~key, prompt, req =>
            switch (OpenAI.handle_chat(req)) {
            | Some({content, _}) => schedule_action(react(content))
            | None => print_endline("Assistant: response parse failed")
            }
          );
        };
        Model.{chat: model.chat @ [message], currSender: LLM}
        |> Updated.return_quiet;
      | _ =>
        Model.{
          chat:
            model.chat
            @ [
              {
                party: LS,
                content: "Message Not Sent: Waiting for LLM Response",
              },
            ],
          currSender: LLM,
        }
        |> Updated.return_quiet
      }
    | NewChat => Model.{chat: [], currSender: LS} |> Updated.return_quiet
    | Respond(message) =>
      Model.{chat: model.chat @ [message], currSender: LS}
      |> Updated.return_quiet
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
