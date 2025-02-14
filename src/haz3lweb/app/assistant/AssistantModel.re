module Sexp = Sexplib.Sexp;
open Haz3lcore;
open Util;
open Util.OptUtil.Syntax;
open Example;

module CodeModel = CodeEditable.Model;

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
    code: option(Segment.t),
    content: string,
    collapsed: bool,
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
    | SetKey(string)
    | SendSketch
    | NewChat
    | Respond(Model.message)
    | ToggleCollapse(int);

  let react = (response: string): t => {
    // let response = response |> sanitize_response |> quote;
    let zipper_of_response = Printer.zipper_of_string(response);
    switch (zipper_of_response) {
    | Some(z) =>
      let segment_of_response =
        Zipper.smart_seg(~dump_backpack=true, ~erase_buffer=true, z);
      let response_as_message: Model.message = {
        party: LLM,
        code: Some(segment_of_response),
        content: "",
        collapsed: String.length(response) >= 200,
      };
      Respond(response_as_message);
    | None =>
      let response_as_message: Model.message = {
        party: LLM,
        code: None,
        content: response,
        collapsed: String.length(response) >= 200,
      };
      Respond(response_as_message);
    };
  };

  let await_llm_response: Model.message = {
    party: LLM,
    code: None,
    content: "...",
    collapsed: false,
  };

  let update =
      (
        ~settings: AssistantSettings.t,
        ~action,
        ~editor: CodeModel.t,
        ~model: Model.t,
        ~schedule_action,
      )
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
        Model.{
          chat: model.chat @ [message, await_llm_response],
          currSender: LLM,
        }
        |> Updated.return_quiet;
      | _ => Model.{chat: model.chat, currSender: LLM} |> Updated.return_quiet
      }
    | SetKey(api_key) =>
      Store.Generic.save("API", api_key);
      model |> Updated.return_quiet;
    | NewChat => Model.{chat: [], currSender: LS} |> Updated.return_quiet
    | Respond(message) =>
      Model.{chat: ListUtil.leading(model.chat) @ [message], currSender: LS}
      |> Updated.return_quiet
    | SendSketch =>
      let sketch_seg =
        Zipper.smart_seg(
          ~dump_backpack=true,
          ~erase_buffer=true,
          editor.editor.state.zipper,
        );
      switch (
        {
          let* index = Indicated.index(editor.editor.state.zipper);
          let* ci = Id.Map.find_opt(index, editor.statics.info_map);
          ChatLSP.Prompt.mk_init(ChatLSP.Options.init, ci, sketch_seg);
        }
      ) {
      | None =>
        print_endline("prompt generation failed");
        Model.{chat: model.chat, currSender: LLM} |> Updated.return_quiet;
      | Some(openai_prompt) =>
        let messages =
          List.map(
            (msg: OpenAI.message): string => {msg.content},
            openai_prompt,
          );
        let prompt = ListUtil.concat_strings(messages);
        let llm = OpenAI.Azure_GPT4_0613;
        let key = OpenAI.lookup_key(llm);
        let params: OpenAI.params = {llm, temperature: 1.0, top_p: 1.0};
        OpenAI.start_chat(~params, ~key, openai_prompt, req =>
          switch (OpenAI.handle_chat(req)) {
          | Some({content, _}) => schedule_action(react(content))
          | None => print_endline("Assistant: response parse failed")
          }
        );
        Model.{
          chat:
            model.chat
            @ [
              {
                party: LS,
                code: Some(sketch_seg),
                content: prompt,
                collapsed: String.length(prompt) >= 200,
              },
              await_llm_response,
            ],
          currSender: LLM,
        }
        |> Updated.return_quiet;
      };
    | ToggleCollapse(index) =>
      let updated_chat =
        List.mapi(
          (i: int, msg: Model.message) =>
            if (i == index) {
              {...msg, collapsed: !msg.collapsed};
            } else {
              msg;
            },
          model.chat,
        );
      Model.{...model, chat: updated_chat} |> Updated.return_quiet;
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
