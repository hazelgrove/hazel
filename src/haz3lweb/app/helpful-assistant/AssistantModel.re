module Sexp = Sexplib.Sexp;
open Haz3lcore;
open Util;
open Util.OptUtil.Syntax;
open Example;
open StringUtil;

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
    llm: OpenRouter.chat_models,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  let init: t = {chat: [], currSender: LS, llm: Gemini_Flash_Lite_2_0};
};

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | SendMessage(Model.message)
    | SetKey(string)
    | SendSketch
    | NewChat
    | Respond(Model.message)
    | ToggleCollapse(int)
    | SelectLLM(OpenRouter.chat_models);

  let react = (~response: string, ~code_suggestion: bool): t => {
    // let response = response |> sanitize_response |> quote;
    let zipper_of_response = Printer.zipper_of_string(response);
    let response_as_message: Model.message = {
      party: LLM,
      code: None,
      content: response,
      collapsed: String.length(response) >= 200,
    };
    code_suggestion
      ? switch (zipper_of_response) {
        | Some(z) =>
          let segment_of_response =
            Zipper.smart_seg(~dump_backpack=true, ~erase_buffer=true, z);
          let response_as_message: Model.message = {
            party: LLM,
            code: Some(segment_of_response),
            content: response,
            collapsed: String.length(response) >= 200,
          };
          Respond(response_as_message);
        | None => Respond(response_as_message)
        }
      : Respond(response_as_message);
  };

  let await_llm_response: Model.message = {
    party: LLM,
    code: None,
    content: "...",
    collapsed: false,
  };

  let collect_chat = (~messages: list(Model.message)): string => {
    let chat = "The following is a log of the current conversation. This is solely for the purpose
    to help you recall the entire conversation, in case the user asks you something that needs context
    from before. You should respond as normal, using the entire chat as context, and understand that the
    most recent \"User Input\" is what the user is currently sending/asking, and is what your main focus should be.
    For the most part, you should treat this solely as a prompt, and not explicitly acknowledge it in your
    reponse. Here is the conversation for context: ";
    List.fold_left(
      (chat: string, message: Model.message) =>
        if (message.party == LLM) {
          chat ++ "Your Reponse: " ++ message.content ++ " ";
        } else if (message.party == LS) {
          chat ++ "User Input: " ++ message.content ++ " ";
        } else {
          chat ++ message.content;
        },
      chat,
      messages,
    );
  };

  let check_req =
      (
        char: string,
        schedule_action: t => unit,
        {caret, relatives: {siblings, _}, _} as z: Zipper.t,
      )
      : unit => {
    switch (caret, Zipper.neighbor_monotiles(siblings)) {
    | (Outer, (_, Some(_))) =>
      switch (Zipper.right_neighbor_monotile(siblings)) {
      | Some(c) => c == "??" ? schedule_action(SendSketch) : ()
      | _ => ()
      }
    | (Outer, (_, None)) =>
      switch (Zipper.left_neighbor_monotile(siblings)) {
      | Some(c) => c == "??" ? schedule_action(SendSketch) : ()
      | _ => ()
      }
    | _ => ()
    };
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
      switch (message.party) {
      | LS =>
        let collected_chat = collect_chat(~messages=model.chat @ [message]);
        print_endline(collected_chat);
        switch (Oracle.ask(collected_chat)) {
        | None => print_endline("Oracle: prompt generation failed")
        | Some(prompt) =>
          let llm = model.llm;
          let key = Store.Generic.load("API");
          let params: OpenRouter.params = {llm, temperature: 1.0, top_p: 1.0};
          OpenRouter.start_chat(~params, ~key, prompt, req =>
            switch (OpenRouter.handle_chat(req)) {
            | Some({content, _}) =>
              schedule_action(
                react(~response=content, ~code_suggestion=false),
              )
            | None => print_endline("Assistant: response parse failed")
            }
          );
        };
        Model.{
          ...model,
          chat: model.chat @ [message, await_llm_response],
          currSender: LLM,
        }
        |> Updated.return_quiet;
      | _ =>
        Model.{...model, chat: model.chat, currSender: LLM}
        |> Updated.return_quiet
      }
    | SetKey(api_key) =>
      Store.Generic.save("API", api_key);
      model |> Updated.return_quiet;
    | NewChat =>
      Model.{...model, chat: [], currSender: LS} |> Updated.return_quiet
    | Respond(message) =>
      Model.{
        ...model,
        chat: ListUtil.leading(model.chat) @ [message],
        currSender: LS,
      }
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
        Model.{...model, chat: model.chat, currSender: LLM}
        |> Updated.return_quiet;
      | Some(openrouter_prompt) =>
        let messages =
          List.map(
            (msg: OpenRouter.message): string => {msg.content},
            openrouter_prompt,
          );
        let prompt = ListUtil.concat_strings(messages);
        let message: Model.message = {
          party: LS,
          code: Some(sketch_seg),
          content: prompt,
          collapsed: String.length(prompt) >= 200,
        };
        let collected_chat = collect_chat(~messages=model.chat @ [message]);
        print_endline(collected_chat);
        let llm = model.llm;
        let key = Store.Generic.load("API");
        let params: OpenRouter.params = {llm, temperature: 1.0, top_p: 1.0};
        OpenRouter.start_chat(~params, ~key, openrouter_prompt, req =>
          switch (OpenRouter.handle_chat(req)) {
          | Some({content, _}) =>
            schedule_action(react(~response=content, ~code_suggestion=true))
          | None => print_endline("Assistant: response parse failed")
          }
        );
        Model.{
          ...model,
          chat: model.chat @ [message, await_llm_response],
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
    | SelectLLM(llm) => {...model, llm} |> Updated.return_quiet
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
