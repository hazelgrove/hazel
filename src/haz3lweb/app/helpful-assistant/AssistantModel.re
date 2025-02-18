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
    tile: Id.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  let init: t = {
    chat: [],
    currSender: LS,
    llm: Gemini_Flash_Lite_2_0,
    tile: Id.invalid,
  };
};

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | SendMessage(Model.message)
    | SetKey(string)
    | SendSketch
    | SendError(string, Info.t, int)
    | ErrorRespond(string, Info.t, int)
    | NewChat
    | Respond(Model.message)
    | ToggleCollapse(int)
    | SelectLLM(OpenRouter.chat_models)
    | StoreTile(Id.t)
    | RemoveTile;

  let code_message_of_str =
      (settings, editor: CodeModel.t, response: string, party: Model.party)
      : Model.message => {
    /* Alternate method using Detruct and Insert. We need a memory of cursor location for this however.
       let z = editor.editor.state.zipper;
       let z = Option.get(Destruct.go(Direction.Left, z));
       let z = Option.get(Destruct.go(Direction.Left, z));
       let z = Option.get(Insert.go(response, z));
       let segment_of_response =
         Zipper.smart_seg(~dump_backpack=true, ~erase_buffer=true, z);
       {
         party,
         code: Some(segment_of_response),
         content: response,
         collapsed: String.length(response) >= 200,
       }; */
    // Hack(Russ) Uses same logic Andrew uses in Oracle.re to remove "??"
    let string_of_sketch =
      Printer.zipper_to_string(editor.editor.state.zipper);
    let sketch_with_response =
      Str.global_replace(Str.regexp("\\?\\?"), response, string_of_sketch);
    let zipper_of_response = Printer.zipper_of_string(sketch_with_response);
    switch (zipper_of_response) {
    | Some(z) =>
      let segment_of_response =
        Zipper.smart_seg(~dump_backpack=true, ~erase_buffer=true, z);
      {
        party,
        code: Some(segment_of_response),
        content: response,
        collapsed: String.length(response) >= 200,
      };
    | None => {
        party,
        code: None,
        content: response,
        collapsed: String.length(response) >= 200,
      }
    };
  };

  let text_message_of_str =
      (response: string, party: Model.party): Model.message => {
    {
      party,
      code: None,
      content: response,
      collapsed: String.length(response) >= 200,
    };
  };

  let react =
      (
        ~settings,
        ~editor: CodeModel.t,
        ~response: string,
        ~code_suggestion: bool,
      )
      : t => {
    // let response = response |> sanitize_response |> quote;
    code_suggestion
      ? Respond(code_message_of_str(settings, editor, response, LLM))
      : Respond(text_message_of_str(response, LLM));
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

  let check_req = (_: string, schedule_action: t => unit, z: Zipper.t): unit => {
    let caret = z.caret;
    let siblings = z.relatives.siblings;
    switch (caret, Zipper.neighbor_monotiles(siblings)) {
    | (Outer, (_, Some(_))) =>
      switch (Zipper.right_neighbor_monotile(siblings)) {
      | Some(c) =>
        c == "??"
          ? {
            let id = Option.get(Indicated.index(z));
            schedule_action(StoreTile(id));
            schedule_action(SendSketch);
          }
          : ()
      | _ => ()
      }
    | (Outer, (_, None)) =>
      switch (Zipper.left_neighbor_monotile(siblings)) {
      | Some(c) =>
        c == "??"
          ? {
            let id = Option.get(Indicated.index(z));
            schedule_action(StoreTile(id));
            schedule_action(SendSketch);
          }
          : ()
      | _ => ()
      }
    | _ => ()
    };
  };

  let update =
      (
        ~settings,
        ~action,
        ~editor: CodeModel.t,
        ~model: Model.t,
        ~schedule_action,
        ~schedule_editor_action,
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
                react(
                  ~settings,
                  ~editor,
                  ~response=content,
                  ~code_suggestion=false,
                ),
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
            let index =
              Option.get(Indicated.index(editor.editor.state.zipper));
            let ci =
              Option.get(Id.Map.find_opt(index, editor.statics.info_map));
            schedule_action(
              ErrorRespond(
                content,
                ci,
                ChatLSP.Options.init.error_rounds_max,
              ),
            );
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
    | ErrorRespond(response, ci, fuel) =>
      let message = code_message_of_str(settings, editor, response, LLM);
      switch (ChatLSP.Prompt.mk_error(ci, response)) {
      | None =>
        print_endline("ERROR ROUNDS (Non-error Response): " ++ response);
        schedule_action(RemoveTile);
      | Some(error) =>
        print_endline("ERROR ROUNDS (Error): " ++ error);
        print_endline("ERROR ROUNDS (Error-causing Response): " ++ response);
        schedule_action(SendError(error, ci, fuel - 1));
      };
      Model.{
        ...model,
        chat: ListUtil.leading(model.chat) @ [message],
        currSender: LS,
      }
      |> Updated.return_quiet;
    | SendError(error, ci, fuel) =>
      let error_message =
        text_message_of_str(
          "Your previous response caused the following error. Please fix it in your response: "
          ++ error,
          LS,
        );
      // check that fuel is not 0
      if (fuel <= 0) {
        schedule_action(
          Respond(
            text_message_of_str("Error round limit reached, stopping", LLM),
          ),
        );
      } else {
        let collected_chat =
          collect_chat(~messages=model.chat @ [error_message]);
        switch (Oracle.ask(collected_chat)) {
        | None => print_endline("Oracle: prompt generation failed")
        | Some(openrouter_prompt) =>
          let llm = model.llm;
          let key = Store.Generic.load("API");
          let params: OpenRouter.params = {llm, temperature: 1.0, top_p: 1.0};
          OpenRouter.start_chat(~params, ~key, openrouter_prompt, req =>
            switch (OpenRouter.handle_chat(req)) {
            | Some({content, _}) =>
              schedule_action(ErrorRespond(content, ci, fuel))
            | None => print_endline("Assistant: response parse failed")
            }
          );
        };
      };
      Model.{
        ...model,
        chat: model.chat @ [error_message, await_llm_response],
        currSender: LLM,
      }
      |> Updated.return_quiet;
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
    | StoreTile(id) => {...model, tile: id} |> Updated.return_quiet
    | RemoveTile =>
      print_endline("Here now");
      // Select Question Marks and double-destruct
      let perform_action: CodeEditable.Update.t =
        Perform(Action.Select(Tile(Id(model.tile, Direction.Left))));
      let cell_action: CellEditor.Update.t = MainEditor(perform_action);
      let scratch_action: EditorsUpdate.t = Scratch(CellAction(cell_action));
      schedule_editor_action(scratch_action);
      let perform_action: CodeEditable.Update.t =
        Perform(Action.Destruct(Direction.Left));
      let cell_action: CellEditor.Update.t = MainEditor(perform_action);
      let scratch_action: EditorsUpdate.t = Scratch(CellAction(cell_action));
      schedule_editor_action(scratch_action);
      {...model, tile: Id.invalid} |> Updated.return_quiet;
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
