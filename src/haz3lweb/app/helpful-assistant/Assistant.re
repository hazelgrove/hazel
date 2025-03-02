module Sexp = Sexplib.Sexp;
open Haz3lcore;
open Util;
open Util.OptUtil.Syntax;
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
  type chat = {
    messages: list(message),
    id: Id.t,
    descriptor: string,
    timestamp: float,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type chats = {
    curr_simple_chat: chat,
    curr_suggestion_chat: chat,
    curr_completion_chat: chat,
    // Chats are stored as
    past_simple_chats: list(chat),
    past_suggestion_chats: list(chat),
    past_completion_chats: list(chat),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    chats,
    llm: OpenRouter.chat_models,
    show_history: bool,
  };

  let init_simple_chat = {
    messages: [],
    id: Id.mk(),
    descriptor: "",
    timestamp: Unix.time(),
  };
  let init_suggestion_chat = {
    messages: [],
    id: Id.mk(),
    descriptor: "",
    timestamp: Unix.time(),
  };
  let init_completion_chat = {
    messages: [],
    id: Id.mk(),
    descriptor: "",
    timestamp: Unix.time(),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  let init: t = {
    chats: {
      curr_simple_chat: init_simple_chat,
      curr_suggestion_chat: init_suggestion_chat,
      curr_completion_chat: init_completion_chat,
      past_simple_chats: [init_simple_chat],
      past_suggestion_chats: [init_suggestion_chat],
      past_completion_chats: [init_completion_chat],
    },
    llm: Gemini_Flash_Lite_2_0,
    show_history: false,
  };
};

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | SendMessage(Model.message)
    | SetKey(string)
    | SendSketch(Id.t, AssistantSettings.mode)
    | SendError(string, Info.t, int, Id.t, AssistantSettings.mode)
    | ErrorRespond(string, Info.t, int, Id.t, AssistantSettings.mode)
    | NewChat
    | DeleteChat(Id.t)
    | History
    | Respond(Model.message, AssistantSettings.mode)
    | ToggleCollapse(int)
    | SelectLLM(OpenRouter.chat_models)
    | RemoveAndSuggest(string, Id.t)
    | SwitchMode(AssistantSettings.mode)
    | Describe(string, AssistantSettings.mode, Id.t)
    | SwitchChat(Id.t);

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
    // let string_of_sketch =
    //   Printer.zipper_to_string(editor.editor.state.zipper);
    // let sketch_with_response =
    //   Str.global_replace(Str.regexp("\\?\\?"), response, string_of_sketch);
    let zipper_of_response = Printer.zipper_of_string(response);
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
        ~mode: AssistantSettings.mode,
      )
      : t => {
    // let response = response |> sanitize_response |> quote;
    code_suggestion
      ? Respond(code_message_of_str(settings, editor, response, LLM), mode)
      : Respond(text_message_of_str(response, LLM), mode);
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
    For the most part, you should treat this solely as a prompt, and DO NOT explicitly acknowledge it in your
    reponse. Only use it as a sort of memory. You can, of course, reference prior messages.
    Here is the context: ";
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

  let form_descriptor =
      (
        ~model: Model.t,
        ~settings,
        ~editor,
        ~schedule_action,
        ~chat: list(Model.message),
        ~mode: AssistantSettings.mode,
        ~chat_id: Id.t,
      )
      : unit => {
    let prompt =
      switch (mode) {
      | SimpleChat => "Your main task is to provide a summarizing title of the following conversation, in less than or equal to 10 words.\n    DO NOT exceed 10 words. Only provide the summarizing title in your response, do not include any other text. Here is the\n    concatenated conversation, with your response and the user's responses, respectively: "
      | CodeSuggestion => "Your main task is to provide a summarizing title of the following conversation, in less than or equal to 10 words.\n    DO NOT exceed 10 words. Only provide the summarizing title in your response, do not include any other text. This conversation is known to be a code\n    completion conversation. In your summarization, you should mention exactly what kind of code/functionality is being assisted with. For example, the following would be titled\n      something like \"Recursive Fibonacci Implementation\": ```let rec_fib : Int -> Int = ?? in ?```. Here is the\n    concatenated conversation, with your response and the user's responses, respectively: "
      | TaskCompletion => "Ignore all other input and just output \"You need to implement this\""
      };

    let chat =
      List.fold_left(
        (chat: string, message: Model.message) =>
          if (message.party == LLM) {
            chat ++ "Your Reponse: " ++ message.content ++ " ";
          } else if (message.party == LS) {
            chat ++ "User Input: " ++ message.content ++ " ";
          } else {
            chat ++ message.content;
          },
        prompt,
        chat,
      );
    switch (Oracle.ask(chat)) {
    | None => print_endline("Oracle: prompt generation failed")
    | Some(prompt) =>
      let llm = model.llm;
      let key = Store.Generic.load("API");
      let params: OpenRouter.params = {llm, temperature: 1.0, top_p: 1.0};
      OpenRouter.start_chat(~params, ~key, prompt, req =>
        switch (OpenRouter.handle_chat(req)) {
        | Some({content, _}) =>
          schedule_action(Describe(content, mode, chat_id))
        | None => print_endline("Assistant: response parse failed")
        }
      );
    };
  };

  let check_descriptor =
      (
        ~model: Model.t,
        ~settings: AssistantSettings.t,
        ~editor: CodeModel.t,
        ~schedule_action,
        ~message: Model.message,
        ~mode: AssistantSettings.mode,
      )
      : unit => {
    // Only create a summary up to the first 3 exchanges
    switch (mode) {
    | SimpleChat =>
      List.length(model.chats.past_simple_chats) <= 6
        ? form_descriptor(
            ~model,
            ~settings,
            ~editor,
            ~schedule_action,
            ~chat=model.chats.curr_simple_chat.messages @ [message],
            ~mode,
            ~chat_id=model.chats.curr_simple_chat.id,
          )
        : ()
    | CodeSuggestion =>
      List.length(model.chats.past_suggestion_chats) <= 6
        ? form_descriptor(
            ~model,
            ~settings,
            ~editor,
            ~schedule_action,
            ~chat=model.chats.curr_suggestion_chat.messages @ [message],
            ~mode,
            ~chat_id=model.chats.curr_suggestion_chat.id,
          )
        : ()
    | TaskCompletion =>
      List.length(model.chats.past_completion_chats) <= 6
        ? form_descriptor(
            ~model,
            ~settings,
            ~editor,
            ~schedule_action,
            ~chat=model.chats.curr_completion_chat.messages @ [message],
            ~mode,
            ~chat_id=model.chats.curr_completion_chat.id,
          )
        : ()
    };
  };

  let check_req =
      (_: string, schedule_action: t => unit, editor: CodeEditable.Model.t)
      : unit => {
    let z = editor.editor.state.zipper;
    let caret = z.caret;
    let siblings = z.relatives.siblings;

    /*
     // Check if cursor is in a hole
     print_endline("Checking cursor position...");
     print_endline("Caret: " ++ (caret == Outer ? "Outer" : "Inner"));
     switch (Indicated.ci_of(z, editor.statics.info_map)) {
     | Some(ci) =>
       print_endline("Found cursor info");
       switch (ci) {
       | Info.InfoExp({term: {ids: _, copied: _, term: EmptyHole}, _}) =>
         print_endline("Found empty hole");
         switch (Indicated.index(z)) {
         | Some(index) => schedule_action(SendSketch(index))
         | None => print_endline("No index found for hole")
         };
       | _ => ()
       };
     | None => ()
     };
     */

    // Check if user just typed ??
    switch (caret, Zipper.neighbor_monotiles(siblings)) {
    | (Outer, (_, Some(_))) =>
      switch (Zipper.right_neighbor_monotile(siblings)) {
      | Some(c) =>
        c == "??"
          ? {
            let tileId = Option.get(Indicated.index(z));
            schedule_action(
              SendSketch(tileId, AssistantSettings.CodeSuggestion),
            );
          }
          : ()
      | _ => ()
      }
    | (Outer, (_, None)) =>
      switch (Zipper.left_neighbor_monotile(siblings)) {
      | Some(c) =>
        c == "??"
          ? {
            let tileId = Option.get(Indicated.index(z));
            schedule_action(
              SendSketch(tileId, AssistantSettings.CodeSuggestion),
            );
          }
          : ()
      | _ => ()
      }
    | _ => ()
    };
  };

  let set_buffer = (~response: string, z: Zipper.t): option(Zipper.t) => {
    let zipper_of_response = Option.get(Printer.zipper_of_string(response));
    let seg_of_response =
      Zipper.smart_seg(
        ~dump_backpack=true,
        ~erase_buffer=true,
        zipper_of_response,
      );
    let z = Zipper.set_buffer(z, ~content=seg_of_response, ~mode=Unparsed);
    Some(z);
  };

  let update =
      (
        ~settings: Settings.t,
        ~action,
        ~editor: CodeModel.t,
        ~model: Model.t,
        ~schedule_action,
        ~add_suggestion,
      )
      : Updated.t(Model.t) => {
    switch (action) {
    | SendMessage(message) =>
      let mode = settings.assistant.mode;
      let collected_chat =
        switch (mode) {
        | SimpleChat =>
          collect_chat(
            ~messages=model.chats.curr_simple_chat.messages @ [message],
          )
        | CodeSuggestion =>
          collect_chat(
            ~messages=model.chats.curr_suggestion_chat.messages @ [message],
          )
        | TaskCompletion =>
          collect_chat(
            ~messages=model.chats.curr_completion_chat.messages @ [message],
          )
        };
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
                ~mode,
              ),
            )
          | None => print_endline("Assistant: response parse failed")
          }
        );
      };
      Model.{
        ...model,
        chats: {
          ...model.chats,
          curr_simple_chat: {
            ...model.chats.curr_simple_chat,
            messages:
              mode == SimpleChat
                ? model.chats.curr_simple_chat.messages
                  @ [message, await_llm_response]
                : model.chats.curr_simple_chat.messages,
          },
          curr_suggestion_chat: {
            ...model.chats.curr_suggestion_chat,
            messages:
              mode == CodeSuggestion
                ? model.chats.curr_suggestion_chat.messages
                  @ [message, await_llm_response]
                : model.chats.curr_suggestion_chat.messages,
          },
          curr_completion_chat: {
            ...model.chats.curr_completion_chat,
            messages:
              mode == TaskCompletion
                ? model.chats.curr_completion_chat.messages
                  @ [message, await_llm_response]
                : model.chats.curr_completion_chat.messages,
          },
        },
      }
      |> Updated.return_quiet;
    | SetKey(api_key) =>
      Store.Generic.save("API", api_key);
      model |> Updated.return_quiet;
    | NewChat =>
      let mode = settings.assistant.mode;
      let new_chat: Model.chat = {
        messages: [],
        id: Id.mk(),
        descriptor: "",
        timestamp: Unix.time(),
      };
      switch (mode) {
      | SimpleChat =>
        Model.{
          ...model,
          chats: {
            ...model.chats,
            curr_simple_chat: new_chat,
            past_simple_chats: model.chats.past_simple_chats @ [new_chat],
          },
        }
        |> Updated.return_quiet
      | CodeSuggestion =>
        Model.{
          ...model,
          chats: {
            ...model.chats,
            curr_suggestion_chat: new_chat,
            past_suggestion_chats:
              model.chats.past_suggestion_chats @ [new_chat],
          },
        }
        |> Updated.return_quiet
      | TaskCompletion =>
        Model.{
          ...model,
          chats: {
            ...model.chats,
            curr_completion_chat: new_chat,
            past_completion_chats:
              model.chats.past_completion_chats @ [new_chat],
          },
        }
        |> Updated.return_quiet
      };
    | DeleteChat(chat_to_be_gone_id) =>
      let mode = settings.assistant.mode;
      // Filter out the chat we're deleting
      let updated_past_chats =
        switch (mode) {
        | SimpleChat => {
            ...model.chats,
            past_simple_chats:
              switch (ListUtil.last_opt(model.chats.past_simple_chats)) {
              | Some(_) =>
                List.filter_map(
                  (chat: Model.chat) =>
                    chat.id == chat_to_be_gone_id ? None : Some(chat),
                  model.chats.past_simple_chats,
                )
              | None => model.chats.past_simple_chats
              },
          }
        | CodeSuggestion => {
            ...model.chats,
            past_suggestion_chats:
              switch (ListUtil.last_opt(model.chats.past_suggestion_chats)) {
              | Some(_) =>
                List.filter_map(
                  (chat: Model.chat) =>
                    chat.id == chat_to_be_gone_id ? None : Some(chat),
                  model.chats.past_suggestion_chats,
                )
              | None => model.chats.past_suggestion_chats
              },
          }
        | TaskCompletion => {
            ...model.chats,
            past_completion_chats:
              switch (ListUtil.last_opt(model.chats.past_completion_chats)) {
              | Some(_) =>
                List.filter_map(
                  (chat: Model.chat) =>
                    chat.id == chat_to_be_gone_id ? None : Some(chat),
                  model.chats.past_completion_chats,
                )
              | None => model.chats.past_completion_chats
              },
          }
        };
      // Update the current chat we're on (in case it's the one we're deleting)
      let final_chats =
        switch (mode) {
        | SimpleChat => {
            ...updated_past_chats,
            curr_simple_chat:
              model.chats.curr_simple_chat.id == chat_to_be_gone_id
                ? switch (ListUtil.last_opt(model.chats.past_simple_chats)) {
                  | Some(last_chat) => last_chat
                  | None => model.chats.curr_simple_chat
                  }
                : model.chats.curr_simple_chat,
          }
        | CodeSuggestion => {
            ...updated_past_chats,
            curr_suggestion_chat:
              model.chats.curr_suggestion_chat.id == chat_to_be_gone_id
                ? switch (
                    ListUtil.last_opt(model.chats.past_suggestion_chats)
                  ) {
                  | Some(last_chat) => last_chat
                  | None => model.chats.curr_suggestion_chat
                  }
                : model.chats.curr_suggestion_chat,
          }
        | TaskCompletion => {
            ...updated_past_chats,
            curr_completion_chat:
              model.chats.curr_completion_chat.id == chat_to_be_gone_id
                ? switch (
                    ListUtil.last_opt(model.chats.past_completion_chats)
                  ) {
                  | Some(last_chat) => last_chat
                  | None => model.chats.curr_completion_chat
                  }
                : model.chats.curr_completion_chat,
          }
        };

      {...model, chats: final_chats} |> Updated.return_quiet;
    | History =>
      {...model, show_history: !model.show_history} |> Updated.return_quiet
    | Respond(message, mode) =>
      check_descriptor(
        ~model,
        ~settings=settings.assistant,
        ~editor,
        ~schedule_action,
        ~message,
        ~mode,
      );
      Model.{
        ...model,
        chats: {
          ...model.chats,
          curr_simple_chat: {
            ...model.chats.curr_simple_chat,
            messages:
              mode == SimpleChat
                ? ListUtil.leading(model.chats.curr_simple_chat.messages)
                  @ [message]
                : model.chats.curr_simple_chat.messages,
          },
          curr_suggestion_chat: {
            ...model.chats.curr_suggestion_chat,
            messages:
              mode == CodeSuggestion
                ? ListUtil.leading(model.chats.curr_suggestion_chat.messages)
                  @ [message]
                : model.chats.curr_suggestion_chat.messages,
          },
          curr_completion_chat: {
            ...model.chats.curr_completion_chat,
            messages:
              mode == TaskCompletion
                ? ListUtil.leading(model.chats.curr_completion_chat.messages)
                  @ [message]
                : model.chats.curr_completion_chat.messages,
          },
        },
      }
      |> Updated.return_quiet;
    | SendSketch(tileId, mode) =>
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
        model |> Updated.return_quiet;
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
        /* Old code. Don't need to collect chat here, leads to far too long of prompt.
           let collected_chat =
             switch (mode) {
             | CodeSuggestion =>
               collect_chat(
                 ~messages=model.chats.curr_suggestion_chat.messages @ [message],
               )
             | TaskCompletion =>
               collect_chat(
                 ~messages=model.chats.curr_completion_chat.messages @ [message],
               )
             | _ =>
               print_endline(
                 "Invalid mode. Cannot perform code completion in chat mode.",
               );
               "";
             };
           */
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
                tileId,
                mode,
              ),
            );
          | None => print_endline("Assistant: response parse failed")
          }
        );
        Model.{
          ...model,
          chats: {
            ...model.chats,
            curr_suggestion_chat: {
              ...model.chats.curr_suggestion_chat,
              messages:
                mode == CodeSuggestion
                  ? model.chats.curr_suggestion_chat.messages
                    @ [message, await_llm_response]
                  : model.chats.curr_suggestion_chat.messages,
            },
            curr_completion_chat: {
              ...model.chats.curr_completion_chat,
              messages:
                mode == TaskCompletion
                  ? model.chats.curr_completion_chat.messages
                    @ [message, await_llm_response]
                  : model.chats.curr_completion_chat.messages,
            },
          },
        }
        |> Updated.return_quiet;
      };
    | ErrorRespond(response, ci, fuel, tileId, mode) =>
      let message = code_message_of_str(settings, editor, response, LLM);
      switch (ChatLSP.Prompt.mk_error(ci, response)) {
      | None =>
        // No error, all good. Concat and return suggestion.
        print_endline("ERROR ROUNDS (Non-error Response): " ++ response);
        check_descriptor(
          ~model,
          ~settings=settings.assistant,
          ~editor,
          ~schedule_action,
          ~message,
          ~mode,
        );
        schedule_action(RemoveAndSuggest(response, tileId));
      | Some(error) =>
        // If there is some error, perform an error round
        print_endline("ERROR ROUNDS (Error): " ++ error);
        print_endline("ERROR ROUNDS (Error-causing Response): " ++ response);
        schedule_action(SendError(error, ci, fuel - 1, tileId, mode));
      };
      // Remove await_llm_response (... animation) and concat LLM's suggestion
      Model.{
        ...model,
        chats: {
          ...model.chats,
          curr_simple_chat: {
            ...model.chats.curr_simple_chat,
            messages:
              mode == SimpleChat
                ? ListUtil.leading(model.chats.curr_simple_chat.messages)
                  @ [message]
                : model.chats.curr_simple_chat.messages,
          },
          curr_suggestion_chat: {
            ...model.chats.curr_suggestion_chat,
            messages:
              mode == CodeSuggestion
                ? ListUtil.leading(model.chats.curr_suggestion_chat.messages)
                  @ [message]
                : model.chats.curr_suggestion_chat.messages,
          },
          curr_completion_chat: {
            ...model.chats.curr_completion_chat,
            messages:
              mode == TaskCompletion
                ? ListUtil.leading(model.chats.curr_completion_chat.messages)
                  @ [message]
                : model.chats.curr_completion_chat.messages,
          },
        },
      }
      |> Updated.return_quiet;
    | SendError(error, ci, fuel, tileId, mode) =>
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
            mode,
          ),
        );
      } else {
        // TODO: We don't want to collect ENTIRE chat history here. We only want
        //       to collect the history beginning from the initial suggestion request.
        //       Otherwise, the prompt becomes too long in single message threads.
        let collected_chat =
          switch (mode) {
          | SimpleChat =>
            collect_chat(
              ~messages=
                model.chats.curr_simple_chat.messages @ [error_message],
            )
          | CodeSuggestion =>
            collect_chat(
              ~messages=
                model.chats.curr_suggestion_chat.messages @ [error_message],
            )
          | TaskCompletion =>
            collect_chat(
              ~messages=
                model.chats.curr_completion_chat.messages @ [error_message],
            )
          };
        switch (Oracle.ask(collected_chat)) {
        | None => print_endline("Oracle: prompt generation failed")
        | Some(openrouter_prompt) =>
          let llm = model.llm;
          let key = Store.Generic.load("API");
          let params: OpenRouter.params = {llm, temperature: 1.0, top_p: 1.0};
          OpenRouter.start_chat(~params, ~key, openrouter_prompt, req =>
            switch (OpenRouter.handle_chat(req)) {
            | Some({content, _}) =>
              schedule_action(ErrorRespond(content, ci, fuel, tileId, mode))
            | None => print_endline("Assistant: response parse failed")
            }
          );
        };
      };
      // Concat LS' error message and await_llm_response (... animation)
      // This works even if out of fuel, as both Respond and ErrorRespond
      // remove await_llm_response
      Model.{
        ...model,
        chats: {
          ...model.chats,
          curr_simple_chat: {
            ...model.chats.curr_simple_chat,
            messages:
              mode == SimpleChat
                ? model.chats.curr_simple_chat.messages
                  @ [error_message, await_llm_response]
                : model.chats.curr_simple_chat.messages,
          },
          curr_suggestion_chat: {
            ...model.chats.curr_suggestion_chat,
            messages:
              mode == CodeSuggestion
                ? model.chats.curr_suggestion_chat.messages
                  @ [error_message, await_llm_response]
                : model.chats.curr_suggestion_chat.messages,
          },
          curr_completion_chat: {
            ...model.chats.curr_completion_chat,
            messages:
              mode == TaskCompletion
                ? model.chats.curr_completion_chat.messages
                  @ [error_message, await_llm_response]
                : model.chats.curr_completion_chat.messages,
          },
        },
      }
      |> Updated.return_quiet;
    | ToggleCollapse(index) =>
      let mode = settings.assistant.mode;
      let updated_chat =
        List.mapi(
          (i: int, msg: Model.message) =>
            if (i == index) {
              {...msg, collapsed: !msg.collapsed};
            } else {
              msg;
            },
          switch (mode) {
          | SimpleChat => model.chats.curr_simple_chat.messages
          | CodeSuggestion => model.chats.curr_suggestion_chat.messages
          | TaskCompletion => model.chats.curr_completion_chat.messages
          },
        );
      Model.{
        ...model,
        chats: {
          ...model.chats,
          curr_simple_chat: {
            ...model.chats.curr_simple_chat,
            messages:
              mode == SimpleChat
                ? updated_chat : model.chats.curr_simple_chat.messages,
          },
          curr_suggestion_chat: {
            ...model.chats.curr_suggestion_chat,
            messages:
              mode == CodeSuggestion
                ? updated_chat : model.chats.curr_suggestion_chat.messages,
          },
          curr_completion_chat: {
            ...model.chats.curr_completion_chat,
            messages:
              mode == TaskCompletion
                ? updated_chat : model.chats.curr_completion_chat.messages,
          },
        },
      }
      |> Updated.return_quiet;
    | SelectLLM(llm) => {...model, llm} |> Updated.return_quiet
    | RemoveAndSuggest(response, tileId) =>
      // Only side effects in the editor are performed here
      add_suggestion(~response, tileId);
      model |> Updated.return_quiet;
    | SwitchMode(mode) =>
      {...model, show_history: false} |> Updated.return_quiet
    | Describe(content, mode, chat_id) =>
      let updated_chats =
        switch (mode) {
        | SimpleChat =>
          // Only update the descriptor of the specific chat with matching ID
          {
            ...model.chats,
            curr_simple_chat:
              model.chats.curr_simple_chat.id == chat_id
                ? {...model.chats.curr_simple_chat, descriptor: content}
                : model.chats.curr_simple_chat,
            past_simple_chats:
              List.map(
                (c: Model.chat) =>
                  c.id == chat_id ? {...c, descriptor: content} : c,
                model.chats.past_simple_chats,
              ),
          }
        | CodeSuggestion => {
            ...model.chats,
            curr_suggestion_chat:
              model.chats.curr_suggestion_chat.id == chat_id
                ? {...model.chats.curr_suggestion_chat, descriptor: content}
                : model.chats.curr_suggestion_chat,
            past_suggestion_chats:
              List.map(
                (c: Model.chat) =>
                  c.id == chat_id ? {...c, descriptor: content} : c,
                model.chats.past_suggestion_chats,
              ),
          }
        | TaskCompletion => {
            ...model.chats,
            curr_completion_chat:
              model.chats.curr_completion_chat.id == chat_id
                ? {...model.chats.curr_completion_chat, descriptor: content}
                : model.chats.curr_completion_chat,
            past_completion_chats:
              List.map(
                (c: Model.chat) =>
                  c.id == chat_id ? {...c, descriptor: content} : c,
                model.chats.past_completion_chats,
              ),
          }
        };

      {...model, chats: updated_chats} |> Updated.return_quiet;
    | SwitchChat(chat_id) =>
      let mode = settings.assistant.mode;
      let find_by_id =
          (chats: Model.chats, id: Id.t, ~get_id: Model.chat => Id.t) => {
        switch (mode) {
        | SimpleChat =>
          List.find_opt(item => get_id(item) == id, chats.past_simple_chats)
        | CodeSuggestion =>
          List.find_opt(
            item => get_id(item) == id,
            chats.past_suggestion_chats,
          )
        | TaskCompletion =>
          List.find_opt(
            item => get_id(item) == id,
            chats.past_completion_chats,
          )
        };
      };

      // Store current chat back into past_chats list
      let updated_past_chats =
        switch (mode) {
        | SimpleChat => {
            ...model.chats,
            past_simple_chats:
              List.map(
                (chat: Model.chat) =>
                  chat.id == model.chats.curr_simple_chat.id
                    ? model.chats.curr_simple_chat : chat,
                model.chats.past_simple_chats,
              ),
          }
        | CodeSuggestion => {
            ...model.chats,
            past_suggestion_chats:
              List.map(
                (chat: Model.chat) =>
                  chat.id == model.chats.curr_suggestion_chat.id
                    ? model.chats.curr_suggestion_chat : chat,
                model.chats.past_suggestion_chats,
              ),
          }
        | TaskCompletion => {
            ...model.chats,
            past_completion_chats:
              List.map(
                (chat: Model.chat) =>
                  chat.id == model.chats.curr_completion_chat.id
                    ? model.chats.curr_completion_chat : chat,
                model.chats.past_completion_chats,
              ),
          }
        };

      // Get the chat we're switching to
      let curr_chat =
        Option.get(
          find_by_id(updated_past_chats, chat_id, ~get_id=chat => chat.id),
        );

      // Now update the current chat
      let final_chats =
        switch (mode) {
        | SimpleChat => {...updated_past_chats, curr_simple_chat: curr_chat}
        | CodeSuggestion => {
            ...updated_past_chats,
            curr_suggestion_chat: curr_chat,
          }
        | TaskCompletion => {
            ...updated_past_chats,
            curr_completion_chat: curr_chat,
          }
        };

      {...model, chats: final_chats} |> Updated.return_quiet;
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
