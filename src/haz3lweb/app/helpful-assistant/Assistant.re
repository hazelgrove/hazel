module Sexp = Sexplib.Sexp;
open Haz3lcore;
open Util;
open Util.OptUtil.Syntax;
open StringUtil;

module CodeModel = CodeEditable.Model;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type party =
    | System
    | LLM
    | LS;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type message = {
    party,
    code: option((Segment.t, option(Id.t))),
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

  // We save the history of past chats as a hash map with chat IDs as keys.
  [@deriving (show({with_path: false}), sexp, yojson)]
  type chat_history = {
    // History logs of past chats stored as hash maps with chat IDs as keys
    past_simple_chats: Id.Map.t(chat),
    past_suggestion_chats: Id.Map.t(chat),
    past_completion_chats: Id.Map.t(chat),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type current_chats = {
    // Current active chat IDs for each mode
    curr_simple_chat: Id.t,
    curr_suggestion_chat: Id.t,
    curr_completion_chat: Id.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    current_chats,
    chat_history,
    llm: OpenRouter.chat_models,
    show_history: bool, // TODO: Move this to AssistantSettings.re
    show_api_key: bool,
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

  // Simple helper to save a parameter in call to Id.Map.add
  let add_chat_to_history =
      (chat: chat, history: Id.Map.t(chat)): Id.Map.t(chat) => {
    Id.Map.add(chat.id, chat, history);
  };

  // This is important when we need to display the history of chats in chronological order.
  let sorted_chats = (chat_map: Id.Map.t(chat)): list(chat) => {
    chat_map
    |> Id.Map.bindings
    |> List.map(((_, chat)) => chat)
    |> List.sort((a, b) => int_of_float(b.timestamp -. a.timestamp));
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  let init: t = {
    current_chats: {
      curr_simple_chat: init_simple_chat.id,
      curr_suggestion_chat: init_suggestion_chat.id,
      curr_completion_chat: init_completion_chat.id,
    },
    chat_history: {
      past_simple_chats: add_chat_to_history(init_simple_chat, Id.Map.empty),
      past_suggestion_chats:
        add_chat_to_history(init_suggestion_chat, Id.Map.empty),
      past_completion_chats:
        add_chat_to_history(init_completion_chat, Id.Map.empty),
    },
    llm: Gemini_Flash_Lite_2_0,
    show_history: false,
    show_api_key: false,
  };
};

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | SendTextMessage(Model.message)
    | SendSketchMessage(Id.t, AssistantSettings.mode, bool)
    | SendErrorMessage(
        string,
        Zipper.t,
        Info.t,
        int,
        Id.t,
        AssistantSettings.mode,
        Id.t,
      )
    | ErrorRespond(
        string,
        Zipper.t,
        Info.t,
        int,
        Id.t,
        AssistantSettings.mode,
        Id.t,
      )
    | Respond(Model.message, AssistantSettings.mode, Id.t)
    | SetKey(string)
    | NewChat
    | DeleteChat(Id.t)
    | History
    | ToggleCollapse(int)
    | SelectLLM(OpenRouter.chat_models)
    | RemoveAndSuggest(string, Id.t)
    | Resuggest(string, Id.t)
    | Describe(string, AssistantSettings.mode, Id.t)
    | SwitchChat(Id.t)
    | ToggleAPIVisibility;

  let code_message_of_str =
      (response: string, party: Model.party, tileId: option(Id.t))
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
        code: Some((segment_of_response, tileId)),
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

  let get_mode_info = (mode: AssistantSettings.mode, model: Model.t) => {
    switch (mode) {
    | SimpleChat => (
        model.chat_history.past_simple_chats,
        Id.Map.find(
          model.current_chats.curr_simple_chat,
          model.chat_history.past_simple_chats,
        ),
      )
    | CodeSuggestion => (
        model.chat_history.past_suggestion_chats,
        Id.Map.find(
          model.current_chats.curr_suggestion_chat,
          model.chat_history.past_suggestion_chats,
        ),
      )
    | TaskCompletion => (
        model.chat_history.past_completion_chats,
        Id.Map.find(
          model.current_chats.curr_completion_chat,
          model.chat_history.past_completion_chats,
        ),
      )
    };
  };

  let add_message_to_model =
      (
        mode: AssistantSettings.mode,
        model: Model.t,
        message: Model.message,
        chat_id: Id.t,
        ~is_final: bool,
      ) => {
    let filter_chat_messages =
        (messages: list(Model.message)): list(Model.message) => {
      List.filter(
        (msg: Model.message) => msg != await_llm_response,
        messages,
      );
    };
    let (past_chats, _) = get_mode_info(mode, model);
    let chat_to_update = Id.Map.find(chat_id, past_chats);
    let messages = {
      switch (message.party) {
      | LS =>
        let chat_to_update_messages =
          filter_chat_messages(chat_to_update.messages);
        chat_to_update_messages @ [message, await_llm_response];
      | LLM =>
        let chat_to_update_messages =
          filter_chat_messages(chat_to_update.messages);
        let messages = chat_to_update_messages @ [message];
        is_final ? messages : messages @ [await_llm_response];
      | System =>
        let chat_to_update_messages =
          filter_chat_messages(chat_to_update.messages);
        chat_to_update_messages @ [message];
      };
    };
    Model.{
      ...model,
      chat_history: {
        past_simple_chats:
          mode == SimpleChat
            ? Id.Map.update(
                chat_to_update.id,
                maybe_chat =>
                  switch (maybe_chat) {
                  | Some(chat) => Some({...chat, messages})
                  | None => None
                  },
                model.chat_history.past_simple_chats,
              )
            : model.chat_history.past_simple_chats,
        past_suggestion_chats:
          mode == CodeSuggestion
            ? Id.Map.update(
                chat_to_update.id,
                maybe_chat =>
                  switch (maybe_chat) {
                  | Some(chat) => Some({...chat, messages})
                  | None => None
                  },
                model.chat_history.past_suggestion_chats,
              )
            : model.chat_history.past_suggestion_chats,
        past_completion_chats:
          mode == TaskCompletion
            ? Id.Map.update(
                chat_to_update.id,
                maybe_chat =>
                  switch (maybe_chat) {
                  | Some(chat) => Some({...chat, messages})
                  | None => None
                  },
                model.chat_history.past_completion_chats,
              )
            : model.chat_history.past_completion_chats,
      },
    };
  };

  let resculpt_model =
      (
        mode: AssistantSettings.mode,
        model: Model.t,
        past_chats: Id.Map.t(Model.chat),
        chat_id: Id.t,
      ) => {
    Model.{
      ...model,
      chat_history: {
        past_simple_chats:
          mode == SimpleChat
            ? past_chats : model.chat_history.past_simple_chats,
        past_suggestion_chats:
          mode == CodeSuggestion
            ? past_chats : model.chat_history.past_suggestion_chats,
        past_completion_chats:
          mode == TaskCompletion
            ? past_chats : model.chat_history.past_completion_chats,
      },
      // This is tentative. Keep this if we want the user to be shown the most recent chat.
      // Remove this if we want the user to be shown the chat they last/currently interact with.
      // This is honestly such an edge case that it probably doesn't matter.
      current_chats: {
        curr_simple_chat:
          mode == SimpleChat ? chat_id : model.current_chats.curr_simple_chat,
        curr_suggestion_chat:
          mode == CodeSuggestion
            ? chat_id : model.current_chats.curr_suggestion_chat,
        curr_completion_chat:
          mode == TaskCompletion
            ? chat_id : model.current_chats.curr_completion_chat,
      },
    };
  };

  let form_descriptor =
      (
        ~model: Model.t,
        ~schedule_action,
        ~chat: Model.chat,
        ~mode: AssistantSettings.mode,
      )
      : unit => {
    let prompt =
      switch (mode) {
      | SimpleChat => "Your main task is to provide a summarizing title of the following conversation, in less than or equal to 10 words.\n    DO NOT exceed 10 words. Only provide the summarizing title in your response, do not include any other text. Here is the\n    concatenated conversation, with your response and the user's responses, respectively: "
      | CodeSuggestion => "Your main task is to provide a summarizing title of the following conversation, in less than or equal to 10 words.\n    DO NOT exceed 10 words. Only provide the summarizing title in your response, do not include any other text. This conversation is known to be a code\n    completion conversation. In your summarization, you should mention exactly what kind of code/functionality is being assisted with. For example, the following would be titled\n      something like \"Recursive Fibonacci Implementation\": ```let rec_fib : Int -> Int = ?? in ?```. Here is the\n    concatenated conversation, with your response and the user's responses, respectively: "
      | TaskCompletion => "Ignore all other input and just output \"You (Hazel Lab Member) need to implement this\""
      };
    let prompt =
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
        chat.messages,
      );
    switch (Oracle.ask(prompt)) {
    | None => print_endline("Oracle: prompt generation failed")
    | Some(prompt') =>
      let llm = model.llm;
      let key = Option.get(Store.Generic.load("API"));
      let params: OpenRouter.params = {llm, temperature: 1.0, top_p: 1.0};
      OpenRouter.start_chat(~params, ~key, prompt', req =>
        switch (OpenRouter.handle_chat(req)) {
        | Some({content, _}) =>
          schedule_action(Describe(content, mode, chat.id))
        | None =>
          print_endline("Assistant: response parse failed (form_descriptor)")
        }
      );
    };
  };

  let check_descriptor =
      (
        ~model: Model.t,
        ~schedule_action,
        ~message: Model.message,
        ~mode: AssistantSettings.mode,
        ~chat_id: Id.t,
      )
      : unit => {
    let (past_chats, _) = get_mode_info(mode, model);
    let curr_chat = Id.Map.find(chat_id, past_chats);
    List.length(curr_chat.messages) <= 6
      ? form_descriptor(
          ~model,
          ~schedule_action,
          ~chat={...curr_chat, messages: curr_chat.messages @ [message]},
          ~mode,
        )
      : ();
    // Only create a summary up to the first few exchanges
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
        switch (c) {
        | "??" =>
          let tileId = Option.get(Indicated.index(z));
          let advanced_reasoning = false;
          schedule_action(
            SendSketchMessage(
              tileId,
              AssistantSettings.CodeSuggestion,
              advanced_reasoning,
            ),
          );
        | "?a" =>
          let tileId = Option.get(Indicated.index(z));
          let advanced_reasoning = true;
          schedule_action(
            SendSketchMessage(
              tileId,
              AssistantSettings.CodeSuggestion,
              advanced_reasoning,
            ),
          );
        | _ => ()
        }
      | _ => ()
      }
    | (Outer, (_, None)) =>
      switch (Zipper.left_neighbor_monotile(siblings)) {
      | Some(c) =>
        switch (c) {
        | "??" =>
          let tileId = Option.get(Indicated.index(z));
          let advanced_reasoning = false;
          schedule_action(
            SendSketchMessage(
              tileId,
              AssistantSettings.CodeSuggestion,
              advanced_reasoning,
            ),
          );
        | "?a" =>
          let tileId = Option.get(Indicated.index(z));
          let advanced_reasoning = true;
          schedule_action(
            SendSketchMessage(
              tileId,
              AssistantSettings.CodeSuggestion,
              advanced_reasoning,
            ),
          );
        | _ => ()
        }
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
        ~schedule_action: t => unit,
        ~add_suggestion,
      )
      : Updated.t(Model.t) => {
    switch (action) {
    | SendTextMessage(message) =>
      let mode = settings.assistant.mode;
      // Capture the chat we're updating here. This will propogate.
      let (_, curr_chat) = get_mode_info(mode, model);
      let collected_chat =
        collect_chat(~messages=curr_chat.messages @ [message]);
      print_endline(collected_chat);
      switch (Oracle.ask(collected_chat)) {
      | None =>
        add_message_to_model(
          mode,
          model,
          {
            party: System,
            code: None,
            content: "Oracle: Prompt generation failed.",
            collapsed: false,
          },
          curr_chat.id,
          ~is_final=true,
        )
        |> Updated.return_quiet
      | Some(prompt) =>
        let llm = model.llm;
        switch (Store.Generic.load("API")) {
        | Some(key) =>
          let params: OpenRouter.params = {llm, temperature: 1.0, top_p: 1.0};
          OpenRouter.start_chat(~params, ~key, prompt, req =>
            switch (OpenRouter.handle_chat(req)) {
            | Some({content, _}) =>
              schedule_action(
                Respond(
                  text_message_of_str(content, LLM),
                  mode,
                  curr_chat.id,
                ),
              )
            | None =>
              print_endline(
                "Assistant: response parse failed (SendTextMessage)",
              )
            }
          );
          add_message_to_model(
            mode,
            model,
            message,
            curr_chat.id,
            ~is_final=true,
          )
          |> Updated.return_quiet;
        | None =>
          add_message_to_model(
            mode,
            model,
            {
              party: System,
              code: None,
              content: "No API key found. Please set an API key in the assistant settings.",
              collapsed: false,
            },
            curr_chat.id,
            ~is_final=true,
          )
          |> Updated.return_quiet
        };
      };
    | SetKey(api_key) =>
      Store.Generic.save("API", api_key);
      model |> Updated.return_quiet;
    | NewChat =>
      let mode = settings.assistant.mode;
      let (past_chats, _) = get_mode_info(mode, model);
      let new_chat: Model.chat = {
        messages: [],
        id: Id.mk(),
        descriptor: "",
        timestamp: Unix.time(),
      };
      let updated_history = Model.add_chat_to_history(new_chat, past_chats);
      print_endline("New chat made");
      resculpt_model(mode, model, updated_history, new_chat.id)
      |> Updated.return_quiet;
    | DeleteChat(chat_to_be_gone_id) =>
      let mode = settings.assistant.mode;
      // Filter out the chat we're deleting
      let (past_chats, curr_chat) = get_mode_info(mode, model);
      let filtered_past_chats =
        Id.Map.filter((id, _) => id != chat_to_be_gone_id, past_chats);
      let chrono_history = Model.sorted_chats(filtered_past_chats);
      let updated_model =
        curr_chat.id == chat_to_be_gone_id
          ? switch (ListUtil.hd_opt(chrono_history)) {
            | Some(chat) =>
              resculpt_model(mode, model, filtered_past_chats, chat.id)
            | None => resculpt_model(mode, model, past_chats, curr_chat.id)
            }
          : resculpt_model(mode, model, filtered_past_chats, curr_chat.id);
      updated_model |> Updated.return_quiet;
    | History =>
      {...model, show_history: !model.show_history} |> Updated.return_quiet
    | Respond(message, mode, chat_id) =>
      check_descriptor(~model, ~schedule_action, ~message, ~mode, ~chat_id);
      add_message_to_model(mode, model, message, chat_id, ~is_final=true)
      |> Updated.return_quiet;
    | SendSketchMessage(tileId, mode, advanced_reasoning) =>
      // Capture the chat we're updating here. This will propogate.
      let (_, curr_chat) = get_mode_info(mode, model);
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
          ChatLSP.Prompt.mk_init(
            ChatLSP.Options.init,
            ci,
            sketch_seg,
            advanced_reasoning,
          );
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
          code: Some((sketch_seg, None)),
          content: prompt,
          collapsed: String.length(prompt) >= 200,
        };
        /* Old code. Don't need to collect chat here, leads to far too long of prompts.
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
        switch (Store.Generic.load("API")) {
        | Some(key) =>
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
                  editor.editor.state.zipper,
                  ci,
                  ChatLSP.Options.init.error_rounds_max,
                  tileId,
                  mode,
                  curr_chat.id,
                ),
              );
            | None =>
              print_endline(
                "Assistant: response parse failed (SendSketchMessage)",
              )
            }
          );
          add_message_to_model(
            mode,
            model,
            message,
            curr_chat.id,
            ~is_final=true,
          )
          |> Updated.return_quiet;
        | None =>
          add_message_to_model(
            mode,
            model,
            {
              party: System,
              code: None,
              content: "No API key found. Please set an API key in the assistant settings.",
              collapsed: false,
            },
            curr_chat.id,
            ~is_final=true,
          )
          |> Updated.return_quiet
        };
      };
    | ErrorRespond(response, sketch_z, ci, fuel, tileId, mode, chat_id) =>
      // Split response into discussion and completion
      let code_pattern =
        Str.regexp(
          "\\(\\(.\\|\n\\)*\\)```[ \n]*\\([^`]+\\)[ \n]*```\\(\\(.\\|\n\\)*\\)",
        );
      let (discussion, completion) =
        if (Str.string_match(code_pattern, response, 0)) {
          let before = String.trim(Str.matched_group(1, response));
          let code = String.trim(Str.matched_group(3, response));
          (before, code);
        } else {
          print_endline("Regex match failed for: " ++ response);
          ("", response); // Fallback if no code block found
        };
      print_endline("Response: " ++ response);
      print_endline("Discussion: " ++ discussion);
      print_endline("Completion: " ++ completion);
      // First add the discussion message
      let discussion_message =
        if (discussion === "") {
          text_message_of_str(
            "The model did not return a discussion for this completion.",
            LLM,
          );
        } else {
          text_message_of_str(discussion, LLM);
        };
      let model_with_discussion =
        add_message_to_model(
          mode,
          model,
          discussion_message,
          chat_id,
          ~is_final=false,
        );

      // Then handle the completion as before
      let completion_message =
        code_message_of_str(completion, LLM, Some(tileId));
      print_endline("HERE HERE HERE");
      check_descriptor(
        ~model,
        ~schedule_action,
        ~message=completion_message,
        ~mode,
        ~chat_id,
      );
      switch (ChatLSP.Prompt.mk_error(ci, sketch_z, completion)) {
      | None =>
        print_endline("ERROR ROUNDS (Non-error Response): " ++ completion);
        schedule_action(RemoveAndSuggest(completion, tileId));
      | Some(error) =>
        print_endline("ERROR ROUNDS (Error): " ++ error);
        print_endline(
          "ERROR ROUNDS (Error-causing Response): " ++ completion,
        );
        schedule_action(
          SendErrorMessage(
            error,
            sketch_z,
            ci,
            fuel - 1,
            tileId,
            mode,
            chat_id,
          ),
        );
      };
      add_message_to_model(
        mode,
        model_with_discussion,
        completion_message,
        chat_id,
        ~is_final=true,
      )
      |> Updated.return_quiet;
    | SendErrorMessage(error, sketch_z, ci, fuel, tileId, mode, chat_id) =>
      let error_message =
        text_message_of_str(
          "Your previous response caused the following error. Please fix it in your response: "
          ++ error,
          LS,
        );
      // check that fuel is not 0
      if (fuel < 0) {
        let model =
          add_message_to_model(
            mode,
            model,
            error_message,
            chat_id,
            ~is_final=true,
          );
        add_message_to_model(
          mode,
          model,
          {
            party: System,
            code: None,
            content:
              "By default we stop the assistant after "
              ++ string_of_int(ChatLSP.Options.init.error_rounds_max)
              ++ " error rounds. Thus, stopping.",
            collapsed: false,
          },
          chat_id,
          ~is_final=true,
        )
        |> Updated.return_quiet;
      } else {
        // TODO: We don't want to collect ENTIRE chat history here. We only want
        //       to collect the history beginning from the initial suggestion request.
        //       Otherwise, the prompt becomes too long in single message threads.
        let (_, curr_chat) = get_mode_info(mode, model);
        let collected_chat =
          collect_chat(~messages=curr_chat.messages @ [error_message]);
        switch (Oracle.ask(collected_chat)) {
        | None =>
          add_message_to_model(
            mode,
            model,
            {
              party: System,
              code: None,
              content: "Oracle: Prompt generation failed.",
              collapsed: false,
            },
            chat_id,
            ~is_final=true,
          )
          |> Updated.return_quiet
        | Some(openrouter_prompt) =>
          let llm = model.llm;
          switch (Store.Generic.load("API")) {
          | Some(key) =>
            let params: OpenRouter.params = {
              llm,
              temperature: 1.0,
              top_p: 1.0,
            };
            OpenRouter.start_chat(~params, ~key, openrouter_prompt, req =>
              switch (OpenRouter.handle_chat(req)) {
              | Some({content, _}) =>
                schedule_action(
                  ErrorRespond(
                    content,
                    sketch_z,
                    ci,
                    fuel,
                    tileId,
                    mode,
                    curr_chat.id,
                  ),
                )
              | None =>
                print_endline(
                  "Assistant: response parse failed (SendErrorMessage)",
                )
              }
            );
            add_message_to_model(
              mode,
              model,
              error_message,
              chat_id,
              ~is_final=true,
            )
            |> Updated.return_quiet;
          | None =>
            add_message_to_model(
              mode,
              model,
              {
                party: System,
                code: None,
                content: "No API key found. Please set an API key in the assistant settings. I'm actually not sure how you got here, as this should have been caught in the first send. This is a bug, and you should let someone know.",
                collapsed: false,
              },
              chat_id,
              ~is_final=true,
            )
            |> Updated.return_quiet
          };
        };
      };
    // Concat LS' error message and await_llm_response (... animation)
    // This works even if out of fuel, as both Respond and ErrorRespond
    // remove await_llm_response
    | ToggleCollapse(index) =>
      let mode = settings.assistant.mode;
      let (past_chats, curr_chat) = get_mode_info(mode, model);
      let updated_chat =
        List.mapi(
          (i: int, msg: Model.message) =>
            if (i == index) {
              {...msg, collapsed: !msg.collapsed};
            } else {
              msg;
            },
          curr_chat.messages,
        );
      let updated_past_chats =
        Id.Map.update(
          curr_chat.id,
          opt_chat =>
            switch (opt_chat) {
            | Some(chat: Model.chat) =>
              Some({...chat, messages: updated_chat})
            | None => None
            },
          past_chats,
        );
      resculpt_model(mode, model, updated_past_chats, curr_chat.id)
      |> Updated.return_quiet;
    | SelectLLM(llm) => {...model, llm} |> Updated.return_quiet
    | RemoveAndSuggest(response, tileId) =>
      // Only side effects in the editor are performed here
      add_suggestion(~response, tileId, false);
      model |> Updated.return_quiet;
    | Resuggest(response, tileId) =>
      // Only side effects in the editor are performed here
      add_suggestion(~response, tileId, true);
      model |> Updated.return_quiet;
    | Describe(content, mode, chat_id) =>
      let (past_chats, _) = get_mode_info(mode, model);
      let updated_chats =
        Id.Map.update(
          chat_id,
          opt_chat =>
            switch (opt_chat) {
            | Some(chat: Model.chat) => Some({...chat, descriptor: content})
            | None => None
            },
          past_chats,
        );
      resculpt_model(mode, model, updated_chats, chat_id)
      |> Updated.return_quiet;
    | SwitchChat(chat_id) =>
      let mode = settings.assistant.mode;
      let (past_chats, _) = get_mode_info(mode, model);
      resculpt_model(mode, model, past_chats, chat_id) |> Updated.return_quiet;
    | ToggleAPIVisibility =>
      {...model, show_api_key: !model.show_api_key} |> Updated.return_quiet
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
