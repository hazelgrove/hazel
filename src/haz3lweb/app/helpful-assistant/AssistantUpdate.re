module Sexp = Sexplib.Sexp;
open Haz3lcore;
open Util;
open Util.OptUtil.Syntax;
open API;

module CodeModel = CodeEditable.Model;

module Model = AssistantModel;

[@deriving (show({with_path: false}), sexp, yojson)]
type completion =
  | Request(Id.t, bool) // When user presses ?? or ?a
  | Query(string) // User may followup with a query
  | Loop(string, Id.t, int); // Error rounds

[@deriving (show({with_path: false}), sexp, yojson)]
type composition =
  | Request(string) // User-submitted task, question, etc
  | Loop(int); // Iterative tool completion loop

// Actions to send various kinds of messages to the LLM
[@deriving (show({with_path: false}), sexp, yojson)]
type send_message =
  | Tutor(string)
  | Completion(completion)
  | Composition(composition);

// Actions to handle certain kinds of LLM responses
[@deriving (show({with_path: false}), sexp, yojson)]
type handle_response =
  | Tutor
  | CompletionErrorRound(CodeModel.t, int, Id.t)
  | CompletionQueryResponse
  | CompositionLoopRound(CodeModel.t, int);

// Actions which actualize actions via LLM responses
[@deriving (show({with_path: false}), sexp, yojson)]
type employ_llm_action =
  | RemoveAndSuggest(string, Id.t)
  | Resuggest(string, Id.t)
  | Describe(string, AssistantSettings.mode, Id.t);

// Future Todo: (Check whether) These might be able to be relocated to AssistantSettings
//              Although, arguably, the chat is inherently part of the assistant model,
//              serving as a sort of memory.
// Actions that are related to the chat history and/or display of chat messages
[@deriving (show({with_path: false}), sexp, yojson)]
type chat_action =
  | NewChat
  | DeleteChat(Id.t)
  | SwitchChat(Id.t)
  | CollapseMessage(int)
  | FilterLoadingMessages;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | SendMessage(send_message, CodeModel.t, Id.t)
  | HandleResponse(handle_response, string, Id.t)
  | EmployLLMAction(employ_llm_action)
  | ChatAction(chat_action)
  | InternalError(string, AssistantSettings.mode, Id.t);

let parse_blocks = (response: string): list(Model.block_kind) => {
  let rec parse_blocks =
          (str: string, acc: list(Model.block_kind)): list(Model.block_kind) => {
    let pattern = Str.regexp("```[ \n]*\\([^`]+\\)[ \n]*```");
    switch (Str.search_forward(pattern, str, 0)) {
    | exception Not_found => acc
    | pos =>
      let acc = ListUtil.leading(acc);
      let code = Str.matched_group(1, str);
      let zipper_of_code = Printer.zipper_of_string(code);
      let sketch =
        switch (zipper_of_code) {
        | Some(z) => Zipper.seg_for_view(z)
        | None =>
          print_endline("Failed to parse content into segment.\n");
          Zipper.seg_for_view(Zipper.init());
        };
      let before = Str.string_before(str, pos);
      let rest_start = pos + String.length(Str.matched_string(str));
      if (rest_start >= String.length(str)) {
        acc @ [Text(before), Code(sketch)];
      } else {
        let rest = Str.string_after(str, rest_start);
        parse_blocks(
          rest,
          acc @ [Text(before), Code(sketch), Text(rest)],
        );
      };
    };
  };
  parse_blocks(response, [Text(response)]);
};

let mk_message_display = (~content: string, ~role: Model.role): Model.display => {
  {
    displayable_content: parse_blocks(content),
    original_content: content,
    role,
    collapsed:
      String.length(content) > Model.max_collapsed_length
      || role == System(AssistantPrompt),
  };
};

let extract_tool_calls = (response: string): list(string) => {
  let rec extract_tool_calls =
          (text: string, acc: list(string)): list(string) => {
    let pattern = Str.regexp("~~~[ \n]*\\([^~]+\\)[ \n]*~~~");
    switch (Str.search_forward(pattern, text, 0)) {
    | exception Not_found => List.rev(acc)
    | pos =>
      let matched = Str.matched_group(1, text);
      let rest =
        String.sub(
          text,
          pos + String.length(Str.matched_string(text)),
          String.length(text)
          - (pos + String.length(Str.matched_string(text))),
        );
      extract_tool_calls(rest, [matched, ...acc]);
    };
  };
  extract_tool_calls(response, []);
};

let add_chat_to_history =
    (chat: Model.chat, history: Id.Map.t(Model.chat)): Id.Map.t(Model.chat) => {
  Id.Map.add(chat.id, chat, history);
};

let get_mode_info = (mode: AssistantSettings.mode, model: Model.t) => {
  switch (mode) {
  | HazelTutor => (
      model.chat_history.past_tutor_chats,
      Id.Map.find(
        model.current_chats.curr_tutor_chat,
        model.chat_history.past_tutor_chats,
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
      model.chat_history.past_composition_chats,
      Id.Map.find(
        model.current_chats.curr_composition_chat,
        model.chat_history.past_composition_chats,
      ),
    )
  };
};

/*
 let await_llm_response: Model.display = {
   displayable_content: [Text("...")],
   original_content: "...",
   role: Assistant,
   collapsed: false,
 };
 */

let filter_chat_messages =
    (message_displays: list(Model.display)): list(Model.display) => {
  List.filter(
    (message_display: Model.display) => {
      !(
        message_display.role == Assistant
        && message_display.displayable_content == [Text("...")]
        && !message_display.collapsed
      )
    },
    message_displays,
  );
};

let resculpt_model =
    (
      ~model: Model.t,
      ~mode: AssistantSettings.mode,
      ~updated_past_chats: Id.Map.t(Model.chat),
      ~chat_id: Id.t,
    ) => {
  Model.{
    chat_history: {
      past_tutor_chats:
        mode == HazelTutor
          ? updated_past_chats : model.chat_history.past_tutor_chats,
      past_suggestion_chats:
        mode == CodeSuggestion
          ? updated_past_chats : model.chat_history.past_suggestion_chats,
      past_composition_chats:
        mode == TaskCompletion
          ? updated_past_chats : model.chat_history.past_composition_chats,
    },
    // This is tentative. Keep this if we want the user to be shown the most recent chat.
    // Remove this if we want the user to be shown the chat they last/currently interact with.
    // This is honestly such an edge case that it probably doesn't matter.
    current_chats: {
      curr_tutor_chat:
        mode == HazelTutor ? chat_id : model.current_chats.curr_tutor_chat,
      curr_suggestion_chat:
        mode == CodeSuggestion
          ? chat_id : model.current_chats.curr_suggestion_chat,
      curr_composition_chat:
        mode == TaskCompletion
          ? chat_id : model.current_chats.curr_composition_chat,
    },
  };
};

let update_model_chat_history =
    (
      ~model: Model.t,
      ~mode: AssistantSettings.mode,
      ~updated_chat: Model.chat,
    )
    : Model.t => {
  let new_chat =
    switch (mode) {
    | HazelTutor =>
      Id.Map.update(
        updated_chat.id,
        maybe_chat =>
          switch (maybe_chat) {
          | Some(_) => Some(updated_chat)
          | None => None
          },
        model.chat_history.past_tutor_chats,
      )
    | CodeSuggestion =>
      Id.Map.update(
        updated_chat.id,
        maybe_chat =>
          switch (maybe_chat) {
          | Some(_) => Some(updated_chat)
          | None => None
          },
        model.chat_history.past_suggestion_chats,
      )
    | TaskCompletion =>
      Id.Map.update(
        updated_chat.id,
        maybe_chat =>
          switch (maybe_chat) {
          | Some(_) => Some(updated_chat)
          | None => None
          },
        model.chat_history.past_composition_chats,
      )
    };
  let updated_chat_history =
    switch (mode) {
    | HazelTutor => {
        ...model.chat_history,
        past_tutor_chats: new_chat,
      }
    | CodeSuggestion => {
        ...model.chat_history,
        past_suggestion_chats: new_chat,
      }
    | TaskCompletion => {
        ...model.chat_history,
        past_composition_chats: new_chat,
      }
    };
  {
    ...model,
    chat_history: updated_chat_history,
  };
};

let create_chat_descriptor =
    (
      ~model: Model.t,
      ~schedule_action,
      ~mode: AssistantSettings.mode,
      ~chat_id: Id.t,
    )
    : unit => {
  let (past_chats, _) = get_mode_info(mode, model);
  let curr_chat = Id.Map.find(chat_id, past_chats);

  let this_prompt =
    String.concat(
      "\n",
      [
        "You are a helpful assistant that *summarizes* conversations between other assistants and users. ",
        "Your summaries should be less than or equal to 7 words, and may include 1 or 2 emojis, if appropriate. ",
        "NEVER exceed 7 words. ",
        "ONLY provide the summarizing title in your response, do NOT include any other text. ",
        "You will be given a conversation between an assistant and a user. ",
        "Focus on the giving a summarizing topic title to the conversation between the assistant and the user. ",
        "NEVER use first person pronouns in your response. ",
      ],
    );

  let filtered_messages =
    List.filter(
      (message: Model.display) => {
        message.role == User || message.role == Assistant
      },
      curr_chat.message_displays,
    );

  let combined_messages =
    String.concat(
      "\n",
      List.map(
        (message: Model.display) => {
          "<"
          ++ Model.string_of_role(message.role)
          ++ ">"
          ++ message.original_content
          ++ "</"
          ++ Model.string_of_role(message.role)
          ++ ">"
        },
        filtered_messages,
      ),
    );

  let outgoing_messages_for_descriptor = [
    OpenRouter.mk_system_msg(this_prompt),
    OpenRouter.mk_user_msg(combined_messages),
  ];

  // Only make descriptor after first few exchanges
  List.length(filtered_messages) <= AssistantSettings.make_descriptor_max
    ? try({
        let model_id = Option.get(Store.Generic.load("MODEL"));
        let key = Option.get(Store.Generic.load("API"));
        let params: OpenRouter.params = {
          ...OpenRouter.default_params,
          model_id,
          stream: false,
        };
        OpenRouter.start_chat(
          ~params,
          ~key,
          ~outgoing_messages=outgoing_messages_for_descriptor,
          req =>
          switch (OpenRouter.handle_chat(req)) {
          | Some(Reply({content, _})) =>
            schedule_action(
              EmployLLMAction(Describe(content, mode, chat_id)),
            )
          | Some(Error(_)) =>
            raise(
              Invalid_argument(
                "Error in receiving response from OpenRouter when creating descriptor",
              ),
            )
          | None => ()
          }
        );
      }) {
      | Invalid_argument(e) =>
        print_endline("Invalid_argument when creating descriptor: " ++ e);
        ();
      }
    : ();
};

let check_req =
    (
      _: string,
      schedule_action: t => unit,
      editor: CodeEditable.Model.t,
      chat_id: Id.t,
    )
    : unit => {
  let z = editor.editor.state.zipper;
  let caret = z.caret;
  let siblings = z.relatives.siblings;
  let send_message = (tile_id, advanced_reasoning) => {
    schedule_action(
      SendMessage(
        Completion(Request(tile_id, advanced_reasoning)),
        editor,
        chat_id,
      ),
    );
  };

  // Check if user just typed ??
  switch (caret, Zipper.neighbor_monotiles(siblings)) {
  | (Outer, (_, Some(_))) =>
    switch (Zipper.right_neighbor_monotile(siblings)) {
    | Some(c) =>
      switch (c) {
      | "??" =>
        let tileId = Option.get(Indicated.index(z));
        let advanced_reasoning = false;
        send_message(tileId, advanced_reasoning);
      | "?a" =>
        let tileId = Option.get(Indicated.index(z));
        let advanced_reasoning = true;
        send_message(tileId, advanced_reasoning);
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
        send_message(tileId, advanced_reasoning);
      | "?a" =>
        let tileId = Option.get(Indicated.index(z));
        let advanced_reasoning = true;
        send_message(tileId, advanced_reasoning);
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

// Sends a request to OpenRouter given outgoing messages.
// Handles the response from OpenRouter.
// Emits internal error if API key or model ID is not set.
let mk_llm_call =
    (
      ~mode: AssistantSettings.mode,
      ~schedule_action: t => unit,
      ~updated_chat: Model.chat,
      ~response_handler: string => t,
    )
    : unit => {
  switch (Store.Generic.load("API"), Store.Generic.load("MODEL")) {
  | (Some(key), Some(model_id)) =>
    let params: OpenRouter.params = {
      ...OpenRouter.default_params,
      model_id,
    };
    try(
      OpenRouter.start_chat(
        ~params, ~key, ~outgoing_messages=updated_chat.outgoing_messages, req =>
        switch (OpenRouter.handle_chat(req)) {
        | Some(Reply(response)) =>
          schedule_action(response_handler(response.content))
        | Some(Error({message, code})) =>
          schedule_action(
            InternalError(
              "Error: " ++ message ++ " (code: " ++ string_of_int(code) ++ ")",
              mode,
              updated_chat.id,
            ),
          )
        | None =>
          let str_of_mode =
            switch (mode) {
            | HazelTutor => "HazelTutor"
            | CodeSuggestion => "CodeSuggestion"
            | TaskCompletion => "TaskCompletion"
            };
          print_endline(
            "Assistant: response parse failed (" ++ str_of_mode ++ ")",
          );
        }
      )
    ) {
    | Invalid_argument(e) =>
      print_endline(
        "Issue when making LLM call. (This is likely from an Option.get during sketch sending): "
        ++ e,
      )
    | _ => ()
    };
  | (None, _) =>
    let content = "No API key found. Please set an API key in the assistant settings.";
    schedule_action(InternalError(content, mode, updated_chat.id));
  | (_, None) =>
    let content = "No model ID found. Please set a model ID in the assistant settings.";
    schedule_action(InternalError(content, mode, updated_chat.id));
  };
};

let mk_mode_prompt = (~mode: AssistantSettings.mode): OpenRouter.message => {
  let prompt =
    switch (mode) {
    | HazelTutor => InitPrompts.mk_tutor()
    | CodeSuggestion => InitPrompts.mk_suggestion()
    | TaskCompletion => InitPrompts.mk_composition()
    };
  prompt;
};

let init_chat = (mode: AssistantSettings.mode): Model.chat => {
  let init_message = mk_mode_prompt(~mode);
  let init_message_display =
    mk_message_display(
      ~content=init_message.content,
      ~role=System(AssistantPrompt),
    );
  {
    outgoing_messages: [init_message],
    message_displays: [init_message_display],
    id: Id.mk(),
    descriptor: "",
    timestamp: JsUtil.timestamp(),
  };
};

let update =
    (
      ~settings: Settings.t,
      ~action,
      ~model: Model.t,
      ~schedule_action: t => unit,
      ~add_suggestion,
      ~goto,
      ~edit,
    )
    : Updated.t(Model.t) => {
  switch (action) {
  | SendMessage(kind, editor, chat_id) =>
    switch (kind) {
    | Tutor(content) =>
      let mode = AssistantSettings.HazelTutor;
      let curr_chat =
        Id.Map.find(chat_id, model.chat_history.past_tutor_chats);
      let user_message = OpenRouter.mk_user_msg(content);
      let ctx =
        OpenRouter.mk_user_msg(
          String.concat("\n", ChatLSP.get_sketch_and_error_ctx(editor)),
        );
      let new_message_displays = [
        mk_message_display(~content=user_message.content, ~role=User),
        mk_message_display(
          ~content=ctx.content,
          ~role=System(AssistantPrompt),
        ),
      ];
      let new_outgoing_messages =
        curr_chat.outgoing_messages @ [user_message, ctx];

      let updated_chat = {
        ...curr_chat,
        outgoing_messages: curr_chat.outgoing_messages @ new_outgoing_messages,
        message_displays: curr_chat.message_displays @ new_message_displays,
      };

      mk_llm_call(
        ~mode, ~schedule_action, ~updated_chat, ~response_handler=response =>
        HandleResponse(Tutor, response, chat_id)
      );

      update_model_chat_history(~model, ~mode, ~updated_chat)
      |> Updated.return_quiet;

    | Composition(kind) =>
      let mode = AssistantSettings.TaskCompletion;
      let curr_chat =
        Id.Map.find(chat_id, model.chat_history.past_composition_chats);
      switch (kind) {
      | Request(content) =>
        let user_message = OpenRouter.mk_user_msg(content);
        let ctx =
          ChatLSP.Composition.mk_ctx_prompt(ChatLSP.Options.init, editor);
        let new_message_displays = [
          mk_message_display(~content=user_message.content, ~role=User),
          mk_message_display(
            ~content=ctx.content,
            ~role=System(AssistantPrompt),
          ),
        ];
        let new_outgoing_messages = [user_message, ctx];

        let updated_chat = {
          ...curr_chat,
          outgoing_messages:
            curr_chat.outgoing_messages @ new_outgoing_messages,
          message_displays: curr_chat.message_displays @ new_message_displays,
        };

        mk_llm_call(
          ~mode, ~schedule_action, ~updated_chat, ~response_handler=response =>
          HandleResponse(
            CompositionLoopRound(editor, ChatLSP.Composition.max_tool_calls),
            response,
            chat_id,
          )
        );

        update_model_chat_history(~model, ~mode, ~updated_chat)
        |> Updated.return_quiet;

      | Loop(fuel) =>
        let ctx =
          ChatLSP.Composition.mk_ctx_prompt(ChatLSP.Options.init, editor);
        let new_message_displays = [
          mk_message_display(
            ~content=ctx.content,
            ~role=System(AssistantPrompt),
          ),
        ];
        let new_outgoing_messages = [ctx];

        let updated_chat = {
          ...curr_chat,
          outgoing_messages:
            curr_chat.outgoing_messages @ new_outgoing_messages,
          message_displays: curr_chat.message_displays @ new_message_displays,
        };

        mk_llm_call(
          ~mode, ~schedule_action, ~updated_chat, ~response_handler=response =>
          HandleResponse(
            CompositionLoopRound(editor, fuel),
            response,
            chat_id,
          )
        );

        update_model_chat_history(~model, ~mode, ~updated_chat)
        |> Updated.return_quiet;
      };

    | Completion(kind) =>
      let mode = AssistantSettings.CodeSuggestion;
      let curr_chat =
        Id.Map.find(chat_id, model.chat_history.past_suggestion_chats);
      switch (kind) {
      | Request(tile_id, advanced_reasoning) =>
        let tag = String.sub(Id.to_string(tile_id), 0, 3);
        switch (
          {
            let* sketch_z_with_tag =
              Perform.paste(editor.editor.state.zipper, tag);
            let sketch_seg =
              Zipper.smart_seg(
                ~dump_backpack=true,
                ~erase_buffer=true,
                sketch_z_with_tag,
              );
            let* index = Indicated.index(editor.editor.state.zipper);
            print_endline("Debug: Index found: " ++ Id.to_string(index));
            print_endline(
              "Debug: Info map size: "
              ++ string_of_int(Id.Map.cardinal(editor.statics.info_map)),
            );
            Id.Map.iter(
              (k, _) =>
                print_endline("Debug: Map entry: " ++ Id.to_string(k)),
              editor.statics.info_map,
            );
            let+ ci = Id.Map.find_opt(index, editor.statics.info_map);
            ChatLSP.Completion.prompt(
              ChatLSP.Options.init,
              ci,
              sketch_seg,
              (advanced_reasoning ? "?a" : "??") ++ tag,
              advanced_reasoning,
            );
          }
        ) {
        | None =>
          print_endline("Suggestion prompt generation failed");
          model |> Updated.return_quiet;
        | Some(suggestion_prompt) =>
          let new_message_displays =
            List.map(
              (msg: OpenRouter.message) =>
                mk_message_display(
                  ~content=msg.content,
                  ~role=System(AssistantPrompt),
                ),
              suggestion_prompt,
            );
          let updated_chat = {
            ...curr_chat,
            outgoing_messages: curr_chat.outgoing_messages @ suggestion_prompt,
            message_displays:
              curr_chat.message_displays @ new_message_displays,
          };
          mk_llm_call(
            ~mode, ~schedule_action, ~updated_chat, ~response_handler=response =>
            HandleResponse(
              CompletionErrorRound(
                editor,
                ChatLSP.Options.init.error_rounds_max,
                tile_id,
              ),
              response,
              chat_id,
            )
          );
          update_model_chat_history(
            ~model,
            ~mode=settings.assistant.mode,
            ~updated_chat,
          )
          |> Updated.return_quiet;
        };
      | Query(content) =>
        let user_message = OpenRouter.mk_user_msg(content);
        let ctx =
          OpenRouter.mk_user_msg(
            String.concat("\n", ChatLSP.get_sketch_and_error_ctx(editor)),
          );
        let new_message_displays = [
          mk_message_display(~content=user_message.content, ~role=User),
          mk_message_display(
            ~content=ctx.content,
            ~role=System(AssistantPrompt),
          ),
        ];
        let new_outgoing_messages =
          curr_chat.outgoing_messages @ [user_message, ctx];
        let updated_chat = {
          ...curr_chat,
          outgoing_messages:
            curr_chat.outgoing_messages @ new_outgoing_messages,
          message_displays: curr_chat.message_displays @ new_message_displays,
        };

        mk_llm_call(
          ~mode, ~schedule_action, ~updated_chat, ~response_handler=response =>
          HandleResponse(CompletionQueryResponse, response, chat_id)
        );

        update_model_chat_history(~model, ~mode, ~updated_chat)
        |> Updated.return_quiet;

      | Loop(error, tile_id, fuel) =>
        let error_message =
          OpenRouter.mk_user_msg(
            "Your previous response caused the following error. Please fix it in your response: "
            ++ error,
          );
        let new_outgoing_messages = [error_message];
        let new_message_displays = [
          mk_message_display(
            ~content=error_message.content,
            ~role=System(AssistantPrompt),
          ),
        ];
        let updated_chat = {
          ...curr_chat,
          outgoing_messages:
            curr_chat.outgoing_messages @ new_outgoing_messages,
          message_displays: curr_chat.message_displays @ new_message_displays,
        };

        // check that fuel is not 0
        if (fuel < 0) {
          let content =
            "By default we stop the assistant after "
            ++ string_of_int(ChatLSP.Options.init.error_rounds_max)
            ++ " error rounds.";
          schedule_action(InternalError(content, mode, updated_chat.id));
        } else {
          mk_llm_call(
            ~mode, ~schedule_action, ~updated_chat, ~response_handler=response =>
            HandleResponse(
              CompletionErrorRound(
                editor,
                ChatLSP.Options.init.error_rounds_max,
                tile_id,
              ),
              response,
              chat_id,
            )
          );
        };
        update_model_chat_history(~model, ~mode, ~updated_chat)
        |> Updated.return_quiet;
      };
    }
  | InternalError(content, mode, chat_id) =>
    let curr_chat =
      switch (
        Id.Map.find_opt(chat_id, model.chat_history.past_composition_chats)
      ) {
      | Some(chat) => chat
      | None =>
        print_endline("Error: Chat not found");
        get_mode_info(mode, model) |> snd;
      };

    let new_message_displays = [
      mk_message_display(~content, ~role=System(InternalError)),
    ];

    let updated_chat = {
      ...curr_chat,
      message_displays: curr_chat.message_displays @ new_message_displays,
    };
    update_model_chat_history(~model, ~mode, ~updated_chat)
    |> Updated.return_quiet;

  | HandleResponse(response_kind, content, chat_id) =>
    let (curr_chat, mode) =
      switch (response_kind) {
      | Tutor => (
          Id.Map.find(chat_id, model.chat_history.past_tutor_chats),
          AssistantSettings.HazelTutor,
        )
      | CompositionLoopRound(_) => (
          Id.Map.find(chat_id, model.chat_history.past_composition_chats),
          AssistantSettings.TaskCompletion,
        )
      | CompletionErrorRound(_) => (
          Id.Map.find(chat_id, model.chat_history.past_suggestion_chats),
          AssistantSettings.CodeSuggestion,
        )
      | CompletionQueryResponse => (
          Id.Map.find(chat_id, model.chat_history.past_suggestion_chats),
          AssistantSettings.CodeSuggestion,
        )
      };
    create_chat_descriptor(~model, ~schedule_action, ~mode, ~chat_id);

    // If streaming, update the last message display
    let (updated_outgoing_messages, updated_message_displays) = {
      let last_display = ListUtil.last(curr_chat.message_displays);
      if (last_display.role == Assistant) {
        let updated_content = last_display.original_content ++ content;
        (
          ListUtil.leading(curr_chat.outgoing_messages)
          @ [OpenRouter.mk_assistant_msg(updated_content)],
          ListUtil.leading(curr_chat.message_displays)
          @ [mk_message_display(~content=updated_content, ~role=Assistant)],
        );
      } else {
        (
          curr_chat.outgoing_messages @ [OpenRouter.mk_assistant_msg(content)],
          curr_chat.message_displays
          @ [mk_message_display(~content, ~role=Assistant)],
        );
      };
    };

    let updated_chat = {
      ...curr_chat,
      outgoing_messages: updated_outgoing_messages,
      message_displays: updated_message_displays,
    };

    switch (response_kind) {
    | Tutor => ()
    | CompositionLoopRound(editor, fuel) =>
      if (fuel == 0) {
        schedule_action(
          InternalError(
            "By default, we stop the agent after "
            ++ string_of_int(ChatLSP.Composition.max_tool_calls)
            ++ " tool calls.",
            mode,
            chat_id,
          ),
        );
      } else {
        let tool_calls = extract_tool_calls(content);
        let rec process_tool_calls = (calls: list(string)) => {
          switch (calls) {
          | [] => ()
          | [tool_call, ...remaining] =>
            let parsed_response =
              try(
                switch (Json.from_string(tool_call)) {
                | `Assoc(fields) =>
                  let tool = List.assoc_opt("tool", fields);
                  let arg = List.assoc_opt("args", fields);
                  (tool, arg);
                | _ => (None, None)
                }
              ) {
              | Yojson.Json_error(_) => (None, None)
              };
            let tool =
              switch (parsed_response) {
              | (Some(`String(tool)), _) => tool
              | _ =>
                schedule_action(
                  InternalError("Unable to parse tool call", mode, chat_id),
                );
                "submit";
              };
            let args =
              switch (parsed_response) {
              | (_, Some(`Assoc(args))) => args
              | _ => []
              };

            let invalid_arg_type =
                (tool: string, arg: string, typ: Json.t): string => {
              tool
              ++ " called with invalid type at argument position "
              ++ arg
              ++ ": "
              ++ Json.to_string(typ);
            };

            let invalid_num_args =
                (tool: string, expected: int, received: int): string => {
              tool
              ++ " expected "
              ++ string_of_int(expected)
              ++ " argument"
              ++ (expected == 1 ? "" : "s")
              ++ " but "
              ++ string_of_int(received)
              ++ " were given";
            };

            try(
              switch (tool) {
              | "goto_definition" =>
                switch (
                  List.assoc_opt("variable_name", args),
                  List.length(args),
                ) {
                | (Some(`String(arg)), 1) =>
                  goto(editor, ChatLSP.Composition.Definition, arg)
                | (Some(inv_type), _) =>
                  raise(
                    Failure(
                      invalid_arg_type(
                        "goto_definition",
                        "variable_name",
                        inv_type,
                      ),
                    ),
                  )
                | (_, n) =>
                  raise(Failure(invalid_num_args("goto_definition", 1, n)))
                };
                process_tool_calls(remaining);
              | "goto_body" =>
                switch (
                  List.assoc_opt("variable_name", args),
                  List.length(args),
                ) {
                | (Some(`String(arg)), 1) =>
                  goto(editor, ChatLSP.Composition.Body, arg)
                | (Some(inv_type), _) =>
                  raise(
                    Failure(
                      invalid_arg_type(
                        "goto_body",
                        "variable_name",
                        inv_type,
                      ),
                    ),
                  )
                | (_, n) =>
                  raise(Failure(invalid_num_args("goto_body", 1, n)))
                };
                process_tool_calls(remaining);
              | "select_all" =>
                goto(editor, ChatLSP.Composition.All, "");
                process_tool_calls(remaining);
              | "paste" =>
                switch (List.assoc_opt("code", args), List.length(args)) {
                | (Some(`String(arg)), 1) =>
                  edit(ChatLSP.Composition.Current, arg)
                | (Some(inv_type), _) =>
                  raise(Failure(invalid_arg_type("edit", "code", inv_type)))
                | (_, n) => raise(Failure(invalid_num_args("edit", 1, n)))
                };
                process_tool_calls(remaining);
              | "delete" =>
                List.length(args) != 0
                  ? raise(
                      Failure(
                        invalid_num_args("delete", 0, List.length(args)),
                      ),
                    )
                  : edit(ChatLSP.Composition.Current, "");
                process_tool_calls(remaining);
              | "view_sketch" =>
                List.length(args) != 0
                  ? raise(
                      Failure(
                        invalid_num_args(
                          "view_sketch",
                          0,
                          List.length(args),
                        ),
                      ),
                    )
                  : schedule_action(
                      SendMessage(
                        Composition(Loop(fuel - 1)),
                        editor,
                        chat_id,
                      ),
                    )
              | "submit" =>
                List.length(args) != 0
                  ? raise(
                      Failure(
                        invalid_num_args("submit", 0, List.length(args)),
                      ),
                    )
                  : ()
              | _ => raise(Failure("Unknown tool call: " ++ tool_call))
              }
            ) {
            | Failure(err) =>
              schedule_action(InternalError(err, mode, chat_id))
            };
          };
        };
        process_tool_calls(tool_calls);
      }
    | CompletionErrorRound(editor, fuel, tileId) =>
      // Split response into discussion and completion
      let code_pattern =
        Str.regexp(
          "\\(\\(.\\|\n\\)*\\)```[ \n]*\\([^`]+\\)[ \n]*```\\(\\(.\\|\n\\)*\\)",
        );
      let index = Option.get(Indicated.index(editor.editor.state.zipper));
      let ci = Option.get(Id.Map.find_opt(index, editor.statics.info_map));
      let sketch_z = editor.editor.state.zipper;

      let (_, completion) =
        if (Str.string_match(code_pattern, content, 0)) {
          let before = String.trim(Str.matched_group(1, content));
          let code = String.trim(Str.matched_group(3, content));
          (before, code |> StringUtil.trim_leading);
        } else {
          print_endline("Regex match failed for: " ++ content);
          ("", content |> StringUtil.trim_leading); // Fallback if no code block found
        };

      switch (ChatLSP.ErrorRound.mk_reply(ci, sketch_z, completion)) {
      | None =>
        print_endline("ERROR ROUNDS (Non-error Response): " ++ completion);
        schedule_action(
          EmployLLMAction(RemoveAndSuggest(completion, tileId)),
        );
      | Some(error) =>
        print_endline("ERROR ROUNDS (Error): " ++ error);
        print_endline(
          "ERROR ROUNDS (Error-causing Response): " ++ completion,
        );
        schedule_action(
          SendMessage(
            Completion(Loop(error, tileId, fuel - 1)),
            editor,
            chat_id,
          ),
        );
      };
    | CompletionQueryResponse => ()
    };
    update_model_chat_history(~model, ~mode, ~updated_chat)
    |> Updated.return_quiet;
  | EmployLLMAction(action) =>
    switch (action) {
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
      let updated_past_chats =
        Id.Map.update(
          chat_id,
          opt_chat =>
            switch (opt_chat) {
            | Some(chat: Model.chat) =>
              Some({
                ...chat,
                descriptor: content,
              })
            | None => None
            },
          past_chats,
        );
      resculpt_model(~model, ~mode, ~updated_past_chats, ~chat_id)
      |> Updated.return_quiet;
    }

  | ChatAction(action) =>
    switch (action) {
    | NewChat =>
      let mode = settings.assistant.mode;
      let (past_chats, _) = get_mode_info(mode, model);
      let new_chat: Model.chat = init_chat(mode);
      let updated_history = add_chat_to_history(new_chat, past_chats);
      resculpt_model(
        ~model,
        ~mode,
        ~updated_past_chats=updated_history,
        ~chat_id=new_chat.id,
      )
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
              resculpt_model(
                ~model,
                ~mode,
                ~updated_past_chats=filtered_past_chats,
                ~chat_id=chat.id,
              )
            | None =>
              resculpt_model(
                ~model,
                ~mode,
                ~updated_past_chats=past_chats,
                ~chat_id=curr_chat.id,
              )
            }
          : resculpt_model(
              ~model,
              ~mode,
              ~updated_past_chats=filtered_past_chats,
              ~chat_id=curr_chat.id,
            );
      updated_model |> Updated.return_quiet;

    // Concat LS' error message and await_llm_response (... animation)
    // This works even if out of fuel, as both Respond and ErrorRespond
    // remove await_llm_response
    | CollapseMessage(index) =>
      let mode = settings.assistant.mode;
      let (past_chats, curr_chat) = get_mode_info(mode, model);
      let is_prompt_display =
        try(
          List.nth(curr_chat.message_displays, index).role
          == System(AssistantPrompt)
        ) {
        | Invalid_argument(_) => true
        };
      print_endline(
        "Is prompt display: " ++ string_of_bool(is_prompt_display),
      );
      let updated_message_displays =
        List.mapi(
          (i: int, msg: Model.display) =>
            if (i == index) {
              {
                ...msg,
                collapsed: !msg.collapsed,
              };
            } else if (msg.role == System(AssistantPrompt)
                       && is_prompt_display) {
              print_endline(
                "Collapsing prompt display message at index: "
                ++ string_of_int(i),
              );
              {
                ...msg,
                collapsed: true,
              };
            } else {
              msg;
            },
          curr_chat.message_displays,
        );
      let updated_past_chats =
        Id.Map.update(
          curr_chat.id,
          opt_chat =>
            switch (opt_chat) {
            | Some(chat: Model.chat) =>
              Some({
                ...chat,
                message_displays: updated_message_displays,
              })
            | None => None
            },
          past_chats,
        );
      resculpt_model(
        ~model,
        ~mode,
        ~updated_past_chats,
        ~chat_id=curr_chat.id,
      )
      |> Updated.return_quiet;

    | SwitchChat(chat_id) =>
      let mode = settings.assistant.mode;
      let (past_chats, _) = get_mode_info(mode, model);
      resculpt_model(~model, ~mode, ~updated_past_chats=past_chats, ~chat_id)
      |> Updated.return_quiet;
    | FilterLoadingMessages =>
      Model.{
        ...model,
        chat_history: {
          past_tutor_chats:
            Id.Map.map(
              (chat: Model.chat) => {
                {
                  ...chat,
                  message_displays:
                    filter_chat_messages(chat.message_displays),
                }
              },
              model.chat_history.past_tutor_chats,
            ),
          past_suggestion_chats:
            Id.Map.map(
              (chat: Model.chat) => {
                {
                  ...chat,
                  message_displays:
                    filter_chat_messages(chat.message_displays),
                }
              },
              model.chat_history.past_suggestion_chats,
            ),
          past_composition_chats:
            Id.Map.map(
              (chat: Model.chat) => {
                {
                  ...chat,
                  message_displays:
                    filter_chat_messages(chat.message_displays),
                }
              },
              model.chat_history.past_composition_chats,
            ),
        },
      }
      |> Updated.return_quiet
    }
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
let init: Model.t = {
  let (init_tutor_chat, init_suggestion_chat, init_composition_chat) = (
    init_chat(HazelTutor),
    init_chat(CodeSuggestion),
    init_chat(TaskCompletion),
  );
  {
    current_chats: {
      curr_tutor_chat: init_tutor_chat.id,
      curr_suggestion_chat: init_suggestion_chat.id,
      curr_composition_chat: init_composition_chat.id,
    },
    chat_history: {
      past_tutor_chats: add_chat_to_history(init_tutor_chat, Id.Map.empty),
      past_suggestion_chats:
        add_chat_to_history(init_suggestion_chat, Id.Map.empty),
      past_composition_chats:
        add_chat_to_history(init_composition_chat, Id.Map.empty),
    },
  };
};
