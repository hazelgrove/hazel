open Haz3lcore;
open Util;
open Util.OptUtil.Syntax;
open API;
open Util.Maps;

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
  | Loop(int, OpenRouter.tool_contents); // Iterative tool completion loop

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
  | Describe(string, AssistantSettings.mode, Id.t)
  | SetLoop(bool);

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
  | FilterLoadingMessages
  | Lop(int);

[@deriving (show({with_path: false}), sexp, yojson)]
type external_api_action =
  // Sets the LLM model
  | SetLLM(string)
  // Sets the API key.
  // This will implicitely make a call to OpenRouter to get and set the list of available LLMs.
  | SetAPIKey(string)
  // Sets the list of available LLMs from OpenRouter
  | SetListOfLLMs(list(OpenRouter.model_info));

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | SendMessage(send_message, option(CodeModel.t), Id.t)
  | HandleResponse(handle_response, OpenRouter.reply, Id.t)
  | EmployLLMAction(employ_llm_action)
  | ChatAction(chat_action)
  | InternalError(string, AssistantSettings.mode, Id.t)
  | ExternalAPIAction(external_api_action)
  | InitializeAssistant;

let can_undo = (action: t) => {
  // TODO: Implement the handling of actions that should be undoable
  // I'm thinking none of these actions should be undoable...
  // Maybe set API key?
  // That could be a good starter project to navigate this assistant part of the codebase.
  switch (action) {
  | SendMessage(_) => false
  | HandleResponse(_) => true /* Necessary to make completion instantiation undoable */
  | EmployLLMAction(_) => false
  | ChatAction(_) => false
  | InternalError(_) => false
  | ExternalAPIAction(_) => false
  | InitializeAssistant => false
  };
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
    (messages: list(Model.message)): list(Model.message) => {
  List.filter(
    (message: Model.message) => {
      message.role != Assistant || message.display.collapsed
    },
    messages,
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
    ...model,
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
        "EVERY response will be displayed as a summarizaing title, so do NOT respond with anything other than a summarizing title. ",
        switch (mode) {
        | HazelTutor => "This is known to be a chat between a hazel user and an LLM acting as a tutor."
        | CodeSuggestion => "This is known to be a chat between a hazel user and an LLM acting as a code suggestion assistant. This means there won't be much dialogue, rather just a prompt, code contexts, and a code suggestion (potentially with a chain of thought), so please do your best to summarize based on the code context and the code suggestion."
        | TaskCompletion => "This is known to be a chat between a student and an LLM acting as a task completion assistant."
        },
        "With this said, please now provide a summary for the conversation: ",
      ],
    );

  let filtered_messages =
    List.filter(
      (message: Model.message) => {
        message.role == User || message.role == Assistant
      },
      curr_chat.messages,
    );

  let combined_messages =
    String.concat(
      "\n",
      List.map(
        (message: Model.message) => {
          "<"
          ++ Model.string_of_role(message.role)
          ++ ">"
          ++ message.content.content
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
        let model_id = model.external_api_info.set_model;
        let key = model.external_api_info.api_key;
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
      ~schedule_action: t => unit,
      ~schedule_setting: AssistantSettings.action => unit,
      ~editor: CodeEditable.Model.t,
      ~chat_id: Id.t,
    )
    : unit => {
  let z = editor.editor.state.zipper;
  let caret = z.caret;
  let siblings = z.relatives.siblings;
  let send_message = (tile_id, advanced_reasoning) => {
    schedule_setting(AssistantSettings.SwitchMode(CodeSuggestion));
    schedule_action(
      SendMessage(
        Completion(Request(tile_id, advanced_reasoning)),
        None,
        chat_id,
      ),
    );
  };

  // Check if user just typed ??
  switch (caret, Zipper.neighbor_monotiles(siblings)) {
  | (Outer, (_, Some("??")))
  | (Outer, (Some("??"), _)) =>
    let tileId = Option.get(Indicated.index(z));
    let advanced_reasoning = false;
    send_message(tileId, advanced_reasoning);
  | (Outer, (_, Some("?a")))
  | (Outer, (Some("?a"), _)) =>
    let tileId = Option.get(Indicated.index(z));
    let advanced_reasoning = true;
    send_message(tileId, advanced_reasoning);
  | _ => ()
  };
};

// Sends a request to OpenRouter given outgoing messages.
// Handles the response from OpenRouter.
// Emits internal error if API key or model ID is not set.
let mk_llm_call =
    (
      ~mode: AssistantSettings.mode,
      ~model: Model.t,
      ~schedule_action: t => unit,
      ~updated_chat: Model.chat,
      ~response_handler: OpenRouter.reply => t,
    )
    : unit => {
  switch (model.external_api_info.api_key, model.external_api_info.set_model) {
  | ("", _) =>
    let content = "No API key found. Please set an API key in the assistant settings.";
    schedule_action(InternalError(content, mode, updated_chat.id));
  | (_, "") =>
    let content = "No model ID found. Please set a model ID in the assistant settings.";
    schedule_action(InternalError(content, mode, updated_chat.id));
  | (key, model_id) =>
    let tools =
      if (mode == TaskCompletion) {
        [
          CompositionPrompt.update_pattern,
          CompositionPrompt.update_definition,
          CompositionPrompt.update_body,
          CompositionPrompt.delete_body,
          CompositionPrompt.update_binding,
          CompositionPrompt.delete_binding,
          CompositionPrompt.add_before,
          CompositionPrompt.add_after,
          // CompositionPrompt.goto_definition,
          // CompositionPrompt.goto_body,
          // //CompositionPrompt.goto_type_definition,
          // //CompositionPrompt.goto_type_body,
          // CompositionPrompt.select_all,
          // CompositionPrompt.paste,
          // CompositionPrompt.delete,
          //CompositionPrompt.submit,
        ];
      } else {
        [];
      };
    let params: OpenRouter.params = {
      ...OpenRouter.default_params,
      model_id,
      tools,
    };
    try(
      OpenRouter.start_chat(
        ~params,
        ~key,
        ~outgoing_messages=Model.get_messages_content(updated_chat.messages),
        req =>
        switch (OpenRouter.handle_chat(req)) {
        | Some(Reply(response)) =>
          schedule_action(response_handler(response))
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
          ();
          print_endline(
            "Assistant: response still generating: " ++ str_of_mode,
          );
          ();
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
  };
};

let mk_user_content_message =
    (~content: string, ~role: Model.role, ~editor: CodeEditable.Model.t)
    : Model.message => {
  {
    content: OpenRouter.mk_user_msg(content),
    display: Model.mk_message_display(~content, ~role),
    role,
    sketch_snapshot: Some(editor),
  };
};

let mk_structure_edit_msg =
    (~tool_call: string, ~args: option(StringMap.t(string))) =>
  try({
    let enclose_in_backticks = (str: string) => "```" ++ str ++ "```";
    let args = Option.get(args);
    switch (OpenRouter.structure_action_of_string(tool_call)) {
    | OpenRouter.UpdatePattern =>
      let variable_name = StringMap.find("variable_name", args);
      let new_pattern = StringMap.find("new_pattern", args);
      "Agent updated the pattern of the variable "
      ++ variable_name
      ++ " to: "
      ++ enclose_in_backticks(new_pattern);

    | OpenRouter.UpdateDefinition =>
      let variable_name = StringMap.find("variable_name", args);
      let new_definition = StringMap.find("new_definition", args);
      "Agent updated the definition of the variable "
      ++ variable_name
      ++ " to: "
      ++ enclose_in_backticks(new_definition);
    | OpenRouter.UpdateBinding =>
      let variable_name = StringMap.find("variable_name", args);
      let new_binding = StringMap.find("new_binding", args);
      "Agent updated the entire binding of the variable "
      ++ variable_name
      ++ " to: "
      ++ enclose_in_backticks(new_binding);
    | OpenRouter.UpdateBody =>
      let variable_name = StringMap.find("variable_name", args);
      let new_body = StringMap.find("new_body", args);
      "Agent updated the body of the variable "
      ++ variable_name
      ++ " to: "
      ++ enclose_in_backticks(new_body);
    | OpenRouter.DeleteBinding =>
      let variable_name = StringMap.find("variable_name", args);
      "Agent deleted the variable " ++ variable_name;
    | OpenRouter.DeleteBody =>
      switch (StringMap.find_opt("variable_name", args)) {
      | Some(variable_name) =>
        "Agent deleted the body of the variable " ++ variable_name
      | None => "Agent deleted the entire sketch"
      }
    | OpenRouter.AddBefore =>
      let code = StringMap.find("code", args);
      switch (StringMap.find_opt("variable_name", args)) {
      | Some(variable_name) =>
        "Agent added code before the variable "
        ++ variable_name
        ++ ": "
        ++ enclose_in_backticks(code)
      | None =>
        "Agent added code at the beginning of the sketch "
        ++ enclose_in_backticks(code)
      };

    | OpenRouter.AddAfter =>
      let code = StringMap.find("code", args);
      switch (StringMap.find_opt("variable_name", args)) {
      | Some(variable_name) =>
        "Agent added code after the variable "
        ++ variable_name
        ++ ": "
        ++ enclose_in_backticks(code)
      | None =>
        "Agent added code at the end of the sketch "
        ++ enclose_in_backticks(code)
      };
    | OpenRouter.InvalidStructureAction =>
      raise(Failure("Unknown structure action: " ++ tool_call))
    };
  }) {
  | Not_found => "Agent called " ++ tool_call ++ " with invalid arguments"
  | Invalid_argument(e) =>
    "Not sure what the agent did here, but the argument map creation failed: "
    ++ e
  };

let update =
    (
      ~settings: Settings.t,
      ~action,
      ~model: Model.t,
      // todo: Find a way to track unqique editor between concurrent actions
      ~editor: CodeModel.t,
      ~schedule_action: t => unit,
      ~schedule_editor_action: Editors.Update.t => unit,
    )
    : Updated.t(Model.t) => {
  switch (action) {
  | SendMessage(kind, editor_opt, chat_id) =>
    let editor =
      switch (editor_opt) {
      | Some(editor) => editor
      | None => editor
      };
    if (model.current_chats.curr_tutor_chat == Id.invalid) {
      model |> Updated.return_quiet;
    } else {
      switch (kind) {
      | Tutor(content) =>
        let mode = AssistantSettings.HazelTutor;
        let curr_chat =
          Id.Map.find(chat_id, model.chat_history.past_tutor_chats);
        let content_message =
          mk_user_content_message(~content, ~role=User, ~editor);
        let ctx_message: Model.message = {
          content:
            OpenRouter.mk_user_msg(
              String.concat("\n", ChatLSP.get_sketch_and_error_ctx(editor)),
            ),
          display:
            Model.mk_message_display(
              ~content=
                String.concat(
                  "\n",
                  ChatLSP.get_sketch_and_error_ctx(editor),
                ),
              ~role=System(AssistantPrompt),
            ),
          role: System(AssistantPrompt),
          sketch_snapshot: None,
        };

        let updated_chat = {
          ...curr_chat,
          messages: curr_chat.messages @ [content_message, ctx_message],
        };

        mk_llm_call(
          ~mode,
          ~model,
          ~schedule_action,
          ~updated_chat,
          ~response_handler=response =>
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
          print_endline("handling composition request");
          schedule_action(EmployLLMAction(SetLoop(false)));
          let content_message: Model.message =
            mk_user_content_message(~content, ~role=User, ~editor);
          let ctx_message: Model.message = {
            content:
              OpenRouter.mk_user_msg(
                String.concat(
                  "\n",
                  ChatLSP.get_sketch_and_error_ctx(editor),
                ),
              ),
            display:
              Model.mk_message_display(
                ~content=
                  String.concat(
                    "\n",
                    ChatLSP.get_sketch_and_error_ctx(editor),
                  ),
                ~role=System(AssistantPrompt),
              ),
            role: System(AssistantPrompt),
            sketch_snapshot: None,
          };

          // print all current messages
          print_endline("current messages: ");
          List.iter(
            (msg: Model.message) => print_endline(msg.content.content),
            curr_chat.messages,
          );

          let updated_chat = {
            ...curr_chat,
            messages: curr_chat.messages @ [content_message, ctx_message],
          };

          mk_llm_call(
            ~mode,
            ~model,
            ~schedule_action,
            ~updated_chat,
            ~response_handler=response =>
            HandleResponse(
              CompositionLoopRound(
                editor,
                ChatLSP.Composition.max_tool_calls,
              ),
              response,
              chat_id,
            )
          );

          update_model_chat_history(~model, ~mode, ~updated_chat)
          |> Updated.return_quiet;

        | Loop(fuel, tool_contents) =>
          let ctx =
            ChatLSP.Composition.mk_ctx_prompt(ChatLSP.Options.init, editor);

          let ctx_message: Model.message = {
            content: OpenRouter.mk_tool_msg(ctx.content, tool_contents),
            display:
              Model.mk_message_display(
                ~content=ctx.content,
                ~role=System(AssistantPrompt),
              ),
            role: System(AssistantPrompt),
            sketch_snapshot: None,
          };

          let updated_chat = {
            ...curr_chat,
            messages: curr_chat.messages @ [ctx_message],
          };

          mk_llm_call(
            ~mode,
            ~model,
            ~schedule_action,
            ~updated_chat,
            ~response_handler=response =>
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
        switch (kind) {
        | Request(tile_id, advanced_reasoning) =>
          let new_chat = Model.new_chat(model, mode);
          print_endline("new_chat: " ++ Id.to_string(new_chat.id));
          let updated_past_chats =
            Model.add_chat_to_history(
              new_chat,
              model.chat_history.past_suggestion_chats,
            );
          let model_with_new_chat =
            resculpt_model(
              ~model,
              ~mode,
              ~updated_past_chats,
              ~chat_id=new_chat.id,
            );
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
              let+ ci = Id.Map.find_opt(index, editor.statics.info_map);
              ChatLSP.Completion.mk_ctx_prompt(
                ChatLSP.Options.init,
                ci,
                sketch_seg,
                (advanced_reasoning ? "?a" : "??") ++ tag,
              );
            }
          ) {
          | None =>
            print_endline("Suggestion prompt generation failed");
            model_with_new_chat |> Updated.return_quiet;
          | Some(ctx_prompt) =>
            let ctx_message: Model.message = {
              content: ctx_prompt,
              display:
                Model.mk_message_display(
                  ~content=ctx_prompt.content,
                  ~role=System(AssistantPrompt),
                ),
              role: System(AssistantPrompt),
              sketch_snapshot: None,
            };
            let updated_chat = {
              ...new_chat,
              messages: new_chat.messages @ [ctx_message],
            };
            mk_llm_call(
              ~mode,
              ~model,
              ~schedule_action,
              ~updated_chat,
              ~response_handler=response =>
              HandleResponse(
                CompletionErrorRound(
                  editor,
                  ChatLSP.Options.init.error_rounds_max,
                  tile_id,
                ),
                response,
                new_chat.id,
              )
            );
            update_model_chat_history(
              ~model=model_with_new_chat,
              ~mode=settings.assistant.mode,
              ~updated_chat,
            )
            |> Updated.return_quiet;
          };
        | Query(content) =>
          let curr_chat =
            Id.Map.find(chat_id, model.chat_history.past_suggestion_chats);
          let ctx =
            OpenRouter.mk_user_msg(
              String.concat("\n", ChatLSP.get_sketch_and_error_ctx(editor)),
            );
          let ctx_message: Model.message = {
            content: ctx,
            display:
              Model.mk_message_display(
                ~content=ctx.content,
                ~role=System(AssistantPrompt),
              ),
            role: System(AssistantPrompt),
            sketch_snapshot: None,
          };
          let content_message =
            mk_user_content_message(~content, ~role=User, ~editor);
          let updated_chat = {
            ...curr_chat,
            messages: curr_chat.messages @ [ctx_message, content_message],
          };

          mk_llm_call(
            ~mode,
            ~model,
            ~schedule_action,
            ~updated_chat,
            ~response_handler=response =>
            HandleResponse(CompletionQueryResponse, response, chat_id)
          );

          update_model_chat_history(~model, ~mode, ~updated_chat)
          |> Updated.return_quiet;

        | Loop(error, tile_id, fuel) =>
          let curr_chat =
            Id.Map.find(chat_id, model.chat_history.past_suggestion_chats);
          let error_message =
            OpenRouter.mk_user_msg(
              "Your previous response caused the following error. Please fix it in your response: "
              ++ error,
            );
          let error_message: Model.message = {
            content: error_message,
            display:
              Model.mk_message_display(
                ~content=error_message.content,
                ~role=System(AssistantPrompt),
              ),
            role: System(AssistantPrompt),
            sketch_snapshot: None,
          };
          let updated_chat = {
            ...curr_chat,
            messages: curr_chat.messages @ [error_message],
          };

          // check that fuel is not 0
          if (fuel < 0) {
            let content =
              "By default we stop the assistant after "
              ++ string_of_int(ChatLSP.Options.init.error_rounds_max)
              ++ " error rounds.";
            schedule_action(EmployLLMAction(SetLoop(false)));
            schedule_action(InternalError(content, mode, updated_chat.id));
          } else {
            mk_llm_call(
              ~mode,
              ~model,
              ~schedule_action,
              ~updated_chat,
              ~response_handler=response =>
              HandleResponse(
                CompletionErrorRound(editor, fuel, tile_id),
                response,
                chat_id,
              )
            );
          };
          update_model_chat_history(~model, ~mode, ~updated_chat)
          |> Updated.return_quiet;
        };
      };
    };
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

    // todo: Should this be a user, assistant, or system message?
    //       We could make it assistant and put it in the first-person.
    let system_message: Model.message = {
      content: OpenRouter.mk_system_msg(content),
      display:
        Model.mk_message_display(~content, ~role=System(InternalError)),
      role: System(InternalError),
      sketch_snapshot: None,
    };

    // Note: We aren't sending a message here, but we do add it to the chat history.
    //       for future reference for the LLM so it isn't confused.
    //       (Eg: Max tool call limit reached, agent should know from history that this
    //        is why their prior task completion was not successful.)
    let updated_chat = {
      ...curr_chat,
      messages: curr_chat.messages @ [system_message],
    };
    update_model_chat_history(~model, ~mode, ~updated_chat)
    |> Updated.return_quiet;

  | HandleResponse(response_kind, response, chat_id) =>
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

    let content = response.content;
    let tool_call = response.tool_call;
    let assistant_message: Model.message = {
      content: OpenRouter.mk_assistant_msg(content),
      display: Model.mk_message_display(~content, ~role=Assistant),
      role: Assistant,
      sketch_snapshot: None,
    };

    // If streaming, update the last message display
    let updated_messages = {
      /* let last_display = ListUtil.last(curr_chat.message_displays);
         if (last_display.role == Assistant) {
           let updated_content = last_display.original_content ++ content;
           (
             ListUtil.leading(curr_chat.messages)
             @ [OpenRouter.mk_assistant_msg(updated_content)],
             ListUtil.leading(curr_chat.messages)
             @ [
               Model.mk_message_display(
                 ~content=updated_content,
                 ~role=Assistant,
               ),
             ],
           );
         } else */
      switch (tool_call) {
      | Some(tool_call) =>
        let structure_edit_message: Model.message = {
          content: OpenRouter.mk_system_msg(""),
          display:
            Model.mk_message_display(
              ~content=
                mk_structure_edit_msg(
                  ~tool_call=
                    OpenRouter.string_of_structure_action(tool_call.name),
                  ~args=Json.get_string_kvs(tool_call.args),
                ),
              ~role=Tool,
            ),
          role: Tool,
          sketch_snapshot: None,
        };
        switch (content) {
        | "" => curr_chat.messages @ [structure_edit_message]
        | _ =>
          curr_chat.messages @ [assistant_message, structure_edit_message]
        };
      | None => curr_chat.messages @ [assistant_message]
      };
    };

    let updated_chat = {
      ...curr_chat,
      messages: updated_messages,
    };

    switch (response_kind) {
    | Tutor => ()
    | CompositionLoopRound(_, fuel) =>
      switch (tool_call, fuel) {
      | (None, _) => ()
      | (_, 0) =>
        schedule_action(
          InternalError(
            "By default, we stop the agent after "
            ++ string_of_int(ChatLSP.Composition.max_tool_calls)
            ++ " tool calls.",
            mode,
            chat_id,
          ),
        )
      | (Some(tool_call), _) =>
        let loop_message =
          SendMessage(
            Composition(
              Loop(
                fuel - 1,
                {
                  tool_call_id: tool_call.id,
                  name: OpenRouter.string_of_structure_action(tool_call.name),
                },
              ),
            ),
            None,
            chat_id,
          );
        let apply_edit_action =
          ChatLSP.Composition.apply_edit_action(
            ~schedule_action=schedule_editor_action,
          );
        try(
          switch (tool_call.name) {
          | OpenRouter.UpdatePattern =>
            let (variable_name, variable_id, new_pattern) =
              switch (
                Json.dot("variable_name", tool_call.args),
                Json.dot("variable_id", tool_call.args),
                Json.dot("new_pattern", tool_call.args),
              ) {
              | (
                  Some(`String(variable_name)),
                  Some(`String(variable_id)),
                  Some(`String(new_pattern)),
                ) => (
                  Some(variable_name),
                  Some(variable_id),
                  new_pattern,
                )
              | _ =>
                raise(
                  Failure(
                    "Invalid arguments for "
                    ++ OpenRouter.string_of_structure_action(tool_call.name),
                  ),
                )
              };
            apply_edit_action(
              ~ed=editor,
              ~edit_action=ChatLSP.Composition.UpdatePattern(new_pattern),
              ~variable_name,
              ~variable_id,
            );
            schedule_action(loop_message);
          | OpenRouter.UpdateDefinition =>
            let (variable_name, variable_id, new_definition) =
              switch (
                Json.dot("variable_name", tool_call.args),
                Json.dot("variable_id", tool_call.args),
                Json.dot("new_definition", tool_call.args),
              ) {
              | (
                  Some(`String(variable_name)),
                  Some(`String(variable_id)),
                  Some(`String(new_definition)),
                ) => (
                  Some(variable_name),
                  Some(variable_id),
                  new_definition,
                )
              | _ =>
                raise(
                  Failure(
                    "Invalid arguments for "
                    ++ OpenRouter.string_of_structure_action(tool_call.name),
                  ),
                )
              };
            apply_edit_action(
              ~ed=editor,
              ~edit_action=
                ChatLSP.Composition.UpdateDefinition(new_definition),
              ~variable_name,
              ~variable_id,
            );
            schedule_action(loop_message);
          | OpenRouter.DeleteBinding =>
            let (variable_name, variable_id) =
              switch (
                Json.dot("variable_name", tool_call.args),
                Json.dot("variable_id", tool_call.args),
              ) {
              | (Some(`String(variable_name)), Some(`String(variable_id))) => (
                  Some(variable_name),
                  Some(variable_id),
                )
              | _ =>
                raise(
                  Failure(
                    "Invalid argument for "
                    ++ OpenRouter.string_of_structure_action(tool_call.name),
                  ),
                )
              };
            apply_edit_action(
              ~ed=editor,
              ~edit_action=ChatLSP.Composition.DeleteBinding,
              ~variable_name,
              ~variable_id,
            );
            schedule_action(loop_message);
          | OpenRouter.UpdateBinding =>
            let (variable_name, variable_id, new_binding) =
              switch (
                Json.dot("variable_name", tool_call.args),
                Json.dot("variable_id", tool_call.args),
                Json.dot("new_binding", tool_call.args),
              ) {
              | (
                  Some(`String(variable_name)),
                  Some(`String(variable_id)),
                  Some(`String(new_binding)),
                ) => (
                  Some(variable_name),
                  Some(variable_id),
                  new_binding,
                )
              | _ =>
                raise(
                  Failure(
                    "Invalid arguments for "
                    ++ OpenRouter.string_of_structure_action(tool_call.name),
                  ),
                )
              };
            apply_edit_action(
              ~ed=editor,
              ~edit_action=ChatLSP.Composition.UpdateBinding(new_binding),
              ~variable_name,
              ~variable_id,
            );
            schedule_action(loop_message);
          | OpenRouter.UpdateBody =>
            let (variable_name, variable_id, new_body) =
              switch (
                Json.dot("variable_name", tool_call.args),
                Json.dot("variable_id", tool_call.args),
                Json.dot("new_body", tool_call.args),
              ) {
              | (
                  Some(`String(variable_name)),
                  Some(`String(variable_id)),
                  Some(`String(new_body)),
                ) => (
                  Some(variable_name),
                  Some(variable_id),
                  new_body,
                )
              | _ =>
                raise(
                  Failure(
                    "Invalid arguments for "
                    ++ OpenRouter.string_of_structure_action(tool_call.name),
                  ),
                )
              };
            apply_edit_action(
              ~ed=editor,
              ~edit_action=ChatLSP.Composition.UpdateBody(new_body),
              ~variable_name,
              ~variable_id,
            );
            schedule_action(loop_message);
          | OpenRouter.DeleteBody =>
            let (variable_name, variable_id) =
              switch (
                Json.dot("variable_name", tool_call.args),
                Json.dot("variable_id", tool_call.args),
              ) {
              | (Some(`String(variable_name)), Some(`String(variable_id))) => (
                  Some(variable_name),
                  Some(variable_id),
                )
              | _ => (None, None)
              };
            apply_edit_action(
              ~ed=editor,
              ~edit_action=ChatLSP.Composition.DeleteBody,
              ~variable_name,
              ~variable_id,
            );
            schedule_action(loop_message);
          | OpenRouter.AddBefore =>
            let (variable_name, variable_id, code) =
              switch (
                Json.dot("variable_name", tool_call.args),
                Json.dot("variable_id", tool_call.args),
                Json.dot("code", tool_call.args),
              ) {
              | (
                  Some(`String(variable_name)),
                  Some(`String(variable_id)),
                  Some(`String(code)),
                ) => (
                  Some(variable_name),
                  Some(variable_id),
                  code,
                )
              | (_, _, Some(`String(code))) => (None, None, code)
              | _ =>
                raise(
                  Failure(
                    "Invalid arguments for "
                    ++ OpenRouter.string_of_structure_action(tool_call.name),
                  ),
                )
              };
            apply_edit_action(
              ~ed=editor,
              ~edit_action=ChatLSP.Composition.Add(Before, code),
              ~variable_name,
              ~variable_id,
            );
            schedule_action(loop_message);
          | OpenRouter.AddAfter =>
            let (variable_name, variable_id, code) =
              switch (
                Json.dot("variable_name", tool_call.args),
                Json.dot("variable_id", tool_call.args),
                Json.dot("code", tool_call.args),
              ) {
              | (
                  Some(`String(variable_name)),
                  Some(`String(variable_id)),
                  Some(`String(code)),
                ) => (
                  Some(variable_name),
                  Some(variable_id),
                  code,
                )
              | (_, _, Some(`String(code))) => (None, None, code)
              | _ =>
                raise(
                  Failure(
                    "Invalid arguments for "
                    ++ OpenRouter.string_of_structure_action(tool_call.name),
                  ),
                )
              };
            apply_edit_action(
              ~ed=editor,
              ~edit_action=ChatLSP.Composition.Add(After, code),
              ~variable_name,
              ~variable_id,
            );
            schedule_action(loop_message);
          | OpenRouter.InvalidStructureAction =>
            raise(
              Failure(
                "Unknown tool call: "
                ++ OpenRouter.string_of_structure_action(tool_call.name),
              ),
            )
          // | "goto_definition" =>
          //   switch (Json.dot("variable", tool_call.args)) {
          //   | Some(`String(arg)) =>
          //     goto(
          //       ~ed=editor,
          //       ~loc=ChatLSP.Composition.Definition,
          //       ~name=arg,
          //     );
          //     schedule_action(loop_message);
          //   | _ => raise(Failure("Invalid argument for goto_definition"))
          //   }
          // | "goto_body" =>
          //   switch (Json.dot("variable", tool_call.args)) {
          //   | Some(`String(arg)) =>
          //     goto(~ed=editor, ~loc=ChatLSP.Composition.Body, ~name=arg);
          //     schedule_action(loop_message);
          //   | _ => raise(Failure("Invalid argument for goto_body"))
          //   }
          // | "goto_type_definition" =>
          //   switch (Json.dot("variable", tool_call.args)) {
          //   | Some(`String(arg)) =>
          //     goto(
          //       ~ed=editor,
          //       ~loc=ChatLSP.Composition.Definition,
          //       ~name=arg,
          //     );
          //     schedule_action(loop_message);
          //   | _ =>
          //     raise(Failure("Invalid argument for goto_type_definition"))
          //   }
          // | "goto_type_body" =>
          //   switch (Json.dot("variable", tool_call.args)) {
          //   | Some(`String(arg)) =>
          //     goto(~ed=editor, ~loc=ChatLSP.Composition.Body, ~name=arg);
          //     schedule_action(loop_message);
          //   | _ => raise(Failure("Invalid argument for goto_type_body"))
          //   }
          // | "select_all" =>
          //   goto(~ed=editor, ~loc=ChatLSP.Composition.All, ~name="");
          //   schedule_action(loop_message);
          // | "paste" =>
          //   switch (Json.dot("code", tool_call.args)) {
          //   | Some(`String(arg)) =>
          //     edit(~loc=ChatLSP.Composition.Current, ~code=arg);
          //     schedule_action(loop_message);
          //   | _ => raise(Failure("Invalid argument for paste"))
          //   }
          // | "delete" =>
          //   edit(~loc=ChatLSP.Composition.Current, ~code="");
          //   schedule_action(loop_message);
          // | "submit" => ()
          }
        ) {
        | Failure(err) => schedule_action(InternalError(err, mode, chat_id))
        };
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
            None,
            chat_id,
          ),
        );
      };
    | CompletionQueryResponse => ()
    };
    update_model_chat_history(~model, ~mode, ~updated_chat)
    |> Updated.return_quiet;
  | EmployLLMAction(action) =>
    let add_suggestion =
      ChatLSP.Completion.add_suggestion(
        ~schedule_action=schedule_editor_action,
      );
    switch (action) {
    | RemoveAndSuggest(response, tileId) =>
      // Only side effects in the editor are performed here
      add_suggestion(~response, ~tile=tileId);
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
      let curr_chat_id =
        switch (mode) {
        | HazelTutor => model.current_chats.curr_tutor_chat
        | CodeSuggestion => model.current_chats.curr_suggestion_chat
        | TaskCompletion => model.current_chats.curr_composition_chat
        };
      resculpt_model(
        ~model,
        ~mode,
        ~updated_past_chats,
        ~chat_id=curr_chat_id,
      )
      |> Updated.return_quiet;
    | SetLoop(loop) =>
      {
        ...model,
        loop,
      }
      |> Updated.return_quiet
    };

  | ChatAction(action) =>
    switch (action) {
    | NewChat =>
      let mode = settings.assistant.mode;
      let (past_chats, _) = get_mode_info(mode, model);
      let new_chat: Model.chat = Model.new_chat(model, mode);
      let updated_history = Model.add_chat_to_history(new_chat, past_chats);
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
          List.nth(curr_chat.messages, index).role == System(AssistantPrompt)
        ) {
        | Invalid_argument(_) => true
        };
      let updated_messages =
        List.mapi(
          (i: int, msg: Model.message) =>
            if (i == index) {
              {
                ...msg,
                display: {
                  ...msg.display,
                  collapsed: !msg.display.collapsed,
                },
              };
            } else if (msg.role == System(AssistantPrompt)
                       && is_prompt_display) {
              {
                ...msg,
                display: {
                  ...msg.display,
                  collapsed: true,
                },
              };
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
              Some({
                ...chat,
                messages: updated_messages,
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
                  messages: filter_chat_messages(chat.messages),
                }
              },
              model.chat_history.past_tutor_chats,
            ),
          past_suggestion_chats:
            Id.Map.map(
              (chat: Model.chat) => {
                {
                  ...chat,
                  messages: filter_chat_messages(chat.messages),
                }
              },
              model.chat_history.past_suggestion_chats,
            ),
          past_composition_chats:
            Id.Map.map(
              (chat: Model.chat) => {
                {
                  ...chat,
                  messages: filter_chat_messages(chat.messages),
                }
              },
              model.chat_history.past_composition_chats,
            ),
        },
      }
      |> Updated.return_quiet
    | Lop(index) =>
      // Lop off the messages after the index
      let mode = settings.assistant.mode;
      let (_, curr_chat) = get_mode_info(mode, model);
      print_endline(
        "Lopping off messages after index: " ++ string_of_int(index),
      );
      let sketch_snapshot =
        List.nth(curr_chat.messages, index).sketch_snapshot;
      switch (sketch_snapshot) {
      | Some(sketch) =>
        let perform_action =
          CodeEditable.Update.Perform(Restore(sketch.editor.state.zipper));
        let cell_action = CellEditor.Update.MainEditor(perform_action);
        let scratch_action = Editors.Update.Scratch(CellAction(cell_action));
        schedule_editor_action(scratch_action);
      | None => ()
      };
      let updated_messages =
        curr_chat.messages |> ListUtil.take_up_to_n(index);
      let updated_chat = {
        ...curr_chat,
        messages: updated_messages,
      };
      update_model_chat_history(~model, ~mode, ~updated_chat)
      |> Updated.return_quiet;
    }
  | ExternalAPIAction(external_api_action) =>
    switch (external_api_action) {
    | SetLLM(llm_id) =>
      {
        ...model,
        external_api_info: {
          ...model.external_api_info,
          set_model: llm_id,
        },
      }
      |> Updated.return_quiet
    | SetAPIKey(api_key) =>
      // Set the available models using the provided API key
      OpenRouter.get_models(~key=api_key, ~handler=response => {
        switch (response) {
        | Some(json) =>
          switch (OpenRouter.parse_models_response(json)) {
          | Some(models_response) =>
            schedule_action(
              ExternalAPIAction(SetListOfLLMs(models_response.data)),
            )
          | None =>
            print_endline("Assistant: failed to parse models response")
          }
        | None =>
          print_endline("Assistant: no response received from OpenRouter API")
        }
      });
      {
        ...model,
        external_api_info: {
          ...model.external_api_info,
          api_key,
        },
      }
      |> Updated.return_quiet;
    | SetListOfLLMs(llms) =>
      {
        ...model,
        external_api_info: {
          ...model.external_api_info,
          available_models: llms,
        },
      }
      |> Updated.return_quiet
    }
  | InitializeAssistant => AssistantModel.init() |> Updated.return_quiet
  };
};
