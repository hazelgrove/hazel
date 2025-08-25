open Util;
open Util.Maps;
open Haz3lcore;

module CodeModel = CodeEditable.Model;
module Model = AssistantModel;

[@deriving (show({with_path: false}), sexp, yojson)]
type completion =
  | Request(Id.t, bool) // When user presses ?? or ?a
  | Query(string) // User may followup with a query
  | Loop(string, Id.t, int); // Error rounds

[@deriving (show({with_path: false}), sexp, yojson)]
type status =
  | Success(string)
  | Failure(string);

[@deriving (show({with_path: false}), sexp, yojson)]
type composition =
  | Request(string) // User-submitted task, question, etc
  // TODO: maybe remove in the future, this is a hack for selecting the current code the
  //       agent is at, a useful UI feature
  | Intermediate
  | Loop(int, OpenRouter.tool_contents, status); // Iterative tool completion loop

// Actions to send various kinds of messages to the LLM
[@deriving (show({with_path: false}), sexp, yojson)]
type send_message =
  | Tutor(string)
  | Completion(completion)
  | Composition(composition, bool);

// Actions to handle certain kinds of LLM responses
[@deriving (show({with_path: false}), sexp, yojson)]
type handle_response =
  | Tutor
  | CompletionErrorRound(CodeModel.t, int, Id.t)
  | CompletionQueryResponse
  | CompositionLoopRound(CodeModel.t, int, bool);

// Actions which actualize actions via LLM responses
[@deriving (show({with_path: false}), sexp, yojson)]
type employ_llm_action =
  | RemoveAndSuggest(string, Id.t)
  | Describe(string, AssistantSettings.mode, Id.t)
  | Summarize(string, AssistantSettings.mode, Id.t)
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
  | SetLLM(OpenRouter.model_info)
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

let filter_chat_messages =
    (messages: list(Model.message)): list(Model.message) => {
  List.filter((_: Model.message) => {true}, messages);
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
      ~awaiting_response: bool,
    )
    : Model.t => {
  let updated_chat = {
    ...updated_chat,
    awaiting_response,
  };
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
      List.filter_map(
        (message: Model.message) => {
          switch (message.content) {
          | Some(content) =>
            Some(
              "<"
              ++ Model.string_of_role(message.role)
              ++ ">"
              ++ content.content
              ++ "</"
              ++ Model.string_of_role(message.role)
              ++ ">",
            )
          | None => None
          }
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
        let model_id = model.external_api_info.set_model_info.id;
        let key = model.external_api_info.api_key;
        let params: OpenRouter.params = {
          ...OpenRouter.default_params,
          model_id,
          stream: false // No streaming for descriptor
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
  switch (
    model.external_api_info.api_key,
    model.external_api_info.set_model_info.id,
  ) {
  | ("", _) =>
    let content = "No API key found. Please set an API key in the assistant settings.";
    schedule_action(InternalError(content, mode, updated_chat.id));
  | (_, "") =>
    let content = "No model ID found. Please set a model ID in the assistant settings.";
    schedule_action(InternalError(content, mode, updated_chat.id));
  | (key, model_id) =>
    let tools =
      if (mode == TaskCompletion) {
        CompositionTools.tools;
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
  let _ = editor;
  {
    content: Some(OpenRouter.mk_user_msg(content)),
    display: Some(Model.mk_message_display(~content)),
    role,
    sketch_snapshot: None // Some(editor), todo: figure out how to serialize editor
  };
};

let update_chat = (chat: Model.chat, messages: list(Model.message)) => {
  {
    ...chat,
    messages: chat.messages @ messages,
  };
};

let summarize_chat =
    (
      model: Model.t,
      chat: Model.chat,
      mode: AssistantSettings.mode,
      schedule_action: t => unit,
    )
    : unit => {
  // Filter our initial prompt
  let outgoing_messages: list(OpenRouter.message) =
    List.filter_map(
      (message: Model.message) =>
        switch (message.content) {
        | Some(content) =>
          switch (content.role) {
          | System => None
          | _ => Some(content)
          }
        | None => None
        },
      chat.messages,
    );
  let summarize_message: OpenRouter.message =
    OpenRouter.mk_user_msg(SummarizePrompt.prelude);
  let outgoing_messages = outgoing_messages @ [summarize_message];
  try({
    let model_id = model.external_api_info.set_model_info.id;
    let key = model.external_api_info.api_key;
    let params: OpenRouter.params = {
      ...OpenRouter.default_params,
      model_id,
      stream: false // No streaming for summarization
    };
    OpenRouter.start_chat(~params, ~key, ~outgoing_messages, req =>
      switch (OpenRouter.handle_chat(req)) {
      | Some(Reply({content, _})) =>
        schedule_action(EmployLLMAction(Summarize(content, mode, chat.id)))
      | Some(Error(_)) =>
        raise(
          Invalid_argument(
            "Error in receiving response from OpenRouter when summarizing chat",
          ),
        )
      | None => ()
      }
    );
  }) {
  | Invalid_argument(e) =>
    print_endline("Invalid_argument when summarizing chat: " ++ e);
    ();
  };
};
