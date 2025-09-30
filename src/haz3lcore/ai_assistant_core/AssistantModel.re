open Util;

/*
   The main model for the AI assistant.
 */

[@deriving (show({with_path: false}), sexp, yojson)]
type system =
  // Issue that we catch and inform the user about.
  // Do not send this to the model and display to user as error message
  | InternalError
  // The system prompt for the model.
  // Send this to the model.
  // Display to user as expandable/collapsable system message.
  | AssistantPrompt;

// Role of the entity sending the message.
// This is kept separate from the OpenRouter.role type,
// as we need to keep track of the system role for display purposes.
// (AssistantModel.System(InternalError) != OpenRouter.System)
// (AssistantModel.System(AssistantPrompt) != OpenRouter.System, an example
// of this is sketch displays. The only thing that is an OpenRouter.System type is
// the initial prompt itself.)
[@deriving (show({with_path: false}), sexp, yojson)]
type role =
  | System(system)
  | User
  | Assistant
  | Tool;

let string_of_role =
  fun
  | System(AssistantPrompt) => "System"
  | System(InternalError) => "Error"
  | User => "User"
  | Assistant => "Assistant"
  | Tool => "Tool";

// We currently parse code blocks out here
// In the future we could move this to have the Omd module handle this
[@deriving (show({with_path: false}), sexp, yojson)]
type block_kind =
  | Text(string)
  | Code(Segment.t);

// The displayable content of a message. This is here mainly to cache it
// in storage, avoiding runtime hindrances from parsing the content on the fly.
[@deriving (show({with_path: false}), sexp, yojson)]
type display = {
  displayable_content: list(block_kind),
  raw_content: string,
  collapsed: bool,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type sketch_snapshot = option(Zipper.t);

// A coupling of a message sent to the LLM and the displayable content of the message.
[@deriving (show({with_path: false}), sexp, yojson)]
type message = {
  // It may be the case we don't want to send a message to the LLM
  // E.g. Tool descriptions (user-facing, describing what the agent did)
  content: option(OpenRouter.message),
  // It may be the case we don't want to display a message to the user
  // E.g. Empty LLM responses/responses that only contain tool calls (who's text content is empty)
  display: option(display),
  role,
  sketch_snapshot,
};

// A chat is a collection of messages, attached to an ID
// We also include a timestamp, a descriptor, and a loading dots flag for stylistic purposes.
[@deriving (show({with_path: false}), sexp, yojson)]
type chat = {
  messages: list(message),
  id: Id.t,
  descriptor: string,
  timestamp: float,
  context_usage: int,
  awaiting_response: bool,
};

// We save the history of past chats as a hash map with chat IDs as keys.
[@deriving (show({with_path: false}), sexp, yojson)]
type chat_history = {
  // History logs of past chats stored as hash maps with chat IDs as keys
  past_tutor_chats: Id.Map.t(chat),
  past_suggestion_chats: Id.Map.t(chat),
  past_composition_chats: Id.Map.t(chat),
};

// We need to keep track of the chats which the user currently has active in each mode.
[@deriving (show({with_path: false}), sexp, yojson)]
type current_chats = {
  // Current active chat IDs for each mode
  curr_tutor_chat: Id.t,
  curr_suggestion_chat: Id.t,
  curr_composition_chat: Id.t,
};

// A record of the external API information, typically set in the settings menu.
[@deriving (show({with_path: false}), sexp, yojson)]
type external_api_info = {
  available_models: list(OpenRouter.model_info),
  set_model_info: OpenRouter.model_info,
  api_key: string,
};

// We cache these to avoid runtime hindrances from parsing the content on the fly.
// Pitfall: If prompt(s) ever change, must do hard reset of Hazel/clear local storage.
[@deriving (show({with_path: false}), sexp, yojson)]
type init_prompt_data = {
  init_tutor_chat: chat,
  init_composition_chat: chat,
  init_suggestion_chat_basic: chat,
  init_suggestion_chat_cot: chat,
};

// The AssistantModel type houses the current active chats, the history of past chats,
// the external API information, and the initial prompt data.
// The loop parameter is used exclusively for the task completion mode...
// there is likely a much better way to do this.
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  // Uuids of the currently active chats in each mode
  current_chats,
  // Collection of past chats of each mode, stored as a hash map with chat IDs as keys
  chat_history,
  // Information for OpenRouter API
  external_api_info,
  // Loading in and parsing the prompts is an expensive operation, so we perform this eagerly,
  // before the user sends their first request. This is currently done when the user sets an API key.
  init_prompt_data,
  // Agent looping flag - indicates when the agent is actively editing code
  // Used to trigger blue cursor/selection theme
  agent_looping: bool,
};

// Allow for the displaying of chats in chronological order.
let sorted_chats = (chat_map: Id.Map.t(chat)): list(chat) => {
  chat_map
  |> Id.Map.bindings
  |> List.map(((_, chat)) => chat)
  |> List.sort((a, b) => int_of_float(b.timestamp -. a.timestamp));
};

// --- Helper Functions ---

let get_messages_content =
    (messages: list(message)): list(OpenRouter.message) => {
  List.filter_map(message => message.content, messages);
};

let get_messages_display = (messages: list(message)): list(display) => {
  List.filter_map(message => message.display, messages);
};

let mk_mode_prompt = (~mode: AssistantSettings.mode): OpenRouter.message => {
  let prompt =
    switch (mode) {
    | HazelTutor => InitPrompts.mk_tutor()
    | CodeSuggestion =>
      InitPrompts.mk_suggestion(
        ChatLSP.Options.init,
        "code_suggestion",
        false,
      )
    | TaskCompletion => InitPrompts.mk_composition()
    };
  prompt;
};

let mk_message_display = (~content: string): display => {
  {
    displayable_content: [Text(content)],
    raw_content: content,
    collapsed: true,
  };
};

let init_chat = (mode: AssistantSettings.mode): chat => {
  let init_message = mk_mode_prompt(~mode);
  {
    messages: [
      {
        content: Some(init_message),
        display: Some(mk_message_display(~content=init_message.content)),
        role: System(AssistantPrompt),
        sketch_snapshot: None,
      },
    ],
    id: Id.mk(),
    descriptor: "",
    timestamp: JsUtil.timestamp(),
    context_usage: 0,
    awaiting_response: false,
  };
};

let new_chat = (model: t, mode: AssistantSettings.mode): chat => {
  let init_message =
    switch (mode) {
    | HazelTutor => model.init_prompt_data.init_tutor_chat.messages
    | CodeSuggestion =>
      model.init_prompt_data.init_suggestion_chat_basic.messages
    | TaskCompletion => model.init_prompt_data.init_composition_chat.messages
    };
  {
    messages: init_message,
    id: Id.mk(),
    descriptor: "",
    timestamp: JsUtil.timestamp(),
    context_usage: 0,
    awaiting_response: false,
  };
};

let add_chat_to_history =
    (chat: chat, history: Id.Map.t(chat)): Id.Map.t(chat) =>
  Id.Map.add(chat.id, chat, history);

let init = (): t => {
  let (init_tutor_chat, init_suggestion_chat, init_composition_chat) = (
    init_chat(HazelTutor),
    init_chat(CodeSuggestion),
    init_chat(TaskCompletion),
  );
  {
    init_prompt_data: {
      init_tutor_chat,
      init_composition_chat,
      init_suggestion_chat_basic: init_suggestion_chat,
      init_suggestion_chat_cot: init_suggestion_chat,
    },
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
    external_api_info: {
      available_models: [],
      set_model_info: {
        id: "n/a",
        name: "n/a",
        context_length: 0,
        pricing: {
          prompt: "n/a",
          completion: "n/a",
        },
      },
      api_key: "",
    },
    agent_looping: false,
  };
};

// We defer true initialization of the assistant model until the user opens the chat interface.
let null_model = (): t => {
  let null_chat = {
    messages: [],
    id: Id.invalid,
    descriptor: "Please set an API key",
    timestamp: JsUtil.timestamp(),
    context_usage: 0,
    awaiting_response: false,
  };
  {
    init_prompt_data: {
      init_tutor_chat: null_chat,
      init_composition_chat: null_chat,
      init_suggestion_chat_basic: null_chat,
      init_suggestion_chat_cot: null_chat,
    },
    current_chats: {
      curr_tutor_chat: null_chat.id,
      curr_suggestion_chat: null_chat.id,
      curr_composition_chat: null_chat.id,
    },
    chat_history: {
      past_tutor_chats: add_chat_to_history(null_chat, Id.Map.empty),
      past_suggestion_chats: add_chat_to_history(null_chat, Id.Map.empty),
      past_composition_chats: add_chat_to_history(null_chat, Id.Map.empty),
    },
    external_api_info: {
      available_models: [],
      set_model_info: {
        id: "n/a",
        name: "n/a",
        context_length: 0,
        pricing: {
          prompt: "n/a",
          completion: "n/a",
        },
      },
      api_key: "",
    },
    agent_looping: false,
  };
};

[@deriving (show({with_path: false}), yojson, sexp)]
type model = t;
