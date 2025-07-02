open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type system =
  // Issue that we catch and inform the user about.
  // Do not send this to the model and display to user as error message
  | InternalError
  // The system prompt for the model.
  // Send this to the model.
  // Display to user as expandable/collapsable system message.
  | AssistantPrompt;

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

[@deriving (show({with_path: false}), sexp, yojson)]
type block_kind =
  | Text(string)
  | Code(Haz3lcore.Segment.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type display = {
  displayable_content: list(block_kind),
  original_content: string,
  role,
  collapsed: bool,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type chat = {
  outgoing_messages: list(OpenRouter.message),
  message_displays: list(display),
  id: Id.t,
  descriptor: string,
  timestamp: float,
};

// We save the history of past chats as a hash map with chat IDs as keys.
[@deriving (show({with_path: false}), sexp, yojson)]
type chat_history = {
  // History logs of past chats stored as hash maps with chat IDs as keys
  past_tutor_chats: Id.Map.t(chat),
  past_suggestion_chats: Id.Map.t(chat),
  past_composition_chats: Id.Map.t(chat),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type current_chats = {
  // Current active chat IDs for each mode
  curr_tutor_chat: Id.t,
  curr_suggestion_chat: Id.t,
  curr_composition_chat: Id.t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type external_api_info = {
  available_models: list(OpenRouter.model_info),
  set_model: string,
  api_key: string,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  current_chats,
  chat_history,
  external_api_info,
  loop: bool,
};

// This is important when we need to display the history of chats in chronological order.
let sorted_chats = (chat_map: Id.Map.t(chat)): list(chat) => {
  chat_map
  |> Id.Map.bindings
  |> List.map(((_, chat)) => chat)
  |> List.sort((a, b) => int_of_float(b.timestamp -. a.timestamp));
};

// --- Constant Magic Ints ---
let max_collapsed_length: int = 500;
// --------------------------------
