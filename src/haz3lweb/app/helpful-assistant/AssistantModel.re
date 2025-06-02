module Sexp = Sexplib.Sexp;
open Haz3lcore;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type system =
  // Issue that we catch and inform the user about. Do not send this to the model.
  | InternalError
  // The system prompt that we send to the model.
  | AssistantPrompt;

[@deriving (show({with_path: false}), sexp, yojson)]
type role =
  | System(system)
  | User
  | Assistant;

let string_of_role =
  fun
  | System(AssistantPrompt) => "System"
  | System(InternalError) => "Error"
  | User => "User"
  | Assistant => "Assistant";

[@deriving (show({with_path: false}), sexp, yojson)]
type block_kind =
  | Text(string)
  | Code(Segment.t);

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
type t = {
  current_chats,
  chat_history,
};

// This is important when we need to display the history of chats in chronological order.
let sorted_chats = (chat_map: Id.Map.t(chat)): list(chat) => {
  chat_map
  |> Id.Map.bindings
  |> List.map(((_, chat)) => chat)
  |> List.sort((a, b) => int_of_float(b.timestamp -. a.timestamp));
};

let max_collapsed_length: int = 500;
