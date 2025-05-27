module Sexp = Sexplib.Sexp;
open Haz3lcore;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type system =
  | Error
  | Prompt;

[@deriving (show({with_path: false}), sexp, yojson)]
type party =
  | System(system)
  | LLM
  | User;

// Represents a code segment with an optional tile ID
// The outer option indicates if there is any code at all
// The inner option indicates if the code is associated with a specific tile
[@deriving (show({with_path: false}), sexp, yojson)]
type code_segment = option((Segment.t, option(Id.t)));

[@deriving (show({with_path: false}), sexp, yojson)]
type block_kind =
  | Text(string)
  | Code(Segment.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type message = {
  party, // Who sent the message (System, LLM, or LS)
  content: string, // The text content of the message
  displayable_content: list(block_kind), // The text/code blocks to display (we opt to store these for efficiency)
  collapsed: bool // Whether the message is collapsed in the UI
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
};

// This is important when we need to display the history of chats in chronological order.
let sorted_chats = (chat_map: Id.Map.t(chat)): list(chat) => {
  chat_map
  |> Id.Map.bindings
  |> List.map(((_, chat)) => chat)
  |> List.sort((a, b) => int_of_float(b.timestamp -. a.timestamp));
};

let max_collapsed_length: int = 500;
