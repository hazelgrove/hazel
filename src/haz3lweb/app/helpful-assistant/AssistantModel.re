module Sexp = Sexplib.Sexp;
open Haz3lcore;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type party =
  | System
  | LLM
  | LS;

// Represents a code segment with an optional tile ID
// The outer option indicates if there is any code at all
// The inner option indicates if the code is associated with a specific tile
[@deriving (show({with_path: false}), sexp, yojson)]
type code_segment = option((Segment.t, option(Id.t)));

[@deriving (show({with_path: false}), sexp, yojson)]
type message = {
  party, // Who sent the message (System, LLM, or LS)
  code: code_segment, // Optional code segment with optional tile ID
  content: string, // The text content of the message
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

let init_simple_chat = {
  messages: [],
  id: Id.mk(),
  descriptor: "",
  timestamp: JsUtil.timestamp(),
};
let init_suggestion_chat = {
  messages: [],
  id: Id.mk(),
  descriptor: "",
  timestamp: JsUtil.timestamp(),
};
let init_completion_chat = {
  messages: [],
  id: Id.mk(),
  descriptor: "",
  timestamp: JsUtil.timestamp(),
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
};
