open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type mode =
  | CodeSuggestion
  | TaskCompletion
  | HazelTutor;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  mode,
  ongoing_chat: bool,
  show_history: bool,
  show_api_key: bool,
};

// Note: Settings actions are handled in Settings.re
[@deriving (show({with_path: false}), sexp, yojson)]
type action =
  // Flips between ongoing and non-ongoing chat
  | UpdateChatStatus
  // Switches the mode of the assistant (Suggest, Complete, Compose)
  // Future Idea: Combine modes into a single interface, similar to what Cursor or Copilot does
  | SwitchMode(mode)
  // Toggles the chat history to be displayed or collapsed, adjacent to the sidebar-chat interface
  | ToggleHistory
  // Toggles the visibility of the API key in the settings menu
  | ToggleAPIKeyVisibility;

// --- Constant Magic Ints ---
let make_descriptor_max = 3;

let max_collapsed_length: int = 500;

let context_threshold_ratio: float = 0.3;
// --- End Constant Magic Ints ---
