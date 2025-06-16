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
  available_models: list(OpenRouter.model_info),
};

// Note: Settings actions are handled in Settings.re
[@deriving (show({with_path: false}), sexp, yojson)]
type action =
  // Flips between ongoing and not ongoing chat
  | UpdateChatStatus
  // Switches the mode of the assistant (Suggest, Complete, Compose)
  | SwitchMode(mode)
  // Toggles the chat history
  | ToggleHistory
  // Toggles the visibility of the API key in the settings menu
  | ToggleAPIKeyVisibility
  // Sets the LLM model
  | SetLLM(string)
  // Sets the API key
  | SetAPIKey(string)
  // Sets the list of available LLMs from OpenRouter
  | SetListOfLLMs(list(OpenRouter.model_info));

let make_descriptor_max = 3;
