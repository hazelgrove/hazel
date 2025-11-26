open Util;

/* Update Action Types are defined here to avoid circular dependencies */

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
  | CompletionErrorRound(Zipper.t, int, Id.t)
  | CompletionQueryResponse
  | CompositionLoopRound(Zipper.t, int, bool);

// Actions which actualize actions via LLM responses
[@deriving (show({with_path: false}), sexp, yojson)]
type employ_llm_action =
  | RemoveAndSuggest(string, Id.t)
  | Describe(string, AssistantSettings.mode, Id.t)
  | Summarize(string, AssistantSettings.mode, Id.t)
  | Quit;

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
type caller =
  | Agent(status => unit)
  | User;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | SendMessage(send_message, option(Zipper.t), Id.t)
  | HandleResponse(handle_response, OpenRouter.reply, Id.t)
  | EmployLLMAction(employ_llm_action)
  | ChatAction(chat_action)
  | InternalError(string, AssistantSettings.mode, Id.t)
  | ExternalAPIAction(external_api_action)
  | InitializeAssistant
  | CompositionAgentWorkbenchAction(
      CompositionAgentWorkbench.Update.Action.action,
      caller,
      Id.t,
    );
