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
type init_prompt_data = {
  init_tutor_chat: chat,
  init_composition_chat: chat,
  init_suggestion_chat_basic: chat,
  init_suggestion_chat_cot: chat,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  current_chats,
  chat_history,
  external_api_info,
  init_prompt_data,
  loop: bool,
};

// This is important when we need to display the history of chats in chronological order.
let sorted_chats = (chat_map: Id.Map.t(chat)): list(chat) => {
  chat_map
  |> Id.Map.bindings
  |> List.map(((_, chat)) => chat)
  |> List.sort((a, b) => int_of_float(b.timestamp -. a.timestamp));
};

let max_collapsed_length: int = 500;

let mk_mode_prompt =
    (~mode: AssistantSettings.mode): option(OpenRouter.message) => {
  let prompt =
    switch (mode) {
    | HazelTutor => Some(InitPrompts.mk_tutor())
    | CodeSuggestion =>
      Some(
        ChatLSP.Completion.mk_const_prompt(
          ChatLSP.Options.init,
          "code_suggestion",
          false,
        ),
      )
    | TaskCompletion => Some(InitPrompts.mk_composition())
    };
  prompt;
};

let parse_blocks = (response: string): list(block_kind) => {
  let rec parse_blocks =
          (str: string, acc: list(block_kind)): list(block_kind) => {
    open Haz3lcore;
    let pattern = Str.regexp("```[ \n]*\\([^`]+\\)[ \n]*```");
    switch (Str.search_forward(pattern, str, 0)) {
    | exception Not_found => acc
    | pos =>
      let acc = ListUtil.leading(acc);
      let code = Str.matched_group(1, str);
      let zipper_of_code = Parser.to_zipper(code, ~root=Exp);
      let sketch_z =
        switch (zipper_of_code) {
        | Some(z) => z
        | None =>
          print_endline("Failed to parse content into segment.\n");
          Zipper.init(~root=Exp);
        };
      let sketch = Dump.to_segment(sketch_z);
      let before = Str.string_before(str, pos);
      let rest_start = pos + String.length(Str.matched_string(str));
      if (rest_start >= String.length(str)) {
        acc @ [Text(before), Code(sketch)];
      } else {
        let rest = Str.string_after(str, rest_start);
        parse_blocks(
          rest,
          acc @ [Text(before), Code(sketch), Text(rest)],
        );
      };
    };
  };
  parse_blocks(response, [Text(response)]);
};

let mk_message_display = (~content: string, ~role: role): display => {
  {
    displayable_content: [Text(content)],
    original_content: content,
    role,
    collapsed:
      String.length(content) > max_collapsed_length
      || role == System(AssistantPrompt),
  };
};

let init_chat = (mode: AssistantSettings.mode): chat => {
  let (init_message, init_message_display) =
    switch (mk_mode_prompt(~mode)) {
    | Some(init_message) => (
        [init_message],
        [
          mk_message_display(
            ~content=init_message.content,
            ~role=System(AssistantPrompt),
          ),
        ],
      )
    | None => ([], [])
    };

  {
    outgoing_messages: init_message,
    message_displays: init_message_display,
    id: Id.mk(),
    descriptor: "",
    timestamp: JsUtil.timestamp(),
  };
};

let new_chat = (model: t, mode: AssistantSettings.mode): chat => {
  let (init_message, init_message_display) =
    switch (mode) {
    | HazelTutor => (
        model.init_prompt_data.init_tutor_chat.outgoing_messages,
        model.init_prompt_data.init_tutor_chat.message_displays,
      )
    | CodeSuggestion => (
        model.init_prompt_data.init_suggestion_chat_basic.outgoing_messages,
        model.init_prompt_data.init_suggestion_chat_basic.message_displays,
      )
    | TaskCompletion => (
        model.init_prompt_data.init_composition_chat.outgoing_messages,
        model.init_prompt_data.init_composition_chat.message_displays,
      )
    };
  {
    outgoing_messages: init_message,
    message_displays: init_message_display,
    id: Id.mk(),
    descriptor: "",
    timestamp: JsUtil.timestamp(),
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
      set_model: "",
      api_key: "",
    },
    loop: false,
  };
};

// We defer true initialization of the assistant model until the user opens the chat interface.
let null_model = (): t => {
  let null_chat = {
    outgoing_messages: [],
    message_displays: [
      mk_message_display(
        ~content=
          "Please set an API key in the settings to start using the Hazel Assistant.",
        ~role=System(InternalError),
      ),
    ],
    id: Id.invalid,
    descriptor: "Please set an API key",
    timestamp: JsUtil.timestamp(),
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
      set_model: "",
      api_key: "",
    },
    loop: false,
  };
};

[@deriving (show({with_path: false}), yojson, sexp)]
type model = t;

module Store =
  Store.F({
    [@deriving (show({with_path: false}), yojson, sexp)]
    type t = model;
    let default = () => null_model();

    let key = Store.Assistant;
  });
