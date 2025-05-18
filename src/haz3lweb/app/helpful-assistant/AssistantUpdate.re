module Sexp = Sexplib.Sexp;
open Haz3lcore;
open Util;
open Util.OptUtil.Syntax;

module CodeModel = CodeEditable.Model;

module Model = AssistantModel;

// Actions to send various kinds of messages to the LLM
[@deriving (show({with_path: false}), sexp, yojson)]
type send_message =
  | Basic(Model.message)
  | Sketch(Id.t, AssistantSettings.mode, bool)
  | ErrorRound(
      string,
      Zipper.t,
      Info.t,
      int,
      Id.t,
      AssistantSettings.mode,
      Id.t,
    )
  | System(string, AssistantSettings.mode, Id.t);

// Actions to handle certain kinds of LLM responses
[@deriving (show({with_path: false}), sexp, yojson)]
type handle_response =
  | Basic(Model.message, AssistantSettings.mode, Id.t)
  | ErrorRound(
      string,
      Zipper.t,
      Info.t,
      int,
      Id.t,
      AssistantSettings.mode,
      Id.t,
    );

// Actions which actualize actions via LLM responses
[@deriving (show({with_path: false}), sexp, yojson)]
type employ_llm_action =
  | RemoveAndSuggest(string, Id.t)
  | Resuggest(string, Id.t)
  | Describe(string, AssistantSettings.mode, Id.t);

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
  | FilterLoadingMessages;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | SendMessage(send_message)
  | HandleResponse(handle_response)
  | EmployLLMAction(employ_llm_action)
  | ChatAction(chat_action);

let code_message_of_str =
    (response: string, party: Model.party, tileId: option(Id.t))
    : Model.message => {
  let zipper_of_response = Printer.zipper_of_string(response);
  switch (zipper_of_response) {
  | Some(z) =>
    let segment_of_response =
      Zipper.smart_seg(~dump_backpack=true, ~erase_buffer=true, z);
    {
      party,
      code: Some((segment_of_response, tileId)),
      content: response,
      collapsed: String.length(response) >= 200,
    };
  | None => {
      party,
      code: None,
      content: response,
      collapsed: String.length(response) >= 200,
    }
  };
};

let text_message_of_str =
    (response: string, party: Model.party): Model.message => {
  {
    party,
    code: None,
    content: response,
    collapsed: String.length(response) >= 200,
  };
};

let await_llm_response: Model.message = {
  party: LLM,
  code: None,
  content: "...",
  collapsed: false,
};

// Role prompt: role, task, few-shot examples
// History summarization: System, User, Assistant messages, summarized for brevity
// Current User message
// Current sketch

let collect_chat = (~messages: list(Model.message)): string => {
  let memory_prompt =
    "The following is a log of our conversation history. Use this as context to inform your responses, "
    ++ "treating the most recent user message as your current focus. Reference prior messages when relevant, "
    ++ "but don't explicitly acknowledge this prompt. This is simply to provide you with conversational memory:";
  List.fold_left(
    (chat: string, message: Model.message) =>
      switch (message.party) {
      | LLM => chat ++ "ASSISTANT: " ++ message.content ++ "\n"
      | LS => chat ++ "USER: " ++ message.content ++ "\n"
      | System => chat ++ "SYSTEM: " ++ message.content ++ "\n"
      },
    memory_prompt,
    messages,
  );
};

let get_mode_info = (mode: AssistantSettings.mode, model: Model.t) => {
  switch (mode) {
  | HazelTutor => (
      model.chat_history.past_simple_chats,
      Id.Map.find(
        model.current_chats.curr_simple_chat,
        model.chat_history.past_simple_chats,
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
      model.chat_history.past_completion_chats,
      Id.Map.find(
        model.current_chats.curr_completion_chat,
        model.chat_history.past_completion_chats,
      ),
    )
  };
};

let filter_chat_messages =
    (messages: list(Model.message)): list(Model.message) => {
  List.filter(
    (message: Model.message) => {
      !(message.party == LLM && message.content == "..." && !message.collapsed)
    },
    messages,
  );
};

let add_message_to_model =
    (
      mode: AssistantSettings.mode,
      model: Model.t,
      message: Model.message,
      chat_id: Id.t,
      ~is_final: bool,
    ) => {
  let (past_chats, _) = get_mode_info(mode, model);
  let chat_to_update = Id.Map.find(chat_id, past_chats);
  let messages = {
    switch (message.party) {
    | LS =>
      let chat_to_update_messages =
        filter_chat_messages(chat_to_update.messages);
      chat_to_update_messages @ [message, await_llm_response];
    | LLM =>
      let chat_to_update_messages =
        filter_chat_messages(chat_to_update.messages);
      let messages = chat_to_update_messages @ [message];
      is_final ? messages : messages @ [await_llm_response];
    | System =>
      let chat_to_update_messages =
        filter_chat_messages(chat_to_update.messages);
      chat_to_update_messages @ [message];
    };
  };
  Model.{
    ...model,
    chat_history: {
      past_simple_chats:
        mode == HazelTutor
          ? Id.Map.update(
              chat_to_update.id,
              maybe_chat =>
                switch (maybe_chat) {
                | Some(chat) =>
                  Some({
                    ...chat,
                    messages,
                  })
                | None => None
                },
              model.chat_history.past_simple_chats,
            )
          : model.chat_history.past_simple_chats,
      past_suggestion_chats:
        mode == CodeSuggestion
          ? Id.Map.update(
              chat_to_update.id,
              maybe_chat =>
                switch (maybe_chat) {
                | Some(chat) =>
                  Some({
                    ...chat,
                    messages,
                  })
                | None => None
                },
              model.chat_history.past_suggestion_chats,
            )
          : model.chat_history.past_suggestion_chats,
      past_completion_chats:
        mode == TaskCompletion
          ? Id.Map.update(
              chat_to_update.id,
              maybe_chat =>
                switch (maybe_chat) {
                | Some(chat) =>
                  Some({
                    ...chat,
                    messages,
                  })
                | None => None
                },
              model.chat_history.past_completion_chats,
            )
          : model.chat_history.past_completion_chats,
    },
  };
};

let resculpt_model =
    (
      mode: AssistantSettings.mode,
      model: Model.t,
      past_chats: Id.Map.t(Model.chat),
      chat_id: Id.t,
    ) => {
  Model.{
    chat_history: {
      past_simple_chats:
        mode == HazelTutor ? past_chats : model.chat_history.past_simple_chats,
      past_suggestion_chats:
        mode == CodeSuggestion
          ? past_chats : model.chat_history.past_suggestion_chats,
      past_completion_chats:
        mode == TaskCompletion
          ? past_chats : model.chat_history.past_completion_chats,
    },
    // This is tentative. Keep this if we want the user to be shown the most recent chat.
    // Remove this if we want the user to be shown the chat they last/currently interact with.
    // This is honestly such an edge case that it probably doesn't matter.
    current_chats: {
      curr_simple_chat:
        mode == HazelTutor ? chat_id : model.current_chats.curr_simple_chat,
      curr_suggestion_chat:
        mode == CodeSuggestion
          ? chat_id : model.current_chats.curr_suggestion_chat,
      curr_completion_chat:
        mode == TaskCompletion
          ? chat_id : model.current_chats.curr_completion_chat,
    },
  };
};

let standardize_prompt = (body: string): option(OpenRouter.prompt) => {
  switch (String.trim(body)) {
  | "" => None
  | _ =>
    let input = [
      {
        OpenRouter.role: User,
        OpenRouter.content: body,
      },
    ];
    Some(input);
  };
};

let form_descriptor =
    (~schedule_action, ~chat: Model.chat, ~mode: AssistantSettings.mode): unit => {
  let prompt =
    switch (mode) {
    | HazelTutor => "Your main task is to provide a summarizing title of the following conversation, in less than or equal to 5 words, and include 1 or 2 emojis. \n            DO NOT exceed 7 words. Only provide the summarizing title in your response, do not include any other text. Here is the\n            concatenated conversation, with your response and the user's responses, respectively: "
    | CodeSuggestion => "Your main task is to provide a summarizing title of the following conversation, in less than or equal to 5 words, and include 1 or 2 emojis.\n            DO NOT exceed 7 words. Only provide the summarizing title in your response, do not include any other text. This conversation is known to be a code\n            completion conversation. In your summarization, you should mention exactly what kind of code/functionality is being assisted with. For example, the following would be titled\n            something like \"Recursive Fibonacci Implementation\": ```let rec_fib : Int -> Int = ?? in ?```. Here is the\n            concatenated conversation, with your response and the user's responses, respectively: "
    | TaskCompletion => "Your main task is to provide a summarizing title of the following conversation, in less than or equal to 5 words, and include 1 or 2 emojis.\n            DO NOT exceed 7 words. Only provide the summarizing title in your response, do not include any other text. This conversation is known to be a task completion conversation.\n            In your summarization, you should mention exactly what kind of task is being completed. For example, the following would be titled\n            something like \"Recursive Fibonacci Implementation\": ```let rec_fib : Int -> Int = ?? in ?```. Here is the\n            concatenated conversation, with your response and the user's responses, respectively: "
    };
  let prompt =
    List.fold_left(
      (chat: string, message: Model.message) =>
        if (message.party == LLM) {
          chat ++ "Your Reponse: " ++ message.content ++ " ";
        } else if (message.party == LS) {
          chat ++ "User Input: " ++ message.content ++ " ";
        } else {
          chat ++ message.content;
        },
      prompt,
      chat.messages,
    );
  switch (standardize_prompt(prompt)) {
  | None => print_endline("Prompt generation failed")
  | Some(prompt') =>
    let model_id = Option.get(Store.Generic.load("MODEL"));
    let key = Option.get(Store.Generic.load("API"));
    let params: OpenRouter.params = {
      model_id,
      temperature: 1.0,
      top_p: 1.0,
    };
    OpenRouter.start_chat(~params, ~key, prompt', req =>
      switch (OpenRouter.handle_chat(req)) {
      | Some(Reply({content, _})) =>
        schedule_action(EmployLLMAction(Describe(content, mode, chat.id)))
      | Some(Error(_)) => () // Don't need to handle error since we have "New Chat" descriptor as failsafe
      | None =>
        print_endline("Assistant: response parse failed (form_descriptor)")
      }
    );
  };
};

let check_descriptor =
    (
      ~model: Model.t,
      ~schedule_action,
      ~message: Model.message,
      ~mode: AssistantSettings.mode,
      ~chat_id: Id.t,
    )
    : unit => {
  let (past_chats, _) = get_mode_info(mode, model);
  let curr_chat = Id.Map.find(chat_id, past_chats);
  List.length(curr_chat.messages) <= 6
    ? form_descriptor(
        ~schedule_action,
        ~chat={
          ...curr_chat,
          messages: curr_chat.messages @ [message],
        },
        ~mode,
      )
    : ();
  // Only create a summary up to the first few exchanges
};

let check_req =
    (_: string, schedule_action: t => unit, editor: CodeEditable.Model.t)
    : unit => {
  let z = editor.editor.state.zipper;
  let caret = z.caret;
  let siblings = z.relatives.siblings;

  // Check if user just typed ??
  switch (caret, Zipper.neighbor_monotiles(siblings)) {
  | (Outer, (_, Some(_))) =>
    switch (Zipper.right_neighbor_monotile(siblings)) {
    | Some(c) =>
      switch (c) {
      | "??" =>
        let tileId = Option.get(Indicated.index(z));
        let advanced_reasoning = false;
        schedule_action(
          SendMessage(
            Sketch(
              tileId,
              AssistantSettings.CodeSuggestion,
              advanced_reasoning,
            ),
          ),
        );
      | "?a" =>
        let tileId = Option.get(Indicated.index(z));
        let advanced_reasoning = true;
        schedule_action(
          SendMessage(
            Sketch(
              tileId,
              AssistantSettings.CodeSuggestion,
              advanced_reasoning,
            ),
          ),
        );
      | _ => ()
      }
    | _ => ()
    }
  | (Outer, (_, None)) =>
    switch (Zipper.left_neighbor_monotile(siblings)) {
    | Some(c) =>
      switch (c) {
      | "??" =>
        let tileId = Option.get(Indicated.index(z));
        let advanced_reasoning = false;
        schedule_action(
          SendMessage(
            Sketch(
              tileId,
              AssistantSettings.CodeSuggestion,
              advanced_reasoning,
            ),
          ),
        );
      | "?a" =>
        let tileId = Option.get(Indicated.index(z));
        let advanced_reasoning = true;
        schedule_action(
          SendMessage(
            Sketch(
              tileId,
              AssistantSettings.CodeSuggestion,
              advanced_reasoning,
            ),
          ),
        );
      | _ => ()
      }
    | _ => ()
    }
  | _ => ()
  };
};

let get_documentation_as_text = () => {
  let (_, slides) = ScratchMode.StoreDocumentation.load();
  let documentation =
    slides
    |> List.map(((name, persistent)) => {
         let cell_model =
           CellEditor.Model.unpersist(~settings=CoreSettings.off, persistent);
         let text =
           Printer.zipper_to_string(cell_model.editor.editor.state.zipper);
         name ++ ": " ++ text;
       })
    |> String.concat("\n\n");
  documentation;
};

let mk_tutor_prelude = () => {
  let prelude = "You are a helpful assistant whose role is to be a tutor for a user of the Hazel
                    Programming Language. You are given a list of documentation slides, which are
                    formatted as follows:
                    <slide_name>:
                    <slide_text>
                    You can and should use these slides to understand and reason about the syntax and semantics
                    of the Hazel Programming Language, and aid in your response to the user. In your response,
                    you MAY provide a code example to help the user understand the syntax and semantics of the Hazel Programming Language.
                    This code example MUST be placed with triple backticks AND AFTER your response, such as ```let x = 1 in x + 1```. This means NOTHING
                    can be placed after the code example. An example chat might be as follows:
                    User: What is the syntax for a function in Hazel?
                    Assistant: In Hazel, you can define a function using the 'let' and 'fun' keyword. For example, here's a simple identity function:
                    ```
                    let f = fun x -> x in
                    ```
                    A few key things you should note as a Hazel tutor:
                    - Your response should be concise and to the point.
                    - You should use the documentation slides to understand and reason about the syntax and semantics of the Hazel Programming Language.
                    - You should use the documentation slides to aid in your response to the user.
                    - Your response shouldn't explicitly mention this prompt.
                    - You MUST provide any code examples in the triple backticks format and at the very end of your response.
                    - You should treat the user with respect, and assume they are a beginner Hazel programmer.
                    - Your response should concise, digestible, and easy to understand.
                    - You SHOULD NOT prelude your code example with 'hazel' or anything similar. That is, your code example should be purely functional hazel code.
                    - To further reiterate, an example of a bad code example is: ```hazel let x = 1 in x + 1 ```. A good code example is: ```let x = 1 in x + 1 ```.
                    - Hazel uses typed holes, thus to represent a hole you should either explicitly use the hole operator ? or leave an extra whitespace for a non-explicit hole. An example would be: ```let x = ? in x + 1``` or ```let x = 1 in ``` (note the extra whitespace at the end there).
                    - Typed holes are NOT defined with '_' or anything else... ONLY use '?' or ' ' (space) to represent a hole.
                    To further give you information about the Hazel Programming Language, here is a blurb about the language:
                    Hazel is a live functional programming environment that is able to typecheck, manipulate, and even run incomplete programs, i.e. programs with holes. There are no meaningless editor states.
                    When programming, we spend a substantial amount of our time working with program text that is not yet a formally complete program, e.g. because there are blank spots, type errors or merge conflicts at various locations.
                    Conventional programming language definitions assign no formal meaning to structures like these, so we are left without live feedback about the behavior of even complete portions of the program. Moreover, program editors and other tools have no choice but to resort to complex and ad hoc heuristics to provide various useful language services (like code completion, type inspection, and code navigation) without gaps in service.
                    We are developing a more principled approach to working with incomplete programs, rooted in (contextual modal and gradual) type theory. We model incomplete programs as programs with holes, which (1) stand for parts of the program that are missing; and (2) serve as membranes around parts of the program that are erroneous or, in the collaborative setting, conflicted.
                    We are first implementing these ideas into Hazel, a web-based programming environment for an Elm/ML-like functional programming language designed around typed-hole-driven development.
                    Uniquely, every incomplete program that you can construct using Hazel's language of edit actions is both statically and dynamically well-defined, i.e. it has a (possibly incomplete) type, and you can run it to produce a (possibly incomplete) result. Consequently, Hazel serves as an elegant platform for research on the future of programming (and programming education).
                    ";
  prelude ++ "\n\n" ++ get_documentation_as_text();
};

let set_buffer = (~response: string, z: Zipper.t): option(Zipper.t) => {
  let zipper_of_response = Option.get(Printer.zipper_of_string(response));
  let seg_of_response =
    Zipper.smart_seg(
      ~dump_backpack=true,
      ~erase_buffer=true,
      zipper_of_response,
    );
  let z = Zipper.set_buffer(z, ~content=seg_of_response, ~mode=Unparsed);
  Some(z);
};

let mk_LLM_call =
    (
      ~model: Model.t,
      ~curr_chat: Model.chat,
      ~prompt: list(OpenRouter.message),
      ~message: Model.message,
      ~mode: AssistantSettings.mode,
      ~schedule_action: t => unit,
    ) => {
  switch (Store.Generic.load("API"), Store.Generic.load("MODEL")) {
  | (Some(key), Some(model_id)) =>
    let params: OpenRouter.params = {
      model_id,
      temperature: 1.0,
      top_p: 1.0,
    };
    OpenRouter.start_chat(~params, ~key, prompt, req =>
      switch (OpenRouter.handle_chat(req)) {
      | Some(Reply({content, _})) =>
        schedule_action(
          HandleResponse(
            Basic(text_message_of_str(content, LLM), mode, curr_chat.id),
          ),
        )
      | Some(Error({message, code})) =>
        schedule_action(
          SendMessage(
            System(
              "Error: " ++ message ++ " (code: " ++ string_of_int(code) ++ ")",
              mode,
              curr_chat.id,
            ),
          ),
        )
      | None =>
        print_endline("Assistant: response parse failed (SendTutorMessage)")
      }
    );
    add_message_to_model(mode, model, message, curr_chat.id, ~is_final=true)
    |> Updated.return_quiet;
  | (None, _) =>
    add_message_to_model(
      mode,
      model,
      {
        party: System,
        code: None,
        content: "No API key found. Please set an API key in the assistant settings.",
        collapsed: false,
      },
      curr_chat.id,
      ~is_final=true,
    )
    |> Updated.return_quiet
  | (_, None) =>
    add_message_to_model(
      mode,
      model,
      {
        party: System,
        code: None,
        content: "No model ID found. Please set a model ID in the assistant settings.",
        collapsed: false,
      },
      curr_chat.id,
      ~is_final=true,
    )
    |> Updated.return_quiet
  };
};

let failed_prompt_generation =
    (~mode: AssistantSettings.mode, ~model: Model.t, ~curr_chat: Model.chat) => {
  add_message_to_model(
    mode,
    model,
    {
      party: System,
      code: None,
      content: "Prompt generation failed.",
      collapsed: false,
    },
    curr_chat.id,
    ~is_final=true,
  )
  |> Updated.return_quiet;
};

let collect_tool_calls = (response: string): list(string) => {
  let rec extract_blocks = (text: string, acc: list(string)): list(string) => {
    let pattern = Str.regexp("```[ \n]*\\([^`]+\\)[ \n]*```");
    switch (Str.search_forward(pattern, text, 0)) {
    | exception Not_found => List.rev(acc)
    | pos =>
      let matched = Str.matched_group(1, text);
      let rest =
        String.sub(
          text,
          pos + String.length(Str.matched_string(text)),
          String.length(text)
          - (pos + String.length(Str.matched_string(text))),
        );
      extract_blocks(rest, [matched, ...acc]);
    };
  };
  extract_blocks(response, []);
};

let mk_mode_prompt =
    (~settings: AssistantSettings.t, ~model: Model.t, ~editor: CodeModel.t)
    : string => {
  let mode = settings.mode;
  let (_, curr_chat) = get_mode_info(mode, model);
  let prompt =
    switch (mode) {
    | HazelTutor =>
      let tutor_prelude = mk_tutor_prelude();
      // If the chat is just beginning, let us add the tutor prelude
      let tutor_chat =
        List.length(curr_chat.messages) == 0 ? tutor_prelude : "";
      tutor_chat;
    | CodeSuggestion =>
      // Just leave as is, no prelude needed, already prompted in ChatLSP
      // Messages are typically sent during code completion
      ""
    | TaskCompletion =>
      // Task completion will go as follows:
      // 1. User will type in desired functionality and send message. It will be
      //    this message being sent here
      // 2. This will be sent, along with the prompt (toolkit + few-shot examples + documentation?)
      // 3. The LLM will respond with a response, from the toolkit. This will iterate
      //    until the LLM responds with "submit".
      // IMPORTANT: In step 2, and steps 3.1...3.n, we will need to give the LLM
      // a sketch of the program, and an idea of "where" it currently is (like a cursor location).
      // This can simply be added to the prompt.
      // Thus, let us construct the prompt as follows:
      // 1. Add the prelude
      // 2. Add the few-shot examples
      // 3. Add the documentation
      // 4. Add the sketch of the program
      // 5. Add the cursor location
      // 6. Add the user task-to-be-completed
      let sketch_seg =
        Zipper.smart_seg(
          ~dump_backpack=true,
          ~erase_buffer=true,
          editor.editor.state.zipper,
        );
      let index = Option.get(Indicated.index(editor.editor.state.zipper));
      // Prompting fails here if the cursor is over whitespace, since find_opt returns None
      // Is there some default ci value we could use?
      let ci = Option.get(Id.Map.find_opt(index, editor.statics.info_map)); // Fails here
      ChatLSP.Composition.mk_prompt(
        ChatLSP.Options.init,
        ci,
        sketch_seg,
        List.length(curr_chat.messages) == 0,
      );
    };
  prompt;
};

let update =
    (
      ~settings: Settings.t,
      ~action,
      ~editor: CodeModel.t,
      ~model: Model.t,
      ~schedule_action: t => unit,
      ~add_suggestion,
      ~goto,
      ~edit,
    )
    : Updated.t(Model.t) => {
  switch (action) {
  | SendMessage(kind) =>
    switch (kind) {
    | Basic(message) =>
      // Notes: The following could certainly be improved. The prompt_with_user_message
      //        is separate from the prompt_with_chat as we feed the latter as input to the LLM
      //        and the former is what is saved to the chat history (and (if needed) collected
      //        in prompt_with_chats in later SendTextMessage calls).
      let mode = settings.assistant.mode;
      // Capture the entire chat to give historical context to LLM
      let (_, curr_chat) = get_mode_info(mode, model);
      // Gathers info/prompt given the mode
      let prompt =
        mk_mode_prompt(~settings=settings.assistant, ~model, ~editor);
      // The user message input itself. This is the message that the user typed.
      let user_message = "USER MESSAGE/REQUEST: " ++ message.content;
      // The prompt concatenated with the user message
      let prompt_with_user_message = prompt ++ "\n\n" ++ user_message;
      // Collects the chat history, including our new message.
      let prompt_with_chat =
        collect_chat(
          ~messages=
            curr_chat.messages
            @ [text_message_of_str(prompt_with_user_message, LLM)],
        );
      switch (standardize_prompt(prompt_with_chat)) {
      | None => failed_prompt_generation(~mode, ~model, ~curr_chat)
      | Some(prompt) =>
        mk_LLM_call(
          ~model,
          ~curr_chat,
          ~prompt,
          ~message=text_message_of_str(prompt_with_user_message, LS),
          ~mode,
          ~schedule_action,
        )
      };
    | Sketch(tileId, mode, advanced_reasoning) =>
      // Capture the chat we're updating
      let (_, curr_chat) = get_mode_info(mode, model);
      let sketch_seg =
        Zipper.smart_seg(
          ~dump_backpack=true,
          ~erase_buffer=true,
          editor.editor.state.zipper,
        );
      let tag = String.sub(Id.to_string(tileId), 0, 3);
      switch (
        {
          let* sketch_z_with_tag =
            Perform.paste(editor.editor.state.zipper, tag);
          let sketch_seg =
            Zipper.smart_seg(
              ~dump_backpack=true,
              ~erase_buffer=true,
              sketch_z_with_tag,
            );
          let* index = Indicated.index(editor.editor.state.zipper);
          let+ ci = Id.Map.find_opt(index, editor.statics.info_map);
          ChatLSP.Completion.prompt(
            ChatLSP.Options.init,
            ci,
            sketch_seg,
            (advanced_reasoning ? "?a" : "??") ++ tag,
            advanced_reasoning,
          );
        }
      ) {
      | None =>
        print_endline("prompt generation failed");
        model |> Updated.return_quiet;
      | Some(openrouter_prompt) =>
        let messages =
          List.map(
            (msg: OpenRouter.message): string => {msg.content},
            openrouter_prompt,
          );
        let prompt = String.concat("\n", messages);
        let message: Model.message = {
          party: LS,
          code: Some((sketch_seg, None)),
          content: prompt,
          collapsed: String.length(prompt) >= 200,
        };
        switch (Store.Generic.load("API"), Store.Generic.load("MODEL")) {
        | (Some(key), Some(model_id)) =>
          let params: OpenRouter.params = {
            model_id,
            temperature: 1.0,
            top_p: 1.0,
          };
          OpenRouter.start_chat(~params, ~key, openrouter_prompt, req =>
            switch (OpenRouter.handle_chat(req)) {
            | Some(Reply({content, _})) =>
              let index =
                Option.get(Indicated.index(editor.editor.state.zipper));
              let ci =
                Option.get(Id.Map.find_opt(index, editor.statics.info_map));
              schedule_action(
                HandleResponse(
                  ErrorRound(
                    content,
                    editor.editor.state.zipper,
                    ci,
                    ChatLSP.Options.init.error_rounds_max,
                    tileId,
                    mode,
                    curr_chat.id,
                  ),
                ),
              );
            | Some(Error({message, code})) =>
              schedule_action(
                SendMessage(
                  System(
                    "Error: "
                    ++ message
                    ++ " (code: "
                    ++ string_of_int(code)
                    ++ ")",
                    mode,
                    curr_chat.id,
                  ),
                ),
              )
            | None =>
              print_endline("Non-error but None");
              print_endline(
                "Assistant: response parse failed (SendSketchMessage)",
              );
            }
          );
          add_message_to_model(
            mode,
            model,
            message,
            curr_chat.id,
            ~is_final=true,
          )
          |> Updated.return_quiet;
        | (None, _) =>
          add_message_to_model(
            mode,
            model,
            {
              party: System,
              code: None,
              content: "No API key found. Please set an API key in the assistant settings.",
              collapsed: false,
            },
            curr_chat.id,
            ~is_final=true,
          )
          |> Updated.return_quiet
        | (_, None) =>
          add_message_to_model(
            mode,
            model,
            {
              party: System,
              code: None,
              content: "No API key or model ID found. Please set an API key and model ID in the assistant settings.",
              collapsed: false,
            },
            curr_chat.id,
            ~is_final=true,
          )
          |> Updated.return_quiet
        };
      };
    | ErrorRound(error, sketch_z, ci, fuel, tileId, mode, chat_id) =>
      let error_message =
        text_message_of_str(
          "Your previous response caused the following error. Please fix it in your response: "
          ++ error,
          LS,
        );
      // check that fuel is not 0
      if (fuel < 0) {
        let model =
          add_message_to_model(
            mode,
            model,
            error_message,
            chat_id,
            ~is_final=true,
          );
        add_message_to_model(
          mode,
          model,
          {
            party: System,
            code: None,
            content:
              "By default we stop the assistant after "
              ++ string_of_int(ChatLSP.Options.init.error_rounds_max)
              ++ " error rounds. Thus, stopping.",
            collapsed: false,
          },
          chat_id,
          ~is_final=true,
        )
        |> Updated.return_quiet;
      } else {
        // TODO: We don't want to collect ENTIRE chat history here. We only want
        //       to collect the history beginning from the initial suggestion request.
        //       Otherwise, the prompt becomes too long in single message threads.
        let (_, curr_chat) = get_mode_info(mode, model);
        let collected_chat =
          collect_chat(~messages=curr_chat.messages @ [error_message]);
        switch (standardize_prompt(collected_chat)) {
        | None =>
          add_message_to_model(
            mode,
            model,
            {
              party: System,
              code: None,
              content: "Prompt generation failed.",
              collapsed: false,
            },
            chat_id,
            ~is_final=true,
          )
          |> Updated.return_quiet
        | Some(openrouter_prompt) =>
          switch (Store.Generic.load("API"), Store.Generic.load("MODEL")) {
          | (Some(key), Some(model_id)) =>
            let params: OpenRouter.params = {
              model_id,
              temperature: 1.0,
              top_p: 1.0,
            };
            OpenRouter.start_chat(~params, ~key, openrouter_prompt, req =>
              switch (OpenRouter.handle_chat(req)) {
              | Some(Reply({content, _})) =>
                schedule_action(
                  HandleResponse(
                    ErrorRound(
                      content,
                      sketch_z,
                      ci,
                      fuel,
                      tileId,
                      mode,
                      curr_chat.id,
                    ),
                  ),
                )
              | Some(Error({message, code})) =>
                schedule_action(
                  SendMessage(
                    System(
                      "Error: "
                      ++ message
                      ++ " (code: "
                      ++ string_of_int(code)
                      ++ ")",
                      mode,
                      curr_chat.id,
                    ),
                  ),
                )
              | None =>
                print_endline(
                  "Assistant: response parse failed (SendErrorMessage)",
                )
              }
            );
            add_message_to_model(
              mode,
              model,
              error_message,
              chat_id,
              ~is_final=true,
            )
            |> Updated.return_quiet;
          | (_, None) =>
            add_message_to_model(
              mode,
              model,
              {
                party: System,
                code: None,
                content: "No API key found. Please set an API key in the assistant settings. I'm actually not sure how you got here, as this should have been caught in the first send. This is a bug, and you should let someone know.",
                collapsed: false,
              },
              chat_id,
              ~is_final=true,
            )
            |> Updated.return_quiet
          | (None, _) =>
            add_message_to_model(
              mode,
              model,
              {
                party: System,
                code: None,
                content: "No model ID found. Please set a model ID in the assistant settings.",
                collapsed: false,
              },
              curr_chat.id,
              ~is_final=true,
            )
            |> Updated.return_quiet
          }
        };
      };
    | System(content, mode, chat_id) =>
      add_message_to_model(
        mode,
        model,
        {
          party: System,
          code: None,
          content,
          collapsed: false,
        },
        chat_id,
        ~is_final=true,
      )
      |> Updated.return_quiet
    }
  | HandleResponse(response) =>
    switch (response) {
    | Basic(message, mode, chat_id) =>
      let response = message.content;
      check_descriptor(~model, ~schedule_action, ~message, ~mode, ~chat_id);
      if (mode == HazelTutor || mode == CodeSuggestion) {
        let code_pattern =
          Str.regexp(
            "\\(\\(.\\|\n\\)*\\)```[ \n]*\\([^`]+\\)[ \n]*```\\(\\(.\\|\n\\)*\\)",
          );
        let (discussion, code_example) =
          if (Str.string_match(code_pattern, response, 0)) {
            let before = String.trim(Str.matched_group(1, response));
            let code = String.trim(Str.matched_group(3, response));
            (before, code |> StringUtil.trim_leading);
          } else {
            print_endline("Regex match failed for: " ++ response);
            (response |> StringUtil.trim_leading, "");
          };
        let discussion_message = text_message_of_str(discussion, LLM);
        if (code_example == "") {
          add_message_to_model(
            mode,
            model,
            discussion_message,
            chat_id,
            ~is_final=true,
          )
          |> Updated.return_quiet;
        } else {
          let model_with_discussion =
            add_message_to_model(
              mode,
              model,
              discussion_message,
              chat_id,
              ~is_final=false,
            );
          // Then handle the completion as before
          let message_with_example =
            code_message_of_str(code_example, LLM, None);
          add_message_to_model(
            mode,
            model_with_discussion,
            message_with_example,
            chat_id,
            ~is_final=true,
          )
          |> Updated.return_quiet;
        };
      } else {
        let tool_calls = collect_tool_calls(response);
        List.iter(
          (tool_call: string) => {print_endline("Tool call: " ++ tool_call)},
          tool_calls,
        );

        let rec process_tool_calls = (calls: list(string)) => {
          switch (calls) {
          | [] =>
            schedule_action(
              SendMessage(
                Basic(
                  text_message_of_str(
                    "SYSTEM: After your most recent edits, here is the current state of the code.",
                    LS,
                  ),
                ),
              ),
            )
          | [tool_call, ...rest] =>
            let parsed_response =
              switch (String.index_opt(tool_call, ' ')) {
              | Some(idx) => [
                  String.sub(tool_call, 0, idx),
                  String.sub(
                    tool_call,
                    idx + 1,
                    String.length(tool_call) - idx - 1,
                  ),
                ]
              | None => [tool_call]
              };
            let tool_call = List.hd(parsed_response);
            let arg =
              List.length(parsed_response) > 1
                ? Some(List.hd(List.tl(parsed_response))) : None;

            switch (tool_call) {
            | "goto_definition" =>
              goto(editor, Option.get(arg), ChatLSP.Composition.Definition);
              process_tool_calls(rest);
            | "goto_body" =>
              goto(editor, Option.get(arg), ChatLSP.Composition.Body);
              process_tool_calls(rest);
            | "edit" =>
              edit(Option.get(arg), ChatLSP.Composition.Current);
              process_tool_calls(rest);
            | "insert_before" =>
              edit(Option.get(arg), ChatLSP.Composition.Before);
              process_tool_calls(rest);
            | "insert_after" =>
              edit(Option.get(arg), ChatLSP.Composition.After);
              process_tool_calls(rest);
            | "delete" =>
              edit("", ChatLSP.Composition.Current);
              process_tool_calls(rest);
            | "view_sketch" => process_tool_calls(rest)
            | "submit" => ()
            | _ =>
              schedule_action(
                SendMessage(
                  System("Unknown tool call: " ++ tool_call, mode, chat_id),
                ),
              )
            };
          };
        };

        process_tool_calls(tool_calls);

        print_endline("Adding message to model");
        add_message_to_model(mode, model, message, chat_id, ~is_final=true)
        |> Updated.return_quiet;
      };
    | ErrorRound(response, sketch_z, ci, fuel, tileId, mode, chat_id) =>
      // Split response into discussion and completion
      let code_pattern =
        Str.regexp(
          "\\(\\(.\\|\n\\)*\\)```[ \n]*\\([^`]+\\)[ \n]*```\\(\\(.\\|\n\\)*\\)",
        );
      let (discussion, completion) =
        if (Str.string_match(code_pattern, response, 0)) {
          let before = String.trim(Str.matched_group(1, response));
          let code = String.trim(Str.matched_group(3, response));
          (before, code |> StringUtil.trim_leading);
        } else {
          print_endline("Regex match failed for: " ++ response);
          ("", response |> StringUtil.trim_leading); // Fallback if no code block found
        };
      print_endline("Response: " ++ response);
      print_endline("Discussion: " ++ discussion);
      print_endline("Completion: " ++ completion);
      // First add the discussion message
      let discussion_message =
        if (discussion === "") {
          text_message_of_str(
            "The model did not return a discussion for this completion.",
            LLM,
          );
        } else {
          text_message_of_str(discussion, LLM);
        };
      let model_with_discussion =
        add_message_to_model(
          mode,
          model,
          discussion_message,
          chat_id,
          ~is_final=false,
        );

      // Then handle the completion as before
      let completion_message =
        code_message_of_str(completion, LLM, Some(tileId));
      check_descriptor(
        ~model,
        ~schedule_action,
        ~message=completion_message,
        ~mode,
        ~chat_id,
      );
      switch (ChatLSP.ErrorRound.mk_reply(ci, sketch_z, completion)) {
      | None =>
        print_endline("ERROR ROUNDS (Non-error Response): " ++ completion);
        schedule_action(
          EmployLLMAction(RemoveAndSuggest(completion, tileId)),
        );
      | Some(error) =>
        print_endline("ERROR ROUNDS (Error): " ++ error);
        print_endline(
          "ERROR ROUNDS (Error-causing Response): " ++ completion,
        );
        schedule_action(
          SendMessage(
            ErrorRound(error, sketch_z, ci, fuel - 1, tileId, mode, chat_id),
          ),
        );
      };
      add_message_to_model(
        mode,
        model_with_discussion,
        completion_message,
        chat_id,
        ~is_final=true,
      )
      |> Updated.return_quiet;
    }

  | EmployLLMAction(action) =>
    switch (action) {
    | RemoveAndSuggest(response, tileId) =>
      // Only side effects in the editor are performed here
      add_suggestion(~response, tileId, false);
      model |> Updated.return_quiet;
    | Resuggest(response, tileId) =>
      // Only side effects in the editor are performed here
      add_suggestion(~response, tileId, true);
      model |> Updated.return_quiet;
    | Describe(content, mode, chat_id) =>
      let (past_chats, _) = get_mode_info(mode, model);
      let updated_chats =
        Id.Map.update(
          chat_id,
          opt_chat =>
            switch (opt_chat) {
            | Some(chat: Model.chat) =>
              Some({
                ...chat,
                descriptor: content,
              })
            | None => None
            },
          past_chats,
        );
      resculpt_model(mode, model, updated_chats, chat_id)
      |> Updated.return_quiet;
    }

  | ChatAction(action) =>
    switch (action) {
    | NewChat =>
      let mode = settings.assistant.mode;
      let (past_chats, _) = get_mode_info(mode, model);
      let new_chat: Model.chat = {
        messages: [],
        id: Id.mk(),
        descriptor: "",
        timestamp: JsUtil.timestamp(),
      };
      let updated_history = Model.add_chat_to_history(new_chat, past_chats);
      resculpt_model(mode, model, updated_history, new_chat.id)
      |> Updated.return_quiet;
    | DeleteChat(chat_to_be_gone_id) =>
      let mode = settings.assistant.mode;
      // Filter out the chat we're deleting
      let (past_chats, curr_chat) = get_mode_info(mode, model);
      let filtered_past_chats =
        Id.Map.filter((id, _) => id != chat_to_be_gone_id, past_chats);
      let chrono_history = Model.sorted_chats(filtered_past_chats);
      let updated_model =
        curr_chat.id == chat_to_be_gone_id
          ? switch (ListUtil.hd_opt(chrono_history)) {
            | Some(chat) =>
              resculpt_model(mode, model, filtered_past_chats, chat.id)
            | None => resculpt_model(mode, model, past_chats, curr_chat.id)
            }
          : resculpt_model(mode, model, filtered_past_chats, curr_chat.id);
      updated_model |> Updated.return_quiet;

    // Concat LS' error message and await_llm_response (... animation)
    // This works even if out of fuel, as both Respond and ErrorRespond
    // remove await_llm_response
    | CollapseMessage(index) =>
      let mode = settings.assistant.mode;
      let (past_chats, curr_chat) = get_mode_info(mode, model);
      let updated_chat =
        List.mapi(
          (i: int, msg: Model.message) =>
            if (i == index) {
              {
                ...msg,
                collapsed: !msg.collapsed,
              };
            } else {
              msg;
            },
          curr_chat.messages,
        );
      let updated_past_chats =
        Id.Map.update(
          curr_chat.id,
          opt_chat =>
            switch (opt_chat) {
            | Some(chat: Model.chat) =>
              Some({
                ...chat,
                messages: updated_chat,
              })
            | None => None
            },
          past_chats,
        );
      resculpt_model(mode, model, updated_past_chats, curr_chat.id)
      |> Updated.return_quiet;

    | SwitchChat(chat_id) =>
      let mode = settings.assistant.mode;
      let (past_chats, _) = get_mode_info(mode, model);
      resculpt_model(mode, model, past_chats, chat_id) |> Updated.return_quiet;
    | FilterLoadingMessages =>
      Model.{
        ...model,
        chat_history: {
          past_simple_chats:
            Id.Map.map(
              (chat: Model.chat) => {
                {
                  ...chat,
                  messages: filter_chat_messages(chat.messages),
                }
              },
              model.chat_history.past_simple_chats,
            ),
          past_suggestion_chats:
            Id.Map.map(
              (chat: Model.chat) => {
                {
                  ...chat,
                  messages: filter_chat_messages(chat.messages),
                }
              },
              model.chat_history.past_suggestion_chats,
            ),
          past_completion_chats:
            Id.Map.map(
              (chat: Model.chat) => {
                {
                  ...chat,
                  messages: filter_chat_messages(chat.messages),
                }
              },
              model.chat_history.past_completion_chats,
            ),
        },
      }
      |> Updated.return_quiet
    }
  };
};
