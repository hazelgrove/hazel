module Sexp = Sexplib.Sexp;
open Haz3lcore;
open Util;
open Util.OptUtil.Syntax;
open API;

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
  | SystemError(string, AssistantSettings.mode, Id.t);

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

let parse_blocks = (response: string): list(Model.block_kind) => {
  let rec parse_blocks =
          (str: string, acc: list(Model.block_kind)): list(Model.block_kind) => {
    let pattern = Str.regexp("```[ \n]*\\([^`]+\\)[ \n]*```");
    switch (Str.search_forward(pattern, str, 0)) {
    | exception Not_found => acc
    | pos =>
      let acc = ListUtil.leading(acc);
      let code = Str.matched_group(1, str);
      let zipper_of_code = Printer.zipper_of_string(code);
      let sketch =
        switch (zipper_of_code) {
        | Some(z) => Zipper.seg_for_view(z)
        | None =>
          print_endline("Failed to parse content into segment.\n");
          Zipper.seg_for_view(Zipper.init());
        };
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

let text_message_of_str =
    (response: string, party: Model.party): Model.message => {
  {
    party,
    content: response,
    displayable_content: parse_blocks(response),
    collapsed:
      String.length(response) >= Model.max_collapsed_length
      || party == System(Prompt),
  };
};

let add_chat_to_history =
    (chat: Model.chat, history: Id.Map.t(Model.chat)): Id.Map.t(Model.chat) => {
  Id.Map.add(chat.id, chat, history);
};

let init_chat = (kind: AssistantSettings.mode): Model.chat => {
  {
    messages:
      switch (kind) {
      | HazelTutor => [
          text_message_of_str(InitPrompts.mk_tutor(), System(Prompt)),
        ]
      | CodeSuggestion => []
      | TaskCompletion => [
          text_message_of_str(InitPrompts.mk_composition(), System(Prompt)),
        ]
      },
    id: Id.mk(),
    descriptor: "",
    timestamp: JsUtil.timestamp(),
  };
};

let extract_tool_calls = (response: string): list(string) => {
  let rec extract_tool_calls =
          (text: string, acc: list(string)): list(string) => {
    let pattern = Str.regexp("~~~[ \n]*\\([^~]+\\)[ \n]*~~~");
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
      extract_tool_calls(rest, [matched, ...acc]);
    };
  };
  extract_tool_calls(response, []);
};

let await_llm_response: Model.message = {
  party: LLM,
  content: "...",
  displayable_content: [Text("...")],
  collapsed: false,
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
    let chat_to_update_messages =
      filter_chat_messages(chat_to_update.messages);
    let messages = chat_to_update_messages @ [message];
    is_final ? messages : messages @ [await_llm_response];
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

let standardize_prompt =
    (~messages: list(Model.message), ~include_system: bool)
    : option(OpenRouter.prompt) => {
  // Role prompt: role, task, few-shot examples
  // History summarization: System, User, Assistant messages, summarized for brevity
  // Current User message
  // Current sketch
  let collected_chat =
    if (include_system) {
      List.fold_left(
        (chat: OpenRouter.prompt, message: Model.message) => {
          let role =
            switch (message.party) {
            | LLM => OpenRouter.Assistant
            | User
            | System(Prompt)
            | System(Error) => OpenRouter.User
            };
          switch (chat) {
          | [] => [
              {
                OpenRouter.role: OpenRouter.System,
                OpenRouter.content: message.content,
              },
            ]
          | _ =>
            chat
            @ [
              {
                OpenRouter.role,
                OpenRouter.content: message.content,
              },
            ]
          };
        },
        [],
        messages,
      );
    } else {
      List.fold_left(
        (chat: OpenRouter.prompt, message: Model.message) => {
          let role =
            switch (message.party) {
            | LLM => OpenRouter.Assistant
            | User
            | System(Prompt)
            | System(Error) => OpenRouter.User
            };
          chat
          @ [
            {
              OpenRouter.role,
              OpenRouter.content: message.content,
            },
          ];
        },
        [],
        messages,
      );
    };

  switch (collected_chat) {
  | [] => None
  | _ => Some(collected_chat)
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
  switch (
    standardize_prompt(
      ~messages=
        [text_message_of_str(prompt, System(Prompt))] @ chat.messages,
      ~include_system=true,
    )
  ) {
  | None => print_endline("Prompt generation failed")
  | Some(prompt) =>
    let model_id = Option.get(Store.Generic.load("MODEL"));
    let key = Option.get(Store.Generic.load("API"));
    let params: OpenRouter.params = {
      model_id,
      temperature: 1.0,
      top_p: 1.0,
    };
    OpenRouter.start_chat(~params, ~key, prompt, req =>
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
            SystemError(
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
    model |> Updated.return_quiet;
  | (None, _) =>
    let content = "No API key found. Please set an API key in the assistant settings.";
    add_message_to_model(
      mode,
      model,
      {
        party: System(Error),
        content,
        displayable_content: [Text(content)],
        collapsed: false,
      },
      curr_chat.id,
      ~is_final=true,
    )
    |> Updated.return_quiet;
  | (_, None) =>
    let content = "No model ID found. Please set a model ID in the assistant settings.";
    add_message_to_model(
      mode,
      model,
      {
        party: System(Error),
        content,
        displayable_content: [Text(content)],
        collapsed: false,
      },
      curr_chat.id,
      ~is_final=true,
    )
    |> Updated.return_quiet;
  };
};

let failed_prompt_generation =
    (~mode: AssistantSettings.mode, ~model: Model.t, ~curr_chat: Model.chat) => {
  let content = "Prompt generation failed.";
  add_message_to_model(
    mode,
    model,
    {
      party: System(Error),
      content,
      displayable_content: [Text(content)],
      collapsed: false,
    },
    curr_chat.id,
    ~is_final=true,
  )
  |> Updated.return_quiet;
};

let mk_mode_prompt = (~settings: AssistantSettings.t): string => {
  let mode = settings.mode;
  let prompt =
    switch (mode) {
    | HazelTutor => InitPrompts.mk_tutor()
    | CodeSuggestion => ""
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
      InitPrompts.mk_composition()
    };
  prompt;
};

let mk_mode_ctx_prompt =
    (~settings: AssistantSettings.t, ~editor: CodeModel.t): option(string) => {
  let mode = settings.mode;
  switch (mode) {
  | HazelTutor =>
    // No context needed in tutor mode. Initial prompt gives all needed information.
    // Actually, we could add context here in the future in case the user asks sketch-specific questions.
    None
  | CodeSuggestion =>
    // todo
    None
  | TaskCompletion =>
    let sketch_seg =
      Zipper.smart_seg(
        ~dump_backpack=true,
        ~erase_buffer=true,
        editor.editor.state.zipper,
      );
    Some(
      ChatLSP.Composition.mk_ctx_prompt(
        ChatLSP.Options.init,
        sketch_seg,
        editor,
      ),
    );
  };
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
      let ctx_prompt =
        mk_mode_ctx_prompt(~settings=settings.assistant, ~editor);
      // We want to send the LLM all of the chat history, relevant context, and user message
      // But, note we don't want to add all of this to a single "message"
      print_endline("Message party: " ++ Model.show_party(message.party));
      let include_system =
        switch (mode) {
        | CodeSuggestion => false
        | HazelTutor
        | TaskCompletion => true
        };
      let llm_input =
        switch (ctx_prompt) {
        | Some(prompt) =>
          standardize_prompt(
            ~messages=
              curr_chat.messages
              @ [
                text_message_of_str(prompt, System(Prompt)),
                text_message_of_str(message.content, User),
              ],
            ~include_system,
          )
        | None =>
          standardize_prompt(
            ~messages=
              curr_chat.messages
              @ [text_message_of_str(message.content, User)],
            ~include_system,
          )
        };
      switch (llm_input) {
      | None => failed_prompt_generation(~mode, ~model, ~curr_chat)
      | Some(llm_input) =>
        mk_LLM_call(
          ~model={
            let model =
              add_message_to_model(
                mode,
                model,
                message,
                curr_chat.id,
                ~is_final=false,
              );
            switch (ctx_prompt) {
            | Some(prompt) =>
              add_message_to_model(
                mode,
                model,
                text_message_of_str(prompt, System(Prompt)),
                curr_chat.id,
                ~is_final=false,
              )
            | None => model
            };
          },
          ~curr_chat,
          ~prompt=llm_input,
          ~mode,
          ~schedule_action,
        )
      };
    | Sketch(tileId, mode, advanced_reasoning) =>
      // Capture the chat we're updating
      let (_, curr_chat) = get_mode_info(mode, model);
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
        let message: Model.message = text_message_of_str(prompt, User);
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
                  SystemError(
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
          let content = "No API key found. Please set an API key in the assistant settings.";
          add_message_to_model(
            mode,
            model,
            {
              party: System(Error),
              content,
              displayable_content: [Text(content)],
              collapsed: false,
            },
            curr_chat.id,
            ~is_final=true,
          )
          |> Updated.return_quiet;
        | (_, None) =>
          let content = "No API key or model ID found. Please set an API key and model ID in the assistant settings.";
          add_message_to_model(
            mode,
            model,
            {
              party: System(Error),
              content,
              displayable_content: [Text(content)],
              collapsed: false,
            },
            curr_chat.id,
            ~is_final=true,
          )
          |> Updated.return_quiet;
        };
      };
    | ErrorRound(error, sketch_z, ci, fuel, tileId, mode, chat_id) =>
      let error_message =
        text_message_of_str(
          "Your previous response caused the following error. Please fix it in your response: "
          ++ error,
          User,
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
        let content =
          "By default we stop the assistant after "
          ++ string_of_int(ChatLSP.Options.init.error_rounds_max)
          ++ " error rounds. Thus, stopping.";
        add_message_to_model(
          mode,
          model,
          {
            party: System(Error),
            content,
            displayable_content: [Text(content)],
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
        switch (
          standardize_prompt(
            ~messages=curr_chat.messages @ [error_message],
            ~include_system=false // Assuming that Error Round actions only occur in Code Suggestion mode
          )
        ) {
        | None =>
          let content = "Prompt generation failed.";
          add_message_to_model(
            mode,
            model,
            {
              party: System(Error),
              content,
              displayable_content: [Text(content)],
              collapsed: false,
            },
            chat_id,
            ~is_final=true,
          )
          |> Updated.return_quiet;
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
                    SystemError(
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
            let content = "No API key found. Please set an API key in the assistant settings.";
            add_message_to_model(
              mode,
              model,
              {
                party: System(Error),
                content,
                displayable_content: [Text(content)],
                collapsed: false,
              },
              chat_id,
              ~is_final=true,
            )
            |> Updated.return_quiet;
          | (None, _) =>
            let content = "No model ID found. Please set a model ID in the assistant settings.";
            add_message_to_model(
              mode,
              model,
              {
                party: System(Error),
                content,
                displayable_content: [Text(content)],
                collapsed: false,
              },
              curr_chat.id,
              ~is_final=true,
            )
            |> Updated.return_quiet;
          }
        };
      };
    | SystemError(content, mode, chat_id) =>
      add_message_to_model(
        mode,
        model,
        {
          party: System(Error),
          content,
          displayable_content: parse_blocks(content),
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
      check_descriptor(~model, ~schedule_action, ~message, ~mode, ~chat_id);
      if (mode == HazelTutor || mode == CodeSuggestion) {
        add_message_to_model(mode, model, message, chat_id, ~is_final=true)
        |> Updated.return_quiet;
      } else {
        let tool_calls = extract_tool_calls(message.content);

        let rec process_tool_calls = (calls: list(string)) => {
          switch (calls) {
          | [] =>
            schedule_action(
              SendMessage(
                Basic(
                  text_message_of_str(
                    "You have omitted the \"submit\" tool call from your message. Displaying sketch for you to continue iterative task completion process.",
                    System(Prompt),
                  ),
                ),
              ),
            )
          | [tool_call, ...remaining] =>
            let parsed_response =
              try(
                switch (Json.from_string(tool_call)) {
                | `Assoc(fields) =>
                  let tool = List.assoc_opt("tool", fields);
                  let arg = List.assoc_opt("args", fields);
                  (tool, arg);
                | _ => (None, None)
                }
              ) {
              | Yojson.Json_error(_) => (None, None)
              };
            let tool =
              switch (parsed_response) {
              | (Some(`String(tool)), _) => tool
              | _ =>
                schedule_action(
                  SendMessage(
                    Basic(
                      text_message_of_str(
                        "Unable to parse tool call:\n"
                        ++ tool_call
                        ++ "\nEnsure proper json formatting is used.",
                        System(Error),
                      ),
                    ),
                  ),
                );
                "submit";
              };
            let args =
              switch (parsed_response) {
              | (_, Some(`Assoc(args))) => args
              | _ => []
              };

            let invalid_arg_type =
                (tool: string, arg: string, typ: Json.t): string => {
              tool
              ++ " called with invalid type at argument position "
              ++ arg
              ++ ": "
              ++ Json.to_string(typ);
            };

            let invalid_num_args =
                (tool: string, expected: int, received: int): string => {
              tool
              ++ " expected "
              ++ string_of_int(expected)
              ++ " argument"
              ++ (expected == 1 ? "" : "s")
              ++ " but "
              ++ string_of_int(received)
              ++ " were given";
            };

            try(
              switch (tool) {
              | "goto_definition" =>
                switch (
                  List.assoc_opt("variable_name", args),
                  List.length(args),
                ) {
                | (Some(`String(arg)), 1) =>
                  goto(editor, ChatLSP.Composition.Definition, arg)
                | (Some(inv_type), _) =>
                  raise(
                    Failure(
                      invalid_arg_type(
                        "goto_definition",
                        "variable_name",
                        inv_type,
                      ),
                    ),
                  )
                | (_, n) =>
                  raise(Failure(invalid_num_args("goto_definition", 1, n)))
                };
                process_tool_calls(remaining);
              | "goto_body" =>
                switch (
                  List.assoc_opt("variable_name", args),
                  List.length(args),
                ) {
                | (Some(`String(arg)), 1) =>
                  goto(editor, ChatLSP.Composition.Body, arg)
                | (Some(inv_type), _) =>
                  raise(
                    Failure(
                      invalid_arg_type(
                        "goto_body",
                        "variable_name",
                        inv_type,
                      ),
                    ),
                  )
                | (_, n) =>
                  raise(Failure(invalid_num_args("goto_body", 1, n)))
                };
                process_tool_calls(remaining);
              | "select_all" =>
                goto(editor, ChatLSP.Composition.All, "");
                process_tool_calls(remaining);
              | "paste" =>
                switch (List.assoc_opt("code", args), List.length(args)) {
                | (Some(`String(arg)), 1) =>
                  edit(ChatLSP.Composition.Current, arg)
                | (Some(inv_type), _) =>
                  raise(Failure(invalid_arg_type("edit", "code", inv_type)))
                | (_, n) => raise(Failure(invalid_num_args("edit", 1, n)))
                };
                process_tool_calls(remaining);
              | "delete" =>
                List.length(args) != 0
                  ? raise(
                      Failure(
                        invalid_num_args("delete", 0, List.length(args)),
                      ),
                    )
                  : edit(ChatLSP.Composition.Current, "");
                process_tool_calls(remaining);
              | "view_sketch" =>
                List.length(args) != 0
                  ? raise(
                      Failure(
                        invalid_num_args(
                          "view_sketch",
                          0,
                          List.length(args),
                        ),
                      ),
                    )
                  : schedule_action(
                      SendMessage(
                        Basic(
                          text_message_of_str(
                            "You have requested to view the sketch. Please review and continue with completing the user-specified task.",
                            System(Prompt),
                          ),
                        ),
                      ),
                    )
              | "submit" =>
                List.length(args) != 0
                  ? raise(
                      Failure(
                        invalid_num_args("submit", 0, List.length(args)),
                      ),
                    )
                  : ()
              | _ => raise(Failure("Unknown tool call: " ++ tool_call))
              }
            ) {
            | Failure(err) =>
              schedule_action(
                SendMessage(
                  Basic(text_message_of_str(err, System(Error))),
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
      let (_, completion) =
        if (Str.string_match(code_pattern, response, 0)) {
          let before = String.trim(Str.matched_group(1, response));
          let code = String.trim(Str.matched_group(3, response));
          (before, code |> StringUtil.trim_leading);
        } else {
          print_endline("Regex match failed for: " ++ response);
          ("", response |> StringUtil.trim_leading); // Fallback if no code block found
        };
      check_descriptor(
        ~model,
        ~schedule_action,
        ~message=text_message_of_str(response, LLM),
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
        model,
        text_message_of_str(response, LLM),
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
      let new_chat: Model.chat = init_chat(mode);
      let updated_history = add_chat_to_history(new_chat, past_chats);
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
      let is_prompt_display =
        try(List.nth(curr_chat.messages, index).party == System(Prompt)) {
        | Invalid_argument(_) => true
        };
      print_endline(
        "Is prompt display: " ++ string_of_bool(is_prompt_display),
      );
      let updated_chat =
        List.mapi(
          (i: int, msg: Model.message) =>
            if (i == index) {
              {
                ...msg,
                collapsed: !msg.collapsed,
              };
            } else if (msg.party == System(Prompt) && is_prompt_display) {
              print_endline(
                "Collapsing prompt display message at index: "
                ++ string_of_int(i),
              );
              {
                ...msg,
                collapsed: true,
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

[@deriving (show({with_path: false}), sexp, yojson)]
let init: Model.t = {
  let (init_simple_chat, init_suggestion_chat, init_completion_chat) = (
    init_chat(HazelTutor),
    init_chat(CodeSuggestion),
    init_chat(TaskCompletion),
  );
  {
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
};
