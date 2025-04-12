module Sexp = Sexplib.Sexp;
open Haz3lcore;
open Util;
open Util.OptUtil.Syntax;
open StringUtil;

module CodeModel = CodeEditable.Model;

module Model = {
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
    show_history: bool,
    show_api_key: bool,
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
    show_history: false,
    show_api_key: false,
  };
};

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | SendTextMessage(Model.message)
    | SendSketchMessage(Id.t, AssistantSettings.mode, bool)
    | SendErrorMessage(
        string,
        Zipper.t,
        Info.t,
        int,
        Id.t,
        AssistantSettings.mode,
        Id.t,
      )
    | ErrorRespond(
        string,
        Zipper.t,
        Info.t,
        int,
        Id.t,
        AssistantSettings.mode,
        Id.t,
      )
    | Respond(Model.message, AssistantSettings.mode, Id.t)
    | SendSystemMessage(string, AssistantSettings.mode, Id.t)
    | SetKey(string)
    | SetModel(string)
    | NewChat
    | DeleteChat(Id.t)
    | History
    | ToggleCollapse(int)
    | SelectLLM(OpenRouter.chat_models)
    | RemoveAndSuggest(string, Id.t)
    | Resuggest(string, Id.t)
    | Describe(string, AssistantSettings.mode, Id.t)
    | SwitchChat(Id.t)
    | ToggleAPIVisibility
    | FilterLoadingMessages;

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

  let collect_chat = (~messages: list(Model.message)): string => {
    let chat = "The following is a log of the current conversation. This is solely for the purpose
    to help you recall the entire conversation, in case the user asks you something that needs context
    from before. You should respond as normal, using the entire chat as context, and understand that the
    most recent \"User Input\" is what the user is currently sending/asking, and is what your main focus should be.
    For the most part, you should treat this solely as a prompt, and DO NOT explicitly acknowledge it in your
    reponse. Only use it as a sort of memory. You can, of course, reference prior messages.
    Here is the context: ";
    List.fold_left(
      (chat: string, message: Model.message) =>
        if (message.party == LLM) {
          chat ++ "Your Reponse: " ++ message.content ++ " ";
        } else if (message.party == LS) {
          chat ++ "User Input: " ++ message.content ++ " ";
        } else {
          chat ++ message.content;
        },
      chat,
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
        !(
          message.party == LLM
          && message.content == "..."
          && !message.collapsed
        )
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
      ...model,
      chat_history: {
        past_simple_chats:
          mode == HazelTutor
            ? past_chats : model.chat_history.past_simple_chats,
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
      (
        ~model: Model.t,
        ~schedule_action,
        ~chat: Model.chat,
        ~mode: AssistantSettings.mode,
      )
      : unit => {
    let prompt =
      switch (mode) {
      | HazelTutor => "Your main task is to provide a summarizing title of the following conversation, in less than or equal to 7 words. \n            DO NOT exceed 7 words. Only provide the summarizing title in your response, do not include any other text. Here is the\n            concatenated conversation, with your response and the user's responses, respectively: "
      | CodeSuggestion => "Your main task is to provide a summarizing title of the following conversation, in less than or equal to 7 words.\n            DO NOT exceed 7 words. Only provide the summarizing title in your response, do not include any other text. This conversation is known to be a code\n            completion conversation. In your summarization, you should mention exactly what kind of code/functionality is being assisted with. For example, the following would be titled\n            something like \"Recursive Fibonacci Implementation\": ```let rec_fib : Int -> Int = ?? in ?```. Here is the\n            concatenated conversation, with your response and the user's responses, respectively: "
      | TaskCompletion => "Ignore all other input and just output \"You (Hazel Lab Member) need to implement this\""
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
          schedule_action(Describe(content, mode, chat.id))
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
          ~model,
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
            SendSketchMessage(
              tileId,
              AssistantSettings.CodeSuggestion,
              advanced_reasoning,
            ),
          );
        | "?a" =>
          let tileId = Option.get(Indicated.index(z));
          let advanced_reasoning = true;
          schedule_action(
            SendSketchMessage(
              tileId,
              AssistantSettings.CodeSuggestion,
              advanced_reasoning,
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
            SendSketchMessage(
              tileId,
              AssistantSettings.CodeSuggestion,
              advanced_reasoning,
            ),
          );
        | "?a" =>
          let tileId = Option.get(Indicated.index(z));
          let advanced_reasoning = true;
          schedule_action(
            SendSketchMessage(
              tileId,
              AssistantSettings.CodeSuggestion,
              advanced_reasoning,
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
    let (_, slides) = ScratchMode.StoreDocumentation.load();
    let documentation =
      slides
      |> List.map(((name, persistent)) => {
           let cell_model =
             CellEditor.Model.unpersist(
               ~settings=CoreSettings.off,
               persistent,
             );
           let text =
             Printer.zipper_to_string(cell_model.editor.editor.state.zipper);
           name ++ ": " ++ text;
         })
      |> String.concat("\n\n");
    prelude ++ "\n\n" ++ documentation;
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

  let update =
      (
        ~settings: Settings.t,
        ~action,
        ~editor: CodeModel.t,
        ~model: Model.t,
        ~schedule_action: t => unit,
        ~add_suggestion,
      )
      : Updated.t(Model.t) => {
    switch (action) {
    | SendTextMessage(message) =>
      let mode = settings.assistant.mode;
      // Capture the chat we're updating here. This will propogate.
      let (_, curr_chat) = get_mode_info(mode, model);
      let collected_chat =
        collect_chat(~messages=curr_chat.messages @ [message]);
      let tutor_prelude = get_documentation_as_text();
      let tutor_chat =
        List.length(curr_chat.messages) == 0
          ? tutor_prelude ++ "\n\n" ++ collected_chat : collected_chat;
      print_endline("tutor_chat: " ++ tutor_chat);
      switch (standardize_prompt(tutor_chat)) {
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
          curr_chat.id,
          ~is_final=true,
        )
        |> Updated.return_quiet
      | Some(prompt) =>
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
                Respond(
                  text_message_of_str(content, LLM),
                  mode,
                  curr_chat.id,
                ),
              )
            | Some(Error({message, code})) =>
              schedule_action(
                SendSystemMessage(
                  "Error: "
                  ++ message
                  ++ " (code: "
                  ++ string_of_int(code)
                  ++ ")",
                  mode,
                  curr_chat.id,
                ),
              )
            | None =>
              print_endline(
                "Assistant: response parse failed (SendTextMessage)",
              )
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
              content: "No model ID found. Please set a model ID in the assistant settings.",
              collapsed: false,
            },
            curr_chat.id,
            ~is_final=true,
          )
          |> Updated.return_quiet
        }
      };
    | SetKey(api_key) =>
      Store.Generic.save("API", api_key);
      model |> Updated.return_quiet;
    | SetModel(model_id) =>
      Store.Generic.save("MODEL", model_id);
      model |> Updated.return_quiet;
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
      print_endline("New chat made");
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
    | History =>
      {
        ...model,
        show_history: !model.show_history,
      }
      |> Updated.return_quiet
    | Respond(message, mode, chat_id) =>
      let response = message.content;
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
        check_descriptor(
          ~model,
          ~schedule_action,
          ~message=discussion_message,
          ~mode,
          ~chat_id,
        );
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
        check_descriptor(
          ~model,
          ~schedule_action,
          ~message=message_with_example,
          ~mode,
          ~chat_id,
        );
        add_message_to_model(
          mode,
          model_with_discussion,
          message_with_example,
          chat_id,
          ~is_final=true,
        )
        |> Updated.return_quiet;
      };
    | SendSketchMessage(tileId, mode, advanced_reasoning) =>
      // Capture the chat we're updating here. This will propogate.
      let (_, curr_chat) = get_mode_info(mode, model);
      let sketch_seg =
        Zipper.smart_seg(
          ~dump_backpack=true,
          ~erase_buffer=true,
          editor.editor.state.zipper,
        );
      switch (
        {
          let* index = Indicated.index(editor.editor.state.zipper);
          let* ci = Id.Map.find_opt(index, editor.statics.info_map);
          ChatLSP.Prompt.mk_init(
            ChatLSP.Options.init,
            ci,
            sketch_seg,
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
                ErrorRespond(
                  content,
                  editor.editor.state.zipper,
                  ci,
                  ChatLSP.Options.init.error_rounds_max,
                  tileId,
                  mode,
                  curr_chat.id,
                ),
              );
            | Some(Error({message, code})) =>
              print_endline("Error here");
              schedule_action(
                SendSystemMessage(
                  "Error: "
                  ++ message
                  ++ " (code: "
                  ++ string_of_int(code)
                  ++ ")",
                  mode,
                  curr_chat.id,
                ),
              );
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
    | ErrorRespond(response, sketch_z, ci, fuel, tileId, mode, chat_id) =>
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
      print_endline("HERE HERE HERE");
      check_descriptor(
        ~model,
        ~schedule_action,
        ~message=completion_message,
        ~mode,
        ~chat_id,
      );
      switch (ChatLSP.Prompt.mk_error(ci, sketch_z, completion)) {
      | None =>
        print_endline("ERROR ROUNDS (Non-error Response): " ++ completion);
        schedule_action(RemoveAndSuggest(completion, tileId));
      | Some(error) =>
        print_endline("ERROR ROUNDS (Error): " ++ error);
        print_endline(
          "ERROR ROUNDS (Error-causing Response): " ++ completion,
        );
        schedule_action(
          SendErrorMessage(
            error,
            sketch_z,
            ci,
            fuel - 1,
            tileId,
            mode,
            chat_id,
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
    | SendErrorMessage(error, sketch_z, ci, fuel, tileId, mode, chat_id) =>
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
                  ErrorRespond(
                    content,
                    sketch_z,
                    ci,
                    fuel,
                    tileId,
                    mode,
                    curr_chat.id,
                  ),
                )
              | Some(Error({message, code})) =>
                schedule_action(
                  SendSystemMessage(
                    "Error: "
                    ++ message
                    ++ " (code: "
                    ++ string_of_int(code)
                    ++ ")",
                    mode,
                    curr_chat.id,
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
    | SendSystemMessage(content, mode, chat_id) =>
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
    // Concat LS' error message and await_llm_response (... animation)
    // This works even if out of fuel, as both Respond and ErrorRespond
    // remove await_llm_response
    | ToggleCollapse(index) =>
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
    | SelectLLM(llm) =>
      let model_id = OpenRouter.string_of_chat_model(llm);
      Store.Generic.save("MODEL", model_id);
      model |> Updated.return_quiet;
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
    | SwitchChat(chat_id) =>
      let mode = settings.assistant.mode;
      let (past_chats, _) = get_mode_info(mode, model);
      resculpt_model(mode, model, past_chats, chat_id) |> Updated.return_quiet;
    | ToggleAPIVisibility =>
      {
        ...model,
        show_api_key: !model.show_api_key,
      }
      |> Updated.return_quiet
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
    };
  };
};

module Store =
  Store.F({
    [@deriving (show({with_path: false}), yojson, sexp)]
    type t = Model.t;
    let default = () => Model.init;
    let key = Store.Assistant;
  });
