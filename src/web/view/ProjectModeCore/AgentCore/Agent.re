open Util;
open Haz3lcore;

/* Thinkpad
   We will want 3 types of display messages
   - Agent
   - User
   - System(system)
   where system can be any of:
   - Error
   - Prompt
   - AgentView
   - ToolCall
   etc.
   i.e. They are backend messages.

   We could technically add things like `user` message types, with:
   - Initial
   - Followup
   etc.

   We could also have `agent` with:
   - Text
   - Thinking/Reasoning
   - Tool
   etc.

   OpenRouter messages must be of the following types:
   - System
   - Agent
   - User
   - Tool
   - Developer


   */

module Message = {
  module Model = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type error_kind =
      | BackendFailure
      | APIFailure;

    [@deriving (show({with_path: false}), sexp, yojson)]
    type tool_result_kind =
      | ToolCall
      | ToolResult;

    [@deriving (show({with_path: false}), sexp, yojson)]
    type system_kind =
      | Error(error_kind)
      | DeveloperNotes // Only one should exist
      | Prompt // Only one should exist
      | AgentEditorView
      | StaticErrorsInfo
      | ToolCall
      | ToolResult(tool_result_kind);

    [@deriving (show({with_path: false}), sexp, yojson)]
    // Separating like such, as agent messages appear on left
    // User messages appear on right
    // System messages auxillary
    type role =
      | Agent
      | User
      | System(system_kind);

    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = {
      id: Id.t,
      content: string,
      timestamp: float,
      role,
      api_message: option(OpenRouter.Message.Model.t),
      children: list(Id.t),
      current_child: option(Id.t),
    };
  };

  module Utils = {
    let set_current_child = (msg: Model.t, child_id: Id.t): Model.t => {
      {
        ...msg,
        current_child: Some(child_id),
      };
    };

    let add_child = (msg: Model.t, child_id: Id.t): Model.t => {
      {
        // Adds child_id to the message's children list and sets it as the current child
        ...msg,
        children: msg.children @ [child_id],
      };
    };

    let mk_prompt_message = (content: string): Model.t => {
      let api_content = "\n<prompt>\n" ++ content ++ "\n</prompt>\n";
      {
        id: Id.mk(),
        content,
        timestamp: JsUtil.timestamp(),
        role: System(Prompt),
        api_message:
          Some(OpenRouter.Message.Utils.mk_system_msg(api_content)),
        children: [],
        current_child: None,
      };
    };
    let mk_developer_notes_message = (content: string): Model.t => {
      let api_content =
        "\n<developerNotes>\n" ++ content ++ "\n</developerNotes>\n";
      {
        id: Id.mk(),
        content,
        timestamp: JsUtil.timestamp(),
        role: System(DeveloperNotes),
        api_message:
          Some(OpenRouter.Message.Utils.mk_developer_msg(api_content)),
        children: [],
        current_child: None,
      };
    };
    let mk_agent_message = (content: string): Model.t => {
      let api_content =
        "\n<agentResponse>\n" ++ content ++ "\n</agentResponse>\n";
      {
        id: Id.mk(),
        content,
        timestamp: JsUtil.timestamp(),
        role: Agent,
        api_message:
          Some(OpenRouter.Message.Utils.mk_assistant_msg(api_content)),
        children: [],
        current_child: None,
      };
    };

    let mk_tool_call_message = (content: string): Model.t => {
      {
        // This is a unique message for our UI that does not correspond to an OpenRouter message,
        // because we parse it out from the preceding agent response message.

        id: Id.mk(),
        content,
        timestamp: JsUtil.timestamp(),
        role: System(ToolCall),
        api_message: None,
        children: [],
        current_child: None,
      };
    };

    let mk_tool_result_message =
        (
          content: string,
          tool_contents: OpenRouter.Message.Model.tool_contents,
          tool_result_kind: Model.tool_result_kind,
        )
        : Model.t => {
      // This is a message from our backend.
      // Protocols require a tool id to be associated, thus we send this is as an OpenRouter.Tool message.contents

      let api_content = "\n<toolResult>\n" ++ content ++ "\n</toolResult>\n";
      {
        id: Id.mk(),
        content,
        timestamp: JsUtil.timestamp(),
        role: System(ToolResult(tool_result_kind)),
        api_message:
          Some(
            OpenRouter.Message.Utils.mk_tool_msg(api_content, tool_contents),
          ),
        children: [],
        current_child: None,
      };
    };

    let mk_user_message = (content: string): Model.t => {
      let api_content = "\n<userMessage>\n" ++ content ++ "\n</userMessage>\n";
      {
        id: Id.mk(),
        content,
        timestamp: JsUtil.timestamp(),
        role: User,
        api_message: Some(OpenRouter.Message.Utils.mk_user_msg(api_content)),
        children: [],
        current_child: None,
      };
    };
    let mk_agent_editor_view_message = (content: string): Model.t => {
      let api_content_prefix = "\n<agentEditorView>\n```";
      let api_content_suffix = "```\n</agentEditorView>\n";
      let api_content = api_content_prefix ++ content ++ api_content_suffix;
      {
        id: Id.mk(),
        content,
        timestamp: JsUtil.timestamp(),
        role: System(AgentEditorView),
        api_message: Some(OpenRouter.Message.Utils.mk_user_msg(api_content)),
        children: [],
        current_child: None,
      };
    };

    let mk_static_errors_info_message = (content: string): Model.t => {
      let api_content =
        "\n<staticErrorsInfo>\n" ++ content ++ "\n</staticErrorsInfo>\n";
      {
        id: Id.mk(),
        content,
        timestamp: JsUtil.timestamp(),
        role: System(StaticErrorsInfo),
        api_message: Some(OpenRouter.Message.Utils.mk_user_msg(api_content)),
        children: [],
        current_child: None,
      };
    };

    let mk_api_failure_message = (content: string): Model.t => {
      id: Id.mk(),
      content,
      timestamp: JsUtil.timestamp(),
      role: System(Error(APIFailure)),
      api_message: None,
      children: [],
      current_child: None,
    };

    let mk_backend_failure_message = (content: string): Model.t => {
      let failure_instructions =
        {|
            Sorry our backend failed to process your request.
            This indicates a fatal bug in our backend code/server and should be reported to developers immediately.
            Please halt the current chat and report the issue to the user.
            Error:
            |}
        ++ content;
      let api_content =
        "\n<backendFailure>\n"
        ++ failure_instructions
        ++ "\n</backendFailure>\n";
      {
        id: Id.mk(),
        content,
        timestamp: JsUtil.timestamp(),
        role: System(Error(BackendFailure)),
        api_message: Some(OpenRouter.Message.Utils.mk_user_msg(api_content)),
        children: [],
        current_child: None,
      };
    };

    let api_message_of_message =
        (message: Model.t): option(OpenRouter.Message.Model.t) => {
      switch (message.api_message) {
      | Some(api_message) => Some(api_message)
      | None => None
      };
    };
  };
};

module Chat = {
  module Model = {
    // Chats are a tree of messages
    // The user can branch off from the current or past message and create threads
    // The current thread is made via linearizing the branch denoted by the current head
    // The user can always toggle between branches in this tree via changing the child pointer

    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = {
      id: Id.t,
      title: string,
      message_map: Id.Map.t(Message.Model.t),
      root: Id.t,
      agent_view: AgentContext.Model.t,
      agent_workbench: AgentWorkbench.Model.t // The agent's todo lists/workbench associated with this chat
    };
  };

  module Utils = {
    let find_message = (id: Id.t, chat: Model.t): Message.Model.t => {
      Id.Map.find_opt(id, chat.message_map)
      |> OptUtil.get_or_fail(
           "[Chat.Utils.find_message] Message not found from the message map",
         );
    };

    let linearize = (chat: Model.t): list(Message.Model.t) => {
      // Linearizes the the chat model into a list of current messages
      // via recursively traversing the tree from the root through current child nodes
      let rec go =
              (msg: Message.Model.t, messages: list(Message.Model.t))
              : list(Message.Model.t) => {
        switch (msg.current_child) {
        | None => messages
        | Some(child_id) =>
          let child_msg = find_message(child_id, chat);
          go(child_msg, messages @ [msg]);
        };
      };
      go(find_message(chat.root, chat), []);
    };

    let api_messages_of_messages =
        (messages: list(Message.Model.t)): list(OpenRouter.Message.Model.t) => {
      List.filter_map(Message.Utils.api_message_of_message, messages);
    };

    let current_tail = (chat: Model.t): Message.Model.t => {
      // Calling linearize makes this a little costly
      // but it also keeps code safer rather than storing a current tail pointer
      let linear = linearize(chat);
      List.hd(List.rev(linear));
    };

    let update_message = (message: Message.Model.t, chat: Model.t): Model.t => {
      {
        ...chat,
        message_map: Id.Map.add(message.id, message, chat.message_map),
      };
    };

    let append = (new_msg: Message.Model.t, chat: Model.t): Model.t => {
      // Inserts a new message at the current tail of the chat

      // Add new message's id to the current tail message's children
      let tail_msg = current_tail(chat);
      let updated_tail_msg = Message.Utils.add_child(tail_msg, new_msg.id);
      let updated_chat = update_message(updated_tail_msg, chat);

      // Add the new message to the chat
      update_message(new_msg, updated_chat);
    };

    let truncate = (id: Id.t, chat: Model.t): Model.t => {
      let message = find_message(id, chat);
      let updated_message = {
        ...message,
        current_child: None,
      };
      update_message(updated_message, chat);
    };

    let switch_branch =
        (~fork_id: Id.t, ~new_child_id: Id.t, chat: Model.t): Model.t => {
      let fork_msg = find_message(fork_id, chat);
      let updated_fork_msg = {
        ...fork_msg,
        current_child: Some(new_child_id),
      };
      update_message(updated_fork_msg, chat);
    };

    let init = (~system_prompt: string, ~dev_notes: string): Model.t => {
      let system_prompt = Message.Utils.mk_prompt_message(system_prompt);
      let chat: Model.t = {
        id: Id.mk(),
        title: "New Chat",
        message_map: Id.Map.singleton(system_prompt.id, system_prompt),
        root: system_prompt.id,
        agent_view: AgentContext.Utils.init(),
        agent_workbench: AgentWorkbench.Utils.MainUtils.init(),
      };
      let dev_notes = Message.Utils.mk_developer_notes_message(dev_notes);
      append(dev_notes, chat);
    };
  };

  module Update = {
    module Action = {
      [@deriving (show({with_path: false}), sexp, yojson)]
      type t =
        | AppendMessage(Message.Model.t)
        | SwitchBranch(Id.t, Id.t)
        | BranchOff(Id.t, Message.Model.t)
        | AgentViewAction(AgentContext.Update.action)
        | WorkbenchAction(AgentWorkbench.Update.Action.action);
    };

    let update = (action: Action.t, model: Model.t): Model.t => {
      switch (action) {
      | AppendMessage(message) => Utils.append(message, model)
      | SwitchBranch(fork_id, new_child_id) =>
        Utils.switch_branch(~fork_id, ~new_child_id, model)
      | BranchOff(fork_id, message) =>
        let truncated_model = Utils.truncate(fork_id, model);
        let branched_model = Utils.append(message, truncated_model);
        branched_model;
      | AgentViewAction(agent_view_action) => {
          ...model,
          agent_view:
            AgentContext.Update.update(agent_view_action, model.agent_view),
        }
      | WorkbenchAction(workbench_action) => {
          ...model,
          agent_workbench: {
            switch (
              AgentWorkbench.Update.update(
                ~model=model.agent_workbench,
                ~action=workbench_action,
              )
            ) {
            | AgentWorkbench.Update.Action.Success(updated_workbench) => updated_workbench
            | AgentWorkbench.Update.Action.Failure(msg) =>
              failwith("[Chat.Update] Failed to update workbench: " ++ msg)
            };
          },
        }
      };
    };
  };
};

module ChatSystem = {
  module Model = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = {
      chat_map: Id.Map.t(Chat.Model.t),
      current: Id.t,
    };
  };

  module Utils = {
    let find_chat = (id: Id.t, model: Model.t): Chat.Model.t => {
      Id.Map.find_opt(id, model.chat_map)
      |> OptUtil.get_or_fail("[ChatSystem.Utils.find_chat] Chat not found");
    };

    let switch_chat = (id: Id.t, model: Model.t): Model.t => {
      {
        ...model,
        current: id,
      };
    };

    let update_chat = (chat: Chat.Model.t, model: Model.t): Model.t => {
      {
        ...model,
        chat_map: Id.Map.add(chat.id, chat, model.chat_map),
      };
    };

    let new_chat =
        (~system_prompt: string, ~dev_notes: string, model: Model.t): Model.t => {
      let new_chat = Chat.Utils.init(~system_prompt, ~dev_notes);
      update_chat(new_chat, model);
    };

    let delete_chat = (id: Id.t, model: Model.t): Model.t => {
      {
        ...model,
        chat_map: Id.Map.remove(id, model.chat_map),
      };
    };

    let init = (~system_prompt: string, ~dev_notes: string): Model.t => {
      let initial_chat = Chat.Utils.init(~system_prompt, ~dev_notes);
      {
        chat_map: Id.Map.singleton(initial_chat.id, initial_chat),
        current: initial_chat.id,
      };
    };
  };

  module Update = {
    module Action = {
      [@deriving (show({with_path: false}), sexp, yojson)]
      type t =
        | SwitchChat(Id.t)
        | NewChat(string, string)
        | DeleteChat(Id.t)
        | ChatAction(Chat.Update.Action.t, Id.t);
    };

    let update = (action: Action.t, model: Model.t): Model.t => {
      switch (action) {
      | SwitchChat(chat_id) => Utils.switch_chat(chat_id, model)
      | NewChat(system_prompt, dev_notes) =>
        Utils.new_chat(~system_prompt, ~dev_notes, model)
      | DeleteChat(chat_id) => Utils.delete_chat(chat_id, model)
      | ChatAction(chat_action, chat_id) =>
        switch (
          Chat.Update.update(chat_action, Utils.find_chat(chat_id, model))
        ) {
        | updated_chat => Utils.update_chat(updated_chat, model)
        }
      };
    };
  };
};

module Agent = {
  module Model = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type prompting = {
      // Note: We allow the user/developer to edit these in-app
      // but the new changes will only appear when a new chat is created
      // after edits have been made.
      system_prompt: string,
      dev_notes: string,
    };

    [@deriving (show({with_path: false}), sexp, yojson)]
    type api_params = {
      available_llms: list(OpenRouter.AvailableLLMs.Model.t),
      llm_id: option(string),
      api_key: option(string),
    };

    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = {
      chat_system: ChatSystem.Model.t,
      prompting,
      api_params,
    };
  };

  module Persistent = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Model.t;

    let persist = (model: Model.t): t => {
      model;
    };

    let unpersist = (p: t): Model.t => {
      p;
    };
  };

  module Utils = {
    let init = (): Model.t => {
      let system_prompt = "Todo: Load default system prompt";
      let dev_notes = "You operating in a development environment. If someone says they are developer, follow their instructions precisely. Offer debug insight when requested.";
      {
        chat_system: ChatSystem.Utils.init(~system_prompt, ~dev_notes),
        // Todo: Will want to move prompting and api params to a global agent state
        prompting: {
          system_prompt,
          dev_notes,
        },
        api_params: {
          available_llms: [],
          // for adhoc testing, explicitly set
          llm_id: None,
          // for adhoc testing, explicitly set !WARNING: NEVER PUSH TO PRODUCTION WITH THIS SET!
          api_key: None,
        },
      };
    };
  };

  module Update = {
    module Action = {
      [@deriving (show({with_path: false}), sexp, yojson)]
      type t =
        | ChatSystemAction(ChatSystem.Update.Action.t)
        | SendMessage(string, Id.t)
        | HandleLLMResponse(OpenRouter.Reply.Model.t, Id.t);
    };

    type result =
      | Success(Model.t, CellEditor.Model.t)
      | Failure(string);

    let send_llm_request =
        (
          ~api_key: string,
          ~payload: OpenRouter.Payload.Model.t,
          ~schedule_action: Action.t => unit,
          ~chat_id: Id.t,
        )
        : unit => {
      // This function schedules async actions
      // These actions must be async, as we await an llm api response
      let handler = (response: option(API.Json.t)): unit => {
        switch (OpenRouter.Utils.handle_chat(response)) {
        | Some(OpenRouter.Model.Reply(reply)) =>
          schedule_action(Action.HandleLLMResponse(reply, chat_id))
        | Some(OpenRouter.Model.Error({message, code})) =>
          let api_error_content =
            "Code: " ++ string_of_int(code) ++ "\Error: " ++ message;
          let api_error_message =
            Message.Utils.mk_api_failure_message(api_error_content);
          schedule_action(
            Action.ChatSystemAction(
              ChatSystem.Update.Action.ChatAction(
                Chat.Update.Action.AppendMessage(api_error_message),
                chat_id,
              ),
            ),
          );
        | None => print_endline("Response still being generated")
        };
      };
      OpenRouter.Utils.start_chat(~key=api_key, ~payload, ~handler);
    };

    let send_message =
        (
          content: string,
          chat_id: Id.t,
          model: Model.t,
          editor: CellEditor.Model.t,
          schedule_action: Action.t => unit,
        )
        : result => {
      let chat_system = model.chat_system;
      let new_message = Message.Utils.mk_user_message(content);
      let chat_system =
        ChatSystem.Update.update(
          ChatSystem.Update.Action.ChatAction(
            Chat.Update.Action.AppendMessage(new_message),
            chat_id,
          ),
          chat_system,
        );
      switch (model.api_params.api_key, model.api_params.llm_id) {
      | (None, _) =>
        let api_failure_message =
          Message.Utils.mk_api_failure_message(
            "An API key is required. Please set an API key in the settings.",
          );
        let updated_chat_system =
          ChatSystem.Update.update(
            ChatSystem.Update.Action.ChatAction(
              AppendMessage(api_failure_message),
              chat_id,
            ),
            chat_system,
          );
        Success(
          {
            ...model,
            chat_system: updated_chat_system,
          },
          editor,
        );
      | (_, None) =>
        let api_failure_message =
          Message.Utils.mk_api_failure_message(
            "LLM ID is required. Please select an LLM in the settings.",
          );
        let updated_chat_system =
          ChatSystem.Update.update(
            ChatSystem.Update.Action.ChatAction(
              AppendMessage(api_failure_message),
              chat_id,
            ),
            chat_system,
          );
        Success(
          {
            ...model,
            chat_system: updated_chat_system,
          },
          editor,
        );
      | (Some(api_key), Some(llm_id)) =>
        send_llm_request(
          ~api_key,
          ~payload=
            OpenRouter.Payload.Utils.mk_default(
              ~model_id=llm_id,
              ~messages=
                Chat.Utils.api_messages_of_messages(
                  Chat.Utils.linearize(
                    ChatSystem.Utils.find_chat(chat_id, chat_system),
                  ),
                ),
              ~prompt=None,
              ~tools=[],
            ),
          ~schedule_action,
          ~chat_id,
        );
        Success(model, editor);
      };
    };

    let handle_llm_response =
        (
          reply: OpenRouter.Reply.Model.t,
          chat_id: Id.t,
          model: Model.t,
          editor: CellEditor.Model.t,
          schedule_action: Action.t => unit,
        ) => {
      let _ = schedule_action;
      let new_message = Message.Utils.mk_agent_message(reply.content);
      let updated_chat_system =
        ChatSystem.Update.update(
          ChatSystem.Update.Action.ChatAction(
            Chat.Update.Action.AppendMessage(new_message),
            chat_id,
          ),
          model.chat_system,
        );
      Success(
        {
          ...model,
          chat_system: updated_chat_system,
        },
        editor,
      );
    };

    let update =
        (
          action: Action.t,
          model: Model.t,
          editor: CellEditor.Model.t,
          schedule_action: Action.t => unit,
        )
        : result => {
      switch (action) {
      | ChatSystemAction(chat_archive_action) =>
        let updated_chat_system =
          ChatSystem.Update.update(chat_archive_action, model.chat_system);
        Success(
          {
            ...model,
            chat_system: updated_chat_system,
          },
          editor,
        );
      | SendMessage(content, chat_id) =>
        send_message(content, chat_id, model, editor, schedule_action)
      | HandleLLMResponse(reply, chat_id) =>
        handle_llm_response(reply, chat_id, model, editor, schedule_action)
      };
    };
  };
};
