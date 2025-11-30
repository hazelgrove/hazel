open Haz3lcore;
open Util;
open OptUtil.Syntax;

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
      | Prompt
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

    let mk_API_failure_message = (content: string): Model.t => {
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
    };
  };

  module Utils = {
    let init = (): Model.t => {
      // We have the invariant that there always exist >= 1 message in the chat,
      // That message being this initial prompt message.
      // We depend on this as the root of our tree.
      let prompt =
        Message.Utils.mk_prompt_message("Todo: Inject agent prompt");
      {
        id: Id.mk(),
        title: "New Chat",
        message_map: Id.Map.singleton(prompt.id, prompt),
        root: prompt.id,
      };
    };

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
  };
};

module ChatArchive = {
  module Model = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = {
      id: Id.t,
      chat_map: Id.Map.t(Chat.Model.t),
      current: Id.t,
    };
  };

  module Utils = {
    let init = (): Model.t => {
      let initial_chat = Chat.Utils.init();
      {
        id: Id.mk(),
        chat_map: Id.Map.singleton(initial_chat.id, initial_chat),
        current: initial_chat.id,
      };
    };

    let find_chat = (id: Id.t, model: Model.t): Chat.Model.t => {
      Id.Map.find_opt(id, model.chat_map)
      |> OptUtil.get_or_fail("[ChatArchive.Utils.find_chat] Chat not found");
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

    let new_chat = (model: Model.t): Model.t => {
      let new_chat = Chat.Utils.init();
      update_chat(new_chat, model);
    };

  };
};
