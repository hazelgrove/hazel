open Util;
open Util.API;
open Haz3lcore;
open OptUtil.Syntax;
open Ppx_yojson_conv_lib.Yojson_conv;

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

module Failure = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Info(string);
};

module Result = {
  include Result;
  type t('success) = Result.t('success, Failure.t);
};

module Message = {
  module Model = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type system_kind =
      | ApiFailure
      | DeveloperNotes // Only one should exist
      | Prompt // Only one should exist
      | Context;

    [@deriving (show({with_path: false}), sexp, yojson)]
    // Separating like such, as agent messages appear on left
    // User messages appear on right
    // System messages auxillary
    type role =
      | Agent(option(OpenRouter.Reply.Model.usage))
      | ToolResult(OpenRouter.Reply.Model.tool_result)
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
        current_child: Some(child_id),
      };
    };

    let mk_prompt_message = (content: string): Model.t => {
      let sanitized_content = String.trim(content);
      {
        id: Id.mk(),
        content: sanitized_content,
        timestamp: JsUtil.timestamp(),
        role: System(Prompt),
        api_message:
          Some(OpenRouter.Message.Utils.mk_system_msg(sanitized_content)),
        children: [],
        current_child: None,
      };
    };
    let mk_developer_notes_message = (content: string): Model.t => {
      let sanitized_content = String.trim(content);
      {
        id: Id.mk(),
        content: sanitized_content,
        timestamp: JsUtil.timestamp(),
        role: System(DeveloperNotes),
        api_message:
          Some(OpenRouter.Message.Utils.mk_developer_msg(sanitized_content)),
        children: [],
        current_child: None,
      };
    };
    let mk_agent_message =
        (content: string, usage: option(OpenRouter.Reply.Model.usage))
        : Model.t => {
      let sanitized_content = String.trim(content);
      {
        id: Id.mk(),
        content: sanitized_content,
        timestamp: JsUtil.timestamp(),
        role: Agent(usage),
        api_message:
          Some(OpenRouter.Message.Utils.mk_assistant_msg(sanitized_content)),
        children: [],
        current_child: None,
      };
    };

    let mk_tool_result_message =
        (tool_result: OpenRouter.Reply.Model.tool_result): Model.t => {
      let sanitized_content = String.trim(tool_result.content);

      let msg =
        tool_result.success
          ? "The "
            ++ tool_result.tool_call.name
            ++ " tool call with the following arguments was successful and has been applied to the model. "
            ++ " Arguments: "
            ++ Yojson.Safe.to_string(tool_result.tool_call.args)
          : sanitized_content;
      {
        // This is a message from our backend.
        // Protocols require a tool id to be associated, thus we send this is as an OpenRouter.Tool message.contents

        id: Id.mk(),
        content: sanitized_content,
        timestamp: JsUtil.timestamp(),
        role: ToolResult(tool_result),
        api_message:
          Some(
            OpenRouter.Message.Utils.mk_tool_msg(msg, tool_result.tool_call),
          ),
        children: [],
        current_child: None,
      };
    };

    let mk_user_message = (content: string): Model.t => {
      let sanitized_content = String.trim(content);
      {
        id: Id.mk(),
        content: sanitized_content,
        timestamp: JsUtil.timestamp(),
        role: User,
        api_message:
          Some(OpenRouter.Message.Utils.mk_user_msg(sanitized_content)),
        children: [],
        current_child: None,
      };
    };
    let mk_context_message =
        (
          agent_editor_content: string,
          static_errors_content: string,
          workbench_content: string,
        )
        : Model.t => {
      let sanitized_agent_editor_content = String.trim(agent_editor_content);
      let sanitized_static_errors_content =
        String.trim(static_errors_content);
      let sanitized_workbench_content = String.trim(workbench_content);

      let agent_editor_content_prefix = "\n<agentEditorView>\n```";
      let agent_editor_content_suffix = "```\n</agentEditorView>\n";
      let agent_editor_content =
        agent_editor_content_prefix
        ++ sanitized_agent_editor_content
        ++ agent_editor_content_suffix;

      let static_errors_content_prefix = "\n<staticErrorsInfo>\n";
      let static_errors_content_suffix = "\n</staticErrorsInfo>\n";
      let static_errors_content =
        static_errors_content_prefix
        ++ sanitized_static_errors_content
        ++ static_errors_content_suffix;

      let workbench_content_prefix = "\n<workbenchTaskInfo>\n";
      let workbench_content_suffix = "\n</workbenchTaskInfo>\n";
      let workbench_content =
        workbench_content_prefix
        ++ sanitized_workbench_content
        ++ workbench_content_suffix;

      let context_prefix = "\n<context>\n";
      let context_suffix = "\n</context>\n";
      let context_content =
        context_prefix
        ++ agent_editor_content
        ++ static_errors_content
        ++ workbench_content
        ++ context_suffix;
      let content =
        context_content
        ++ "\nNOTE: This is an automated message triggered before every message send. This is to provide you with rich information of the current task, editor, code, and project as a whole.";

      {
        id: Id.mk(),
        content,
        timestamp: JsUtil.timestamp(),
        role: System(Context),
        api_message: Some(OpenRouter.Message.Utils.mk_user_msg(content)),
        children: [],
        current_child: None,
      };
    };

    let mk_api_failure_message = (content: string): Model.t => {
      let sanitized_content = String.trim(content);
      {
        id: Id.mk(),
        content: sanitized_content,
        timestamp: JsUtil.timestamp(),
        role: System(ApiFailure),
        api_message: None,
        children: [],
        current_child: None,
      };
    };

    // let mk_backend_failure_message = (content: string): Model.t => {
    //   let sanitized_content = String.trim(content);
    //   let failure_instructions =
    //     {|
    //         Sorry our backend failed to process your request.
    //         This indicates a fatal bug in our backend code/server and should be reported to developers immediately.
    //         Please halt the current chat and report the issue to the user.
    //         Error:
    //         |}
    //     ++ sanitized_content;
    //   {
    //     id: Id.mk(),
    //     content,
    //     timestamp: JsUtil.timestamp(),
    //     role: System(Error(BackendFailure)),
    //     api_message:
    //       Some(OpenRouter.Message.Utils.mk_user_msg(failure_instructions)),
    //     children: [],
    //     current_child: None,
    //   };
    // };

    let api_message_of_message =
        (message: Model.t): option(OpenRouter.Message.Model.t) => {
      switch (message.api_message) {
      | Some(api_message) => Some(api_message)
      | None => None
      };
    };

    let json_of_message = (message: Model.t): Json.t =>
      `Assoc([
        (
          "role",
          switch (message.role) {
          | Agent(_) => `String("assistant")
          | User => `String("user")
          | ToolResult(_) => `String("tool")
          | System(_) => `String("system")
          },
        ),
        ("content", `String(message.content)),
        (
          "details",
          switch (message.role) {
          | System(system_kind) =>
            switch (system_kind) {
            | ApiFailure => `String("api_failure")
            | DeveloperNotes => `String("developer_notes")
            | Prompt => `String("prompt")
            | Context => `String("context")
            }
          | ToolResult(tool_result) =>
            `Assoc([
              ("tool_call_id", `String(tool_result.tool_call.id)),
              ("name", `String(tool_result.tool_call.name)),
              (
                "arguments",
                `String(Yojson.Safe.to_string(tool_result.tool_call.args)),
              ),
              ("success", `Bool(tool_result.success)),
              (
                "diff",
                switch (tool_result.diff) {
                | Some(diff) =>
                  switch (diff.new_segment) {
                  | Some(new_segment) =>
                    `Assoc([
                      (
                        "old",
                        `String(
                          Printer.of_segment(~holes="?", diff.old_segment),
                        ),
                      ),
                      (
                        "new",
                        `String(Printer.of_segment(~holes="?", new_segment)),
                      ),
                    ])
                  | None => `Null
                  }
                | None => `Null
                },
              ),
              (
                "before",
                switch (tool_result.before_segment) {
                | Some(before_segment) =>
                  `String(Printer.of_segment(~holes="?", before_segment))
                | None => `Null
                },
              ),
              (
                "after",
                switch (tool_result.after_segment) {
                | Some(after_segment) =>
                  `String(Printer.of_segment(~holes="?", after_segment))
                | None => `Null
                },
              ),
            ])
          | _ => `Null
          },
        ),
        (
          "usage",
          switch (message.role) {
          | Agent(usage) =>
            switch (usage) {
            | Some(usage) => OpenRouter.Reply.Model.yojson_of_usage(usage)
            | None => `Null
            }
          | _ => `Null
          },
        ),
        ("timestamp", `Float(message.timestamp)),
      ]);

    let append_to_message = (message: Model.t, content: string): Model.t => {
      let updated_content = message.content ++ content;
      let sanitized_content = StringUtil.trim_leading(updated_content);
      let api_message: option(OpenRouter.Message.Model.t) = {
        let* api_message = message.api_message;
        Some({
          ...api_message,
          content: sanitized_content,
        });
      };
      {
        ...message,
        content: sanitized_content,
        api_message,
      };
    };
  };

  module Update = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type action =
      | SetToolResultExpanded(bool);

    let update = (action: action, model: Model.t): Model.t => {
      switch (action) {
      | SetToolResultExpanded(expanded) =>
        switch (model.role) {
        | ToolResult(tool_result) =>
          let updated_tool_result = {
            ...tool_result,
            expanded,
          };
          {
            ...model,
            role: ToolResult(updated_tool_result),
          };
        | _ => model
        }
      };
    };
  };
};

module Chat = {
  module Model = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type current_view =
      | Messages
      | Workbench
      | AgentEditorView
      | StaticErrors
      | Prompt
      | DeveloperNotes;

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
      agent_workbench: AgentWorkbench.Model.t, // The agent's todo lists/workbench associated with this chat
      context: option(Message.Model.t),
      created_at: float,
      current_view,
    };
  };

  module Utils = {
    let find_message_opt = (id: Id.t, chat: Model.t): option(Message.Model.t) => {
      Id.Map.find_opt(id, chat.message_map);
    };

    let find_message = (id: Id.t, chat: Model.t): Message.Model.t => {
      find_message_opt(id, chat)
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
          go(child_msg, messages @ [child_msg]);
        };
      };
      let root_msg = find_message(chat.root, chat);
      go(find_message(chat.root, chat), [root_msg]);
    };

    let api_messages_of_messages =
        (messages: list(Message.Model.t)): list(OpenRouter.Message.Model.t) => {
      List.filter_map(Message.Utils.api_message_of_message, messages);
    };

    let json_of_messages =
        (messages: list(Message.Model.t), model_id: option(string)): Json.t => {
      `Assoc([
        (
          "model_id",
          switch (model_id) {
          | Some(id) => `String(id)
          | None => `Null
          },
        ),
        (
          "messages",
          `List(List.map(Message.Utils.json_of_message, messages)),
        ),
      ]);
    };

    let current_tail = (chat: Model.t): Message.Model.t => {
      // Calling linearize makes this a little costly
      // but it also keeps code safer rather than storing a current tail pointer
      let linear = linearize(chat);
      ListUtil.hd_opt(List.rev(linear))
      |> OptUtil.get_or_fail(
           "[Chat.Utils.current_tail] Failed to get current tail",
         );
    };

    let parent_of = (message_id: Id.t, chat: Model.t): Message.Model.t => {
      // Searches through all messages in the chat and checks if message_id
      // is in the list of children of any message
      // Note: It should hold that the caller knows the passed message id will have a parent
      let parent =
        List.find_map(
          (message: Message.Model.t) =>
            if (List.mem(message_id, message.children)) {
              Some(message);
            } else {
              None;
            },
          linearize(chat),
        );
      parent
      |> OptUtil.get_or_fail(
           "[Chat.Utils.parent_of] No parent exists for message: "
           ++ Id.to_string(message_id),
         );
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
      print_endline("[Chat.Utils.append] Adding new message to the chat");
      let tail_msg = current_tail(chat);
      print_endline(
        "[Chat.Utils.append] Current tail message: "
        ++ Message.Model.show(tail_msg),
      );
      let updated_tail_msg = Message.Utils.add_child(tail_msg, new_msg.id);
      print_endline(
        "[Chat.Utils.append] Updated tail message: "
        ++ Message.Model.show(updated_tail_msg),
      );
      let updated_chat = update_message(updated_tail_msg, chat);

      // Add the new message to the chat
      update_message(new_msg, updated_chat);
    };

    let overwrite_message =
        (message_id: Id.t, message: Message.Model.t, chat: Model.t): Model.t => {
      // Like append, but instead of appending, simply overwrites the message with the new one
      // This is helpful, for example, if an api error occurs after we generate an agent response.
      let updated_message_map =
        Id.Map.add(message_id, message, chat.message_map);
      {
        ...chat,
        message_map: updated_message_map,
      };
    };

    let truncate = (id: Id.t, chat: Model.t): Model.t => {
      let message = find_message(id, chat);
      // This effecively, forcefully, sets this msg as the new tail
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

    let update_context =
        (agent_editor_view: string, static_errors_info: string, chat: Model.t)
        : Model.t => {
      let workbench =
        Message.Utils.mk_context_message(
          agent_editor_view,
          static_errors_info,
          AgentWorkbench.Utils.MainUtils.active_task_to_pretty_string(
            chat.agent_workbench,
          ),
        );
      {
        ...chat,
        context: Some(workbench),
      };
    };

    let append_to_message_content =
        (message_id: Id.t, content: string, chat: Model.t): Model.t => {
      switch (find_message_opt(message_id, chat)) {
      | Some(message) =>
        let updated_message = {
          ...message,
          content: message.content ++ content,
        };
        update_message(updated_message, chat);
      | None => chat
      };
    };

    let get = (chat: Model.t): list(Message.Model.t) => {
      let linear = linearize(chat);
      switch (chat.context) {
      | Some(context) => linear @ [context]
      | None => linear
      };
    };

    let init = (~system_prompt: string, ~dev_notes: string): Model.t => {
      print_endline("[Chat.Utils.init] Initializing chat");
      let system_prompt = Message.Utils.mk_prompt_message(system_prompt);
      let chat: Model.t = {
        id: Id.mk(),
        title: "New Chat",
        message_map: Id.Map.singleton(system_prompt.id, system_prompt),
        root: system_prompt.id,
        agent_view: AgentContext.Utils.init(),
        agent_workbench: AgentWorkbench.Utils.MainUtils.init(),
        context: None,
        created_at: JsUtil.timestamp(),
        current_view: Messages,
      };
      let dev_notes = Message.Utils.mk_developer_notes_message(dev_notes);
      let chat = append(dev_notes, chat);
      chat;
    };
  };

  module Update = {
    module Action = {
      [@deriving (show({with_path: false}), sexp, yojson)]
      type t =
        | AppendMessage(Message.Model.t)
        | SwitchBranch(Id.t, Id.t)
        | BranchOff(Id.t)
        | AgentContextAction(AgentContext.Update.action)
        | WorkbenchAction(AgentWorkbench.Update.Action.action)
        | UpdateContext(string, string)
        | AppendToMessageContent(Id.t, string)
        | OverwriteMessage(Id.t, Message.Model.t)
        | SwitchView(Model.current_view)
        | MessageAction(Id.t, Message.Update.action);
    };

    let update = (action: Action.t, model: Model.t): Result.t(Model.t) => {
      switch (action) {
      | AppendMessage(message) => Ok(Utils.append(message, model))
      | SwitchBranch(fork_id, new_child_id) =>
        Ok(Utils.switch_branch(~fork_id, ~new_child_id, model))
      | BranchOff(fork_id) =>
        let truncated_model = Utils.truncate(fork_id, model);
        Ok(truncated_model);
      | AgentContextAction(agent_context_action) =>
        Ok({
          ...model,
          agent_view:
            AgentContext.Update.update(
              agent_context_action,
              model.agent_view,
            ),
        })
      | WorkbenchAction(workbench_action) =>
        let workbench =
          AgentWorkbench.Update.update(
            ~model=model.agent_workbench,
            ~action=workbench_action,
          );
        switch (workbench) {
        | Success(updated_workbench) =>
          Ok({
            ...model,
            agent_workbench: updated_workbench,
          })
        | Failure(error) => Error(Failure.Info(error))
        };
      | UpdateContext(agent_editor_view, static_errors_info) =>
        Ok(
          Utils.update_context(agent_editor_view, static_errors_info, model),
        )
      | AppendToMessageContent(message_id, content) =>
        Ok(Utils.append_to_message_content(message_id, content, model))
      | OverwriteMessage(message_id, message) =>
        Ok(Utils.overwrite_message(message_id, message, model))
      | SwitchView(current_view) =>
        Ok({
          ...model,
          current_view,
        })
      | MessageAction(message_id, message_action) =>
        let message = Utils.find_message(message_id, model);
        let updated_message = Message.Update.update(message_action, message);
        Ok(Utils.update_message(updated_message, model));
      };
    };
  };
};

module ChunkedUIChat = {
  // A dynamic, runtime converter of our chat messages to a UI-friendly format
  // This converts a linear log of messages into something more digestible for the user
  module Model = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type user_message = {
      content: string,
      origin_id: Id.t,
    };

    [@deriving (show({with_path: false}), sexp, yojson)]
    type agent_response_chunk = {
      content: list(Message.Model.t),
      agent_reasoning: list(string),
      tool_results: list(OpenRouter.Reply.Model.tool_result),
      // add workbench info
    };

    [@deriving (show({with_path: false}), sexp, yojson)]
    type chunk =
      | UserMessage(user_message)
      | AgentResponseChunk(agent_response_chunk)
      | ErrorMessage(string);

    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = {
      prompt: string,
      developer_notes: string,
      context: string,
      log: list(chunk),
    };
  };

  module Utils = {
    let mk_user_message_chunk = (message: Message.Model.t): Model.chunk => {
      UserMessage({
        content: message.content,
        origin_id: message.id,
      });
    };

    let mk_agent_response_chunk = (message: Message.Model.t): Model.chunk => {
      AgentResponseChunk({
        content: [message],
        agent_reasoning: [],
        tool_results: [],
      });
    };

    let init = (): Model.t => {
      {
        prompt: "",
        developer_notes: "",
        context: "",
        log: [],
      };
    };

    let curr_last_chunk = (model: Model.t): Model.chunk => {
      model.log
      |> List.rev
      |> ListUtil.hd_opt
      |> OptUtil.get_or_fail("No last chunk found");
    };

    let mk = (chat: Chat.Model.t): Model.t => {
      // Converts a list of messages into a list of displayable chunks.
      // The algorithm is roughly as follows:
      /*
       1. Iterate through the log of messages
         1.1 If the message is a user message, create a user message chunk
         1.2 If the message is an agent response, create an agent response chunk
       */

      let rec convert_helper =
              (chat: list(Message.Model.t), acc_model: Model.t): Model.t => {
        switch (chat) {
        | [] => acc_model
        | [message, ...rest] =>
          switch (message.role) {
          | User =>
            let chunk = mk_user_message_chunk(message);
            let updated_model = {
              ...acc_model,
              log: acc_model.log @ [chunk],
            };
            convert_helper(rest, updated_model);
          | Agent(_) =>
            switch (curr_last_chunk(acc_model)) {
            | AgentResponseChunk(agent_response_chunk) =>
              let agent_response_chunk = {
                ...agent_response_chunk,
                content: agent_response_chunk.content @ [message],
              };
              let log =
                (acc_model.log |> List.rev |> List.tl |> List.rev)
                @ [Model.AgentResponseChunk(agent_response_chunk)];
              let acc_model = {
                ...acc_model,
                log,
              };
              convert_helper(rest, acc_model);
            | _ =>
              let chunk = mk_agent_response_chunk(message);
              let updated_model = {
                ...acc_model,
                log: acc_model.log @ [chunk],
              };
              convert_helper(rest, updated_model);
            }
          | System(Prompt) =>
            let updated_model = {
              ...acc_model,
              prompt: message.content,
            };
            convert_helper(rest, updated_model);
          | System(DeveloperNotes) =>
            let updated_model = {
              ...acc_model,
              developer_notes: message.content,
            };
            convert_helper(rest, updated_model);
          | System(Context) =>
            let updated_model = {
              ...acc_model,
              context: message.content,
            };
            convert_helper(rest, updated_model);
          | ToolResult(tool_result) =>
            let curr_last_chunk = curr_last_chunk(acc_model);
            let updated_agent_chunk =
              switch (curr_last_chunk) {
              | AgentResponseChunk(agent_response_chunk) => {
                  ...agent_response_chunk,
                  content: agent_response_chunk.content @ [message],
                  tool_results:
                    agent_response_chunk.tool_results @ [tool_result],
                }
              | _ => failwith("Expected AgentResponseChunk before ToolResult")
              };
            let updated_log =
              (acc_model.log |> List.rev |> List.tl |> List.rev)
              @ [Model.AgentResponseChunk(updated_agent_chunk)];
            let updated_model = {
              ...acc_model,
              log: updated_log,
            };
            convert_helper(rest, updated_model);
          | System(ApiFailure) =>
            let chunk = Model.ErrorMessage(message.content);
            let updated_model = {
              ...acc_model,
              log: acc_model.log @ [chunk],
            };
            convert_helper(rest, updated_model);
          }
        };
      };

      let chat = Chat.Utils.get(chat);
      let res = init();

      convert_helper(chat, res);
    };
  };
};

module ChatSystem = {
  module Model = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type active_screen =
      | Chat
      | History;

    [@deriving (show({with_path: false}), sexp, yojson)]
    type ui = {
      active_screen,
      current_text_box_content: string,
    };

    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = {
      chat_map: Id.Map.t(Chat.Model.t),
      current: Id.t,
      ui,
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
      let model = update_chat(new_chat, model);
      {
        ...model,
        current: new_chat.id,
      };
    };

    let delete_chat = (id: Id.t, model: Model.t): Model.t => {
      {
        ...model,
        chat_map: Id.Map.remove(id, model.chat_map),
        current: Id.Map.choose(model.chat_map) |> fst,
      };
    };

    let chats_to_list = (model: Model.t): list(Chat.Model.t) => {
      // Converts the map of chats to a list of chats
      // ordered by the created_at timestamp
      Id.Map.bindings(model.chat_map)
      |> List.map(((_, chat: Chat.Model.t)) => chat)
      |> List.sort((a: Chat.Model.t, b: Chat.Model.t) =>
           Float.compare(a.created_at, b.created_at)
         );
    };

    let init = (~system_prompt: string, ~dev_notes: string): Model.t => {
      let initial_chat = Chat.Utils.init(~system_prompt, ~dev_notes);
      {
        chat_map: Id.Map.singleton(initial_chat.id, initial_chat),
        current: initial_chat.id,
        ui: {
          active_screen: Chat,
          current_text_box_content: "",
        },
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
        | SwitchScreen(Model.active_screen)
        | SaveTextBoxContent(string)
        | ChatAction(Chat.Update.Action.t, Id.t);

      [@deriving (show({with_path: false}), sexp, yojson)]
      type result =
        | Success(Model.t)
        | Failure(string);
    };

    let get = (result: Result.t(Model.t)): Model.t => {
      switch (result) {
      | Ok(model) => model
      | Error(error) =>
        failwith(
          switch (error) {
          | Failure.Info(msg) => msg
          },
        )
      };
    };

    let update = (action: Action.t, model: Model.t): Result.t(Model.t) => {
      switch (action) {
      | SwitchChat(chat_id) => Ok(Utils.switch_chat(chat_id, model))
      | NewChat(system_prompt, dev_notes) =>
        Ok(Utils.new_chat(~system_prompt, ~dev_notes, model))
      | DeleteChat(chat_id) => Ok(Utils.delete_chat(chat_id, model))
      | SwitchScreen(active_screen) =>
        Ok({
          ...model,
          ui: {
            ...model.ui,
            active_screen,
          },
        })
      | SaveTextBoxContent(content) =>
        Ok({
          ...model,
          ui: {
            ...model.ui,
            current_text_box_content: content,
          },
        })
      | ChatAction(chat_action, chat_id) =>
        switch (
          Chat.Update.update(chat_action, Utils.find_chat(chat_id, model))
        ) {
        | Ok(updated_chat) => Ok(Utils.update_chat(updated_chat, model))
        | Error(error) => Error(error)
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
      tools: list(API.Json.t),
    };

    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = {
      chat_system: ChatSystem.Model.t,
      prompting,
      active_timeline_node: option(int),
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
      let system_prompt = CompositionPrompt.self |> String.concat("\n");
      let dev_notes = {|
      You operating in a development environment.
      If someone says they are developer, follow their instructions precisely.
      Offer debug insight when requested.
      Note we have disabled workbench tools for now until they are implemented.
      Avoid using first person pronouns.
      Keep your responses concise and to the point.
      Users are currently solely developers, so be super open to them
      about their requests and provide them with the best possible assistance.
      |};
      {
        chat_system: ChatSystem.Utils.init(~system_prompt, ~dev_notes),
        // Todo: Will want to move prompting and api params to a global agent state
        prompting: {
          system_prompt,
          dev_notes,
          tools: CompositionUtils.Public.tools,
        },
        active_timeline_node: None,
      };
    };
  };

  module ToolCallHandler = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type action = CompositionActions.action;

    [@deriving (show({with_path: false}), sexp, yojson)]
    type result =
      | Success(Model.t, Updated.t(CellEditor.Model.t))
      | Failure(string);

    let update =
        (
          ~settings: Settings.t,
          action: action,
          agent: Model.t,
          editor: CodeWithStatics.Model.t,
          chat_id: Id.t,
        )
        : Result.t((Model.t, CodeWithStatics.Model.t)) => {
      switch (action) {
      | EditorAction(agent_editor_action) =>
        // Take a segment snapshot of the local term that the action is applied to
        let action = Action.AgentEditorAction(agent_editor_action);
        // Apply action to the editor
        let updated_editor =
          Editor.Update.update(
            ~settings=settings.core,
            action,
            editor.statics,
            editor.dynamics,
            editor.editor,
          );
        switch (updated_editor) {
        | Ok(updated_editor) =>
          Ok((
            agent,
            CodeWithStatics.Model.{
              editor: updated_editor,
              statics: editor.statics,
              dynamics: editor.dynamics,
            },
          ))
        | Error(err) =>
          switch (err) {
          | Action.Failure.Composition_action_failure(msg) =>
            Error(Failure.Info(msg))
          | _ =>
            Error(
              Failure.Info(
                "Unknown error occured when trying to apply tool request to editor",
              ),
            )
          }
        };
      | WorkbenchAction(workbench_action) =>
        let action =
          AgentWorkbench.Update.Action.BackendAction(workbench_action);
        let chat_system =
          ChatSystem.Update.update(
            ChatSystem.Update.Action.ChatAction(
              Chat.Update.Action.WorkbenchAction(action),
              chat_id,
            ),
            agent.chat_system,
          );
        switch (chat_system) {
        | Ok(updated_chat_system) =>
          Ok((
            {
              ...agent,
              chat_system: updated_chat_system,
            },
            editor,
          ))
        | Error(error) => Error(error)
        };
      | AgentContextAction(agent_context_action) =>
        let action = agent_context_action;
        let chat_system =
          ChatSystem.Update.update(
            ChatSystem.Update.Action.ChatAction(
              Chat.Update.Action.AgentContextAction(action),
              chat_id,
            ),
            agent.chat_system,
          );
        switch (chat_system) {
        | Ok(updated_chat_system) =>
          Ok((
            {
              ...agent,
              chat_system: updated_chat_system,
            },
            editor,
          ))
        | Error(error) => Error(error)
        };
      };
    };
  };

  module Update = {
    module Action = {
      [@deriving (show({with_path: false}), sexp, yojson)]
      type t =
        | ChatSystemAction(ChatSystem.Update.Action.t)
        | SendMessage(Message.Model.t, Id.t)
        | HandleLLMResponse(OpenRouter.Reply.Model.t, Id.t)
        | LoadSegmentIntoEditor(Segment.t)
        | SetActiveTimelineNode(option(int));
    };

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
      print_endline("Defining handler");
      let handler = (response: option(API.Json.t)): unit => {
        switch (response) {
        | Some(response) =>
          print_endline(
            "Handler triggered with response: "
            ++ API.Json.to_string(response),
          )
        | None => print_endline("No response received yet")
        };
        switch (OpenRouter.Utils.handle_chat(response)) {
        | Some(OpenRouter.Model.Reply(reply)) =>
          schedule_action(Action.HandleLLMResponse(reply, chat_id))
        | Some(OpenRouter.Model.Error({message, code})) =>
          let api_error_content =
            "Code: " ++ string_of_int(code) ++ "\\Error: " ++ message;
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
        | None => print_endline("Response failed to be parsed")
        };
      };
      OpenRouter.Utils.start_chat(~key=api_key, ~payload, ~handler);
    };

    let send_message =
        (
          ~api_key: option(string),
          ~llm_id: option(string),
          new_message: Message.Model.t,
          chat_id: Id.t,
          model: Model.t,
          schedule_action: Action.t => unit,
        )
        : Result.t(Model.t) => {
      let chat_system = model.chat_system;
      let chat_system =
        ChatSystem.Update.update(
          ChatSystem.Update.Action.ChatAction(
            Chat.Update.Action.AppendMessage(new_message),
            chat_id,
          ),
          chat_system,
        )
        |> ChatSystem.Update.get;
      // NOTE: deprecating streaming for now.
      // Has too many downstream implications and case-handling.
      // let empty_agent_response = Message.Utils.mk_agent_message("");
      // let chat_system =
      //   ChatSystem.Update.update(
      //     ChatSystem.Update.Action.ChatAction(
      //       Chat.Update.Action.AppendMessage(empty_agent_response),
      //       chat_id,
      //     ),
      //     chat_system,
      //   )
      //   |> ChatSystem.Update.get;
      // let agent_msg_id =
      //   Chat.Utils.current_tail(
      //     ChatSystem.Utils.find_chat(chat_id, chat_system),
      //   ).
      //     id;
      switch (api_key, llm_id) {
      | (None, _) =>
        let api_failure_message =
          Message.Utils.mk_api_failure_message(
            "An API key is required. Please set an API key in the settings.",
          );
        let chat_system =
          ChatSystem.Update.update(
            ChatSystem.Update.Action.ChatAction(
              AppendMessage(api_failure_message),
              chat_id,
            ),
            chat_system,
          )
          |> ChatSystem.Update.get;
        Ok({
          ...model,
          chat_system,
        });
      | (_, None) =>
        let api_failure_message =
          Message.Utils.mk_api_failure_message(
            "LLM ID is required. Please select an LLM in the settings.",
          );
        let chat_system =
          ChatSystem.Update.update(
            ChatSystem.Update.Action.ChatAction(
              AppendMessage(api_failure_message),
              chat_id,
            ),
            chat_system,
          )
          |> ChatSystem.Update.get;
        Ok({
          ...model,
          chat_system,
        });
      | (Some(api_key), Some(llm_id)) =>
        print_endline(
          "Showing outgoing chat messages\' content: "
          ++ String.concat(
               "\n",
               List.map(
                 (msg: Message.Model.t) => msg.content,
                 Chat.Utils.get(
                   ChatSystem.Utils.find_chat(chat_id, chat_system),
                 )
                 |> List.filter((msg: Message.Model.t) =>
                      msg.role != System(Prompt)
                    ),
               ),
             ),
        );
        send_llm_request(
          ~api_key,
          ~payload=
            OpenRouter.Payload.Utils.mk_default(
              ~model_id=llm_id,
              ~messages=
                Chat.Utils.api_messages_of_messages(
                  Chat.Utils.get(
                    ChatSystem.Utils.find_chat(chat_id, chat_system),
                  ),
                ),
              ~tools=model.prompting.tools,
            ),
          ~schedule_action,
          ~chat_id,
        );
        Ok({
          ...model,
          chat_system,
        });
      };
    };

    let update_context =
        (model: Model.t, editor: CodeWithStatics.Model.t, chat_id: Id.t)
        : Model.t => {
      let curr_chat = ChatSystem.Utils.find_chat(chat_id, model.chat_system);
      let agent_editor_view_string =
        CompositionView.Public.print(editor.editor, curr_chat.agent_view);
      let static_errors_info_string =
        ErrorPrint.all(Perform.mk_statics(editor.editor.state.zipper))
        |> String.concat("\n");
      let chat_system =
        ChatSystem.Update.update(
          ChatSystem.Update.Action.ChatAction(
            Chat.Update.Action.UpdateContext(
              agent_editor_view_string,
              static_errors_info_string,
            ),
            chat_id,
          ),
          model.chat_system,
        );
      switch (chat_system) {
      | Ok(chat_system) => {
          ...model,
          chat_system,
        }
      | Error(error) =>
        failwith(
          switch (error) {
          | Failure.Info(msg) => msg
          },
        )
      };
    };

    let add_tool_result_to_active_subtask =
        (
          ~tool_result: OpenRouter.Reply.Model.tool_result,
          ~action: CompositionActions.action,
          ~model: Model.t,
          ~chat_id: Id.t,
        )
        : Model.t => {
      switch (action) {
      // Only add editor and context tools to subtask tool results for now
      // i.e. don't include workbench tool calls in the workbench view lol
      | EditorAction(_)
      | AgentContextAction(_) =>
        let chat_system =
          ChatSystem.Update.update(
            ChatSystem.Update.Action.ChatAction(
              Chat.Update.Action.WorkbenchAction(
                AgentWorkbench.Update.Action.UIAction(
                  AgentWorkbench.Update.Action.UIAction.AddToolResultToActiveSubtask(
                    tool_result,
                  ),
                ),
              ),
              chat_id,
            ),
            model.chat_system,
          )
          |> ChatSystem.Update.get;
        {
          ...model,
          chat_system,
        };
      | _ => model
      };
    };

    let mk_diff =
        (
          ~old_editor: Editor.t,
          ~new_editor: Editor.t,
          action: CompositionActions.action,
        )
        : option(OpenRouter.Reply.Model.diff) => {
      let* action =
        switch (action) {
        | EditorAction(editor_action) =>
          switch (editor_action) {
          | Edit(edit_action) => Some(edit_action)
          | _ => None
          }
        | _ => None
        };
      let* diff =
        CompositionGo.Local.get_diff(
          old_editor.state.zipper,
          new_editor.state.zipper,
          action,
          Perform.mk_statics,
          old_editor.syntax,
        );
      Some(
        OpenRouter.Reply.Model.{
          old_segment: diff |> fst,
          new_segment: diff |> snd,
        },
      );
    };

    let mk_segment_snapshots =
        (
          ~old_editor: Editor.t,
          ~new_editor: Editor.t,
          action: CompositionActions.action,
        )
        : (option(Segment.t), option(Segment.t)) => {
      switch (action) {
      | EditorAction(_) =>
        let old_segment =
          Select.all(old_editor.state.zipper).selection.content;
        let new_segment =
          Select.all(new_editor.state.zipper).selection.content;
        (Some(old_segment), Some(new_segment));
      | _ => (None, None)
      };
    };

    let handle_tool_call =
        (
          ~tool_call: OpenRouter.Reply.Model.tool_call,
          ~model: Model.t,
          ~cell_editor: CellEditor.Model.t,
          ~settings: Settings.t,
          ~schedule_action: Action.t => unit,
          ~chat_id: Id.t,
        ) => {
      switch (
        CompositionUtils.Public.action_of(
          ~tool_name=tool_call.name,
          ~args=tool_call.args,
        )
      ) {
      | Action(action) =>
        switch (
          ToolCallHandler.update(
            ~settings,
            action,
            model,
            cell_editor.editor,
            chat_id,
          )
        ) {
        | Ok((model, editor)) =>
          let model = update_context(model, editor, chat_id);
          let success_message =
            "The "
            ++ tool_call.name
            ++ " tool call was successful and has been applied to the model.";
          let (before_segment, after_segment) =
            mk_segment_snapshots(
              ~old_editor=cell_editor.editor.editor,
              ~new_editor=editor.editor,
              action,
            );
          let tool_result: OpenRouter.Reply.Model.tool_result = {
            tool_call,
            success: true,
            expanded: false,
            diff:
              mk_diff(
                ~old_editor=cell_editor.editor.editor,
                ~new_editor=editor.editor,
                action,
              ),
            before_segment,
            after_segment,
            content: success_message,
          };
          let model =
            add_tool_result_to_active_subtask(
              ~tool_result,
              ~action,
              ~model,
              ~chat_id,
            );
          schedule_action(
            Action.SendMessage(
              Message.Utils.mk_tool_result_message(tool_result),
              chat_id,
            ),
          );
          (
            model,
            {
              ...cell_editor,
              editor,
            }
            |> Updated.return,
          );
        | Error(error) =>
          switch (error) {
          | Failure.Info(msg) =>
            let before_segment =
              switch (action) {
              | EditorAction(_) =>
                Some(
                  Select.all(cell_editor.editor.editor.state.zipper).selection.
                    content,
                )
              | _ => None
              };
            let tool_result: OpenRouter.Reply.Model.tool_result = {
              tool_call,
              success: false,
              expanded: false,
              diff: None,
              before_segment,
              after_segment: None,
              content: msg,
            };
            let model =
              add_tool_result_to_active_subtask(
                ~tool_result,
                ~action,
                ~model,
                ~chat_id,
              );
            schedule_action(
              Action.SendMessage(
                Message.Utils.mk_tool_result_message(tool_result),
                chat_id,
              ),
            );
            (model, cell_editor |> Updated.return_quiet);
          }
        }
      | Failure(msg) =>
        let tool_result: OpenRouter.Reply.Model.tool_result = {
          tool_call,
          success: false,
          expanded: false,
          diff: None,
          before_segment: None,
          after_segment: None,
          content: msg,
        };
        // Do not add unparseable tool calls to subtask tool results for now
        schedule_action(
          Action.SendMessage(
            Message.Utils.mk_tool_result_message(tool_result),
            chat_id,
          ),
        );
        (model, cell_editor |> Updated.return_quiet);
      };
    };

    let handle_llm_response =
        (
          reply: OpenRouter.Reply.Model.t,
          chat_id: Id.t,
          model: Model.t,
          cell_editor: CellEditor.Model.t,
          settings: Settings.t,
          schedule_action: Action.t => unit,
        )
        : (Model.t, Updated.t(CellEditor.Model.t)) => {
      print_endline(
        "Handling LLM response: " ++ OpenRouter.Reply.Model.show(reply),
      );
      let tool_call = ListUtil.hd_opt(reply.tool_calls);
      let new_message =
        Message.Utils.mk_agent_message(reply.content, reply.usage);
      let chat_system =
        ChatSystem.Update.update(
          ChatSystem.Update.Action.ChatAction(
            Chat.Update.Action.AppendMessage(new_message),
            chat_id,
          ),
          model.chat_system,
        )
        |> ChatSystem.Update.get;
      let model = {
        ...model,
        chat_system,
      };
      switch (tool_call) {
      | Some(tool_call) =>
        handle_tool_call(
          ~tool_call,
          ~model,
          ~cell_editor,
          ~settings,
          ~schedule_action,
          ~chat_id,
        )
      | None => (model, cell_editor |> Updated.return_quiet)
      };
    };

    let update =
        (
          action: Action.t,
          model: Model.t,
          editor: CellEditor.Model.t,
          settings: Settings.t,
          schedule_action: Action.t => unit,
        )
        : (Model.t, Updated.t(CellEditor.Model.t)) => {
      switch (action) {
      | ChatSystemAction(chat_archive_action) =>
        let chat_system =
          ChatSystem.Update.update(chat_archive_action, model.chat_system);
        switch (chat_system) {
        | Ok(chat_system) => (
            {
              ...model,
              chat_system,
            },
            editor |> Updated.return,
          )
        | Error(error) =>
          failwith(
            switch (error) {
            | Failure.Info(msg) => msg
            },
          )
        };
      | SendMessage(message, chat_id) =>
        let model = update_context(model, editor.editor, chat_id);
        switch (
          send_message(
            ~api_key=settings.agent_globals.api_key,
            ~llm_id=AgentGlobals.get_active_llm_id(settings.agent_globals),
            message,
            chat_id,
            model,
            schedule_action,
          )
        ) {
        | Ok(model) => (model, editor |> Updated.return)
        | Error(error) =>
          failwith(
            switch (error) {
            | Failure.Info(msg) => msg
            },
          )
        };
      | HandleLLMResponse(reply, chat_id) =>
        handle_llm_response(
          reply,
          chat_id,
          model,
          editor,
          settings,
          schedule_action,
        )
      | LoadSegmentIntoEditor(segment) =>
        // Replace editor with segment by converting to zipper
        let new_zipper = Zipper.unzip(~direction=Right, segment);
        let new_editor_model = Editor.Model.mk(new_zipper);
        let new_code_with_statics =
          CodeWithStatics.Model.mk(new_editor_model);
        (
          model,
          {
            ...editor,
            editor: new_code_with_statics,
          }
          |> Updated.return,
        );
      | SetActiveTimelineNode(node_index) => (
          {
            ...model,
            active_timeline_node: node_index,
          },
          editor |> Updated.return,
        )
      };
    };
  };
};
