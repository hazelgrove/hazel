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
      | Context
      | RetryNote // Research transparency: empty/API retries
      | CompactionSummary(string); // method label for UI; ends API prefix before this on older turns

    [@deriving (show({with_path: false}), sexp, yojson)]
    // Separating like such, as agent messages appear on left
    // User messages appear on right
    // System messages auxillary
    type role =
      | Agent(option(OpenRouter.Reply.Model.usage))
      | ToolResult(AgentToolResult.tool_result)
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
    let mk_retry_note_message =
        (~content: string, ~sent_to_api: bool): Model.t => {
      let sanitized_content = String.trim(content);
      {
        id: Id.mk(),
        content: sanitized_content,
        timestamp: JsUtil.timestamp(),
        role: System(RetryNote),
        api_message:
          sent_to_api
            ? Some(
                OpenRouter.Message.Utils.mk_developer_msg(sanitized_content),
              )
            : None,
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
        (
          ~tool_calls: list(OpenRouter.Reply.Model.tool_call)=[],
          content: string,
          usage: option(OpenRouter.Reply.Model.usage),
        )
        : Model.t => {
      let sanitized_content = String.trim(content);
      {
        id: Id.mk(),
        content: sanitized_content,
        timestamp: JsUtil.timestamp(),
        role: Agent(usage),
        api_message:
          Some(
            OpenRouter.Message.Utils.mk_assistant_msg(
              ~tool_calls,
              sanitized_content,
            ),
          ),
        children: [],
        current_child: None,
      };
    };

    let is_read_action = (name: string): bool =>
      switch (name) {
      | "get_syntax"
      | "get_statics"
      | "get_context" => true
      | _ => false
      };

    let mk_tool_result_message =
        (tool_result: AgentToolResult.tool_result): Model.t => {
      let sanitized_content = String.trim(tool_result.content);

      let msg =
        if (!tool_result.success) {
          sanitized_content;
        } else if (is_read_action(tool_result.tool_call.name)) {
          /* Read actions return their content directly to the LLM */
          sanitized_content;
        } else {
          "The "
          ++ tool_result.tool_call.name
          ++ " tool call with the following arguments was successful and has been applied to the model. "
          ++ " Arguments: "
          ++ Yojson.Safe.to_string(tool_result.tool_call.args);
        };
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
          ~cursor_context_content: string="",
          agent_editor_content: string,
          static_errors_content: string,
          test_results_content: string,
          workbench_content: string,
        )
        : Model.t => {
      let sanitized_agent_editor_content = String.trim(agent_editor_content);
      let sanitized_static_errors_content =
        String.trim(static_errors_content);
      let sanitized_test_results_content = String.trim(test_results_content);
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

      let test_results_content_prefix = "\n<testResultsInfo>\n";
      let test_results_content_suffix = "\n</testResultsInfo>\n";
      let test_results_content =
        test_results_content_prefix
        ++ sanitized_test_results_content
        ++ test_results_content_suffix;

      let workbench_content_prefix = "\n<workbenchTaskInfo>\n";
      let workbench_content_suffix = "\n</workbenchTaskInfo>\n";
      let workbench_content =
        workbench_content_prefix
        ++ sanitized_workbench_content
        ++ workbench_content_suffix;

      /* TODO: cursor context is sent on every message but the agent operates
         path-based, not cursor-based. This may be unnecessary noise.
         Consider gating on relevance or removing for path-based agents. */
      let cursor_context_section =
        switch (String.trim(cursor_context_content)) {
        | "" => ""
        | s => "\n<cursorContext>\n" ++ s ++ "\n</cursorContext>\n"
        };

      let context_prefix = "\n<context>\n";
      let context_suffix = "\n</context>\n";
      let context_content =
        context_prefix
        ++ agent_editor_content
        ++ static_errors_content
        ++ test_results_content
        ++ cursor_context_section
        ++ workbench_content
        ++ context_suffix;
      let content =
        context_content
        ++ "\n[CONTEXT UPDATE — Do not respond to this. It is an automated snapshot of the current program state. Continue with your current task without acknowledging this message.]";

      {
        id: Id.mk(),
        content,
        timestamp: JsUtil.timestamp(),
        role: System(Context),
        api_message: Some(OpenRouter.Message.Utils.mk_system_msg(content)),
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

    /** Summary of prior turns for the API; [method] is shown in the chat UI. */
    let mk_compaction_summary = (~method: string, content: string): Model.t => {
      let sanitized_content = String.trim(content);
      let api_text =
        "[Prior conversation summary — "
        ++ method
        ++ "]\n\n"
        ++ sanitized_content;
      {
        id: Id.mk(),
        content: sanitized_content,
        timestamp: JsUtil.timestamp(),
        role: System(CompactionSummary(method)),
        api_message: Some(OpenRouter.Message.Utils.mk_system_msg(api_text)),
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
            | RetryNote => `String("retry_note")
            | CompactionSummary(method) =>
              `Assoc([("compaction_summary", `String(method))])
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
                          CompositionView.Public.print_segment(
                            diff.old_segment,
                          ),
                        ),
                      ),
                      (
                        "new",
                        `String(
                          CompositionView.Public.print_segment(new_segment),
                        ),
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
                  `String(
                    CompositionView.Public.print_segment(before_segment),
                  )
                | None => `Null
                },
              ),
              (
                "after",
                switch (tool_result.after_segment) {
                | Some(after_segment) =>
                  `String(
                    CompositionView.Public.print_segment(after_segment),
                  )
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
      | DeveloperNotes
      | Tools;

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
      let tail_msg = current_tail(chat);
      let updated_tail_msg = Message.Utils.add_child(tail_msg, new_msg.id);
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
        (
          ~cursor_context: string="",
          agent_editor_view: string,
          static_errors_info: string,
          test_results_info: string,
          chat: Model.t,
        )
        : Model.t => {
      let workbench =
        Message.Utils.mk_context_message(
          ~cursor_context_content=cursor_context,
          agent_editor_view,
          static_errors_info,
          test_results_info,
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

    let rec last_compaction_index =
            (i: int, acc: option(int), msgs: list(Message.Model.t))
            : option(int) => {
      switch (msgs) {
      | [] => acc
      | [m, ...rest] =>
        let acc' =
          switch (m.role) {
          | Message.Model.System(Message.Model.CompactionSummary(_)) =>
            Some(i)
          | _ => acc
          };
        last_compaction_index(i + 1, acc', rest);
      };
    };

    /** Transcript segment to summarize: after dev notes (or after the latest compaction on this branch). */
    let dialogue_slice_for_compaction_summary =
        (chat: Model.t): list(Message.Model.t) => {
      let linear = linearize(chat);
      let start =
        switch (last_compaction_index(0, None, linear)) {
        | None => 2
        | Some(i) => i + 1
        };
      ListUtil.remove_first_n(start, linear);
    };

    /** OpenRouter payload for the main agent: always prompt + dev notes + active suffix from latest compaction, then context. */
    let messages_for_openrouter = (chat: Model.t): list(Message.Model.t) => {
      let linear = linearize(chat);
      let trimmed =
        switch (last_compaction_index(0, None, linear)) {
        | None => linear
        | Some(idx) =>
          switch (linear) {
          | [p, d, ..._] =>
            let suffix = ListUtil.remove_first_n(idx, linear);
            [p, d, ...suffix];
          | _ => linear
          }
        };
      switch (chat.context) {
      | Some(context) => trimmed @ [context]
      | None => trimmed
      };
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
    [@deriving (show({with_path: false}), sexp, yojson)]
    type context_snapshot = {
      agent_editor_view: string,
      static_errors: string,
      test_results: string,
      cursor_context: string,
    };

    module Action = {
      [@deriving (show({with_path: false}), sexp, yojson)]
      type t =
        | AppendMessage(Message.Model.t)
        | SwitchBranch(Id.t, Id.t)
        | BranchOff(Id.t)
        | AgentContextAction(AgentContext.Update.action)
        | WorkbenchAction(AgentWorkbench.Update.Action.action)
        | UpdateContext(context_snapshot)
        | AppendToMessageContent(Id.t, string)
        | OverwriteMessage(Id.t, Message.Model.t)
        | SwitchView(Model.current_view)
        | MessageAction(Id.t, Message.Update.action)
        | SetTitle(string);
    };

    let update = (action: Action.t, model: Model.t): Result.t(Model.t) => {
      switch (action) {
      | SetTitle(title) =>
        Ok({
          ...model,
          title,
        })
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
      | UpdateContext({
          agent_editor_view,
          static_errors,
          test_results,
          cursor_context,
        }) =>
        Ok(
          Utils.update_context(
            ~cursor_context,
            agent_editor_view,
            static_errors,
            test_results,
            model,
          ),
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
      tool_results: list(AgentToolResult.tool_result),
      // add workbench info
    };

    [@deriving (show({with_path: false}), sexp, yojson)]
    type compaction_notice = {
      method: string,
      content: string,
    };

    [@deriving (show({with_path: false}), sexp, yojson)]
    type chunk =
      | UserMessage(user_message)
      | AgentResponseChunk(agent_response_chunk)
      | CompactionNotice(compaction_notice)
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
          | System(CompactionSummary(method)) =>
            let chunk =
              Model.CompactionNotice({
                method,
                content: message.content,
              });
            let updated_model = {
              ...acc_model,
              log: acc_model.log @ [chunk],
            };
            convert_helper(rest, updated_model);
          | System(RetryNote) =>
            switch (curr_last_chunk(acc_model)) {
            | AgentResponseChunk(agent_response_chunk) =>
              let agent_response_chunk = {
                ...agent_response_chunk,
                content: agent_response_chunk.content @ [message],
              };
              let log =
                (acc_model.log |> List.rev |> List.tl |> List.rev)
                @ [Model.AgentResponseChunk(agent_response_chunk)];
              let updated_model = {
                ...acc_model,
                log,
              };
              convert_helper(rest, updated_model);
            | UserMessage(_)
            | CompactionNotice(_)
            | ErrorMessage(_) =>
              let chunk = mk_agent_response_chunk(message);
              let updated_model = {
                ...acc_model,
                log: acc_model.log @ [chunk],
              };
              convert_helper(rest, updated_model);
            }
          }
        };
      };

      let chat = Chat.Utils.get(chat);
      let res = init();

      convert_helper(chat, res);
    };
  };
};

/** Slash commands in the chat input (see ChatBottomBar). Alphabetically ordered names. */
module ChatSlashCommands = {
  let all_alphabetical: list((string, string)) = [
    ("compact", "Summarize the conversation"),
  ];

  let filtered = (filter: string): list((string, string)) => {
    let f = String.lowercase_ascii(filter);
    all_alphabetical
    |> List.filter(((name, _)) =>
         String.length(f) == 0
         || String.starts_with(~prefix=f, String.lowercase_ascii(name))
       );
  };
};

module ChatSystem = {
  module Model = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type active_screen =
      | Chat
      | History;

    [@deriving (show({with_path: false}), sexp, yojson)]
    type slash_menu_state = {
      filter: string,
      selected_index: int,
    };

    [@deriving (show({with_path: false}), sexp, yojson)]
    type ui = {
      active_screen,
      current_text_box_content: string,
      [@yojson.default None]
      slash_menu: option(slash_menu_state),
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
          slash_menu: None,
        },
      };
    };

    let derive_slash_menu_from_content =
        (~prev: option(Model.slash_menu_state), content: string)
        : option(Model.slash_menu_state) =>
      if (String.length(content) < 1 || content.[0] != '/') {
        None;
      } else {
        let after_slash = String.sub(content, 1, String.length(content) - 1);
        if (String.contains(after_slash, ' ')) {
          None;
        } else {
          let prev_filter =
            Option.map((s: Model.slash_menu_state) => s.filter, prev);
          let selected_index =
            switch (prev_filter) {
            | Some(f) when f == after_slash =>
              Option.map(
                (s: Model.slash_menu_state) => s.selected_index,
                prev,
              )
              |> Option.value(~default=0)
            | _ => 0
            };
          Some({
            filter: after_slash,
            selected_index,
          });
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
        | SlashMenuAdjustSelection(int)
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
      | SwitchChat(chat_id) =>
        let m = Utils.switch_chat(chat_id, model);
        Ok({
          ...m,
          ui: {
            ...m.ui,
            slash_menu: None,
          },
        });
      | NewChat(system_prompt, dev_notes) =>
        let m = Utils.new_chat(~system_prompt, ~dev_notes, model);
        Ok({
          ...m,
          ui: {
            ...m.ui,
            slash_menu: None,
          },
        });
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
            slash_menu:
              Utils.derive_slash_menu_from_content(
                ~prev=model.ui.slash_menu,
                content,
              ),
          },
        })
      | SlashMenuAdjustSelection(delta) =>
        switch (model.ui.slash_menu) {
        | None => Ok(model)
        | Some(sm) =>
          let cmds = ChatSlashCommands.filtered(sm.filter);
          let n = List.length(cmds);
          if (n == 0) {
            Ok(model);
          } else {
            let idx = (sm.selected_index + delta + n * 1000) mod n;
            Ok({
              ...model,
              ui: {
                ...model.ui,
                slash_menu:
                  Some({
                    ...sm,
                    selected_index: idx,
                  }),
              },
            });
          };
        }
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
      disabled_tool_names: list(string),
    };

    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = {
      chat_system: ChatSystem.Model.t,
      prompting,
      active_timeline_node: option(int),
      awaiting_response: option(Id.t),
      restore_editor_state: option(Segment.t),
      last_empty_retry_attempt: option(int),
      last_active_task_nudge_attempt: option(int),
      tools_view_expanded: list(string),
      [@yojson.default None]
      compaction_in_progress: option(Id.t),
      [@yojson.default None]
      compaction_method_override: option(string),
    };
  };

  module Persistent = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Model.t;

    let persist = (model: Model.t): t => {
      {
        ...model,
        restore_editor_state: None,
        last_empty_retry_attempt: None,
        last_active_task_nudge_attempt: None,
        tools_view_expanded: [],
        compaction_in_progress: None,
        compaction_method_override: None,
      };
    };

    let unpersist = (p: t): Model.t => {
      {
        ...p,
        awaiting_response: None,
        restore_editor_state: None,
        last_empty_retry_attempt: None,
        last_active_task_nudge_attempt: None,
        tools_view_expanded: [],
        compaction_in_progress: None,
        compaction_method_override: None,
      };
    };
  };

  module ToolUtils = {
    let get_name = (tool: API.Json.t): option(string) => {
      switch (API.Json.dot("function", tool)) {
      | Some(func) =>
        switch (API.Json.dot("name", func)) {
        | Some(name_json) => API.Json.str(name_json)
        | None => None
        }
      | None => None
      };
    };

    let get_description = (tool: API.Json.t): option(string) => {
      switch (API.Json.dot("function", tool)) {
      | Some(func) =>
        switch (API.Json.dot("description", func)) {
        | Some(desc_json) => API.Json.str(desc_json)
        | None => None
        }
      | None => None
      };
    };

    let category_of_tool = (name: string): string => {
      switch (name) {
      | n
          when
            List.mem(
              n,
              [
                "expand",
                "collapse",
                "place_probe",
                "remove_probe",
                "toggle_probe",
              ],
            ) => "View"
      | n
          when
            List.mem(
              n,
              [
                "initialize",
                "update_definition",
                "update_body",
                "update_pattern",
                "update_binding_clause",
                "delete_binding_clause",
                "delete_body",
                "insert_after",
                "insert_before",
              ],
            ) => "Edit"
      | n
          when
            List.mem(
              n,
              [
                "create_new_task",
                "set_active_task",
                "unset_active_task",
                "set_active_subtask",
                "unset_active_subtask",
                "mark_active_task_complete",
                "mark_active_task_incomplete",
                "mark_active_subtask_complete",
                "mark_active_subtask_incomplete",
                "mark_active_subtask_failed",
                "mark_active_task_failed",
              ],
            ) => "Workbench"
      | _ => "Other"
      };
    };
  };

  module Utils = {
    let init = (): Model.t => {
      let system_prompt = CompositionPrompt.self |> String.concat("\n");
      let dev_notes = {|Development mode active. Follow developer instructions precisely. Be concise. No first-person pronouns.|};
      {
        chat_system: ChatSystem.Utils.init(~system_prompt, ~dev_notes),
        // Todo: Will want to move prompting and api params to a global agent state
        prompting: {
          system_prompt,
          dev_notes,
          tools: CompositionUtils.Public.tools,
          disabled_tool_names: [],
        },
        active_timeline_node: None,
        awaiting_response: None,
        restore_editor_state: None,
        last_empty_retry_attempt: None,
        last_active_task_nudge_attempt: None,
        tools_view_expanded: [],
        compaction_in_progress: None,
        compaction_method_override: None,
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
        let action = Action.Structural(agent_editor_action);
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
              context_menu: editor.context_menu,
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
      | ReadAction(read_action) =>
        let z = editor.editor.state.zipper;
        let info_map = CompositionGo.Public.mk_statics(z);
        let syntax = CachedSyntax.init(z);
        switch (
          CompositionGo.Local.read_dispatch(
            ~action=read_action,
            ~z,
            ~info_map,
            ~syntax,
          )
        ) {
        | Ok(_content) =>
          /* Read actions don't modify the editor. The content is
             returned as the tool_result content in handle_tool_call. */
          Ok((agent, editor))
        | Error(err) =>
          switch (err) {
          | Action.Failure.Composition_action_failure(msg) =>
            Error(Failure.Info(msg))
          | _ => Error(Failure.Info("Failed to execute read action"))
          }
        };
      | LanguageServerAction(_) =>
        /* TODO: implement language server queries */
        Ok((agent, editor))
      | Initialize(code) =>
        /* Replace entire program content (select-all + paste).
           Only allowed when program has no let/type-alias bindings. */
        let z = editor.editor.state.zipper;
        let mk_statics = CompositionGo.Public.mk_statics;
        let info_map = mk_statics(z);
        switch (HighLevelNodeMap.build(z, info_map)) {
        | Some(_) =>
          Error(
            Failure.Info(
              "Once a program has let/type alias expressions, you can never use initialize on it ever again.",
            ),
          )
        | None =>
          let return = (error: Action.Failure.t, z: option(Zipper.t)) =>
            Result.of_option(~error, z);
          switch (
            CompositionGo.Local.PerformUtils.introduce(
              Select.all(z),
              code,
              return,
            )
          ) {
          | Error(_) => Error(Failure.Info("Failed to initialize program"))
          | Ok(new_z) =>
            let new_statics = mk_statics(new_z);
            let new_errors = ErrorPrint.all(new_statics);
            if (List.length(new_errors) > 0) {
              Error(
                Failure.Info(
                  "Not applying the action you requested as it would have the following static error(s): "
                  ++ String.concat(", ", new_errors),
                ),
              );
            } else {
              let new_z = Dump.to_zipper(new_z, ~root=Exp);
              let new_editor_model = Editor.Model.mk(new_z, ~root=Exp);
              let new_code_with_statics =
                CodeWithStatics.Model.mk(new_editor_model);
              Ok((agent, new_code_with_statics));
            };
          };
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
      | ProbeAction(probe_action) =>
        let z = editor.editor.state.zipper;
        let info_map = CompositionGo.Public.mk_statics(z);
        switch (HighLevelNodeMap.build(z, info_map)) {
        | None =>
          Error(
            Failure.Info(
              "No bindings in the program to probe. Add let/type bindings first.",
            ),
          )
        | Some(node_map) =>
          let syntax = CachedSyntax.init(z);
          let resolve_path = (path: string): option(Id.t) =>
            HighLevelNodeMap.Public.path_to_id_opt(node_map, path);

          let apply_probe_action =
              (z: Zipper.t, paths: list(string)): (Zipper.t, list(string)) => {
            List.fold_left(
              ((z, expanded), path) =>
                switch (resolve_path(path)) {
                | Some(id) =>
                  switch (probe_action) {
                  | PlaceProbe(_) =>
                    let z = ProbePerform.add_manual(~syntax, id, info_map, z);
                    (z, [path, ...expanded]);
                  | RemoveProbe(_) =>
                    let target_ids =
                      ProbePerform.target_subterm_ids(id, info_map);
                    let z = ProbePerform.rm_manual(target_ids, z);
                    (z, expanded);
                  | ToggleProbe(_) =>
                    let z =
                      ProbePerform.toggle_manual(~syntax, id, ~info_map, z);
                    let has_probe = ProbePerform.has_probe(id, z);
                    let expanded = has_probe ? [path, ...expanded] : expanded;
                    (z, expanded);
                  }
                | None => (z, expanded)
                },
              (z, []),
              paths,
            );
          };

          let paths =
            switch (probe_action) {
            | PlaceProbe(p)
            | RemoveProbe(p)
            | ToggleProbe(p) => p
            };
          let (new_z, paths_to_expand) = apply_probe_action(z, paths);
          let new_z = Dump.to_zipper(new_z, ~root=Exp);
          let new_editor_model = Editor.Model.mk(new_z, ~root=Exp);
          let new_cws =
            CodeWithStatics.Model.mk(
              ~dynamics=editor.dynamics,
              new_editor_model,
            );

          /* Auto-expand probed definitions so results are visible */
          if (List.length(paths_to_expand) > 0) {
            let expand_action = AgentContext.Update.Expand(paths_to_expand);
            let chat_system =
              ChatSystem.Update.update(
                ChatSystem.Update.Action.ChatAction(
                  Chat.Update.Action.AgentContextAction(expand_action),
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
                new_cws,
              ))
            | Error(_) => Ok((agent, new_cws))
            };
          } else {
            Ok((agent, new_cws));
          };
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
        | HandleCompactionLLMReply(OpenRouter.Reply.Model.t, Id.t)
        | HandleChatNamingResponse(string, Id.t)
        | ApiErrorResponse(Id.t, Message.Model.t)
        | RetryApiError(Id.t, int)
        | DoRetryApiSend(Id.t, int)
        | RetryEmptyResponse(Id.t, int)
        | LoadTimelineSegment(Segment.t, int)
        | RestoreOriginal
        | LoadSegmentIntoEditor(Segment.t)
        | SetActiveTimelineNode(option(int))
        | SetToolEnabled(string, bool)
        | ToggleToolsViewExpanded(string)
        | RequestForcedCompaction(Id.t);
    };

    let max_api_retries = 3;
    let is_retryable_api_error = (code: int): bool =>
      code == 429 || code == 500 || code == 502 || code == 503;

    let enabled_tools = (prompting: Model.prompting): list(API.Json.t) =>
      List.filter(
        (tool: API.Json.t) =>
          switch (ToolUtils.get_name(tool)) {
          | Some(name) => !List.mem(name, prompting.disabled_tool_names)
          | None => true
          },
        prompting.tools,
      );
    // Exponential backoff
    let backoff_ms = (attempt: int): float =>
      1000.0 *. 2.0 ** float(attempt);

    let chat_naming_model_id = "google/gemini-2.0-flash-lite-001";

    let request_chat_name =
        (
          ~api_key: string,
          ~user_message: string,
          ~schedule_action: Action.t => unit,
          ~chat_id: Id.t,
        )
        : unit => {
      let prompt = "Generate a short, concise chat title (3-6 words max) that captures the essence of what the user is asking or working on. Respond with ONLY the title text, nothing else. No quotes, no punctuation at the end, no explanation.";
      let payload =
        OpenRouter.Payload.Utils.mk_default(
          ~model_id=chat_naming_model_id,
          ~messages=[
            OpenRouter.Message.Utils.mk_system_msg(prompt),
            OpenRouter.Message.Utils.mk_user_msg(user_message),
          ],
          ~tools=[],
        );
      let handler = (response: option(API.Json.t)): unit => {
        switch (OpenRouter.Utils.handle_chat(response)) {
        | Some(OpenRouter.Model.Reply(reply)) =>
          let title = String.trim(reply.content);
          if (String.length(title) > 0 && String.length(title) < 80) {
            schedule_action(Action.HandleChatNamingResponse(title, chat_id));
          };
        | _ => ()
        };
      };
      OpenRouter.Utils.start_chat(~key=api_key, ~payload, ~handler);
    };

    let test_results_string =
        (test_results: option(Language.TestResults.t)): string => {
      switch (test_results) {
      | None => "No test results available (evaluator may still be running)."
      | Some(results) when results.total == 0 => "No tests in program."
      | Some(results) =>
        let summary = Language.TestResults.test_summary_str(results);
        let details =
          List.mapi(
            (i, status: Language.TestStatus.t) => {
              let status_str = Language.TestStatus.to_string(status);
              "Test " ++ string_of_int(i + 1) ++ ": " ++ status_str;
            },
            results.statuses,
          );
        summary ++ "\n" ++ String.concat("\n", details);
      };
    };

    /** Same [[context]] payload the main agent sees ([[mk_context_message]]), built from the
        live editor and [[agent_view]] — appended last to the compaction API so the summarizer
        has current program text, errors, tests, and workbench (without changing UI state). */
    let compaction_context_snapshot_message =
        (model: Model.t, cell_editor: CellEditor.Model.t, chat_id: Id.t)
        : Message.Model.t => {
      let curr_chat = ChatSystem.Utils.find_chat(chat_id, model.chat_system);
      let cws = cell_editor.editor;
      let agent_editor_view_string =
        CompositionView.Public.print(
          ~probe_map=cws.dynamics,
          cws.editor,
          curr_chat.agent_view,
        );
      let static_errors_info_string =
        ErrorPrint.all(
          CompositionGo.Public.mk_statics(cws.editor.state.zipper),
        )
        |> String.concat("\n");
      let test_results_info_string =
        test_results_string(
          EvalResult.Model.test_results(cell_editor.result),
        );
      Message.Utils.mk_context_message(
        agent_editor_view_string,
        static_errors_info_string,
        test_results_info_string,
        AgentWorkbench.Utils.MainUtils.active_task_to_pretty_string(
          curr_chat.agent_workbench,
        ),
      );
    };

    let compaction_summary_method_label = "Model-generated summary";

    let send_compaction_request =
        (
          ~api_key: string,
          ~llm_id: string,
          ~messages: list(OpenRouter.Message.Model.t),
          ~schedule_action: Action.t => unit,
          ~chat_id: Id.t,
        )
        : unit => {
      let handler = (response: option(API.Json.t)): unit => {
        switch (OpenRouter.Utils.handle_chat(response)) {
        | Some(OpenRouter.Model.Reply(reply)) =>
          schedule_action(Action.HandleCompactionLLMReply(reply, chat_id))
        | Some(OpenRouter.Model.Error({message, code})) =>
          let api_error_content =
            "Compaction failed (code "
            ++ string_of_int(code)
            ++ "): "
            ++ message;
          let api_error_message =
            Message.Utils.mk_api_failure_message(api_error_content);
          schedule_action(
            Action.ApiErrorResponse(chat_id, api_error_message),
          );
        | None =>
          schedule_action(
            Action.ApiErrorResponse(
              chat_id,
              Message.Utils.mk_api_failure_message(
                "Compaction failed: empty API response.",
              ),
            ),
          )
        };
      };
      let payload =
        OpenRouter.Payload.Utils.mk_default(
          ~model_id=llm_id,
          ~messages,
          ~tools=[],
        );
      OpenRouter.Utils.start_chat(~key=api_key, ~payload, ~handler);
    };

    /** Shared auto (token limit) and manual (/compact) compaction kickoff. */
    let maybe_start_compaction =
        (
          ~manual: bool,
          ~model: Model.t,
          ~chat_id: Id.t,
          ~settings: Settings.t,
          ~schedule_action: Action.t => unit,
          ~cell_editor: CellEditor.Model.t,
        )
        : Model.t =>
      if (Option.is_some(model.compaction_in_progress)) {
        if (manual) {
          let msg =
            Message.Utils.mk_api_failure_message(
              "Compaction is already in progress.",
            );
          let chat_system =
            ChatSystem.Update.update(
              ChatSystem.Update.Action.ChatAction(
                Chat.Update.Action.AppendMessage(msg),
                chat_id,
              ),
              model.chat_system,
            )
            |> ChatSystem.Update.get;
          {
            ...model,
            chat_system,
          };
        } else {
          model;
        };
      } else if (manual && Option.is_some(model.awaiting_response)) {
        let msg =
          Message.Utils.mk_api_failure_message(
            "Wait for the assistant to finish before compacting.",
          );
        let chat_system =
          ChatSystem.Update.update(
            ChatSystem.Update.Action.ChatAction(
              Chat.Update.Action.AppendMessage(msg),
              chat_id,
            ),
            model.chat_system,
          )
          |> ChatSystem.Update.get;
        {
          ...model,
          chat_system,
        };
      } else {
        let chat = ChatSystem.Utils.find_chat(chat_id, model.chat_system);
        let dialogue = Chat.Utils.dialogue_slice_for_compaction_summary(chat);
        if (dialogue == []) {
          if (manual) {
            let msg =
              Message.Utils.mk_api_failure_message("Nothing to compact yet.");
            let chat_system =
              ChatSystem.Update.update(
                ChatSystem.Update.Action.ChatAction(
                  Chat.Update.Action.AppendMessage(msg),
                  chat_id,
                ),
                model.chat_system,
              )
              |> ChatSystem.Update.get;
            {
              ...model,
              chat_system,
            };
          } else {
            model;
          };
        } else {
          let context_msg =
            compaction_context_snapshot_message(model, cell_editor, chat_id);
          /* Send the live program snapshot as a **user** message, not a second
             system message. Some providers return empty assistant text when the
             request ends with [system] after [assistant]. */
          let summary_api_msgs =
            [
              OpenRouter.Message.Utils.mk_system_msg(
                CompactionPrompt.mk_system_prompt(
                  ~agent_system_prompt=model.prompting.system_prompt,
                  ~dev_notes=model.prompting.dev_notes,
                ),
              ),
            ]
            @ List.filter_map(Message.Utils.api_message_of_message, dialogue)
            @ [
              OpenRouter.Message.Utils.mk_user_msg(
                "[Compaction: current Hazel program / workbench snapshot]\n\n"
                ++ context_msg.content,
              ),
            ];
          switch (
            settings.agent_globals.api_key,
            AgentGlobals.get_active_llm_id(settings.agent_globals),
          ) {
          | (Some(api_key), Some(llm_id)) =>
            send_compaction_request(
              ~api_key,
              ~llm_id,
              ~messages=summary_api_msgs,
              ~schedule_action,
              ~chat_id,
            );
            {
              ...model,
              compaction_in_progress: Some(chat_id),
              compaction_method_override:
                manual ? Some("Slash command (/compact)") : None,
            };
          | _ =>
            if (manual) {
              let msg =
                Message.Utils.mk_api_failure_message(
                  "API key or LLM not configured. Cannot compact.",
                );
              let chat_system =
                ChatSystem.Update.update(
                  ChatSystem.Update.Action.ChatAction(
                    Chat.Update.Action.AppendMessage(msg),
                    chat_id,
                  ),
                  model.chat_system,
                )
                |> ChatSystem.Update.get;
              {
                ...model,
                chat_system,
              };
            } else {
              model;
            }
          };
        };
      };

    let send_llm_request =
        (
          ~api_key: string,
          ~payload: OpenRouter.Payload.Model.t,
          ~schedule_action: Action.t => unit,
          ~chat_id: Id.t,
          ~retry_attempt: int,
        )
        : unit => {
      let handler = (response: option(API.Json.t)): unit => {
        switch (OpenRouter.Utils.handle_chat(response)) {
        | Some(OpenRouter.Model.Reply(reply)) =>
          schedule_action(Action.HandleLLMResponse(reply, chat_id))
        | Some(OpenRouter.Model.Error({message, code})) =>
          if (is_retryable_api_error(code) && retry_attempt < max_api_retries) {
            schedule_action(Action.RetryApiError(chat_id, retry_attempt));
          } else {
            let api_error_content =
              "Code: " ++ string_of_int(code) ++ "\\Error: " ++ message;
            let api_error_message =
              Message.Utils.mk_api_failure_message(api_error_content);
            schedule_action(
              Action.ApiErrorResponse(chat_id, api_error_message),
            );
          }
        | None => ()
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
      switch (model.compaction_in_progress) {
      | Some(id) when id == chat_id => Ok(model)
      | _ =>
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
          send_llm_request(
            ~api_key,
            ~payload=
              OpenRouter.Payload.Utils.mk_default(
                ~model_id=llm_id,
                ~messages=
                  Chat.Utils.api_messages_of_messages(
                    Chat.Utils.messages_for_openrouter(
                      ChatSystem.Utils.find_chat(chat_id, chat_system),
                    ),
                  ),
                ~tools=enabled_tools(model.prompting),
              ),
            ~schedule_action,
            ~chat_id,
            ~retry_attempt=0,
          );
          let current_chat = ChatSystem.Utils.find_chat(chat_id, chat_system);
          if (current_chat.title == "New Chat" && new_message.role == User) {
            request_chat_name(
              ~api_key,
              ~user_message=new_message.content,
              ~schedule_action,
              ~chat_id,
            );
          };
          Ok({
            ...model,
            chat_system,
            awaiting_response: Some(chat_id),
          });
        };
      };
    };

    let format_cursor_context =
        (z: Zipper.t, info_map: Language.Statics.Map.t): string => {
      switch (Indicated.ci_of(z, info_map)) {
      | Some(InfoExp({ana, ctx, ty, _})) =>
        let expected = ErrorPrint.Print.typ(ana);
        let synthesized = ErrorPrint.Print.typ(ty);
        let ctx_entries =
          Language.Ctx.filter_shadowed(ctx).entries
          |> List.filter_map((entry: Language.Ctx.entry) =>
               switch (entry) {
               | VarEntry({name, typ, _}) =>
                 Some(name ++ " : " ++ ErrorPrint.Print.typ(typ))
               | _ => None
               }
             );
        let vars_str =
          switch (ctx_entries) {
          | [] => "none"
          | entries =>
            let shown = ListUtil.take(10, entries);
            let suffix =
              List.length(entries) > 10
                ? " (+"
                  ++ string_of_int(List.length(entries) - 10)
                  ++ " more)"
                : "";
            String.concat(", ", shown) ++ suffix;
          };
        "Expected type: "
        ++ expected
        ++ "\nSynthesized type: "
        ++ synthesized
        ++ "\nVariables in scope: "
        ++ vars_str;
      | Some(InfoPat({ana, ctx, _})) =>
        let expected = ErrorPrint.Print.typ(ana);
        let ctx_entries =
          Language.Ctx.filter_shadowed(ctx).entries
          |> List.filter_map((entry: Language.Ctx.entry) =>
               switch (entry) {
               | ConstructorEntry({name, typ, _}) =>
                 Some(name ++ " : " ++ ErrorPrint.Print.typ(typ))
               | _ => None
               }
             );
        let ctors_str =
          switch (ctx_entries) {
          | [] => "none"
          | entries => String.concat(", ", ListUtil.take(10, entries))
          };
        "Pattern expected type: "
        ++ expected
        ++ "\nConstructors in scope: "
        ++ ctors_str;
      | _ => ""
      };
    };

    let update_context =
        (
          ~test_results: option(Language.TestResults.t)=?,
          model: Model.t,
          editor: CodeWithStatics.Model.t,
          chat_id: Id.t,
        )
        : Model.t => {
      let curr_chat = ChatSystem.Utils.find_chat(chat_id, model.chat_system);
      let agent_editor_view_string =
        CompositionView.Public.print(
          ~probe_map=editor.dynamics,
          editor.editor,
          curr_chat.agent_view,
        );
      let info_map =
        CompositionGo.Public.mk_statics(editor.editor.state.zipper);
      let static_errors_info_string =
        ErrorPrint.all(info_map) |> String.concat("\n");
      let test_results_info_string = test_results_string(test_results);
      let cursor_context_string =
        format_cursor_context(editor.editor.state.zipper, info_map);
      let chat_system =
        ChatSystem.Update.update(
          ChatSystem.Update.Action.ChatAction(
            Chat.Update.Action.UpdateContext({
              agent_editor_view: agent_editor_view_string,
              static_errors: static_errors_info_string,
              test_results: test_results_info_string,
              cursor_context: cursor_context_string,
            }),
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
          ~tool_result: AgentToolResult.tool_result,
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
        : option(AgentToolResult.diff) => {
      let* edit_action =
        switch (action) {
        | EditorAction(edit_action) => Some(edit_action)
        | _ => None
        };
      let* diff =
        CompositionGo.Local.get_diff(
          old_editor.state.zipper,
          new_editor.state.zipper,
          edit_action,
          CompositionGo.Public.mk_statics,
          old_editor.syntax,
        );
      Some(
        AgentToolResult.{
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
      | EditorAction(_)
      | Initialize(_)
      | ProbeAction(_) =>
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
          try(
            ToolCallHandler.update(
              ~settings,
              action,
              model,
              cell_editor.editor,
              chat_id,
            )
          ) {
          | Failure(msg) => Error(Failure.Info(msg))
          | exn =>
            /* Catch all exceptions (e.g. Path not found) — report to agent, do not break state */
            Error(Failure.Info(Printexc.to_string(exn)))
          }
        ) {
        | Ok((model, editor)) =>
          let model = update_context(model, editor, chat_id);
          let success_message =
            switch (action) {
            | ReadAction(read_action) =>
              /* For read actions, compute the result content */
              let z = cell_editor.editor.editor.state.zipper;
              switch (
                CompositionGo.Public.read_dispatch(~action=read_action, ~z)
              ) {
              | Ok(content) => content
              | Error(_) =>
                "The " ++ tool_call.name ++ " tool call encountered an error."
              };
            | _ =>
              let base_msg =
                "The "
                ++ tool_call.name
                ++ " tool call was successful and has been applied to the model.";
              switch (CompositionGo.Public.last_warning^) {
              | Some(warning) =>
                CompositionGo.Public.last_warning := None;
                base_msg ++ "\n" ++ warning;
              | None => base_msg
              };
            };
          let (before_segment, after_segment) =
            mk_segment_snapshots(
              ~old_editor=cell_editor.editor.editor,
              ~new_editor=editor.editor,
              action,
            );
          let diff_result =
            try(
              Ok(
                mk_diff(
                  ~old_editor=cell_editor.editor.editor,
                  ~new_editor=editor.editor,
                  action,
                ),
              )
            ) {
            | exn => Error(exn)
            };
          switch (diff_result) {
          | Error(exn) =>
            /* mk_diff can raise (e.g. path_to_id); report to agent, keep state */
            let msg = Printexc.to_string(exn);
            let tool_result: AgentToolResult.tool_result = {
              tool_call,
              success: false,
              expanded: false,
              diff: None,
              before_segment:
                Some(
                  Select.all(cell_editor.editor.editor.state.zipper).selection.
                    content,
                ),
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
          | Ok(diff) =>
            let tool_result: AgentToolResult.tool_result = {
              tool_call,
              success: true,
              expanded: false,
              diff,
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
          };
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
            let tool_result: AgentToolResult.tool_result = {
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
        let tool_result: AgentToolResult.tool_result = {
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
      let tool_call = ListUtil.hd_opt(reply.tool_calls);
      let is_empty = String.trim(reply.content) == "";
      let max_empty_retries = 2;

      // Empty response with no tool calls: retry with failure context (up to max_empty_retries)
      if (tool_call == None && is_empty) {
        let current_retry =
          Option.value(~default=-1, model.last_empty_retry_attempt);
        let next_retry = current_retry + 1;
        if (next_retry < max_empty_retries) {
          schedule_action(Action.RetryEmptyResponse(chat_id, next_retry));
          (
            {
              ...model,
              last_empty_retry_attempt: Some(next_retry),
            },
            cell_editor |> Updated.return_quiet,
          );
        } else {
          // Exhausted retries: show fallback message
          let fallback_content =
            "(The assistant returned an empty response after retries. "
            ++ "This often happens when the agent left a task or subtask active — the agent must close it (mark complete or failed) before responding. "
            ++ "Never end the tool loop with an active task/subtask. You can try rephrasing your message.)";
          let new_message =
            Message.Utils.mk_agent_message(fallback_content, reply.usage);
          let chat_system =
            ChatSystem.Update.update(
              ChatSystem.Update.Action.ChatAction(
                Chat.Update.Action.AppendMessage(new_message),
                chat_id,
              ),
              model.chat_system,
            )
            |> ChatSystem.Update.get;
          (
            {
              ...model,
              chat_system,
              awaiting_response: None,
              last_empty_retry_attempt: None,
            },
            cell_editor |> Updated.return_quiet,
          );
        };
      } else {
        let content = reply.content;
        let new_message =
          Message.Utils.mk_agent_message(
            ~tool_calls=reply.tool_calls,
            content,
            reply.usage,
          );
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
          last_empty_retry_attempt: None,
          last_active_task_nudge_attempt: None,
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
        | None =>
          let max_task_nudges = 1;
          let current_chat =
            ChatSystem.Utils.find_chat(chat_id, model.chat_system);
          let workbench = current_chat.agent_workbench;
          let has_incomplete_active_subtask =
            switch (AgentWorkbench.Utils.MainUtils.active_task(workbench)) {
            | Some(task) =>
              switch (task.completion_info) {
              | Some(_) => false
              | None =>
                switch (task.active_subtask) {
                | Some(st) =>
                  switch (
                    AgentWorkbench.Utils.SubtaskUtils.find_subtask(task, st)
                  ) {
                  | Some(sub) =>
                    !AgentWorkbench.Utils.SubtaskUtils.is_completed(sub)
                  | None => false
                  }
                | None => false
                }
              }
            | None => false
            };
          let nudge_count =
            Option.value(~default=0, model.last_active_task_nudge_attempt);
          if (has_incomplete_active_subtask && nudge_count < max_task_nudges) {
            let nudge_content = "[System] You still have an active task/subtask in progress. Please do one of the following:\n1. If the subtask is complete, call mark_active_subtask_complete with a summary.\n2. If you want to continue working on it, make your next tool call.\n3. If the subtask is unattainable or you are stuck, call mark_active_subtask_failed with a reason.";
            let nudge_message =
              Message.Utils.mk_retry_note_message(
                ~content=nudge_content,
                ~sent_to_api=true,
              );
            schedule_action(Action.SendMessage(nudge_message, chat_id));
            (
              {
                ...model,
                last_active_task_nudge_attempt: Some(nudge_count + 1),
              },
              cell_editor |> Updated.return_quiet,
            );
          } else {
            let model_idle = {
              ...model,
              awaiting_response: None,
              last_active_task_nudge_attempt: None,
            };
            let limit_opt =
              AgentGlobals.context_meter_limit_for_active(
                settings.agent_globals,
              );
            let should_compact =
              switch (reply.usage, limit_opt) {
              | (Some(usage), Some(limit)) => usage.prompt_tokens >= limit
              | _ => false
              };
            let may_start_compaction =
              Option.is_none(model_idle.compaction_in_progress);
            if (should_compact && may_start_compaction) {
              let model' =
                maybe_start_compaction(
                  ~manual=false,
                  ~model=model_idle,
                  ~chat_id,
                  ~settings,
                  ~schedule_action,
                  ~cell_editor,
                );
              (model', cell_editor |> Updated.return_quiet);
            } else {
              (model_idle, cell_editor |> Updated.return_quiet);
            };
          };
        };
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
        let model =
          update_context(
            ~test_results=?EvalResult.Model.test_results(editor.result),
            model,
            editor.editor,
            chat_id,
          );
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
      | HandleCompactionLLMReply(reply, chat_id) =>
        let method_label =
          Option.value(
            ~default=compaction_summary_method_label,
            model.compaction_method_override,
          );
        let model_cleared = {
          ...model,
          compaction_in_progress: None,
          compaction_method_override: None,
        };
        let content = String.trim(reply.content);
        if (content == "" && reply.tool_calls != []) {
          let err =
            Message.Utils.mk_api_failure_message(
              "Compaction returned tool calls instead of a text summary. Try another model, or one that does not emit tools on compaction.",
            );
          let chat_system =
            ChatSystem.Update.update(
              ChatSystem.Update.Action.ChatAction(
                Chat.Update.Action.AppendMessage(err),
                chat_id,
              ),
              model_cleared.chat_system,
            )
            |> ChatSystem.Update.get;
          (
            {
              ...model_cleared,
              chat_system,
            },
            editor |> Updated.return,
          );
        } else if (content == "") {
          let err =
            Message.Utils.mk_api_failure_message(
              "Compaction returned an empty summary.",
            );
          let chat_system =
            ChatSystem.Update.update(
              ChatSystem.Update.Action.ChatAction(
                Chat.Update.Action.AppendMessage(err),
                chat_id,
              ),
              model_cleared.chat_system,
            )
            |> ChatSystem.Update.get;
          (
            {
              ...model_cleared,
              chat_system,
            },
            editor |> Updated.return,
          );
        } else {
          let summary =
            Message.Utils.mk_compaction_summary(
              ~method=method_label,
              content,
            );
          let chat_system =
            ChatSystem.Update.update(
              ChatSystem.Update.Action.ChatAction(
                Chat.Update.Action.AppendMessage(summary),
                chat_id,
              ),
              model_cleared.chat_system,
            )
            |> ChatSystem.Update.get;
          (
            {
              ...model_cleared,
              chat_system,
            },
            editor |> Updated.return,
          );
        };
      | RequestForcedCompaction(chat_id) =>
        let model' =
          maybe_start_compaction(
            ~manual=true,
            ~model,
            ~chat_id,
            ~settings,
            ~schedule_action,
            ~cell_editor=editor,
          );
        (model', editor |> Updated.return);
      | HandleChatNamingResponse(title, chat_id) =>
        let chat_system =
          ChatSystem.Update.update(
            ChatSystem.Update.Action.ChatAction(
              Chat.Update.Action.SetTitle(title),
              chat_id,
            ),
            model.chat_system,
          )
          |> ChatSystem.Update.get;
        (
          {
            ...model,
            chat_system,
          },
          editor |> Updated.return,
        );
      | ApiErrorResponse(chat_id, api_error_message) =>
        let chat_system =
          ChatSystem.Update.update(
            ChatSystem.Update.Action.ChatAction(
              Chat.Update.Action.AppendMessage(api_error_message),
              chat_id,
            ),
            model.chat_system,
          )
          |> ChatSystem.Update.get;
        (
          {
            ...model,
            chat_system,
            awaiting_response: None,
            compaction_in_progress:
              switch (model.compaction_in_progress) {
              | Some(id) when id == chat_id => None
              | c => c
              },
            compaction_method_override:
              switch (model.compaction_in_progress) {
              | Some(id) when id == chat_id => None
              | _ => model.compaction_method_override
              },
          },
          editor |> Updated.return,
        );
      | RetryApiError(chat_id, attempt) =>
        let delay_s = int_of_float(backoff_ms(attempt) /. 1000.0);
        let retry_note =
          Message.Utils.mk_retry_note_message(
            ~content=
              "[API retry "
              ++ string_of_int(attempt + 2)
              ++ "/"
              ++ string_of_int(max_api_retries + 1)
              ++ "] Server/rate limit error. Retrying in "
              ++ string_of_int(delay_s)
              ++ "s...",
            ~sent_to_api=false,
          );
        let chat_system =
          ChatSystem.Update.update(
            ChatSystem.Update.Action.ChatAction(
              Chat.Update.Action.AppendMessage(retry_note),
              chat_id,
            ),
            model.chat_system,
          )
          |> ChatSystem.Update.get;
        let delay_ms = backoff_ms(attempt);
        JsUtil.delay(delay_ms, () =>
          schedule_action(Action.DoRetryApiSend(chat_id, attempt))
        );
        (
          {
            ...model,
            chat_system,
          },
          editor |> Updated.return,
        );
      | DoRetryApiSend(chat_id, attempt) =>
        let model =
          update_context(
            ~test_results=?EvalResult.Model.test_results(editor.result),
            model,
            editor.editor,
            chat_id,
          );
        let chat_system = model.chat_system;
        switch (
          settings.agent_globals.api_key,
          AgentGlobals.get_active_llm_id(settings.agent_globals),
        ) {
        | (Some(api_key), Some(llm_id)) =>
          send_llm_request(
            ~api_key,
            ~payload=
              OpenRouter.Payload.Utils.mk_default(
                ~model_id=llm_id,
                ~messages=
                  Chat.Utils.api_messages_of_messages(
                    Chat.Utils.messages_for_openrouter(
                      ChatSystem.Utils.find_chat(chat_id, chat_system),
                    ),
                  ),
                ~tools=enabled_tools(model.prompting),
              ),
            ~schedule_action,
            ~chat_id,
            ~retry_attempt=attempt + 1,
          );
          (model, editor |> Updated.return);
        | _ =>
          let api_failure_message =
            Message.Utils.mk_api_failure_message(
              "API key or LLM not configured. Cannot retry.",
            );
          let chat_system =
            ChatSystem.Update.update(
              ChatSystem.Update.Action.ChatAction(
                Chat.Update.Action.AppendMessage(api_failure_message),
                chat_id,
              ),
              model.chat_system,
            )
            |> ChatSystem.Update.get;
          (
            {
              ...model,
              chat_system,
              awaiting_response: None,
            },
            editor |> Updated.return,
          );
        };
      | RetryEmptyResponse(chat_id, attempt) =>
        let retry_msg =
          "[Retry "
          ++ string_of_int(attempt + 1)
          ++ "/2] Your previous response was empty or invalid. "
          ++ "If a task or subtask is active, close it first: call mark_active_task_complete, mark_active_task_failed, mark_active_subtask_complete, or mark_active_subtask_failed. "
          ++ "Never end your turn with an active task/subtask. Then respond with a message for the user—either directly answering their question or summarizing what you did.";
        let retry_message =
          Message.Utils.mk_retry_note_message(
            ~content=retry_msg,
            ~sent_to_api=true,
          );
        let chat_system =
          ChatSystem.Update.update(
            ChatSystem.Update.Action.ChatAction(
              Chat.Update.Action.AppendMessage(retry_message),
              chat_id,
            ),
            model.chat_system,
          )
          |> ChatSystem.Update.get;
        let model = {
          ...model,
          chat_system,
        };
        switch (
          settings.agent_globals.api_key,
          AgentGlobals.get_active_llm_id(settings.agent_globals),
        ) {
        | (Some(api_key), Some(llm_id)) =>
          send_llm_request(
            ~api_key,
            ~payload=
              OpenRouter.Payload.Utils.mk_default(
                ~model_id=llm_id,
                ~messages=
                  Chat.Utils.api_messages_of_messages(
                    Chat.Utils.messages_for_openrouter(
                      ChatSystem.Utils.find_chat(chat_id, model.chat_system),
                    ),
                  ),
                ~tools=enabled_tools(model.prompting),
              ),
            ~schedule_action,
            ~chat_id,
            ~retry_attempt=0,
          );
          (model, editor |> Updated.return);
        | _ =>
          let api_failure_message =
            Message.Utils.mk_api_failure_message(
              "API key or LLM not configured. Cannot retry.",
            );
          let chat_system =
            ChatSystem.Update.update(
              ChatSystem.Update.Action.ChatAction(
                Chat.Update.Action.AppendMessage(api_failure_message),
                chat_id,
              ),
              model.chat_system,
            )
            |> ChatSystem.Update.get;
          (
            {
              ...model,
              chat_system,
              awaiting_response: None,
              last_empty_retry_attempt: None,
            },
            editor |> Updated.return,
          );
        };
      | LoadTimelineSegment(segment, node_index) =>
        // On first click into history: save current editor state for Restore Original
        let restore_editor_state =
          switch (model.restore_editor_state) {
          | None =>
            Some(
              Select.all(editor.editor.editor.state.zipper).selection.content,
            )
          | Some(_) => model.restore_editor_state
          };
        let new_zipper = Zipper.unzip(~direction=Right, segment);
        let new_editor_model = Editor.Model.mk(new_zipper, ~root=Exp);
        let new_code_with_statics =
          CodeWithStatics.Model.mk(new_editor_model);
        (
          {
            ...model,
            restore_editor_state,
            active_timeline_node: Some(node_index),
          },
          {
            ...editor,
            editor: new_code_with_statics,
          }
          |> Updated.return,
        );
      | RestoreOriginal =>
        switch (model.restore_editor_state) {
        | Some(saved_segment) =>
          let new_zipper = Zipper.unzip(~direction=Right, saved_segment);
          let new_editor_model = Editor.Model.mk(new_zipper, ~root=Exp);
          let new_code_with_statics =
            CodeWithStatics.Model.mk(new_editor_model);
          (
            {
              ...model,
              restore_editor_state: None,
              active_timeline_node: None,
            },
            {
              ...editor,
              editor: new_code_with_statics,
            }
            |> Updated.return,
          );
        | None => (model, editor |> Updated.return)
        }
      | LoadSegmentIntoEditor(segment) =>
        // Replace editor with segment by converting to zipper
        let new_zipper = Zipper.unzip(~direction=Right, segment);
        let new_editor_model = Editor.Model.mk(new_zipper, ~root=Exp);
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
      | SetToolEnabled(name, enabled) =>
        let disabled_tool_names =
          if (enabled) {
            List.filter(
              (n: string) => n != name,
              model.prompting.disabled_tool_names,
            );
          } else {
            List.mem(name, model.prompting.disabled_tool_names)
              ? model.prompting.disabled_tool_names
              : [name, ...model.prompting.disabled_tool_names];
          };
        (
          {
            ...model,
            prompting: {
              ...model.prompting,
              disabled_tool_names,
            },
          },
          editor |> Updated.return,
        );
      | ToggleToolsViewExpanded(name) =>
        let tools_view_expanded =
          List.mem(name, model.tools_view_expanded)
            ? List.filter(
                (n: string) => n != name,
                model.tools_view_expanded,
              )
            : [name, ...model.tools_view_expanded];
        (
          {
            ...model,
            tools_view_expanded,
          },
          editor |> Updated.return,
        );
      };
    };
  };
};
