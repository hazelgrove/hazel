open Util;
open Util.API;
open Haz3lcore;
open OptUtil.Syntax;
open Ppx_yojson_conv_lib.Yojson_conv;

/* Agent chat: UI roles vs API payload ([[Message.Model.api_message]])

   Policy for what we send to the LLM (see also [[CompositionPrompt.message_channels]]):

   - **System (prompt, context, compaction summary)** — Use [[OpenRouter.Message.Utils.mk_system_msg]].
     Hazel's "developer notes" use [[mk_developer_msg]], which serializes as **system** in
     [[OpenRouter.Message.Utils.json_of_message]] (there is no separate developer role on the wire).

   - **Context snapshot** ([[Message.Utils.mk_context_message]]) — One structured message: program
     view, static/type diagnostics inside `<staticErrorsInfo>`, tests, workbench. Refresh in place
     via [[Chat.Utils.update_context]]; do **not** emit separate "linter" system messages per error.
     Future language-service feedback should use extra tagged blocks here, same cadence.

   - **Tool results** — [[OpenRouter.Message.Utils.mk_tool_msg]]; short, actionable success/failure text.

   - **Mandatory protocol nudges** (empty reply, open subtask) — [[Message.Utils.mk_retry_note_message]]
     with [deliver_as_user_on_api]: UI stays [[System(RetryNote)]], API uses **user** plus an explicit
     prefix so the model attends without confusing the human's messages.

   - **UI-only** — e.g. [[mk_api_failure_message]] with [api_message: None]: never sent to the API.

   OpenRouter wire roles: system, user, assistant, tool (see [[OpenRouter.Message.Model.role]]).
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
    /** Typed payloads for the inline output of chat slash commands (/cost, /credits, /usage, /help).
        Carries the raw values so the view layer owns formatting. */
    [@deriving (show({with_path: false}), sexp, yojson)]
    type cost_output = {
      cost_model: string, // "" if no model selected
      cost_input_tokens: int,
      cost_output_tokens: int,
      cost_estimated_usd: option(float) // None when no model selected
    };

    [@deriving (show({with_path: false}), sexp, yojson)]
    type credits_output = {
      credits_used: float,
      credits_total: float,
    };

    [@deriving (show({with_path: false}), sexp, yojson)]
    type usage_output = {
      usage_label: option(string),
      usage_is_free_tier: bool,
      usage_total: float,
      usage_daily: option(float),
      usage_weekly: option(float),
      usage_monthly: option(float),
      usage_limit: option(float),
      usage_remaining: option(float),
    };

    [@deriving (show({with_path: false}), sexp, yojson)]
    type help_entry = {
      help_name: string,
      help_description: string,
    };

    [@deriving (show({with_path: false}), sexp, yojson)]
    type help_output = {help_entries: list(help_entry)};

    [@deriving (show({with_path: false}), sexp, yojson)]
    type slash_command_payload =
      | CostOutput(cost_output)
      | CreditsOutput(credits_output)
      | UsageOutput(usage_output)
      | KeyOutput(string) // current OpenRouter API key; "" means none set
      | HelpOutput(help_output)
      | Notice(string) // plain-text confirmation (e.g., toggle ack)
      | SlashError(string);

    [@deriving (show({with_path: false}), sexp, yojson)]
    type system_kind =
      | ApiFailure
      | DeveloperNotes // Only one should exist
      | Prompt // Only one should exist
      | Context
      | RetryNote // Research transparency: empty/API retries
      | ResponseCancelled // User stopped in-flight LLM; UI-only, not sent to API
      | SlashCommandOutput(slash_command_payload) // Inline result of a chat slash command. UI-only.
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
      [@yojson.default None] [@sexp.default None]
      reasoning: option(string),
      [@yojson.default None] [@sexp.default None]
      reasoning_duration_ms: option(int),
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
        reasoning: None,
        reasoning_duration_ms: None,
      };
    };
    /** Shown when the user stops the agent or compaction and the HTTP reply arrives later. */
    let mk_response_cancelled_message = (~content: string): Model.t => {
      let sanitized_content = String.trim(content);
      {
        id: Id.mk(),
        content: sanitized_content,
        timestamp: JsUtil.timestamp(),
        role: System(ResponseCancelled),
        api_message: None,
        children: [],
        current_child: None,
        reasoning: None,
        reasoning_duration_ms: None,
      };
    };

    /** UI-only inline output from a chat slash command (/cost, /credits, /usage, /help).
        `content` is a brief plain-text fallback for archival/copy; the real rendering
        reads the typed payload off the role. Never sent to the API. */
    let mk_slash_command_output_message =
        (~payload: Model.slash_command_payload, ~content: string): Model.t => {
      let sanitized_content = String.trim(content);
      {
        id: Id.mk(),
        content: sanitized_content,
        timestamp: JsUtil.timestamp(),
        role: System(SlashCommandOutput(payload)),
        api_message: None,
        children: [],
        current_child: None,
        reasoning: None,
        reasoning_duration_ms: None,
      };
    };

    let mk_retry_note_message =
        (~content: string, ~sent_to_api: bool, ~deliver_as_user_on_api: bool)
        : Model.t => {
      let sanitized_content = String.trim(content);
      let api_message =
        switch (sent_to_api, deliver_as_user_on_api) {
        | (false, _) => None
        | (true, true) =>
          Some(
            OpenRouter.Message.Utils.mk_user_msg(
              "[Required follow-up — injected by Hazel, not the human user]\n"
              ++ sanitized_content,
            ),
          )
        | (true, false) =>
          Some(OpenRouter.Message.Utils.mk_developer_msg(sanitized_content))
        };
      {
        id: Id.mk(),
        content: sanitized_content,
        timestamp: JsUtil.timestamp(),
        role: System(RetryNote),
        api_message,
        children: [],
        current_child: None,
        reasoning: None,
        reasoning_duration_ms: None,
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
        reasoning: None,
        reasoning_duration_ms: None,
      };
    };
    let mk_agent_message =
        (
          ~tool_calls: list(OpenRouter.Reply.Model.tool_call)=[],
          ~reasoning: option(string)=None,
          ~reasoning_duration_ms: option(int)=None,
          content: string,
          usage: option(OpenRouter.Reply.Model.usage),
        )
        : Model.t => {
      let sanitized_content = String.trim(content);
      let reasoning =
        switch (reasoning) {
        | Some(s) when String.trim(s) != "" => Some(String.trim(s))
        | _ => None
        };
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
        reasoning,
        reasoning_duration_ms,
      };
    };

    let mk_tool_result_message =
        (tool_result: AgentToolResult.tool_result): Model.t => {
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
        reasoning: None,
        reasoning_duration_ms: None,
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
        reasoning: None,
        reasoning_duration_ms: None,
      };
    };
    /** Exact [content] string embedded in the context system message on the API (tags + footer).
        Program text is [[CompositionView.Public.print]] with probes when present (see [[CompositionView]]). */
    let context_snapshot_body_for_llm =
        (
          ~session_mode: AgentGlobals.Model.session_mode,
          agent_editor_content: string,
          static_errors_content: string,
          test_results_content: string,
          workbench_content: string,
        )
        : string => {
      let sanitized_agent_editor_content = String.trim(agent_editor_content);
      let sanitized_static_errors_content =
        String.trim(static_errors_content);
      let sanitized_test_results_content = String.trim(test_results_content);
      let sanitized_workbench_content = String.trim(workbench_content);

      let session_mode_label = AgentGlobals.session_mode_label(session_mode);
      let session_mode_block =
        "\n<sessionMode>" ++ session_mode_label ++ "</sessionMode>\n";

      let agent_editor_content_prefix = "\n<agentEditorView>\n```";
      let agent_editor_content_suffix = "```\n</agentEditorView>\n";
      let agent_editor_block =
        agent_editor_content_prefix
        ++ sanitized_agent_editor_content
        ++ agent_editor_content_suffix;

      let static_errors_content_prefix = "\n<staticErrorsInfo>\n";
      let static_errors_content_suffix = "\n</staticErrorsInfo>\n";
      let static_errors_block =
        static_errors_content_prefix
        ++ sanitized_static_errors_content
        ++ static_errors_content_suffix;

      let test_results_content_prefix = "\n<testResultsInfo>\n";
      let test_results_content_suffix = "\n</testResultsInfo>\n";
      let test_results_block =
        test_results_content_prefix
        ++ sanitized_test_results_content
        ++ test_results_content_suffix;

      let workbench_content_prefix = "\n<workbenchTaskInfo>\n";
      let workbench_content_suffix = "\n</workbenchTaskInfo>\n";
      let workbench_block =
        workbench_content_prefix
        ++ sanitized_workbench_content
        ++ workbench_content_suffix;

      let context_prefix = "\n<context>\n";
      let context_suffix = "\n</context>\n";
      let context_content =
        context_prefix
        ++ session_mode_block
        ++ agent_editor_block
        ++ static_errors_block
        ++ test_results_block
        ++ workbench_block
        ++ context_suffix;
      context_content
      ++ "\n[CONTEXT UPDATE — Do not respond to this. It is an automated snapshot of the current program state. Continue with your current task without acknowledging this message.]";
    };

    let mk_context_system_message = (content: string): Model.t => {
      {
        id: Id.mk(),
        content,
        timestamp: JsUtil.timestamp(),
        role: System(Context),
        api_message: Some(OpenRouter.Message.Utils.mk_system_msg(content)),
        children: [],
        current_child: None,
        reasoning: None,
        reasoning_duration_ms: None,
      };
    };

    /** Rolling environment snapshot for the model: editor + **static errors** + tests + workbench.
        Diagnostics belong in [<staticErrorsInfo>], not in separate system messages. See file header. */
    let mk_context_message =
        (
          ~session_mode: AgentGlobals.Model.session_mode,
          agent_editor_content: string,
          static_errors_content: string,
          test_results_content: string,
          workbench_content: string,
        )
        : Model.t => {
      mk_context_system_message(
        context_snapshot_body_for_llm(
          ~session_mode,
          agent_editor_content,
          static_errors_content,
          test_results_content,
          workbench_content,
        ),
      );
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
        reasoning: None,
        reasoning_duration_ms: None,
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
        reasoning: None,
        reasoning_duration_ms: None,
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
            | ResponseCancelled => `String("response_cancelled")
            | SlashCommandOutput(payload) =>
              let kind =
                switch (payload) {
                | CostOutput(_) => "cost"
                | CreditsOutput(_) => "credits"
                | UsageOutput(_) => "usage"
                | KeyOutput(_) => "key"
                | HelpOutput(_) => "help"
                | Notice(_) => "notice"
                | SlashError(_) => "error"
                };
              `Assoc([("slash_command_output", `String(kind))]);
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
              ("skipped", `Bool(tool_result.skipped)),
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
      [@yojson.default []]
      pending_send_queue: list(string),
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
          ~session_mode: AgentGlobals.Model.session_mode,
          agent_editor_view: string,
          static_errors_info: string,
          test_results_info: string,
          chat: Model.t,
        )
        : Model.t => {
      let workbench =
        Message.Utils.mk_context_message(
          ~session_mode,
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

    /** Transcript segment to send to the compaction LLM: after dev notes; if a prior compaction
        summary exists on this branch, **include** that message so the model can merge it with newer
        turns (slice starts at that summary, not after it). */
    let dialogue_slice_for_compaction_summary =
        (chat: Model.t): list(Message.Model.t) => {
      let linear = linearize(chat);
      let start =
        switch (last_compaction_index(0, None, linear)) {
        | None => 2
        | Some(i) => i
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

    /** Value for the context meter: **only** the provider-reported [[prompt_tokens]] from the last
        assistant message in the transcript when that message is still “current” for the bar:
        - No compaction on the branch → last agent message’s usage (if any).
        - Compaction present → that usage **only** if the last agent message is **after** the latest
          compaction summary (same request shape the API already counted). Otherwise [None] so the UI
        shows “—” (no client-side estimates). */
    let context_meter_prompt_tokens = (chat: Model.t): option(int) => {
      let messages = get(chat);
      let (_, last_agent_idx, last_agent_tokens, last_compaction_idx) =
        List.fold_left(
          (
            (i, last_agent_idx, last_agent_tokens, last_compaction_idx),
            msg: Message.Model.t,
          ) => {
            let last_agent_idx' =
              switch (msg.role) {
              | Message.Model.Agent(Some(_)) => Some(i)
              | _ => last_agent_idx
              };
            let last_agent_tokens' =
              switch (msg.role) {
              | Message.Model.Agent(Some(u)) => Some(u.prompt_tokens)
              | _ => last_agent_tokens
              };
            let last_compaction_idx' =
              switch (msg.role) {
              | Message.Model.System(Message.Model.CompactionSummary(_)) =>
                Some(i)
              | _ => last_compaction_idx
              };
            (
              i + 1,
              last_agent_idx',
              last_agent_tokens',
              last_compaction_idx',
            );
          },
          (0, None, None, None),
          messages,
        );
      switch (last_compaction_idx, last_agent_idx, last_agent_tokens) {
      | (Some(ci), Some(ai), Some(tok)) when ai > ci => Some(tok)
      | (Some(_), _, _) => None
      | (None, _, Some(tok)) => Some(tok)
      | (None, _, None) => None
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
        pending_send_queue: [],
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
        | UpdateContext(
            AgentGlobals.Model.session_mode,
            string,
            string,
            string,
          )
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
      | UpdateContext(
          session_mode,
          agent_editor_view,
          static_errors_info,
          test_results_info,
        ) =>
        Ok(
          Utils.update_context(
            ~session_mode,
            agent_editor_view,
            static_errors_info,
            test_results_info,
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
      | ErrorMessage(string)
      | /** Stopped in-flight LLM/compaction — not under Filbert (see ChunkedUIChat.Utils.mk). */
        ResponseCancelledMessage(
          string,
        )
      | /** Inline output of a chat slash command (/cost, /credits, /usage, /help). UI-only. */
        SlashCommandOutputMessage(
          Message.Model.slash_command_payload,
        );

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
            | ErrorMessage(_)
            | ResponseCancelledMessage(_)
            | SlashCommandOutputMessage(_) =>
              let chunk = mk_agent_response_chunk(message);
              let updated_model = {
                ...acc_model,
                log: acc_model.log @ [chunk],
              };
              convert_helper(rest, updated_model);
            }
          | System(ResponseCancelled) =>
            let chunk = Model.ResponseCancelledMessage(message.content);
            let updated_model = {
              ...acc_model,
              log: acc_model.log @ [chunk],
            };
            convert_helper(rest, updated_model);
          | System(SlashCommandOutput(payload)) =>
            let chunk = Model.SlashCommandOutputMessage(payload);
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

/** Slash commands in the chat input (see ChatBottomBar). Alphabetically ordered names. */
module ChatSlashCommands = {
  let all_alphabetical: list((string, string)) = [
    ("account-usage", "Show your OpenRouter account credit balance"),
    ("compact", "Summarize the conversation"),
    ("help", "List available slash commands"),
    ("key", "Show the currently-set OpenRouter API key"),
    ("key-usage", "Show usage and limits for the active OpenRouter key"),
    ("session-usage", "Estimate $ cost of this chat from token usage"),
    ("show-thinking", "Toggle display of agent thinking/reasoning text"),
  ];

  let filtered = (filter: string): list((string, string)) => {
    let f = String.lowercase_ascii(filter);
    all_alphabetical
    |> List.filter(((name, _)) =>
         String.length(f) == 0
         || String.starts_with(~prefix=f, String.lowercase_ascii(name))
       );
  };

  /** Typed payload for the /help command — entries are rendered into a custom card. */
  let help_payload = (): Message.Model.help_output => {
    let entries: list(Message.Model.help_entry) =
      List.map(
        ((name, description)): Message.Model.help_entry =>
          {
            help_name: name,
            help_description: description,
          },
        all_alphabetical,
      );
    {help_entries: entries};
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
  [@deriving (show({with_path: false}), sexp, yojson)]
  type llm_error_origin =
    | MainRequest(int)
    | CompactionRequest(int);

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
      main_llm_seq: int,
      compaction_llm_seq: int,
      [@yojson.default None]
      pending_ignore_main_reply_seq: option(int),
      [@yojson.default None]
      pending_ignore_compaction_reply_seq: option(int),
    };
  };

  module Persistent = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Model.t;

    let persist = (model: Model.t): t => {
      {
        ...model,
        prompting: {
          ...model.prompting,
          tools: CompositionUtils.Public.tools,
        },
        restore_editor_state: None,
        last_empty_retry_attempt: None,
        last_active_task_nudge_attempt: None,
        tools_view_expanded: [],
        compaction_in_progress: None,
        compaction_method_override: None,
        main_llm_seq: 0,
        compaction_llm_seq: 0,
        pending_ignore_main_reply_seq: None,
        pending_ignore_compaction_reply_seq: None,
      };
    };

    let unpersist = (p: t): Model.t => {
      {
        ...p,
        prompting: {
          ...p.prompting,
          /* Always use the in-code tool registry so new tools (probes, statics, …)
             appear after upgrades; disabled_tool_names still applies per name. */
          tools: CompositionUtils.Public.tools,
        },
        awaiting_response: None,
        restore_editor_state: None,
        last_empty_retry_attempt: None,
        last_active_task_nudge_attempt: None,
        tools_view_expanded: [],
        compaction_in_progress: None,
        compaction_method_override: None,
        main_llm_seq: 0,
        compaction_llm_seq: 0,
        pending_ignore_main_reply_seq: None,
        pending_ignore_compaction_reply_seq: None,
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
                "place_statics",
                "remove_statics",
                "toggle_statics",
                "place_syntax_projector",
                "remove_syntax_projector",
                "toggle_syntax_projector",
              ],
            ) => "View"
      | n
          when
            List.mem(
              n,
              [
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
                "add_new_subtask_to_active_task",
                "reorder_subtasks_in_active_task",
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
        main_llm_seq: 0,
        compaction_llm_seq: 0,
        pending_ignore_main_reply_seq: None,
        pending_ignore_compaction_reply_seq: None,
      };
    };

    let test_results_for_context =
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

    /** Exact context snapshot string the API uses ([[Message.Utils.context_snapshot_body_for_llm]]),
        from the live scratchpad and chat agent view / workbench — same as [[mk_context_message]]
        when built from the same inputs (e.g. after [[Update.update_context]] on send). */
    let llm_context_snapshot_text =
        (
          ~session_mode: AgentGlobals.Model.session_mode,
          ~cell_result: EvalResult.Model.t,
          cws: CodeWithStatics.Model.t,
          chat: Chat.Model.t,
        )
        : string => {
      let agent_editor_view_string =
        CompositionView.Public.print(
          ~probe_map=cws.dynamics,
          cws.editor,
          chat.agent_view,
        );
      let static_errors_info_string =
        ErrorPrint.all(
          CompositionGo.Public.mk_statics(cws.editor.state.zipper),
        )
        |> String.concat("\n");
      let test_results_info_string =
        test_results_for_context(EvalResult.Model.test_results(cell_result));
      Message.Utils.context_snapshot_body_for_llm(
        ~session_mode,
        agent_editor_view_string,
        static_errors_info_string,
        test_results_info_string,
        AgentWorkbench.Utils.MainUtils.active_task_to_pretty_string(
          chat.agent_workbench,
        ),
      );
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
                Action.Failure.show(err)
                ++ " (structural editor tool could not be applied)",
              ),
            )
          }
        };
      | LanguageServerAction(_) =>
        /* TODO: implement language server queries */
        Ok((agent, editor))
      | InsertAtProgramBoundary(direction, code) =>
        /* No-path variant of insert_before/insert_after.
           - Before: move caret to program start, then paste code (prepend).
           - After: move caret to program end, then paste code (append).
           For an empty program (just `?`), either boundary effectively
           seeds the program with the provided code. */
        let z = editor.editor.state.zipper;
        let mk_statics = CompositionGo.Public.mk_statics;
        let initial_info_map = mk_statics(z);
        let return = (error: Action.Failure.t, z: option(Zipper.t)) =>
          Result.of_option(~error, z);
        let z_at_boundary =
          switch ((direction: Action.Structural.insert_target)) {
          | Before => Move.to_start(z)
          | After => Move.to_end(z)
          };
        switch (
          CompositionGo.Local.PerformUtils.introduce(
            z_at_boundary,
            "\n" ++ code ++ "\n",
            return,
          )
        ) {
        | Error(_) =>
          Error(Failure.Info("Failed to insert code at program boundary"))
        | Ok(new_z) =>
          let new_statics = mk_statics(new_z);
          let old_errors = ErrorPrint.all(initial_info_map);
          let new_errors = ErrorPrint.all(new_statics);
          if (List.length(new_errors) > List.length(old_errors)) {
            Error(
              Failure.Info(
                "Not applying the action you requested as it would introduce new static error(s): "
                ++ String.concat(", ", new_errors),
              ),
            );
          } else {
            let new_z = Dump.to_zipper(new_z);
            let new_editor_model = Editor.Model.mk(new_z);
            let new_code_with_statics =
              CodeWithStatics.Model.mk(new_editor_model);
            Ok((agent, new_code_with_statics));
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
          let new_z = Dump.to_zipper(new_z);
          let new_editor_model = Editor.Model.mk(new_z);
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
      | StaticsAction(statics_action) =>
        let z = editor.editor.state.zipper;
        let info_map = CompositionGo.Public.mk_statics(z);
        switch (HighLevelNodeMap.build(z, info_map)) {
        | None =>
          Error(
            Failure.Info(
              "No bindings in the program. Add let/type bindings first.",
            ),
          )
        | Some(node_map) =>
          let syntax = CachedSyntax.init(z);
          let resolve_path = (path: string): option(Id.t) =>
            HighLevelNodeMap.Public.path_to_id_opt(node_map, path);

          let apply_statics_action =
              (z: Zipper.t, paths: list(string)): (Zipper.t, list(string)) => {
            List.fold_left(
              ((z, expanded), path) =>
                switch (resolve_path(path)) {
                | Some(id) =>
                  switch (statics_action) {
                  | PlaceStatics(_) =>
                    let z =
                      ProbePerform.place_statics_at(~syntax, id, info_map, z);
                    let expanded =
                      switch (
                        ProbePerform.probe_status(id, info_map, z.refractors)
                      ) {
                      | Statics(_) => [path, ...expanded]
                      | _ => expanded
                      };
                    (z, expanded);
                  | RemoveStatics(_) =>
                    let z =
                      ProbePerform.remove_statics_at(
                        ~syntax,
                        id,
                        info_map,
                        z,
                      );
                    (z, expanded);
                  | ToggleStatics(_) =>
                    let z =
                      ProbePerform.toggle_statics_at(
                        ~syntax,
                        id,
                        info_map,
                        z,
                      );
                    let expanded =
                      switch (
                        ProbePerform.probe_status(id, info_map, z.refractors)
                      ) {
                      | Statics(_) => [path, ...expanded]
                      | _ => expanded
                      };
                    (z, expanded);
                  }
                | None => (z, expanded)
                },
              (z, []),
              paths,
            );
          };

          let paths =
            switch (statics_action) {
            | PlaceStatics(p)
            | RemoveStatics(p)
            | ToggleStatics(p) => p
            };
          let (new_z, paths_to_expand) = apply_statics_action(z, paths);
          let new_z = Dump.to_zipper(new_z);
          let new_editor_model = Editor.Model.mk(new_z);
          let new_cws =
            CodeWithStatics.Model.mk(
              ~dynamics=editor.dynamics,
              new_editor_model,
            );

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
      | SyntaxProjectorAction(syntax_projector_action) =>
        let z = editor.editor.state.zipper;
        let info_map = CompositionGo.Public.mk_statics(z);
        switch (HighLevelNodeMap.build(z, info_map)) {
        | None =>
          Error(
            Failure.Info(
              "No bindings in the program. Add let/type bindings first.",
            ),
          )
        | Some(node_map) =>
          let syntax = CachedSyntax.init(z);
          let resolve_path = (path: string): option(Id.t) =>
            HighLevelNodeMap.Public.path_to_syntax_projector_target_id_opt(
              node_map,
              path,
            );

          let apply_syntax_projector_action =
              (z: Zipper.t, paths: list(string))
              : (Zipper.t, list(string), int) => {
            List.fold_left(
              ((z, expanded, n_placed), path) =>
                switch (resolve_path(path)) {
                | Some(id) =>
                  let z_opt =
                    switch (syntax_projector_action) {
                    | PlaceSyntaxProjector(kind, _) =>
                      ProjectorPerform.try_place_syntax_projector(
                        ~term_data=syntax.term_data,
                        id,
                        kind,
                        z,
                      )
                    | ToggleSyntaxProjector(kind, _) =>
                      ProjectorPerform.try_toggle_syntax_projector(
                        ~term_data=syntax.term_data,
                        id,
                        kind,
                        z,
                      )
                    | RemoveSyntaxProjector(_) =>
                      ProjectorPerform.try_remove_syntax_projector(
                        ~term_data=syntax.term_data,
                        id,
                        z,
                      )
                    };
                  switch (z_opt) {
                  | Some(z2) => (z2, [path, ...expanded], n_placed + 1)
                  | None => (z, expanded, n_placed)
                  };
                | None => (z, expanded, n_placed)
                },
              (z, [], 0),
              paths,
            );
          };

          let paths =
            switch (syntax_projector_action) {
            | PlaceSyntaxProjector(_, p)
            | ToggleSyntaxProjector(_, p)
            | RemoveSyntaxProjector(p) => p
            };
          let (new_z, paths_to_expand, n_placed) =
            apply_syntax_projector_action(z, paths);
          if (List.length(paths) > 0 && n_placed == 0) {
            Error(
              Failure.Info(
                "Syntax projector tool did not update the program: no path produced a change. "
                ++ "Paths must be **HighLevelNodeMap binding paths** (e.g. \"map\", \"filter\", or \"outer/inner\" for nested lets), "
                ++ "not pretty-printed expressions or type-applied text from statics overlays. "
                ++ "If a path matched but placement failed, the term at that binding may not support this projector kind.",
              ),
            );
          } else {
            let new_z = Dump.to_zipper(new_z);
            let new_editor_model = Editor.Model.mk(new_z);
            let new_cws =
              CodeWithStatics.Model.mk(
                ~dynamics=editor.dynamics,
                new_editor_model,
              );

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
  };

  module Update = {
    module Action = {
      [@deriving (show({with_path: false}), sexp, yojson)]
      type t =
        | ChatSystemAction(ChatSystem.Update.Action.t)
        | SendMessage(Message.Model.t, Id.t)
        | /** Last [int] is elapsed wall-time (ms) from send to reply,
              used to render "Thought for Ns" on reasoning-bearing turns. */
          HandleLLMResponse(
            OpenRouter.Reply.Model.t,
            Id.t,
            int,
            int,
          )
        | HandleCompactionLLMReply(OpenRouter.Reply.Model.t, Id.t, int)
        | HandleChatNamingResponse(string, Id.t)
        | ApiErrorResponse(Id.t, Message.Model.t, llm_error_origin)
        | RetryApiError(Id.t, int)
        | DoRetryApiSend(Id.t, int)
        | RetryEmptyResponse(Id.t, int)
        | LoadTimelineSegment(Segment.t, int)
        | RestoreOriginal
        | LoadSegmentIntoEditor(Segment.t)
        | SetActiveTimelineNode(option(int))
        | SetToolEnabled(string, bool)
        | SetToolsInCategoryEnabled(string, bool)
        | ToggleToolsViewExpanded(string)
        | RequestForcedCompaction(Id.t)
        | StopAgenticLoop
        | FlushPendingSend(Id.t)
        | RunSlashCommandCost(Id.t)
        | RunSlashCommandHelp(Id.t)
        | RunSlashCommandShowKey(Id.t)
        | RunSlashCommandFetchCredits(Id.t)
        | RunSlashCommandFetchUsage(Id.t)
        | AppendSlashCommandOutput(Id.t, Message.Model.slash_command_payload);
    };

    let max_api_retries = 3;
    let is_retryable_api_error = (code: int): bool =>
      code == 429 || code == 500 || code == 502 || code == 503;

    let format_api_error_content = (~code: int, ~message: string): string =>
      "Code: " ++ string_of_int(code) ++ "\nError: " ++ message;

    /* -- Slash-command result formatters and helpers ----------------------- */

    /** Per-token pricing parsed from OpenRouter's [pricing.prompt]/[pricing.completion]
        strings (dollars per token). Returns 0.0 on parse failure — callers note the
        approximation when prices are missing. */
    let pricing_per_token =
        (llm: option(OpenRouter.AvailableLLMs.Model.llm_info))
        : (float, float) =>
      switch (llm) {
      | None => (0.0, 0.0)
      | Some(info) =>
        let parse = s =>
          try(float_of_string(s)) {
          | _ => 0.0
          };
        (parse(info.pricing.prompt), parse(info.pricing.completion));
      };

    /** Walk a chat and sum prompt/completion tokens across all Agent messages
        that carry usage. Returns (in_tokens, out_tokens). */
    let chat_usage_totals = (chat: Chat.Model.t): (int, int) => {
      let messages = Chat.Utils.get(chat);
      List.fold_left(
        (acc, msg: Message.Model.t) =>
          switch (msg.role) {
          | Agent(Some(usage)) =>
            let (i, o) = acc;
            (i + usage.prompt_tokens, o + usage.completion_tokens);
          | _ => acc
          },
        (0, 0),
        messages,
      );
    };

    /** Build the typed payload for /cost from the current chat + active model. */
    let cost_payload =
        (
          ~chat: Chat.Model.t,
          ~active_llm: option(OpenRouter.AvailableLLMs.Model.llm_info),
        )
        : Message.Model.cost_output => {
      let (in_tok, out_tok) = chat_usage_totals(chat);
      let (price_in, price_out) = pricing_per_token(active_llm);
      let estimated =
        switch (active_llm) {
        | Some(_) =>
          Some(
            float_of_int(in_tok)
            *. price_in
            +. float_of_int(out_tok)
            *. price_out,
          )
        | None => None
        };
      let model =
        switch (active_llm) {
        | Some(info) => info.id
        | None => ""
        };
      {
        cost_model: model,
        cost_input_tokens: in_tok,
        cost_output_tokens: out_tok,
        cost_estimated_usd: estimated,
      };
    };

    /** Plain-text one-liner used as the message's stored `content` (archival/copy). */
    let cost_fallback_text = (p: Message.Model.cost_output): string => {
      let cost_str =
        switch (p.cost_estimated_usd) {
        | None => "(no model)"
        | Some(c) => Printf.sprintf("$%.4f", c)
        };
      let model_str = p.cost_model == "" ? "(no model)" : p.cost_model;
      Printf.sprintf(
        "Session cost: %d in / %d out tokens, est. %s (%s)",
        p.cost_input_tokens,
        p.cost_output_tokens,
        cost_str,
        model_str,
      );
    };

    let credits_payload =
        (credits: OpenRouter.Credits.Model.t): Message.Model.credits_output => {
      credits_used: credits.total_usage,
      credits_total: credits.total_credits,
    };

    let credits_fallback_text = (p: Message.Model.credits_output): string =>
      Printf.sprintf(
        "Credits: $%.2f used of $%.2f (~$%.2f remaining)",
        p.credits_used,
        p.credits_total,
        p.credits_total -. p.credits_used,
      );

    let usage_payload =
        (k: OpenRouter.KeyInfo.Model.t): Message.Model.usage_output => {
      usage_label: k.label,
      usage_is_free_tier: k.is_free_tier,
      usage_total: k.usage,
      usage_daily: k.usage_daily,
      usage_weekly: k.usage_weekly,
      usage_monthly: k.usage_monthly,
      usage_limit: k.limit,
      usage_remaining: k.limit_remaining,
    };

    let usage_fallback_text = (p: Message.Model.usage_output): string =>
      Printf.sprintf(
        "Key usage: $%.2f total (%s)",
        p.usage_total,
        p.usage_is_free_tier ? "free" : "paid",
      );

    let help_fallback_text = (p: Message.Model.help_output): string => {
      let names =
        List.map(
          (e: Message.Model.help_entry) => "/" ++ e.help_name,
          p.help_entries,
        );
      "Slash commands: " ++ String.concat(", ", names);
    };

    let key_fallback_text = (key: string): string =>
      key == "" ? "No OpenRouter API key set." : "OpenRouter API key: " ++ key;

    /** Fire `/api/v1/credits`; on response (or failure), schedule
        [AppendSlashCommandOutput] so the result appears inline in the chat. */
    let fetch_credits_for_slash =
        (~api_key: string, ~chat_id: Id.t, ~schedule_action: Action.t => unit)
        : unit => {
      let handler = (response: option(API.Json.t)): unit => {
        let payload: Message.Model.slash_command_payload =
          switch (response) {
          | None =>
            SlashError(
              "Couldn't reach OpenRouter — check your network and API key.",
            )
          | Some(json) =>
            switch (OpenRouter.Credits.Utils.parse_credits_response(json)) {
            | Some(credits) => CreditsOutput(credits_payload(credits))
            | None =>
              SlashError(
                "OpenRouter responded but the credits payload was unrecognized.",
              )
            }
          };
        schedule_action(Action.AppendSlashCommandOutput(chat_id, payload));
      };
      OpenRouter.Credits.Utils.get_credits(~key=api_key, ~handler);
    };

    let fetch_key_for_slash =
        (~api_key: string, ~chat_id: Id.t, ~schedule_action: Action.t => unit)
        : unit => {
      let handler = (response: option(API.Json.t)): unit => {
        let payload: Message.Model.slash_command_payload =
          switch (response) {
          | None =>
            SlashError(
              "Couldn't reach OpenRouter — check your network and API key.",
            )
          | Some(json) =>
            switch (OpenRouter.KeyInfo.Utils.parse_key_response(json)) {
            | Some(key_info) => UsageOutput(usage_payload(key_info))
            | None =>
              SlashError(
                "OpenRouter responded but the key payload was unrecognized.",
              )
            }
          };
        schedule_action(Action.AppendSlashCommandOutput(chat_id, payload));
      };
      OpenRouter.KeyInfo.Utils.get_key(~key=api_key, ~handler);
    };

    /** Max retries when the assistant returns an empty reply with no tool calls. */
    let max_empty_retries = 2;

    /** Names of tools that mutate the program (EditTools.*). Blocked in Plan mode. */
    let edit_tool_names = [
      "update_definition",
      "update_body",
      "update_pattern",
      "update_binding_clause",
      "delete_binding_clause",
      "delete_body",
      "insert_after",
      "insert_before",
    ];

    /** Names of tools that mutate the workbench task board (WorkbenchTools.*).
        Blocked in Converse mode (along with edit + overlay tools). */
    let workbench_tool_names = [
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
      "add_new_subtask_to_active_task",
      "reorder_subtasks_in_active_task",
    ];

    /** Names of overlay-placement tools (probes / statics / syntax projectors).
        Blocked in Converse mode; allowed in Plan and Edit. */
    let overlay_tool_names = [
      "place_probe",
      "remove_probe",
      "toggle_probe",
      "place_statics",
      "remove_statics",
      "toggle_statics",
      "place_syntax_projector",
      "remove_syntax_projector",
      "toggle_syntax_projector",
    ];

    /** True iff [name] is allowed in [mode]. Edit allows everything; Plan
        blocks edit tools; Converse blocks edit + workbench + overlay tools
        (only ViewTools like expand/collapse remain). */
    let tool_allowed_in_mode =
        (mode: AgentGlobals.Model.session_mode, name: string): bool =>
      switch (mode) {
      | Edit => true
      | Plan => !List.mem(name, edit_tool_names)
      | Converse =>
        !List.mem(name, edit_tool_names)
        && !List.mem(name, workbench_tool_names)
        && !List.mem(name, overlay_tool_names)
      };

    let enabled_tools =
        (~mode: AgentGlobals.Model.session_mode, prompting: Model.prompting)
        : list(API.Json.t) =>
      List.filter(
        (tool: API.Json.t) =>
          switch (ToolUtils.get_name(tool)) {
          | Some(name) =>
            !List.mem(name, prompting.disabled_tool_names)
            && tool_allowed_in_mode(mode, name)
          | None => true
          },
        CompositionUtils.Public.tools,
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
          (),
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

    /** Same [[context]] payload the main agent sees ([[mk_context_message]]), built from the
        live editor and [[agent_view]] — appended last to the compaction API so the summarizer
        has current program text, errors, tests, and workbench (without changing UI state). */
    let compaction_context_snapshot_message =
        (
          ~session_mode: AgentGlobals.Model.session_mode,
          model: Model.t,
          cell_editor: CellEditor.Model.t,
          chat_id: Id.t,
        )
        : Message.Model.t => {
      let curr_chat = ChatSystem.Utils.find_chat(chat_id, model.chat_system);
      Message.Utils.mk_context_system_message(
        Utils.llm_context_snapshot_text(
          ~session_mode,
          ~cell_result=cell_editor.result,
          cell_editor.editor,
          curr_chat,
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
          ~compaction_flight_seq: int,
        )
        : unit => {
      let handler = (response: option(API.Json.t)): unit => {
        switch (OpenRouter.Utils.handle_chat(response)) {
        | Some(OpenRouter.Model.Reply(reply)) =>
          schedule_action(
            Action.HandleCompactionLLMReply(
              reply,
              chat_id,
              compaction_flight_seq,
            ),
          )
        | Some(OpenRouter.Model.Error({message, code})) =>
          let api_error_content =
            "Compaction failed (code "
            ++ string_of_int(code)
            ++ "): "
            ++ message;
          let api_error_message =
            Message.Utils.mk_api_failure_message(api_error_content);
          schedule_action(
            Action.ApiErrorResponse(
              chat_id,
              api_error_message,
              CompactionRequest(compaction_flight_seq),
            ),
          );
        | None =>
          schedule_action(
            Action.ApiErrorResponse(
              chat_id,
              Message.Utils.mk_api_failure_message(
                "Compaction failed: empty API response.",
              ),
              CompactionRequest(compaction_flight_seq),
            ),
          )
        };
      };
      let payload =
        OpenRouter.Payload.Utils.mk_default(
          ~model_id=llm_id,
          ~messages,
          ~tools=[],
          (),
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
      } else if (Option.is_some(model.awaiting_response)) {
        if (manual) {
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
          model;
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
            compaction_context_snapshot_message(
              ~session_mode=settings.agent_globals.session_mode,
              model,
              cell_editor,
              chat_id,
            );
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
            let compaction_flight_seq = model.compaction_llm_seq + 1;
            send_compaction_request(
              ~api_key,
              ~llm_id,
              ~messages=summary_api_msgs,
              ~schedule_action,
              ~chat_id,
              ~compaction_flight_seq,
            );
            {
              ...model,
              compaction_in_progress: Some(chat_id),
              compaction_method_override:
                manual ? Some("Slash command (/compact)") : None,
              compaction_llm_seq: compaction_flight_seq,
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
          ~main_flight_seq: int,
        )
        : unit => {
      let send_started_at = JsUtil.timestamp();
      let handler = (response: option(API.Json.t)): unit => {
        let elapsed_ms = int_of_float(JsUtil.timestamp() -. send_started_at);
        switch (OpenRouter.Utils.handle_chat(response)) {
        | Some(OpenRouter.Model.Reply(reply)) =>
          schedule_action(
            Action.HandleLLMResponse(
              reply,
              chat_id,
              main_flight_seq,
              elapsed_ms,
            ),
          )
        | Some(OpenRouter.Model.Error({message, code})) =>
          if (is_retryable_api_error(code) && retry_attempt < max_api_retries) {
            schedule_action(Action.RetryApiError(chat_id, retry_attempt));
          } else {
            let api_error_content = format_api_error_content(~code, ~message);
            let api_error_message =
              Message.Utils.mk_api_failure_message(api_error_content);
            schedule_action(
              Action.ApiErrorResponse(
                chat_id,
                api_error_message,
                MainRequest(main_flight_seq),
              ),
            );
          }
        | None =>
          schedule_action(
            Action.ApiErrorResponse(
              chat_id,
              Message.Utils.mk_api_failure_message(
                "No response from the language model.",
              ),
              MainRequest(main_flight_seq),
            ),
          )
        };
      };
      OpenRouter.Utils.start_chat(~key=api_key, ~payload, ~handler);
    };

    let send_message =
        (
          ~api_key: option(string),
          ~llm_id: option(string),
          ~reasoning_effort: option(OpenRouter.Payload.Model.effort_level),
          ~session_mode: AgentGlobals.Model.session_mode,
          new_message: Message.Model.t,
          chat_id: Id.t,
          model: Model.t,
          schedule_action: Action.t => unit,
        )
        : Result.t(Model.t) => {
      let user_trying_new_round =
        switch (new_message.role) {
        | User => true
        | _ => false
        };
      let enqueue_while_busy = (m: Model.t, text: string): Model.t => {
        let trimmed = String.trim(text);
        if (trimmed == "") {
          m;
        } else {
          let chat = ChatSystem.Utils.find_chat(chat_id, m.chat_system);
          let chat' = {
            ...chat,
            pending_send_queue: chat.pending_send_queue @ [trimmed],
          };
          {
            ...m,
            chat_system: ChatSystem.Utils.update_chat(chat', m.chat_system),
          };
        };
      };
      if (user_trying_new_round
          && (
            Option.is_some(model.compaction_in_progress)
            || Option.is_some(model.awaiting_response)
          )) {
        Ok(enqueue_while_busy(model, new_message.content));
      } else {
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
          let main_flight_seq = model.main_llm_seq + 1;
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
                ~tools=enabled_tools(~mode=session_mode, model.prompting),
                ~reasoning=?
                  Option.map(
                    e => OpenRouter.Payload.Model.Effort(e),
                    reasoning_effort,
                  ),
                (),
              ),
            ~schedule_action,
            ~chat_id,
            ~retry_attempt=0,
            ~main_flight_seq,
          );
          let current_chat = ChatSystem.Utils.find_chat(chat_id, chat_system);
          if (current_chat.title == "New Chat" && new_message.role == User) {
            if (Option.is_none(model.awaiting_response)
                && Option.is_none(model.compaction_in_progress)) {
              request_chat_name(
                ~api_key,
                ~user_message=new_message.content,
                ~schedule_action,
                ~chat_id,
              );
            };
          };
          Ok({
            ...model,
            chat_system,
            awaiting_response: Some(chat_id),
            main_llm_seq: main_flight_seq,
          });
        };
      };
    };

    /** Start the next model turn after tool result messages are already on the chat (no extra append). */
    let dispatch_follow_up_llm =
        (
          model: Model.t,
          chat_id: Id.t,
          settings: Settings.t,
          schedule_action: Action.t => unit,
        )
        : Model.t => {
      switch (model.compaction_in_progress) {
      | Some(_) => model
      | None =>
        switch (
          settings.agent_globals.api_key,
          AgentGlobals.get_active_llm_id(settings.agent_globals),
        ) {
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
              model.chat_system,
            )
            |> ChatSystem.Update.get;
          {
            ...model,
            chat_system,
            awaiting_response: None,
          };
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
              model.chat_system,
            )
            |> ChatSystem.Update.get;
          {
            ...model,
            chat_system,
            awaiting_response: None,
          };
        | (Some(api_key), Some(llm_id)) =>
          let main_flight_seq = model.main_llm_seq + 1;
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
                ~tools=
                  enabled_tools(
                    ~mode=settings.agent_globals.session_mode,
                    model.prompting,
                  ),
                ~reasoning=?
                  Option.map(
                    e => OpenRouter.Payload.Model.Effort(e),
                    settings.agent_globals.reasoning_effort,
                  ),
                (),
              ),
            ~schedule_action,
            ~chat_id,
            ~retry_attempt=0,
            ~main_flight_seq,
          );
          {
            ...model,
            awaiting_response: Some(chat_id),
            main_llm_seq: main_flight_seq,
          };
        }
      };
    };

    let update_context =
        (
          ~session_mode: AgentGlobals.Model.session_mode,
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
      let static_errors_info_string =
        ErrorPrint.all(
          CompositionGo.Public.mk_statics(editor.editor.state.zipper),
        )
        |> String.concat("\n");
      let test_results_info_string =
        Utils.test_results_for_context(test_results);
      let chat_system =
        ChatSystem.Update.update(
          ChatSystem.Update.Action.ChatAction(
            Chat.Update.Action.UpdateContext(
              session_mode,
              agent_editor_view_string,
              static_errors_info_string,
              test_results_info_string,
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
      switch (action) {
      | EditorAction(edit_action) =>
        switch (
          CompositionGo.Local.get_diff(
            old_editor.state.zipper,
            new_editor.state.zipper,
            edit_action,
            CompositionGo.Public.mk_statics,
            old_editor.syntax,
          )
        ) {
        | Some((old_segment, new_segment)) =>
          Some(
            AgentToolResult.{
              old_segment,
              new_segment,
            },
          )
        | None => None
        }
      | SyntaxProjectorAction(_)
      | ProbeAction(_)
      | StaticsAction(_) =>
        let old_segment =
          Select.all(old_editor.state.zipper).selection.content;
        let new_segment =
          Select.all(new_editor.state.zipper).selection.content;
        let old_s = CompositionView.Public.print_segment(old_segment);
        let new_s = CompositionView.Public.print_segment(new_segment);
        if (old_s == new_s) {
          None;
        } else {
          Some(
            AgentToolResult.{
              old_segment,
              new_segment: Some(new_segment),
            },
          );
        };
      | _ => None
      };
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
      | InsertAtProgramBoundary(_)
      | ProbeAction(_)
      | StaticsAction(_)
      | SyntaxProjectorAction(_) =>
        let old_segment =
          Select.all(old_editor.state.zipper).selection.content;
        let new_segment =
          Select.all(new_editor.state.zipper).selection.content;
        (Some(old_segment), Some(new_segment));
      | _ => (None, None)
      };
    };

    /** Run one tool; returns chat message to append (caller batches append + one LLM request). */
    let execute_one_tool_call =
        (
          ~tool_call: OpenRouter.Reply.Model.tool_call,
          ~model: Model.t,
          ~cell_editor: CellEditor.Model.t,
          ~settings: Settings.t,
          ~chat_id: Id.t,
        )
        : (Model.t, Updated.t(CellEditor.Model.t), Message.Model.t) => {
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
          let model =
            update_context(
              ~session_mode=settings.agent_globals.session_mode,
              model,
              editor,
              chat_id,
            );
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
              skipped: false,
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
            (
              model,
              cell_editor |> Updated.return_quiet,
              Message.Utils.mk_tool_result_message(tool_result),
            );
          | Ok(diff) =>
            let tool_result: AgentToolResult.tool_result = {
              tool_call,
              success: true,
              skipped: false,
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
            (
              model,
              {
                ...cell_editor,
                editor,
              }
              |> Updated.return,
              Message.Utils.mk_tool_result_message(tool_result),
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
              skipped: false,
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
            (
              model,
              cell_editor |> Updated.return_quiet,
              Message.Utils.mk_tool_result_message(tool_result),
            );
          }
        }
      | Failure(msg) =>
        let tool_result: AgentToolResult.tool_result = {
          tool_call,
          success: false,
          skipped: false,
          expanded: false,
          diff: None,
          before_segment: None,
          after_segment: None,
          content: msg,
        };
        // Do not add unparseable tool calls to subtask tool results for now
        (
          model,
          cell_editor |> Updated.return_quiet,
          Message.Utils.mk_tool_result_message(tool_result),
        );
      };
    };

    let handle_llm_response =
        (
          reply: OpenRouter.Reply.Model.t,
          chat_id: Id.t,
          flight_seq: int,
          elapsed_ms: int,
          model: Model.t,
          cell_editor: CellEditor.Model.t,
          settings: Settings.t,
          schedule_action: Action.t => unit,
        )
        : (Model.t, Updated.t(CellEditor.Model.t)) => {
      switch (model.pending_ignore_main_reply_seq) {
      | Some(ignore_seq) when ignore_seq == flight_seq =>
        /* Cancel line was already appended in StopAgenticLoop. */
        (
          {
            ...model,
            pending_ignore_main_reply_seq: None,
            awaiting_response:
              model.main_llm_seq > flight_seq ? model.awaiting_response : None,
            last_empty_retry_attempt: None,
          },
          cell_editor |> Updated.return_quiet,
        )
      | _ =>
        let is_empty = String.trim(reply.content) == "";

        // Empty response with no tool calls: retry with failure context (up to max_empty_retries)
        if (reply.tool_calls == [] && is_empty) {
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
              "(The assistant returned an empty response after retries and did not produce the required user acknowledgment. "
              ++ "If you are using an open workbench plan, close or update it when appropriate; otherwise just reply with at least one sentence for the user. "
              ++ "You can try rephrasing your message.)";
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
              ~reasoning=reply.reasoning,
              ~reasoning_duration_ms=
                Option.is_some(reply.reasoning) ? Some(elapsed_ms) : None,
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
          let merge_cell_editor_updates =
              (
                acc: Updated.t(CellEditor.Model.t),
                step: Updated.t(CellEditor.Model.t),
              )
              : Updated.t(CellEditor.Model.t) => {
            {
              model: step.model,
              is_edit: acc.is_edit || step.is_edit,
              recalculate: acc.recalculate || step.recalculate,
              scroll_active: acc.scroll_active || step.scroll_active,
              logged: acc.logged || step.logged,
              historic: acc.historic || step.historic,
            };
          };
          switch (reply.tool_calls) {
          | [] =>
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
              let nudge_content =
                "[Workbench] You have an **active workbench plan** with an open subtask.\n"
                ++ "MANDATORY: Your next assistant message MUST include at least one full sentence to the user (acknowledge their request or briefly say what you did next). Empty assistant text is invalid.\n"
                ++ "Either: (1) continue program work with edit tools, (2) mark_active_subtask_complete if that milestone is done, (3) mark_active_subtask_failed if stuck, or (4) mark_active_task_complete / mark_active_task_failed when the whole plan is done or abandoned.\n"
                ++ "If you no longer want this plan, close or fail it so the board matches reality.";
              let nudge_message =
                Message.Utils.mk_retry_note_message(
                  ~content=nudge_content,
                  ~sent_to_api=true,
                  ~deliver_as_user_on_api=true,
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
          | tool_calls =>
            let (model_after_tools, _, tool_msgs_rev, cell_editor_updated, _) =
              List.fold_left(
                ((m, ce_model, msgs, ce_updated, prior_failed), tc) =>
                  if (prior_failed) {
                    let skipped = AgentToolResult.mk_skipped(tc);
                    let msg = Message.Utils.mk_tool_result_message(skipped);
                    (m, ce_model, [msg, ...msgs], ce_updated, true);
                  } else {
                    let (m2, step_u, msg) =
                      execute_one_tool_call(
                        ~tool_call=tc,
                        ~model=m,
                        ~cell_editor=ce_model,
                        ~settings,
                        ~chat_id,
                      );
                    let failed =
                      switch (msg.role) {
                      | ToolResult(tr) => !tr.skipped && !tr.success
                      | _ => false
                      };
                    (
                      m2,
                      step_u.model,
                      [msg, ...msgs],
                      merge_cell_editor_updates(ce_updated, step_u),
                      failed,
                    );
                  },
                (
                  model,
                  cell_editor,
                  [],
                  cell_editor |> Updated.return_quiet,
                  false,
                ),
                tool_calls,
              );
            let tool_msgs = List.rev(tool_msgs_rev);
            let chat_system_after_tools =
              List.fold_left(
                (cs, msg) =>
                  ChatSystem.Update.get(
                    ChatSystem.Update.update(
                      ChatSystem.Update.Action.ChatAction(
                        Chat.Update.Action.AppendMessage(msg),
                        chat_id,
                      ),
                      cs,
                    ),
                  ),
                model_after_tools.chat_system,
                tool_msgs,
              );
            let model_with_tool_msgs = {
              ...model_after_tools,
              chat_system: chat_system_after_tools,
            };
            (
              dispatch_follow_up_llm(
                model_with_tool_msgs,
                chat_id,
                settings,
                schedule_action,
              ),
              cell_editor_updated,
            );
          };
        };
      };
    };

    let schedule_flush_pending_if_idle_for_chat =
        (
          model_after: Model.t,
          chat_id: Id.t,
          schedule_action: Action.t => unit,
        )
        : unit =>
      if (Option.is_some(model_after.awaiting_response)
          || Option.is_some(model_after.compaction_in_progress)) {
        ();
      } else {
        let chat =
          ChatSystem.Utils.find_chat(chat_id, model_after.chat_system);
        if (chat.pending_send_queue != []) {
          schedule_action(Action.FlushPendingSend(chat_id));
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
            ~session_mode=settings.agent_globals.session_mode,
            ~test_results=?EvalResult.Model.test_results(editor.result),
            model,
            editor.editor,
            chat_id,
          );
        switch (
          send_message(
            ~api_key=settings.agent_globals.api_key,
            ~llm_id=AgentGlobals.get_active_llm_id(settings.agent_globals),
            ~reasoning_effort=settings.agent_globals.reasoning_effort,
            ~session_mode=settings.agent_globals.session_mode,
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
      | StopAgenticLoop =>
        let (m, e) =
          switch (model.awaiting_response, model.compaction_in_progress) {
          | (Some(awaiting_chat_id), _) =>
            /* Append cancel immediately so it stays above e.g. a flushed
               queued user message (late HTTP still uses pending_ignore_*). */
            let cancelled =
              Message.Utils.mk_response_cancelled_message(
                ~content="Agent response cancelled.",
              );
            let chat_system =
              ChatSystem.Update.update(
                ChatSystem.Update.Action.ChatAction(
                  Chat.Update.Action.AppendMessage(cancelled),
                  awaiting_chat_id,
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
                last_active_task_nudge_attempt: None,
                pending_ignore_main_reply_seq: Some(model.main_llm_seq),
              },
              editor |> Updated.return,
            );
          | (None, Some(compaction_chat_id)) =>
            let cancelled =
              Message.Utils.mk_response_cancelled_message(
                ~content="Compaction cancelled.",
              );
            let chat_system =
              ChatSystem.Update.update(
                ChatSystem.Update.Action.ChatAction(
                  Chat.Update.Action.AppendMessage(cancelled),
                  compaction_chat_id,
                ),
                model.chat_system,
              )
              |> ChatSystem.Update.get;
            (
              {
                ...model,
                chat_system,
                compaction_in_progress: None,
                compaction_method_override: None,
                pending_ignore_compaction_reply_seq:
                  Some(model.compaction_llm_seq),
              },
              editor |> Updated.return,
            );
          | (None, None) => (model, editor |> Updated.return)
          };
        schedule_flush_pending_if_idle_for_chat(
          m,
          m.chat_system.current,
          schedule_action,
        );
        (m, e);
      | FlushPendingSend(chat_id) =>
        if (Option.is_some(model.awaiting_response)
            || Option.is_some(model.compaction_in_progress)) {
          (model, editor |> Updated.return);
        } else {
          let chat = ChatSystem.Utils.find_chat(chat_id, model.chat_system);
          switch (chat.pending_send_queue) {
          | [] => (model, editor |> Updated.return)
          | [first, ...rest] =>
            let chat' = {
              ...chat,
              pending_send_queue: rest,
            };
            let model' = {
              ...model,
              chat_system:
                ChatSystem.Utils.update_chat(chat', model.chat_system),
            };
            schedule_action(
              Action.SendMessage(
                Message.Utils.mk_user_message(first),
                chat_id,
              ),
            );
            (model', editor |> Updated.return);
          };
        }
      | HandleLLMResponse(reply, chat_id, flight_seq, elapsed_ms) =>
        let (m, e) =
          handle_llm_response(
            reply,
            chat_id,
            flight_seq,
            elapsed_ms,
            model,
            editor,
            settings,
            schedule_action,
          );
        schedule_flush_pending_if_idle_for_chat(m, chat_id, schedule_action);
        (m, e);
      | HandleCompactionLLMReply(reply, chat_id, flight_seq) =>
        let (m, e) =
          switch (model.pending_ignore_compaction_reply_seq) {
          | Some(ignore_seq) when ignore_seq == flight_seq =>
            /* "Compaction cancelled." was already appended in StopAgenticLoop. */
            (
              {
                ...model,
                pending_ignore_compaction_reply_seq: None,
              },
              editor |> Updated.return,
            )
          | _ =>
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
          };
        schedule_flush_pending_if_idle_for_chat(m, chat_id, schedule_action);
        (m, e);
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
      | AppendSlashCommandOutput(chat_id, payload) =>
        let content: string =
          switch (payload) {
          | CostOutput(p) => cost_fallback_text(p)
          | CreditsOutput(p) => credits_fallback_text(p)
          | UsageOutput(p) => usage_fallback_text(p)
          | KeyOutput(k) => key_fallback_text(k)
          | HelpOutput(p) => help_fallback_text(p)
          | Notice(s) => s
          | SlashError(s) => s
          };
        let msg =
          Message.Utils.mk_slash_command_output_message(~payload, ~content);
        let chat_system =
          ChatSystem.Update.update(
            ChatSystem.Update.Action.ChatAction(
              Chat.Update.Action.AppendMessage(msg),
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
      | RunSlashCommandHelp(chat_id) =>
        schedule_action(
          Action.AppendSlashCommandOutput(
            chat_id,
            HelpOutput(ChatSlashCommands.help_payload()),
          ),
        );
        (model, editor |> Updated.return);
      | RunSlashCommandCost(chat_id) =>
        let chat = ChatSystem.Utils.find_chat(chat_id, model.chat_system);
        let payload =
          cost_payload(~chat, ~active_llm=settings.agent_globals.active_llm);
        schedule_action(
          Action.AppendSlashCommandOutput(chat_id, CostOutput(payload)),
        );
        (model, editor |> Updated.return);
      | RunSlashCommandShowKey(chat_id) =>
        let key =
          switch (settings.agent_globals.api_key) {
          | None => ""
          | Some(k) => k
          };
        schedule_action(
          Action.AppendSlashCommandOutput(chat_id, KeyOutput(key)),
        );
        (model, editor |> Updated.return);
      | RunSlashCommandFetchCredits(chat_id) =>
        switch (settings.agent_globals.api_key) {
        | None =>
          schedule_action(
            Action.AppendSlashCommandOutput(
              chat_id,
              SlashError("Set an OpenRouter API key first."),
            ),
          );
          (model, editor |> Updated.return);
        | Some(api_key) =>
          fetch_credits_for_slash(~api_key, ~chat_id, ~schedule_action);
          (model, editor |> Updated.return);
        }
      | RunSlashCommandFetchUsage(chat_id) =>
        switch (settings.agent_globals.api_key) {
        | None =>
          schedule_action(
            Action.AppendSlashCommandOutput(
              chat_id,
              SlashError("Set an OpenRouter API key first."),
            ),
          );
          (model, editor |> Updated.return);
        | Some(api_key) =>
          fetch_key_for_slash(~api_key, ~chat_id, ~schedule_action);
          (model, editor |> Updated.return);
        }
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
      | ApiErrorResponse(chat_id, api_error_message, origin) =>
        let (m, e) =
          switch (origin) {
          | MainRequest(seq) =>
            switch (model.pending_ignore_main_reply_seq) {
            | Some(ig) when ig == seq => (
                {
                  ...model,
                  pending_ignore_main_reply_seq: None,
                  awaiting_response:
                    model.main_llm_seq > seq ? model.awaiting_response : None,
                },
                editor |> Updated.return,
              )
            | _ =>
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
                },
                editor |> Updated.return,
              );
            }
          | CompactionRequest(seq) =>
            switch (model.pending_ignore_compaction_reply_seq) {
            | Some(ig) when ig == seq => (
                {
                  ...model,
                  pending_ignore_compaction_reply_seq: None,
                  compaction_in_progress:
                    model.compaction_llm_seq > seq
                      ? model.compaction_in_progress : None,
                  compaction_method_override:
                    model.compaction_llm_seq > seq
                      ? model.compaction_method_override : None,
                },
                editor |> Updated.return,
              )
            | _ =>
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
            }
          };
        schedule_flush_pending_if_idle_for_chat(m, chat_id, schedule_action);
        (m, e);
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
            ~deliver_as_user_on_api=false,
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
            ~session_mode=settings.agent_globals.session_mode,
            ~test_results=?EvalResult.Model.test_results(editor.result),
            model,
            editor.editor,
            chat_id,
          );
        if (Option.is_some(model.pending_ignore_main_reply_seq)) {
          (
            {
              ...model,
              pending_ignore_main_reply_seq: None,
              awaiting_response: None,
            },
            editor |> Updated.return,
          );
        } else {
          let main_flight_seq = model.main_llm_seq + 1;
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
                  ~tools=
                    enabled_tools(
                      ~mode=settings.agent_globals.session_mode,
                      model.prompting,
                    ),
                  ~reasoning=?
                    Option.map(
                      e => OpenRouter.Payload.Model.Effort(e),
                      settings.agent_globals.reasoning_effort,
                    ),
                  (),
                ),
              ~schedule_action,
              ~chat_id,
              ~retry_attempt=attempt + 1,
              ~main_flight_seq,
            );
            (
              {
                ...model,
                main_llm_seq: main_flight_seq,
              },
              editor |> Updated.return,
            );
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
        };
      | RetryEmptyResponse(chat_id, attempt) =>
        if (Option.is_some(model.pending_ignore_main_reply_seq)) {
          (
            {
              ...model,
              pending_ignore_main_reply_seq: None,
              awaiting_response: None,
              last_empty_retry_attempt: None,
            },
            editor |> Updated.return,
          );
        } else {
          let retry_msg =
            "[Retry "
            ++ string_of_int(attempt + 1)
            ++ "/"
            ++ string_of_int(max_empty_retries)
            ++ "] Your previous assistant message had no visible text (empty or whitespace-only). That is not allowed.\n"
            ++ "MANDATORY: Reply now with at least one full sentence addressed to the user — acknowledge what they asked for, or summarize what you changed or attempted. Do not send an empty message again.\n"
            ++ "If you started a workbench plan and it is still open, update or close it when it matches your intent (e.g. mark_active_task_complete, mark_active_task_failed, or subtask tools). In the same turn or the next, write the required user-facing sentence.\n"
            ++ "Never end with both empty text and no tool calls.";
          let retry_message =
            Message.Utils.mk_retry_note_message(
              ~content=retry_msg,
              ~sent_to_api=true,
              ~deliver_as_user_on_api=true,
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
            let main_flight_seq = model.main_llm_seq + 1;
            send_llm_request(
              ~api_key,
              ~payload=
                OpenRouter.Payload.Utils.mk_default(
                  ~model_id=llm_id,
                  ~messages=
                    Chat.Utils.api_messages_of_messages(
                      Chat.Utils.messages_for_openrouter(
                        ChatSystem.Utils.find_chat(
                          chat_id,
                          model.chat_system,
                        ),
                      ),
                    ),
                  ~tools=
                    enabled_tools(
                      ~mode=settings.agent_globals.session_mode,
                      model.prompting,
                    ),
                  ~reasoning=?
                    Option.map(
                      e => OpenRouter.Payload.Model.Effort(e),
                      settings.agent_globals.reasoning_effort,
                    ),
                  (),
                ),
              ~schedule_action,
              ~chat_id,
              ~retry_attempt=0,
              ~main_flight_seq,
            );
            (
              {
                ...model,
                main_llm_seq: main_flight_seq,
              },
              editor |> Updated.return,
            );
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
        }
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
        let new_editor_model = Editor.Model.mk(new_zipper);
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
          let new_editor_model = Editor.Model.mk(new_zipper);
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
      | SetToolsInCategoryEnabled(category, enabled) =>
        let names_in_cat =
          CompositionUtils.Public.tools
          |> List.filter_map((tool: API.Json.t) =>
               switch (ToolUtils.get_name(tool)) {
               | Some(name) when ToolUtils.category_of_tool(name) == category =>
                 Some(name)
               | _ => None
               }
             );
        let disabled_tool_names =
          if (enabled) {
            List.filter(
              (n: string) => !List.mem(n, names_in_cat),
              model.prompting.disabled_tool_names,
            );
          } else {
            List.fold_left(
              (acc: list(string), n: string) =>
                List.mem(n, acc) ? acc : [n, ...acc],
              model.prompting.disabled_tool_names,
              names_in_cat,
            );
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
