open Util_web;
open Haz3lcore;
open Util_web.API;
open Ppx_yojson_conv_lib.Yojson_conv;

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
      /* Cache breakpoint anchor: caching the prefix up to dev-notes covers
         tools + the ~20k system prompt (render order is tools → system →
         messages), the large static block reused every turn. */
      api_message:
        Some({
          ...OpenRouter.Message.Utils.mk_developer_msg(sanitized_content),
          cache_anchor: true,
        }),
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
    let sanitized_static_errors_content = String.trim(static_errors_content);
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
                `String(CompositionView.Public.print_segment(before_segment))
              | None => `Null
              },
            ),
            (
              "after",
              switch (tool_result.after_segment) {
              | Some(after_segment) =>
                `String(CompositionView.Public.print_segment(after_segment))
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
