open Util;
open Util.API;
open Haz3lcore;
open Ppx_yojson_conv_lib.Yojson_conv;
open AgentResult;
open AgentModel;

module ToolUtils = AgentToolUtils;
module Utils = AgentUtils;
module ToolCallHandler = AgentToolCallHandler;

module Action = AgentAction;

/* Re-exported for external callers/tests ([Agent.Update.*]). */
let defer_dispatch_send = AgentSend.defer_dispatch_send;
let tool_allowed_in_mode = AgentSend.tool_allowed_in_mode;
let backoff_ms = AgentSend.backoff_ms;
let format_api_error_content = AgentSend.format_api_error_content;

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
  /* The streamed pending text has been superseded by [reply.content]; the
     complete assistant message is about to be appended via AppendMessage. */
  let model = Utils.clear_pending_assistant_stream(model);
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
      if (next_retry < AgentSend.max_empty_retries) {
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
        (
          {
            ...Utils.append_message(~chat_id, new_message, model),
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
      let model = {
        ...Utils.append_message(~chat_id, new_message, model),
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
        let model_idle = {
          ...model,
          awaiting_response: None,
          last_active_task_nudge_attempt: None,
        };
        let limit_opt =
          AgentGlobals.context_meter_limit_for_active(settings.agent_globals);
        let should_compact =
          switch (reply.usage, limit_opt) {
          | (Some(usage), Some(limit)) => usage.prompt_tokens >= limit
          | _ => false
          };
        let may_start_compaction =
          Option.is_none(model_idle.compaction_in_progress);
        if (should_compact && may_start_compaction) {
          let model' =
            AgentCompaction.maybe_start_compaction(
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
                  AgentToolExec.execute_one_tool_call(
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
        let model_with_tool_msgs =
          List.fold_left(
            (m, msg) => Utils.append_message(~chat_id, msg, m),
            model_after_tools,
            tool_msgs,
          );
        (
          AgentSend.dispatch_follow_up_llm(
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
    AgentSend.send_message(message, chat_id, model, editor, schedule_action)
  | DispatchSend(chat_id) =>
    AgentSend.handle_dispatch_send(
      chat_id,
      model,
      editor,
      settings,
      schedule_action,
    )
  | StopAgenticLoop =>
    AgentSend.stop_agentic_loop(model, editor, schedule_action)
  | FlushPendingSend(chat_id) =>
    AgentSend.flush_pending_send(chat_id, model, editor, schedule_action)
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
    AgentSend.schedule_flush_pending_if_idle_for_chat(
      m,
      chat_id,
      schedule_action,
    );
    (m, e);
  | HandleCompactionLLMReply(reply, chat_id, flight_seq) =>
    let (m, e) =
      AgentCompaction.handle_compaction_reply(
        reply,
        chat_id,
        flight_seq,
        model,
        editor,
      );
    AgentSend.schedule_flush_pending_if_idle_for_chat(
      m,
      chat_id,
      schedule_action,
    );
    (m, e);
  | RequestForcedCompaction(chat_id) =>
    let model' =
      AgentCompaction.maybe_start_compaction(
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
      | CostOutput(p) => AgentSlashFormat.cost_fallback_text(p)
      | CreditsOutput(p) => AgentSlashFormat.credits_fallback_text(p)
      | UsageOutput(p) => AgentSlashFormat.usage_fallback_text(p)
      | KeyOutput(k) => AgentSlashFormat.key_fallback_text(k)
      | HelpOutput(p) => AgentSlashFormat.help_fallback_text(p)
      | Notice(s) => s
      | SlashError(s) => s
      };
    let msg =
      Message.Utils.mk_slash_command_output_message(~payload, ~content);
    (Utils.append_message(~chat_id, msg, model), editor |> Updated.return);
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
      AgentSlashFormat.cost_payload(
        ~chat,
        ~active_llm=settings.agent_globals.active_llm,
      );
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
      AgentSlashFormat.fetch_credits_for_slash(
        ~api_key,
        ~chat_id,
        ~schedule_action,
      );
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
      AgentSlashFormat.fetch_key_for_slash(
        ~api_key,
        ~chat_id,
        ~schedule_action,
      );
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
    AgentSend.handle_api_error_response(
      chat_id,
      api_error_message,
      origin,
      model,
      editor,
      schedule_action,
    )
  | RetryApiError(chat_id, attempt) =>
    AgentSend.retry_api_error(
      chat_id,
      attempt,
      model,
      editor,
      schedule_action,
    )
  | DoRetryApiSend(chat_id, attempt) =>
    AgentSend.do_retry_api_send(
      chat_id,
      attempt,
      model,
      editor,
      settings,
      schedule_action,
    )
  | RetryEmptyResponse(chat_id, attempt) =>
    AgentSend.retry_empty_response(
      chat_id,
      attempt,
      model,
      editor,
      settings,
      schedule_action,
    )
  | LoadTimelineSegment(segment, node_index) =>
    // On first click into history: save current editor state for Restore Original
    let restore_editor_state =
      switch (model.restore_editor_state) {
      | None =>
        Some(Select.all(editor.editor.editor.state.zipper).selection.content)
      | Some(_) => model.restore_editor_state
      };
    let new_zipper = Zipper.unzip(~direction=Right, segment);
    let new_editor_model = Editor.Model.mk(new_zipper);
    let new_code_with_statics = CodeWithStatics.Model.mk(new_editor_model);
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
      let new_code_with_statics = CodeWithStatics.Model.mk(new_editor_model);
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
    let new_code_with_statics = CodeWithStatics.Model.mk(new_editor_model);
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
        ? List.filter((n: string) => n != name, model.tools_view_expanded)
        : [name, ...model.tools_view_expanded];
    (
      {
        ...model,
        tools_view_expanded,
      },
      editor |> Updated.return,
    );
  | StreamDelta(_chat_id, flight_seq, content_delta, reasoning_delta) =>
    /* Late deltas from a stream the user Stopped or superseded are
       dropped. The same seq-gate protects HandleLLMResponse / ApiError. */
    switch (model.pending_ignore_main_reply_seq) {
    | Some(ignore_seq) when ignore_seq == flight_seq => (
        model,
        editor |> Updated.return_quiet,
      )
    | _ => (
        {
          ...model,
          pending_assistant_content:
            model.pending_assistant_content ++ content_delta,
          pending_assistant_reasoning:
            model.pending_assistant_reasoning ++ reasoning_delta,
        },
        editor |> Updated.return_quiet,
      )
    }
  };
};
