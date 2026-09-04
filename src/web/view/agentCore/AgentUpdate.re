open Util_web;
open Haz3lcore;
open AgentResult;
open AgentModel;

module ToolUtils = AgentToolUtils;
module Utils = AgentUtils;

module Action = AgentAction;

/* Re-exported for external callers/tests ([Agent.Update.*]). */
let defer_dispatch_send = AgentSend.defer_dispatch_send;
let tool_allowed_in_mode = AgentSend.tool_allowed_in_mode;
let backoff_ms = AgentSend.backoff_ms;
let format_api_error_content = AgentSend.format_api_error_content;

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
      AgentResponse.handle_llm_response(
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
    let new_editor_model = Editor.Model.mk(new_zipper, ~root=Exp);
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
      let new_editor_model = Editor.Model.mk(new_zipper, ~root=Exp);
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
    let new_editor_model = Editor.Model.mk(new_zipper, ~root=Exp);
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
