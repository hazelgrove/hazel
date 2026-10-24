open Util;
open Haz3lcore;
open AgentModel;

module Utils = AgentUtils;
module Action = AgentAction;

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
        let (model_after_tools, cell_editor_updated, tool_msgs) =
          AgentToolExec.Reply.run(
            ~settings,
            ~chat_id,
            ~model,
            ~cell_editor,
            tool_calls,
          );
        let model_with_tool_msgs =
          List.fold_left(
            (m, msg) => Utils.append_message(~chat_id, msg, m),
            model_after_tools,
            tool_msgs,
          );
        (
          AgentSend.defer_follow_up_llm(
            model_with_tool_msgs,
            chat_id,
            schedule_action,
          ),
          cell_editor_updated,
        );
      };
    };
  };
};
