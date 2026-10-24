open Util;
open Haz3lcore;
open AgentModel;

module Utils = AgentUtils;
module Action = AgentAction;

let tool_action = (tr: AgentToolResult.tool_result) =>
  CompositionUtils.Public.action_of(
    ~tool_name=tr.tool_call.name,
    ~args=tr.tool_call.args,
  );

/* a structural edit's result */
let is_edit = (tr: AgentToolResult.tool_result): bool =>
  switch (tool_action(tr)) {
  | Action(EditorAction(_)) => true
  | _ => false
  };

/* a tool whose change is the editor's: a reply's rollback undoes it */
let changes_editor = (tr: AgentToolResult.tool_result): bool =>
  switch (tool_action(tr)) {
  | Action(ProbeAction(_) | StaticsAction(_) | SyntaxProjectorAction(_)) =>
    true
  | _ => false
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
          tentative: acc.tentative || step.tentative,
          retract: acc.retract || step.retract,
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
        /* a reply's structural calls run as one transaction
           (AgentToolExec.Edits): their results wait for its end, and a
           failure anywhere in a reply with edits restores the editor it
           began with */
        let calls = Array.of_list(tool_calls);
        let (
          model_after_tools,
          slots_rev,
          cell_editor_updated,
          edits,
          failure,
        ) =
          List.fold_left(
            (
              (
                m,
                slots,
                ce_updated: Updated.t(CellEditor.Model.t),
                edits,
                failure,
              ),
              (i, tc),
            ) =>
              if (failure != None) {
                (m, [`Skipped(tc), ...slots], ce_updated, edits, failure);
              } else {
                switch (
                  CompositionUtils.Public.action_of(
                    ~tool_name=tc.OpenRouter.Reply.Model.name,
                    ~args=tc.args,
                  )
                ) {
                | Action(EditorAction(call)) =>
                  let e =
                    switch (edits) {
                    | Some(e) => e
                    | None =>
                      AgentToolExec.Edits.begin_(~settings, ce_updated.model)
                    };
                  switch (
                    try(AgentToolExec.Edits.add(e, ~index=i, call)) {
                    | exn =>
                      AgentToolExec.report_tool_exn(tc.name, exn);
                      Error(Printexc.to_string(exn));
                    }
                  ) {
                  | Ok(e) =>
                    let step =
                      AgentToolExec.Edits.editor(e) |> Updated.return;
                    (
                      m,
                      [`Edit(tc), ...slots],
                      merge_cell_editor_updates(ce_updated, step),
                      Some(e),
                      None,
                    );
                  | Error(message) => (
                      m,
                      [`Edit(tc), ...slots],
                      ce_updated,
                      Some(e),
                      Some((i, message)),
                    )
                  };
                | _ =>
                  let (m2, step_u, msg) =
                    AgentToolExec.execute_one_tool_call(
                      ~tool_call=tc,
                      ~model=m,
                      ~cell_editor=ce_updated.model,
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
                    [`Done(msg), ...slots],
                    merge_cell_editor_updates(ce_updated, step_u),
                    edits,
                    failed ? Some((i, "")) : None,
                  );
                };
              },
            (model, [], cell_editor |> Updated.return_quiet, None, None),
            List.mapi((i, tc) => (i, tc), tool_calls),
          );
        let tool_call = i => calls[i];
        /* the reply's edits, checked: the editor and the edits' results,
           or the call that failed and why */
        let (cell_editor_updated, results, failure) =
          switch (edits, failure) {
          | (None, _) => (cell_editor_updated, [], failure)
          | (Some(_), Some(_)) => (
              cell_editor |> Updated.return_quiet,
              [],
              failure,
            )
          | (Some(e), None) =>
            switch (
              try(AgentToolExec.Edits.finish(~settings, ~tool_call, e)) {
              | exn =>
                AgentToolExec.report_tool_exn("reply", exn);
                Error({
                  index: 0,
                  message: Printexc.to_string(exn),
                });
              }
            ) {
            | Ok((ce, results)) => (
                merge_cell_editor_updates(
                  cell_editor_updated,
                  ce |> Updated.return,
                ),
                results,
                None,
              )
            | Error(f) => (
                cell_editor |> Updated.return_quiet,
                [],
                Some((f.index, f.message)),
              )
            }
          };
        let rolled_back = edits != None && failure != None;
        let undone = (failed: int) =>
          " None of this reply's edits were applied: call "
          ++ string_of_int(failed + 1)
          ++ " ("
          ++ tool_call(failed).name
          ++ ") failed.";
        let tool_msgs_rev =
          List.mapi(
            (k, slot) => {
              let i = List.length(slots_rev) - 1 - k;
              let before = e =>
                Some(
                  AgentToolExec.Edits.text_of(
                    CompositionGo.Reply.program(
                      (e: AgentToolExec.Edits.t).reply,
                    ),
                  ),
                );
              switch (slot, failure) {
              | (`Skipped(tc), _) =>
                Message.Utils.mk_tool_result_message(
                  AgentToolResult.mk_skipped(tc),
                )
              | (`Edit(tc), Some((f, message))) when f == i =>
                Message.Utils.mk_tool_result_message(
                  AgentToolExec.Edits.failed(
                    ~tool_call=tc,
                    ~before=Option.bind(edits, before),
                    message ++ undone(f),
                  ),
                )
              | (`Edit(tc), Some((f, _))) =>
                Message.Utils.mk_tool_result_message(
                  AgentToolExec.Edits.failed(
                    ~tool_call=tc,
                    ~before=None,
                    "Rolled back." ++ undone(f),
                  ),
                )
              | (`Edit(tc), None) =>
                Message.Utils.mk_tool_result_message(
                  switch (List.assoc_opt(i, results)) {
                  | Some(tr) => tr
                  | None =>
                    AgentToolExec.Edits.failed(
                      ~tool_call=tc,
                      ~before=None,
                      "No result.",
                    )
                  },
                )
              | (`Done(msg: Message.Model.t), Some((f, _)))
                  when rolled_back && f != i =>
                switch (msg.role) {
                | ToolResult(tr) when tr.success && changes_editor(tr) =>
                  Message.Utils.mk_tool_result_message({
                    ...tr,
                    success: false,
                    content: "Rolled back." ++ undone(f),
                    content_is_payload: false,
                  })
                | _ => msg
                }
              | (`Done(msg), _) => msg
              };
            },
            slots_rev,
          );
        let model_after_tools =
          List.fold_left(
            (m, msg: Message.Model.t) =>
              switch (msg.role) {
              | ToolResult(tr) when is_edit(tr) =>
                AgentToolExec.add_tool_result_to_active_subtask(
                  ~tool_result=tr,
                  ~action=EditorAction(Delete(BindingClause, "")),
                  ~model=m,
                  ~chat_id,
                )
              | _ => m
              },
            model_after_tools,
            List.rev(tool_msgs_rev),
          );
        let tool_msgs = List.rev(tool_msgs_rev);
        /* the applied edits' last result takes the tests' change once
           evaluation settles */
        switch (
          edits,
          failure,
          List.find_opt(
            (msg: Message.Model.t) =>
              switch (msg.role) {
              | ToolResult(tr) => tr.success && is_edit(tr)
              | _ => false
              },
            tool_msgs_rev,
          ),
        ) {
        | (Some(e), None, Some(msg)) =>
          AgentToolExec.Tests.pending :=
            Some({
              chat_id,
              message: msg.id,
              root: e.reply.root,
              before_program: e.reply.start,
              before: EvalResult.Model.test_results(cell_editor.result),
              after_program: CompositionGo.Reply.program(e.reply),
            })
        | _ => ()
        };
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
