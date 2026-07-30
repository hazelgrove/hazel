open Haz3lcore;
open AgentResult;
open AgentModel;

let init = (): Model.t => {
  let system_prompt = CompositionPrompt.self |> String.concat("\n");
  let dev_notes = {|Development mode active. Follow developer instructions precisely. Be concise. No first-person pronouns.|};
  /* reset_transients is the source of truth for the transient fields below. */
  Model.reset_transients({
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
    pending_dispatch_send: None,
    pending_assistant_content: "",
    pending_assistant_reasoning: "",
  });
};

let append_to_chat_system =
    (~chat_id: Id.t, msg: Message.Model.t, chat_system: ChatSystem.Model.t)
    : ChatSystem.Model.t =>
  ChatSystem.Update.update(
    ChatSystem.Update.Action.ChatAction(
      Chat.Update.Action.AppendMessage(msg),
      chat_id,
    ),
    chat_system,
  )
  |> ChatSystem.Update.get;

/** Append [msg] to [chat_id]'s transcript. */
let append_message =
    (~chat_id: Id.t, msg: Message.Model.t, model: Model.t): Model.t => {
  ...model,
  chat_system: append_to_chat_system(~chat_id, msg, model.chat_system),
};

let clear_pending_assistant_stream = (model: Model.t): Model.t => {
  ...model,
  pending_assistant_content: "",
  pending_assistant_reasoning: "",
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
      ~probe_map=cws.dynamics.probe_map,
      cws.editor,
      chat.agent_view,
    );
  let static_errors_info_string =
    ErrorPrint.all(CompositionGo.Public.mk_statics(cws.editor.state.zipper))
    |> String.concat("\n");
  let test_results_info_string =
    test_results_for_context(
      EvalResult.Model.test_results(cell_result) |> Util.Calc.get_value,
    );
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
      ~probe_map=editor.dynamics.probe_map,
      editor.editor,
      curr_chat.agent_view,
    );
  let static_errors_info_string =
    ErrorPrint.all(
      CompositionGo.Public.mk_statics(editor.editor.state.zipper),
    )
    |> String.concat("\n");
  let test_results_info_string = test_results_for_context(test_results);
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
