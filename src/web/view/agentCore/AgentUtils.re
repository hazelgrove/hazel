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
      tools: CompositionUtils.tools,
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

/* the agent's view of [cws] for [chat]: the text, the open set against
   this program, and what the view was made from */
let agent_view_of =
    (
      ~view: CompositionView.settings,
      ~test_results: option(Language.TestResults.t),
      ~value: option(Language.Exp.t),
      cws: CodeWithStatics.Model.t,
      chat: Chat.Model.t,
    )
    : (string, AgentContext.Model.t, SentView.basis) => {
  let z = cws.editor.state.zipper;
  let root = cws.editor.root;
  let outline = Outline.of_program(~root, Zipper.unselect_and_zip(z));
  let opened = AgentContext.Utils.refresh(outline, chat.agent_view);
  let facts: CompositionView.facts = {
    info_map: cws.statics.info_map,
    error_ids: cws.statics.error_ids,
    tests: test_results,
    probes: cws.dynamics,
    refractors: z.refractors.manuals,
    value,
    term_data: cws.editor.syntax.term_data,
  };
  let view =
    CompositionView.render(
      ~settings=view,
      ~facts,
      outline,
      ~opened=AgentContext.Utils.ids(opened),
    );
  (
    view,
    opened,
    {
      b_root: root,
      b_text: SentView.text_of_zipper(z),
      b_opened: opened.opened,
      b_probes: List.length(z.refractors.manuals),
    },
  );
};

/** Exact context snapshot string the API uses ([[Message.Utils.context_snapshot_body_for_llm]]),
    from the live scratchpad and chat agent view / workbench — same as [[mk_context_message]]
    when built from the same inputs (e.g. after [[Update.update_context]] on send). */
let llm_context_snapshot_text =
    (
      ~session_mode: AgentGlobals.Model.session_mode,
      ~view: CompositionView.settings,
      ~cell_result: EvalResult.Model.t,
      cws: CodeWithStatics.Model.t,
      chat: Chat.Model.t,
    )
    : string => {
  let (view, _, _) =
    agent_view_of(
      ~view,
      ~test_results=EvalResult.Model.test_results(cell_result),
      ~value=EvalResult.Model.get_value(cell_result),
      cws,
      chat,
    );
  Message.Utils.context_snapshot_body_for_llm(
    ~session_mode,
    view,
    AgentWorkbench.Utils.MainUtils.active_task_to_pretty_string(
      chat.agent_workbench,
    ),
  );
};

/* the last view's inputs and outputs: a reply's tools followed by its
   send ask several times for one program */
type memo = {
  m_zipper: Zipper.t,
  m_info: Language.Statics.Map.t,
  m_dynamics: Language.Dynamics.Map.t,
  m_tests: option(Language.TestResults.t),
  m_value: option(Language.Exp.t),
  m_view: AgentContext.Model.t,
  m_view_settings: CompositionView.settings,
  m_out: (string, AgentContext.Model.t, SentView.basis),
};
let context_memo: ref(option(memo)) = ref(None);

let update_context =
    (
      ~session_mode: AgentGlobals.Model.session_mode,
      ~view: CompositionView.settings=CompositionView.default_settings,
      ~test_results: option(Language.TestResults.t)=?,
      ~value: option(Language.Exp.t)=?,
      model: Model.t,
      editor: CodeWithStatics.Model.t,
      chat_id: Id.t,
    )
    : Model.t => {
  let curr_chat = ChatSystem.Utils.find_chat(chat_id, model.chat_system);
  let (view, agent_view, basis) =
    switch (context_memo^) {
    | Some(m)
        when
          m.m_zipper === editor.editor.state.zipper
          && m.m_info === editor.statics.info_map
          && m.m_dynamics === editor.dynamics
          && m.m_tests === test_results
          && m.m_value === value
          && m.m_view == curr_chat.agent_view
          && m.m_view_settings == view =>
      m.m_out
    | _ =>
      let out =
        agent_view_of(~view, ~test_results, ~value, editor, curr_chat);
      context_memo :=
        Some({
          m_zipper: editor.editor.state.zipper,
          m_info: editor.statics.info_map,
          m_dynamics: editor.dynamics,
          m_tests: test_results,
          m_value: value,
          m_view: curr_chat.agent_view,
          m_view_settings: view,
          m_out: out,
        });
      out;
    };
  let chat_system =
    ChatSystem.Update.update(
      ChatSystem.Update.Action.ChatAction(
        Chat.Update.Action.UpdateContext(
          session_mode,
          agent_view,
          Some(basis),
          view,
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
