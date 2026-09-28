/** Tests for [[Agent.Update]]: Stop / flight sequence ignore, send queue flush,
    and matching [[ApiErrorResponse]] ignore paths — deterministic (no HTTP). */
open Alcotest;
open Haz3lcore;
open Util;
open Web;

let mk_reply =
    (~content: string="", tool_calls: list(OpenRouter.Reply.Model.tool_call))
    : OpenRouter.Reply.Model.t => {
  content,
  tool_calls,
  usage: None,
  reasoning: None,
};

let cell_editor = () =>
  CellEditor.Model.mk(Editor.Model.mk(Zipper.init(), ~root=Exp));

/* Run the phase-2 send deferral synchronously so [drain_scheduled] sees
   DispatchSend; in the browser it is a 0ms timeout (paint gap). */
let () = Agent.Update.defer_dispatch_send := (thunk => thunk());

let with_busy_main = (~seq: int, agent: Agent.Model.t): Agent.Model.t => {
  let chat_id = agent.chat_system.current;
  {
    ...agent,
    awaiting_response: Some(chat_id),
    main_llm_seq: seq,
  };
};

let with_chat_queue = (agent: Agent.Model.t, q: list(string)): Agent.Model.t => {
  let chat_id = agent.chat_system.current;
  let chat = ChatSystem.Utils.find_chat(chat_id, agent.chat_system);
  let chat' = {
    ...chat,
    pending_send_queue: q,
  };
  {
    ...agent,
    chat_system: ChatSystem.Utils.update_chat(chat', agent.chat_system),
  };
};

let with_compaction_in_flight =
    (~seq: int, agent: Agent.Model.t): Agent.Model.t => {
  let chat_id = agent.chat_system.current;
  {
    ...agent,
    compaction_in_progress: Some(chat_id),
    compaction_llm_seq: seq,
    awaiting_response: None,
  };
};

let run_update =
    (
      ~settings: Settings.t=Settings.Model.init,
      ~editor: CellEditor.Model.t=cell_editor(),
      action: Agent.Update.Action.t,
      agent: Agent.Model.t,
      scheduled: ref(list(Agent.Update.Action.t)),
    )
    : Agent.Model.t => {
  let (agent', _) =
    Agent.Update.update(action, agent, editor, settings, x =>
      scheduled := scheduled^ @ [x]
    );
  agent';
};

/** Drain [[schedule_action]] callbacks breadth-first until empty or step bound. */
let rec drain_scheduled =
        (
          ~settings: Settings.t=Settings.Model.init,
          ~editor: CellEditor.Model.t=cell_editor(),
          agent: Agent.Model.t,
          scheduled: ref(list(Agent.Update.Action.t)),
          ~max_rounds: int,
        )
        : Agent.Model.t =>
  if (max_rounds <= 0) {
    Alcotest.fail("drain_scheduled: exceeded max_rounds");
  } else {
    switch (scheduled^) {
    | [] => agent
    | actions =>
      scheduled := [];
      let agent' =
        List.fold_left(
          (ag, act) => run_update(~settings, ~editor, act, ag, scheduled),
          agent,
          actions,
        );
      drain_scheduled(
        ~settings,
        ~editor,
        agent',
        scheduled,
        ~max_rounds=max_rounds - 1,
      );
    };
  };

let linear_contents = (agent: Agent.Model.t, chat_id: Id.t): list(string) => {
  let chat = ChatSystem.Utils.find_chat(chat_id, agent.chat_system);
  Chat.Utils.linearize(chat) |> List.map((m: Message.Model.t) => m.content);
};

let count_substring = (needle: string, haystack: list(string)): int =>
  List.length(
    List.filter(c => StringUtil.plain_search(needle, c, 0) >= 0, haystack),
  );

let index_of_substring =
    (needle: string, haystack: list(string)): option(int) => {
  let rec go = (i: int, xs: list(string)): option(int) =>
    switch (xs) {
    | [] => None
    | [c, ...rest] =>
      if (StringUtil.plain_search(needle, c, 0) >= 0) {
        Some(i);
      } else {
        go(i + 1, rest);
      }
    };
  go(0, haystack);
};

let test_stop_sets_ignore_and_clears_awaiting = () => {
  let agent = with_busy_main(~seq=3, Agent.Utils.init());
  let chat_id = agent.chat_system.current;
  let scheduled = ref([]);
  let agent' =
    run_update(Agent.Update.Action.StopAgenticLoop, agent, scheduled);
  check(
    bool,
    "awaiting cleared",
    true,
    Option.is_none(agent'.awaiting_response),
  );
  check(
    bool,
    "pending_ignore_main matches flight",
    true,
    agent'.pending_ignore_main_reply_seq == Some(3),
  );
  let cs = linear_contents(agent', chat_id);
  check(
    bool,
    "cancel message present",
    true,
    List.exists(
      c => StringUtil.plain_search("Agent response cancelled", c, 0) >= 0,
      cs,
    ),
  );
};

let test_handle_llm_response_ignored_for_stopped_flight = () => {
  let agent = with_busy_main(~seq=2, Agent.Utils.init());
  let chat_id = agent.chat_system.current;
  let scheduled = ref([]);
  let after_stop =
    run_update(Agent.Update.Action.StopAgenticLoop, agent, scheduled);
  let n_before =
    List.length(
      List.filter(
        (m: Message.Model.t) =>
          switch (m.role) {
          | Message.Model.Agent(_) => true
          | _ => false
          },
        Chat.Utils.linearize(
          ChatSystem.Utils.find_chat(chat_id, after_stop.chat_system),
        ),
      ),
    );
  let late = mk_reply(~content="late_reply_should_not_appear", []);
  let scheduled2 = ref([]);
  let after_late =
    run_update(
      Agent.Update.Action.HandleLLMResponse(late, chat_id, 2, 0),
      after_stop,
      scheduled2,
    );
  check(
    bool,
    "ignore flag cleared after stale reply",
    true,
    Option.is_none(after_late.pending_ignore_main_reply_seq),
  );
  let msgs =
    Chat.Utils.linearize(
      ChatSystem.Utils.find_chat(chat_id, after_late.chat_system),
    );
  let n_after =
    List.length(
      List.filter(
        (m: Message.Model.t) =>
          switch (m.role) {
          | Message.Model.Agent(_) => true
          | _ => false
          },
        msgs,
      ),
    );
  check(
    bool,
    "no new agent message from ignored flight",
    true,
    n_after == n_before,
  );
  check(
    bool,
    "late content not in transcript",
    true,
    !
      List.exists(
        c =>
          StringUtil.plain_search("late_reply_should_not_appear", c, 0) >= 0,
        List.map((m: Message.Model.t) => m.content, msgs),
      ),
  );
};

let test_api_error_main_ignored_for_stopped_flight = () => {
  let agent = with_busy_main(~seq=4, Agent.Utils.init());
  let chat_id = agent.chat_system.current;
  let scheduled = ref([]);
  let after_stop =
    run_update(Agent.Update.Action.StopAgenticLoop, agent, scheduled);
  let err = Message.Utils.mk_api_failure_message("should not append");
  let scheduled2 = ref([]);
  let after_err =
    run_update(
      Agent.Update.Action.ApiErrorResponse(
        chat_id,
        err,
        Agent.MainRequest(4),
      ),
      after_stop,
      scheduled2,
    );
  check(
    bool,
    "ignore cleared",
    true,
    Option.is_none(after_err.pending_ignore_main_reply_seq),
  );
  let cs = linear_contents(after_err, chat_id);
  check(
    bool,
    "ignored API error content absent",
    true,
    !
      List.exists(
        c => StringUtil.plain_search("should not append", c, 0) >= 0,
        cs,
      ),
  );
};

let test_stop_then_flush_queue_sends_user_after_cancel = () => {
  let agent =
    with_chat_queue(
      with_busy_main(~seq=1, Agent.Utils.init()),
      ["queued_after_stop"],
    );
  let chat_id = agent.chat_system.current;
  let scheduled = ref([]);
  let after_stop =
    run_update(Agent.Update.Action.StopAgenticLoop, agent, scheduled);
  check(
    bool,
    "FlushPendingSend scheduled",
    true,
    List.mem(Agent.Update.Action.FlushPendingSend(chat_id), scheduled^),
  );
  let after_drain = drain_scheduled(after_stop, scheduled, ~max_rounds=8);
  let chat = ChatSystem.Utils.find_chat(chat_id, after_drain.chat_system);
  check(bool, "queue drained", true, chat.pending_send_queue == []);
  let cs = linear_contents(after_drain, chat_id);
  let i_cancel =
    index_of_substring("Agent response cancelled", cs)
    |> Option.value(~default=-1);
  let i_queued = index_of_substring("queued_after_stop", cs);
  check(bool, "cancel before queued user", true, i_cancel >= 0);
  switch (i_queued) {
  | None => Alcotest.fail("expected queued user message")
  | Some(iq) => check(bool, "ordering", true, i_cancel < iq)
  };
};

let test_send_while_busy_enqueues = () => {
  let agent = with_busy_main(~seq=1, Agent.Utils.init());
  let chat_id = agent.chat_system.current;
  let scheduled = ref([]);
  let after_send =
    run_update(
      Agent.Update.Action.SendMessage(
        Message.Utils.mk_user_message("queued_while_busy_token"),
        chat_id,
      ),
      agent,
      scheduled,
    );
  let chat = ChatSystem.Utils.find_chat(chat_id, after_send.chat_system);
  check(
    bool,
    "message queued not inline-appended as sole path",
    true,
    chat.pending_send_queue == ["queued_while_busy_token"],
  );
  check(bool, "no dispatch deferred while busy", true, scheduled^ == []);
  check(
    int,
    "queued message not appended to transcript",
    0,
    count_substring(
      "queued_while_busy_token",
      linear_contents(after_send, chat_id),
    ),
  );
};

/* Phase 1 of SendMessage appends the user message immediately (optimistic
   feedback); all dispatch work is deferred to DispatchSend. */
let test_send_appends_user_message_before_dispatch = () => {
  let agent = Agent.Utils.init();
  let chat_id = agent.chat_system.current;
  let scheduled = ref([]);
  let after_phase1 =
    run_update(
      Agent.Update.Action.SendMessage(
        Message.Utils.mk_user_message("optimistic_hello"),
        chat_id,
      ),
      agent,
      scheduled,
    );
  let cs = linear_contents(after_phase1, chat_id);
  check(
    int,
    "user message visible before deferred dispatch",
    1,
    count_substring("optimistic_hello", cs),
  );
  check(
    int,
    "dispatch has not run yet (no api-key failure)",
    0,
    count_substring("API key is required", cs),
  );
  check(
    bool,
    "dispatch pending for this chat",
    true,
    after_phase1.pending_dispatch_send == Some(chat_id),
  );
  check(
    bool,
    "DispatchSend deferred",
    true,
    List.mem(Agent.Update.Action.DispatchSend(chat_id), scheduled^),
  );
  check(
    bool,
    "not yet awaiting a response",
    true,
    Option.is_none(after_phase1.awaiting_response),
  );
};

let test_send_dispatches_exactly_once_after_drain = () => {
  let agent = Agent.Utils.init();
  let chat_id = agent.chat_system.current;
  let scheduled = ref([]);
  let after_phase1 =
    run_update(
      Agent.Update.Action.SendMessage(
        Message.Utils.mk_user_message("dispatch_once"),
        chat_id,
      ),
      agent,
      scheduled,
    );
  let after = drain_scheduled(after_phase1, scheduled, ~max_rounds=8);
  let cs = linear_contents(after, chat_id);
  /* Settings.Model.init has no API key, so each dispatch appends exactly
     one api-key failure: a single occurrence pins a single dispatch. */
  check(
    int,
    "dispatched exactly once",
    1,
    count_substring("API key is required", cs),
  );
  check(
    int,
    "user message appended once",
    1,
    count_substring("dispatch_once", cs),
  );
  check(
    bool,
    "dispatch flag cleared",
    true,
    Option.is_none(after.pending_dispatch_send),
  );
};

let test_stop_during_dispatch_gap_cancels_send = () => {
  let agent = Agent.Utils.init();
  let chat_id = agent.chat_system.current;
  let scheduled = ref([]);
  let after_phase1 =
    run_update(
      Agent.Update.Action.SendMessage(
        Message.Utils.mk_user_message("gap_message"),
        chat_id,
      ),
      agent,
      scheduled,
    );
  let after_stop =
    run_update(Agent.Update.Action.StopAgenticLoop, after_phase1, scheduled);
  check(
    bool,
    "pending dispatch cleared by Stop",
    true,
    Option.is_none(after_stop.pending_dispatch_send),
  );
  let after = drain_scheduled(after_stop, scheduled, ~max_rounds=8);
  let cs = linear_contents(after, chat_id);
  check(
    int,
    "stale DispatchSend did not fire",
    0,
    count_substring("API key is required", cs),
  );
  check(
    bool,
    "cancel line present",
    true,
    List.exists(
      c => StringUtil.plain_search("Agent response cancelled", c, 0) >= 0,
      cs,
    ),
  );
  check(
    bool,
    "not awaiting after stopped gap",
    true,
    Option.is_none(after.awaiting_response),
  );
};

let test_handle_compaction_reply_ignored_after_stop = () => {
  let agent = with_compaction_in_flight(~seq=2, Agent.Utils.init());
  let chat_id = agent.chat_system.current;
  let scheduled = ref([]);
  let after_stop =
    run_update(Agent.Update.Action.StopAgenticLoop, agent, scheduled);
  check(
    bool,
    "compaction cleared",
    true,
    Option.is_none(after_stop.compaction_in_progress),
  );
  check(
    bool,
    "pending_ignore_compaction",
    true,
    after_stop.pending_ignore_compaction_reply_seq == Some(2),
  );
  let summary = mk_reply(~content="phantom summary", []);
  let scheduled2 = ref([]);
  let after =
    run_update(
      Agent.Update.Action.HandleCompactionLLMReply(summary, chat_id, 2),
      after_stop,
      scheduled2,
    );
  check(
    bool,
    "ignore compaction cleared",
    true,
    Option.is_none(after.pending_ignore_compaction_reply_seq),
  );
  let cs = linear_contents(after, chat_id);
  check(
    bool,
    "no compaction summary content",
    true,
    !
      List.exists(
        c => StringUtil.plain_search("phantom summary", c, 0) >= 0,
        cs,
      ),
  );
};

let test_tool_allowed_in_mode_edit_allows_all = () => {
  check(
    bool,
    "Edit allows edit tool",
    true,
    Agent.Update.tool_allowed_in_mode(Edit, "update_definition"),
  );
  check(
    bool,
    "Edit allows workbench tool",
    true,
    Agent.Update.tool_allowed_in_mode(Edit, "create_new_task"),
  );
  check(
    bool,
    "Edit allows overlay tool",
    true,
    Agent.Update.tool_allowed_in_mode(Edit, "place_probe"),
  );
};

let test_tool_allowed_in_mode_plan_blocks_edit_only = () => {
  check(
    bool,
    "Plan blocks edit tool",
    false,
    Agent.Update.tool_allowed_in_mode(Plan, "update_definition"),
  );
  check(
    bool,
    "Plan allows workbench tool",
    true,
    Agent.Update.tool_allowed_in_mode(Plan, "create_new_task"),
  );
  check(
    bool,
    "Plan allows overlay tool",
    true,
    Agent.Update.tool_allowed_in_mode(Plan, "place_probe"),
  );
};

let test_tool_allowed_in_mode_converse_blocks_edit_workbench_overlay = () => {
  check(
    bool,
    "Converse blocks edit tool",
    false,
    Agent.Update.tool_allowed_in_mode(Converse, "update_definition"),
  );
  check(
    bool,
    "Converse blocks workbench tool",
    false,
    Agent.Update.tool_allowed_in_mode(Converse, "create_new_task"),
  );
  check(
    bool,
    "Converse blocks overlay tool",
    false,
    Agent.Update.tool_allowed_in_mode(Converse, "place_probe"),
  );
  check(
    bool,
    "Converse allows unknown/view tool",
    true,
    Agent.Update.tool_allowed_in_mode(Converse, "expand_binding"),
  );
};

let test_backoff_ms_exponential_formula = () => {
  check(
    float(0.0),
    "attempt 0 = 1000ms",
    1000.0,
    Agent.Update.backoff_ms(0),
  );
  check(
    float(0.0),
    "attempt 1 = 2000ms",
    2000.0,
    Agent.Update.backoff_ms(1),
  );
  check(
    float(0.0),
    "attempt 2 = 4000ms",
    4000.0,
    Agent.Update.backoff_ms(2),
  );
  check(
    float(0.0),
    "attempt 3 = 8000ms",
    8000.0,
    Agent.Update.backoff_ms(3),
  );
};

let test_stream_delta_dropped_when_flight_ignored = () => {
  let agent = Agent.Utils.init();
  let chat_id = agent.chat_system.current;
  let agent = {
    ...agent,
    main_llm_seq: 5,
    pending_ignore_main_reply_seq: Some(5),
    pending_assistant_content: "",
  };
  let scheduled = ref([]);
  let after =
    run_update(
      Agent.Update.Action.StreamDelta(
        chat_id,
        5,
        "should_not_land",
        "also_should_not_land",
      ),
      agent,
      scheduled,
    );
  check(
    string,
    "content unchanged when seq matches pending_ignore",
    "",
    after.pending_assistant_content,
  );
  check(
    string,
    "reasoning unchanged when seq matches pending_ignore",
    "",
    after.pending_assistant_reasoning,
  );
};

let test_stream_delta_accumulates_when_not_ignored = () => {
  let agent = Agent.Utils.init();
  let chat_id = agent.chat_system.current;
  let agent = {
    ...agent,
    main_llm_seq: 7,
    pending_ignore_main_reply_seq: None,
    pending_assistant_content: "pre_",
    pending_assistant_reasoning: "r_",
  };
  let scheduled = ref([]);
  let after =
    run_update(
      Agent.Update.Action.StreamDelta(chat_id, 7, "post", "eason"),
      agent,
      scheduled,
    );
  check(
    string,
    "content appended",
    "pre_post",
    after.pending_assistant_content,
  );
  check(
    string,
    "reasoning appended",
    "r_eason",
    after.pending_assistant_reasoning,
  );
};

let test_stream_delta_accumulates_on_seq_mismatch = () => {
  /* pending_ignore is set for a *different* flight_seq; current delta
     belongs to a fresh flight and should accumulate. */
  let agent = Agent.Utils.init();
  let chat_id = agent.chat_system.current;
  let agent = {
    ...agent,
    main_llm_seq: 9,
    pending_ignore_main_reply_seq: Some(8),
    pending_assistant_content: "",
  };
  let scheduled = ref([]);
  let after =
    run_update(
      Agent.Update.Action.StreamDelta(chat_id, 9, "live", ""),
      agent,
      scheduled,
    );
  check(
    string,
    "live delta accumulates despite stale ignore flag",
    "live",
    after.pending_assistant_content,
  );
};

/* ---- Jev navigation: pre-pass + modify_view (docs/notes/jev-nav) ----
   Every test swaps [AgentJev.select_view] for a fake, so no HTTP
   runs and nothing depends on JevNav's internals. */

let jev_settings = (~prepass: bool, ~view_tool: bool): Settings.t => {
  let settings = Settings.Model.init;
  {
    ...settings,
    agent_globals: {
      ...settings.agent_globals,
      jev_prepass: prepass,
      jev_view_tool: view_tool,
    },
  };
};

let editor_of = (code: string): CellEditor.Model.t =>
  switch (Parser.to_zipper(code, ~root=Exp)) {
  | Some(z) => CellEditor.Model.mk(Editor.Model.mk(z, ~root=Exp))
  | None => Alcotest.fail("failed to parse fixture")
  };

let mk_selection =
    (~intent: string, open_paths: option(list(string))): JevNav.selection => {
  open_paths: Option.value(~default=[], open_paths),
  metrics: {
    intent,
    requests: 1,
    questions: 0,
    input_tokens: 0,
    cost_usd: 0.0,
    latency_ms: 0.0,
    yes: [],
    closure_added: [],
    failed: Option.is_none(open_paths),
  },
};

/** Fake Jev: logs each intent into [calls] and hands [on_done] to
    [answer], which decides when (and whether) the selection lands. */
let with_fake_select =
    (
      ~calls: ref(list(string)),
      ~answer: (string, JevNav.selection => unit) => unit,
      body: unit => unit,
    )
    : unit => {
  let real = AgentJev.select_view^;
  AgentJev.select_view :=
    (
      (~api_key as _, ~max_tokens as _, ~intent, ~on_done, _z) => {
        calls := calls^ @ [intent];
        answer(intent, on_done);
      }
    );
  Fun.protect(~finally=() => AgentJev.select_view := real, body);
};

let answer_now = (open_paths, intent, on_done) =>
  on_done(mk_selection(~intent, open_paths));

let agent_view = (agent: Agent.Model.t): AgentContext.Model.t =>
  ChatSystem.Utils.find_chat(agent.chat_system.current, agent.chat_system).
    agent_view;

let tool_results = (agent: Agent.Model.t): list(AgentToolResult.tool_result) =>
  Chat.Utils.linearize(
    ChatSystem.Utils.find_chat(agent.chat_system.current, agent.chat_system),
  )
  |> List.filter_map((m: Message.Model.t) =>
       switch (m.role) {
       | ToolResult(tr) => Some(tr)
       | _ => None
       }
     );

let modify_view_reply =
    (~replace: option(bool)=?, intent: string): OpenRouter.Reply.Model.t =>
  mk_reply([
    OpenRouter.Reply.Model.{
      id: "call-1",
      name: "modify_view",
      args:
        `Assoc(
          [("intent", `String(intent))]
          @ (
            switch (replace) {
            | Some(b) => [("replace", `Bool(b))]
            | None => []
            }
          ),
        ),
    },
  ]);

let tool_names = (tools: list(API.Json.t)): list(string) =>
  List.filter_map(Agent.ToolUtils.get_name, tools);

let tools_for = (globals: Web.AgentGlobals.Model.t): list(string) =>
  tool_names(
    Agent.Update.enabled_tools(~globals, Agent.Utils.init().prompting),
  );

let with_jev_edit = (settings: Settings.t): Settings.t => {
  ...settings,
  agent_globals: {
    ...settings.agent_globals,
    jev_edit_tool: true,
  },
};

let test_modify_view_exposed_only_with_flag = () => {
  let off =
    tools_for(jev_settings(~prepass=false, ~view_tool=false).agent_globals);
  let on =
    tools_for(jev_settings(~prepass=false, ~view_tool=true).agent_globals);
  check(bool, "hidden when off", false, List.mem("modify_view", off));
  check(bool, "present when on", true, List.mem("modify_view", on));
  check(bool, "expand hidden when on", false, List.mem("expand", on));
  check(bool, "collapse hidden when on", false, List.mem("collapse", on));
  check(
    list(string),
    "off = on with modify_view swapped for expand/collapse (control unchanged)",
    List.filter(n => n != "expand" && n != "collapse", off),
    List.filter(n => n != "modify_view", on),
  );
  check(
    bool,
    "allowed in converse, as expand/collapse are in the control",
    true,
    List.mem(
      "modify_view",
      tools_for({
        ...jev_settings(~prepass=false, ~view_tool=true).agent_globals,
        session_mode: Converse,
      }),
    ),
  );
};

let edit_tool_names = Agent.Update.edit_tools_replaced_by_jev;

let test_jev_edit_arm_swaps_edit_tools = () => {
  let control = jev_settings(~prepass=false, ~view_tool=false);
  let off = tools_for(control.agent_globals);
  let on = tools_for(with_jev_edit(control).agent_globals);
  check(
    list(string),
    "control = every declared tool except the Jev-backed ones",
    List.filter_map(Agent.ToolUtils.get_name, CompositionUtils.Public.tools)
    |> List.filter(n =>
         !List.mem(n, ["modify_view", "jev_edit", "add_tests"])
       ),
    off,
  );
  check(bool, "jev_edit present when on", true, List.mem("jev_edit", on));
  check(bool, "add_tests present when on", true, List.mem("add_tests", on));
  check(
    bool,
    "add_tests hidden when off",
    false,
    List.mem("add_tests", off),
  );
  check(
    bool,
    "every direct edit tool hidden when on",
    true,
    List.for_all(n => !List.mem(n, on), edit_tool_names),
  );
  check(
    list(string),
    "off = on with jev_edit swapped for the edit tools",
    List.filter(n => !List.mem(n, edit_tool_names), off),
    List.filter(n => n != "jev_edit" && n != "add_tests", on),
  );
  let both =
    tools_for(
      with_jev_edit(jev_settings(~prepass=false, ~view_tool=true)).
        agent_globals,
    );
  check(
    bool,
    "arms combine: modify_view and jev_edit, no expand or update_definition",
    true,
    List.mem("modify_view", both)
    && List.mem("jev_edit", both)
    && !List.mem("expand", both)
    && !List.mem("update_definition", both),
  );
  check(
    bool,
    "Plan mode still blocks jev_edit (it edits code)",
    false,
    List.mem(
      "jev_edit",
      tools_for({
        ...with_jev_edit(control).agent_globals,
        session_mode: Plan,
      }),
    ),
  );
};

let test_edit_tools_refused_in_edit_arm = () => {
  let settings = with_jev_edit(Settings.Model.init);
  let agent = Agent.Utils.init();
  let editor = editor_of("let a = 1 in a").editor;
  let refused = action =>
    switch (
      Agent.ToolCallHandler.update(
        ~settings,
        action,
        agent,
        editor,
        agent.chat_system.current,
      )
    ) {
    | Ok(_) => false
    | Error(_) => true
    };
  check(
    bool,
    "update_definition refused",
    true,
    refused(EditorAction(Update(Definition, "a", "2"))),
  );
  check(
    bool,
    "boundary insert refused",
    true,
    refused(InsertAtProgramBoundary(After, "let c = 3 in c")),
  );
};

/* ---- Jev edit arm (V3): jev_edit through a fake [AgentJev.edit_code] ---- */

let zipper_of = (code: string): Zipper.t =>
  switch (Parser.to_zipper(code, ~root=Exp)) {
  | Some(z) => z
  | None => Alcotest.fail("failed to parse: " ++ code)
  };

let mk_edit_metrics =
    (
      ~filled: int,
      ~holes_seen: int,
      ~escalated: list(JevEdit.hole)=[],
      ~error: option(string)=?,
      request: JevEdit.request,
    )
    : JevEdit.metrics => {
  intent: request.intent,
  path: request.path,
  rounds: 1,
  holes_seen,
  filled,
  escalated,
  requests: 1,
  input_tokens: 0,
  cost_usd: 0.0,
  latency_ms: 0.0,
  failed: Option.is_some(error),
  error,
};

/** Fake Jev editor: logs each (request, context) and lets [answer] build
    the outcome from the program it was handed. */
let with_fake_edit =
    (
      ~calls: ref(list((JevEdit.request, string))),
      ~answer: (JevEdit.request, Zipper.t) => JevEdit.outcome,
      body: unit => unit,
    )
    : unit => {
  let real = AgentJev.edit_code^;
  AgentJev.edit_code :=
    (
      (~api_key as _, ~context, ~request, ~on_done, z) => {
        calls := calls^ @ [(request, context)];
        on_done(answer(request, z));
      }
    );
  Fun.protect(~finally=() => AgentJev.edit_code := real, body);
};

let jev_edit_call = (~id="call-1", path: string, sketch: string) =>
  OpenRouter.Reply.Model.{
    id,
    name: "jev_edit",
    args:
      `Assoc([
        ("path", `String(path)),
        ("sketch", `String(sketch)),
        ("intent", `String("set " ++ path ++ " to " ++ sketch)),
      ]),
  };

/* Runs a reply through HandleLLMResponse and the Resolved re-entry, keeping
   the editor (drain_scheduled drops it). */
let run_jev_reply =
    (
      ~settings: Settings.t,
      ~code: string,
      tool_calls: list(OpenRouter.Reply.Model.tool_call),
    )
    : (Agent.Model.t, Updated.t(CellEditor.Model.t)) => {
  let editor = editor_of(code);
  let agent = Agent.Utils.init();
  let scheduled = ref([]);
  let schedule = a => scheduled := scheduled^ @ [a];
  let (agent, _) =
    Agent.Update.update(
      HandleLLMResponse(
        mk_reply(tool_calls),
        agent.chat_system.current,
        agent.main_llm_seq,
        0,
      ),
      agent,
      editor,
      settings,
      schedule,
    );
  switch (
    List.find_opt(
      fun
      | Agent.Update.Action.HandleLLMResponseResolved(_) => true
      | _ => false,
      scheduled^,
    )
  ) {
  | Some(resolved) =>
    Agent.Update.update(resolved, agent, editor, settings, schedule)
  | None => Alcotest.fail("expected HandleLLMResponseResolved")
  };
};

let editor_text = (updated: Updated.t(CellEditor.Model.t)): string =>
  AgentJev.program_text(updated.model.editor.editor.state.zipper);

/* Str, not StringUtil.plain_search: the snapshot holds multibyte `⋱`. */
let contains = (needle: string, haystack: string): bool =>
  switch (Str.search_forward(Str.regexp_string(needle), haystack, 0)) {
  | _ => true
  | exception Not_found => false
  };

let test_jev_edit_applies_outcome = () => {
  let settings = with_jev_edit(Settings.Model.init);
  let calls = ref([]);
  with_fake_edit(
    ~calls,
    ~answer=
      (request, _z) =>
        {
          /* Body changes too: the snapshot folds [a] but shows the body. */
          zipper: zipper_of("let a = 2 in a + 2"),
          metrics: mk_edit_metrics(~filled=1, ~holes_seen=1, request),
        },
    () => {
      let (agent, updated) =
        run_jev_reply(
          ~settings,
          ~code="let a = 1 in a",
          [jev_edit_call("a", "?")],
        );
      switch (calls^) {
      | [(request, context)] =>
        check(string, "path parsed", "a", request.path);
        check(list(string), "names default empty", [], request.names);
        check(
          bool,
          "context is the program view",
          true,
          contains("let a", context),
        );
      | _ => Alcotest.fail("expected one Jev edit call")
      };
      check(
        bool,
        "editor holds Jev's program",
        true,
        contains("let a = 2", editor_text(updated)),
      );
      check(bool, "counts as an edit", true, updated.is_edit);
      switch (tool_results(agent)) {
      | [tr] =>
        check(bool, "success", true, tr.success);
        check(string, "one-line result", "filled 1/1 holes", tr.content);
        check(bool, "diff recorded", true, Option.is_some(tr.diff));
      | _ => Alcotest.fail("expected one tool result")
      };
      let chat =
        ChatSystem.Utils.find_chat(
          agent.chat_system.current,
          agent.chat_system,
        );
      check(
        bool,
        "snapshot refreshed from the new program",
        true,
        contains(
          "a + 2",
          Option.fold(
            ~none="",
            ~some=(m: Message.Model.t) => m.content,
            chat.context,
          ),
        ),
      );
    },
  );
};

let test_jev_edit_reports_unfilled_holes = () => {
  let settings = with_jev_edit(Settings.Model.init);
  let hole = (hole_id, expected_type): JevEdit.hole => {
    hole_id,
    expected_type,
    candidates: [],
  };
  with_fake_edit(
    ~calls=ref([]),
    ~answer=
      (request, z) =>
        {
          zipper: z,
          metrics:
            mk_edit_metrics(
              ~filled=1,
              ~holes_seen=3,
              ~escalated=[hole("h2", "Int"), hole("h3", "[Int]")],
              request,
            ),
        },
    () => {
      let (agent, _) =
        run_jev_reply(
          ~settings,
          ~code="let a = 1 in a",
          [jev_edit_call("a", "?")],
        );
      switch (tool_results(agent)) {
      | [tr] =>
        check(
          string,
          "escalated holes with types",
          "filled 1/3 holes · unfilled: h2 : Int, h3 : [Int]",
          tr.content,
        )
      | _ => Alcotest.fail("expected one tool result")
      };
    },
  );
};

let test_jev_edit_failure_leaves_program = () => {
  let settings = with_jev_edit(Settings.Model.init);
  with_fake_edit(
    ~calls=ref([]),
    ~answer=
      (request, z) =>
        {
          zipper: z,
          metrics:
            mk_edit_metrics(
              ~filled=0,
              ~holes_seen=0,
              ~error="bad sketch",
              request,
            ),
        },
    () => {
      let (agent, updated) =
        run_jev_reply(
          ~settings,
          ~code="let a = 1 in a",
          [jev_edit_call("a", "?")],
        );
      check(
        bool,
        "program unchanged",
        true,
        contains("let a = 1", editor_text(updated)),
      );
      switch (tool_results(agent)) {
      | [tr] =>
        check(bool, "failed", false, tr.success);
        check(
          bool,
          "reason surfaced",
          true,
          contains("bad sketch", tr.content),
        );
      | _ => Alcotest.fail("expected one tool result")
      };
    },
  );
};

let with_builds = (settings: Settings.t): Settings.t => {
  ...settings,
  agent_globals: {
    ...settings.agent_globals,
    jev_edit_builds: true,
  },
};

/* Parameter names of the jev_edit schema the model is offered. */
let jev_edit_params = (settings: Settings.t): (list(string), list(string)) => {
  let tool =
    Agent.Update.enabled_tools(
      ~globals=settings.agent_globals,
      Agent.Utils.init().prompting,
    )
    |> List.find(tool => Agent.ToolUtils.get_name(tool) == Some("jev_edit"));
  let params =
    Option.bind(API.Json.dot("function", tool), API.Json.dot("parameters"))
    |> Option.get;
  let keys =
    switch (API.Json.dot("properties", params)) {
    | Some(`Assoc(props)) => List.map(fst, props)
    | _ => []
    };
  let required =
    switch (API.Json.dot("required", params)) {
    | Some(`List(names)) => List.filter_map(API.Json.str, names)
    | _ => []
    };
  (keys, required);
};

let test_jev_mode_is_spec_only = () => {
  let globals =
    Web.AgentGlobals.Update.update(
      SetJevMode(true), Web.AgentGlobals.init(), _ =>
      ()
    );
  check(
    list(bool),
    "prepass, view, edit, builds all on",
    [true, true, true, true],
    [
      globals.jev_prepass,
      globals.jev_view_tool,
      globals.jev_edit_tool,
      globals.jev_edit_builds,
    ],
  );
  check(bool, "reads as on", true, Web.AgentGlobals.jev_mode_on(globals));
  check(
    bool,
    "builds alone off ⇒ jev mode reads off",
    false,
    Web.AgentGlobals.jev_mode_on({
      ...globals,
      jev_edit_builds: false,
    }),
  );
};

let fib_program = "let fib = fun n -> if n <= 1 then n else fib(n - 1) + fib(n - 2) in fib(10)";

let add_tests_on = (~settings: Settings.t, code: string, tests: list(string)) => {
  let agent = Agent.Utils.init();
  Agent.ToolCallHandler.update(
    ~settings,
    AddTests(tests),
    agent,
    editor_of(code).editor,
    agent.chat_system.current,
  );
};

let test_add_tests_before_final_expression = () => {
  let settings = with_jev_edit(Settings.Model.init);
  switch (
    add_tests_on(~settings, fib_program, ["fib(0) == 0", "fib(10) == 55"])
  ) {
  | Ok((_, cws)) =>
    let text = AgentJev.program_text(cws.editor.state.zipper);
    let lines =
      String.split_on_char('\n', text)
      |> List.map(String.trim)
      |> List.filter(l => l != "");
    check(
      list(string),
      "tests sit between the last binding and the final expression",
      ["test fib(0) == 0 end;", "test fib(10) == 55 end;", "fib(10)"],
      lines
      |> List.rev
      |> (ls => [List.nth(ls, 2), List.nth(ls, 1), List.hd(ls)]),
    );
    check(
      bool,
      "definition untouched",
      true,
      contains("fib(n - 1) + fib(n - 2)", text),
    );
  | Error(_) => Alcotest.fail("add_tests should succeed on the fib program")
  };
};

let test_add_tests_refused = () => {
  let refused = r =>
    switch (r) {
    | Ok(_) => false
    | Error(_) => true
    };
  check(
    bool,
    "edit arm off",
    true,
    refused(
      add_tests_on(~settings=Settings.Model.init, fib_program, ["true"]),
    ),
  );
  check(
    bool,
    "static error veto",
    true,
    refused(
      add_tests_on(
        ~settings=with_jev_edit(Settings.Model.init),
        fib_program,
        ["no_such_name == 1"],
      ),
    ),
  );
};

let test_jev_builds_drops_sketch_from_schema = () => {
  let sketch_arm = with_jev_edit(Settings.Model.init);
  let (keys, required) = jev_edit_params(sketch_arm);
  check(bool, "sketch offered", true, List.mem("sketch", keys));
  check(bool, "sketch required", true, List.mem("sketch", required));
  let (keys, required) = jev_edit_params(with_builds(sketch_arm));
  check(bool, "no sketch when builds", false, List.mem("sketch", keys));
  check(bool, "sketch not required", false, List.mem("sketch", required));
  check(
    list(string),
    "signature replaces sketch; rest shared",
    ["path", "signature", "names", "literals", "intent"],
    keys,
  );
};

let test_jev_builds_alone_keeps_control_tools = () => {
  let json = (settings: Settings.t) =>
    Agent.Update.enabled_tools(
      ~globals=settings.agent_globals,
      Agent.Utils.init().prompting,
    )
    |> List.map(API.Json.to_string);
  check(
    list(string),
    "builds without the edit arm: control tool JSON, byte for byte",
    json(Settings.Model.init),
    json(with_builds(Settings.Model.init)),
  );
};

let test_jev_builds_ignores_sent_sketch = () => {
  let settings = with_builds(with_jev_edit(Settings.Model.init));
  let calls = ref([]);
  with_fake_edit(
    ~calls,
    ~answer=
      (request, _z) =>
        {
          zipper: zipper_of("let a = 2 in a"),
          metrics: mk_edit_metrics(~filled=3, ~holes_seen=3, request),
        },
    () => {
      let (agent, _) =
        run_jev_reply(
          ~settings,
          ~code="let a = 1 in a",
          [jev_edit_call("a", "1 + ?")],
        );
      switch (calls^) {
      | [(request, _)] =>
        check(string, "Jev builds from scratch", "", request.sketch)
      | _ => Alcotest.fail("expected one Jev edit call")
      };
      switch (tool_results(agent)) {
      | [tr] =>
        check(bool, "applied", true, tr.success);
        check(
          string,
          "result line unchanged",
          "filled 3/3 holes",
          tr.content,
        );
      | _ => Alcotest.fail("expected one tool result")
      };
    },
  );
};

let test_jev_edits_chain_in_one_reply = () => {
  let settings = with_jev_edit(Settings.Model.init);
  /* Each fake edit rewrites the first "= 1" of the program it is handed, so
     the second only sees "b = 1" if it ran on the first's result. */
  with_fake_edit(
    ~calls=ref([]),
    ~answer=
      (request, z) =>
        {
          zipper:
            zipper_of(
              Str.replace_first(
                Str.regexp_string("= 1"),
                "= " ++ request.sketch,
                AgentJev.program_text(z),
              ),
            ),
          metrics: mk_edit_metrics(~filled=1, ~holes_seen=1, request),
        },
    () => {
      let (agent, updated) =
        run_jev_reply(
          ~settings,
          ~code="let a = 1 in let b = 1 in a + b",
          [jev_edit_call("a", "2"), jev_edit_call(~id="call-2", "b", "3")],
        );
      let text = editor_text(updated);
      check(bool, "first edit kept", true, contains("a = 2", text));
      check(bool, "second edit applied", true, contains("b = 3", text));
      check(
        bool,
        "both succeeded",
        true,
        List.for_all(
          (tr: AgentToolResult.tool_result) => tr.success,
          tool_results(agent),
        ),
      );
    },
  );
};

let test_expand_refused_in_jev_arm = () => {
  let settings = jev_settings(~prepass=false, ~view_tool=true);
  let agent = Agent.Utils.init();
  let chat_id = agent.chat_system.current;
  let editor =
    CodeWithStatics.Model.mk(Editor.Model.mk(Zipper.init(), ~root=Exp));
  switch (
    Agent.ToolCallHandler.update(
      ~settings,
      AgentContextAction(Expand(["a"])),
      agent,
      editor,
      chat_id,
    )
  ) {
  | Ok(_) => Alcotest.fail("expand must be refused when jev_view_tool is on")
  | Error(_) => ()
  };
};

let test_modify_view_applies_selection = () => {
  let settings = jev_settings(~prepass=false, ~view_tool=true);
  let calls = ref([]);
  with_fake_select(
    ~calls,
    ~answer=answer_now(Some(["a", "M/inner"])),
    () => {
      let agent = Agent.Utils.init();
      let chat_id = agent.chat_system.current;
      let scheduled = ref([]);
      let agent =
        run_update(
          ~settings,
          Agent.Update.Action.HandleLLMResponse(
            modify_view_reply("fix a"),
            chat_id,
            agent.main_llm_seq,
            0,
          ),
          agent,
          scheduled,
        );
      let agent = drain_scheduled(~settings, agent, scheduled, ~max_rounds=8);
      check(list(string), "Jev asked once", ["fix a"], calls^);
      check(
        list(string),
        "suggested set",
        ["a", "M/inner"],
        agent_view(agent).suggested_paths,
      );
      switch (tool_results(agent)) {
      | [tr] =>
        check(bool, "success", true, tr.success);
        check(
          string,
          "one-line result",
          "open: a, M/inner · added: a, M/inner",
          tr.content,
        );
      | _ => Alcotest.fail("expected exactly one tool result")
      };
    },
  );
};

/* Runs one modify_view over a view where Jev already opened [x]. */
let modify_view_over_x = (~replace: option(bool)=?, ()) => {
  let settings = jev_settings(~prepass=false, ~view_tool=true);
  let result = ref(None);
  with_fake_select(
    ~calls=ref([]),
    ~answer=answer_now(Some(["a", "x"])),
    () => {
      let agent = Agent.Utils.init();
      let chat_id = agent.chat_system.current;
      let agent = {
        ...agent,
        chat_system:
          ChatSystem.Update.update(
            ChatAction(AgentContextAction(SetSuggested(["x"])), chat_id),
            agent.chat_system,
          )
          |> ChatSystem.Update.get,
      };
      let scheduled = ref([]);
      let agent =
        run_update(
          ~settings,
          Agent.Update.Action.HandleLLMResponse(
            modify_view_reply(~replace?, "fix a"),
            chat_id,
            agent.main_llm_seq,
            0,
          ),
          agent,
          scheduled,
        );
      let agent = drain_scheduled(~settings, agent, scheduled, ~max_rounds=8);
      result :=
        Some((
          agent_view(agent).suggested_paths,
          List.map(
            (tr: AgentToolResult.tool_result) => tr.content,
            tool_results(agent),
          ),
        ));
    },
  );
  Option.get(result^);
};

let test_modify_view_adds_by_default = () => {
  let (suggested, results) = modify_view_over_x();
  check(list(string), "union, no duplicates", ["x", "a"], suggested);
  check(list(string), "result", ["open: x, a · added: a"], results);
};

let test_modify_view_replace_resets = () => {
  let (suggested, _) = modify_view_over_x(~replace=true, ());
  check(list(string), "exactly Jev's picks", ["a", "x"], suggested);
  let (suggested, _) = modify_view_over_x(~replace=false, ());
  check(list(string), "explicit false adds", ["x", "a"], suggested);
};

let test_modify_view_flag_off_fails_without_jev = () => {
  let calls = ref([]);
  with_fake_select(
    ~calls,
    ~answer=answer_now(Some(["a"])),
    () => {
      let agent = Agent.Utils.init();
      let scheduled = ref([]);
      let agent =
        run_update(
          Agent.Update.Action.HandleLLMResponse(
            modify_view_reply("fix a"),
            agent.chat_system.current,
            agent.main_llm_seq,
            0,
          ),
          agent,
          scheduled,
        );
      check(list(string), "no Jev call", [], calls^);
      check(
        list(string),
        "view unchanged",
        [],
        agent_view(agent).suggested_paths,
      );
      check(
        bool,
        "tool failed",
        true,
        List.for_all(
          (tr: AgentToolResult.tool_result) => !tr.success,
          tool_results(agent),
        ),
      );
    },
  );
};

let test_stop_during_modify_view_resolution = () => {
  let settings = jev_settings(~prepass=false, ~view_tool=true);
  let held = ref(None);
  with_fake_select(
    ~calls=ref([]),
    ~answer=
      (intent, on_done) =>
        held := Some(() => answer_now(Some(["a"]), intent, on_done)),
    () => {
      let agent = with_busy_main(~seq=1, Agent.Utils.init());
      let chat_id = agent.chat_system.current;
      let scheduled = ref([]);
      let agent =
        run_update(
          ~settings,
          Agent.Update.Action.HandleLLMResponse(
            modify_view_reply("fix a"),
            chat_id,
            1,
            0,
          ),
          agent,
          scheduled,
        );
      let agent =
        run_update(
          ~settings,
          Agent.Update.Action.StopAgenticLoop,
          agent,
          scheduled,
        );
      Option.get(held^, ());
      let agent = drain_scheduled(~settings, agent, scheduled, ~max_rounds=8);
      check(int, "no tool ran", 0, List.length(tool_results(agent)));
      check(
        list(string),
        "view unchanged",
        [],
        agent_view(agent).suggested_paths,
      );
      check(
        bool,
        "not awaiting",
        true,
        Option.is_none(agent.awaiting_response),
      );
    },
  );
};

let prepass_code = "let a = 1 in let b = 2 in a + b";

let test_prepass_sets_view_before_request = () => {
  let settings = jev_settings(~prepass=true, ~view_tool=false);
  let editor = editor_of(prepass_code);
  let calls = ref([]);
  with_fake_select(
    ~calls,
    ~answer=answer_now(Some(["a"])),
    () => {
      let agent = Agent.Utils.init();
      let chat_id = agent.chat_system.current;
      let scheduled = ref([]);
      let agent =
        run_update(
          ~settings,
          ~editor,
          Agent.Update.Action.SendMessage(
            Message.Utils.mk_user_message("fix a"),
            chat_id,
          ),
          agent,
          scheduled,
        );
      let agent =
        drain_scheduled(~settings, ~editor, agent, scheduled, ~max_rounds=8);
      let chat = ChatSystem.Utils.find_chat(chat_id, agent.chat_system);
      let snapshot =
        Option.map((m: Message.Model.t) => m.content, chat.context)
        |> Option.value(~default="");
      check(
        list(string),
        "Jev asked with the user message",
        ["fix a"],
        calls^,
      );
      check(
        list(string),
        "suggested set",
        ["a"],
        agent_view(agent).suggested_paths,
      );
      check(
        bool,
        "snapshot shows a",
        true,
        StringUtil.plain_search("let a = 1", snapshot, 0) >= 0,
      );
      check(
        bool,
        "snapshot folds b",
        true,
        StringUtil.plain_search("let b = ⋱", snapshot, 0) >= 0,
      );
      check(
        int,
        "request dispatched once",
        1,
        count_substring(
          "API key is required",
          linear_contents(agent, chat_id),
        ),
      );
    },
  );
};

/* The wire a send produces, for a fixed user message and editor. */
let wire_after_send =
    (~settings: Settings.t, message: Message.Model.t)
    : list(OpenRouter.Message.Model.t) => {
  let editor = editor_of(prepass_code);
  let agent = Agent.Utils.init();
  let chat_id = agent.chat_system.current;
  let scheduled = ref([]);
  let agent =
    run_update(
      ~settings,
      ~editor,
      Agent.Update.Action.SendMessage(message, chat_id),
      agent,
      scheduled,
    );
  let agent =
    drain_scheduled(~settings, ~editor, agent, scheduled, ~max_rounds=8);
  Chat.Utils.api_messages_for_openrouter(
    ChatSystem.Utils.find_chat(chat_id, agent.chat_system),
  );
};

let test_flags_off_no_jev_and_same_wire = () => {
  let message = Message.Utils.mk_user_message("fix a");
  let calls = ref([]);
  with_fake_select(
    ~calls,
    ~answer=answer_now(None),
    () => {
      let off =
        wire_after_send(
          ~settings=jev_settings(~prepass=false, ~view_tool=false),
          message,
        );
      check(list(string), "flags off: no Jev call", [], calls^);
      let failed_prepass =
        wire_after_send(
          ~settings=jev_settings(~prepass=true, ~view_tool=false),
          message,
        );
      check(list(string), "pre-pass ran", ["fix a"], calls^);
      check(
        bool,
        "failed pre-pass leaves the wire byte-identical",
        true,
        off == failed_prepass,
      );
    },
  );
};

let test_stop_during_prepass = () => {
  let settings = jev_settings(~prepass=true, ~view_tool=false);
  let held = ref(None);
  with_fake_select(
    ~calls=ref([]),
    ~answer=
      (intent, on_done) =>
        held := Some(() => answer_now(Some(["a"]), intent, on_done)),
    () => {
      let agent = Agent.Utils.init();
      let chat_id = agent.chat_system.current;
      let scheduled = ref([]);
      let agent =
        run_update(
          ~settings,
          Agent.Update.Action.SendMessage(
            Message.Utils.mk_user_message("fix a"),
            chat_id,
          ),
          agent,
          scheduled,
        );
      let agent = drain_scheduled(~settings, agent, scheduled, ~max_rounds=8);
      check(
        bool,
        "send held (busy) while Jev runs",
        true,
        agent.pending_dispatch_send == Some(chat_id),
      );
      let agent =
        run_update(
          ~settings,
          Agent.Update.Action.StopAgenticLoop,
          agent,
          scheduled,
        );
      Option.get(held^, ());
      let agent = drain_scheduled(~settings, agent, scheduled, ~max_rounds=8);
      let cs = linear_contents(agent, chat_id);
      check(
        int,
        "request never sent",
        0,
        count_substring("API key is required", cs),
      );
      check(
        int,
        "cancel line",
        1,
        count_substring("Agent response cancelled", cs),
      );
      check(
        list(string),
        "view unchanged",
        [],
        agent_view(agent).suggested_paths,
      );
    },
  );
};

let test_stale_prepass_cannot_hijack_next_send = () => {
  let settings = jev_settings(~prepass=true, ~view_tool=false);
  let held = ref([]);
  with_fake_select(
    ~calls=ref([]),
    ~answer=(intent, on_done) => held := held^ @ [(intent, on_done)],
    () => {
      let agent = Agent.Utils.init();
      let chat_id = agent.chat_system.current;
      let send = (text, agent, scheduled) =>
        run_update(
          ~settings,
          Agent.Update.Action.SendMessage(
            Message.Utils.mk_user_message(text),
            chat_id,
          ),
          agent,
          scheduled,
        );
      let scheduled = ref([]);
      let agent = send("first", agent, scheduled);
      let agent = drain_scheduled(~settings, agent, scheduled, ~max_rounds=8);
      let agent =
        run_update(
          ~settings,
          Agent.Update.Action.StopAgenticLoop,
          agent,
          scheduled,
        );
      let agent = drain_scheduled(~settings, agent, scheduled, ~max_rounds=8);
      /* Second send is in its phase gap when the first pre-pass lands. */
      let agent = send("second", agent, ref([]));
      let (_, first_on_done) = List.hd(held^);
      first_on_done(mk_selection(~intent="first", Some(["a"])));
      let agent = drain_scheduled(~settings, agent, scheduled, ~max_rounds=8);
      check(
        bool,
        "second send still pending",
        true,
        agent.pending_dispatch_send == Some(chat_id),
      );
      check(
        list(string),
        "stale view dropped",
        [],
        agent_view(agent).suggested_paths,
      );
      check(
        int,
        "nothing dispatched",
        0,
        count_substring(
          "API key is required",
          linear_contents(agent, chat_id),
        ),
      );
    },
  );
};

let tests = [
  (
    "AgentControlFlow",
    [
      test_case(
        "Jev: modify_view exposed only when jev_view_tool is on",
        `Quick,
        test_modify_view_exposed_only_with_flag,
      ),
      test_case(
        "Jev: expand refused when jev_view_tool is on",
        `Quick,
        test_expand_refused_in_jev_arm,
      ),
      test_case(
        "Jev edit: arm swaps edit tools for jev_edit; control unchanged",
        `Quick,
        test_jev_edit_arm_swaps_edit_tools,
      ),
      test_case(
        "Jev edit: direct edit tools refused when the arm is on",
        `Quick,
        test_edit_tools_refused_in_edit_arm,
      ),
      test_case(
        "Jev edit: outcome becomes the editor state like an edit",
        `Quick,
        test_jev_edit_applies_outcome,
      ),
      test_case(
        "Jev edit: unfilled holes listed with expected types",
        `Quick,
        test_jev_edit_reports_unfilled_holes,
      ),
      test_case(
        "Jev edit: failed edit leaves the program and reports why",
        `Quick,
        test_jev_edit_failure_leaves_program,
      ),
      test_case(
        "Jev mode: turns on prepass, view, edit and builds",
        `Quick,
        test_jev_mode_is_spec_only,
      ),
      test_case(
        "add_tests: tests land before the final expression",
        `Quick,
        test_add_tests_before_final_expression,
      ),
      test_case(
        "add_tests: refused when the arm is off or on static errors",
        `Quick,
        test_add_tests_refused,
      ),
      test_case(
        "Jev builds: jev_edit schema drops sketch only when on",
        `Quick,
        test_jev_builds_drops_sketch_from_schema,
      ),
      test_case(
        "Jev builds: alone leaves the control tool list byte-identical",
        `Quick,
        test_jev_builds_alone_keeps_control_tools,
      ),
      test_case(
        "Jev builds: a sketch the model sends anyway is ignored",
        `Quick,
        test_jev_builds_ignores_sent_sketch,
      ),
      test_case(
        "Jev edit: two edits in one reply chain",
        `Quick,
        test_jev_edits_chain_in_one_reply,
      ),
      test_case(
        "Jev: modify_view applies the selection and returns one line",
        `Quick,
        test_modify_view_applies_selection,
      ),
      test_case(
        "Jev: modify_view adds to the view by default",
        `Quick,
        test_modify_view_adds_by_default,
      ),
      test_case(
        "Jev: modify_view replace=true resets the view",
        `Quick,
        test_modify_view_replace_resets,
      ),
      test_case(
        "Jev: modify_view with flag off fails without calling Jev",
        `Quick,
        test_modify_view_flag_off_fails_without_jev,
      ),
      test_case(
        "Jev: Stop while modify_view resolves drops the reply",
        `Quick,
        test_stop_during_modify_view_resolution,
      ),
      test_case(
        "Jev: pre-pass sets the view before the request",
        `Quick,
        test_prepass_sets_view_before_request,
      ),
      test_case(
        "Jev: flags off make no Jev call; failed pre-pass keeps the wire",
        `Quick,
        test_flags_off_no_jev_and_same_wire,
      ),
      test_case(
        "Jev: Stop during pre-pass cancels the held send",
        `Quick,
        test_stop_during_prepass,
      ),
      test_case(
        "Jev: stale pre-pass cannot hijack the next send",
        `Quick,
        test_stale_prepass_cannot_hijack_next_send,
      ),
      test_case(
        "tool_allowed_in_mode: Edit allows edit/workbench/overlay tools",
        `Quick,
        test_tool_allowed_in_mode_edit_allows_all,
      ),
      test_case(
        "tool_allowed_in_mode: Plan blocks edit, allows workbench/overlay",
        `Quick,
        test_tool_allowed_in_mode_plan_blocks_edit_only,
      ),
      test_case(
        "tool_allowed_in_mode: Converse blocks edit/workbench/overlay",
        `Quick,
        test_tool_allowed_in_mode_converse_blocks_edit_workbench_overlay,
      ),
      test_case(
        "backoff_ms: exponential 1000 * 2^n for attempts 0..3",
        `Quick,
        test_backoff_ms_exponential_formula,
      ),
      test_case(
        "StreamDelta: dropped when flight_seq matches pending_ignore_main",
        `Quick,
        test_stream_delta_dropped_when_flight_ignored,
      ),
      test_case(
        "StreamDelta: accumulates content+reasoning when not ignored",
        `Quick,
        test_stream_delta_accumulates_when_not_ignored,
      ),
      test_case(
        "StreamDelta: accumulates when pending_ignore is for a different seq",
        `Quick,
        test_stream_delta_accumulates_on_seq_mismatch,
      ),
      test_case(
        "StopAgenticLoop: clears awaiting, sets pending_ignore_main for current flight",
        `Quick,
        test_stop_sets_ignore_and_clears_awaiting,
      ),
      test_case(
        "HandleLLMResponse: matching flight after Stop is ignored (no agent text)",
        `Quick,
        test_handle_llm_response_ignored_for_stopped_flight,
      ),
      test_case(
        "ApiErrorResponse MainRequest: matching flight after Stop does not append error",
        `Quick,
        test_api_error_main_ignored_for_stopped_flight,
      ),
      test_case(
        "Send while busy enqueues into pending_send_queue",
        `Quick,
        test_send_while_busy_enqueues,
      ),
      test_case(
        "SendMessage phase 1: user message appended before deferred dispatch",
        `Quick,
        test_send_appends_user_message_before_dispatch,
      ),
      test_case(
        "SendMessage: draining deferred DispatchSend dispatches exactly once",
        `Quick,
        test_send_dispatches_exactly_once_after_drain,
      ),
      test_case(
        "Stop in phase-1/phase-2 gap cancels the pending dispatch",
        `Quick,
        test_stop_during_dispatch_gap_cancels_send,
      ),
      test_case(
        "Stop then flush: cancel line before queued user send",
        `Quick,
        test_stop_then_flush_queue_sends_user_after_cancel,
      ),
      test_case(
        "HandleCompactionLLMReply: ignored after compaction Stop",
        `Quick,
        test_handle_compaction_reply_ignored_after_stop,
      ),
    ],
  ),
];
