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
      action: Agent.Update.Action.t,
      agent: Agent.Model.t,
      scheduled: ref(list(Agent.Update.Action.t)),
    )
    : Agent.Model.t => {
  let settings = Settings.Model.init;
  let editor = cell_editor();
  let (agent', _) =
    Agent.Update.update(action, agent, editor, settings, x =>
      scheduled := scheduled^ @ [x]
    );
  agent';
};

/** Drain [[schedule_action]] callbacks breadth-first until empty or step bound. */
let rec drain_scheduled =
        (
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
          (ag, act) => run_update(act, ag, scheduled),
          agent,
          actions,
        );
      drain_scheduled(agent', scheduled, ~max_rounds=max_rounds - 1);
    };
  };

let linear_contents = (agent: Agent.Model.t, chat_id: Id.t): list(string) => {
  let chat = ChatSystem.Utils.find_chat(chat_id, agent.chat_system);
  Chat.Utils.linearize(chat) |> List.map((m: Message.Model.t) => m.content);
};

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
        Message.Utils.mk_user_message("hold"),
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
    chat.pending_send_queue == ["hold"],
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

let tests = [
  (
    "AgentControlFlow",
    [
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
