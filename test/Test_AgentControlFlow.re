/** Tests for [[Agent.Agent.Update]]: Stop / flight sequence ignore, send queue flush,
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
};

let cell_editor = () => CellEditor.Model.mk(Editor.Model.mk(Zipper.init()));

let with_busy_main =
    (~seq: int, agent: Agent.Agent.Model.t): Agent.Agent.Model.t => {
  let chat_id = agent.chat_system.current;
  {
    ...agent,
    awaiting_response: Some(chat_id),
    main_llm_seq: seq,
  };
};

let with_chat_queue =
    (agent: Agent.Agent.Model.t, q: list(string)): Agent.Agent.Model.t => {
  let chat_id = agent.chat_system.current;
  let chat = Agent.ChatSystem.Utils.find_chat(chat_id, agent.chat_system);
  let chat' = {
    ...chat,
    pending_send_queue: q,
  };
  {
    ...agent,
    chat_system: Agent.ChatSystem.Utils.update_chat(chat', agent.chat_system),
  };
};

let with_compaction_in_flight =
    (~seq: int, agent: Agent.Agent.Model.t): Agent.Agent.Model.t => {
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
      action: Agent.Agent.Update.Action.t,
      agent: Agent.Agent.Model.t,
      scheduled: ref(list(Agent.Agent.Update.Action.t)),
    )
    : Agent.Agent.Model.t => {
  let settings = Settings.Model.init;
  let editor = cell_editor();
  let (agent', _) =
    Agent.Agent.Update.update(action, agent, editor, settings, x =>
      scheduled := scheduled^ @ [x]
    );
  agent';
};

/** Drain [[schedule_action]] callbacks breadth-first until empty or step bound. */
let rec drain_scheduled =
        (
          agent: Agent.Agent.Model.t,
          scheduled: ref(list(Agent.Agent.Update.Action.t)),
          ~max_rounds: int,
        )
        : Agent.Agent.Model.t =>
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

let linear_contents =
    (agent: Agent.Agent.Model.t, chat_id: Id.t): list(string) => {
  let chat = Agent.ChatSystem.Utils.find_chat(chat_id, agent.chat_system);
  Agent.Chat.Utils.linearize(chat)
  |> List.map((m: Agent.Message.Model.t) => m.content);
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
  let agent = with_busy_main(~seq=3, Agent.Agent.Utils.init());
  let chat_id = agent.chat_system.current;
  let scheduled = ref([]);
  let agent' =
    run_update(Agent.Agent.Update.Action.StopAgenticLoop, agent, scheduled);
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
  let agent = with_busy_main(~seq=2, Agent.Agent.Utils.init());
  let chat_id = agent.chat_system.current;
  let scheduled = ref([]);
  let after_stop =
    run_update(Agent.Agent.Update.Action.StopAgenticLoop, agent, scheduled);
  let n_before =
    List.length(
      List.filter(
        (m: Agent.Message.Model.t) =>
          switch (m.role) {
          | Agent.Message.Model.Agent(_) => true
          | _ => false
          },
        Agent.Chat.Utils.linearize(
          Agent.ChatSystem.Utils.find_chat(chat_id, after_stop.chat_system),
        ),
      ),
    );
  let late = mk_reply(~content="late_reply_should_not_appear", []);
  let scheduled2 = ref([]);
  let after_late =
    run_update(
      Agent.Agent.Update.Action.HandleLLMResponse(late, chat_id, 2),
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
    Agent.Chat.Utils.linearize(
      Agent.ChatSystem.Utils.find_chat(chat_id, after_late.chat_system),
    );
  let n_after =
    List.length(
      List.filter(
        (m: Agent.Message.Model.t) =>
          switch (m.role) {
          | Agent.Message.Model.Agent(_) => true
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
        List.map((m: Agent.Message.Model.t) => m.content, msgs),
      ),
  );
};

let test_api_error_main_ignored_for_stopped_flight = () => {
  let agent = with_busy_main(~seq=4, Agent.Agent.Utils.init());
  let chat_id = agent.chat_system.current;
  let scheduled = ref([]);
  let after_stop =
    run_update(Agent.Agent.Update.Action.StopAgenticLoop, agent, scheduled);
  let err = Agent.Message.Utils.mk_api_failure_message("should not append");
  let scheduled2 = ref([]);
  let after_err =
    run_update(
      Agent.Agent.Update.Action.ApiErrorResponse(
        chat_id,
        err,
        Agent.Agent.MainRequest(4),
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
      with_busy_main(~seq=1, Agent.Agent.Utils.init()),
      ["queued_after_stop"],
    );
  let chat_id = agent.chat_system.current;
  let scheduled = ref([]);
  let after_stop =
    run_update(Agent.Agent.Update.Action.StopAgenticLoop, agent, scheduled);
  check(
    bool,
    "FlushPendingSend scheduled",
    true,
    List.mem(
      Agent.Agent.Update.Action.FlushPendingSend(chat_id),
      scheduled^,
    ),
  );
  let after_drain = drain_scheduled(after_stop, scheduled, ~max_rounds=8);
  let chat =
    Agent.ChatSystem.Utils.find_chat(chat_id, after_drain.chat_system);
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
  let agent = with_busy_main(~seq=1, Agent.Agent.Utils.init());
  let chat_id = agent.chat_system.current;
  let scheduled = ref([]);
  let after_send =
    run_update(
      Agent.Agent.Update.Action.SendMessage(
        Agent.Message.Utils.mk_user_message("hold"),
        chat_id,
      ),
      agent,
      scheduled,
    );
  let chat =
    Agent.ChatSystem.Utils.find_chat(chat_id, after_send.chat_system);
  check(
    bool,
    "message queued not inline-appended as sole path",
    true,
    chat.pending_send_queue == ["hold"],
  );
};

let test_handle_compaction_reply_ignored_after_stop = () => {
  let agent = with_compaction_in_flight(~seq=2, Agent.Agent.Utils.init());
  let chat_id = agent.chat_system.current;
  let scheduled = ref([]);
  let after_stop =
    run_update(Agent.Agent.Update.Action.StopAgenticLoop, agent, scheduled);
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
      Agent.Agent.Update.Action.HandleCompactionLLMReply(summary, chat_id, 2),
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

let tests = [
  (
    "AgentControlFlow",
    [
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
