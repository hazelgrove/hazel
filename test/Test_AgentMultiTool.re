/** Integration-style tests for [[Agent.Update]] handling of assistant replies
    that contain **multiple** tool_calls: calls run in order until the first
    **failure**; later calls are not executed and produce **skipped** ToolResult
    rows. On an all-success batch, each call runs and one follow-up dispatch runs
    (no HTTP when API key is unset).

    Isolated run (aligned with the Makefile test target): after a test build,
    `./run_tests test 'AgentMultiTool' -q` or the same `node --stack-size=8192
    --require _build/default/test/idb_stub.js ...` invocation as in the Makefile.
    Full structural tool suite: `./run_tests test 'AgentTools' -q`. */
open Alcotest;
open Haz3lcore;
open Util_web;
open Web;

let yo_assoc = pairs => `Assoc(pairs);

let tool_call = (id: string, name: string, args: Yojson.Safe.t) =>
  OpenRouter.Reply.Model.{
    id,
    name,
    args,
  };

let mk_reply =
    (~content: string="", tool_calls: list(OpenRouter.Reply.Model.tool_call))
    : OpenRouter.Reply.Model.t => {
  content,
  tool_calls,
  usage: None,
  reasoning: None,
};

let tool_result_names_in_order =
    (model: Agent.Model.t, chat_id: Id.t): list(string) => {
  let chat = ChatSystem.Utils.find_chat(chat_id, model.chat_system);
  Chat.Utils.linearize(chat)
  |> List.filter_map((m: Message.Model.t) =>
       switch (m.role) {
       | ToolResult(tr) => Some(tr.tool_call.name)
       | _ => None
       }
     );
};

let tool_results_in_order =
    (model: Agent.Model.t, chat_id: Id.t): list(AgentToolResult.tool_result) => {
  let chat = ChatSystem.Utils.find_chat(chat_id, model.chat_system);
  Chat.Utils.linearize(chat)
  |> List.filter_map((m: Message.Model.t) =>
       switch (m.role) {
       | ToolResult(tr) => Some(tr)
       | _ => None
       }
     );
};

let run_handle_llm_response =
    (
      reply: OpenRouter.Reply.Model.t,
      ~agent: Agent.Model.t,
      ~cell_editor: CellEditor.Model.t,
      ~settings: Settings.t,
    )
    : (Agent.Model.t, list(Agent.Update.Action.t)) => {
  let chat_id = agent.chat_system.current;
  let scheduled = ref([]);
  let schedule_action = (a: Agent.Update.Action.t) =>
    scheduled := scheduled^ @ [a];
  let (agent', _cell_ed) =
    Agent.Update.update(
      Agent.Update.Action.HandleLLMResponse(
        reply,
        chat_id,
        agent.main_llm_seq,
        0,
      ),
      agent,
      cell_editor,
      settings,
      schedule_action,
    );
  (agent', scheduled^);
};

let empty_args = yo_assoc([]);

let test_two_workbench_tools_two_results = () => {
  let settings = Settings.Model.init;
  let agent = Agent.Utils.init();
  let chat_id = agent.chat_system.current;
  let cell_editor =
    CellEditor.Model.mk(Editor.Model.mk(Zipper.init(), ~root=Exp));
  let reply =
    mk_reply(
      ~content="ok",
      [
        tool_call("call-1", "unset_active_task", empty_args),
        tool_call("call-2", "unset_active_subtask", empty_args),
      ],
    );
  let (agent', scheduled) =
    run_handle_llm_response(reply, ~agent, ~cell_editor, ~settings);
  let names = tool_result_names_in_order(agent', chat_id);
  check(int, "two ToolResult messages", 2, List.length(names));
  check(string, "first tool name", "unset_active_task", List.nth(names, 0));
  check(
    string,
    "second tool name",
    "unset_active_subtask",
    List.nth(names, 1),
  );
  /* No API key: dispatch appends settings error, must not chain another LLM response action. */
  let has_llm_response =
    List.exists(
      fun
      | Agent.Update.Action.HandleLLMResponse(_) => true
      | _ => false,
      scheduled,
    );
  check(
    bool,
    "no nested HandleLLMResponse from scheduler",
    false,
    has_llm_response,
  );
};

let test_three_tools_preserve_order = () => {
  let settings = Settings.Model.init;
  let agent = Agent.Utils.init();
  let chat_id = agent.chat_system.current;
  let cell_editor =
    CellEditor.Model.mk(Editor.Model.mk(Zipper.init(), ~root=Exp));
  let task_payload =
    yo_assoc([
      (
        "task",
        yo_assoc([
          ("title", `String("mt-order")),
          ("description", `String("desc")),
          (
            "subtasks",
            `List([
              yo_assoc([
                ("title", `String("s1")),
                ("description", `String("d1")),
              ]),
            ]),
          ),
        ]),
      ),
    ]);
  let reply =
    mk_reply(
      ~content="batch",
      [
        tool_call("a", "create_new_task", task_payload),
        tool_call(
          "b",
          "mark_active_subtask_complete",
          yo_assoc([("summary", `String("s1 done"))]),
        ),
        tool_call(
          "c",
          "mark_active_task_complete",
          yo_assoc([("summary", `String("all done"))]),
        ),
      ],
    );
  let (agent', _) =
    run_handle_llm_response(reply, ~agent, ~cell_editor, ~settings);
  let names = tool_result_names_in_order(agent', chat_id);
  check(int, "three ToolResult messages", 3, List.length(names));
  check(string, "tool 1", "create_new_task", List.nth(names, 0));
  check(
    string,
    "tool 2",
    "mark_active_subtask_complete",
    List.nth(names, 1),
  );
  check(string, "tool 3", "mark_active_task_complete", List.nth(names, 2));
};

let test_invalid_tool_skips_following_tools = () => {
  let settings = Settings.Model.init;
  let agent = Agent.Utils.init();
  let chat_id = agent.chat_system.current;
  let cell_editor =
    CellEditor.Model.mk(Editor.Model.mk(Zipper.init(), ~root=Exp));
  let reply =
    mk_reply(
      ~content="partial",
      [
        tool_call("bad", "not_a_real_hazel_tool", empty_args),
        tool_call("ok", "unset_active_task", empty_args),
      ],
    );
  let (agent', _) =
    run_handle_llm_response(reply, ~agent, ~cell_editor, ~settings);
  let trs = tool_results_in_order(agent', chat_id);
  check(int, "two ToolResult rows", 2, List.length(trs));
  let first = List.nth(trs, 0);
  let second = List.nth(trs, 1);
  check(string, "first name", "not_a_real_hazel_tool", first.tool_call.name);
  check(bool, "first executed and failed", false, first.success);
  check(bool, "first not skipped", false, first.skipped);
  check(string, "second name", "unset_active_task", second.tool_call.name);
  check(bool, "second skipped", true, second.skipped);
  check(bool, "second not success", false, second.success);
};

let test_success_then_fail_then_skip = () => {
  let settings = Settings.Model.init;
  let agent = Agent.Utils.init();
  let chat_id = agent.chat_system.current;
  let cell_editor =
    CellEditor.Model.mk(Editor.Model.mk(Zipper.init(), ~root=Exp));
  let reply =
    mk_reply(
      ~content="batch",
      [
        tool_call("t1", "unset_active_task", empty_args),
        tool_call("t2", "not_a_real_hazel_tool", empty_args),
        tool_call("t3", "unset_active_subtask", empty_args),
      ],
    );
  let (agent', _) =
    run_handle_llm_response(reply, ~agent, ~cell_editor, ~settings);
  let trs = tool_results_in_order(agent', chat_id);
  check(int, "three ToolResult rows", 3, List.length(trs));
  check(bool, "first ok", true, List.nth(trs, 0).success);
  check(bool, "first not skipped", false, List.nth(trs, 0).skipped);
  check(bool, "second failed", false, List.nth(trs, 1).success);
  check(bool, "third skipped", true, List.nth(trs, 2).skipped);
};

let test_single_tool_call_regression = () => {
  let settings = Settings.Model.init;
  let agent = Agent.Utils.init();
  let chat_id = agent.chat_system.current;
  let cell_editor =
    CellEditor.Model.mk(Editor.Model.mk(Zipper.init(), ~root=Exp));
  let reply =
    mk_reply(
      ~content="one",
      [tool_call("only", "unset_active_task", empty_args)],
    );
  let (agent', _) =
    run_handle_llm_response(reply, ~agent, ~cell_editor, ~settings);
  let names = tool_result_names_in_order(agent', chat_id);
  check(int, "single ToolResult", 1, List.length(names));
  check(string, "name", "unset_active_task", List.hd(names));
};

let tests = [
  (
    "AgentMultiTool",
    [
      test_case(
        "HandleLLMResponse: two tool_calls => two ToolResult messages in order",
        `Quick,
        test_two_workbench_tools_two_results,
      ),
      test_case(
        "HandleLLMResponse: three tool_calls preserve execution order",
        `Quick,
        test_three_tools_preserve_order,
      ),
      test_case(
        "HandleLLMResponse: invalid tool then rest skipped (not executed)",
        `Quick,
        test_invalid_tool_skips_following_tools,
      ),
      test_case(
        "HandleLLMResponse: success then fail => remaining tools skipped",
        `Quick,
        test_success_then_fail_then_skip,
      ),
      test_case(
        "HandleLLMResponse: single tool_call still works",
        `Quick,
        test_single_tool_call_regression,
      ),
    ],
  ),
];
