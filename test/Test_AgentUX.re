/** Unit tests for coding-agent UX: slash commands, compaction dialogue slicing,
    CompactionPrompt assembly, OpenRouter chat response parsing, context-meter math,
    workbench completion semantics.
    (Structural tool-call editing tests: [[Test_AgentTools]]; multi-tool LLM replies: [[Test_AgentMultiTool]].) */
open Alcotest;
open Util;
open Haz3lcore;
open Web;

let check_string = (name: string, expected: string, actual: string) =>
  check(string, name, expected, actual);

let check_int = (name: string, expected: int, actual: int) =>
  check(int, name, expected, actual);

let check_bool = (name: string, expected: bool, actual: bool) =>
  check(bool, name, expected, actual);

/* -------------------------------------------------------------------------- */
/* Slash commands (Agent.ChatSlashCommands) */

let slash_command_tests = [
  test_case(
    "ChatSlashCommands.filtered: empty filter lists all commands alphabetically",
    `Quick,
    () => {
      let xs = Agent.ChatSlashCommands.filtered("");
      check(int, "count", 1, List.length(xs));
      switch (xs) {
      | [(name, desc), ..._] =>
        check_string("name", "compact", name);
        check_bool("desc non-empty", true, String.length(desc) > 0);
      | [] => fail("expected at least one command")
      };
    },
  ),
  test_case(
    "ChatSlashCommands.filtered: prefix matches case-insensitively",
    `Quick,
    () => {
      let xs = Agent.ChatSlashCommands.filtered("COM");
      check(int, "one match", 1, List.length(xs));
      switch (xs) {
      | [(name, _), ..._] => check_string("name", "compact", name)
      | [] => fail("expected match")
      };
    },
  ),
  test_case(
    "ChatSlashCommands.filtered: no match returns empty",
    `Quick,
    () => {
      let xs = Agent.ChatSlashCommands.filtered("zzz");
      check(int, "empty", 0, List.length(xs));
    },
  ),
];

/* -------------------------------------------------------------------------- */
/* Slash menu derivation (ChatSystem.Utils.derive_slash_menu_from_content) */

let slash_menu_tests = [
  test_case(
    "derive_slash_menu: plain text does not open menu",
    `Quick,
    () => {
      let r =
        Agent.ChatSystem.Utils.derive_slash_menu_from_content(
          ~prev=None,
          "hello",
        );
      check_bool("no slash", true, Option.is_none(r));
    },
  ),
  test_case(
    "derive_slash_menu: '/' opens menu with empty filter token", `Quick, () => {
    switch (
      Agent.ChatSystem.Utils.derive_slash_menu_from_content(~prev=None, "/")
    ) {
    | None => fail("expected Some")
    | Some(sm) =>
      check_string("filter", "", sm.filter);
      check_int("selected_index", 0, sm.selected_index);
    }
  }),
  test_case("derive_slash_menu: '/compact' sets filter token", `Quick, () => {
    switch (
      Agent.ChatSystem.Utils.derive_slash_menu_from_content(
        ~prev=None,
        "/compact",
      )
    ) {
    | None => fail("expected Some")
    | Some(sm) =>
      check_string("filter", "compact", sm.filter);
      check_int("selected_index", 0, sm.selected_index);
    }
  }),
  test_case(
    "derive_slash_menu: space after slash closes menu",
    `Quick,
    () => {
      let r =
        Agent.ChatSystem.Utils.derive_slash_menu_from_content(
          ~prev=None,
          "/foo bar",
        );
      check_bool("space closes", true, Option.is_none(r));
    },
  ),
  test_case(
    "SaveTextBoxContent + SlashMenuAdjustSelection cycles selection",
    `Quick,
    () => {
      let cs =
        Agent.ChatSystem.Utils.init(~system_prompt="p", ~dev_notes="d");
      let cs =
        switch (
          Agent.ChatSystem.Update.update(
            Agent.ChatSystem.Update.Action.SaveTextBoxContent("/"),
            cs,
          )
        ) {
        | Ok(m) => m
        | Error(_) => fail("SaveTextBoxContent failed")
        };
      switch (cs.ui.slash_menu) {
      | None => fail("slash menu should be open")
      | Some(sm0) =>
        check_int("initial index", 0, sm0.selected_index);
        let cs2 =
          switch (
            Agent.ChatSystem.Update.update(
              Agent.ChatSystem.Update.Action.SlashMenuAdjustSelection(1),
              cs,
            )
          ) {
          | Ok(m) => m
          | Error(_) => fail("SlashMenuAdjustSelection failed")
          };
        switch (cs2.ui.slash_menu) {
        | None => fail("menu should stay open")
        | Some(sm1) =>
          /* Single command => modulo 1 keeps index 0 */
          check_int("index after down", 0, sm1.selected_index)
        };
      };
    },
  ),
];

/* -------------------------------------------------------------------------- */
/* Compaction dialogue slice (Agent.Chat.Utils.dialogue_slice_for_compaction_summary) */

let dialogue_slice_tests = [
  test_case(
    "dialogue_slice: fresh chat (prompt + dev only) yields empty slice",
    `Quick,
    () => {
      let chat = Agent.Chat.Utils.init(~system_prompt="sp", ~dev_notes="dn");
      let slice =
        Agent.Chat.Utils.dialogue_slice_for_compaction_summary(chat);
      check(int, "empty slice", 0, List.length(slice));
    },
  ),
  test_case(
    "dialogue_slice: after user message, slice is that message only",
    `Quick,
    () => {
      let chat = Agent.Chat.Utils.init(~system_prompt="sp", ~dev_notes="dn");
      let u = Agent.Message.Utils.mk_user_message("hello");
      let chat = Agent.Chat.Utils.append(u, chat);
      let slice =
        Agent.Chat.Utils.dialogue_slice_for_compaction_summary(chat);
      check(int, "one message", 1, List.length(slice));
      switch (List.hd(slice).role) {
      | User => ()
      | _ => fail("expected User")
      };
      check_string("content", "hello", List.hd(slice).content);
    },
  ),
  test_case(
    "dialogue_slice: after compaction summary, slice includes summary and later turns",
    `Quick,
    () => {
      let chat = Agent.Chat.Utils.init(~system_prompt="sp", ~dev_notes="dn");
      let sum =
        Agent.Message.Utils.mk_compaction_summary(
          ~method="test-method",
          "prior summary body",
        );
      let chat = Agent.Chat.Utils.append(sum, chat);
      let u = Agent.Message.Utils.mk_user_message("after compact");
      let chat = Agent.Chat.Utils.append(u, chat);
      let slice =
        Agent.Chat.Utils.dialogue_slice_for_compaction_summary(chat);
      check(int, "summary + post-summary", 2, List.length(slice));
      switch (List.hd(slice).role) {
      | Agent.Message.Model.System(Agent.Message.Model.CompactionSummary(m)) =>
        check_string("method", "test-method", m)
      | _ => fail("expected CompactionSummary first in slice")
      };
      check_string(
        "prior body",
        "prior summary body",
        List.hd(slice).content,
      );
      check_string(
        "after content",
        "after compact",
        List.nth(slice, 1).content,
      );
    },
  ),
];

/* -------------------------------------------------------------------------- */
/* Chat.Utils.context_meter_prompt_tokens & messages_for_openrouter */

let tok = (n: int): OpenRouter.Reply.Model.usage =>
  OpenRouter.Reply.Model.{
    prompt_tokens: n,
    completion_tokens: 0,
    total_tokens: n,
  };

let chat_context_meter_tests = [
  test_case(
    "context_meter_prompt_tokens: last agent usage when no compaction",
    `Quick,
    () => {
      let chat = Agent.Chat.Utils.init(~system_prompt="sp", ~dev_notes="dn");
      let chat =
        Agent.Chat.Utils.append(
          Agent.Message.Utils.mk_user_message("u"),
          chat,
        );
      let chat =
        Agent.Chat.Utils.append(
          Agent.Message.Utils.mk_agent_message("a", Some(tok(42))),
          chat,
        );
      check(
        bool,
        "Some(42)",
        true,
        Agent.Chat.Utils.context_meter_prompt_tokens(chat) == Some(42),
      );
    },
  ),
  test_case(
    "context_meter_prompt_tokens: None when last agent is before latest compaction",
    `Quick,
    () => {
      let chat = Agent.Chat.Utils.init(~system_prompt="sp", ~dev_notes="dn");
      let chat =
        Agent.Chat.Utils.append(
          Agent.Message.Utils.mk_agent_message("old", Some(tok(10))),
          chat,
        );
      let chat =
        Agent.Chat.Utils.append(
          Agent.Message.Utils.mk_compaction_summary(~method="m", "sum"),
          chat,
        );
      let chat =
        Agent.Chat.Utils.append(
          Agent.Message.Utils.mk_user_message("after"),
          chat,
        );
      check(
        bool,
        "no meter until post-compaction agent reply",
        true,
        Agent.Chat.Utils.context_meter_prompt_tokens(chat) == None,
      );
    },
  ),
  test_case(
    "context_meter_prompt_tokens: Some after agent follows compaction summary",
    `Quick,
    () => {
      let chat = Agent.Chat.Utils.init(~system_prompt="sp", ~dev_notes="dn");
      let chat =
        Agent.Chat.Utils.append(
          Agent.Message.Utils.mk_compaction_summary(~method="m", "sum"),
          chat,
        );
      let chat =
        Agent.Chat.Utils.append(
          Agent.Message.Utils.mk_agent_message("fresh", Some(tok(99))),
          chat,
        );
      check(
        bool,
        "Some(99)",
        true,
        Agent.Chat.Utils.context_meter_prompt_tokens(chat) == Some(99),
      );
    },
  ),
];

/* -------------------------------------------------------------------------- */
/* Agent workbench — MarkActiveTaskComplete folds incomplete subtasks */

let agent_workbench_tests = [
  test_case(
    "MarkActiveTaskComplete auto-completes remaining subtasks",
    `Quick,
    () => {
      let apply_wb = (model, action) =>
        switch (AgentWorkbench.Update.update(~model, ~action)) {
        | AgentWorkbench.Update.Action.Success(m) => m
        | AgentWorkbench.Update.Action.Failure(msg) => fail(msg)
        };
      let model0 = AgentWorkbench.Utils.MainUtils.init();
      let s1 =
        AgentWorkbench.Utils.SubtaskUtils.mk(~title="s1", ~description="a");
      let s2 =
        AgentWorkbench.Utils.SubtaskUtils.mk(~title="s2", ~description="b");
      let task =
        AgentWorkbench.Utils.TaskUtils.mk(
          ~title="Parent",
          ~description="p",
          ~subtasks=[s1, s2],
        );
      let ba_create =
        AgentWorkbench.Update.Action.BackendAction(
          AgentWorkbench.Update.Action.BackendAction.CreateNewTask(task),
        );
      let model1 = apply_wb(model0, ba_create);
      let ba_done =
        AgentWorkbench.Update.Action.BackendAction(
          AgentWorkbench.Update.Action.BackendAction.MarkActiveTaskComplete(
            "finished",
          ),
        );
      let model2 = apply_wb(model1, ba_done);
      let t =
        AgentWorkbench.Utils.TaskUtils.find_task(model2, "Parent")
        |> OptUtil.get_or_fail("task Parent");
      let subs = AgentWorkbench.Utils.TaskUtils.ordered_subtasks_of(t);
      check(
        bool,
        "both subtasks completed",
        true,
        List.for_all(AgentWorkbench.Utils.SubtaskUtils.is_completed, subs),
      );
      check(
        bool,
        "parent task completed",
        true,
        Option.is_some(t.completion_info),
      );
    },
  ),
];

let chat_messages_for_openrouter_tests = [
  test_case(
    "messages_for_openrouter: keeps prompt+dev then suffix from latest compaction",
    `Quick,
    () => {
      let chat = Agent.Chat.Utils.init(~system_prompt="sp", ~dev_notes="dn");
      let chat =
        Agent.Chat.Utils.append(
          Agent.Message.Utils.mk_compaction_summary(~method="meth", "body"),
          chat,
        );
      let chat =
        Agent.Chat.Utils.append(
          Agent.Message.Utils.mk_user_message("post"),
          chat,
        );
      let ms = Agent.Chat.Utils.messages_for_openrouter(chat);
      check(int, "prompt+dev+suffix", 4, List.length(ms));
      switch (List.nth(ms, 0).role) {
      | Agent.Message.Model.System(Agent.Message.Model.Prompt) => ()
      | _ => fail("expected Prompt first")
      };
      switch (List.nth(ms, 1).role) {
      | Agent.Message.Model.System(Agent.Message.Model.DeveloperNotes) => ()
      | _ => fail("expected DeveloperNotes second")
      };
      let has_compact =
        List.exists(
          (m: Agent.Message.Model.t) =>
            switch (m.role) {
            | Agent.Message.Model.System(
                Agent.Message.Model.CompactionSummary(_),
              ) =>
              true
            | _ => false
            },
          ms,
        );
      check_bool("includes compaction summary", true, has_compact);
      check_string("post user last", "post", List.nth(ms, 3).content);
    },
  ),
  test_case(
    "messages_for_openrouter: no compaction returns full linear transcript",
    `Quick,
    () => {
      let chat = Agent.Chat.Utils.init(~system_prompt="sp", ~dev_notes="dn");
      let chat =
        Agent.Chat.Utils.append(
          Agent.Message.Utils.mk_user_message("only"),
          chat,
        );
      let ms = Agent.Chat.Utils.messages_for_openrouter(chat);
      check(int, "three messages", 3, List.length(ms));
      check_string("user tail", "only", List.nth(ms, 2).content);
    },
  ),
];

/* -------------------------------------------------------------------------- */
/* CompactionPrompt.mk_system_prompt */

let compaction_prompt_tests = [
  test_case(
    "CompactionPrompt.mk_system_prompt includes dev notes and preamble markers",
    `Quick,
    () => {
      let s =
        CompactionPrompt.mk_system_prompt(
          ~agent_system_prompt="SHORT_AGENT_PROMPT",
          ~dev_notes="DEV_NOTES_LINE",
        );
      let has = (needle, hay) =>
        StringUtil.plain_search(needle, hay, 0) >= 0;
      check_bool("has preamble tag", true, has("compactionSummarizer", s));
      check_bool(
        "has agent excerpt header",
        true,
        has("## Agent system prompt", s),
      );
      check_bool("embeds agent prompt", true, has("SHORT_AGENT_PROMPT", s));
      check_bool("embeds dev notes", true, has("DEV_NOTES_LINE", s));
    },
  ),
  test_case(
    "CompactionPrompt: long agent prompt is abbreviated with notice",
    `Quick,
    () => {
      let long =
        String.make(
          CompactionPrompt.compaction_system_prompt_max_chars + 50,
          'x',
        );
      let s =
        CompactionPrompt.mk_system_prompt(
          ~agent_system_prompt=long,
          ~dev_notes="dn",
        );
      let has = (needle, hay) =>
        StringUtil.plain_search(needle, hay, 0) >= 0;
      check_bool("truncation notice", true, has("truncated for length", s));
      check_bool(
        "long combined prompt (abbreviated agent section)",
        true,
        String.length(s) > 12_000,
      );
    },
  ),
];

/* -------------------------------------------------------------------------- */
/* OpenRouter.Utils.handle_chat — assistant content shapes */

let json_of_string = (s: string): API.Json.t => Yojson.Safe.from_string(s);

let handle_chat_reply_content = (json: API.Json.t): string =>
  switch (OpenRouter.Utils.handle_chat(Some(json))) {
  | None => fail("handle_chat returned None")
  | Some(OpenRouter.Model.Error(_)) => fail("unexpected API error")
  | Some(OpenRouter.Model.Reply(r)) => r.content
  };

let openrouter_tests = [
  test_case(
    "handle_chat: string content",
    `Quick,
    () => {
      let j =
        json_of_string(
          {|{"choices":[{"message":{"role":"assistant","content":"hello world"}}],"usage":{"prompt_tokens":1,"completion_tokens":2,"total_tokens":3}}|},
        );
      check_string("content", "hello world", handle_chat_reply_content(j));
    },
  ),
  test_case(
    "handle_chat: array of text parts",
    `Quick,
    () => {
      let j =
        json_of_string(
          {|{"choices":[{"message":{"role":"assistant","content":[{"type":"text","text":"partA"},{"type":"text","text":"partB"}]}}],"usage":{"prompt_tokens":1,"completion_tokens":1,"total_tokens":2}}|},
        );
      check_string(
        "concatenated",
        "partApartB",
        handle_chat_reply_content(j),
      );
    },
  ),
  test_case(
    "handle_chat: empty string content uses reasoning when present",
    `Quick,
    () => {
      let j =
        json_of_string(
          {|{"choices":[{"message":{"role":"assistant","content":"","reasoning":"think here"}}],"usage":{"prompt_tokens":1,"completion_tokens":1,"total_tokens":2}}|},
        );
      check_string(
        "reasoning fallback",
        "think here",
        handle_chat_reply_content(j),
      );
    },
  ),
];

/* -------------------------------------------------------------------------- */
/* AgentGlobals.effective_context_meter_limit */

let context_meter_tests = [
  test_case(
    "effective_context_meter_limit: 131072 -> 80% rounded then capped at 100k",
    `Quick,
    () => {
    check_int(
      "131072",
      100_000,
      AgentGlobals.effective_context_meter_limit(131072),
    )
  }),
  test_case(
    "effective_context_meter_limit: under cap keeps 80% rounded to 1k steps",
    `Quick,
    () => {
    check_int(
      "65536 -> 80% 52428.8 floored to 52000",
      52_000,
      AgentGlobals.effective_context_meter_limit(65_536),
    )
  }),
  test_case(
    "effective_context_meter_limit: small windows clamp to at least 1000",
    `Quick,
    () => {
    check_int("tiny", 1000, AgentGlobals.effective_context_meter_limit(100))
  }),
  test_case(
    "effective_context_meter_limit: capped at default_context_meter_max_tokens",
    `Quick,
    () => {
    check_int(
      "200000 context -> 80% rounded 160000 -> min cap 100000",
      100_000,
      AgentGlobals.effective_context_meter_limit(200_000),
    )
  }),
];

/* -------------------------------------------------------------------------- */
/* LLM context snapshot (Message.Utils + Agent.Agent.Utils.llm_context_snapshot_text) */

/* -------------------------------------------------------------------------- */
/* ToolCallHandler — non-composition failures surface Action.Failure.show */

let toolcall_handler_tests = [
  test_case(
    "structural EditorAction on empty editor returns descriptive error, not generic unknown",
    `Quick,
    () => {
      let settings = Settings.Model.init;
      let agent = Agent.Agent.Utils.init();
      let chat_id = agent.chat_system.current;
      let cws = CodeWithStatics.Model.mk(Editor.Model.mk(Zipper.init()));
      let action =
        CompositionActions.EditorAction(
          Action.Structural.Update(Action.Structural.Definition, "a", "1"),
        );
      switch (
        Agent.Agent.ToolCallHandler.update(
          ~settings,
          action,
          agent,
          cws,
          chat_id,
        )
      ) {
      | Ok(_) =>
        fail(
          "expected structural tool to fail on empty editor (no derivable AST)",
        )
      | Error(Agent.Failure.Info(msg)) =>
        let unknown = "Unknown error occured when trying to apply tool request to editor";
        check_bool("not the old opaque message", false, msg == unknown);
        check_bool(
          "includes Action.Failure.show text (Cant_derive or similar)",
          true,
          StringUtil.plain_search("Cant_derive", msg, 0) >= 0
          || StringUtil.plain_search("Cant_move", msg, 0) >= 0
          || StringUtil.plain_search("Cant_select", msg, 0) >= 0,
        );
        check_bool(
          "suffix explains tool context",
          true,
          StringUtil.plain_search("structural editor tool", msg, 0) >= 0,
        );
      };
    },
  ),
];

let context_llm_snapshot_tests = [
  test_case(
    "context_snapshot_body_for_llm equals mk_context_message.content",
    `Quick,
    () => {
      let body =
        Agent.Message.Utils.context_snapshot_body_for_llm(
          "  prog  ",
          " err ",
          " tests ",
          " wb ",
        );
      let msg =
        Agent.Message.Utils.mk_context_message(
          "  prog  ",
          " err ",
          " tests ",
          " wb ",
        );
      check_string("body vs message.content", body, msg.content);
    },
  ),
  test_case(
    "llm_context_snapshot_text matches mk_context_message from same editor/chat inputs",
    `Quick,
    () => {
      let z =
        switch (Parser.to_zipper("2")) {
        | Some(z) => z
        | None => fail("expected parse")
        };
      let editor = Editor.Model.mk(z);
      let cws = CodeWithStatics.Model.mk(editor);
      let chat = Agent.Chat.Utils.init(~system_prompt="p", ~dev_notes="d");
      let eval_result = EvalResult.Model.init;
      let prog =
        CompositionView.Public.print(
          ~probe_map=cws.dynamics,
          cws.editor,
          chat.agent_view,
        );
      let errs =
        ErrorPrint.all(
          CompositionGo.Public.mk_statics(cws.editor.state.zipper),
        )
        |> String.concat("\n");
      let tests =
        Agent.Agent.Utils.test_results_for_context(
          EvalResult.Model.test_results(eval_result),
        );
      let wb =
        AgentWorkbench.Utils.MainUtils.active_task_to_pretty_string(
          chat.agent_workbench,
        );
      let expected =
        Agent.Message.Utils.mk_context_message(prog, errs, tests, wb).content;
      let actual =
        Agent.Agent.Utils.llm_context_snapshot_text(
          ~cell_result=eval_result,
          cws,
          chat,
        );
      check_string("snapshot vs mk_context_message", expected, actual);
    },
  ),
];

/* -------------------------------------------------------------------------- */
/* ToolCallSummary: category mapping, signifier extraction, path joining */

let mk_tc =
    (~name: string, ~args: API.Json.t): OpenRouter.Reply.Model.tool_call =>
  OpenRouter.Reply.Model.{
    id: "id",
    name,
    args,
  };

let get_summary = (tc: OpenRouter.Reply.Model.tool_call): ToolCallSummary.t =>
  switch (ToolCallSummary.of_tool_call(tc)) {
  | Some(s) => s
  | None => fail("summary: unknown tool name " ++ tc.name)
  };

let check_category =
    (name: string, expected: ToolCallSummary.category, tc): unit => {
  let s = get_summary(tc);
  check_bool(name, true, s.category == expected);
};

let check_signifier = (name: string, expected: option(string), tc): unit => {
  let s = get_summary(tc);
  check_bool(name ++ " signifier", true, s.signifier == expected);
};

let tool_call_summary_tests = [
  test_case(
    "category mapping: edit tools -> Edit",
    `Quick,
    () => {
      let args = `Assoc([("path", `String("fib"))]);
      check_category(
        "update_definition",
        Edit,
        mk_tc(~name="update_definition", ~args),
      );
      check_category("update_body", Edit, mk_tc(~name="update_body", ~args));
      check_category(
        "delete_binding_clause",
        Edit,
        mk_tc(~name="delete_binding_clause", ~args),
      );
      check_category(
        "insert_before",
        Edit,
        mk_tc(~name="insert_before", ~args),
      );
    },
  ),
  test_case(
    "category mapping: read tools -> Read",
    `Quick,
    () => {
      let args = `Assoc([]);
      check_category(
        "view_entire_definition",
        Read,
        mk_tc(~name="view_entire_definition", ~args),
      );
      check_category(
        "view_context",
        Read,
        mk_tc(~name="view_context", ~args),
      );
      check_category(
        "show_references",
        Read,
        mk_tc(~name="show_references", ~args),
      );
    },
  ),
  test_case(
    "category mapping: view/projector/probe/statics/workbench",
    `Quick,
    () => {
      let empty = `Assoc([]);
      let paths = `Assoc([("paths", `List([`String("x")]))]);
      check_category("expand", View, mk_tc(~name="expand", ~args=paths));
      check_category(
        "place_syntax_projector",
        Projector,
        mk_tc(~name="place_syntax_projector", ~args=paths),
      );
      check_category(
        "place_probe",
        Probe,
        mk_tc(~name="place_probe", ~args=paths),
      );
      check_category(
        "place_statics",
        Statics,
        mk_tc(~name="place_statics", ~args=paths),
      );
      check_category(
        "unset_active_task",
        Workbench,
        mk_tc(~name="unset_active_task", ~args=empty),
      );
    },
  ),
  test_case(
    "unknown tool name returns None",
    `Quick,
    () => {
      let r =
        ToolCallSummary.of_tool_call(mk_tc(~name="nope", ~args=`Assoc([])));
      check_bool("None", true, r == None);
    },
  ),
  test_case(
    "single-path edit: signifier and jump_paths from `path` string",
    `Quick,
    () => {
      let args = `Assoc([("path", `String("fib"))]);
      let s = get_summary(mk_tc(~name="update_definition", ~args));
      check_signifier(
        "update_definition",
        Some("fib"),
        mk_tc(~name="update_definition", ~args),
      );
      check_bool("persists=true", true, s.persists);
      check_bool("jump_paths=[fib]", true, s.jump_paths == ["fib"]);
    },
  ),
  test_case(
    "delete tools: persists=false",
    `Quick,
    () => {
      let args = `Assoc([("path", `String("fib"))]);
      let s = get_summary(mk_tc(~name="delete_body", ~args));
      check_bool("persists=false", false, s.persists);
    },
  ),
  test_case(
    "insert_before with no path: signifier defaults to 'cursor'",
    `Quick,
    () => {
      let s = get_summary(mk_tc(~name="insert_before", ~args=`Assoc([])));
      check_signifier(
        "insert_before cursor",
        Some("cursor"),
        mk_tc(~name="insert_before", ~args=`Assoc([])),
      );
      check_bool("no jump_paths when path absent", true, s.jump_paths == []);
    },
  ),
  test_case(
    "paths joining: 2 entries joined with ', '",
    `Quick,
    () => {
      let args = `Assoc([("paths", `List([`String("a"), `String("b")]))]);
      check_signifier("a, b", Some("a, b"), mk_tc(~name="expand", ~args));
    },
  ),
  test_case(
    "paths joining: 5 entries truncate to 'a, b +3'",
    `Quick,
    () => {
      let args =
        `Assoc([
          (
            "paths",
            `List([
              `String("a"),
              `String("b"),
              `String("c"),
              `String("d"),
              `String("e"),
            ]),
          ),
        ]);
      check_signifier(
        "a, b +3",
        Some("a, b +3"),
        mk_tc(~name="expand", ~args),
      );
    },
  ),
  test_case(
    "projector place: signifier appends [kind]",
    `Quick,
    () => {
      let args =
        `Assoc([
          ("paths", `List([`String("fib")])),
          ("kind", `String("slider")),
        ]);
      check_signifier(
        "fib  [slider]",
        Some("fib  [slider]"),
        mk_tc(~name="place_syntax_projector", ~args),
      );
    },
  ),
  test_case(
    "multi-path: jump_paths preserves full list",
    `Quick,
    () => {
      let args =
        `Assoc([
          ("paths", `List([`String("a"), `String("b"), `String("c")])),
        ]);
      let s = get_summary(mk_tc(~name="place_probe", ~args));
      check_bool(
        "jump_paths=[a;b;c]",
        true,
        s.jump_paths == ["a", "b", "c"],
      );
    },
  ),
  test_case(
    "workbench create_new_task: signifier from task.title, truncated past 40 chars",
    `Quick,
    () => {
      let long_title = String.make(60, 'x');
      let args =
        `Assoc([
          (
            "task",
            `Assoc([
              ("title", `String(long_title)),
              ("description", `String("d")),
            ]),
          ),
        ]);
      let s = get_summary(mk_tc(~name="create_new_task", ~args));
      switch (s.signifier) {
      | Some(sig_) =>
        check_bool(
          "truncated with ellipsis",
          true,
          String.length(sig_) < 60 && String.length(sig_) > 0,
        );
        check_bool(
          "ends with ellipsis",
          true,
          String.length(sig_) > 1
          && String.sub(sig_, String.length(sig_) - 3, 3) == {|…|},
        );
      | None => fail("expected signifier")
      };
    },
  ),
  test_case(
    "workbench reorder_subtasks_in_active_task: signifier is count of subtasks",
    `Quick,
    () => {
      let args =
        `Assoc([
          (
            "subtasks_ordering",
            `List([`String("a"), `String("b"), `String("c")]),
          ),
        ]);
      check_signifier(
        "3 subtasks",
        Some("3 subtasks"),
        mk_tc(~name="reorder_subtasks_in_active_task", ~args),
      );
    },
  ),
  test_case(
    "workbench tools with no natural signifier: signifier=None",
    `Quick,
    () => {
      check_signifier(
        "unset_active_task",
        None,
        mk_tc(~name="unset_active_task", ~args=`Assoc([])),
      );
      check_signifier(
        "mark_active_task_complete",
        None,
        mk_tc(~name="mark_active_task_complete", ~args=`Assoc([])),
      );
    },
  ),
  test_case(
    "read tools: signifier=None, jump_paths=[], persists=false",
    `Quick,
    () => {
      let s =
        get_summary(
          mk_tc(~name="view_entire_definition", ~args=`Assoc([])),
        );
      check_bool("signifier=None", true, s.signifier == None);
      check_bool("jump_paths=[]", true, s.jump_paths == []);
      check_bool("persists=false", false, s.persists);
    },
  ),
];

let api_error_format_tests = [
  test_case(
    "format_api_error_content: newline between Code and Error (not backslash-E)",
    `Quick,
    () => {
      let s =
        Agent.Agent.Update.format_api_error_content(
          ~code=429,
          ~message="rate limited",
        );
      check_string("expected format", "Code: 429\nError: rate limited", s);
      check_bool("no literal backslash-E", false, String.contains(s, '\\'));
    },
  ),
];

let tests = (
  "Agent UX",
  slash_command_tests
  @ slash_menu_tests
  @ dialogue_slice_tests
  @ chat_context_meter_tests
  @ chat_messages_for_openrouter_tests
  @ agent_workbench_tests
  @ compaction_prompt_tests
  @ openrouter_tests
  @ context_meter_tests
  @ toolcall_handler_tests
  @ context_llm_snapshot_tests
  @ tool_call_summary_tests
  @ api_error_format_tests,
);
