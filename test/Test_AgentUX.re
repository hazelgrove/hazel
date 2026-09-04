/** Unit tests for coding-agent UX: slash commands, compaction dialogue slicing,
    CompactionPrompt assembly, OpenRouter chat response parsing, context-meter math,
    workbench completion semantics.
    (Structural tool-call editing tests: [[Test_AgentTools]]; multi-tool LLM replies: [[Test_AgentMultiTool]].) */
open Alcotest;
open Util_web;
open Haz3lcore;
open Web;

let check_string = (name: string, expected: string, actual: string) =>
  check(string, name, expected, actual);

let check_int = (name: string, expected: int, actual: int) =>
  check(int, name, expected, actual);

let check_bool = (name: string, expected: bool, actual: bool) =>
  check(bool, name, expected, actual);

/* -------------------------------------------------------------------------- */
/* Slash commands (ChatSlashCommands) */

let slash_command_tests = [
  test_case(
    "ChatSlashCommands.filtered: empty filter lists all commands alphabetically",
    `Quick,
    () => {
      let xs = ChatSlashCommands.filtered("");
      let names = List.map(fst, xs);
      check(
        list(string),
        "alphabetical order",
        [
          "account-usage",
          "compact",
          "help",
          "key",
          "key-usage",
          "session-usage",
          "show-thinking",
        ],
        names,
      );
      List.iter(
        ((_, desc)) =>
          check_bool("desc non-empty", true, String.length(desc) > 0),
        xs,
      );
    },
  ),
  test_case(
    "ChatSlashCommands.filtered: 'c' matches compact only",
    `Quick,
    () => {
      let xs = ChatSlashCommands.filtered("c");
      let names = List.map(fst, xs);
      check(list(string), "c-prefix", ["compact"], names);
    },
  ),
  test_case(
    "ChatSlashCommands.filtered: prefix matches case-insensitively",
    `Quick,
    () => {
      let xs = ChatSlashCommands.filtered("COM");
      check(int, "one match", 1, List.length(xs));
      switch (xs) {
      | [(name, _), ..._] => check_string("name", "compact", name)
      | [] => fail("expected match")
      };
    },
  ),
  test_case(
    "ChatSlashCommands.filtered: 'h' matches help only",
    `Quick,
    () => {
      let xs = ChatSlashCommands.filtered("h");
      check(int, "one match", 1, List.length(xs));
      switch (xs) {
      | [(name, _), ..._] => check_string("name", "help", name)
      | [] => fail("expected match")
      };
    },
  ),
  test_case(
    "ChatSlashCommands.filtered: no match returns empty",
    `Quick,
    () => {
      let xs = ChatSlashCommands.filtered("zzz");
      check(int, "empty", 0, List.length(xs));
    },
  ),
  test_case(
    "ChatSlashCommands.help_payload: contains every command",
    `Quick,
    () => {
      let p = ChatSlashCommands.help_payload();
      let names =
        List.map(
          (e: Message.Model.help_entry) => e.help_name,
          p.help_entries,
        );
      let has = needle => List.mem(needle, names);
      check_bool("compact listed", true, has("compact"));
      check_bool("session-usage listed", true, has("session-usage"));
      check_bool("account-usage listed", true, has("account-usage"));
      check_bool("help listed", true, has("help"));
      check_bool("key-usage listed", true, has("key-usage"));
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
        ChatSystem.Utils.derive_slash_menu_from_content(~prev=None, "hello");
      check_bool("no slash", true, Option.is_none(r));
    },
  ),
  test_case(
    "derive_slash_menu: '/' opens menu with empty filter token", `Quick, () => {
    switch (ChatSystem.Utils.derive_slash_menu_from_content(~prev=None, "/")) {
    | None => fail("expected Some")
    | Some(sm) =>
      check_string("filter", "", sm.filter);
      check_int("selected_index", 0, sm.selected_index);
    }
  }),
  test_case("derive_slash_menu: '/compact' sets filter token", `Quick, () => {
    switch (
      ChatSystem.Utils.derive_slash_menu_from_content(~prev=None, "/compact")
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
        ChatSystem.Utils.derive_slash_menu_from_content(
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
      let cs = ChatSystem.Utils.init(~system_prompt="p", ~dev_notes="d");
      let cs =
        switch (
          ChatSystem.Update.update(
            ChatSystem.Update.Action.SaveTextBoxContent("/"),
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
            ChatSystem.Update.update(
              ChatSystem.Update.Action.SlashMenuAdjustSelection(1),
              cs,
            )
          ) {
          | Ok(m) => m
          | Error(_) => fail("SlashMenuAdjustSelection failed")
          };
        switch (cs2.ui.slash_menu) {
        | None => fail("menu should stay open")
        | Some(sm1) =>
          /* Down moves to index 1 (5 commands listed). */
          check_int("index after down", 1, sm1.selected_index)
        };
      };
    },
  ),
];

/* -------------------------------------------------------------------------- */
/* Compaction dialogue slice (Chat.Utils.dialogue_slice_for_compaction_summary) */

let dialogue_slice_tests = [
  test_case(
    "dialogue_slice: fresh chat (prompt + dev only) yields empty slice",
    `Quick,
    () => {
      let chat = Chat.Utils.init(~system_prompt="sp", ~dev_notes="dn");
      let slice = Chat.Utils.dialogue_slice_for_compaction_summary(chat);
      check(int, "empty slice", 0, List.length(slice));
    },
  ),
  test_case(
    "dialogue_slice: after user message, slice is that message only",
    `Quick,
    () => {
      let chat = Chat.Utils.init(~system_prompt="sp", ~dev_notes="dn");
      let u = Message.Utils.mk_user_message("hello");
      let chat = Chat.Utils.append(u, chat);
      let slice = Chat.Utils.dialogue_slice_for_compaction_summary(chat);
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
      let chat = Chat.Utils.init(~system_prompt="sp", ~dev_notes="dn");
      let sum =
        Message.Utils.mk_compaction_summary(
          ~method="test-method",
          "prior summary body",
        );
      let chat = Chat.Utils.append(sum, chat);
      let u = Message.Utils.mk_user_message("after compact");
      let chat = Chat.Utils.append(u, chat);
      let slice = Chat.Utils.dialogue_slice_for_compaction_summary(chat);
      check(int, "summary + post-summary", 2, List.length(slice));
      switch (List.hd(slice).role) {
      | Message.Model.System(Message.Model.CompactionSummary(m)) =>
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
    cache_read_input_tokens: None,
    cache_creation_input_tokens: None,
    cost: None,
    upstream_inference_cost: None,
    cache_write_tokens: None,
    model_id: None,
  };

let chat_context_meter_tests = [
  test_case(
    "context_meter_prompt_tokens: last agent usage when no compaction",
    `Quick,
    () => {
      let chat = Chat.Utils.init(~system_prompt="sp", ~dev_notes="dn");
      let chat = Chat.Utils.append(Message.Utils.mk_user_message("u"), chat);
      let chat =
        Chat.Utils.append(
          Message.Utils.mk_agent_message("a", Some(tok(42))),
          chat,
        );
      check(
        bool,
        "Some(42)",
        true,
        Chat.Utils.context_meter_prompt_tokens(chat) == Some(42),
      );
    },
  ),
  test_case(
    "context_meter_prompt_tokens: None when last agent is before latest compaction",
    `Quick,
    () => {
      let chat = Chat.Utils.init(~system_prompt="sp", ~dev_notes="dn");
      let chat =
        Chat.Utils.append(
          Message.Utils.mk_agent_message("old", Some(tok(10))),
          chat,
        );
      let chat =
        Chat.Utils.append(
          Message.Utils.mk_compaction_summary(~method="m", "sum"),
          chat,
        );
      let chat =
        Chat.Utils.append(Message.Utils.mk_user_message("after"), chat);
      check(
        bool,
        "no meter until post-compaction agent reply",
        true,
        Chat.Utils.context_meter_prompt_tokens(chat) == None,
      );
    },
  ),
  test_case(
    "context_meter_prompt_tokens: Some after agent follows compaction summary",
    `Quick,
    () => {
      let chat = Chat.Utils.init(~system_prompt="sp", ~dev_notes="dn");
      let chat =
        Chat.Utils.append(
          Message.Utils.mk_compaction_summary(~method="m", "sum"),
          chat,
        );
      let chat =
        Chat.Utils.append(
          Message.Utils.mk_agent_message("fresh", Some(tok(99))),
          chat,
        );
      check(
        bool,
        "Some(99)",
        true,
        Chat.Utils.context_meter_prompt_tokens(chat) == Some(99),
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
      let chat = Chat.Utils.init(~system_prompt="sp", ~dev_notes="dn");
      let chat =
        Chat.Utils.append(
          Message.Utils.mk_compaction_summary(~method="meth", "body"),
          chat,
        );
      let chat =
        Chat.Utils.append(Message.Utils.mk_user_message("post"), chat);
      let ms = Chat.Utils.messages_for_openrouter(chat);
      check(int, "prompt+dev+suffix", 4, List.length(ms));
      switch (List.nth(ms, 0).role) {
      | Message.Model.System(Message.Model.Prompt) => ()
      | _ => fail("expected Prompt first")
      };
      switch (List.nth(ms, 1).role) {
      | Message.Model.System(Message.Model.DeveloperNotes) => ()
      | _ => fail("expected DeveloperNotes second")
      };
      let has_compact =
        List.exists(
          (m: Message.Model.t) =>
            switch (m.role) {
            | Message.Model.System(Message.Model.CompactionSummary(_)) => true
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
      let chat = Chat.Utils.init(~system_prompt="sp", ~dev_notes="dn");
      let chat =
        Chat.Utils.append(Message.Utils.mk_user_message("only"), chat);
      let ms = Chat.Utils.messages_for_openrouter(chat);
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

/* -------------------------------------------------------------------------- */
/* Prompt caching: cache_control placement + advancing history breakpoint */

let count_substring = (needle: string, hay: string): int => {
  let nlen = String.length(needle);
  let hlen = String.length(hay);
  let rec go = (i, acc) =>
    if (nlen == 0 || i > hlen - nlen) {
      acc;
    } else if (String.sub(hay, i, nlen) == needle) {
      go(i + nlen, acc + 1);
    } else {
      go(i + 1, acc);
    };
  go(0, 0);
};

let or_msg_json = (m: OpenRouter.Message.Model.t): string =>
  OpenRouter.Message.Utils.json_of_message(m) |> Yojson.Safe.to_string;

let anchored = (m: OpenRouter.Message.Model.t): OpenRouter.Message.Model.t => {
  ...m,
  cache_anchor: true,
};

let prompt_caching_tests = [
  test_case(
    "json_of_message: anchored user message renders cache_control on a text block",
    `Quick,
    () => {
      let s =
        or_msg_json(anchored(OpenRouter.Message.Utils.mk_user_msg("hi")));
      check_int("one cache_control", 1, count_substring("cache_control", s));
      check_bool(
        "ephemeral type",
        true,
        count_substring("ephemeral", s) == 1,
      );
    },
  ),
  test_case(
    "json_of_message: anchored tool result keeps tool_call_id and carries cache_control",
    `Quick,
    () => {
      let tc: OpenRouter.Reply.Model.tool_call = {
        id: "call_42",
        name: "edit",
        args: `Null,
      };
      let s =
        or_msg_json(
          anchored(OpenRouter.Message.Utils.mk_tool_msg("result body", tc)),
        );
      check_int("one cache_control", 1, count_substring("cache_control", s));
      check_bool(
        "retains tool_call_id",
        true,
        count_substring("call_42", s) == 1,
      );
    },
  ),
  test_case(
    "json_of_message: unanchored message has no cache_control (plain string content)",
    `Quick,
    () => {
      let s = or_msg_json(OpenRouter.Message.Utils.mk_user_msg("hi"));
      check_int("no cache_control", 0, count_substring("cache_control", s));
    },
  ),
  test_case(
    "json_of_message: anchored but blank content emits no cache_control marker",
    `Quick,
    () => {
      let s =
        or_msg_json(anchored(OpenRouter.Message.Utils.mk_user_msg("   ")));
      check_int(
        "no marker on blank",
        0,
        count_substring("cache_control", s),
      );
    },
  ),
  test_case(
    "api_messages_for_openrouter: anchors dev-notes floor + history message before snapshot",
    `Quick,
    () => {
      let chat = Chat.Utils.init(~system_prompt="sp", ~dev_notes="dn");
      let chat =
        Chat.Utils.append(Message.Utils.mk_user_message("hello"), chat);
      let chat = {
        ...chat,
        context:
          Some(
            Message.Utils.mk_context_message(
              ~session_mode=AgentGlobals.Model.Converse,
              "SNAPSHOT_BODY",
              "",
              "",
              "",
            ),
          ),
      };
      let msgs = Chat.Utils.api_messages_for_openrouter(chat);
      check_int("prompt+dev+user+snapshot", 4, List.length(msgs));
      /* Dev-notes (index 1) keeps its floor anchor; the user message (index 2,
         just before the snapshot) gets the advancing anchor; the snapshot
         (index 3) stays unmarked. */
      let anchored_count =
        List.length(
          List.filter(
            (m: OpenRouter.Message.Model.t) => m.cache_anchor,
            msgs,
          ),
        );
      check_int("two anchors", 2, anchored_count);
      let snapshot = List.nth(msgs, 3);
      check_bool("snapshot unanchored", false, snapshot.cache_anchor);
      let history = List.nth(msgs, 2);
      check_bool("history message anchored", true, history.cache_anchor);
    },
  ),
  test_case(
    "json_of_payload: Anthropic model emits two breakpoints + session_id; snapshot unmarked",
    `Quick,
    () => {
      let chat = Chat.Utils.init(~system_prompt="sp", ~dev_notes="dn");
      let chat =
        Chat.Utils.append(Message.Utils.mk_user_message("hello"), chat);
      let chat = {
        ...chat,
        context:
          Some(
            Message.Utils.mk_context_message(
              ~session_mode=AgentGlobals.Model.Converse,
              "SNAPSHOT_BODY",
              "",
              "",
              "",
            ),
          ),
      };
      let payload =
        OpenRouter.Payload.Utils.mk_default(
          ~model_id="anthropic/claude-opus-4.6",
          ~messages=Chat.Utils.api_messages_for_openrouter(chat),
          ~tools=[],
          ~session_id=Some("chat-xyz"),
          (),
        );
      let s =
        OpenRouter.Payload.Utils.json_of_payload(~payload)
        |> Yojson.Safe.to_string;
      check_int(
        "two cache breakpoints",
        2,
        count_substring("cache_control", s),
      );
      check_bool(
        "session_id present",
        true,
        count_substring("chat-xyz", s) == 1,
      );
    },
  ),
  test_case(
    "json_of_payload: auto-caching provider strips cache_control but keeps session_id",
    `Quick,
    () => {
      let chat = Chat.Utils.init(~system_prompt="sp", ~dev_notes="dn");
      let chat =
        Chat.Utils.append(Message.Utils.mk_user_message("hello"), chat);
      let chat = {
        ...chat,
        context:
          Some(
            Message.Utils.mk_context_message(
              ~session_mode=AgentGlobals.Model.Converse,
              "SNAPSHOT_BODY",
              "",
              "",
              "",
            ),
          ),
      };
      let payload =
        OpenRouter.Payload.Utils.mk_default(
          ~model_id="openai/gpt-4o",
          ~messages=Chat.Utils.api_messages_for_openrouter(chat),
          ~tools=[],
          ~session_id=Some("chat-xyz"),
          (),
        );
      let s =
        OpenRouter.Payload.Utils.json_of_payload(~payload)
        |> Yojson.Safe.to_string;
      check_int("no cache_control", 0, count_substring("cache_control", s));
      check_bool(
        "session_id still sent",
        true,
        count_substring("chat-xyz", s) == 1,
      );
    },
  ),
  test_case(
    "json_of_payload: Gemini and Qwen keep explicit breakpoints (same syntax)",
    `Quick,
    () => {
      let chat = Chat.Utils.init(~system_prompt="sp", ~dev_notes="dn");
      let chat =
        Chat.Utils.append(Message.Utils.mk_user_message("hello"), chat);
      let chat = {
        ...chat,
        context:
          Some(
            Message.Utils.mk_context_message(
              ~session_mode=AgentGlobals.Model.Converse,
              "SNAPSHOT_BODY",
              "",
              "",
              "",
            ),
          ),
      };
      let breakpoints = (model_id: string): int => {
        let payload =
          OpenRouter.Payload.Utils.mk_default(
            ~model_id,
            ~messages=Chat.Utils.api_messages_for_openrouter(chat),
            ~tools=[],
            (),
          );
        OpenRouter.Payload.Utils.json_of_payload(~payload)
        |> Yojson.Safe.to_string
        |> count_substring("cache_control");
      };
      check_int(
        "gemini keeps 2",
        2,
        breakpoints("google/gemini-3-flash-preview"),
      );
      check_int("qwen keeps 2", 2, breakpoints("qwen/qwen3-max"));
    },
  ),
  test_case(
    "json_of_payload: omits session_id when None",
    `Quick,
    () => {
      let payload =
        OpenRouter.Payload.Utils.mk_default(
          ~model_id="anthropic/claude-opus-4.6",
          ~messages=[OpenRouter.Message.Utils.mk_user_msg("hi")],
          ~tools=[],
          (),
        );
      let s =
        OpenRouter.Payload.Utils.json_of_payload(~payload)
        |> Yojson.Safe.to_string;
      check_int("no session_id field", 0, count_substring("session_id", s));
    },
  ),
];

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
/* LLM context snapshot (Message.Utils + Agent.Utils.llm_context_snapshot_text) */

/* -------------------------------------------------------------------------- */
/* ToolCallHandler — non-composition failures surface Action.Failure.show */

let toolcall_handler_tests = [
  test_case(
    "place_probe with only unresolved paths surfaces Error with unresolved path list",
    `Quick,
    () => {
      let settings = Settings.Model.init;
      let agent = Agent.Utils.init();
      let chat_id = agent.chat_system.current;
      let z =
        switch (Parser.to_zipper("let a = 1 in a", ~root=Exp)) {
        | Some(z) => z
        | None => fail("failed to parse fixture")
        };
      let cws = CodeWithStatics.Model.mk(Editor.Model.mk(z, ~root=Exp));
      let action =
        CompositionActions.ProbeAction(
          CompositionActions.PlaceProbe(["bogus", "also_bogus"]),
        );
      switch (
        Agent.ToolCallHandler.update(~settings, action, agent, cws, chat_id)
      ) {
      | Ok(_) => fail("expected Error when no path resolves to a binding")
      | Error(AgentResult.Failure.Info(msg)) =>
        check_bool(
          "message mentions no change",
          true,
          StringUtil.plain_search("did not update the program", msg, 0) >= 0,
        );
        check_bool(
          "message lists bogus",
          true,
          StringUtil.plain_search("bogus", msg, 0) >= 0,
        );
        check_bool(
          "message lists also_bogus",
          true,
          StringUtil.plain_search("also_bogus", msg, 0) >= 0,
        );
      };
    },
  ),
  test_case(
    "place_statics with only unresolved paths surfaces Error with unresolved path list",
    `Quick,
    () => {
      let settings = Settings.Model.init;
      let agent = Agent.Utils.init();
      let chat_id = agent.chat_system.current;
      let z =
        switch (Parser.to_zipper("let a = 1 in a", ~root=Exp)) {
        | Some(z) => z
        | None => fail("failed to parse fixture")
        };
      let cws = CodeWithStatics.Model.mk(Editor.Model.mk(z, ~root=Exp));
      let action =
        CompositionActions.StaticsAction(
          CompositionActions.PlaceStatics(["ghost"]),
        );
      switch (
        Agent.ToolCallHandler.update(~settings, action, agent, cws, chat_id)
      ) {
      | Ok(_) => fail("expected Error when no path resolves to a binding")
      | Error(AgentResult.Failure.Info(msg)) =>
        check_bool(
          "message mentions no change",
          true,
          StringUtil.plain_search("did not update the program", msg, 0) >= 0,
        );
        check_bool(
          "message lists ghost",
          true,
          StringUtil.plain_search("ghost", msg, 0) >= 0,
        );
      };
    },
  ),
  test_case(
    "place_probe with mixed paths (one valid, one unresolved) still succeeds",
    `Quick,
    () => {
      let settings = Settings.Model.init;
      let agent = Agent.Utils.init();
      let chat_id = agent.chat_system.current;
      let z =
        switch (Parser.to_zipper("let a = 1 in a", ~root=Exp)) {
        | Some(z) => z
        | None => fail("failed to parse fixture")
        };
      let cws = CodeWithStatics.Model.mk(Editor.Model.mk(z, ~root=Exp));
      let action =
        CompositionActions.ProbeAction(
          CompositionActions.PlaceProbe(["a", "bogus"]),
        );
      switch (
        Agent.ToolCallHandler.update(~settings, action, agent, cws, chat_id)
      ) {
      | Ok(_) => ()
      | Error(AgentResult.Failure.Info(msg)) =>
        fail("expected Ok on partial resolve; got Error: " ++ msg)
      };
    },
  ),
  test_case(
    "structural EditorAction on empty editor returns descriptive error, not generic unknown",
    `Quick,
    () => {
      let settings = Settings.Model.init;
      let agent = Agent.Utils.init();
      let chat_id = agent.chat_system.current;
      let cws =
        CodeWithStatics.Model.mk(Editor.Model.mk(Zipper.init(), ~root=Exp));
      let action =
        CompositionActions.EditorAction(
          Action.Structural.Update(Action.Structural.Definition, "a", "1"),
        );
      switch (
        Agent.ToolCallHandler.update(~settings, action, agent, cws, chat_id)
      ) {
      | Ok(_) =>
        fail(
          "expected structural tool to fail on empty editor (no derivable AST)",
        )
      | Error(AgentResult.Failure.Info(msg)) =>
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
        Message.Utils.context_snapshot_body_for_llm(
          ~session_mode=Web.AgentGlobals.Model.Edit,
          "  prog  ",
          " err ",
          " tests ",
          " wb ",
        );
      let msg =
        Message.Utils.mk_context_message(
          ~session_mode=Web.AgentGlobals.Model.Edit,
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
        switch (Parser.to_zipper("2", ~root=Exp)) {
        | Some(z) => z
        | None => fail("expected parse")
        };
      let editor = Editor.Model.mk(z, ~root=Exp);
      let cws = CodeWithStatics.Model.mk(editor);
      let chat = Chat.Utils.init(~system_prompt="p", ~dev_notes="d");
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
        Agent.Utils.test_results_for_context(
          EvalResult.Model.test_results(eval_result),
        );
      let wb =
        AgentWorkbench.Utils.MainUtils.active_task_to_pretty_string(
          chat.agent_workbench,
        );
      let expected =
        Message.Utils.mk_context_message(
          ~session_mode=Web.AgentGlobals.Model.Edit,
          prog,
          errs,
          tests,
          wb,
        ).
          content;
      let actual =
        Agent.Utils.llm_context_snapshot_text(
          ~session_mode=Web.AgentGlobals.Model.Edit,
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
    "insert_before with no path: signifier=None and display_name='insert'",
    `Quick,
    () => {
      let tc = mk_tc(~name="insert_before", ~args=`Assoc([]));
      let s = get_summary(tc);
      check_signifier("no signifier when path absent", None, tc);
      check_bool("no jump_paths when path absent", true, s.jump_paths == []);
      check_bool(
        "display_name='insert'",
        true,
        ToolCallSummary.display_name_for(tc) == "insert",
      );
    },
  ),
  test_case(
    "insert_before with path: display_name preserves tool name",
    `Quick,
    () => {
      let tc =
        mk_tc(
          ~name="insert_before",
          ~args=`Assoc([("path", `String("fib"))]),
        );
      check_bool(
        "display_name unchanged when path present",
        true,
        ToolCallSummary.display_name_for(tc) == "insert_before",
      );
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
        Agent.Update.format_api_error_content(
          ~code=429,
          ~message="rate limited",
        );
      check_string("expected format", "Code: 429\nError: rate limited", s);
      check_bool("no literal backslash-E", false, String.contains(s, '\\'));
    },
  ),
];

/* -------------------------------------------------------------------------- */
/* OpenRouter.Utils.StreamAccumulator — SSE delta assembly */

let feed_json = (acc, s: string) =>
  OpenRouter.Utils.StreamAccumulator.feed(acc, json_of_string(s));

let finalize_reply =
    (acc: OpenRouter.Utils.StreamAccumulator.t): OpenRouter.Reply.Model.t =>
  switch (OpenRouter.Utils.StreamAccumulator.finalize(acc)) {
  | OpenRouter.Model.Reply(r) => r
  | OpenRouter.Model.Error(_) => fail("unexpected accumulator error")
  };

let stream_accumulator_tests = [
  test_case(
    "StreamAccumulator: content-only deltas concatenate",
    `Quick,
    () => {
      let acc = OpenRouter.Utils.StreamAccumulator.create();
      let d1 =
        feed_json(acc, {|{"choices":[{"delta":{"content":"hello "}}]}|});
      let d2 =
        feed_json(acc, {|{"choices":[{"delta":{"content":"world"}}]}|});
      check_string("first delta", "hello ", d1.content_delta);
      check_string("first reasoning", "", d1.reasoning_delta);
      check_string("second delta", "world", d2.content_delta);
      let r = finalize_reply(acc);
      check_string("final content", "hello world", r.content);
      check_bool("no reasoning", true, r.reasoning == None);
      check_int("no tool calls", 0, List.length(r.tool_calls));
    },
  ),
  test_case(
    "StreamAccumulator: reasoning-only deltas stay separate per chunk",
    `Quick,
    () => {
      let acc = OpenRouter.Utils.StreamAccumulator.create();
      let d1 =
        feed_json(acc, {|{"choices":[{"delta":{"reasoning":"thinking "}}]}|});
      let d2 =
        feed_json(acc, {|{"choices":[{"delta":{"reasoning":"out loud"}}]}|});
      /* Per-chunk deltas must route reasoning to the reasoning field,
         not silently promote it to content — streaming UX needs the
         "Thinking" section to fill independently. */
      check_string("no content delta 1", "", d1.content_delta);
      check_string("reasoning delta 1", "thinking ", d1.reasoning_delta);
      check_string("no content delta 2", "", d2.content_delta);
      check_string("reasoning delta 2", "out loud", d2.reasoning_delta);
    },
  ),
  test_case(
    "StreamAccumulator: reasoning-only finalize surfaces reasoning as content",
    `Quick,
    () => {
      /* Back-compat with [handle_chat]: when the entire reply is
         reasoning-only, downstream consumers that only read [content]
         should still see text. [reasoning] is dropped in that case to
         avoid double-rendering. */
      let acc = OpenRouter.Utils.StreamAccumulator.create();
      let _ =
        feed_json(acc, {|{"choices":[{"delta":{"reasoning":"alone"}}]}|});
      let r = finalize_reply(acc);
      check_string("content takes over", "alone", r.content);
      check_bool("reasoning cleared", true, r.reasoning == None);
    },
  ),
  test_case(
    "StreamAccumulator: mixed finalize keeps reasoning alongside content",
    `Quick,
    () => {
      let acc = OpenRouter.Utils.StreamAccumulator.create();
      let _ =
        feed_json(
          acc,
          {|{"choices":[{"delta":{"content":"answer","reasoning":"why"}}]}|},
        );
      let r = finalize_reply(acc);
      check_string("content preserved", "answer", r.content);
      switch (r.reasoning) {
      | Some(s) => check_string("reasoning preserved", "why", s)
      | None => fail("expected reasoning")
      };
    },
  ),
  test_case(
    "StreamAccumulator: mixed content and reasoning in a single delta",
    `Quick,
    () => {
      let acc = OpenRouter.Utils.StreamAccumulator.create();
      let d =
        feed_json(
          acc,
          {|{"choices":[{"delta":{"content":"hi","reasoning":"why"}}]}|},
        );
      check_string("content delta", "hi", d.content_delta);
      check_string("reasoning delta", "why", d.reasoning_delta);
    },
  ),
  test_case(
    "StreamAccumulator: tool call args split across chunks reassemble at finalize",
    `Quick,
    () => {
      let acc = OpenRouter.Utils.StreamAccumulator.create();
      let _ =
        feed_json(
          acc,
          {|{"choices":[{"delta":{"tool_calls":[{"index":0,"id":"call_a","function":{"name":"probe","arguments":"{\"pa"}}]}}]}|},
        );
      let _ =
        feed_json(
          acc,
          {|{"choices":[{"delta":{"tool_calls":[{"index":0,"function":{"arguments":"th\":\"x\"}"}}]}}]}|},
        );
      let r = finalize_reply(acc);
      check_int("one tool call", 1, List.length(r.tool_calls));
      switch (r.tool_calls) {
      | [tc] =>
        check_string("id", "call_a", tc.id);
        check_string("name", "probe", tc.name);
      | _ => fail("expected exactly one tool call")
      };
    },
  ),
  test_case(
    "StreamAccumulator: usage chunk captured into finalize",
    `Quick,
    () => {
      let acc = OpenRouter.Utils.StreamAccumulator.create();
      let _ = feed_json(acc, {|{"choices":[{"delta":{"content":"x"}}]}|});
      let _ =
        feed_json(
          acc,
          {|{"choices":[],"usage":{"prompt_tokens":7,"completion_tokens":3,"total_tokens":10}}|},
        );
      let r = finalize_reply(acc);
      switch (r.usage) {
      | Some(u) =>
        check_int("prompt", 7, u.prompt_tokens);
        check_int("completion", 3, u.completion_tokens);
        check_int("total", 10, u.total_tokens);
      | None => fail("expected usage")
      };
    },
  ),
  test_case(
    "StreamAccumulator: error chunk surfaces as Model.Error",
    `Quick,
    () => {
      let acc = OpenRouter.Utils.StreamAccumulator.create();
      let _ =
        feed_json(acc, {|{"error":{"code":429,"message":"rate limited"}}|});
      switch (OpenRouter.Utils.StreamAccumulator.finalize(acc)) {
      | OpenRouter.Model.Error(_) => ()
      | OpenRouter.Model.Reply(_) => fail("expected error finalize")
      };
    },
  ),
  test_case(
    "StreamAccumulator: empty choices array does not break feed",
    `Quick,
    () => {
      let acc = OpenRouter.Utils.StreamAccumulator.create();
      let d = feed_json(acc, {|{"choices":[]}|});
      check_string("no content", "", d.content_delta);
      check_string("no reasoning", "", d.reasoning_delta);
    },
  ),
];

/* -------------------------------------------------------------------------- */
/* OpenRouter.Utils.error_of_http_failure — HTTP/network failure conversion */

let http_failure_tests = [
  test_case(
    "error_of_http_failure: provider JSON error body wins",
    `Quick,
    () => {
      let e =
        OpenRouter.Utils.error_of_http_failure(
          ~status=401,
          ~body={|{"error":{"message":"Invalid API key","code":401}}|},
        );
      check_string("provider message", "Invalid API key", e.message);
      check_int("provider code", 401, e.code);
    },
  ),
  test_case(
    "error_of_http_failure: non-JSON body falls back to HTTP status",
    `Quick,
    () => {
      let e =
        OpenRouter.Utils.error_of_http_failure(
          ~status=502,
          ~body="<html>Bad Gateway</html>",
        );
      check_string("fallback message", "HTTP error 502", e.message);
      check_int("status as code", 502, e.code);
    },
  ),
  test_case(
    "error_of_http_failure: status 0 reports network error",
    `Quick,
    () => {
      let e = OpenRouter.Utils.error_of_http_failure(~status=0, ~body="");
      check_string(
        "network message",
        "Network error: no response received",
        e.message,
      );
      check_int("code 0", 0, e.code);
    },
  ),
  test_case(
    "error_of_http_failure: non-int provider code falls back to status",
    `Quick,
    () => {
      let e =
        OpenRouter.Utils.error_of_http_failure(
          ~status=401,
          ~body={|{"error":{"message":"bad key","code":"invalid_api_key"}}|},
        );
      check_string("provider message kept", "bad key", e.message);
      check_int("status as code", 401, e.code);
    },
  ),
  test_case(
    "error_chunk_of_http_failure: chunk finalizes as Model.Error",
    `Quick,
    () => {
      /* End-to-end shape check: the synthetic chunk emitted on a streaming
         HTTP failure must round-trip through the accumulator's error path. */
      let acc = OpenRouter.Utils.StreamAccumulator.create();
      let _ =
        OpenRouter.Utils.StreamAccumulator.feed(
          acc,
          OpenRouter.Utils.error_chunk_of_http_failure(
            ~status=401,
            ~body={|{"error":{"message":"Invalid API key","code":401}}|},
          ),
        );
      switch (OpenRouter.Utils.StreamAccumulator.finalize(acc)) {
      | OpenRouter.Model.Error(e) =>
        check_string("message", "Invalid API key", e.message);
        check_int("code", 401, e.code);
      | OpenRouter.Model.Reply(_) => fail("expected error finalize")
      };
    },
  ),
];

/* Streaming markdown: the in-progress bubble now renders partial buffers
   through AgentMessageMarkdown on every frame. The safety of that rests on
   CommonMark being total over incomplete input — pin the load-bearing case
   (an unclosed fence parses as a code block to end-of-input), and smoke the
   renderer over stream-shaped prefixes. */
let streaming_markdown_tests = [
  test_case(
    "Omd: unclosed fence mid-stream parses as a code block",
    `Quick,
    () => {
      let blocks = Omd.of_string("intro\n```\nlet x = 1");
      let is_code_tail =
        switch (List.rev(blocks)) {
        | [Omd.Code_block(_, _, body), ..._] =>
          String.trim(body) == "let x = 1"
        | _ => false
        };
      check_bool("tail is code block", true, is_code_tail);
    },
  ),
  test_case(
    "AgentMessageMarkdown: total over stream-shaped prefixes",
    `Quick,
    () => {
      /* successive prefixes of a message with bold, link, fence */
      let full = "**bold** start, a [link](https://x.y) then\n```\ncode";
      let n = String.length(full);
      let rec go = i =>
        if (i <= n) {
          ignore(
            AgentMessageMarkdown.view_streaming(String.sub(full, 0, i)),
          );
          go(i + 1);
        };
      go(0);
      check_bool("no prefix raised", true, true);
    },
  ),
  test_case(
    "close_dangling_inline: closes strong and inline code, narrowly",
    `Quick,
    () => {
      let close = AgentMessageMarkdown.close_dangling_inline;
      check_string("dangling strong", "**bo**", close("**bo"));
      check_string("dangling code", "a `co`", close("a `co"));
      check_string(
        "balanced untouched",
        "a **b** `c`",
        close("a **b** `c`"),
      );
      check_string(
        "inside fence untouched",
        "x\n```\ncode **y",
        close("x\n```\ncode **y"),
      );
      check_string(
        "earlier paragraph out of scope",
        "**a\n\nplain tail",
        close("**a\n\nplain tail"),
      );
      check_string(
        "last paragraph only",
        "done **ok**\n\ntail **bo**",
        close("done **ok**\n\ntail **bo"),
      );
      check_string(
        "single star and underscores alone",
        "2 * 3 and snake_case",
        close("2 * 3 and snake_case"),
      );
      check_string(
        "star-star inside code span is literal",
        "`a ** b` fine",
        close("`a ** b` fine"),
      );
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
  @ prompt_caching_tests
  @ openrouter_tests
  @ context_meter_tests
  @ toolcall_handler_tests
  @ context_llm_snapshot_tests
  @ tool_call_summary_tests
  @ api_error_format_tests
  @ stream_accumulator_tests
  @ http_failure_tests
  @ streaming_markdown_tests,
);
