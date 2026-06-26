/** Unit tests for coding-agent UX: slash commands, compaction dialogue slicing,
    CompactionPrompt assembly, OpenRouter chat response parsing, and context-meter math.
    (Tool-call editing tests live in [[Test_AgentTools]].) */
open Alcotest;
open Util;
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
    "dialogue_slice: after compaction summary, slice starts after summary",
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
      check(int, "only post-summary turns", 1, List.length(slice));
      check_string("content", "after compact", List.hd(slice).content);
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
    "effective_context_meter_limit: 131072 -> 104000 (80% floored to 1k steps)",
    `Quick,
    () => {
    check_int(
      "131072",
      104000,
      AgentGlobals.effective_context_meter_limit(131072),
    )
  }),
  test_case(
    "effective_context_meter_limit: small windows clamp to at least 1000",
    `Quick,
    () => {
    check_int("tiny", 1000, AgentGlobals.effective_context_meter_limit(100))
  }),
];

let tests = (
  "Agent UX",
  slash_command_tests
  @ slash_menu_tests
  @ dialogue_slice_tests
  @ compaction_prompt_tests
  @ openrouter_tests
  @ context_meter_tests,
);
