open Alcotest;
open Haz3lcore;
open Util;
open Web;

/* a reply's structural calls on the editing kernel: one transaction, its
   results by canonical path; tests addressed by hint; move */

let mk_zipper = (code: string): Zipper.t =>
  switch (Parser.to_zipper(~root=Exp, code)) {
  | Some(z) => z
  | None => fail("unparseable: " ++ code)
  };

let text = (z: Zipper.t): string => Printer.of_zipper(~holes="?", z);

let call = (i, name, args): OpenRouter.Reply.Model.tool_call => {
  id: "call-" ++ string_of_int(i),
  name,
  args: `Assoc(List.map(((k, v)) => (k, `String(v)), args)),
};

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

/* [replies] in turn on [code], one assistant reply each: the program
   after each, and each reply's tool results in order */
let replies =
    (
      ~settings=Settings.Model.init,
      code: string,
      replies: list(list(OpenRouter.Reply.Model.tool_call)),
    )
    : list((string, list(AgentToolResult.tool_result))) => {
  let rec go = (agent: Agent.Model.t, ce: CellEditor.Model.t, replies) =>
    switch (replies) {
    | [] => []
    | [calls, ...rest] =>
      let before = List.length(tool_results(agent));
      let (agent', ce') =
        Agent.Update.update(
          Agent.Update.Action.HandleLLMResponse(
            {
              content: "",
              tool_calls: calls,
              usage: None,
              reasoning: None,
            },
            agent.chat_system.current,
            agent.main_llm_seq,
            0,
          ),
          agent,
          ce,
          settings,
          _ =>
          ()
        );
      [
        (
          text(ce'.model.editor.editor.state.zipper),
          ListUtil.remove_first_n(before, tool_results(agent')),
        ),
        ...go(agent', ce'.model, rest),
      ];
    };
  go(
    Agent.Utils.init(),
    CellEditor.Model.mk(Editor.Model.mk(mk_zipper(code), ~root=Exp)),
    replies,
  );
};

/* [calls] as one assistant reply on [code]: the program after, and the
   tool results in order */
let reply =
    (
      ~settings=?,
      code: string,
      calls: list((string, list((string, string)))),
    )
    : (string, list(AgentToolResult.tool_result)) =>
  List.hd(
    replies(
      ~settings?,
      code,
      [List.mapi((i, (n, a)) => call(i, n, a), calls)],
    ),
  );

/* an example's calls as a reply's */
let example_calls = (calls: list(ToolExamples.call)) =>
  List.mapi(
    (i, c: ToolExamples.call): OpenRouter.Reply.Model.tool_call =>
      {
        id: "call-" ++ string_of_int(i),
        name: c.tool,
        args: `Assoc(c.args),
      },
    calls,
  );

let has = (needle, s) => StringUtil.plain_search(needle, s, 0) >= 0;
let mentions = (what, needle, s) =>
  check(
    bool,
    what ++ " mentions \"" ++ needle ++ "\": " ++ s,
    true,
    has(needle, s),
  );

let same_text = (what, want, got) =>
  check(
    string,
    what,
    String.concat(" ", String.split_on_char('\n', want)),
    String.concat(" ", String.split_on_char('\n', got)),
  );

let geo = "let area = fun s -> s * s in\nlet total = fun xs -> 0 in\nmodule Geo = { let perimeter = 4 } in\n?";

let transactions = [
  test_case(
    "a later call edits what an earlier one made",
    `Quick,
    () => {
      /* the stale-syntax bug: each call resolves against the program as the
         calls before it left it */
      let (program, results) =
        reply(
          "let a = 1 in a",
          [
            ("insert", [("after", "a"), ("code", "let b = a + 1 in")]),
            ("replace", [("target", "b:definition"), ("code", "a + 2")]),
            ("replace", [("target", "=>"), ("code", "b")]),
          ],
        );
      same_text("program", "let a = 1 in let b = a + 2 in b", program);
      check(
        list(bool),
        "all applied",
        [true, true, true],
        List.map((tr: AgentToolResult.tool_result) => tr.success, results),
      );
      mentions("insert's result", "Added b.", List.hd(results).content);
      mentions(
        "update's result",
        "Replaced b:definition.",
        List.nth(results, 1).content,
      );
      mentions(
        "the last result",
        "Static errors: none.",
        List.nth(results, 2).content,
      );
    },
  ),
  test_case(
    "a failed call rolls back the whole reply, naming it",
    `Quick,
    () => {
      let code = "let a = 1 in a";
      let (program, results) =
        reply(
          code,
          [
            ("insert", [("after", "a"), ("code", "let b = 2 in")]),
            ("replace", [("target", "nope:definition"), ("code", "3")]),
            ("insert", [("after", "a"), ("code", "let c = 3 in")]),
          ],
        );
      same_text("program unchanged", code, program);
      switch (results) {
      | [first, second, third] =>
        check(bool, "first not applied", false, first.success);
        mentions("first", "Rolled back.", first.content);
        mentions("first", "call 2 (replace) failed", first.content);
        check(bool, "second failed", false, second.success);
        mentions("second", "No entry \"nope\"", second.content);
        mentions(
          "second",
          "None of this reply's edits were applied",
          second.content,
        );
        check(bool, "third skipped", true, third.skipped);
      | _ => fail("three results")
      };
    },
  ),
  test_case(
    "a reply adding errors is refused at its end",
    `Quick,
    () => {
      let code = "let a = 1 in a";
      let (program, results) =
        reply(
          code,
          [
            ("insert", [("after", "a"), ("code", "let b = 2 in")]),
            (
              "replace",
              [("target", "a:definition"), ("code", "true + 1")],
            ),
          ],
        );
      same_text("program unchanged", code, program);
      let second = List.nth(results, 1);
      check(bool, "the erring call failed", false, second.success);
      mentions("its result", "it would add static errors", second.content);
      mentions("its result", "a:", second.content);
      mentions("the first", "Rolled back.", List.hd(results).content);
    },
  ),
  test_case(
    "fixing one error doesn't pay for another",
    `Quick,
    () => {
      let code = "let a = true + 1 in let b = 2 in b";
      let (program, results) =
        reply(
          code,
          [
            ("replace", [("target", "a:definition"), ("code", "1")]),
            (
              "replace",
              [("target", "b:definition"), ("code", "false + 2")],
            ),
          ],
        );
      same_text("program unchanged", code, program);
      let second = List.nth(results, 1);
      check(bool, "the call adding it failed", false, second.success);
      mentions("its result", "1 before, 1 after", second.content);
      mentions("its result", "- b:", second.content);
    },
  ),
  test_case(
    "an error restated where it was is not new",
    `Quick,
    () => {
      let (program, results) =
        reply(
          "let a = true + 1 in a",
          [
            ("replace", [("target", "a:definition"), ("code", "true + 2")]),
          ],
        );
      same_text("applied", "let a = true + 2 in a", program);
      mentions(
        "result",
        "Static errors: 1 (1 before), none new.",
        List.hd(results).content,
      );
    },
  ),
  test_case(
    "the checks switch off, for ablation",
    `Quick,
    () => {
      let off = (g: AgentGlobals.Model.t => AgentGlobals.Model.t) => {
        let s = Settings.Model.init;
        {
          ...s,
          agent_globals: g(s.agent_globals),
        };
      };
      let (program, results) =
        reply(
          ~settings=
            off(g =>
              {
                ...g,
                check_no_new_errors: false,
              }
            ),
          "let a = 1 in a",
          [
            ("replace", [("target", "a:definition"), ("code", "true + 1")]),
          ],
        );
      same_text("applied with its error", "let a = true + 1 in a", program);
      mentions("result", "New:", List.hd(results).content);
      let (program, _) =
        reply(
          ~settings=
            off(g =>
              {
                ...g,
                check_dangling_uses: false,
                check_no_new_errors: false,
              }
            ),
          "let a = 1 in let b = a in b",
          [("delete", [("target", "a")])],
        );
      same_text("deleted, its use left", "let b = a in b", program);
    },
  ),
  test_case(
    "a reply may break and fix uses together",
    `Quick,
    () => {
      let (program, results) =
        reply(
          "let f = fun x -> x + 1 in f(1)",
          [
            (
              "replace",
              [("target", "f:definition"), ("code", "fun x -> x > 1")],
            ),
            (
              "replace",
              [("target", "=>"), ("code", "if f(1) then 1 else 0")],
            ),
          ],
        );
      same_text(
        "program",
        "let f = fun x -> x > 1 in if f(1) then 1 else 0",
        program,
      );
      check(
        bool,
        "applied",
        true,
        List.for_all(
          (tr: AgentToolResult.tool_result) => tr.success,
          results,
        ),
      );
    },
  ),
  test_case(
    "results carry the kernel's steps",
    `Quick,
    () => {
      let (_, results) =
        reply(
          "let a = 1 in a",
          [("insert", [("after", "a"), ("code", "let b = 2 in")])],
        );
      switch (List.hd(results).steps) {
      | [step] =>
        check(string, "op", "insert", step.op);
        check(option(string), "its entry", Some("b"), step.into);
        check(int, "one added", 1, List.length(step.added));
      | _ => fail("one step")
      };
    },
  ),
];

let deletes_and_renames = [
  test_case(
    "a delete leaving uses dangling is refused",
    `Quick,
    () => {
      let code = "let a = 1 in let b = a + 1 in let c = a * 2 in b + c";
      let (program, results) =
        reply(code, [("delete", [("target", "a")])]);
      same_text("program unchanged", code, program);
      let r = List.hd(results);
      check(bool, "refused", false, r.success);
      mentions("result", "a, used at b, c", r.content);
    },
  ),
  test_case(
    "deleting the uses in the same reply lets it go",
    `Quick,
    () => {
      let (program, _) =
        reply(
          "let a = 1 in let b = a + 1 in b",
          [
            ("delete", [("target", "a")]),
            ("replace", [("target", "b:definition"), ("code", "1")]),
          ],
        );
      same_text("program", "let b = 1 in b", program);
    },
  ),
  test_case(
    "a rename keeps the binder's and its uses' ids",
    `Quick,
    () => {
      let z = mk_zipper("let total = 1 in total + total");
      let seg = Zipper.unselect_and_zip(z);
      let rec toks = (seg: Segment.t): list((Id.t, string)) =>
        List.concat_map(
          (p: Piece.t) =>
            switch (p) {
            | Tile({form: Tok(s), id, _}) => [(id, s)]
            | Tile(t) => List.concat_map(toks, t.children)
            | _ => []
            },
          seg,
        );
      let ids_of = (seg, name) =>
        toks(seg) |> List.filter(((_, s)) => s == name) |> List.map(fst);
      let before = ids_of(seg, "total");
      switch (
        CompositionGo.go(
          ~settings=Language.CoreSettings.on,
          ~root=Exp,
          ~statics=CachedStatics.empty,
          z,
          Rename("total", "sum"),
        )
      ) {
      | Ok(z') =>
        let seg' = Zipper.unselect_and_zip(z');
        check(int, "three tokens", 3, List.length(before));
        check(
          list(string),
          "same ids, renamed",
          List.map(Id.to_string, before),
          List.map(Id.to_string, ids_of(seg', "sum")),
        );
      | Error(e) => fail(Action.Failure.show(e))
      };
    },
  ),
  test_case(
    "a rename that would capture is refused",
    `Quick,
    () => {
      let code = "let a = 1 in let b = 2 in a + b";
      let (program, results) =
        reply(code, [("rename", [("target", "b"), ("to", "a")])]);
      same_text("program unchanged", code, program);
      mentions("result", "Not renamed to a", List.hd(results).content);
    },
  ),
  test_case(
    "a member used outside its module renames with its M.x",
    `Quick,
    () => {
      let (program, _) =
        reply(
          "module M = { let x = 1 } in M.x + M.x",
          [("rename", [("target", "M/x"), ("to", "y")])],
        );
      same_text("program", "module M = { let y = 1 } in M.y + M.y", program);
    },
  ),
];

let tests_by_hint = [
  test_case(
    "a hinted test is addressed by its hint",
    `Quick,
    () => {
      let (program, results) =
        reply(
          "let double = fun x -> 2 * x in\nhint \"doubles\" test double(1) == 3 end;\ndouble(5)",
          [
            (
              "replace",
              [
                ("target", "\"doubles\""),
                ("code", "hint \"doubles\" test double(1) == 2 end"),
              ],
            ),
          ],
        );
      mentions("program", "double(1) == 2", program);
      mentions("result", "Replaced \"doubles\".", List.hd(results).content);
    },
  ),
  test_case(
    "an unhinted test by test#k",
    `Quick,
    () => {
      let (program, _) =
        reply(
          "let a = 1 in\ntest a == 2 end;\ntest a == 1 end;\na",
          [
            ("replace", [("target", "test#1"), ("code", "test a > 0 end")]),
          ],
        );
      same_text(
        "program",
        "let a = 1 in\ntest a > 0 end;\ntest a == 1 end;\na",
        program,
      );
    },
  ),
  test_case(
    "an inserted test is reported by its path",
    `Quick,
    () => {
      let (_, results) =
        reply(
          "let a = 1 in a",
          [("insert", [("after", "a"), ("code", "test a == 1 end")])],
        );
      mentions("result", "Added test#1.", List.hd(results).content);
    },
  ),
];

let moves = [
  test_case(
    "move into a module, last",
    `Quick,
    () => {
      let (program, results) =
        reply(geo, [("move", [("target", "area"), ("last_in", "Geo")])]);
      mentions(
        "program",
        "let perimeter = 4; let area = fun s -> s * s",
        String.concat(" ", String.split_on_char('\n', program)),
      );
      check(
        bool,
        "area left the top",
        false,
        has("let area = fun s -> s * s in", program),
      );
      mentions(
        "result",
        "Moved area last in Geo (now Geo/area).",
        List.hd(results).content,
      );
    },
  ),
  test_case(
    "move before and after",
    `Quick,
    () => {
      let (program, _) =
        reply(geo, [("move", [("target", "area"), ("after", "total")])]);
      mentions(
        "after",
        "let total = fun xs -> 0 in\nlet area = fun s -> s * s in",
        program,
      );
      let (program, _) =
        reply(geo, [("move", [("target", "Geo"), ("before", "area")])]);
      mentions(
        "before",
        "module Geo = { let perimeter = 4 } in\nlet area",
        program,
      );
    },
  ),
  test_case(
    "move keeps the entry's ids",
    `Quick,
    () => {
      let z = mk_zipper(geo);
      let seg = Zipper.unselect_and_zip(z);
      let anchor = seg =>
        switch (EntryPath.resolve_string(~root=Exp, seg, "area")) {
        | Ok(f) => Some(f.id)
        | Error(_) =>
          switch (EntryPath.resolve_string(~root=Exp, seg, "Geo/area")) {
          | Ok(f) => Some(f.id)
          | Error(_) => None
          }
        };
      switch (
        CompositionGo.go(
          ~settings=Language.CoreSettings.on,
          ~root=Exp,
          ~statics=CachedStatics.empty,
          z,
          Move("area", FirstIn("Geo")),
        )
      ) {
      | Ok(z') =>
        check(
          option(string),
          "same anchor",
          Option.map(Id.to_string, anchor(seg)),
          Option.map(Id.to_string, anchor(Zipper.unselect_and_zip(z'))),
        )
      | Error(e) => fail(Action.Failure.show(e))
      };
    },
  ),
  test_case(
    "move to the top level's end",
    `Quick,
    () => {
      let (program, _) =
        reply(geo, [("move", [("target", "area"), ("last_in", "")])]);
      mentions(
        "program",
        "module Geo = { let perimeter = 4 } in\nlet area = fun s -> s * s in\n?",
        program,
      );
    },
  ),
  test_case(
    "move takes exactly one place",
    `Quick,
    () => {
      let (_, results) =
        reply(
          geo,
          [
            (
              "move",
              [("target", "area"), ("before", "total"), ("after", "Geo")],
            ),
          ],
        );
      mentions("result", "exactly one of", List.hd(results).content);
    },
  ),
];

/* [ce] evaluated, synchronously */
let evaluated = (ce: CellEditor.Model.t): CellEditor.Model.t =>
  CellEditor.Update.calculate(
    ~settings=Language.CoreSettings.on,
    ~is_edited=true,
    ~statics_mode=Force,
    ~queue_worker=None,
    ~stitch=x => x,
    ce,
  );

let test_results = [
  test_case(
    "a reply's last edit result takes the tests' change",
    `Quick,
    () => {
      let settings = Settings.Model.init;
      let agent = Agent.Utils.init();
      let chat_id = agent.chat_system.current;
      let ce =
        evaluated(
          CellEditor.Model.mk(
            Editor.Model.mk(
              mk_zipper(
                "let double = fun x -> 3 * x in\ntest double(1) == 2 end;\nhint \"zero\" test double(0) == 0 end;\ndouble(5)",
              ),
              ~root=Exp,
            ),
          ),
        );
      let (agent', ce') =
        Agent.Update.update(
          Agent.Update.Action.HandleLLMResponse(
            {
              content: "",
              tool_calls: [
                call(
                  0,
                  "replace",
                  [
                    ("target", "double:definition"),
                    ("code", "fun x -> 2 * x"),
                  ],
                ),
              ],
              usage: None,
              reasoning: None,
            },
            chat_id,
            agent.main_llm_seq,
            0,
          ),
          agent,
          ce,
          settings,
          _ =>
          ()
        );
      check(bool, "pending", true, AgentToolExec.Tests.pending^ != None);
      let after = evaluated(ce'.model);
      let chat_system =
        AgentToolExec.Tests.report(
          ~chat_id,
          EvalResult.Model.test_results(after.result),
          agent'.chat_system,
        );
      check(bool, "taken", true, AgentToolExec.Tests.pending^ == None);
      let result =
        Chat.Utils.linearize(
          ChatSystem.Utils.find_chat(chat_id, chat_system),
        )
        |> List.filter_map((m: Message.Model.t) =>
             switch (m.role) {
             | ToolResult(tr) => Some(tr.content)
             | _ => None
             }
           )
        |> List.hd;
      mentions(
        "result",
        "Tests changed: test#1 failing → passing.",
        result,
      );
    },
  ),
];

/* the prompt's few-shot example (Eg_RecFib, map_fib) runs as written */
/* the system prompt's examples and the few-shot example run as written */
let prompt_examples = [
  test_case("the tool examples", `Quick, () =>
    List.iter(
      (e: ToolExamples.example) => {
        let (_, results) =
          List.hd(replies(e.program, [example_calls(e.calls)]));
        List.iteri(
          (i, tr: AgentToolResult.tool_result) => {
            let c = List.nth(e.calls, i);
            switch (e.results) {
            | Some(rs) =>
              let want = List.nth(rs, i);
              tr.success
                ? mentions(e.intent, c.tool ++ ": " ++ want, tr.content)
                : mentions(e.intent, want, tr.content);
            | None =>
              check(bool, e.intent ++ ": " ++ tr.content, true, tr.success)
            };
          },
          results,
        );
        check(
          int,
          e.intent ++ ": all ran",
          List.length(e.calls),
          List.length(results),
        );
      },
      ToolExamples.all,
    )
  ),
  test_case("the few-shot example", `Quick, () =>
    List.iter(
      (e: Eg_RecFib.example) => {
        Option.iter(
          view =>
            Test_AgentView.golden(
              e.asks ++ ": the start",
              view,
              Test_AgentView.render(~evaluate=true, e.program),
            ),
          e.start,
        );
        let steps =
          replies(
            e.program,
            List.map(
              (s: Eg_RecFib.step) => example_calls(s.calls),
              e.steps,
            ),
          );
        List.iter2(
          (s: Eg_RecFib.step, (program, results)) => {
            List.iter(
              (tr: AgentToolResult.tool_result) =>
                check(
                  bool,
                  e.asks ++ ": " ++ tr.tool_call.name ++ ": " ++ tr.content,
                  true,
                  tr.success,
                ),
              results,
            );
            switch (s.sees) {
            | Some((view, opened)) =>
              Test_AgentView.golden(
                e.asks,
                view,
                Test_AgentView.render(~evaluate=true, ~opened, program),
              )
            | None => ()
            };
          },
          e.steps,
          steps,
        );
      },
      Eg_RecFib.examples,
    )
  ),
];

let tests = [
  ("AgentReply.PromptExamples", prompt_examples),
  ("AgentReply.Transactions", transactions),
  ("AgentReply.TestResults", test_results),
  ("AgentReply.DeletesAndRenames", deletes_and_renames),
  ("AgentReply.TestsByHint", tests_by_hint),
  ("AgentReply.Move", moves),
];
