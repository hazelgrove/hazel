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
   after each and each reply's tool results in order, and the agent after
   them all */
let run =
    (
      ~settings=Settings.Model.init,
      code: string,
      replies: list(list(OpenRouter.Reply.Model.tool_call)),
    )
    : (list((string, list(AgentToolResult.tool_result))), Agent.Model.t) => {
  let rec go = (agent: Agent.Model.t, ce: CellEditor.Model.t, replies) =>
    switch (replies) {
    | [] => ([], agent)
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
      let (later, last) = go(agent', ce'.model, rest);
      (
        [
          (
            text(ce'.model.editor.editor.state.zipper),
            ListUtil.remove_first_n(before, tool_results(agent')),
          ),
          ...later,
        ],
        last,
      );
    };
  go(
    Agent.Utils.init(),
    CellEditor.Model.mk(Editor.Model.mk(mk_zipper(code), ~root=Exp)),
    replies,
  );
};

let replies = (~settings=?, code, replies) =>
  fst(run(~settings?, code, replies));

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
    "a definition spanning lines lands with its `in` on a line of its own",
    `Quick,
    () => {
      let (program, _) =
        reply(
          ToolExamples.shapes,
          [
            (
              "insert",
              [
                ("after", "area"),
                (
                  "code",
                  "let is_round(s: Shape): Bool =\n  case s\n  | Circle(_) => true\n  | Rect(_, _) => false\n  end",
                ),
              ],
            ),
          ],
        );
      mentions(
        "program",
        "  | Rect(_, _) => false\n  end\nin\nhint \"rect area\" test\n  area",
        program,
      );
    },
  ),
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
          Rename("total", None, "sum"),
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

/* a reply of one call on [code]: the program after and its result */
let one = (code, tool, args) => {
  let (program, results) = reply(code, [(tool, args)]);
  (program, List.hd(results));
};

let bound_renames = [
  test_case(
    "a parameter renames with its uses",
    `Quick,
    () => {
      let (program, r) =
        one(
          "let f(x) = x + 1 in f(2)",
          "rename",
          [("target", "f"), ("name", "x"), ("to", "z")],
        );
      same_text("program", "let f(z) = z + 1 in f(2)", program);
      mentions("result", "Renamed x to z in f, and its 1 use.", r.content);
    },
  ),
  test_case(
    "a destructured name, and a `fun` head's parameter",
    `Quick,
    () => {
      let (program, _) =
        one(
          "let (a, b) = (1, 2) in a + b",
          "rename",
          [("target", "a"), ("name", "b"), ("to", "c")],
        );
      same_text("destructured", "let (a, c) = (1, 2) in a + c", program);
      let (program, _) =
        one(
          "let g = fun y -> y * 2 in g(1)",
          "rename",
          [("target", "g"), ("name", "y"), ("to", "w")],
        );
      same_text("fun head", "let g = fun w -> w * 2 in g(1)", program);
    },
  ),
  test_case(
    "a name the pattern doesn't bind is refused",
    `Quick,
    () => {
      let code = "let f(x) = let q = x in q in f(1)";
      let (program, r) =
        one(
          code,
          "rename",
          [("target", "f"), ("name", "q"), ("to", "z")],
        );
      same_text("program unchanged", code, program);
      check(bool, "refused", false, r.success);
      mentions(
        "result",
        "\"f\" binds no q; its pattern binds f, x",
        r.content,
      );
    },
  ),
  test_case(
    "a parameter's rename that would capture is refused",
    `Quick,
    () => {
      let code = "let y = 1 in let f(x) = x + y in f(y)";
      let (program, r) =
        one(
          code,
          "rename",
          [("target", "f"), ("name", "x"), ("to", "y")],
        );
      same_text("program unchanged", code, program);
      mentions("result", "Not renamed to y", r.content);
    },
  ),
];

let rebinding = [
  test_case(
    "a pattern replaced so a use reads an outer binder is refused",
    `Quick,
    () => {
      let code = "let x = 1 in let f(x) = x + 1 in f(x)";
      let (program, r) =
        one(code, "replace", [("target", "f:pattern"), ("code", "f(z)")]);
      same_text("program unchanged", code, program);
      check(bool, "refused", false, r.success);
      mentions("result", "x at f: bound at f before, at x after", r.content);
    },
  ),
  test_case(
    "a pattern replaced so its new binder takes a use is refused",
    `Quick,
    () => {
      let code = "let x = 1 in let f(y) = x + 1 in f(x)";
      let (program, r) =
        one(code, "replace", [("target", "f:pattern"), ("code", "f(x)")]);
      same_text("program unchanged", code, program);
      mentions("result", "x at f: bound at x before, at f after", r.content);
    },
  ),
  test_case(
    "a whole entry replaced so a use reads an outer binder is refused",
    `Quick,
    () => {
      let code = "let x = 0 in let x = 1 in x";
      let (program, r) =
        one(code, "replace", [("target", "x#2"), ("code", "let y = 1")]);
      same_text("program unchanged", code, program);
      mentions("result", "uses outside it would change", r.content);
    },
  ),
  test_case(
    "shadowing that changes no use is fine",
    `Quick,
    () => {
      let (program, r) =
        one(
          "let x = 1 in let f(y) = y + 1 in f(x)",
          "replace",
          [("target", "f"), ("code", "let f(x) = x + 1")],
        );
      check(bool, "applied: " ++ r.content, true, r.success);
      same_text("program", "let x = 1 in let f(x) = x + 1 in f(x)", program);
      let (program, r) =
        one(
          "let f(x) = x + 1 in f(2)",
          "replace",
          [("target", "f:pattern"), ("code", "f(x: Int)")],
        );
      check(bool, "annotated: " ++ r.content, true, r.success);
      let (_, r) =
        one(
          "let f(y) = x + 1 in f(2)",
          "replace",
          [("target", "f:pattern"), ("code", "f(x)")],
        );
      check(bool, "an unbound use bound: " ++ r.content, true, r.success);
      same_text("annotated", "let f(x: Int) = x + 1 in f(2)", program);
    },
  ),
  test_case(
    "a use replaced in the same reply is the new code's",
    `Quick,
    () => {
      let (program, _) =
        reply(
          "let x = 1 in let f(x) = x + 1 in f(x)",
          [
            ("replace", [("target", "f:pattern"), ("code", "f(z)")]),
            ("replace", [("target", "f:definition"), ("code", "z + 1")]),
          ],
        );
      same_text("program", "let x = 1 in let f(z) = z + 1 in f(x)", program);
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
        "let total = fun xs -> 0 in\n\nlet area = fun s -> s * s in",
        program,
      );
      let (program, _) =
        reply(geo, [("move", [("target", "Geo"), ("before", "area")])]);
      mentions(
        "before",
        "module Geo = { let perimeter = 4 } in\n\nlet area",
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
        "module Geo = { let perimeter = 4 } in\n\nlet area = fun s -> s * s in\n\n?",
        program,
      );
    },
  ),
  test_case(
    "\"/\" names the top level, as \"\" does",
    `Quick,
    () => {
      let moved = top =>
        fst(
          reply(geo, [("move", [("target", "area"), ("last_in", top)])]),
        );
      check(string, "same program", moved(""), moved("/"));
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

/* what a refused or mistaken call is told */
let feedback = [
  test_case(
    "code with `\\n` written as two characters is refused, saying so",
    `Quick,
    () => {
      let (_, results) =
        reply(
          "let a = 1 in a",
          [
            (
              "insert",
              [("after", "a"), ("code", "let b = 2 in\\nlet c = 3")],
            ),
          ],
        );
      mentions(
        "result",
        "`code` contains `\\n` as two characters; write real line breaks (and plain quotes).",
        List.hd(results).content,
      );
      check(
        list(bool),
        "escapes outside strings only",
        [true, true, false, false],
        List.map(
          CompositionGo.escaped,
          [
            "let b = 2 in\\nb",
            "hint \\\"x\\\" test true end",
            "let s = \"a\\nb\" in s",
            "let s = \"a\\\"b\" in s",
          ],
        ),
      );
    },
  ),
  test_case(
    "a whole-entry replace keeps the comment above it",
    `Quick,
    () => {
      let code = "module W = {\n  # two digits #\n  let pad(n: Int): Int = n;\n  let z = 1\n} in\nW.pad(1)";
      let (program, results) =
        reply(
          code,
          [
            (
              "replace",
              [
                ("target", "W/pad"),
                ("code", "let pad(n: Int): Int = n + 1"),
              ],
            ),
          ],
        );
      mentions(
        "program",
        "  # two digits #\n  let pad(n: Int): Int = n + 1;",
        program,
      );
      mentions(
        "result",
        "Replaced W/pad, kept the comment above it.",
        List.hd(results).content,
      );
      let (program, _) =
        reply(
          code,
          [
            (
              "replace",
              [
                ("target", "W/pad"),
                ("code", "# pads #\nlet pad(n: Int): Int = n + 1"),
              ],
            ),
          ],
        );
      check(
        bool,
        "the old comment replaced",
        false,
        has("two digits", program),
      );
      mentions(
        "program",
        "  # pads #\n  let pad(n: Int): Int = n + 1;",
        program,
      );
    },
  ),
  test_case(
    "the same refused reply again is told so; five in a row end the turn",
    `Quick,
    () => {
      let bad = [
        call(0, "insert", [("after", "a"), ("code", "let b = in")]),
      ];
      let other = [
        call(0, "insert", [("after", "a"), ("code", "let b = 1 +")]),
      ];
      let first = ((_, trs)) =>
        (List.hd(trs): AgentToolResult.tool_result).content;
      let last_agent = (agent: Agent.Model.t) =>
        Chat.Utils.linearize(
          ChatSystem.Utils.find_chat(
            agent.chat_system.current,
            agent.chat_system,
          ),
        )
        |> List.filter_map((m: Message.Model.t) =>
             switch (m.role) {
             | Agent(_) => Some(m.content)
             | _ => None
             }
           )
        |> ListUtil.last_opt;
      let notice = AgentToolExec.Repeats.notice;
      let (rs, agent) = run("let a = 1 in a", [bad, bad, other, bad, bad]);
      check(
        list(bool),
        "noticed: only the same as the last",
        [false, true, false, false, true],
        List.map(r => String.starts_with(~prefix=notice, first(r)), rs),
      );
      check(
        bool,
        "not stopped",
        false,
        Option.fold(~none=false, ~some=has("Stopped"), last_agent(agent)),
      );
      let (_, agent) =
        run(
          "let a = 1 in a",
          List.init(AgentToolExec.Repeats.limit, _ => bad),
        );
      mentions(
        "the note to the user",
        "Stopped: the agent sent the same calls 5 times in a row",
        Option.value(last_agent(agent), ~default=""),
      );
      check(bool, "turn ended", true, agent.awaiting_response == None);
    },
  ),
  test_case(
    "the tests' line names the failing tests, newly failing first",
    `Quick,
    () => {
      let agent = Agent.Utils.init();
      let chat_id = agent.chat_system.current;
      let ce =
        evaluated(
          CellEditor.Model.mk(
            Editor.Model.mk(
              mk_zipper(
                "let double = fun x -> 2 * x in\ntest double(1) == 3 end;\nhint \"zero\" test double(0) == 0 end;\ndouble(5)",
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
                    ("code", "fun x -> 3 * x + 1"),
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
          Settings.Model.init,
          _ =>
          ()
        );
      let chat_system =
        AgentToolExec.Tests.report(
          ~chat_id,
          EvalResult.Model.test_results(evaluated(ce'.model).result),
          agent'.chat_system,
        );
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
        "Tests changed: \"zero\" passing → failing. Failing: \"zero\" (newly), test#1.",
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

/* a replayed session from an empty program: the layout around the
   agent's code is house style, comments don't pile up, and entries
   without separators get a message that says so */
let repro = [
  test_case(
    "a session from an empty program",
    `Quick,
    () => {
      let insert = (place, code) => ("insert", [place, ("code", code)]);
      let reply = calls => List.mapi((i, (n, a)) => call(i, n, a), calls);
      let redoc =
        reply([
          (
            "replace",
            [
              ("target", "area:definition"),
              (
                "code",
                "# approximate area, pi taken as 3; lines have none #\ncase s\n| Circle(_, r) => 3 * r * r\n| Rect(_, _) => 2\n| Line(_, _) => 0\nend",
              ),
            ],
          ),
        ]);
      let steps =
        replies(
          "?",
          [
            reply([
              insert(
                ("first_in", "/"),
                "type Point = (Int, Int)\ntype Style = (Int, Bool)",
              ),
            ]),
            reply([
              insert(
                ("first_in", "/"),
                "type Point = (Int, Int) in\ntype Style = (Int, Bool) in\ntype Shape =\n  + Circle(Point, Int)\n  + Rect(Point, Point)\n  + Line(Point, Point)",
              ),
            ]),
            reply([
              insert(
                ("after", "Shape"),
                "let area(s: Shape): Int =\n  # approximate area, pi taken as 3 #\n  case s\n  | Circle(_, r) => 3 * r * r\n  | Rect(_, _) => 1\n  | Line(_, _) => 0\n  end",
              ),
              insert(
                ("after", "area"),
                "hint \"circle area\" test area(Circle((0, 0), 2)) == 12 end",
              ),
            ]),
            redoc,
            redoc,
            reply([
              insert(
                ("after", "\"circle area\""),
                "let double(n: Int): Int =\n  # twice n #\n  2 * n",
              ),
            ]),
          ],
        );
      let result = (k, i) =>
        List.nth(snd(List.nth(steps, k)), i).AgentToolResult.content;
      mentions(
        "entries without `in`",
        "Entries in one `code` need `in` between them (`;` in a module); leave it off the last.",
        result(0, 0),
      );
      mentions(
        "the first replace",
        "- # approximate area, pi taken as 3 #",
        result(3, 0),
      );
      mentions(
        "the second replace",
        "Replaced area:definition. Static",
        result(4, 0),
      );
      check(
        string,
        "the program",
        {|type Point = (Int, Int) in

type Style = (Int, Bool) in

type Shape =
  + Circle(Point, Int)
  + Rect(Point, Point)
  + Line(Point, Point)
in

let area(s: Shape): Int =
  # approximate area, pi taken as 3; lines have none #
  case s
  | Circle(_, r) => 3 * r * r
  | Rect(_, _) => 2
  | Line(_, _) => 0
  end
in
hint "circle area" test area(Circle((0, 0), 2)) == 12 end;

let double(n: Int): Int =
  # twice n #
  2 * n
in

?|},
        fst(ListUtil.last(steps)),
      );
    },
  ),
];

let endings = [
  test_case(
    "a trailing expression fills the program's empty result",
    `Quick,
    () => {
      let (program, results) =
        reply(
          "?",
          [
            (
              "insert",
              [
                ("first_in", "/"),
                (
                  "code",
                  "let celsius = 0 in\nlet f(c: Int): Int = 32 + 9 * c / 5 in\nf(celsius)",
                ),
              ],
            ),
          ],
        );
      check(
        string,
        "program",
        "let celsius = 0 in\n\nlet f(c: Int): Int = 32 + 9 * c / 5 in\n\nf(celsius)",
        program,
      );
      mentions(
        "its result",
        "Added celsius, f. The last line became the result (`=>`).",
        List.hd(results).content,
      );
      let (program, _) =
        reply(
          "let a = 1 in\n?",
          [
            ("insert", [("last_in", "/"), ("code", "let b = a in\nb + 1")]),
          ],
        );
      check(
        string,
        "last in",
        "let a = 1 in\n\nlet b = a in\n\nb + 1",
        program,
      );
      let (program, _) =
        reply(
          "?",
          [("insert", [("first_in", "/"), ("code", "test 1 == 1 end")])],
        );
      check(string, "a test stays a test", "test 1 == 1 end;\n\n?", program);
    },
  ),
  test_case(
    "a trailing expression fills a block's empty result",
    `Quick,
    () => {
      let (program, results) =
        reply(
          "let f(x: Int): Int = ? in f(1)",
          [("insert", [("last_in", "f"), ("code", "x + 1")])],
        );
      same_text("program", "let f(x: Int): Int = x + 1 in f(1)", program);
      mentions(
        "its result",
        "The code became the result (`f:definition`).",
        List.hd(results).content,
      );
      let (program, results) =
        reply(
          "let f(x: Int): Int = ? in f(1)",
          [
            (
              "insert",
              [("last_in", "f"), ("code", "let y = x + 1 in\ny * 2")],
            ),
          ],
        );
      same_text(
        "program",
        "let f(x: Int): Int = let y = x + 1 in y * 2 in f(1)",
        program,
      );
      mentions(
        "its result",
        "Added f/y. The last line became the result (`f/=>`).",
        List.hd(results).content,
      );
    },
  ),
  test_case(
    "a trailing expression beside a real result is a statement, said plainly",
    `Quick,
    () => {
      let (program, results) =
        reply(
          "let a = 1 in a",
          [("insert", [("after", "a"), ("code", "let b = 2 in\nb + 1")])],
        );
      mentions("program", "b + 1;", program);
      mentions(
        "its result",
        "Added b, stmt#1. The last line became a statement (stmt#1); to change the result, replace `=>`.",
        List.hd(results).content,
      );
      let (_, results) =
        reply(
          "let f(x: Int): Int =\n  x\nin\nf(1)",
          [
            (
              "insert",
              [("last_in", "f"), ("code", "let y = x + 1 in\ny * 2")],
            ),
          ],
        );
      mentions(
        "inside a block",
        "(f/stmt#1); to change the result, replace `f/=>`.",
        List.hd(results).content,
      );
    },
  ),
  test_case(
    "a path written as code is told what a path is",
    `Quick,
    () => {
      let hint = "A path names an entry; an unnamed test is `test#k` (its path is shown on its line in the map).";
      let code = "let a = 1 in\ntest a == 1 end;\na";
      let first = calls => List.hd(snd(reply(code, calls))).content;
      mentions("a call", hint, first([("delete", [("target", "a(1)")])]));
      mentions(
        "a test's code",
        hint,
        first([("delete", [("target", "test a == 1 end")])]),
      );
      check(
        bool,
        "not for a plain name",
        false,
        has(hint, first([("delete", [("target", "nope")])])),
      );
    },
  ),
];

let tests = [
  ("AgentReply.PromptExamples", prompt_examples),
  ("AgentReply.Transactions", transactions),
  ("AgentReply.TestResults", test_results),
  ("AgentReply.Feedback", feedback),
  ("AgentReply.DeletesAndRenames", deletes_and_renames),
  ("AgentReply.BoundRenames", bound_renames),
  ("AgentReply.Rebinding", rebinding),
  ("AgentReply.TestsByHint", tests_by_hint),
  ("AgentReply.Move", moves),
  ("AgentReply.Repro", repro),
  ("AgentReply.Endings", endings),
];
