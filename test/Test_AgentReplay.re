/** Unit tests for the agent replay feature: replay step derivation from chat
    tool results (Agent.Replay) and the scripted selector-tool benchmark task
    (AgentBenchmark), verifying every scripted tool call applies successfully
    and produces the expected program at each step. */
open Alcotest;
open Haz3lcore;
open Language;
open Util;
open Web;

/* -------------------------------------------------------------------------- */
/* Helpers */

let mk_zipper = (code: string): Zipper.t => {
  switch (Parser.to_zipper(~root=Exp, code)) {
  | Some(z) => z
  | None => Alcotest.fail("Failed to parse: " ++ code)
  };
};

let render_zipper = (z: Zipper.t): string =>
  Printer.of_zipper(~holes="?", ~indent=" ", z);

let check_rendered = (name: string, expected: string, actual: string) => {
  let normalized = s =>
    s
    |> StringUtil.trim_trailing_whitespace
    |> StringUtil.replace(StringUtil.regexp("[\\s]+"), _, " ")
    |> String.trim;
  check(
    testable(Fmt.string, (a, b) =>
      String.equal(normalized(a), normalized(b))
    ),
    name,
    expected,
    actual,
  );
};

let segment_of_code = (code: string): Segment.t => {
  let z = mk_zipper(code);
  Select.all(z).selection.content;
};

let mk_tool_call =
    (~id: string="tc", ~args: API.Json.t=`Assoc([]), name: string)
    : OpenRouter.Reply.Model.tool_call => {
  id,
  name,
  args,
};

let mk_tool_result =
    (
      ~name: string,
      ~success: bool=true,
      ~before: option(string)=None,
      ~after: option(string)=None,
      ~before_cursor_id: option(Id.t)=None,
      ~after_cursor_id: option(Id.t)=None,
      (),
    )
    : AgentToolResult.tool_result => {
  tool_call: mk_tool_call(name),
  success,
  expanded: false,
  diff: None,
  before_segment: Option.map(segment_of_code, before),
  after_segment: Option.map(segment_of_code, after),
  before_cursor_id,
  after_cursor_id,
  content: "content",
};

/* -------------------------------------------------------------------------- */
/* Replay.Utils.is_edit_tool */

let is_edit_tool_tests = [
  test_case("is_edit_tool: selector-based edit tools are included", `Quick, () => {
    List.iter(
      name => check(bool, name, true, Agent.Replay.Utils.is_edit_tool(name)),
      [
        "selector_update",
        "selector_delete",
        "overwrite",
        "initialize",
        "update_definition",
        "update_type_annotation",
      ],
    )
  }),
  test_case(
    "is_edit_tool: read/workbench/probe tools are excluded", `Quick, () => {
    List.iter(
      name =>
        check(bool, name, false, Agent.Replay.Utils.is_edit_tool(name)),
      [
        "get_syntax",
        "select",
        "get_canonical",
        "place_probe",
        "create_new_task",
        "expand",
      ],
    )
  }),
];

/* -------------------------------------------------------------------------- */
/* Replay.Utils.steps_of_chat */

/* Chat transcript: user msg, agent msg, then tool results:
   1. successful selector_update (edit -> replay step)
   2. successful get_syntax (read -> skipped)
   3. failed selector_delete (skipped)
   4. successful overwrite (edit -> replay step) */
let mk_replay_chat = (): Agent.Chat.Model.t => {
  let chat = Agent.Chat.Utils.init(~system_prompt="sp", ~dev_notes="dn");
  let chat =
    Agent.Chat.Utils.append(
      Agent.Message.Utils.mk_user_message("please edit"),
      chat,
    );
  let chat =
    Agent.Chat.Utils.append(
      Agent.Message.Utils.mk_agent_message("editing", None),
      chat,
    );
  let append_tool_result = (tr, chat) =>
    Agent.Chat.Utils.append(
      Agent.Message.Utils.mk_tool_result_message(tr),
      chat,
    );
  chat
  |> append_tool_result(
       mk_tool_result(
         ~name="selector_update",
         ~before=Some("let x = 1 in x"),
         ~after=Some("let x = 2 in x"),
         (),
       ),
     )
  |> append_tool_result(mk_tool_result(~name="get_syntax", ()))
  |> append_tool_result(
       mk_tool_result(
         ~name="selector_delete",
         ~success=false,
         ~before=Some("let x = 2 in x"),
         (),
       ),
     )
  |> append_tool_result(
       mk_tool_result(
         ~name="overwrite",
         ~before=Some("let x = 2 in x"),
         ~after=Some("let x = 2 in let y = 3 in x"),
         (),
       ),
     );
};

let steps_of_chat_tests = [
  test_case(
    "steps_of_chat: empty chat has no steps",
    `Quick,
    () => {
      let chat = Agent.Chat.Utils.init(~system_prompt="sp", ~dev_notes="dn");
      check(int, "no steps", 0, Agent.Replay.Utils.num_steps(chat));
    },
  ),
  test_case(
    "steps_of_chat: initial + successful edits only",
    `Quick,
    () => {
      let steps = Agent.Replay.Utils.steps_of_chat(mk_replay_chat());
      /* initial + 2 successful edit tools (read + failed edit skipped) */
      check(int, "step count", 3, List.length(steps));
      switch (steps) {
      | [s0, s1, s2] =>
        check(string, "step 0 label", "Initial", s0.label);
        check(int, "step 0 index", 0, s0.index);
        check(string, "step 1 label", "selector_update", s1.label);
        check(string, "step 2 label", "overwrite", s2.label);
        check(
          bool,
          "all steps have segments",
          true,
          List.for_all(
            (s: Agent.Replay.Model.step) => Option.is_some(s.segment),
            steps,
          ),
        );
      | _ => fail("expected exactly 3 steps")
      };
    },
  ),
  test_case(
    "steps_of_chat: step segments round-trip to the recorded programs",
    `Quick,
    () => {
      let steps = Agent.Replay.Utils.steps_of_chat(mk_replay_chat());
      let render_step = (s: Agent.Replay.Model.step) =>
        switch (s.segment) {
        | Some(segment) =>
          render_zipper(Zipper.unzip(~direction=Right, segment))
        | None => fail("expected segment")
        };
      switch (steps) {
      | [s0, _, s2] =>
        check_rendered("step 0 program", "let x = 1 in x", render_step(s0));
        check_rendered(
          "step 2 program",
          "let x = 2 in let y = 3 in x",
          render_step(s2),
        );
      | _ => fail("expected exactly 3 steps")
      };
    },
  ),
  test_case(
    "steps_of_chat: failed edits do not contribute steps",
    `Quick,
    () => {
      let steps = Agent.Replay.Utils.steps_of_chat(mk_replay_chat());
      check(
        bool,
        "no selector_delete step",
        true,
        List.for_all(
          (s: Agent.Replay.Model.step) => s.label != "selector_delete",
          steps,
        ),
      );
    },
  ),
  test_case(
    "steps_of_chat: cursor ids flow into steps",
    `Quick,
    () => {
      let before_id = Id.mk();
      let after_id = Id.mk();
      let chat =
        Agent.Chat.Utils.init(~system_prompt="sp", ~dev_notes="dn")
        |> Agent.Chat.Utils.append(
             Agent.Message.Utils.mk_tool_result_message(
               mk_tool_result(
                 ~name="selector_update",
                 ~before=Some("let x = 1 in x"),
                 ~after=Some("let x = 2 in x"),
                 ~before_cursor_id=Some(before_id),
                 ~after_cursor_id=Some(after_id),
                 (),
               ),
             ),
           );
      switch (Agent.Replay.Utils.steps_of_chat(chat)) {
      | [s0, s1] =>
        check(
          bool,
          "initial step carries the before-cursor id",
          true,
          s0.cursor_id == Some(before_id),
        );
        check(
          bool,
          "edit step carries the after-cursor id",
          true,
          s1.cursor_id == Some(after_id),
        );
      | _ => fail("expected exactly 2 steps")
      };
    },
  ),
];

/* -------------------------------------------------------------------------- */
/* Cursor restoration */

let cursor_restore_tests = [
  test_case(
    "cursor restore: recorded cursor id survives the segment round-trip",
    `Quick,
    () => {
      /* Take a snapshot with the caret at the end of the program (on `2`),
         then reload the segment the way replay does and check the caret
         lands back on the same node. */
      let z = mk_zipper("let x = 1 in x + 2");
      let snapshot = Agent.Replay.Utils.snapshot_of_zipper(z);
      let last_tile_id =
        snapshot.segment
        |> List.filter_map(
             fun
             | Piece.Tile(t) => Some(t.id)
             | _ => None,
           )
        |> ListUtil.last;
      let reloaded = Zipper.unzip(~direction=Right, snapshot.segment);
      switch (Move.jump_to_id_indicated(reloaded, last_tile_id)) {
      | None => fail("expected jump_to_id_indicated to succeed")
      | Some(restored) =>
        check(
          bool,
          "caret indicates the recorded node after reload",
          true,
          Indicated.index(restored) == Some(last_tile_id),
        )
      };
    },
  ),
  test_case(
    "cursor restore: snapshot_of_zipper records the indicated node",
    `Quick,
    () => {
      let z = mk_zipper("let x = 1 in x + 2");
      let snapshot = Agent.Replay.Utils.snapshot_of_zipper(z);
      check(
        bool,
        "snapshot cursor id matches the zipper's indicated node",
        true,
        snapshot.cursor_id == Indicated.index(z),
      );
    },
  ),
];

/* -------------------------------------------------------------------------- */
/* Benchmark script execution */

/* Apply one scripted benchmark tool call to a zipper, mirroring the
   agent's tool-handling path (initialize via introduce; edits via
   Perform.go on structural actions). */
let apply_benchmark_step =
    (z: Zipper.t, step: AgentBenchmark.step): result(Zipper.t, string) => {
  switch (
    CompositionUtils.Public.action_of(
      ~tool_name=step.tool_name,
      ~args=step.args,
    )
  ) {
  | CompositionUtils.Failure(msg) => Error(msg)
  | CompositionUtils.Action(CompositionActions.Initialize(code)) =>
    let return = (error: Action.Failure.t, z: option(Zipper.t)) =>
      Result.of_option(~error, z);
    switch (
      CompositionGo.Local.PerformUtils.introduce(Select.all(z), code, return)
    ) {
    | Ok(new_z) => Ok(Dump.to_zipper(new_z, ~root=Exp))
    | Error(err) => Error(Action.Failure.show(err))
    };
  | CompositionUtils.Action(CompositionActions.EditorAction(a)) =>
    switch (
      Perform.go(
        ~settings=CoreSettings.on,
        ~statics=CachedStatics.empty,
        ~syntax=CachedSyntax.init(z),
        ~root=Exp,
        Structural(a),
        {
          zipper: z,
          col_target: None,
        },
      )
    ) {
    | Ok(z) => Ok(z)
    | Error(err) => Error(Action.Failure.show(err))
    }
  | CompositionUtils.Action(_) =>
    Error("Unexpected non-edit action in benchmark: " ++ step.tool_name)
  };
};

/* Expected program after each of the 7 scripted steps */
let benchmark_expected: list(string) = [
  "let x = 42 in x + 1",
  "let double = fun n -> n * 2 in let x = 42 in x + 1",
  "let double = fun n -> n * 2 in let x = double(21) in x + 1",
  "let double = fun n -> n * 2 in let x = double(21) in let y = double(x) in x + 1",
  "let double = fun n -> n * 2 in let x = double(21) in let y = double(x) + 1 in x + 1",
  "let double = ? in let x = double(21) in let y = double(x) + 1 in x + 1",
  "let double = fun n -> n + n in let x = double(21) in let y = double(x) + 1 in x + 1",
];

let benchmark_tests = [
  test_case(
    "selector_demo: only initialize + selector tools",
    `Quick,
    () => {
      let names =
        List.map(
          (s: AgentBenchmark.step) => s.tool_name,
          AgentBenchmark.selector_demo.steps,
        );
      check(
        bool,
        "all steps are edit tools",
        true,
        List.for_all(Agent.Replay.Utils.is_edit_tool, names),
      );
      check(
        bool,
        "uses selector tools",
        true,
        List.exists(
          name =>
            String.length(name) >= 8 && String.sub(name, 0, 8) == "selector",
          names,
        ),
      );
    },
  ),
  test_case(
    "selector_demo: every scripted step applies and matches the expected program",
    `Quick,
    () => {
      check(
        int,
        "expected list matches step count",
        List.length(AgentBenchmark.selector_demo.steps),
        List.length(benchmark_expected),
      );
      let _ =
        List.fold_left2(
          (z, step: AgentBenchmark.step, expected) => {
            switch (apply_benchmark_step(z, step)) {
            | Error(msg) =>
              fail(
                "Benchmark step failed (" ++ step.tool_name ++ "): " ++ msg,
              )
            | Ok(new_z) =>
              check_rendered(
                "after " ++ step.tool_name,
                expected,
                render_zipper(new_z),
              );
              new_z;
            }
          },
          Zipper.init(),
          AgentBenchmark.selector_demo.steps,
          benchmark_expected,
        );
      ();
    },
  ),
];

let tests = (
  "AgentReplay",
  is_edit_tool_tests
  @ steps_of_chat_tests
  @ cursor_restore_tests
  @ benchmark_tests,
);
