open Alcotest;
open Haz3lcore;
open Util;

module SystemOne = OpenRouter.SystemOne;

let mk_zipper = (code: string): Zipper.t =>
  switch (Parser.to_zipper(~root=Exp, code)) {
  | Some(z) => z
  | None => Alcotest.fail("Failed to parse: " ++ code)
  };

/* The fix-middle shape: [billed] adds where it should multiply. */
let billing_program = {|let volume = fun (a, b, c) -> a * b * c in
let cost = fun (a, b, c) -> a + b + c in
let billed = fun r -> volume(r) + cost(r) in
billed((1, 2, 3))|};

let fix_request: JevEdit.request = {
  path: "billed",
  sketch: "fun r -> ? * cost(r)",
  names: [],
  literals: [],
  intent: "billed should multiply volume by cost",
  signature: "",
};

let usage: SystemOne.usage = {
  input_tokens: 7,
  cost_usd: Some(0.25),
};

/* Evaluates the whole program; the tests' notion of "the edit is right". */
let evaluate = (z: Zipper.t): string => {
  open Language;
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  let (_, elaborated) =
    Statics.mk(
      CoreSettings.on,
      Builtins.ctx_init(Some(Operators.default_mode)),
      term,
    );
  let (result, _) = Evaluator.evaluate(~env=Builtins.env_init, elaborated);
  switch (Exp.term_of(result)) {
  | Atom(Int(n)) => Bigint.to_string(n)
  | _ => "not an Int"
  };
};

let contains = (needle: string, haystack: string): bool =>
  switch (Str.search_forward(Str.regexp_string(needle), haystack, 0)) {
  | _ => true
  | exception Not_found => false
  };

/* Synchronous stand-in for Jev: per question, the first of [preferred]
   that is on offer, else escalate. */
let fake_choices =
    (~calls: ref(int), ~confidence=0.9, preferred: list(string))
    : JevEdit.decide_choices =>
  (~state as _, ~questions, ~handler) => {
    calls := calls^ + 1;
    handler(
      Chosen(
        List.map(
          (q: SystemOne.choice_question): SystemOne.choice_answer =>
            {
              id: q.id,
              choice:
                List.find_opt(p => List.mem(p, q.options), preferred)
                |> Option.value(~default=JevEdit.escalate),
              confidence,
            },
          questions,
        ),
        usage,
      ),
    );
  };

/* Answers every question with [text], on offer or not: exercises the fill
   guard the way a misbehaving model would. */
let fake_answer = (text: string): JevEdit.decide_choices =>
  (~state as _, ~questions, ~handler) =>
    handler(
      Chosen(
        List.map(
          (q: SystemOne.choice_question): SystemOne.choice_answer =>
            {
              id: q.id,
              choice: text,
              confidence: 0.9,
            },
          questions,
        ),
        usage,
      ),
    );

/* [r] also type-checks at the first hole (its type is unknown), so the
   order is what makes this the intended fix. */
let volume_then_r = ["volume(?)", "r"];

let run_edit =
    (~max_rounds=4, ~request=fix_request, decide_choices): JevEdit.outcome => {
  let result = ref(None);
  JevEdit.edit(
    ~decide_choices,
    ~max_rounds,
    ~context="(view)",
    ~request,
    ~on_done=o => result := Some(o),
    mk_zipper(billing_program),
  );
  switch (result^) {
  | Some(o) => o
  | None => Alcotest.fail("edit did not call on_done")
  };
};

let question: SystemOne.choice_question = {
  id: "hole_1",
  instructions: "which?",
  options: ["f(x, y)", "<escalate>"],
};

let choice_tests = (
  "JevEdit.Choice",
  [
    test_case("request uses positional keys", `Quick, () =>
      check(
        string,
        "body",
        {|{"model":"m","state":null,"questions":{"hole_1":{"type":"choice","instructions":"which?","criteria":{"c0":"f(x, y)","c1":"<escalate>"}}}}|},
        API.Json.to_string(
          SystemOne.json_of_choice_request(
            ~model_id="m",
            ~state=`Null,
            [question],
          ),
        ),
      )
    ),
    test_case(
      "reply maps keys back to option text",
      `Quick,
      () => {
        let json =
          API.Json.from_string(
            {|{"answers":{"hole_1":{"type":"choice","choice":"c0","confidence":0.8,"probabilities":{"c0":0.8,"c1":0.2}}},
             "usage":{"input_tokens":5,"cost":0.01}}|},
          );
        switch (
          SystemOne.choice_reply_of_json(~questions=[question], Some(json))
        ) {
        | Chosen([a], u) =>
          check(string, "choice", "f(x, y)", a.choice);
          check(float(1e-9), "confidence", 0.8, a.confidence);
          check(int, "tokens", 5, u.input_tokens);
        | Chosen(_) => Alcotest.fail("expected one answer")
        | ChoiceFailed(_, msg) => Alcotest.fail(msg)
        };
      },
    ),
    test_case(
      "unknown key, missing answer and error body fail",
      `Quick,
      () => {
        let failed = (body: option(string)) =>
          switch (
            SystemOne.choice_reply_of_json(
              ~questions=[question],
              Option.map(API.Json.from_string, body),
            )
          ) {
          | ChoiceFailed(_) => true
          | Chosen(_) => false
          };
        check(
          bool,
          "unknown key",
          true,
          failed(
            Some({|{"answers":{"hole_1":{"choice":"c9","confidence":1}}}|}),
          ),
        );
        check(bool, "missing", true, failed(Some({|{"answers":{}}|})));
        check(
          bool,
          "error",
          true,
          failed(Some({|{"error":{"code":402,"message":"no credit"}}|})),
        );
        check(bool, "no response", true, failed(None));
      },
    ),
    test_case(
      "question caps options and keeps escalate last",
      `Quick,
      () => {
        let q =
          JevEdit.question_of({
            hole_id: "hole_1",
            expected_type: "Int",
            candidates: List.init(300, string_of_int),
          });
        check(int, "options", 255, List.length(q.options));
        check(string, "last", JevEdit.escalate, List.nth(q.options, 254));
        check(
          bool,
          "names the hole",
          true,
          Str.string_match(Str.regexp(".*`hole_1`"), q.instructions, 0),
        );
      },
    ),
  ],
);

let holes_tests = (
  "JevEdit.holes",
  [
    test_case(
      "sketch hole is typed and offers in-scope functions",
      `Quick,
      () => {
        let program = {|let volume = fun (a, b, c) -> a * b * c in
let cost = fun (a, b, c) -> a + b + c in
let billed = fun r -> ? * cost(r) in
billed((1, 2, 3))|};
        switch (JevEdit.holes_in(~path="billed", mk_zipper(program))) {
        | [h] =>
          check(string, "id", "hole_1", h.hole_id);
          check(string, "type", "Int", h.expected_type);
          check(
            bool,
            "volume(?) offered",
            true,
            List.mem("volume(?)", h.candidates),
          );
          check(
            bool,
            "no keyword forms",
            false,
            List.mem("fun ", h.candidates),
          );
          check(
            bool,
            "no builtins",
            false,
            List.exists(String.starts_with(~prefix="string_"), h.candidates),
          );
        | hs => Alcotest.failf("expected 1 hole, got %d", List.length(hs))
        };
      },
    ),
    test_case("holes outside the target are ignored", `Quick, () =>
      check(
        int,
        "holes",
        0,
        List.length(
          JevEdit.holes_in(
            ~path="a",
            mk_zipper("let a = 1 in let b = ? in b"),
          ),
        ),
      )
    ),
  ],
);

let edit_tests = (
  "JevEdit.edit",
  [
    test_case(
      "fills the sketch over two rounds and fixes the bug",
      `Quick,
      () => {
        ignore(JevEdit.Log.drain());
        check(string, "buggy", "12", evaluate(mk_zipper(billing_program)));
        let calls = ref(0);
        let o = run_edit(fake_choices(~calls, volume_then_r));
        check(bool, "failed", false, o.metrics.failed);
        check(string, "fixed", "36", evaluate(o.zipper));
        check(int, "rounds", 2, o.metrics.rounds);
        check(int, "requests", 2, calls^);
        check(int, "filled", 2, o.metrics.filled);
        check(int, "tokens", 14, o.metrics.input_tokens);
        check(int, "logged", 1, List.length(JevEdit.Log.drain()));
      },
    ),
    test_case(
      "state shows where each hole sits",
      `Quick,
      () => {
        let states = ref([]);
        let recording: JevEdit.decide_choices =
          (~state, ~questions, ~handler) => {
            states := states^ @ [state];
            fake_choices(
              ~calls=ref(0),
              volume_then_r,
              ~state,
              ~questions,
              ~handler,
            );
          };
        ignore(run_edit(recording));
        let current = (state: API.Json.t) =>
          Option.bind(
            API.Json.dot("current_definition", state),
            API.Json.str,
          )
          |> Option.value(~default="");
        check(
          list(string),
          "current definition per round",
          [
            "let billed = fun r -> hole_1 * cost(r) in",
            "let billed = fun r -> volume(hole_1) * cost(r) in",
          ],
          List.map(state => String.trim(current(state)), states^),
        );
        ignore(JevEdit.Log.drain());
      },
    ),
    test_case(
      "escalate leaves the hole and is not re-asked",
      `Quick,
      () => {
        let calls = ref(0);
        let o = run_edit(fake_choices(~calls, []));
        check(int, "one request", 1, calls^);
        check(int, "filled", 0, o.metrics.filled);
        check(
          list(string),
          "escalated",
          ["hole_1"],
          List.map((h: JevEdit.hole) => h.hole_id, o.metrics.escalated),
        );
        check(bool, "failed", false, o.metrics.failed);
        ignore(JevEdit.Log.drain());
      },
    ),
    test_case(
      "an ill-typed planner literal escalates instead of breaking types",
      `Quick,
      () => {
        let calls = ref(0);
        let o =
          run_edit(
            ~request={
              ...fix_request,
              literals: ["\"oops\""],
            },
            fake_choices(~calls, ["\"oops\""]),
          );
        check(int, "filled", 0, o.metrics.filled);
        check(int, "escalated", 1, List.length(o.metrics.escalated));
        check(
          int,
          "no new static errors",
          0,
          List.length(
            ErrorPrint.all(CompositionGo.Public.mk_statics(o.zipper)),
          ),
        );
        ignore(JevEdit.Log.drain());
      },
    ),
    test_case(
      "a new path creates code, even in an empty program",
      `Quick,
      () => {
        let calls = ref(0);
        let result = ref(None);
        JevEdit.edit(
          ~decide_choices=fake_choices(~calls, ["x"]),
          ~context="(view)",
          ~request={
            path: "double",
            sketch: "let double(x: Int): Int = ? + x in",
            names: [],
            literals: [],
            intent: "double a number",
            signature: "",
          },
          ~on_done=o => result := Some(o),
          mk_zipper("?"),
        );
        switch (result^) {
        | None => Alcotest.fail("edit did not call on_done")
        | Some(o) =>
          check(option(string), "error", None, o.metrics.error);
          check(bool, "failed", false, o.metrics.failed);
          check(int, "one owned hole", 1, o.metrics.holes_seen);
          check(int, "filled", 1, o.metrics.filled);
          check(
            bool,
            "binding now exists",
            true,
            List.length(JevEdit.holes_in(~path="double", o.zipper)) == 0
            && contains(
                 "x + x",
                 CompositionView.Public.print_zipper(o.zipper),
               ),
          );
        };
        ignore(JevEdit.Log.drain());
      },
    ),
    test_case(
      "a new path goes after the last binding and can use it",
      `Quick,
      () => {
        let result = ref(None);
        JevEdit.edit(
          ~decide_choices=fake_choices(~calls=ref(0), ["base"]),
          ~context="(view)",
          ~request={
            path: "twice",
            sketch: "let twice: Int = ? * 2 in",
            names: [],
            literals: [],
            intent: "twice the base",
            signature: "",
          },
          ~on_done=o => result := Some(o),
          mk_zipper("let base: Int = 21 in\nbase"),
        );
        switch (result^) {
        | None => Alcotest.fail("edit did not call on_done")
        | Some(o) =>
          check(option(string), "error", None, o.metrics.error);
          check(int, "filled", 1, o.metrics.filled);
          check(
            bool,
            "uses base",
            true,
            contains(
              "base * 2",
              CompositionView.Public.print_zipper(o.zipper),
            ),
          );
        };
        ignore(JevEdit.Log.drain());
      },
    ),
    test_case(
      "low confidence escalates",
      `Quick,
      () => {
        let calls = ref(0);
        let o =
          run_edit(fake_choices(~calls, ~confidence=0.2, volume_then_r));
        check(int, "filled", 0, o.metrics.filled);
        check(int, "escalated", 1, List.length(o.metrics.escalated));
        ignore(JevEdit.Log.drain());
      },
    ),
    test_case(
      "max_rounds caps the loop",
      `Quick,
      () => {
        let calls = ref(0);
        let o = run_edit(~max_rounds=1, fake_choices(~calls, volume_then_r));
        check(int, "requests", 1, calls^);
        check(int, "rounds", 1, o.metrics.rounds);
        check(int, "filled", 1, o.metrics.filled);
        ignore(JevEdit.Log.drain());
      },
    ),
    test_case(
      "failed Choice returns the program before the sketch",
      `Quick,
      () => {
        let failing: JevEdit.decide_choices =
          (~state as _, ~questions as _, ~handler) =>
            handler(ChoiceFailed(500, "boom"));
        let o = run_edit(failing);
        check(bool, "failed", true, o.metrics.failed);
        check(string, "unchanged", "12", evaluate(o.zipper));
        ignore(JevEdit.Log.drain());
      },
    ),
    test_case(
      "rejected sketch fails without asking Jev",
      `Quick,
      () => {
        let calls = ref(0);
        let o =
          run_edit(
            ~request={
              ...fix_request,
              sketch: "fun r -> true + cost(r)",
            },
            fake_choices(~calls, ["r"]),
          );
        check(bool, "failed", true, o.metrics.failed);
        check(int, "no requests", 0, calls^);
        ignore(JevEdit.Log.drain());
      },
    ),
  ],
);

/* Jev building structure: a scripted fake answers round k with the first
   of [script[k]] on offer, since the same forms reappear at new holes. */
let scripted =
    (~calls: ref(int), script: list(list(string))): JevEdit.decide_choices =>
  (~state, ~questions, ~handler) => {
    let preferred =
      List.nth_opt(script, calls^) |> Option.value(~default=[]);
    fake_choices(~calls, preferred, ~state, ~questions, ~handler);
  };

let build =
    (~request: JevEdit.request, ~program="?", decide_choices): JevEdit.outcome => {
  let result = ref(None);
  JevEdit.edit(
    ~decide_choices,
    ~context="(view)",
    ~request,
    ~on_done=o => result := Some(o),
    mk_zipper(program),
  );
  switch (result^) {
  | Some(o) => o
  | None => Alcotest.fail("edit did not call on_done")
  };
};

let build_request =
    (~sketch="", ~names=["x"], ~signature="", path): JevEdit.request => {
  path,
  sketch,
  names,
  literals: [],
  intent: "double a number",
  signature,
};

/* Records every round's questions, answering like [scripted]. */
let recording =
    (~asked: ref(list(list(SystemOne.choice_question))), script)
    : JevEdit.decide_choices => {
  let calls = ref(0);
  (~state, ~questions, ~handler) => {
    asked := asked^ @ [questions];
    scripted(~calls, script, ~state, ~questions, ~handler);
  };
};

let root_options = (asked): list(string) =>
  switch (asked) {
  | [[q, ..._], ..._] => q.SystemOne.options
  | _ => Alcotest.fail("no question asked")
  };

let double_script = [["fun x -> ?"], ["? + ?"], ["x"]];

let printed = (z: Zipper.t): string =>
  CompositionView.Public.print_zipper(z);

let build_tests = (
  "JevEdit.build",
  [
    test_case("every form parses back to itself", `Quick, () =>
      List.iter(
        ((form, _)) =>
          check(
            string,
            form,
            form,
            Printer.of_zipper(~holes="?", mk_zipper(form)),
          ),
        JevEdit.Candidates.forms(~names=["x"]),
      )
    ),
    test_case(
      "forms are filtered by the hole's type",
      `Quick,
      () => {
        let program = "let b: Bool = ? in b";
        switch (JevEdit.holes_in(~path="b", mk_zipper(program))) {
        | [h] =>
          check(
            bool,
            "comparison offered",
            true,
            List.mem("? == ?", h.candidates),
          );
          check(
            bool,
            "sum not offered",
            false,
            List.mem("? + ?", h.candidates),
          );
        | hs => Alcotest.failf("expected 1 hole, got %d", List.length(hs))
        };
      },
    ),
    test_case(
      "builds double from an empty sketch",
      `Quick,
      () => {
        let calls = ref(0);
        let o =
          build(
            ~request=build_request("double"),
            scripted(~calls, [["fun x -> ?"], ["? + ?"], ["x"]]),
          );
        check(option(string), "error", None, o.metrics.error);
        check(int, "rounds", 3, o.metrics.rounds);
        check(int, "filled", 4, o.metrics.filled);
        check(
          bool,
          "code",
          true,
          contains("fun x -> x + x", printed(o.zipper)),
        );
        check(int, "no escalations", 0, List.length(o.metrics.escalated));
        ignore(JevEdit.Log.drain());
      },
    ),
    test_case(
      "a signature prunes the root hole and still builds",
      `Quick,
      () => {
        let untyped = ref([]);
        ignore(
          build(
            ~request=build_request("double"),
            recording(~asked=untyped, double_script),
          ),
        );
        let typed = ref([]);
        let o =
          build(
            ~request=build_request(~signature="Int -> Int", "double"),
            recording(~asked=typed, double_script),
          );
        check(option(string), "error", None, o.metrics.error);
        check(
          bool,
          "fewer root options",
          true,
          List.length(root_options(typed^))
          < List.length(root_options(untyped^)),
        );
        check(
          bool,
          "no string concat at an arrow hole",
          false,
          List.mem("? ++ ?", root_options(typed^)),
        );
        check(
          bool,
          "annotated and built",
          true,
          contains(
            "double : Int -> Int = fun x -> x + x",
            printed(o.zipper),
          ),
        );
        ignore(JevEdit.Log.drain());
      },
    ),
    test_case(
      "a bad signature fails before asking Jev",
      `Quick,
      () => {
        List.iter(
          signature => {
            let calls = ref(0);
            let o =
              build(
                ~request=build_request(~signature, "double"),
                scripted(~calls, double_script),
              );
            check(bool, signature ++ " failed", true, o.metrics.failed);
            check(int, signature ++ " no requests", 0, calls^);
          },
          ["Nonsense -> Int", "Int ->"],
        );
        ignore(JevEdit.Log.drain());
      },
    ),
    test_case(
      "a signature on an existing path resets it",
      `Quick,
      () => {
        let o =
          build(
            ~program="let double = 0 in double",
            ~request=build_request(~signature="Int -> Int", "double"),
            scripted(~calls=ref(0), double_script),
          );
        check(option(string), "error", None, o.metrics.error);
        check(
          bool,
          "rebuilt",
          true,
          contains(
            "double : Int -> Int = fun x -> x + x",
            printed(o.zipper),
          ),
        );
        ignore(JevEdit.Log.drain());
      },
    ),
    test_case("a sketch with no holes is refused unapplied", `Quick, () =>
      List.iter(
        ((label, program, request)) => {
          let calls = ref(0);
          let o = build(~program, ~request, scripted(~calls, [["x"]]));
          check(bool, label ++ " failed", true, o.metrics.failed);
          check(
            option(string),
            label ++ " error",
            Some(JevEdit.no_holes_error),
            o.metrics.error,
          );
          check(int, label ++ " no requests", 0, calls^);
          check(string, label ++ " unchanged", program, printed(o.zipper));
        },
        [
          (
            "update",
            "let double = 0 in double",
            build_request(~sketch="fun x -> x + x", "double"),
          ),
          (
            "create",
            "let base = 1 in base",
            build_request(~sketch="let double = fun x -> x + x in", "double"),
          ),
        ],
      )
    ),
    test_case(
      "runaway expansion stops at the hole budget",
      `Quick,
      () => {
        let calls = ref(0);
        let o =
          build(
            ~request=build_request(~names=[], "t"),
            scripted(~calls, List.init(10, _ => ["(?, ?)"])),
          );
        /* 1 + 2 + 4 + 8 + 16 = 31 asked; the next 32 would pass 40. */
        check(int, "holes asked", 31, o.metrics.holes_seen);
        check(int, "requests", 5, calls^);
        check(int, "rest escalated", 32, List.length(o.metrics.escalated));
        check(bool, "failed", false, o.metrics.failed);
        ignore(JevEdit.Log.drain());
      },
    ),
    test_case(
      "a pattern hole is filled from planner names",
      `Quick,
      () => {
        let calls = ref(0);
        let o =
          build(
            ~request=
              build_request(
                ~sketch="let k = fun ? -> 1 in",
                ~names=["n"],
                "k",
              ),
            fake_choices(~calls, ["n"]),
          );
        check(option(string), "error", None, o.metrics.error);
        check(int, "filled", 1, o.metrics.filled);
        check(
          bool,
          "code",
          true,
          contains("fun n -> 1", printed(o.zipper)),
        );
        ignore(JevEdit.Log.drain());
      },
    ),
    test_case(
      "an ill-typed form escalates",
      `Quick,
      () => {
        /* A Bool hole never offers `? + ?`; answering it anyway must hit the
           fill guard rather than write a type error. */
        let o =
          build(
            ~request=
              build_request(~sketch="let b: Bool = ? in", ~names=[], "b"),
            fake_answer("? + ?"),
          );
        check(int, "filled", 0, o.metrics.filled);
        check(int, "escalated", 1, List.length(o.metrics.escalated));
        ignore(JevEdit.Log.drain());
      },
    ),
  ],
);

let tests = [choice_tests, holes_tests, edit_tests, build_tests];
