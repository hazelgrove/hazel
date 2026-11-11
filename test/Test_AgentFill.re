open Alcotest;
open Language;
open Util;
open Haz3lcore;

module Fill = Web.AgentFill;
module Fixture = Test_StepperView.Fixture;

let core = Web.Theorems.with_proof_settings(CoreSettings.on);

/* A program with a `??`, evaluated and checked as the editor does it,
 * from a zipper, so a patch can be applied to the same ids. */
module Cell = {
  type t = {
    zipper: Zipper.t,
    program: Exp.t,
    dynamics: option(Dynamics.t),
    hole_id: Id.t,
  };

  let mk = (src: string): t => {
    let zipper =
      switch (Parser.to_zipper(src, ~root=Exp)) {
      | Some(z) => z
      | None => fail("failed to parse: " ++ src)
      };
    let program = MakeTerm.from_zip_for_sem(zipper, ~root=Exp).term;
    let statics = Fixture.statics_of_term(program);
    let cell =
      Fixture.calculate(~is_edited=true, statics, Web.EvalResult.Model.init);
    switch (Fill.Typed.llm_hole_ids(zipper)) {
    | [hole_id, ..._] => {
        zipper,
        program,
        dynamics: Option.map(snd, cell.theorems.checked),
        hole_id,
      }
    | [] => fail("no ?? in " ++ src)
    };
  };
};

/* A model that answers with `replies` in turn, recording what it was
 * sent, and what the fill reported and wrote. */
module Stub = {
  type run = {
    requests: list(list(OpenRouter.Message.Model.t)),
    statuses: list(Fill.Status.t),
    /* The program after the fill's write, if it wrote. */
    written: option(string),
  };

  let ask = (replies: list(string)) => {
    let replies = ref(replies);
    let requests = ref([]);
    let ask = (messages, k) => {
      requests := requests^ @ [messages];
      switch (replies^) {
      | [r, ...rest] =>
        replies := rest;
        k(Ok(r));
      | [] => k(Error("no more replies"))
      };
    };
    (ask, requests);
  };

  /* A model that answers only when the test calls `answer`, so a test
   * can act while a request is in flight. */
  let pending = () => {
    let waiting = ref(None);
    let requests = ref([]);
    let ask = (messages, k) => {
      requests := requests^ @ [messages];
      waiting := Some(k);
    };
    let answer = reply =>
      switch (waiting^) {
      | Some(k) =>
        waiting := None;
        k(Ok(reply));
      | None => fail("no request is waiting")
      };
    (ask, requests, answer);
  };

  /* Starts a fill with `ask`; the run reads what it has done so far. */
  let start = (~ask, cell: Cell.t): (unit => run) => {
    let statuses = ref([]);
    let written = ref(None);
    Fill.Typed.fill(
      ~core,
      ~ask=Ok(ask),
      ~dynamics=cell.dynamics,
      ~app={
        set_status: s => statuses := statuses^ @ [s],
        write: patch =>
          written :=
            Some(
              EditorTransform.apply_patch(cell.zipper, patch)
              |> Printer.of_zipper(~holes="?"),
            ),
      },
      ~program=cell.program,
      cell.hole_id,
    );
    () => {
      requests: [],
      statuses: statuses^,
      written: written^,
    };
  };

  let run = (cell: Cell.t, replies: list(string)): run => {
    let (ask, requests) = ask(replies);
    {
      ...start(~ask, cell, ()),
      requests: requests^,
    };
  };

  let system = (request: list(OpenRouter.Message.Model.t)) =>
    switch (request) {
    | [{role: System, content, _}, ..._] => content
    | _ => fail("the request doesn't start with a system message")
    };

  let last_user_message = (request: list(OpenRouter.Message.Model.t)) =>
    switch (List.rev(request)) {
    | [{role: User, content, _}, ..._] => content
    | _ => fail("the request doesn't end with a user message")
    };

  /* The `n`th request's last message, from 1. */
  let prompt = (run: run, n: int) =>
    switch (List.nth_opt(run.requests, n - 1)) {
    | Some(request) => last_user_message(request)
    | None =>
      fail(
        Printf.sprintf("no request %d of %d", n, List.length(run.requests)),
      )
    };
};

let contains = (haystack: string, needle: string): bool =>
  StringUtil.plain_search(needle, haystack, 0) >= 0;

let check_contains = (~msg, haystack, needle) =>
  check(
    bool,
    msg ++ ": " ++ needle ++ " in\n" ++ haystack,
    true,
    contains(haystack, needle),
  );

let status = testable(Fmt.of_to_string(Fill.Status.show), (==));

let check_statuses = (expected, run: Stub.run) =>
  check(list(status), "statuses", expected, run.statuses);

/* Fails with the checker's view of the proof when it isn't proven. */
let check_proven = (src: string): unit => {
  let f = Fixture.mk(src);
  let pm = f.cell.theorems.proof_map;
  if (ProofMap.status_of_proof(pm, f.theorem.proof) != Some(true)) {
    fail(
      "not proven: "
      ++ src
      ++ "\n"
      ++ Test_ProofMap.Harness.dump(pm, f.theorem.proof),
    );
  };
};

let assume_src = "theorem t = forall n: Int -> n == 1 ==> n == 1 proof assume n == 1 => ?? in 0";
let assume_fill = "from n do axiom assume end end; eval";

let proof_tests = [
  test_case("the grammar notes' examples are proven", `Quick, () =>
    List.iter(check_proven, ProofGrammarNotes.examples)
  ),
  test_case(
    "the proof prompt has the goal, the facts in scope and the axioms",
    `Quick,
    () => {
      let run = Stub.run(Cell.mk(assume_src), [assume_fill]);
      let prompt = Stub.prompt(run, 1);
      check_contains(~msg="goal", prompt, "Goal at `??`: `n == 1`");
      check_contains(~msg="fact", prompt, "- `assume`: `n == 1`");
      check_contains(
        ~msg="statement",
        prompt,
        "whose statement is `forall n",
      );
      check_contains(~msg="marked hole", prompt, "assume n == 1 => ??");
      let system = Stub.system(List.hd(run.requests));
      check_contains(~msg="axioms", system, "- `refl_eq`:");
      check_contains(~msg="grammar", system, "## Proof syntax");
    },
  ),
  test_case(
    "goals name the functions the checker inlined, without ascriptions",
    `Quick,
    () => {
      let run =
        Stub.run(
          Cell.mk(
            "let rev = fun xs -> case xs | [] => [] | x :: t => rev(t) @ [x] end in\ntheorem t = forall x: Int -> forall t: [Int] -> rev(x :: t) == rev(t) @ [x] proof from rev(x :: t) do eval end; ?? in 0",
          ),
          ["axiom refl_eq end"],
        );
      let prompt = Stub.prompt(run, 1);
      check_contains(
        ~msg="goal",
        prompt,
        "Goal at `??`: `rev(t) @ [x] == rev(t) @ [x]`",
      );
      check(bool, "no inlined body", false, contains(prompt, "fold"));
    },
  ),
  test_case(
    "a reply is read back as the editor will hold it",
    `Quick,
    () => {
      let read = r =>
        switch (Fill.ProofHole.parse(r)) {
        | Ok(p) => Fill.print_any(~inline=false, Proof(p))
        | Error(why) => fail(why)
        };
      let once =
        read(
          "from 1st if $e then $e else $e do eval; eval end; axiom refl_eq end",
        );
      check(
        string,
        "printing and reading again changes nothing",
        once,
        read(once),
      );
    },
  ),
  test_case("a fenced reply parses as its proof", `Quick, () =>
    check(
      result(string, string),
      "parsed",
      Ok(assume_fill),
      Fill.ProofHole.parse("```hazel\n" ++ assume_fill ++ "\n```")
      |> Stdlib.Result.map(p => Fill.print_any(Proof(p))),
    )
  ),
  test_case(
    "a proving reply is written in place of the hole",
    `Quick,
    () => {
      let run = Stub.run(Cell.mk(assume_src), [assume_fill]);
      check_statuses([Filling(1), Filled(1)], run);
      switch (run.written) {
      | Some(written) =>
        check_contains(~msg="written", written, "axiom assume end");
        check_proven(written);
      | None => fail("nothing written")
      };
    },
  ),
  test_case(
    "a reply with marks goes back with its marks",
    `Quick,
    () => {
      let run =
        Stub.run(
          Cell.mk(assume_src),
          ["from n == 2 do axiom assume end end", assume_fill],
        );
      check_statuses([Filling(1), Filling(2), Filled(2)], run);
      check_contains(
        ~msg="mark",
        Stub.prompt(run, 2),
        "on the goal `n == 1`: the goal has fewer than 1 occurrences of `n == 2`",
      );
    },
  ),
  test_case(
    "a reply that leaves the goal open goes back with the goal",
    `Quick,
    () => {
      let run =
        Stub.run(
          Cell.mk("theorem t = 1 + 4 == 5 proof ?? in 0"),
          ["from 1 + 4 do eval end", "from 1 + 4 do eval end; eval"],
        );
      check_statuses([Filling(1), Filling(2), Filled(2)], run);
      check_contains(
        ~msg="reached",
        Stub.prompt(run, 2),
        "your proof left the goal `5 == 5`",
      );
    },
  ),
  test_case(
    "an induction case left open goes back by its pattern",
    `Quick,
    () => {
      let run =
        Stub.run(
          Cell.mk(
            "type Nt = +Z+S(Nt) in\nlet pos = fun e -> case e | Z => true | S(b) => true end in\ntheorem t = forall e: Nt -> pos(e) proof ?? in 0",
          ),
          [
            "induction e | Z => from pos(Z) do eval end | S(b) => ? end",
            "induction e | Z => from pos(Z) do eval end | S(b) => from pos(S(b)) do eval end end",
          ],
        );
      check_statuses([Filling(1), Filling(2), Filled(2)], run);
      check_contains(
        ~msg="open case",
        Stub.prompt(run, 2),
        "the case `S(b)` left the goal `pos(S(b))`",
      );
    },
  ),
  test_case(
    "after the last failed round the fewest-marks attempt is kept",
    `Quick,
    () => {
      let run =
        Stub.run(
          Cell.mk(assume_src),
          List.init(Fill.max_rounds - 2, _ => "axiom nope end; axiom nope end")
          @ [
            "from n == 3 do axiom assume end end",
            "from 7 do eval end; from 8 do eval end",
          ],
        );
      switch (List.rev(run.statuses)) {
      | [GaveUp(n, last), ..._] when n == Fill.max_rounds =>
        check_contains(~msg="the last mark", last, "`from 7 do eval end`")
      | _ =>
        fail(
          "didn't give up: "
          ++ String.concat(", ", List.map(Fill.Status.show, run.statuses)),
        )
      };
      check_contains(
        ~msg="written",
        Option.value(~default="", run.written),
        "from n == 3 do",
      );
    },
  ),
  test_case(
    "a reply checking raises on goes back and is never written",
    `Quick,
    () => {
      let (ask, requests) = Stub.ask(["first", "second"]);
      let written = ref([]);
      Fill.Loop.run(
        ~ask,
        ~system="",
        ~retry=String.concat("\n"),
        ~attempt=
          fun
          | "first" => failwith("nth")
          | reply => Checked(reply, []),
        ~set_status=ignore,
        ~write=r => written := written^ @ [r],
        ~log=ignore,
        "fill",
      );
      check(list(string), "written", ["second"], written^);
      check_contains(
        ~msg="complaint",
        Stub.last_user_message(List.nth(requests^, 1)),
        "checking your reply raised Failure(\"nth\")",
      );
    },
  ),
];

/* `x: Int` and `n: Int` in scope, `Bool` expected. */
let exp_src = "type Shape = +Circle(Int)+Square(Int) in\nlet n: Int = 3 in\nlet positive: Int -> Bool = fun x -> ?? in\npositive(n)";

let typed_tests = [
  test_case(
    "?? parses as a hole and prints back",
    `Quick,
    () => {
      let print = s =>
        Test_ProofMap.Harness.print_exp(Test_Evaluator_Prelude.parse_exp(s));
      check(string, "expression", "1 + ??", print("1 + ??"));
      check(
        string,
        "proof",
        "theorem t = true proof ?? in 0",
        print("theorem t = true proof ?? in 0"),
      );
    },
  ),
  test_case(
    "typing the second ? of ?? starts a fill; other edits don't",
    `Quick,
    () => {
      let zipper = s =>
        switch (Parser.to_zipper(s, ~root=Exp)) {
        | Some(z) => z
        | None => fail("failed to parse: " ++ s)
        };
      let insert = (z, c) => Test_Autoprobe.perform(z, [Insert(c)]);
      let before = zipper("1 + ?");
      let after = insert(before, "?");
      check(
        list(string),
        "typed",
        List.map(Id.to_string, Fill.Typed.llm_hole_ids(after)),
        List.map(
          Id.to_string,
          Fill.Typed.holes(Perform(Insert("?")), ~before, ~after),
        ),
      );
      check(
        int,
        "one ?? typed",
        1,
        List.length(Fill.Typed.llm_hole_ids(after)),
      );
      /* A program loaded with `??` in it, then edited elsewhere. */
      let loaded = zipper("let x = ?? in x + 1");
      check(
        int,
        "typing elsewhere",
        0,
        List.length(
          Fill.Typed.holes(
            Perform(Insert("2")),
            ~before=loaded,
            ~after=insert(loaded, "2"),
          ),
        ),
      );
      check(
        int,
        "not an insertion",
        0,
        List.length(
          Fill.Typed.holes(
            Perform(Move(Local(Left, ByChar))),
            ~before,
            ~after,
          ),
        ),
      );
    },
  ),
  test_case(
    "the expression prompt has the expected type and the scope",
    `Quick,
    () => {
      let run = Stub.run(Cell.mk(exp_src), ["x > 0"]);
      let prompt = Stub.prompt(run, 1);
      check_contains(
        ~msg="expected",
        prompt,
        "Expected type of `??`: `Bool`",
      );
      check_contains(~msg="argument", prompt, "- `x: Int`");
      check_contains(~msg="outer variable", prompt, "- `n: Int`");
      check_contains(~msg="type", prompt, "- `type Shape = ");
      check_contains(~msg="marked hole", prompt, "fun x -> ??");
      check_contains(
        ~msg="language guide",
        Stub.system(List.hd(run.requests)),
        "Hazel language guide",
      );
    },
  ),
  test_case(
    "an expression reply with a type error gets a second round",
    `Quick,
    () => {
      let run = Stub.run(Cell.mk(exp_src), ["x + 1", "x > 0"]);
      check_statuses([Filling(1), Filling(2), Filled(2)], run);
      check_contains(
        ~msg="error",
        Stub.prompt(run, 2),
        "Expecting type Bool",
      );
      check_contains(
        ~msg="written",
        Option.value(~default="", run.written),
        "x > 0",
      );
    },
  ),
  test_case(
    "a proof ?? runs the proof loop",
    `Quick,
    () => {
      let run = Stub.run(Cell.mk(assume_src), [assume_fill]);
      check_contains(
        ~msg="proof prompt",
        Stub.system(List.hd(run.requests)),
        "You write proofs",
      );
      check_statuses([Filling(1), Filled(1)], run);
    },
  ),
];

let cancel_tests = [
  test_case(
    "deleting the hole cancels its fill: nothing more is asked or written",
    `Quick,
    () => {
      let cell = Cell.mk(assume_src);
      let (ask, requests, answer) = Stub.pending();
      let run = Stub.start(~ask, cell);
      Fill.Typed.edited(Option.get(Parser.to_zipper("0", ~root=Exp)));
      answer(assume_fill);
      check_statuses([Filling(1), Cancelled], run());
      check(bool, "nothing written", true, run().written == None);
      check(int, "one request", 1, List.length(requests^));
    },
  ),
  test_case(
    "an edit that keeps the hole doesn't cancel",
    `Quick,
    () => {
      let cell = Cell.mk(assume_src);
      let (ask, _, answer) = Stub.pending();
      let run = Stub.start(~ask, cell);
      Fill.Typed.edited(cell.zipper);
      answer(assume_fill);
      check_statuses([Filling(1), Filled(1)], run());
    },
  ),
  test_case(
    "stop cancels the fills in progress",
    `Quick,
    () => {
      let cell = Cell.mk(assume_src);
      let (ask, _, answer) = Stub.pending();
      let run = Stub.start(~ask, cell);
      Fill.Active.cancel_all();
      answer(assume_fill);
      check_statuses([Filling(1), Cancelled], run());
    },
  ),
];

let tests = ("AgentFill", proof_tests @ typed_tests @ cancel_tests);
