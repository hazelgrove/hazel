open Alcotest;
open Language;
open Util;
open Haz3lcore;

module Fill = Web.AgentFill;
module Fixture = Test_StepperView.Fixture;

let settings = Web.Theorems.with_proof_settings(CoreSettings.on);

/* A program checked as the editor checks it, from a zipper, so a patch
 * can be applied to the same ids. */
module Cell = {
  type t = {
    zipper: Zipper.t,
    fixture: Fixture.t,
    checked: (CachedStatics.t, Dynamics.t),
    hole: Proof.t,
  };

  let rec first_hole = (p: Proof.t): option(Proof.t) =>
    switch (p.term) {
    | EmptyHole => Some(p)
    | _ => List.find_map(first_hole, Proof.children(p))
    };

  let mk = (src: string): t => {
    let zipper =
      switch (Parser.to_zipper(src, ~root=Exp)) {
      | Some(z) => z
      | None => fail("failed to parse: " ++ src)
      };
    let statics =
      Fixture.statics_of_term(
        MakeTerm.from_zip_for_sem(zipper, ~root=Exp).term,
      );
    let fixture =
      Fixture.of_cell(
        ~name="t",
        statics,
        Fixture.calculate(
          ~is_edited=true,
          statics,
          Web.EvalResult.Model.init,
        ),
      );
    switch (fixture.cell.theorems.checked, first_hole(fixture.theorem.proof)) {
    | (Some(checked), Some(hole)) => {
        zipper,
        fixture,
        checked,
        hole,
      }
    | (None, _) => fail("the cell wasn't checked")
    | (_, None) => fail("the proof has no hole")
    };
  };

  let hole_description = (cell: t): Fill.Hole.t => {
    let hole_id = Proof.rep_id(cell.hole);
    switch (
      ProofMap.lookup(hole_id, cell.fixture.cell.theorems.proof_map),
      cell.fixture.theorem.stmt,
    ) {
    | (Some(entry), Some(statement)) =>
      switch (
        Fill.Hole.mk(
          ~program=fst(cell.checked).term,
          ~theorem="t",
          ~statement,
          ~hole_id,
          entry,
        )
      ) {
      | Some(hole) => hole
      | None => fail("no goal at the hole")
      }
    | _ => fail("the hole has no proof-map entry")
    };
  };

  let check = (cell: t) =>
    Fill.Attempt.check(
      ~settings,
      ~checked=cell.checked,
      ~theorem_id=cell.fixture.id,
      ~hole_id=Proof.rep_id(cell.hole),
    );

  /* The program text after writing `proof` over the hole. */
  let write = (cell: t, proof: Proof.t): string =>
    EditorTransform.apply_patch(
      cell.zipper,
      EditorTransform.mk_proof_patch(
        ~target_id=Proof.rep_id(cell.hole),
        proof,
      ),
    )
    |> Printer.of_zipper(~holes="?");
};

/* A model that answers with `replies` in turn, recording what it was
 * sent. */
module Stub = {
  type run = {
    requests: list(list(OpenRouter.Message.Model.t)),
    statuses: list(Fill.Status.t),
    written: option(Proof.t),
  };

  let run = (~check=?, cell: Cell.t, replies: list(string)): run => {
    let replies = ref(replies);
    let requests = ref([]);
    let statuses = ref([]);
    let written = ref(None);
    let ask = (messages, k) => {
      requests := requests^ @ [messages];
      switch (replies^) {
      | [r, ...rest] =>
        replies := rest;
        k(Ok(r));
      | [] => k(Error("no more replies"))
      };
    };
    Fill.Loop.run(
      ~ask,
      ~check=Option.value(~default=Cell.check(cell), check),
      ~set_status=s => statuses := statuses^ @ [s],
      ~write=p => written := Some(p),
      ~log=ignore,
      Cell.hole_description(cell),
    );
    {
      requests: requests^,
      statuses: statuses^,
      written: written^,
    };
  };

  let last_user_message = (request: list(OpenRouter.Message.Model.t)) =>
    switch (List.rev(request)) {
    | [{role: User, content, _}, ..._] => content
    | _ => fail("the request doesn't end with a user message")
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

let assume_src = "theorem t = forall n: Int -> n == 1 ==> n == 1 proof assume n == 1 => ? in 0";
let assume_fill = "from n do axiom assume end end; eval";

let tests = (
  "AgentFill",
  [
    test_case("the grammar notes' examples are proven", `Quick, () =>
      List.iter(check_proven, ProofGrammarNotes.examples)
    ),
    test_case(
      "the prompt has the goal, the facts in scope and the axioms",
      `Quick,
      () => {
        let prompt =
          Fill.Prompt.first(Cell.hole_description(Cell.mk(assume_src)));
        check_contains(~msg="goal", prompt, "Goal at `??`: `n == 1`");
        check_contains(~msg="fact", prompt, "- `assume`: `n == 1`");
        check_contains(
          ~msg="statement",
          prompt,
          "whose statement is `forall n",
        );
        check_contains(~msg="marked hole", prompt, "assume n == 1 => ??");
        check_contains(~msg="axioms", Fill.Prompt.system, "- `refl_eq`:");
        check_contains(~msg="grammar", Fill.Prompt.system, "## Proof syntax");
      },
    ),
    test_case("a fenced reply parses as its proof", `Quick, () =>
      check(
        result(string, string),
        "parsed",
        Ok(assume_fill),
        Fill.Attempt.parse("```hazel\n" ++ assume_fill ++ "\n```")
        |> Stdlib.Result.map(p => Fill.print_any(Proof(p))),
      )
    ),
    test_case(
      "a proving reply is written in place of the hole",
      `Quick,
      () => {
        let cell = Cell.mk(assume_src);
        let run = Stub.run(cell, [assume_fill]);
        check(
          list(status),
          "statuses",
          [Filling(1), Filled(1)],
          run.statuses,
        );
        switch (run.written) {
        | Some(p) =>
          let written = Cell.write(cell, p);
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
        check(
          list(status),
          "statuses",
          [Filling(1), Filling(2), Filled(2)],
          run.statuses,
        );
        switch (run.requests) {
        | [_, second] =>
          let retry = Stub.last_user_message(second);
          check_contains(
            ~msg="mark",
            retry,
            "the goal has fewer than 1 occurrences of `n == 2`",
          );
          check_contains(
            ~msg="the first reply is in the history",
            String.concat(
              "\n",
              List.map((m: OpenRouter.Message.Model.t) => m.content, second),
            ),
            "n == 2",
          );
        | requests =>
          fail(
            Printf.sprintf("%d requests, expected 2", List.length(requests)),
          )
        };
      },
    ),
    test_case(
      "a reply that leaves the goal open goes back with the goal",
      `Quick,
      () => {
        let run =
          Stub.run(
            Cell.mk("theorem t = 1 + 4 == 5 proof ? in 0"),
            ["from 1 + 4 do eval end", "from 1 + 4 do eval end; eval"],
          );
        check(
          list(status),
          "statuses",
          [Filling(1), Filling(2), Filled(2)],
          run.statuses,
        );
        switch (run.requests) {
        | [_, second] =>
          check_contains(
            ~msg="reached",
            Stub.last_user_message(second),
            "your proof left the goal `5 == 5`",
          )
        | _ => fail("expected 2 requests")
        };
      },
    ),
    test_case(
      "a reply the checker raises on goes back and is never written",
      `Quick,
      () => {
        let cell = Cell.mk(assume_src);
        let first = ref(true);
        let raising_once = p =>
          if (first^) {
            first := false;
            failwith("nth");
          } else {
            Cell.check(cell, p);
          };
        let run =
          Stub.run(
            ~check=raising_once,
            cell,
            [assume_fill, "axiom nope end", "axiom nope end"],
          );
        check(
          option(string),
          "written",
          Some("axiom nope end"),
          Option.map(p => Fill.print_any(Proof(p)), run.written),
        );
        switch (run.requests) {
        | [_, second, _] =>
          check_contains(
            ~msg="complaint",
            Stub.last_user_message(second),
            "the checker raised Failure(\"nth\")",
          )
        | _ => fail("expected 3 requests")
        };
      },
    ),
    test_case(
      "an induction case left open goes back by its pattern",
      `Quick,
      () => {
        let src = "type Nt = +Z+S(Nt) in\nlet pos = fun e -> case e | Z => true | S(b) => true end in\ntheorem t = forall e: Nt -> pos(e) proof ? in 0";
        let run =
          Stub.run(
            Cell.mk(src),
            [
              "induction e | Z => from pos(Z) do eval end | S(b) => ? end",
              "induction e | Z => from pos(Z) do eval end | S(b) => from pos(S(b)) do eval end end",
            ],
          );
        check(
          list(status),
          "statuses",
          [Filling(1), Filling(2), Filled(2)],
          run.statuses,
        );
        switch (run.requests) {
        | [_, second] =>
          check_contains(
            ~msg="open case",
            Stub.last_user_message(second),
            "the case `S(b)` left the goal `pos(S(b))`",
          )
        | _ => fail("expected 2 requests")
        };
      },
    ),
    test_case(
      "after three failed rounds the fewest-marks attempt is kept",
      `Quick,
      () => {
        let fewest = "from n == 3 do axiom assume end end";
        let run =
          Stub.run(
            Cell.mk(assume_src),
            [
              "axiom nope end; axiom nope end",
              fewest,
              "from 7 do eval end; from 8 do eval end",
            ],
          );
        check(int, "requests", 3, List.length(run.requests));
        switch (List.rev(run.statuses)) {
        | [GaveUp(3, last), ..._] =>
          check_contains(~msg="the last mark", last, "`from 7 do eval end`")
        | _ =>
          fail(
            "didn't give up: "
            ++ String.concat(", ", List.map(Fill.Status.show, run.statuses)),
          )
        };
        check(
          option(string),
          "written",
          Some(fewest),
          Option.map(p => Fill.print_any(Proof(p)), run.written),
        );
      },
    ),
  ],
);
