open Alcotest;
open Language;

/* In `all_lists(xs) == true` the stepper's only step is the lookup of
 * `all_lists`, which the checker's `step` passes through silently. The
 * stepper offers the step the checker takes at `all_lists(xs)` instead,
 * and taking it writes a `step` step that checks. */

module Rows = Web.StepperRows;
module Picker = Web.StepperPicker;
module Fixture = Test_StepperView.Fixture;

let all_lists = {|let all_lists = fun (xs : [Int]) ->
  case xs
  | [] => true
  | _ :: t => all_lists(t)
  end
in
|};

let every =
  all_lists
  ++ "theorem every = forall (xs : [Int]) -> all_lists(xs) == true proof ? in 0";

let every_by_cases =
  all_lists
  ++ "theorem every = forall (xs : [Int]) -> all_lists(xs) == true "
  ++ "proof induction xs | [] => ? | x :: t => ? end in 0";

let both_nil =
  all_lists ++ "theorem both = all_lists([]) == all_lists([]) proof ? in 0";

/* The goal of a theorem's first row. */
let first_goal = (f: Fixture.t): Picker.Goal.t =>
  Fixture.goal(f, Test_StepperView.first(f.rows));

/* The first row of induction case `i`. */
let case_row = (f: Fixture.t, i): Rows.t =>
  switch (
    List.nth_opt(Test_StepperView.cases(Test_StepperView.first(f.rows)), i)
  ) {
  | Some(rows) => Test_StepperView.first(rows)
  | None => Alcotest.fail("no case " ++ string_of_int(i))
  };

let offered = (goal: Picker.Goal.t): list(EvaluatorStep.step) =>
  List.map(fst, goal.eval_steps);

/* What taking the `n`-th offered step writes, checking that it is `step`
 * focused on `exp`: the focus's selector, printed, and the 0-based index
 * of its match. */
let written_focus = (goal: Picker.Goal.t, n, ~exp: string): (string, int) =>
  switch (Picker.take_step(goal, n)) {
  | Some({at: Some({node, selector, index}), step: {term: EvalStep, _}}) =>
    check(string, "focuses on", exp, Test_ProofMap.print_exp(node));
    (Test_ProofMap.print_exp(selector), index);
  | Some(_) => Alcotest.fail("taking the step writes something else")
  | None => Alcotest.fail("taking the step writes nothing")
  };

/* `from <exp> at <n> do step end`, for the match with 0-based `idx`. */
let focused_eval = (~exp, ~idx) =>
  "from "
  ++ exp
  ++ (idx == 0 ? "" : " at " ++ string_of_int(idx + 1))
  ++ " do step end";

let replace_once = (~sub, ~by, s) =>
  switch (Str.search_forward(Str.regexp_string(sub), s, 0)) {
  | i =>
    String.sub(s, 0, i)
    ++ by
    ++ String.sub(
         s,
         i + String.length(sub),
         String.length(s) - i - String.length(sub),
       )
  | exception Not_found => Alcotest.fail("no " ++ sub)
  };

let check_entry = (entry: ProofMap.entry, ~outgoing: option(string)) => {
  check(
    list(string),
    "the written step checks",
    [],
    List.map(ProofMark.show, entry.marks),
  );
  Option.iter(
    expected =>
      check(
        string,
        "outgoing goal",
        expected,
        Test_ProofMap.print_exp(
          Test_ProofMap.require_exp("no outgoing goal", entry.outgoing),
        ),
      ),
    outgoing,
  );
};

/* Writes `step` focused on the `idx`-th `exp` as the theorem's proof and
 * checks it. */
let check_written_proof = (src, ~exp, ~idx, ~outgoing) => {
  let src =
    replace_once(
      ~sub="proof ?",
      ~by="proof " ++ focused_eval(~exp, ~idx),
      src,
    );
  let (state, _, elab) =
    Test_ProofMap.eval_with_proof(Test_Evaluator_Prelude.parse_exp(src));
  check_entry(Test_ProofMap.proof_entry(state, elab), ~outgoing);
};

let test_offers_the_checker_step = () => {
  let goal = first_goal(Fixture.mk(~name="every", every));
  let steps = offered(goal);
  check(
    bool,
    "offers a step other than a lookup",
    true,
    List.exists(
      s => EvaluatorStep.get_step_kind(s) != Transition.VarLookup,
      steps,
    ),
  );
  let highlighted = id =>
    Haz3lcore.TermData.root_tile(id, goal.editor.editor.syntax.term_data)
    != None;
  check(
    bool,
    "every offered step highlights a tile of the row",
    true,
    steps != [] && List.for_all(highlighted, Picker.Goal.step_ids(goal)),
  );
};

let test_taking_it_writes_a_checked_step = () => {
  let goal = first_goal(Fixture.mk(~name="every", every));
  switch (offered(goal)) {
  | [_] =>
    let (selector, idx) = written_focus(goal, 0, ~exp="all_lists(xs)");
    check(int, "at index", 0, idx);
    check_written_proof(every, ~exp=selector, ~idx, ~outgoing=None);
  | steps =>
    Alcotest.fail(string_of_int(List.length(steps)) ++ " steps offered")
  };
};

/* Takes the first offered step in induction case `i`, writes it into the
 * case's hole, and checks the written step. */
let take_in_case = (i, ~hole, ~exp, ~outgoing) => {
  let f = Fixture.mk(~name="every", every_by_cases);
  let goal = Fixture.goal(f, case_row(f, i));
  let (selector, idx) =
    switch (offered(goal)) {
    | [_, ..._] => written_focus(goal, 0, ~exp)
    | [] => Alcotest.fail("no step offered")
    };
  let src =
    replace_once(
      ~sub=hole ++ " => ?",
      ~by=hole ++ " => " ++ focused_eval(~exp=selector, ~idx),
      every_by_cases,
    );
  switch (case_row(Fixture.mk(~name="every", src), i)) {
  | {kind: Step(_), step: {term: Focus(_), _}, entry: Some(entry), _} =>
    check_entry(entry, ~outgoing)
  | _ => Alcotest.fail("the case is not a checked eval step")
  };
};

/* One step per side, each highlighting its side and writing the index
 * under which the checker reduces that side. */
let test_each_occurrence_writes_its_index = () => {
  let goal = first_goal(Fixture.mk(~name="both", both_nil));
  check(int, "one step per side", 2, List.length(offered(goal)));
  let (left, right) =
    switch (Exp.term_of(goal.written)) {
    | BinOp(Poly(Equals), l, r) => (l, r)
    | _ => Alcotest.fail("not an == goal")
    };
  let check_side = (side: Exp.t, ~outgoing) =>
    switch (
      List.find_index(
        id => id == Exp.rep_id(side),
        Picker.Goal.step_ids(goal),
      )
    ) {
    | Some(n) =>
      let (selector, idx) = written_focus(goal, n, ~exp="all_lists([])");
      check_written_proof(
        both_nil,
        ~exp=selector,
        ~idx,
        ~outgoing=Some(outgoing),
      );
    | None => Alcotest.fail("no step highlights " ++ Exp.show(side))
    };
  check_side(left, ~outgoing="true == all_lists([])");
  check_side(right, ~outgoing="all_lists([]) == true");
};

let tests = (
  "ListCaseStep",
  [
    test_case(
      "a goal applying a let-bound function offers the checker's step",
      `Quick,
      test_offers_the_checker_step,
    ),
    test_case(
      "taking it writes an eval step that checks",
      `Quick,
      test_taking_it_writes_a_checked_step,
    ),
    test_case("in the [] case body", `Quick, () =>
      take_in_case(
        0,
        ~hole="[]",
        ~exp="all_lists([])",
        ~outgoing=Some("true == true"),
      )
    ),
    test_case("in the x :: t case body", `Quick, () =>
      take_in_case(1, ~hole="x :: t", ~exp="all_lists(x:: t)", ~outgoing=None)
    ),
    test_case(
      "equal occurrences each write their own index",
      `Quick,
      test_each_occurrence_writes_its_index,
    ),
  ],
);
