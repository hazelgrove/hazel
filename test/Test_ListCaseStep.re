open Alcotest;
open Language;
open Util;

/* In `all_lists(xs) == true` the stepper's only step is the lookup of
 * `all_lists`, which the checker's `eval` passes through silently. The
 * stepper offers the step the checker takes at `all_lists(xs)` instead,
 * and taking it writes an `eval` step that checks. */

module SB = Web.StepperBase;

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

/* The theorem's stepper and the checker's proof map, set up as
 * Theorems.Update.calculate sets them up. */
let stepper = (src: string, name: string): (SB.next_step, ProofMap.t) => {
  let (state, info_map, _) =
    Test_ProofMap.eval_with_proof(Test_Evaluator_Prelude.parse_exp(src));
  let pm = EvaluatorState.get_proof_map(state);
  let settings = {
    ...CoreSettings.on,
    evaluation: {
      ...CoreSettings.on.evaluation,
      enable_proof: true,
      stepper_history: true,
    },
  };
  switch (
    EvaluatorState.get_theorems(state)
    |> List.find_opt(((_, n, _, _)) => n == name)
  ) {
  | None => Alcotest.fail("no theorem " ++ name)
  | Some((id, _, env, stmt)) =>
    let stmt = Substitution.in_exp(Environment.empty, stmt);
    let goal = stmt |> ProofRule.peel_binders |> (((_, _, core)) => core);
    let ctx =
      info_map |> Statics.Map.ctx_of(id) |> Option.value(~default=Ctx.empty);
    let sem_ctx =
      ProofCheck.peel_stmt_binders(
        SemanticCtx.of_program_state(ctx, env),
        stmt,
      )
      |> fst;
    let proof =
      switch (Statics.Map.lookup_exp(id, info_map)) {
      | Some({user_term, _}) =>
        switch (Exp.term_of(user_term)) {
        | Theorem(_, _, proof, _) => proof
        | _ => Proof.fresh(EmptyHole)
        }
      | None => Proof.fresh(EmptyHole)
      };
    let root =
      SB.Stepper.calculate(
        ~settings=Calc.NewValue(settings),
        ~ctx=Calc.NewValue(sem_ctx),
        ~exp=Calc.NewValue(goal),
        ~ana=Calc.NewValue(Typ.fresh(Atom(Bool))),
        ~proof=Calc.NewValue(proof),
        ~proof_map=Calc.NewValue(pm),
        ~proof_info_map=Calc.NewValue(info_map),
        SB.init_step,
      );
    (root, pm);
  };
};

let missing_step = (row: SB.next_step): Web.MissingStep.Model.t =>
  switch (row) {
  | MissingStep(m, _) => m
  | _ => Alcotest.fail("not a missing step")
  };

let case_row = (root: SB.next_step, i): SB.next_step =>
  switch (root) {
  | NextStep({step_kind: InductionStep(im), _}) =>
    switch (List.nth_opt(im.cases, i)) {
    | Some(c) => c.step
    | None => Alcotest.fail("no case " ++ string_of_int(i))
    }
  | _ => Alcotest.fail("not an induction step")
  };

let offered = (m: Web.MissingStep.Model.t): list(EvaluatorStep.step) =>
  switch (Calc.get_saved_opt(m.next_steps)) {
  | Some(AvailableSteps(steps)) => steps
  | Some(AutoStep(_))
  | None => []
  };

/* The index taking `step` writes, checking that it writes `eval <exp> at
 * <index> end`. */
let written_index = (m: Web.MissingStep.Model.t, step, ~exp: string): int =>
  switch (Web.MissingStep.Update.written_target(m, step)) {
  | Some((at_exp, at_idx)) =>
    check(
      bool,
      "writes " ++ exp,
      true,
      Exp.fast_equal(at_exp, Test_Evaluator_Prelude.parse_exp(exp)),
    );
    at_idx;
  | None => Alcotest.fail("taking the step writes nothing")
  };

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

/* Writes `eval <exp> at <idx> end` as the theorem's proof and checks it. */
let check_written_proof = (src, ~exp, ~idx, ~outgoing) => {
  let src =
    replace_once(
      ~sub="proof ?",
      ~by="proof eval " ++ exp ++ " at " ++ string_of_int(idx) ++ " end",
      src,
    );
  let (state, _, elab) =
    Test_ProofMap.eval_with_proof(Test_Evaluator_Prelude.parse_exp(src));
  check_entry(Test_ProofMap.proof_entry(state, elab), ~outgoing);
};

let test_offers_the_checker_step = () => {
  let m = missing_step(fst(stepper(every, "every")));
  let steps = offered(m);
  check(
    bool,
    "offers a step other than a lookup",
    true,
    List.exists(
      s => EvaluatorStep.get_step_kind(s) != Transition.VarLookup,
      steps,
    ),
  );
  let editor: Web.CodeSelectable.Model.t =
    m.editor |> Calc.get_saved_opt |> Option.get;
  let highlighted = step =>
    Haz3lcore.TermData.root_tile(
      Web.MissingStep.Update.display_step_id(m, step),
      editor.editor.syntax.term_data,
    )
    != None;
  check(
    bool,
    "every offered step highlights a tile of the row",
    true,
    steps != [] && List.for_all(highlighted, steps),
  );
};

let test_taking_it_writes_a_checked_step = () => {
  let m = missing_step(fst(stepper(every, "every")));
  switch (offered(m)) {
  | [step] =>
    check(int, "at index", 0, written_index(m, step, ~exp="all_lists(xs)"));
    check_written_proof(every, ~exp="all_lists(xs)", ~idx=0, ~outgoing=None);
  | steps =>
    Alcotest.fail(string_of_int(List.length(steps)) ++ " steps offered")
  };
};

/* Takes the first offered step in induction case `i`, writes it into the
 * case's hole, and checks the written step. */
let take_in_case = (i, ~hole, ~exp, ~outgoing) => {
  let m = missing_step(case_row(fst(stepper(every_by_cases, "every")), i));
  switch (offered(m)) {
  | [step, ..._] => check(int, "at index", 0, written_index(m, step, ~exp))
  | [] => Alcotest.fail("no step offered")
  };
  let src =
    replace_once(
      ~sub=hole ++ " => ?",
      ~by=hole ++ " => eval " ++ exp ++ " at 0 end",
      every_by_cases,
    );
  let (root, pm) = stepper(src, "every");
  switch (case_row(root, i)) {
  | NextStep({step_kind: EvalStep(_), proof, _}) =>
    let id = Proof.rep_id(Calc.get_saved_opt(proof) |> Option.get);
    switch (ProofMap.lookup(id, pm)) {
    | Some(entry) => check_entry(entry, ~outgoing)
    | None => Alcotest.fail("no proof-map entry")
    };
  | _ => Alcotest.fail("the case is not an eval step")
  };
};

/* One step per side, each highlighting its side and writing the index
 * under which the checker reduces that side. */
let test_each_occurrence_writes_its_index = () => {
  let m = missing_step(fst(stepper(both_nil, "both")));
  let steps = offered(m);
  check(int, "one step per side", 2, List.length(steps));
  let (left, right) =
    switch (Exp.term_of(Calc.get_saved_opt(m.full_exp) |> Option.get)) {
    | BinOp(Poly(Equals), l, r) => (l, r)
    | _ => Alcotest.fail("not an == goal")
    };
  let check_side = (side: Exp.t, ~outgoing) =>
    switch (
      List.find_opt(
        s =>
          Web.MissingStep.Update.display_step_id(m, s) == Exp.rep_id(side),
        steps,
      )
    ) {
    | Some(step) =>
      let idx = written_index(m, step, ~exp="all_lists([])");
      check_written_proof(
        both_nil,
        ~exp="all_lists([])",
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
      take_in_case(
        1,
        ~hole="x :: t",
        ~exp="all_lists(x :: t)",
        ~outgoing=None,
      )
    ),
    test_case(
      "equal occurrences each write their own index",
      `Quick,
      test_each_occurrence_writes_its_index,
    ),
  ],
);
