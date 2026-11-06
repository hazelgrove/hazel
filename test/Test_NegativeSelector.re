open Alcotest;
open Language;
open Util;

/* Evaluation produces negative numbers that source can't spell: `-5`
 * written out reads back as a negation. So the stepper doesn't offer, or
 * write, an `eval` step whose selector holds one; the checker could never
 * find it. Each program's first step produces a negative number, and the
 * hole after it is the row under test. */

module SB = Web.StepperBase;

let hole_row = (src: string): Web.MissingStep.Model.t => {
  let rec find = (row: SB.next_step) =>
    switch (row) {
    | MissingStep(m, _) => m
    | NextStep({next_step, _}) => find(next_step)
    | Finished => Alcotest.fail("no hole row")
    };
  find(fst(Test_ListCaseStep.stepper(src, "t")));
};

/* What each offered step writes. */
let written_targets = (m: Web.MissingStep.Model.t): list((Exp.t, int)) =>
  switch (Calc.get_saved_opt(m.next_steps)) {
  | Some(AvailableSteps(steps)) =>
    List.filter_map(Web.MissingStep.Update.written_target(m), steps)
  | Some(AutoStep(_))
  | None => []
  };

/* The checker's entry for `eval <selector> at <idx> end` written in the
 * hole. */
let check_written = (src, ~selector, ~idx): ProofMap.entry => {
  let src =
    Test_ListCaseStep.replace_once(
      ~sub="; ? in 0",
      ~by=
        "; eval " ++ selector ++ " at " ++ string_of_int(idx) ++ " end in 0",
      src,
    );
  let (state, _, elab) =
    Test_ProofMap.eval_with_proof(Test_Evaluator_Prelude.parse_exp(src));
  switch (Test_ProofMap.proof_of(elab).term) {
  | Seq(_, second) =>
    switch (
      ProofMap.lookup(
        Proof.rep_id(second),
        EvaluatorState.get_proof_map(state),
      )
    ) {
    | Some(entry) => entry
    | None => Alcotest.fail("no entry for the written step")
    }
  | _ => Alcotest.fail("the proof is not two steps")
  };
};

/* Only the side without a negative number is offered, and what it writes
 * checks. */
let offers_only = (src, ~spellable) => {
  let m = hole_row(src);
  switch (written_targets(m)) {
  | [(at_exp, idx)] =>
    check(
      string,
      "the offered selector",
      spellable,
      Test_ProofMap.print_exp(at_exp),
    );
    check(
      list(string),
      "the written step checks",
      [],
      List.map(
        ProofMark.show,
        check_written(src, ~selector=spellable, ~idx).marks,
      ),
    );
  | targets =>
    Alcotest.fail(
      "offered: "
      ++ String.concat(
           ", ",
           List.map(((e, _)) => Test_ProofMap.print_exp(e), targets),
         ),
    )
  };
};

let negative_int = {|theorem t = (0 - 5) * 2 == 0 - 10 proof eval 0 - 5 at 0 end; ? in 0|};

let negative_float = {|theorem t = (0.0 -. 2.5) *. 2.0 ==. 0.0 -. 5.0 proof eval 0.0 -. 2.5 at 0 end; ? in 0|};

let test_stale_offer = () => {
  let m = hole_row(negative_int);
  let written = Calc.get_saved_opt(m.eval_exp) |> Option.get;
  let env = Calc.get_saved_opt(m.cached_env) |> Option.get;
  let negative_side =
    switch (Exp.term_of(written)) {
    | BinOp(_, l, _) => l
    | _ => Alcotest.fail("not an == goal")
    };
  let step =
    switch (EvaluatorStep.get_status(~settings=CoreSettings.on, written, env)) {
    | AvailableSteps(steps) =>
      List.find(
        s => EvaluatorStep.get_step_id(s) == Exp.rep_id(negative_side),
        steps,
      )
    | AutoStep(_) => Alcotest.fail("the goal has a hidden step")
    };
  let stale =
    Web.MissingStep.Model.{
      ...m,
      next_steps: Calc.Calculated(EvaluatorStep.AvailableSteps([step])),
      step_sources: Calc.Calculated([Exp.rep_id(negative_side)]),
    };
  check(
    bool,
    "taking it writes nothing",
    true,
    Web.MissingStep.Update.written_target(stale, step) == None,
  );
};

let tests = (
  "NegativeSelector",
  [
    test_case("a negative Int isn't quoted", `Quick, () =>
      offers_only(negative_int, ~spellable="0 - 10")
    ),
    test_case("a negative Float isn't quoted", `Quick, () =>
      offers_only(negative_float, ~spellable="0.0 -. 5.0")
    ),
    test_case(
      "a stale offer quoting a negative number writes nothing",
      `Quick,
      test_stale_offer,
    ),
  ],
);
