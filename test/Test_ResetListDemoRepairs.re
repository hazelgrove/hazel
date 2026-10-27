open Alcotest;
open Language;
open Util;

/* R-reset-list-step-0116-2 (Matt, 2026-10-27): building the all_lists
 * induction proof ONLY by clicking evals, the cons case's second click
 * wrote an `eval <lambda/fix quote> at 0 end` the checker cannot find
 * ("Could not find occurrence #0"). This replays the click through the
 * real picker (MissingStep offered steps + written_target) and the exact
 * write TakeStep emits (eval_step_term + ReplaceProof patch), then
 * re-parses and re-checks the written program. */

module LC = Test_ResetListCaseStep;

let pr = Test_ProofMap.print_exp;

let parse = Test_Evaluator_Prelude.parse_exp;

let observe = s => print_endline("OBSERVED " ++ s);

/* Matt's program up to and including the first cons-case eval, with the
   hole his second click wrote over. */
let matt_prefix = {|let all_lists = fun (xs:([Int])) -> case xs
| [] => true
| _:: t => all_lists(t)
end in
theorem every = forall(xs:([Int])) -> all_lists(xs) == true proof induction xs
| [] =>
    eval all_lists([]) at 0 end;
    eval true == true at 0 end
| x:: xs =>
    eval all_lists(x :: xs) at 0 end; ?
end in
0|};

/* The proof leaves of a stepper chain, in order. */
let rec chain_proofs = (ns: Web.StepperBase.next_step): list(Proof.t) =>
  switch (ns) {
  | NextStep({proof, next_step, _}) =>
    switch (Calc.get_saved_opt(proof)) {
    | Some(p) => [p, ...chain_proofs(next_step)]
    | None => chain_proofs(next_step)
    }
  | MissingStep(_, tail) => chain_proofs(tail)
  | Finished => []
  };

/* Diagnostics: the constructor tag of a node, and the first structural
   difference between two trees (path of child indices + both tags). */
let tag = (e: Exp.t): string => {
  let s = Exp.show_term(Exp.term_of(e));
  let s = String.length(s) > 0 && s.[0] == '(' ? String.sub(s, 1, String.length(s) - 1) : s;
  switch (String.index_opt(s, ' ')) {
  | Some(i) => String.sub(s, 0, i)
  | None => s
  };
};

let rec first_diff = (path, a: Exp.t, b: Exp.t): option(string) =>
  if (Equality.ignoring_ascriptions.exp(a, b)) {
    None;
  } else {
    let ka = ProofHacks.immediate_exp_children(a);
    let kb = ProofHacks.immediate_exp_children(b);
    let here = () =>
      Some(
        Printf.sprintf(
          "path=[%s] selected=%s(%s) reparsed=%s(%s)",
          String.concat(",", List.map(string_of_int, List.rev(path))),
          tag(a),
          pr(a),
          tag(b),
          pr(b),
        ),
      );
    if (tag(a) != tag(b) || List.length(ka) != List.length(kb)) {
      here();
    } else {
      let rec go = (i, xs, ys) =>
        switch (xs, ys) {
        | ([x, ...xs], [y, ...ys]) =>
          switch (first_diff([i, ...path], x, y)) {
          | Some(d) => Some(d)
          | None => go(i + 1, xs, ys)
          }
        | _ => here()
        };
      go(0, ka, kb);
    };
  };

/* What TakeStep writes for `step` of row `m`: the production
   written_target (selector + checker index), eval_step_term, then
   add_step_patch: ReplaceProof on a hole, ExtendProof (Seq(prev, new)) on
   the synthesized trailing row, whose leaf is `prev`. */
let take_step_write = (~prev: option(Proof.t), z0, m: Web.MissingStep.Model.t, step) => {
  let (at_exp, at_idx) =
    switch (Web.MissingStep.Update.written_target(m, step)) {
    | Some(t) => t
    | None => Alcotest.fail("no written target")
    };
  let head =
    Proof.fresh(
      EvalStep({
        at_idx: Exp.fresh(Atom(Int(Bigint.of_int(at_idx)))),
        at_exp:
          at_exp
          |> Substitution.in_exp(Environment.empty)
          |> Exp.replace_all_ids,
      }),
    );
  let (target, replacement) =
    switch (m.proof |> Calc.get_saved_opt |> Option.join, prev) {
    | (Some(hole), _) => (hole, head)
    | (None, Some(leaf)) => (leaf, Proof.fresh(Seq(leaf, head)))
    | (None, None) => Alcotest.fail("trailing row with no leaf to extend")
    };
  let z1 =
    Haz3lcore.EditorTransform.apply_patch(
      z0,
      Haz3lcore.EditorTransform.mk_proof_patch(
        ~target_id=Proof.rep_id(target),
        replacement,
      ),
    );
  (at_exp, at_idx, z1);
};

let cons_hole = z0 => {
  let (root, _) = LC.app_run(LC.zipper_exp(z0), "every");
  switch (Test_StepperBase.last_missing_step(LC.case_step(root, 1))) {
  | Some(m) => m
  | None => Alcotest.fail("no hole row in the cons case")
  };
};

/* One click on the cons case's last row: returns the next zipper, or None
   when nothing is offered. */
let click = (label, z0) => {
  let (root, _) = LC.app_run(LC.zipper_exp(z0), "every");
  let cons = LC.case_step(root, 1);
  let m =
    switch (Test_StepperBase.last_missing_step(cons)) {
    | Some(m) => m
    | None => Alcotest.fail(label ++ ": no last row in the cons case")
    };
  let steps =
    switch (m.next_steps |> Calc.get_saved_opt) {
    | Some(AvailableSteps(s)) => s
    | _ => []
    };
  observe(
    label
    ++ " goal: "
    ++ pr(m.eval_exp |> Calc.get_saved_opt |> Option.get)
    ++ " | offered: "
    ++ string_of_int(List.length(steps)),
  );
  switch (steps) {
  | [] => None
  | [step, ..._] =>
    let prev =
      switch (List.rev(chain_proofs(cons))) {
      | [p, ..._] => Some(p)
      | [] => None
      };
    let (at_exp, at_idx, z1) = take_step_write(~prev, z0, m, step);
    observe(label ++ " written selector: " ++ pr(at_exp) ++ " @ " ++ string_of_int(at_idx));
    let (root1, pm1) = LC.app_run(LC.zipper_exp(z1), "every");
    let written =
      switch (List.rev(chain_proofs(LC.case_step(root1, 1)))) {
      | [p, ..._] => p
      | [] => Alcotest.fail(label ++ ": no written step")
      };
    switch (written.term) {
    | EvalStep({at_exp: sel, _}) =>
      observe(
        label
        ++ " re-parsed selector vs selected node: "
        ++ Option.value(~default="equal", first_diff([], at_exp, sel)),
      )
    | _ => observe(label ++ " written step is not an EvalStep")
    };
    let entry =
      switch (ProofMap.lookup(Proof.rep_id(written), pm1)) {
      | Some(e) => e
      | None => Alcotest.fail(label ++ ": no proof-map entry")
      };
    let marks = List.map(ProofMark.show, entry.marks);
    observe(label ++ " marks: [" ++ String.concat("; ", marks) ++ "]");
    observe(label ++ " outgoing: " ++ Option.fold(~none="-", ~some=pr, entry.outgoing));
    check(list(string), label ++ ": the written step checks with no marks", [], marks);
    check(
      bool,
      label ++ ": checker outgoing == offered step taken at its source",
      true,
      Exp.fast_equal(
        Test_ProofMap.require_exp("outgoing", entry.outgoing),
        LC.offered_step_at_source(m, step, label),
      ),
    );
    Some(z1);
  };
};

let tests = (
  "ResetListDemoRepairs",
  [
    test_case(
      "R-2: Matt's next cons-case eval click writes a selector the checker finds",
      `Quick,
      () => {
        let z0 = LC.parse_zipper(matt_prefix);
        switch (click("click1", z0)) {
        | Some(_) => ()
        | None => Alcotest.fail("click1: nothing offered on the cons hole")
        };
      },
    ),
    test_case(
      "R-2: further eval clicks in the cons case keep writing findable selectors",
      `Quick,
      () => {
        let rec go = (i, z) =>
          if (i <= 3) {
            switch (click("chain" ++ string_of_int(i), z)) {
            | Some(z1) => go(i + 1, z1)
            | None => observe("chain" ++ string_of_int(i) ++ ": nothing offered; stop")
            };
          };
        go(1, LC.parse_zipper(matt_prefix));
      },
    ),
    test_case("control: quote_selector leaves a selector without lambda heads unchanged", `Quick, () =>
      check(
        bool,
        "all_lists(x :: xs) unchanged",
        true,
        Exp.fast_equal(
          Web.MissingStep.Update.quote_selector(parse("all_lists(x :: xs)")),
          parse("all_lists(x :: xs)"),
        ),
      )
    ),
  ],
);
