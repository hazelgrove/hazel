open Alcotest;
open Language;
open Util;

/* R-reset-list-step-0116-5 (Matt, clicks only): in the cons case, after
 * `eval all_lists(x :: xs) at 0 end`, clicking the `ih` assumption wrote
 * `axiom ih at 0 on <inlined lambda application> end` and got "ih is not
 * a hypothesis in scope". This replays the AxiomsBox offer and write
 * exactly (display node as at_exp, ProofHacks.exp_idx on the display
 * goal, StepperBase.axiom_step_term) for Matt's `x:: xs` (rebinds the
 * scrutinee name) and a non-shadowing `x:: t` control, and separates the
 * STATICS verdict (FreeHypothesis) from the CHECKER's marks. */

module LC = Test_ResetListCaseStep;
module DR = Test_ResetListDemoRepairs;

let pr = Test_ProofMap.print_exp;

let observe = s => print_endline("OBSERVED " ++ s);

let control_prefix = {|let all_lists = fun (xs:([Int])) -> case xs
| [] => true
| _:: t => all_lists(t)
end in
theorem every = forall(xs:([Int])) -> all_lists(xs) == true proof induction xs
| [] =>
    eval all_lists([]) at 0 end;
    eval true == true at 0 end
| x:: t =>
    eval all_lists(x :: t) at 0 end; ?
end in
0|};

/* Statics errors anywhere in a program (Info.show contains the error). */
let statics_errors = (statics: Statics.Map.t): list(string) =>
  Statics.Map.error_ids(statics)
  |> List.filter_map(id => Statics.Map.lookup(id, statics))
  |> List.map(info => {
       let s = Info.show(info);
       let k =
         ["FreeHypothesis", "AxiomSlotNotHypothesis"]
         |> List.filter(k =>
              switch (Str.search_forward(Str.regexp_string(k), s, 0)) {
              | _ => true
              | exception Not_found => false
              }
            );
       k == [] ? "other" : String.concat("+", k);
     });

/* The row's checker facts (MissingStep.assumptions = ProofCtx of the
   row's semantic ctx) and which are active on `sel` (AxiomsBox filter). */
let facts = (m: Web.MissingStep.Model.t) =>
  m.assumptions
  |> Calc.get_saved_opt
  |> Option.join
  |> Option.value(~default=[]);

/* The cons hole, its facts/offers, and Matt's `ih` write (AxiomsBox:
   display node, exp_idx on the display goal, axiom_step_term; Matt's
   `axiom` = Right). `quote` = test-side quote_selector variant. */
let ih_write = (~quote, label, src) => {
  let z0 = LC.parse_zipper(src);
  let m = DR.cons_hole(z0);
  let full = m.full_exp |> Calc.get_saved_opt |> Option.get;
  let env = m.cached_env |> Calc.get_saved_opt |> Option.get;
  let editor = m.editor |> Calc.get_saved_opt |> Option.get;
  let info_map = Web.CodeEditable.Model.get_statics(editor).info_map;
  let (sel, _) = LC.eq_sides(full);
  let all = facts(m);
  let names = List.map((a: Web.AssumptionBox.Model.t) => a.ctx_entry.name, all);
  let active =
    all
    |> List.filter((a: Web.AssumptionBox.Model.t) =>
         ProofRule.is_active(
           ~info_map,
           ~env,
           ProofRule.with_bool_fact_reading(~info_map=Some(info_map), a.ctx_entry.rule),
           sel |> DHExp.strip_ascriptions,
         )
       )
    |> List.map((a: Web.AssumptionBox.Model.t) => a.ctx_entry.name);
  observe(label ++ " checker facts in row ctx include ih: " ++ string_of_bool(List.mem("ih", names)));
  switch (List.find_opt((a: Web.AssumptionBox.Model.t) => a.ctx_entry.name == "ih", all)) {
  | Some(a) => observe(label ++ " ih statement: " ++ pr(ProofRule.rule_to_exp(a.ctx_entry.rule)))
  | None => ()
  };
  observe(label ++ " active on selected: [" ++ String.concat(", ", active) ++ "]");
  let at_idx = ProofHacks.exp_idx(sel, full) |> Option.value(~default=0);
  let at_exp = quote ? Web.MissingStep.Update.quote_selector(sel) : sel;
  let hole = m.proof |> Calc.get_saved_opt |> Option.join |> Option.get;
  let term: TermBase.Proof.term =
    AxiomStep({
      at_idx: Exp.fresh(Atom(Int(Bigint.of_int(at_idx)))),
      at_exp: at_exp |> Substitution.in_exp(Environment.empty) |> Exp.replace_all_ids,
      direction: Right,
      equality: Exp.fresh(Var("ih")),
      instantiation: None,
    });
  let z1 =
    Haz3lcore.EditorTransform.apply_patch(
      z0,
      Haz3lcore.EditorTransform.mk_proof_patch(~target_id=Proof.rep_id(hole), Proof.fresh(term)),
    );
  print_endline("WRITTEN " ++ label ++ ":\n" ++ LC.zipper_text(z1));
  (z0, z1, active);
};

/* STATICS ONLY (no evaluation): prefix baseline vs the written program. */
let statics_verdict = (label, src) => {
  let (z0, z1, active) = ih_write(~quote=false, label, src);
  let (st0, _) = Test_ProofMap.statics_and_elab(LC.zipper_exp(z0));
  let (st1, _) = Test_ProofMap.statics_and_elab(LC.zipper_exp(z1));
  let e0 = statics_errors(st0);
  let e1 = statics_errors(st1);
  observe(label ++ " STATICS prefix errors: [" ++ String.concat(", ", e0) ++ "]");
  observe(label ++ " STATICS after ih write errors: [" ++ String.concat(", ", e1) ++ "]");
  check(bool, label ++ ": ih is offered (checker ctx)", true, List.mem("ih", active));
  check(bool, label ++ ": prefix has no FreeHypothesis", false, List.mem("FreeHypothesis", e0));
  check(bool, label ++ ": predicted: statics marks FreeHypothesis after the ih write", true, List.mem("FreeHypothesis", e1));
};

/* CHECKER (evaluates; run last). */
let checker_marks = (~quote, label, src) => {
  let (_, z1, _) = ih_write(~quote, label, src);
  let (root1, pm1) = LC.app_run(LC.zipper_exp(z1), "every");
  let written =
    switch (List.rev(DR.chain_proofs(LC.case_step(root1, 1)))) {
    | [p, ..._] => p
    | [] => Alcotest.fail(label ++ ": no written step")
    };
  let (marks, outgoing) =
    switch (ProofMap.lookup(Proof.rep_id(written), pm1)) {
    | Some(e) => (List.map(ProofMark.show, e.marks), Option.fold(~none="-", ~some=pr, e.outgoing))
    | None => (["<no proof-map entry>"], "-")
    };
  observe(label ++ " CHECKER marks: [" ++ String.concat("; ", marks) ++ "] outgoing: " ++ outgoing);
  marks;
};

let has = (k, marks) =>
  List.exists(m => switch (Str.search_forward(Str.regexp_string(k), m, 0)) { | _ => true | exception Not_found => false }, marks);

let tests = (
  "ResetListIHGesture",
  [
    test_case("STATICS x:: xs (Matt): FreeHypothesis after the ih write", `Quick, () =>
      statics_verdict("shadow", DR.matt_prefix)
    ),
    test_case("STATICS x:: t (control): FreeHypothesis after the ih write", `Quick, () =>
      statics_verdict("control", control_prefix)
    ),
    test_case("CHECKER x:: xs, production write: predicted PatternNotFound", `Quick, () =>
      check(bool, "shadow-prod: PatternNotFound", true, has("PatternNotFound", checker_marks(~quote=false, "shadow-prod", DR.matt_prefix)))
    ),
    test_case("CHECKER x:: xs, quoted write: predicted no checker marks", `Quick, () =>
      check(list(string), "shadow-quoted: checker marks", [], checker_marks(~quote=true, "shadow-quoted", DR.matt_prefix))
    ),
    test_case("CHECKER x:: t, production write: predicted PatternNotFound", `Quick, () =>
      check(bool, "control-prod: PatternNotFound", true, has("PatternNotFound", checker_marks(~quote=false, "control-prod", control_prefix)))
    ),
    test_case("CHECKER x:: t, quoted write: predicted no checker marks", `Quick, () =>
      check(list(string), "control-quoted: checker marks", [], checker_marks(~quote=true, "control-quoted", control_prefix))
    ),
  ],
);
