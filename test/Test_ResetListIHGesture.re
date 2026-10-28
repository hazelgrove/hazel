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

/* Statics error kinds anywhere in a program, read DIRECTLY from each
   erroneous id's marks (Info.marks_of). The earlier version stringified the
   whole Info with Info.show (recursive ctx included) for every error id; all
   runs that stalled were exactly the runs with statics errors (integrator
   hypothesis, 2026-10-28), so that stringification is the suspect, not
   statics. Semantics unchanged: FreeHypothesis / AxiomSlotNotHypothesis /
   other. */
let statics_errors = (statics: Statics.Map.t): list(string) =>
  Statics.Map.error_ids(statics)
  |> List.filter_map(id => Statics.Map.lookup(id, statics))
  |> List.concat_map(info =>
       switch (Info.marks_of(info)) {
       | [] => ["other"]
       | marks =>
         List.map(
           (m: Mark.t) =>
             switch (m) {
             | FreeHypothesis(_) => "FreeHypothesis"
             | AxiomSlotNotHypothesis(_) => "AxiomSlotNotHypothesis"
             | _ => "other"
             },
           marks,
         )
       }
     );

/* A checker mark's constructor name, without stringifying its payload
   (PatternNotFound/NothingToStep carry whole expressions). */
let mark_tag = (m: ProofMark.t): string =>
  switch (m) {
  | UnknownEquality(name) => "UnknownEquality(" ++ name ++ ")"
  | RuleDoesNotApply(_) => "RuleDoesNotApply"
  | PatternNotFound(_) => "PatternNotFound"
  | NothingToStep(_) => "NothingToStep"
  | _ => "other"
  };

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
    | Some(e) => (List.map(mark_tag, e.marks), Option.fold(~none="-", ~some=pr, e.outgoing))
    | None => (["<no proof-map entry>"], "-")
    };
  observe(label ++ " CHECKER marks: [" ++ String.concat("; ", marks) ++ "] outgoing: " ++ outgoing);
  marks;
};

let has = (k, marks) =>
  List.exists(m => switch (Str.search_forward(Str.regexp_string(k), m, 0)) { | _ => true | exception Not_found => false }, marks);

/* Reduced statics fixture: the same program with a SMALL `axiom ih`
   target written as text (statics does not locate targets). */
let small_target = (src, target) =>
  LC.replace_once(~sub="; ?", ~by="; axiom ih at 0 on " ++ target ++ " end", src);

let statics_only = (label, src) => {
  let t0 = Sys.time();
  let (st, _) = Test_ProofMap.statics_and_elab(LC.zipper_exp(LC.parse_zipper(src)));
  observe(Printf.sprintf("%s CHECKPOINT after statics_and_elab (cpu %.2fs)", label, Sys.time() -. t0));
  let errs = statics_errors(st);
  observe(Printf.sprintf("%s CHECKPOINT after error classification (cpu %.2fs)", label, Sys.time() -. t0));
  observe(
    Printf.sprintf(
      "%s STATICS errors: [%s] (cpu %.2fs)",
      label,
      String.concat(", ", errs),
      Sys.time() -. t0,
    ),
  );
  errs;
};

/* APP checker on the written program: Test_ExistentialsAppContext's
   main-thread recheck with paired statics (the accessor both CellEditor
   consumers read). Marks on the `axiom ih` node and the app status. */
let is_ih_axiom = (p: Proof.t) =>
  switch (p.term) {
  | AxiomStep({equality, _}) =>
    switch (Exp.term_of(equality)) {
    | Var("ih") => true
    | _ => false
    }
  | _ => false
  };

let app_marks = (~quote, label, src) => {
  let (_, z1, active) = ih_write(~quote, label, src);
  check(bool, label ++ ": ih offered (stepper)", true, List.mem("ih", active));
  let t0 = Sys.time();
  let a = Test_ExistentialsAppContext.app_of_source(LC.zipper_text(z1));
  observe(Printf.sprintf("%s CHECKPOINT after app_of_source (cpu %.2fs)", label, Sys.time() -. t0));
  let node =
    switch (List.find_opt(is_ih_axiom, Test_ResetExistentials.proof_nodes(a.proof))) {
    | Some(n) => n
    | None => Alcotest.fail(label ++ ": no axiom ih node")
    };
  let marks = List.map(mark_tag, ProofMap.marks_of(Proof.rep_id(node), a.accessor));
  let out =
    switch (ProofMap.lookup(Proof.rep_id(node), a.accessor)) {
    | Some(e) => Option.fold(~none="-", ~some=pr, e.outgoing)
    | None => "<no app entry>"
    };
  let serr = statics_errors(a.statics.info_map);
  observe(
    Printf.sprintf(
      "%s APP marks: [%s] outgoing: %s | app status %s | statics errors [%s] (cpu %.2fs)",
      label,
      String.concat("; ", marks),
      out,
      a.status,
      String.concat(", ", serr),
      Sys.time() -. t0,
    ),
  );
  (marks, serr);
};

let tests = (
  "ResetListIHGesture",
  [
    /* 0 */ test_case("STATICS prefix (Matt x:: xs, no ih yet)", `Quick, () =>
      check(bool, "prefix: no FreeHypothesis", false, List.mem("FreeHypothesis", statics_only("prefix-shadow", DR.matt_prefix)))
    ),
    /* 1: pre-fix (c1 on c2f5/e7473b88) this case hit a 300s wall-clock
       bound during lid-closed sleep; post-fix contract: ih is known. */
    /* 1 */ test_case("STATICS reduced: x:: xs, axiom ih on all_lists(xs)", `Quick, () =>
      check(bool, "reduced-shadow: post-fix ih known (no FreeHypothesis)", false, List.mem("FreeHypothesis", statics_only("reduced-shadow", small_target(DR.matt_prefix, "all_lists(xs)"))))
    ),
    /* 2 */ test_case("STATICS reduced: x:: t, axiom ih on all_lists(t)", `Quick, () =>
      check(bool, "reduced-control: post-fix ih known (no FreeHypothesis)", false, List.mem("FreeHypothesis", statics_only("reduced-control", small_target(control_prefix, "all_lists(t)"))))
    ),
    /* 3 */ test_case("STATICS Matt exact write as production now writes it (quoted)", `Quick, () => {
      let (_, z1, active) = ih_write(~quote=true, "exact-shadow", DR.matt_prefix);
      check(bool, "exact-shadow: ih offered", true, List.mem("ih", active));
      check(bool, "exact-shadow: post-fix ih known (no FreeHypothesis)", false, List.mem("FreeHypothesis", statics_only("exact-shadow", LC.zipper_text(z1))));
    }),
    /* 4 */ test_case("APP x:: xs, old unquoted write (control): predicted PatternNotFound", `Quick, () => {
      let (marks, _) = app_marks(~quote=false, "shadow-old", DR.matt_prefix);
      check(bool, "shadow-old: PatternNotFound", true, has("PatternNotFound", marks));
    }),
    /* 5 */ test_case("APP x:: xs, production (quoted) write: no app marks, no statics error", `Quick, () => {
      let (marks, serr) = app_marks(~quote=true, "shadow-new", DR.matt_prefix);
      check(list(string), "shadow-new: app marks on axiom ih", [], marks);
      check(bool, "shadow-new: no FreeHypothesis", false, List.mem("FreeHypothesis", serr));
    }),
    /* 6 */ test_case("APP x:: t, old unquoted write (control): predicted PatternNotFound", `Quick, () => {
      let (marks, _) = app_marks(~quote=false, "control-old", control_prefix);
      check(bool, "control-old: PatternNotFound", true, has("PatternNotFound", marks));
    }),
    /* 7 */ test_case("APP x:: t, production (quoted) write: no app marks, no statics error", `Quick, () => {
      let (marks, serr) = app_marks(~quote=true, "control-new", control_prefix);
      check(list(string), "control-new: app marks on axiom ih", [], marks);
      check(bool, "control-new: no FreeHypothesis", false, List.mem("FreeHypothesis", serr));
    }),
  ],
);
