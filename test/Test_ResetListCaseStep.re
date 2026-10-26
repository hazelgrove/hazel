open Alcotest;
open Language;
open Util;

/* Reset-list-case (D73, reference 7a403a0e): a frontier whose goal applies
 * a let-bound function to the theorem's variable (`all_lists(xs) == true`)
 * offered no green step on 8182. Its only offered step was the
 * non-hidden VarLookup of `all_lists`, which the checker's own eval step
 * (Evaluator.re, advance_hidden) treats as proof-silent.
 *
 * Gates: the frontier offers a visible step that lands on a tile of the
 * row; what TakeStep writes (`eval <source> at <n> end`) checks freshly
 * through the evaluator with no marks. The 7a40 picker has no outgoing
 * preview, so the checker's fresh outgoing goal is held to (a) exact
 * agreement with the offered step taken at its source plus the checker's
 * own trailing silent steps (shared Evaluator.advance_hidden), every
 * other node untouched, and (b) literal expected goals where they are
 * closed. Identical occurrences keep their own index; case bodies are
 * written through the same proof patch. Programs are particular; the stepper is driven
 * through the same calculate pipeline as Theorems.re. */

module SB = Web.StepperBase;

let rec_all_lists = {|let all_lists = fun (xs : [Int]) ->
  case xs
  | [] => true
  | _ :: t => all_lists(t)
  end
in
theorem every = forall (xs : [Int]) -> all_lists(xs) == true proof ? in 0|};

let nonrec_all_lists = {|let all_lists = fun (xs : [Int]) ->
  case xs
  | [] => true
  | _ :: t => true
  end
in
theorem every = forall (xs : [Int]) -> all_lists(xs) == true proof ? in 0|};

let pos_skeleton = {|type Nt = +Z+S(Nt) in
let pos = fun e -> case e | Z => true | S(b) => true end in
theorem t = forall e: Nt -> pos(e) proof ? in 0|};

let closed = {|theorem t = 1 + 1 == 2 proof ? in 0|};

/* Two identical occurrences: each offered step keeps its own index. */
let rec_twice = {|let all_lists = fun (xs : [Int]) ->
  case xs
  | [] => true
  | _ :: t => all_lists(t)
  end
in
theorem every = forall (xs : [Int]) -> all_lists(xs) == all_lists(xs) proof ? in 0|};

/* Two identical CLOSED occurrences: each step's outgoing goal is literal,
   with only the selected copy reduced. */
let rec_twice_nil = {|let all_lists = fun (xs : [Int]) ->
  case xs
  | [] => true
  | _ :: t => all_lists(t)
  end
in
theorem both = all_lists([]) == all_lists([]) proof ? in 0|};

/* As Theorems.Update.calculate wires it in the APP: the goal is the
   statement's core as recorded (no Builtins env substitution, no re-id),
   and the semantic ctx comes from SemanticCtx.of_program_state.
   (Test_RootMissingRepro.mk_thm_steppers differs on both points.) */
let rec app_run = (exp: Exp.t, name): (SB.next_step, ProofMap.t) => {
  let (state, info_map, _elab) = Test_ProofMap.eval_with_proof(exp);
  let pm = EvaluatorState.get_proof_map(state);
  let settings = {
    ...CoreSettings.on,
    evaluation: {
      ...CoreSettings.on.evaluation,
      enable_proof: true,
    },
  };
  switch (
    EvaluatorState.get_theorems(state)
    |> List.find_opt(((_, n, _, _)) => n == name)
  ) {
  | None => Alcotest.fail("no theorem " ++ name)
  | Some((id, _, env, stmt)) =>
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
        switch (user_term |> Exp.term_of) {
        | Theorem(_, _, proof, _) => proof
        | _ => Proof.fresh(EmptyHole)
        }
      | None => Proof.fresh(EmptyHole)
      };
    (
      SB.Stepper.calculate(
        ~settings=Calc.NewValue(settings),
        ~ctx=Calc.NewValue(sem_ctx),
        ~exp=Calc.NewValue(goal),
        ~ana=Calc.NewValue(Typ.fresh(Atom(Bool))),
        ~proof=Calc.NewValue(proof),
        ~proof_map=Calc.NewValue(pm),
        ~proof_info_map=Calc.NewValue(info_map),
        SB.init_step,
      ),
      pm,
    );
  };
}
and app_root = (src: string, name): SB.next_step =>
  app_run(Test_Evaluator_Prelude.parse_exp(src), name) |> fst;

let steps_of = (root: SB.next_step): list(EvaluatorStep.step) =>
  switch (root) {
  | MissingStep(m, _) =>
    switch (m.next_steps |> Calc.get_saved_opt) {
    | Some(AvailableSteps(steps)) => steps
    | Some(AutoStep(_))
    | None => []
    }
  | _ => Alcotest.fail("root is not a missing step")
  };

let show_steps = (label, steps) =>
  print_endline(
    "STEPS "
    ++ label
    ++ ": "
    ++ String.concat(
         " | ",
         List.map(
           (s: EvaluatorStep.step) =>
             Transition.show_step_kind(EvaluatorStep.get_step_kind(s))
             ++ " @ "
             ++ Exp.show(EvaluatorStep.get_at_exp(s))
             |> (x => String.length(x) > 120 ? String.sub(x, 0, 120) : x),
           steps,
         ),
       ),
  );

/* The root frontier's offered steps. */
let root_steps = (src, name): list(EvaluatorStep.step) =>
  switch (List.assoc_opt(name, Test_RootMissingRepro.mk_thm_steppers(src))) {
  | Some(MissingStep(m, _)) =>
    switch (m.next_steps |> Calc.get_saved_opt) {
    | Some(AvailableSteps(steps)) => steps
    | Some(AutoStep(_))
    | None => []
    }
  | Some(_) => Alcotest.fail(name ++ ": root is not a missing step")
  | None => Alcotest.fail("no theorem " ++ name)
  };

/* The frontier's offered steps as the row would render them: each
   step's DISPLAY target (MissingStep.Update.display_step_id, onto the
   folded goal) must be a tile of the row's own editor, or the green
   highlight has nowhere to land (StepperEditor.deco). */
let highlightable = (root: SB.next_step, step): bool =>
  switch (root) {
  | MissingStep(m, _) =>
    let editor: Web.CodeSelectable.Model.t =
      m.editor |> Calc.get_saved_opt |> Option.get;
    Haz3lcore.TermData.root_tile(
      Web.MissingStep.Update.display_step_id(m, step),
      editor.editor.syntax.term_data,
    )
    != None;
  | _ => false
  };

let offers_app = (label, src, name) => {
  let root = app_root(src, name);
  let steps = steps_of(root);
  show_steps("app " ++ label, steps);
  print_endline(
    "HIGHLIGHTABLE "
    ++ label
    ++ ": "
    ++ String.concat(
         ",",
         List.map(s => string_of_bool(highlightable(root, s)), steps),
       ),
  );
  check(
    bool,
    label ++ ": a step other than the cosmetic lookup of a name",
    true,
    List.exists(
      (s: EvaluatorStep.step) =>
        EvaluatorStep.get_step_kind(s) != Transition.VarLookup,
      steps,
    ),
  );
  check(
    bool,
    label ++ ": every offered step lands on a tile of the row (highlightable)",
    true,
    steps != [] && List.for_all(highlightable(root), steps),
  );
};

let model_of = (root: SB.next_step): Web.MissingStep.Model.t =>
  switch (root) {
  | MissingStep(m, _) => m
  | _ => Alcotest.fail("root is not a missing step")
  };

let replace_once = (~sub, ~by, s) => {
  let n = String.length(sub);
  let rec go = i =>
    if (i + n > String.length(s)) {
      Alcotest.fail("no " ++ sub);
    } else if (String.sub(s, i, n) == sub) {
      String.sub(s, 0, i)
      ++ by
      ++ String.sub(s, i + n, String.length(s) - i - n);
    } else {
      go(i + 1);
    };
  go(0);
};

/* Agreement, not an app preview (the 7a40 picker shows no outgoing goal;
   it highlights the step's tile and writes `eval <source> at <n> end`):
   the offered step taken at its source node, followed by the checker's
   own trailing silent steps (the shared Evaluator.advance_hidden), with
   every other node of the written goal, including identical occurrences,
   untouched. The fresh checker outgoing must equal this exactly. */
let offered_step_at_source = (m: Web.MissingStep.Model.t, step, label) => {
  let written = m.eval_exp |> Calc.get_saved_opt |> Option.get;
  let env = m.cached_env |> Calc.get_saved_opt |> Option.get;
  let source =
    switch (Web.MissingStep.Update.step_source(m, step)) {
    | Some(id) => id
    | None => Alcotest.fail(label ++ ": no source node")
    };
  switch (EvaluatorStep.take_step(step)) {
  | Some(e) =>
    switch (Evaluator.advance_hidden(~max_steps=64, ~env, e)) {
    | Some((_, e)) => ProofHacks.replace_exp_id(source, written, e)
    | None => Alcotest.fail(label ++ ": trailing silent steps exhausted")
    }
  | None => Alcotest.fail(label ++ ": stepper cannot take the step")
  };
};

/* The operands of an `==` goal. */
let eq_sides = (e: Exp.t): (Exp.t, Exp.t) =>
  switch (Exp.term_of(e)) {
  | BinOp(Poly(Equals), l, r) => (l, r)
  | _ => Alcotest.fail("not an == goal")
  };

/* Which operand of the written `==` goal a duplicate's offered step writes
   (its source), after checking that its highlight lands on the same
   operand of the displayed goal. */
let side_of = (label, m: Web.MissingStep.Model.t, step) => {
  let (l, r) = eq_sides(m.eval_exp |> Calc.get_saved_opt |> Option.get);
  let (dl, dr) = eq_sides(m.full_exp |> Calc.get_saved_opt |> Option.get);
  let source = Web.MissingStep.Update.step_source(m, step) |> Option.get;
  let side =
    if (source == Exp.rep_id(l)) {
      `Left;
    } else if (source == Exp.rep_id(r)) {
      `Right;
    } else {
      Alcotest.fail(label ++ ": source is neither operand");
    };
  check(
    bool,
    label ++ ": highlight is on the written operand",
    true,
    Web.MissingStep.Update.display_step_id(m, step)
    == Exp.rep_id(side == `Left ? dl : dr),
  );
  side;
};

/* A literal expected outgoing goal. */
let check_literal_outgoing = (label, ~expect, outgoing) => {
  let exact =
    Exp.fast_equal(outgoing, Test_Evaluator_Prelude.parse_exp(expect));
  if (!exact) {
    print_endline(
      "CHECKER  " ++ label ++ ": " ++ Test_ProofMap.print_exp(outgoing),
    );
  };
  check(bool, label ++ ": checker outgoing is `" ++ expect ++ "`", true, exact);
};

/* Write what TakeStep writes for `step` (`eval <at_exp> at <idx> end`,
   with at_exp asserted equal to `expect_text`) into the source, check it
   freshly through the evaluator, and require the checker's outgoing goal
   to agree exactly with the offered step taken at its source (and, where
   given, to be a literal expected goal). */
let written_checks =
    (
      label,
      src,
      name,
      ~pick: list(EvaluatorStep.step) => option(EvaluatorStep.step),
      ~expect_text,
      ~expect_idx,
      ~expect_outgoing: option(string)=?,
      ~check_outgoing: Exp.t => unit=_ => (),
      (),
    ) => {
  let root = app_root(src, name);
  let m = model_of(root);
  let step =
    switch (pick(steps_of(root))) {
    | Some(s) => s
    | None => Alcotest.fail(label ++ ": no such offered step")
    };
  let (at_exp, at_idx) =
    switch (Web.MissingStep.Update.written_target(m, step)) {
    | Some(t) => t
    | None => Alcotest.fail(label ++ ": no written target")
    };
  check(
    bool,
    label ++ ": writes " ++ expect_text,
    true,
    Exp.fast_equal(at_exp, Test_Evaluator_Prelude.parse_exp(expect_text)),
  );
  check(int, label ++ ": occurrence index", expect_idx, at_idx);
  let written_src =
    replace_once(
      ~sub="proof ?",
      ~by=
        "proof eval "
        ++ expect_text
        ++ " at "
        ++ string_of_int(at_idx)
        ++ " end",
      src,
    );
  print_endline("WRITTEN " ++ label ++ ": " ++ written_src);
  let (state, _, elab) =
    Test_ProofMap.eval_with_proof(
      Test_Evaluator_Prelude.parse_exp(written_src),
    );
  let entry = Test_ProofMap.proof_entry(state, elab);
  check(
    list(string),
    label ++ ": fresh check has no marks",
    [],
    List.map(ProofMark.show, entry.marks),
  );
  let outgoing =
    Test_ProofMap.require_exp(label ++ ": checker outgoing", entry.outgoing);
  let stepped = offered_step_at_source(m, step, label);
  let exact = Exp.fast_equal(outgoing, stepped);
  if (!exact) {
    let pr = e => Test_ProofMap.print_exp(e);
    print_endline("CHECKER  " ++ label ++ ": " ++ pr(outgoing));
    print_endline("STEPPED  " ++ label ++ ": " ++ pr(stepped));
  };
  check(
    bool,
    label ++ ": checker outgoing == offered step taken at its source",
    true,
    exact,
  );
  Option.iter(
    expect => check_literal_outgoing(label, ~expect, outgoing),
    expect_outgoing,
  );
  check_outgoing(outgoing);
};

/* ---- Case body: TakeStep's write into an induction case's hole ------- */

let rec_cases = {|let all_lists = fun (xs : [Int]) ->
  case xs
  | [] => true
  | _ :: t => all_lists(t)
  end
in
theorem every = forall (xs : [Int]) -> all_lists(xs) == true proof induction xs | [] => ? | x :: t => ? end in 0|};

/* The same program with the reference-native unparenthesized binder
   (`forall xs: [Int]`); the parenthesized fixture above is kept as
   matched against 8182. */
let rec_cases_bare = {|let all_lists = fun (xs : [Int]) ->
  case xs
  | [] => true
  | _ :: t => all_lists(t)
  end
in
theorem every = forall xs: [Int] -> all_lists(xs) == true proof induction xs | [] => ? | x :: t => ? end in 0|};

/* Every mark anywhere in the checker's proof map for the program. */
let all_marks = (pm: ProofMap.t): list(string) =>
  Id.Map.bindings(pm)
  |> List.concat_map(((_, e: ProofMap.entry)) =>
       List.map(ProofMark.show, e.marks)
     );

let parse_zipper = (s: string): Haz3lcore.Zipper.t =>
  switch (Haz3lcore.Parser.to_zipper(s, ~root=Exp)) {
  | Some(z) => z
  | None => Alcotest.fail("failed to parse zipper")
  };

let zipper_exp = z => Haz3lcore.MakeTerm.from_zip_for_sem(z, ~root=Exp).term;

let zipper_text = z => Haz3lcore.Printer.of_zipper(~holes="?", z);

let case_step = (root: SB.next_step, i): SB.next_step =>
  switch (root) {
  | NextStep({step_kind: InductionStep(im), _}) =>
    switch (List.nth_opt(im.cases, i)) {
    | Some(c) => c.step
    | None => Alcotest.fail("no case " ++ string_of_int(i))
    }
  | _ => Alcotest.fail("root is not an induction step")
  };

/* Take the case's first offered step as StepperBase's TakeStep does
   (written_target -> eval_step_term -> ReplaceProof patch on the case's
   hole), check the patched program freshly, and compare the written
   step's outgoing goal with the offered step taken at its source, and
   with a literal expected goal, exactly. */
let case_body_take = (~fixture, ~src, i, ~expect_text, ~expect_outgoing=?, ()) => {
  let z0 = parse_zipper(src);
  let (root, pm0) = app_run(zipper_exp(z0), "every");
  let label = fixture ++ " case " ++ string_of_int(i);
  print_endline(
    "MARKS before " ++ label ++ ": [" ++ String.concat("; ", all_marks(pm0)) ++ "]",
  );
  let m = model_of(case_step(root, i));
  let step =
    switch (steps_of(case_step(root, i))) {
    | [s, ..._] => s
    | [] => Alcotest.fail(label ++ ": no offered step")
    };
  let (at_exp, at_idx) =
    switch (Web.MissingStep.Update.written_target(m, step)) {
    | Some(t) => t
    | None => Alcotest.fail(label ++ ": no written target")
    };
  let hole =
    switch (m.proof |> Calc.get_saved_opt |> Option.join) {
    | Some(p) => p
    | None => Alcotest.fail(label ++ ": no hole")
    };
  /* StepperBase.eval_step_term + add_step_patch(ReplaceProof(hole)). */
  let term: TermBase.Proof.term =
    EvalStep({
      at_idx: Exp.fresh(Atom(Int(Bigint.of_int(at_idx)))),
      at_exp:
        at_exp
        |> Substitution.in_exp(Environment.empty)
        |> Exp.replace_all_ids,
    });
  let z1 =
    Haz3lcore.EditorTransform.apply_patch(
      z0,
      Haz3lcore.EditorTransform.mk_proof_patch(
        ~target_id=Proof.rep_id(hole),
        Proof.fresh(term),
      ),
    );
  let text = zipper_text(z1);
  print_endline("PATCHED " ++ label ++ ": " ++ text);
  check(
    bool,
    label ++ ": the case body now reads `eval " ++ expect_text ++ " at 0 end`",
    true,
    Test_StepperBase.contains(
      text,
      "eval " ++ expect_text ++ " at 0 end",
    ),
  );
  /* Fresh check of the patched program. */
  let (root1, pm1) = app_run(zipper_exp(z1), "every");
  print_endline(
    "MARKS after " ++ label ++ ": [" ++ String.concat("; ", all_marks(pm1)) ++ "]",
  );
  let written_step =
    switch (case_step(root1, i)) {
    | NextStep({step_kind: EvalStep(_), proof, _}) =>
      proof |> Calc.get_saved_opt |> Option.get
    | _ => Alcotest.fail(label ++ ": case is not a written eval step")
    };
  let entry =
    switch (ProofMap.lookup(Proof.rep_id(written_step), pm1)) {
    | Some(e) => e
    | None => Alcotest.fail(label ++ ": no proof-map entry")
    };
  check(
    list(string),
    label ++ ": fresh check has no marks",
    [],
    List.map(ProofMark.show, entry.marks),
  );
  let outgoing =
    Test_ProofMap.require_exp(label ++ ": checker outgoing", entry.outgoing);
  let stepped = offered_step_at_source(m, step, label);
  let exact = Exp.fast_equal(outgoing, stepped);
  if (!exact) {
    print_endline("CHECKER  " ++ label ++ ": " ++ Test_ProofMap.print_exp(outgoing));
    print_endline("STEPPED  " ++ label ++ ": " ++ Test_ProofMap.print_exp(stepped));
  };
  check(
    bool,
    label ++ ": checker outgoing == offered step taken at its source",
    true,
    exact,
  );
  Option.iter(
    expect => check_literal_outgoing(label, ~expect, outgoing),
    expect_outgoing,
  );
  /* The other case is untouched by the write. */
  check(
    bool,
    label ++ ": exactly one hole fewer",
    true,
    List.length(String.split_on_char('?', text))
    == List.length(String.split_on_char('?', zipper_text(z0))) - 1,
  );
};

let not_lookup = (s: EvaluatorStep.step) =>
  EvaluatorStep.get_step_kind(s) != Transition.VarLookup;

let offers = (label, src, name) =>
  check(
    bool,
    label ++ ": the frontier offers a step",
    true,
    root_steps(src, name) != [],
  );

let tests = (
  "ResetListCaseStep",
  [
    test_case("control: closed arithmetic offers a step", `Quick, () =>
      offers_app("closed", closed, "t")
    ),
    test_case(
      "RED before the fix: recursive all_lists(xs) == true offers a step",
      `Quick,
      () =>
      offers_app("recursive all_lists", rec_all_lists, "every")
    ),
    test_case("non-recursive all_lists(xs) == true offers a step", `Quick, () =>
      offers_app("non-recursive all_lists", nonrec_all_lists, "every")
    ),
    test_case("ProofDoc pos(e) offers a step", `Quick, () =>
      offers_app("pos(e)", pos_skeleton, "t")
    ),
    test_case(
      "recursive all_lists: written eval checks freshly and agrees with the offered step",
      `Quick,
      () =>
      written_checks(
        "recursive",
        rec_all_lists,
        "every",
        ~pick=List.find_opt(not_lookup),
        ~expect_text="all_lists(xs)",
        ~expect_idx=0,
        (),
      )
    ),
    test_case(
      "non-recursive all_lists: written eval checks freshly and agrees with the offered step",
      `Quick,
      () =>
      written_checks(
        "non-recursive",
        nonrec_all_lists,
        "every",
        ~pick=List.find_opt(not_lookup),
        ~expect_text="all_lists(xs)",
        ~expect_idx=0,
        (),
      )
    ),
    test_case("pos(e): written eval checks freshly and agrees with the offered step", `Quick, () =>
      written_checks(
        "pos",
        pos_skeleton,
        "t",
        ~pick=List.find_opt(not_lookup),
        ~expect_text="pos(e)",
        ~expect_idx=0,
        (),
      )
    ),
    test_case("closed: written eval checks freshly and agrees with the offered step", `Quick, () =>
      written_checks(
        "closed",
        closed,
        "t",
        ~pick=List.find_opt(not_lookup),
        ~expect_text="1 + 1",
        ~expect_idx=0,
        ~expect_outgoing="2 == 2",
        (),
      )
    ),
    test_case(
      "case body [] (8182 fixture): TakeStep write checks freshly",
      `Quick,
      () =>
      case_body_take(
        ~fixture="paren",
        ~src=rec_cases,
        0,
        ~expect_text="all_lists([])",
        ~expect_outgoing="true == true",
        (),
      )
    ),
    test_case(
      "case body x :: t (8182 fixture): TakeStep write checks freshly",
      `Quick,
      () =>
      case_body_take(
        ~fixture="paren",
        ~src=rec_cases,
        1,
        ~expect_text="all_lists(x :: t)",
        (),
      )
    ),
    test_case(
      "case body [] (forall xs: [Int]): TakeStep write checks freshly",
      `Quick,
      () =>
      case_body_take(
        ~fixture="bare",
        ~src=rec_cases_bare,
        0,
        ~expect_text="all_lists([])",
        ~expect_outgoing="true == true",
        (),
      )
    ),
    test_case(
      "case body x :: t (forall xs: [Int]): TakeStep write checks freshly",
      `Quick,
      () =>
      case_body_take(
        ~fixture="bare",
        ~src=rec_cases_bare,
        1,
        ~expect_text="all_lists(x :: t)",
        (),
      )
    ),
    test_case(
      "repeated all_lists(xs): each occurrence keeps its index",
      `Quick,
      () => {
        let root = app_root(rec_twice, "every");
        let steps = steps_of(root);
        show_steps("twice", steps);
        check(int, "two offered steps", 2, List.length(steps));
        let sides =
          List.mapi(
            (i, s) => side_of("twice#" ++ string_of_int(i), model_of(root), s),
            steps,
          );
        check(bool, "the two steps write different operands", true, List.nth(sides, 0) != List.nth(sides, 1));
        List.iteri(
          (i, side) =>
            written_checks(
              "twice#" ++ string_of_int(i),
              rec_twice,
              "every",
              ~pick=steps' => List.nth_opt(steps', i),
              ~expect_text="all_lists(xs)",
              ~expect_idx=i,
              /* The other copy is untouched. */
              ~check_outgoing=
                outgoing => {
                  let (l, r) = eq_sides(outgoing);
                  check(
                    bool,
                    "twice#" ++ string_of_int(i) ++ ": other copy untouched",
                    true,
                    Exp.fast_equal(
                      side == `Left ? r : l,
                      Test_Evaluator_Prelude.parse_exp("all_lists(xs)"),
                    ),
                  );
                },
              (),
            ),
          sides,
        );
      },
    ),
    test_case(
      "repeated closed all_lists([]): only the written copy reduces",
      `Quick,
      () => {
        let root = app_root(rec_twice_nil, "both");
        let steps = steps_of(root);
        check(int, "two offered steps", 2, List.length(steps));
        let sides =
          List.mapi(
            (i, s) =>
              side_of("twice-nil#" ++ string_of_int(i), model_of(root), s),
            steps,
          );
        check(bool, "the two steps write different operands", true, List.nth(sides, 0) != List.nth(sides, 1));
        List.iteri(
          (i, side) =>
            written_checks(
              "twice-nil#" ++ string_of_int(i),
              rec_twice_nil,
              "both",
              ~pick=steps' => List.nth_opt(steps', i),
              ~expect_text="all_lists([])",
              ~expect_idx=i,
              ~expect_outgoing=
                side == `Left
                  ? "true == all_lists([])" : "all_lists([]) == true",
              (),
            ),
          sides,
        );
      },
    ),
  ],
);
