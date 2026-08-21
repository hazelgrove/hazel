open Alcotest;
open Language;
open Test_Evaluator_Prelude;
open Util;
open Haz3lcore;

// Import StepperBase types from web module
module StepperBase = Web.StepperBase;

// Helper constructors for step_kind_model (full step kinds only; MissingStep is a next_step)
module StepKindHelpers = {
  // Create an AxiomStep at a specific subexpression
  let mk_axiom_step =
      (
        ~name="reflexivity",
        ~at_idx=0,
        ~direction=Direction.Right,
        ~equality="refl_eq",
        at_exp,
      ) // Required parameter at the end
      : StepperBase.step_kind_model => {
    StepperBase.AxiomStep({
      name,
      at_idx,
      at_exp,
      direction,
      equality,
      next_exp: Calc.Pending,
    });
  };

  // Create an InductionStep with a scrutinee expression
  let mk_induction_step =
      (~exp: option(Exp.t)=?, ()): StepperBase.step_kind_model =>
    StepperBase.InductionStep(Web.InductionStep.init(~exp?, ()));

  // Create a ForallStep
  let mk_forall_step = (): StepperBase.step_kind_model =>
    StepperBase.ForallStep(Web.ForallStep.init(StepperBase.Stepper.init));
};

// Cell/no-proof sentinel used by Stepper.calculate
let empty_hole_proof = (): Calc.t(Proof.t) =>
  Calc.OldValue(Proof.fresh(EmptyHole));

// Helper to create a minimal step_model for testing
let mk_test_step_model =
    (
      ~step_kind: StepperBase.step_kind_model,
      ~next_step=StepperBase.init_step,
      ~proof=Calc.Pending,
      (),
    )
    : StepperBase.step_model => {
  cached_proof_map_entry: Calc.Pending,
  pre_editors: Calc.Pending,
  insert: Web.MissingStep.Model.init,
  post_editors: Calc.Pending,
  step_kind,
  next_step,
  proof,
};

// Wrap a full step as the root next_step
let mk_test_step =
    (
      ~step_kind: StepperBase.step_kind_model,
      ~next_step=StepperBase.init_step,
      (),
    )
    : StepperBase.next_step =>
  StepperBase.NextStep(mk_test_step_model(~step_kind, ~next_step, ()));

let mk_missing_step = (): StepperBase.next_step =>
  StepperBase.MissingStep(Web.MissingStep.Model.init, StepperBase.Finished);

// Helper constructors for InductionCase
module InductionCaseHelpers = {
  let mk_pattern_editor = (pattern_str: string): Web.CodeEditable.Model.t => {
    let pattern_exp = parse_exp(pattern_str);
    let editor =
      Editor.Model.mk(
        ~root=Exp,
        Zipper.unzip(
          ExpToSegment.exp_to_segment(
            ~settings=ExpToSegment.Settings.editable(~inline=true),
            pattern_exp,
          ),
        ),
      );
    Web.CodeEditable.Model.mk(editor);
  };

  // Create an InductionCase from a pattern string
  let mk_case =
      (
        ~pattern_str: string,
        ~inner_exp: option(Exp.t)=?,
        ~last_exp: option(Exp.t)=?,
        ~step: option(StepperBase.next_step)=?,
        (),
      )
      : Web.InductionCase.model'(StepperBase.next_step) => {
    {
      pattern: mk_pattern_editor(pattern_str),
      pattern_src: Calc.Pending,
      elab_pattern: Calc.Pending,
      inner_exp:
        inner_exp
        |> Option.map(exp => Calc.Calculated(exp))
        |> Option.value(~default=Calc.Pending),
      step: step |> Option.value(~default=StepperBase.init_step),
      last_exp:
        last_exp
        |> Option.map(exp => Calc.Calculated(exp))
        |> Option.value(~default=Calc.Pending),
      inner_ctx: Calc.Pending,
      hypotheses: Calc.Pending,
    };
  };

  // Create an axiom step for reflexivity
  let mk_reflexivity_step = (goal_exp: Exp.t): StepperBase.next_step => {
    let axiom_kind =
      StepKindHelpers.mk_axiom_step(
        ~name="refl_eq",
        ~equality="refl_eq",
        goal_exp,
      );
    mk_test_step(~step_kind=axiom_kind, ());
  };

  // Create an axiom step that rewrites using a hypothesis
  let mk_rewrite_step =
      (~hypothesis_name: string, goal_exp: Exp.t): StepperBase.next_step => {
    let axiom_kind =
      StepKindHelpers.mk_axiom_step(
        ~name=hypothesis_name,
        ~equality=hypothesis_name,
        goal_exp,
      );
    mk_test_step(~step_kind=axiom_kind, ());
  };

  // Build a step chain by linking step_kinds into next_step fields
  let rec build_step_chain =
          (step_kinds: list(StepperBase.step_kind_model))
          : StepperBase.next_step =>
    switch (step_kinds) {
    | [] => StepperBase.init_step
    | [kind] => mk_test_step(~step_kind=kind, ())
    | [kind, ...rest] =>
      mk_test_step(~step_kind=kind, ~next_step=build_step_chain(rest), ())
    };

  // Create an induction case from a pattern, goal, and list of step_kinds
  let mk_case_with_steps =
      (
        ~pattern_str: string,
        ~goal_str: string,
        step_kinds: list(StepperBase.step_kind_model),
      )
      : Web.InductionCase.model'(StepperBase.next_step) => {
    let goal_exp = parse_exp(goal_str);
    mk_case(
      ~pattern_str,
      ~inner_exp=goal_exp,
      ~step=build_step_chain(step_kinds),
      (),
    );
  };

  // Create an InductionStep with scrutinee and cases
  let mk_induction_with_cases =
      (
        ~scrut_str: string,
        ~cases: list(Web.InductionCase.model'(StepperBase.next_step)),
      )
      : StepperBase.step_kind_model => {
    let scrut_editor = mk_pattern_editor(scrut_str);

    let ind_model: Web.InductionStep.model'(StepperBase.next_step) = {
      scrut: scrut_editor,
      cases,
      scrut_src: Calc.Pending,
      elab_scrut_raw: Calc.Pending,
      elab_scrut_sub: Calc.Pending,
      scrut_ty: Calc.Pending,
      scrut_co_ctx: Calc.Pending,
      result: Calc.Pending,
      join_exp: Calc.Pending,
      inexhaustive: Calc.Pending,
      validity: Calc.Pending,
    };

    (StepperBase.InductionStep(ind_model): StepperBase.step_kind_model);
  };
};

// Helper to run calculate with minimal dependencies.
// Returns one next_step; outgoing/validity come from ProofMap, not the return.
let test_calculate =
    (
      ~exp: Exp.t,
      ~ctx=Builtins.ctx_init(Some(Int)),
      ~ana=IdTagged.FreshGrammar.Typ.int(),
      ~proof=empty_hole_proof(),
      ~proof_map=Calc.OldValue(Language.ProofMap.empty),
      model: StepperBase.next_step,
    )
    : StepperBase.next_step => {
  let settings = Calc.NewValue(CoreSettings.on);
  let exp_calc = Calc.NewValue(exp);
  let ctx_calc =
    Calc.NewValue(SemanticCtx.of_ctx_and_env(ctx, Builtins.env_init));
  let ana_calc = Calc.NewValue(ana);

  StepperBase.Stepper.calculate(
    ~settings,
    ~exp=exp_calc,
    ~ctx=ctx_calc,
    ~ana=ana_calc,
    ~proof,
    ~proof_map,
    model,
  );
};

let is_missing_step = (ns: StepperBase.next_step): bool =>
  StepperBase.is_missing_step(ns);

// A proof sub-term describing one real (axiom) step over `at_exp`
let axiom_step_proof = (~equality="refl_eq", at_exp: Exp.t): Calc.t(Proof.t) =>
  Calc.NewValue(
    Proof.fresh(
      AxiomStep({
        at_idx: Exp.fresh(Atom(Int(Bigint.of_int(0)))),
        at_exp,
        direction: Direction.Right,
        equality: Exp.fresh(Var(equality)),
        instantiation: None,
      }),
    ),
  );

/* ---- Wrapping proof forms: assume / revert / generalize -------------
 *
 * These three own a body proof rather than continuing a `Seq` chain, so a
 * row for them must (a) exist at all (`kind_of_proof`), (b) hand the body
 * to a nested stepper, and (c) hand it the scope and goal the CHECKER
 * computed — not a locally re-derived guess. */

/* Run a program through the checker and hand back its theorem proof and
   proof map, the two inputs a wrapping form's row sources from. */
let checked_proof = (src: string): (Proof.t, Language.ProofMap.t) => {
  let (state, _, elab) = Test_ProofMap.eval_with_proof(parse_exp(src));
  let proof =
    switch (Test_ProofMap.find_theorem_proof(elab)) {
    | Some(p) => p
    | None => Alcotest.fail("no theorem proof in: " ++ src)
    };
  (proof, EvaluatorState.get_proof_map(state));
};

/* Bind `x:Int` in the row's scope. `generalize` only re-quantifies a bare
   IN-SCOPE variable (otherwise ProofCheck marks it MalformedGeneralize and
   passes through), and in the app that binding comes from the theorem
   statement's peeled binders — which this unit-level ctx has to stand in
   for. */
let ctx_with_x =
  Ctx.extend(
    Builtins.ctx_init(Some(Int)),
    Ctx.VarEntry({
      name: "x",
      id: Id.mk(),
      typ: IdTagged.FreshGrammar.Typ.int(),
      custom_statics: None,
    }),
  );

/* Promote a MissingStep row against a checked program's proof. */
let promote =
    (~src: string, ~goal: string, ~ctx=ctx_with_x, ()): StepperBase.next_step => {
  let (proof, pm) = checked_proof(src);
  test_calculate(
    ~exp=parse_exp(goal),
    ~ctx,
    ~proof=Calc.NewValue(proof),
    ~proof_map=Calc.NewValue(pm),
    mk_missing_step(),
  );
};

/* The facts a step's inner scope offers, by name: the entries of the
   THEOREM namespace (docs/prover-obligations.md section 0.1). */
let facts_of_ctx = (ctx: SemanticCtx.t): list((string, Exp.t)) =>
  SemanticCtx.facts(ctx) |> List.map(((name, _id, fact)) => (name, fact));

let saved_exc = (~print: string, x: Calc.saved('a)): 'a =>
  switch (x) {
  | Calc.Calculated(v) => v
  | Calc.Pending => Alcotest.fail(print ++ " not calculated")
  };

/* ---- Insertion round-trip helpers (cf. test/Test_EditorTransform.re) -- */

let parse_zipper = (s: string): Zipper.t =>
  switch (Parser.to_zipper(s, ~root=Exp)) {
  | Some(z) => z
  | None => Alcotest.fail("failed to parse zipper: " ++ s)
  };

/* Write `term` over the theorem's (hole) proof, exactly as the
   step-picker's ProofPatch does for a MissingStep row whose backing proof
   is a hole (StepperBase.add_step_patch / ReplaceProof). Returns the
   patched zipper so callers can also ask where the caret can land. */
let insert_proof_zipper = (~src: string, term: TermBase.Proof.term): Zipper.t => {
  let z = parse_zipper(src);
  let root = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  let target_id =
    switch (Test_ProofMap.find_theorem_proof(root)) {
    | Some(p) => Proof.rep_id(p)
    | None => Alcotest.fail("no theorem proof in: " ++ src)
    };
  EditorTransform.apply_patch(
    z,
    EditorTransform.mk_proof_patch(~target_id, Proof.fresh(term)),
  );
};

let insert_proof_term = (~src: string, term: TermBase.Proof.term): string =>
  insert_proof_zipper(~src, term) |> Printer.of_zipper(~holes="?");

let contains = (haystack: string, needle: string): bool => {
  let hl = String.length(haystack);
  let nl = String.length(needle);
  let rec go = i =>
    i + nl <= hl && (String.sub(haystack, i, nl) == needle || go(i + 1));
  nl == 0 || go(0);
};

let check_contains = (~msg: string, haystack: string, needle: string) =>
  check(
    bool,
    msg ++ " — expected to find:\n" ++ needle ++ "\nin:\n" ++ haystack,
    true,
    contains(haystack, needle),
  );

let tests = (
  "StepperBase",
  [
    test_case(
      "after an eval step the trailing row offers the next redex",
      `Quick,
      () => {
        /* One written eval step on `1 + 4 == 5`: the stepper must
           synthesize a trailing MissingStep row for the `5 == 5` goal
           and offer its `==` redex as a clickable eval step. */
        let src = "theorem thm = 1 + 4 == 5 proof eval 1 + 4 at 0 end in thm";
        let (state, _, elab) =
          Test_ProofMap.eval_with_proof(parse_exp(src));
        let proof =
          switch (Test_ProofMap.find_theorem_proof(elab)) {
          | Some(p) => p
          | None => Alcotest.fail("no proof")
          };
        let pm = EvaluatorState.get_proof_map(state);
        let result =
          test_calculate(
            ~exp=parse_exp("1 + 4 == 5"),
            ~proof=Calc.NewValue(proof),
            ~proof_map=Calc.NewValue(pm),
            mk_missing_step(),
          );
        switch (result) {
        | StepperBase.NextStep({step_kind: EvalStep(_), next_step, _}) =>
          switch (next_step) {
          | StepperBase.MissingStep(m, _) =>
            switch (m.next_steps |> Calc.get_saved_opt) {
            | Some(EvaluatorStep.AvailableSteps([_])) => ()
            | Some(EvaluatorStep.AvailableSteps(steps)) =>
              Alcotest.fail(
                "expected one available step, got "
                ++ string_of_int(List.length(steps)),
              )
            | Some(EvaluatorStep.AutoStep(_)) =>
              Alcotest.fail("next redex unexpectedly auto-stepped")
            | None => Alcotest.fail("next steps not calculated")
            }
          | StepperBase.Finished =>
            Alcotest.fail("row is Finished; goal not yet discharged")
          | StepperBase.NextStep(_) =>
            Alcotest.fail("unexpected synthesized NextStep")
          }
        | _ => Alcotest.fail("expected NextStep(EvalStep) row")
        };
      },
    ),
    test_case(
      "recalculate with old inputs preserves step editor state",
      `Quick,
      () => {
        /* Editors in step rows carry live UI state (caret, selection).
           A calculate pass whose inputs are all old must NOT rebuild
           them — rebuilding snaps the caret to a fresh editor's end
           and makes selection in step rows impossible. */
        let src = "theorem thm = 1 + 4 == 5 proof eval 1 + 4 at 0 end in thm";
        let (state, _, elab) =
          Test_ProofMap.eval_with_proof(parse_exp(src));
        let proof =
          switch (Test_ProofMap.find_theorem_proof(elab)) {
          | Some(p) => p
          | None => Alcotest.fail("no proof")
          };
        let pm = EvaluatorState.get_proof_map(state);
        let goal = parse_exp("1 + 4 == 5");
        let calculated =
          test_calculate(
            ~exp=goal,
            ~proof=Calc.NewValue(proof),
            ~proof_map=Calc.NewValue(pm),
            mk_missing_step(),
          );
        /* Move the caret in the full step row's editor (fresh editors
           sit at the end, so Start is an observable change). */
        let moved =
          StepperBase.Stepper.update(
            ~settings=Web.Settings.Model.init,
            StepperBase.EditorAction(Web.CodeSelectable.Update.Move(Start)),
            calculated,
          ).
            model;
        /* Recalculate with everything marked old (a pure-UI pass). */
        let recalc =
          StepperBase.Stepper.calculate(
            ~settings=Calc.OldValue(CoreSettings.on),
            ~exp=Calc.OldValue(goal),
            ~ctx=
              Calc.OldValue(
                SemanticCtx.of_ctx_and_env(
                  Builtins.ctx_init(Some(Operators.default_mode)),
                  Builtins.env_init,
                ),
              ),
            ~ana=Calc.OldValue(IdTagged.FreshGrammar.Typ.int()),
            ~proof=Calc.OldValue(proof),
            ~proof_map=Calc.OldValue(pm),
            moved,
          );
        let editor_of = (ns: StepperBase.next_step) =>
          switch (ns) {
          | StepperBase.NextStep({insert, _}) =>
            insert.editor |> Calc.saved_to_option
          | _ => None
          };
        switch (editor_of(moved), editor_of(recalc)) {
        | (Some(before), Some(after)) =>
          check(
            bool,
            "step editor survives an old-input recalculate",
            true,
            before === after,
          )
        | _ => Alcotest.fail("expected a step editor before and after")
        };
        /* The synthesized trailing row must also keep its model. */
        let missing_of = (ns: StepperBase.next_step) =>
          switch (ns) {
          | StepperBase.NextStep({next_step: MissingStep(m, _), _}) =>
            Some(m)
          | _ => None
          };
        switch (missing_of(moved), missing_of(recalc)) {
        | (Some(m1), Some(m2)) =>
          switch (
            m1.editor |> Calc.get_saved_opt,
            m2.editor |> Calc.get_saved_opt,
          ) {
          | (Some(e1), Some(e2)) =>
            check(
              bool,
              "trailing row editor state survives an old-input recalculate",
              true,
              e1.editor.state.zipper == e2.editor.state.zipper,
            )
          | _ => Alcotest.fail("expected trailing row editors")
          }
        | _ => Alcotest.fail("expected a trailing MissingStep row")
        };
      },
    ),
    // ============================================================
    // Basic calculation tests
    // ============================================================
    test_case(
      "calculate empty missing step",
      `Quick,
      () => {
        // A hole-shaped proof is what a MissingStep row stands in for, so
        // promote-or-stay stays.
        let exp = parse_exp("1 + 2");
        let result = test_calculate(~exp, mk_missing_step());
        check(bool, "stays MissingStep", true, is_missing_step(result));
      },
    ),
    test_case(
      "calculate: MissingStep with a real step proof promotes to NextStep",
      `Quick,
      () => {
        let exp = parse_exp("1 == 1");
        let result =
          test_calculate(
            ~exp,
            ~proof=axiom_step_proof(exp),
            mk_missing_step(),
          );
        switch (result) {
        | StepperBase.NextStep({step_kind: AxiomStep(_), _}) => ()
        | _ => Alcotest.fail("Expected NextStep(AxiomStep)")
        };
      },
    ),
    test_case(
      "calculate: promoted step is filled in by its own kind calculate",
      `Quick,
      () => {
        /* The promoted kind starts as a placeholder; the kind's calculate
           fills its fields in from the proof on the same pass. */
        let exp = parse_exp("1 == 1");
        let result =
          test_calculate(
            ~exp,
            ~proof=axiom_step_proof(exp),
            mk_missing_step(),
          );
        switch (result) {
        | StepperBase.NextStep({step_kind: AxiomStep(m), _}) =>
          check(
            string,
            "equality read from the proof",
            "refl_eq",
            m.equality,
          );
          check(
            bool,
            "rewritten expression calculated",
            true,
            m.next_exp != Calc.Pending,
          );
        | _ => Alcotest.fail("Expected NextStep(AxiomStep)")
        };
      },
    ),
    test_case(
      "calculate: NextStep with EmptyHole proof collapses to MissingStep",
      `Quick,
      () => {
        // Settled hole-collapse on NextStep when proof head is EmptyHole.
        let exp = parse_exp("1 + 2");
        let step =
          mk_test_step(~step_kind=StepKindHelpers.mk_induction_step(), ());
        let result = test_calculate(~exp, step);
        check(
          bool,
          "EmptyHole proof collapses NextStep to MissingStep",
          true,
          is_missing_step(result),
        );
      },
    ),
    test_case(
      "validity: unknown without a proof map entry",
      `Quick,
      () => {
        /* Validity is read from the step's cached ProofMap entry (see
           Test_ProofMap for the checkmark fixtures), not from the
           expression, so a step with no entry is unknown. */
        let exp = parse_exp("1 == 1");
        let result =
          test_calculate(
            ~exp,
            ~proof=axiom_step_proof(exp),
            mk_missing_step(),
          );
        check(
          bool,
          "no entry, no verdict",
          true,
          StepperBase.Stepper.get_validity(result) == None,
        );
      },
    ),
    // ============================================================
    // Equality tests
    // ============================================================
    test_case(
      "equality: expression with same structure",
      `Quick,
      () => {
        // Test that two expressions with same structure but different IDs
        // are considered equal for validity purposes
        let exp1 = parse_exp("true");
        let exp2 = parse_exp("true");
        // They should be equal according to DHExp.fast_equal
        check(
          bool,
          "structurally equal",
          true,
          DHExp.fast_equal(exp1, exp2),
        );
      },
    ),
    // ============================================================
    // InductionStep tests
    // ============================================================
    test_case(
      "induction step: create with scrutinee",
      `Quick,
      () => {
        // Construction-only: model shape no longer embeds MissingStep in
        // step_kind; nested steppers use next_step.
        let scrut_exp = parse_exp("n");
        let step_kind = StepKindHelpers.mk_induction_step(~exp=scrut_exp, ());
        switch (step_kind) {
        | StepperBase.InductionStep(_) =>
          check(bool, "induction step created", true, true)
        | _ => Alcotest.fail("Expected InductionStep")
        };
      },
    ),
    // ============================================================
    // Wrapping proof forms: assume / revert / generalize
    // ============================================================
    test_case(
      "assume: a proof-side assume promotes to an AssumeStep row",
      `Quick,
      () => {
        /* `assume` used to map to None in kind_of_proof, so the step and
           its whole body proof rendered nothing at all. */
        let src = {|theorem t = forall x -> x == 1 proof assume x == 1 => axiom assume at 0 on x end; axiom refl_eq at 0 on 1 == 1 end in t|};
        switch (promote(~src, ~goal="x == 1", ())) {
        | StepperBase.NextStep({step_kind: AssumeStep(_), _}) => ()
        | _ => Alcotest.fail("expected NextStep(AssumeStep) row")
        };
      },
    ),
    test_case(
      "assume: the body's scope carries the auto-named `assume` hypothesis",
      `Quick,
      () => {
        /* Rows inside an assume cite the hypothesis by its auto-name
           (`axiom assume at ...`), which only works if the nested stepper
           is handed the same scope ProofCheck builds. */
        let src = {|theorem t = forall x -> x == 1 proof assume x == 1 => axiom assume at 0 on x end; axiom refl_eq at 0 on 1 == 1 end in t|};
        switch (promote(~src, ~goal="x == 1", ())) {
        | StepperBase.NextStep({step_kind: AssumeStep(m), _}) =>
          let facts =
            m.inner_ctx
            |> saved_exc(~print="assume inner_ctx")
            |> facts_of_ctx;
          check(
            bool,
            "a fact named `assume` is in the body's scope",
            true,
            List.mem_assoc("assume", facts),
          );
          check(
            bool,
            "and it is the assumed proposition",
            true,
            Exp.fast_equal(
              List.assoc("assume", facts),
              parse_exp("x == 1"),
            ),
          );
        | _ => Alcotest.fail("expected NextStep(AssumeStep) row")
        };
      },
    ),
    test_case(
      "assume: the body proof renders as nested rows",
      `Quick,
      () => {
        /* The row must descend into the body, not stop at the assume. */
        let src = {|theorem t = forall x -> x == 1 proof assume x == 1 => axiom assume at 0 on x end; axiom refl_eq at 0 on 1 == 1 end in t|};
        switch (promote(~src, ~goal="x == 1", ())) {
        | StepperBase.NextStep({step_kind: AssumeStep(m), _}) =>
          switch (m.inner_stepper) {
          | StepperBase.NextStep({step_kind: AxiomStep(_), _}) => ()
          | StepperBase.NextStep(_) =>
            Alcotest.fail("nested row is not the body's axiom step")
          | StepperBase.MissingStep(_, _)
          | StepperBase.Finished =>
            Alcotest.fail("assume body did not render a step row")
          }
        | _ => Alcotest.fail("expected NextStep(AssumeStep) row")
        };
      },
    ),
    test_case(
      "assume: implication intro strips the antecedent from the body's goal",
      `Quick,
      () => {
        /* The body's goal is read off the checker's ProofMap, so the intro
           reading (goal `A ==> B`, assume A, body proves B) is reflected in
           the nested rows without being re-derived here. */
        let src = {|theorem t = forall x -> x == 2 ==> x + 1 == 3 proof assume x == 2 => ? in t|};
        switch (promote(~src, ~goal="x == 2 ==> x + 1 == 3", ())) {
        | StepperBase.NextStep({step_kind: AssumeStep(m), _}) =>
          let inner = m.inner_exp |> saved_exc(~print="assume inner_exp");
          check(
            bool,
            "body works on the consequent",
            true,
            Exp.fast_equal(inner, parse_exp("x + 1 == 3")),
          );
        | _ => Alcotest.fail("expected NextStep(AssumeStep) row")
        };
      },
    ),
    test_case(
      "revert: a proof-side revert promotes to a RevertStep row",
      `Quick,
      () => {
        let src = {|theorem t = 1 == 1 proof revert 1 == 1 => ? in t|};
        switch (promote(~src, ~goal="1 == 1", ())) {
        | StepperBase.NextStep({step_kind: RevertStep(_), _}) => ()
        | _ => Alcotest.fail("expected NextStep(RevertStep) row")
        };
      },
    ),
    test_case(
      "revert: the body's goal is the fact implying the old goal",
      `Quick,
      () => {
        /* `revert F` with goal `G` hands the body `F ==> G`; the fact stays
           in scope (that is what makes the ex-falso idiom work), so the
           nested rows see the enclosing context unchanged. */
        let src = {|theorem t = forall x -> x == 1 ==> x == 1 proof assume x == 1 => revert x == 1 => ? in t|};
        switch (promote(~src, ~goal="x == 1 ==> x == 1", ())) {
        | StepperBase.NextStep({step_kind: AssumeStep(outer), _}) =>
          switch (outer.inner_stepper) {
          | StepperBase.NextStep({step_kind: RevertStep(m), _}) =>
            let inner = m.inner_exp |> saved_exc(~print="revert inner_exp");
            check(
              bool,
              "body works on `F ==> G`",
              true,
              Exp.fast_equal(inner, parse_exp("x == 1 ==> x == 1")),
            );
            let facts =
              m.inner_ctx
              |> saved_exc(~print="revert inner_ctx")
              |> facts_of_ctx;
            check(
              bool,
              "the reverted fact is still citable in the body",
              true,
              List.mem_assoc("assume", facts),
            );
          | _ => Alcotest.fail("expected a nested RevertStep row")
          }
        | _ => Alcotest.fail("expected NextStep(AssumeStep) row")
        };
      },
    ),
    test_case(
      "generalize: a proof-side generalize re-quantifies the body's goal",
      `Quick,
      () => {
        let src = {|theorem t = forall x -> x == x proof generalize x => ? in t|};
        switch (promote(~src, ~goal="x == x", ())) {
        | StepperBase.NextStep({step_kind: GeneralizeStep(m), _}) =>
          let inner = m.inner_exp |> saved_exc(~print="generalize inner_exp");
          switch (inner |> Exp.term_of) {
          | Forall(_, _)
          | ForallWhere(_, _, _) => ()
          | _ =>
            Alcotest.fail(
              "expected a re-quantified body goal, got: " ++ Exp.show(inner),
            )
          };
        | _ => Alcotest.fail("expected NextStep(GeneralizeStep) row")
        };
      },
    ),
    test_case(
      "generalize: facts about the generalized variable leave the body's scope",
      `Quick,
      () => {
        /* Capture soundness: inside the body every fact mentioning x is
           about the OLD x, so the nested rows must not offer it (ProofCheck
           removes them; the row mirrors that removal). */
        let src = {|theorem t = forall x -> x == 1 ==> x == x proof assume x == 1 => generalize x => ? in t|};
        switch (promote(~src, ~goal="x == 1 ==> x == x", ())) {
        | StepperBase.NextStep({step_kind: AssumeStep(outer), _}) =>
          let outer_facts =
            outer.inner_ctx
            |> saved_exc(~print="assume inner_ctx")
            |> facts_of_ctx;
          check(
            bool,
            "the hypothesis is in scope before the generalize",
            true,
            List.mem_assoc("assume", outer_facts),
          );
          switch (outer.inner_stepper) {
          | StepperBase.NextStep({step_kind: GeneralizeStep(m), _}) =>
            let facts =
              m.inner_ctx
              |> saved_exc(~print="generalize inner_ctx")
              |> facts_of_ctx;
            check(
              bool,
              "and it is gone inside the generalize",
              false,
              List.mem_assoc("assume", facts),
            );
          | _ => Alcotest.fail("expected a nested GeneralizeStep row")
          };
        | _ => Alcotest.fail("expected NextStep(AssumeStep) row")
        };
      },
    ),
    test_case(
      "generalize: generalized_ctx removes facts mentioning the variable",
      `Quick,
      () => {
        let (ctx, _) =
          SemanticCtx.add_hypothesis(
            SemanticCtx.of_ctx_and_env(ctx_with_x, Builtins.env_init),
            "assume",
            parse_exp("x == 1"),
          );
        check(
          bool,
          "fact present before",
          true,
          List.mem_assoc("assume", facts_of_ctx(ctx)),
        );
        let ctx' = Web.GeneralizeStep.generalized_ctx(ctx, parse_exp("x"));
        check(
          bool,
          "fact removed after",
          false,
          List.mem_assoc("assume", facts_of_ctx(ctx')),
        );
      },
    ),
    // ============================================================
    // `have`: nested subproof stepper + body continuation
    // ============================================================
    /* `have <exp> proof <sub> => <body>` is the one wrapping form with TWO
       proof children, and they play different roles: the subproof proves
       `<exp>` (its own goal, target literal `true`), the body continues
       the OUTER goal with `<exp>` installed as the auto-named `have`
       hypothesis. `Have` used to map to None in kind_of_proof, so NEITHER
       child rendered — including the subproof hole the (!) panel's "Prove
       here" writes, which is exactly the row the user needs. */
    test_case(
      "have: a proof-side have promotes to a HaveStep row",
      `Quick,
      () => {
        let src = {|theorem t = forall x -> x == 1 proof have x == 1 proof ? => ? in t|};
        switch (promote(~src, ~goal="x == 1", ())) {
        | StepperBase.NextStep({step_kind: HaveStep(_), _}) => ()
        | _ => Alcotest.fail("expected NextStep(HaveStep) row")
        };
      },
    ),
    test_case(
      "have: the subproof's nested stepper is rooted at the proposition",
      `Quick,
      () => {
        /* The subproof's goal comes from the CHECKER (ProofMap incoming of
           the subproof node), never re-derived: ProofCheck's Have arm
           checks `sub` against the proposition in the enclosing scope. */
        let src = {|theorem t = forall x -> x == 1 proof have x == 1 proof ? => ? in t|};
        switch (promote(~src, ~goal="x == 1", ())) {
        | StepperBase.NextStep({step_kind: HaveStep(m), _}) =>
          let sub = m.sub_exp |> saved_exc(~print="have sub_exp");
          check(
            bool,
            "subproof works on the proposition",
            true,
            Exp.fast_equal(sub, parse_exp("x == 1")),
          );
        | _ => Alcotest.fail("expected NextStep(HaveStep) row")
        };
      },
    ),
    test_case(
      "have: a hole subproof is a step-picker row rooted at the proposition",
      `Quick,
      () => {
        /* This is the "Prove here" shape (ObligationsPanel.have_patch:
           `Have(goal, ?, region)`). The subproof hole must be a FUNCTIONAL
           picker row — whose expression is what its overlay writes into
           the proof text — and it must be rooted at the proposition, not
           at the outer goal. */
        let src = {|theorem t = forall x -> x == 1 proof have 1 == 1 proof ? => ? in t|};
        switch (promote(~src, ~goal="x == 1", ())) {
        | StepperBase.NextStep({step_kind: HaveStep(m), _}) =>
          switch (m.sub_stepper) {
          | StepperBase.MissingStep(mm, _) =>
            check(
              string,
              "picker row's goal is the proposition",
              "1 == 1",
              mm.full_exp
              |> saved_exc(~print="sub picker full_exp")
              |> Test_ProofMap.print_exp,
            )
          | StepperBase.NextStep(_)
          | StepperBase.Finished =>
            Alcotest.fail("subproof hole did not render a picker row")
          }
        | _ => Alcotest.fail("expected NextStep(HaveStep) row")
        };
      },
    ),
    test_case(
      "have: a completed subproof renders its rows",
      `Quick,
      () => {
        /* The row must descend into the subproof, not stop at the have. */
        let src = {|theorem t = forall x -> x == 1 proof have 1 == 1 proof axiom refl_eq at 0 on 1 == 1 end => ? in t|};
        switch (promote(~src, ~goal="x == 1", ())) {
        | StepperBase.NextStep({step_kind: HaveStep(m), _}) =>
          switch (m.sub_stepper) {
          | StepperBase.NextStep({step_kind: AxiomStep(_), _}) => ()
          | StepperBase.NextStep(_) =>
            Alcotest.fail("nested row is not the subproof's axiom step")
          | StepperBase.MissingStep(_, _)
          | StepperBase.Finished =>
            Alcotest.fail("have subproof did not render a step row")
          };
          /* And it reached `true`, which is what drops the have's own
             obligation (ProofCheck's `sub_proves`) and what the nested
             box's target row is compared against. */
          check(
            bool,
            "subproof's reached expression is literal true",
            true,
            Exp.fast_equal(
              m.sub_last_exp |> saved_exc(~print="have sub_last_exp"),
              Exp.fresh(Atom(Bool(true))),
            ),
          );
        | _ => Alcotest.fail("expected NextStep(HaveStep) row")
        };
      },
    ),
    test_case(
      "have: the body chain continues with the OUTER goal",
      `Quick,
      () => {
        /* `have` is a pass-through (its outgoing is the body's), so the
           body's incoming is the outer goal unchanged — read off the
           checker, not re-derived. */
        let src = {|theorem t = forall x -> x == 1 proof have 1 == 1 proof ? => ? in t|};
        switch (promote(~src, ~goal="x == 1", ())) {
        | StepperBase.NextStep({step_kind: HaveStep(m), _}) =>
          let body = m.body_exp |> saved_exc(~print="have body_exp");
          check(
            bool,
            "body works on the outer goal",
            true,
            Exp.fast_equal(body, parse_exp("x == 1")),
          );
        | _ => Alcotest.fail("expected NextStep(HaveStep) row")
        };
      },
    ),
    test_case(
      "have: the body's hole is a picker row rooted at the outer goal",
      `Quick,
      () => {
        let src = {|theorem t = forall x -> x == 1 proof have 1 == 1 proof ? => ? in t|};
        switch (promote(~src, ~goal="x == 1", ())) {
        | StepperBase.NextStep({step_kind: HaveStep(m), _}) =>
          switch (m.body_stepper) {
          | StepperBase.MissingStep(mm, _) =>
            check(
              string,
              "picker row's goal is the outer goal",
              "x == 1",
              mm.full_exp
              |> saved_exc(~print="body picker full_exp")
              |> Test_ProofMap.print_exp,
            )
          | StepperBase.NextStep(_)
          | StepperBase.Finished =>
            Alcotest.fail("body hole did not render a picker row")
          }
        | _ => Alcotest.fail("expected NextStep(HaveStep) row")
        };
      },
    ),
    test_case(
      "have: the body's scope carries the auto-named `have` hypothesis",
      `Quick,
      () => {
        /* Installed UNCONDITIONALLY (ProofCheck's Have arm), even with the
           subproof still a hole — that is what lets a body obligation
           discharge against it the moment the wrapper is written. */
        let src = {|theorem t = forall x -> x == 1 proof have 1 == 1 proof ? => ? in t|};
        switch (promote(~src, ~goal="x == 1", ())) {
        | StepperBase.NextStep({step_kind: HaveStep(m), _}) =>
          let facts =
            m.body_ctx |> saved_exc(~print="have body_ctx") |> facts_of_ctx;
          check(
            bool,
            "a fact named `have` is in the body's scope",
            true,
            List.mem_assoc("have", facts),
          );
          check(
            bool,
            "and it is the have's proposition",
            true,
            Exp.fast_equal(List.assoc("have", facts), parse_exp("1 == 1")),
          );
        | _ => Alcotest.fail("expected NextStep(HaveStep) row")
        };
      },
    ),
    test_case(
      "have: the subproof is checked in the ENCLOSING scope",
      `Quick,
      () => {
        /* The subproof may not cite the have's own hypothesis, so its
           nested rows must NOT see it — nesting `have` inside `have` is the
           readable way to pin that. */
        let src = {|theorem t = forall x -> x == 1 proof have 1 == 1 proof have 2 == 2 proof ? => ? => ? in t|};
        switch (promote(~src, ~goal="x == 1", ())) {
        | StepperBase.NextStep({step_kind: HaveStep(outer), _}) =>
          switch (outer.sub_stepper) {
          | StepperBase.NextStep({step_kind: HaveStep(inner), _}) =>
            let facts =
              inner.body_ctx
              |> saved_exc(~print="inner have body_ctx")
              |> facts_of_ctx;
            /* The inner have's body sees its OWN hypothesis... */
            check(
              bool,
              "inner have's own hypothesis is in scope",
              true,
              List.exists(
                ((n, e)) =>
                  n == "have" && Exp.fast_equal(e, parse_exp("2 == 2")),
                facts,
              ),
            );
            /* ...and not the outer one, since the outer subproof runs in
               the scope BEFORE the outer hypothesis was added. */
            check(
              bool,
              "outer have's hypothesis is not in the subproof's scope",
              false,
              List.exists(
                ((n, e)) =>
                  n == "have" && Exp.fast_equal(e, parse_exp("1 == 1")),
                facts,
              ),
            );
          | _ =>
            Alcotest.fail("expected a nested HaveStep row in the subproof")
          }
        | _ => Alcotest.fail("expected NextStep(HaveStep) row")
        };
      },
    ),
    test_case(
      "have: the (!) panel's Prove-here shape renders both chains",
      `Quick,
      () => {
        /* The user-facing repro, at model level: `f` carries a domain
           restriction, the theorem quantifies `y` without it, so `f(y)`
           incurs a PENDING `y != 0` obligation and the (!) panel offers
           "Prove here" — which writes
           `Have(y != 0, ?, <the existing region>)`
           (ObligationsPanel.have_patch). Before HaveStep that produced no
           stepper at all for the new subproof; here both chains must show
           up, each rooted at its own goal. */
        let src = "let f = fun x where x != 0 -> 100 / x in theorem t = forall y -> f(y) == f(y) proof have y != 0 proof ? => ? in t";
        let (proof, pm) = checked_proof(src);
        /* Seeded with the env-INLINED statement, as Theorems.re does. */
        let result =
          test_calculate(
            ~exp=
              parse_exp(
                "(fun x where x != 0 -> 100 / x)(y) == (fun x where x != 0 -> 100 / x)(y)",
              ),
            ~ctx=ctx_with_x,
            ~proof=Calc.NewValue(proof),
            ~proof_map=Calc.NewValue(pm),
            mk_missing_step(),
          );
        switch (result) {
        | StepperBase.NextStep({step_kind: HaveStep(m), _}) =>
          check(
            string,
            "the subproof chain is rooted at the obligation's goal",
            "y != 0",
            m.sub_exp
            |> saved_exc(~print="have sub_exp")
            |> Test_ProofMap.print_exp,
          );
          check(
            string,
            "the body chain continues on the written outer goal",
            "f(y) == f(y)",
            m.body_exp
            |> saved_exc(~print="have body_exp")
            |> Test_ProofMap.print_exp,
          );
          /* Both holes are pickable rows, so the user can actually work
             in either position. */
          check(
            bool,
            "subproof hole is a picker row",
            true,
            is_missing_step(m.sub_stepper),
          );
          check(
            bool,
            "body hole is a picker row",
            true,
            is_missing_step(m.body_stepper),
          );
        | _ => Alcotest.fail("expected NextStep(HaveStep) row")
        };
      },
    ),
    test_case(
      "have: an old-input recalculate keeps both children's saved fields",
      `Quick,
      () => {
        /* Both nested steppers are threaded through `Calc.saved` fields
           like every other step's; a pass whose inputs are all old must
           neither drop them back to Pending nor rebuild the nested rows'
           editors (which carry caret/selection). */
        let src = {|theorem t = forall x -> x == 1 proof have 1 == 1 proof ? => ? in t|};
        let (proof, pm) = checked_proof(src);
        let goal = parse_exp("x == 1");
        let calculated =
          test_calculate(
            ~exp=goal,
            ~ctx=ctx_with_x,
            ~proof=Calc.NewValue(proof),
            ~proof_map=Calc.NewValue(pm),
            mk_missing_step(),
          );
        let recalc =
          StepperBase.Stepper.calculate(
            ~settings=Calc.OldValue(CoreSettings.on),
            ~exp=Calc.OldValue(goal),
            ~ctx=
              Calc.OldValue(
                SemanticCtx.of_ctx_and_env(ctx_with_x, Builtins.env_init),
              ),
            ~ana=Calc.OldValue(IdTagged.FreshGrammar.Typ.int()),
            ~proof=Calc.OldValue(proof),
            ~proof_map=Calc.OldValue(pm),
            calculated,
          );
        let have_of = (ns: StepperBase.next_step) =>
          switch (ns) {
          | StepperBase.NextStep({step_kind: HaveStep(m), _}) => m
          | _ => Alcotest.fail("expected NextStep(HaveStep) row")
          };
        let (before, after) = (have_of(calculated), have_of(recalc));
        check(
          bool,
          "subproof goal is stable",
          true,
          Exp.fast_equal(
            before.sub_exp |> saved_exc(~print="before sub_exp"),
            after.sub_exp |> saved_exc(~print="after sub_exp"),
          ),
        );
        check(
          bool,
          "body goal is stable",
          true,
          Exp.fast_equal(
            before.body_exp |> saved_exc(~print="before body_exp"),
            after.body_exp |> saved_exc(~print="after body_exp"),
          ),
        );
        /* The nested picker rows' editors must be the SAME objects. */
        let editor_of = (~print: string, ns: StepperBase.next_step) =>
          switch (ns) {
          | StepperBase.MissingStep(mm, _) => mm.editor |> saved_exc(~print)
          | _ => Alcotest.fail(print ++ ": expected a picker row")
          };
        check(
          bool,
          "subproof picker editor survives an old-input recalculate",
          true,
          editor_of(~print="before sub editor", before.sub_stepper)
          === editor_of(~print="after sub editor", after.sub_stepper),
        );
        check(
          bool,
          "body picker editor survives an old-input recalculate",
          true,
          editor_of(~print="before body editor", before.body_stepper)
          === editor_of(~print="after body editor", after.body_stepper),
        );
      },
    ),
    // ============================================================
    // Insertion: the step-picker's ProofPatch round-trip
    // ============================================================
    /* The step-picker inserts a no-search wrapping form IMMEDIATELY, with
       an empty argument, and then puts the caret in that argument
       (docs/prover-obligations.md §3.4). These tests pin the two halves of
       that contract at model level: what gets written, and that the hole
       just written is a reachable caret target. */
    test_case(
      "insertion: Assume with no argument writes `assume ? =>`",
      `Quick,
      () => {
        let out =
          insert_proof_term(
            ~src="theorem t = forall x -> x == 2 ==> x == 2 proof ? in t",
            StepperBase.Stepper.assume_term(~exp=EmptyHole |> Exp.fresh),
          );
        check_contains(
          ~msg="assume step landed with a hole",
          out,
          "assume ?",
        );
        check_contains(~msg="the arrow is written too", out, "=>");
        check_contains(
          ~msg="theorem statement survives",
          out,
          "forall x -> x == 2 ==> x == 2",
        );
      },
    ),
    test_case(
      "insertion: Generalize with no argument writes `generalize ? =>`",
      `Quick,
      () => {
        let out =
          insert_proof_term(
            ~src="theorem t = forall x -> x == x proof ? in t",
            StepperBase.Stepper.generalize_term(~exp=EmptyHole |> Exp.fresh),
          );
        check_contains(
          ~msg="generalize step landed with a hole",
          out,
          "generalize ?",
        );
      },
    ),
    test_case(
      "insertion: an empty-argument form reparses with a hole argument",
      `Quick,
      () => {
        /* The inserted text must parse back to `Assume(EmptyHole, EmptyHole)`
           — an argument hole (which the user is about to type into) AND a
           body hole (which becomes the next step-picker row). If the
           argument came back as anything else, the row the user is dropped
           into is not an empty editable slot. */
        let out =
          insert_proof_term(
            ~src="theorem t = forall x -> x == 2 ==> x == 2 proof ? in t",
            StepperBase.Stepper.assume_term(~exp=EmptyHole |> Exp.fresh),
          );
        switch (Test_ProofMap.find_theorem_proof(parse_exp(out))) {
        | Some({
            term: Assume({term: EmptyHole, _}, None, {term: EmptyHole, _}),
            _,
          }) =>
          check(bool, "argument and body are both holes", true, true)
        | Some(p) =>
          Alcotest.fail("reparsed as something else: " ++ Proof.show(p))
        | None => Alcotest.fail("no theorem proof after insertion")
        };
      },
    ),
    test_case(
      "insertion: the new form's argument hole is a reachable caret target",
      `Quick,
      () => {
      /* Focus mechanics: after inserting, the view jumps the MAIN editor's
         caret to `form_arg_id(term)` so the argument's SubEditor splice
         accepts edits (SubEditor.confine_pre drops edits whose caret is
         outside the splice). That id survives into the patched segment
         only because the serializer stamps term ids onto the pieces it
         emits — an EmptyHole argument arrives as a Grout carrying it. If
         this jump cannot resolve, the inserted slot looks and behaves
         dead, so pin it. */
      List.iter(
        ((label, term)) => {
          let arg_id =
            switch (StepperBase.Stepper.form_arg_id(term)) {
            | Some(id) => id
            | None => Alcotest.fail(label ++ ": form_arg_id returned None")
            };
          let z =
            insert_proof_zipper(
              ~src="theorem t = forall x -> x == 2 ==> x == 2 proof ? in t",
              term,
            );
          check(
            bool,
            label ++ ": caret can jump to the argument hole",
            true,
            Haz3lcore.Move.jump_to_id_indicated(z, arg_id) != None,
          );
        },
        [
          (
            "assume",
            StepperBase.Stepper.assume_term(~exp=EmptyHole |> Exp.fresh),
          ),
          (
            "generalize",
            StepperBase.Stepper.generalize_term(~exp=EmptyHole |> Exp.fresh),
          ),
          /* Revert is picked, not typed, but it is focused the same way
             so the user can adjust the pick. */
          (
            "revert (picked)",
            StepperBase.Stepper.revert_term(~exp=parse_exp("x == 2")),
          ),
        ],
      )
    }),
    test_case(
      "insertion: form_arg_id ignores steps that have no argument", `Quick, () =>
      check(
        bool,
        "a hole step has no wrapping-form argument",
        true,
        StepperBase.Stepper.form_arg_id(Proof.fresh(EmptyHole).term) == None,
      )
    ),
    test_case(
      "insertion: a picked Assume argument writes `assume <e> => ?`",
      `Quick,
      () => {
        let out =
          insert_proof_term(
            ~src="theorem t = forall x -> x == 2 ==> x == 2 proof ? in t",
            StepperBase.Stepper.assume_term(~exp=parse_exp("x == 2")),
          );
        check_contains(~msg="assume step landed", out, "assume x == 2 =>");
        check_contains(
          ~msg="theorem statement survives",
          out,
          "forall x -> x == 2 ==> x == 2",
        );
      },
    ),
    test_case(
      "insertion: a picked Revert fact writes `revert <e> => ?`",
      `Quick,
      () => {
        let out =
          insert_proof_term(
            ~src="theorem t = forall x -> x == 1 proof ? in t",
            StepperBase.Stepper.revert_term(~exp=parse_exp("x == 1")),
          );
        check_contains(~msg="revert step landed", out, "revert x == 1 =>");
      },
    ),
    test_case(
      "insertion: a picked Generalize argument writes `generalize <e> => ?`",
      `Quick,
      () => {
        let out =
          insert_proof_term(
            ~src="theorem t = forall x -> x == x proof ? in t",
            StepperBase.Stepper.generalize_term(~exp=parse_exp("x")),
          );
        check_contains(~msg="generalize step landed", out, "generalize x =>");
      },
    ),
    test_case(
      "insertion: Have with no argument writes `have ? proof ? =>`",
      `Quick,
      () => {
        let out =
          insert_proof_term(
            ~src="theorem t = forall x -> x == 1 proof ? in t",
            StepperBase.Stepper.have_term(~exp=EmptyHole |> Exp.fresh),
          );
        check_contains(
          ~msg="have step landed with a hole proposition",
          out,
          "have ?",
        );
        check_contains(~msg="and an open subproof", out, "proof ?");
        check_contains(~msg="the arrow is written too", out, "=>");
      },
    ),
    test_case(
      "insertion: an empty-argument have reparses with three holes",
      `Quick,
      () => {
        /* `have`'s wrapper carries a SECOND hole (the subproof), so the
           inserted text must parse back to
           `Have(EmptyHole, EmptyHole, EmptyHole)`: the proposition the
           user is about to type into, the subproof that becomes its own
           picker row inside the nested box, and the body that becomes the
           next row of the outer chain. */
        let out =
          insert_proof_term(
            ~src="theorem t = forall x -> x == 1 proof ? in t",
            StepperBase.Stepper.have_term(~exp=EmptyHole |> Exp.fresh),
          );
        switch (Test_ProofMap.find_theorem_proof(parse_exp(out))) {
        | Some({
            term:
              Have(
                {term: EmptyHole, _},
                {term: EmptyHole, _},
                {term: EmptyHole, _},
              ),
            _,
          }) =>
          check(
            bool,
            "proposition, subproof and body are all holes",
            true,
            true,
          )
        | Some(p) =>
          Alcotest.fail("reparsed as something else: " ++ Proof.show(p))
        | None => Alcotest.fail("no theorem proof after insertion")
        };
      },
    ),
    test_case(
      "insertion: a picked Have proposition writes `have <e> proof ? => ?`",
      `Quick,
      () => {
        let out =
          insert_proof_term(
            ~src="theorem t = forall x -> x == 1 proof ? in t",
            StepperBase.Stepper.have_term(~exp=parse_exp("1 == 1")),
          );
        check_contains(~msg="have step landed", out, "have 1 == 1 proof ?");
      },
    ),
    test_case(
      "insertion: a new have's proposition hole is a reachable caret target",
      `Quick,
      () => {
        /* `have`'s proposition is child 0 like the other wrapping forms'
           (Form.re: `mk_pre_c(L, ["have", "proof", "=>"], ..., [Exp,
           Proof])`), so `form_arg_id` and the caret jump apply unchanged
           — which is what makes the inserted slot typable. */
        let term = StepperBase.Stepper.have_term(~exp=EmptyHole |> Exp.fresh);
        let arg_id =
          switch (StepperBase.Stepper.form_arg_id(term)) {
          | Some(id) => id
          | None => Alcotest.fail("have: form_arg_id returned None")
          };
        let z =
          insert_proof_zipper(
            ~src="theorem t = forall x -> x == 1 proof ? in t",
            term,
          );
        check(
          bool,
          "caret can jump to the proposition hole",
          true,
          Haz3lcore.Move.jump_to_id_indicated(z, arg_id) != None,
        );
      },
    ),
    test_case(
      "insertion: an inserted wrapping form reparses as that form",
      `Quick,
      () => {
        /* The patch reflows through ExpToSegment, so the written text must
           parse back to the same proof term — otherwise the row that
           `calculate` synthesizes next pass is not the one just inserted. */
        let out =
          insert_proof_term(
            ~src="theorem t = forall x -> x == 2 ==> x == 2 proof ? in t",
            StepperBase.Stepper.assume_term(~exp=parse_exp("x == 2")),
          );
        switch (Test_ProofMap.find_theorem_proof(parse_exp(out))) {
        | Some({term: Assume(e, None, {term: EmptyHole, _}), _}) =>
          check(
            bool,
            "the assumed expression round-trips",
            true,
            Exp.fast_equal(e, parse_exp("x == 2")),
          )
        | Some(p) =>
          Alcotest.fail("reparsed as something else: " ++ Proof.show(p))
        | None => Alcotest.fail("no theorem proof after insertion")
        };
      },
    ),
    /* A missing-step row's expression is what its overlay WRITES into
       the proof text (`axiom ... on <exp> end`). It must be the goal as
       WRITTEN, not the env-inlined one: the theorem statement the app
       seeds the first row with is the evaluator's record, and
       `Transition.re`'s Theorem rule runs `Substitution.in_exp(env, e)`
       before recording it, so a goal mentioning a let-bound `f` arrives
       with f's whole lambda spliced in. The checker's ProofMap keeps the
       written form; the row must prefer it. */
    test_case(
      "missing-step row takes its goal from the checker, not the inlined statement",
      `Quick,
      () => {
        let src = "let f = fun x where x != 0 -> 100 / x in theorem t = forall y where y != 0 -> f(y) == f(y) proof ? in t";
        let (proof, pm) = checked_proof(src);
        /* What Theorems.re seeds the first row with today: the recorded
           (env-inlined) statement, peeled. */
        let inlined_goal =
          parse_exp(
            "(fun x where x != 0 -> 100 / x)(y) == (fun x where x != 0 -> 100 / x)(y)",
          );
        let result =
          test_calculate(
            ~exp=inlined_goal,
            ~ctx=ctx_with_x,
            ~proof=Calc.NewValue(proof),
            ~proof_map=Calc.NewValue(pm),
            mk_missing_step(),
          );
        switch (result) {
        | StepperBase.MissingStep(m, _) =>
          let printed =
            m.full_exp
            |> saved_exc(~print="full_exp")
            |> Test_ProofMap.print_exp;
          check(
            string,
            "row goal is the written form",
            "f(y) == f(y)",
            printed,
          );
        | _ => Alcotest.fail("expected a MissingStep row")
        };
      },
    ),
    /* Note: induction exhaustiveness is no longer recomputed in the stepper;
       the label now reflects the static `InexhaustiveMatch` mark produced by
       the theorem's statics (threaded in via `~proof_info_map`). The empty /
       inexhaustive cases are covered by `Test_Statics_Proof`, so the former
       stepper-level "exhaustiveness check empty" and "validity with no cases"
       unit tests (which relied on the removed local Coverage.check and an
       absent proof context) have been removed. */
    /* The former "induction step: calculate result expression" and
       "induction step: complete proof xs == xs" tests fed a
       NextStep(InductionStep(...)) through Stepper.calculate with a cell
       EmptyHole proof, which now collapses to a MissingStep — a step kind
       only survives while the proof calls for it. Kind calculate is covered
       above by the promoted axiom step; per-case induction results are
       covered by Test_ProofMap. */
  ],
);
