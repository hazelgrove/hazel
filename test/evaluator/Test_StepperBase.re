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
  current_editor: Calc.Pending,
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
  StepperBase.MissingStep(Web.MissingStep.Model.init);

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
      is_exhaustive: Calc.Pending,
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
      }),
    ),
  );

let tests = (
  "StepperBase",
  [
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
