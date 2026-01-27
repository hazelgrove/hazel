open Alcotest;
open Language;
open Test_Evaluator_Prelude;
open Util;
open Haz3lcore;

// Import StepperBase types from web module
module StepperBase = Web.StepperBase;

// Helper constructors for step_kind_model
module StepKindHelpers = {
  // Create a SingleStep that will automatically take the next available step
  let mk_auto_single_step = (exp: Exp.t): StepperBase.step_kind_model => {
    let status =
      EvaluatorStep.get_status(
        ~settings=CoreSettings.on,
        exp,
        Environment.empty,
      );
    switch (status) {
    | AutoStep(step)
    | AvailableSteps([step, ..._]) =>
      StepperBase.SingleStep({
        persistent_evalobj: EvaluatorStep.persist(step),
        evalobj: Calc.Pending,
        next_exp: Calc.Pending,
      })
    | AvailableSteps([]) => failwith("No step available for expression")
    };
  };

  // Create an AxiomStep at a specific subexpression
  let mk_axiom_step =
      (
        ~name="reflexivity",
        ~at_idx=0,
        ~direction=Direction.Right,
        ~equality="Reflexive(==)",
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

  // Create a MissingStep (for testing validation)
  let mk_missing_step = (): StepperBase.step_kind_model =>
    StepperBase.MissingStep(Web.MissingStep.Model.init);

  // Create an InductionStep with a scrutinee expression
  let mk_induction_step =
      (~exp: option(Exp.t)=?, ()): StepperBase.step_kind_model =>
    StepperBase.InductionStep(Web.InductionStep.init(~exp?, ()));

  // Create a ForallStep
  let mk_forall_step = (): StepperBase.step_kind_model =>
    StepperBase.ForallStep(Web.ForallStep.init(StepperBase.Stepper.init));
};

// Helper to create a minimal step model for testing
let mk_test_step =
    (~step_kind: StepperBase.step_kind_model, ~next_step=None, ())
    : StepperBase.step_model => {
  expr: Calc.Pending,
  editor: Calc.Pending,
  step_kind,
  next_step,
  hidden: Calc.Pending,
  proof_validity: Calc.Pending,
  editor_info_map: Calc.Pending,
};

// Helper constructors for InductionCase
module InductionCaseHelpers = {
  let mk_pattern_editor = (pattern_str: string): Web.CodeWithStatics.Model.t => {
    let pattern_exp = parse_exp(pattern_str);
    let editor =
      Editor.Model.mk(
        Zipper.unzip(
          ExpToSegment.exp_to_segment(
            ~settings=ExpToSegment.Settings.editable(~inline=true),
            pattern_exp,
          ),
        ),
      );
    Web.CodeWithStatics.Model.mk(editor);
  };

  // Create an InductionCase from a pattern string
  let mk_case =
      (
        ~pattern_str: string,
        ~inner_exp: option(Exp.t)=?,
        ~last_exp: option(Exp.t)=?,
        ~step: option(StepperBase.step_model)=?,
        (),
      )
      : Web.InductionCase.model'(StepperBase.step_model) => {
    {
      pattern: mk_pattern_editor(pattern_str),
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
      constraint_: Calc.Pending,
    };
  };

  // Create an axiom step for reflexivity
  let mk_reflexivity_step = (goal_exp: Exp.t): StepperBase.step_model => {
    let axiom_kind =
      StepKindHelpers.mk_axiom_step(
        ~name="Reflexive(==)",
        ~equality="Reflexive(==)",
        goal_exp,
      );
    mk_test_step(~step_kind=axiom_kind, ());
  };

  // Create an axiom step that rewrites using a hypothesis
  let mk_rewrite_step =
      (~hypothesis_name: string, goal_exp: Exp.t): StepperBase.step_model => {
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
          : option(StepperBase.step_model) =>
    switch (step_kinds) {
    | [] => None
    | [kind] => Some(mk_test_step(~step_kind=kind, ()))
    | [kind, ...rest] =>
      Some(
        mk_test_step(~step_kind=kind, ~next_step=build_step_chain(rest), ()),
      )
    };

  // Create an induction case from a pattern, goal, and list of step_kinds
  let mk_case_with_steps =
      (
        ~pattern_str: string,
        ~goal_str: string,
        step_kinds: list(StepperBase.step_kind_model),
      )
      : Web.InductionCase.model'(StepperBase.step_model) => {
    let goal_exp = parse_exp(goal_str);
    let step =
      switch (build_step_chain(step_kinds)) {
      | Some(s) => s
      | None => StepperBase.init_step
      };
    mk_case(~pattern_str, ~inner_exp=goal_exp, ~step, ());
  };

  // Create an InductionStep with scrutinee and cases
  let mk_induction_with_cases =
      (
        ~scrut_str: string,
        ~cases: list(Web.InductionCase.model'(StepperBase.step_model)),
      )
      : StepperBase.step_kind_model => {
    // Create a complete InductionStep model directly by matching the structure
    // Since we can't easily access the base fields, we create everything from scratch
    let scrut_editor = mk_pattern_editor(scrut_str);

    let ind_model: Web.InductionStep.model'(StepperBase.step_model) = {
      scrut: scrut_editor,
      cases,
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

// Helper to run calculate with minimal dependencies
let test_calculate =
    (
      ~exp: Exp.t,
      ~ctx=Builtins.ctx_init(Some(Int)),
      ~ana=IdTagged.FreshGrammar.Typ.int(), // Use a fresh type for ana
      step: StepperBase.step_model,
    )
    : (StepperBase.step_model, Calc.t(Exp.t), Calc.t(option(bool))) => {
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
    step,
  );
};

// Extract the final expression from a calculated step
let get_final_exp = ((_, last_exp, _validity)) => {
  Calc.get_value(last_exp);
};

// Extract validity from a calculated step
let get_validity = ((_, _, validity)) => {
  Calc.get_value(validity);
};

// Run a single step and return the result expression
let run_single_step = (~exp, ~step_kind) => {
  let step = mk_test_step(~step_kind, ());
  let (_, final_exp, _) = test_calculate(~exp, step);
  Calc.get_value(final_exp);
};

// Run a step chain and return the final result
let run_step_chain =
    (
      ~initial_exp,
      ~step_kinds: list(StepperBase.step_kind_model),
      ~ctx=?,
      (),
    ) => {
  switch (InductionCaseHelpers.build_step_chain(step_kinds)) {
  | Some(root_step) =>
    let (model, final_exp, validity) =
      test_calculate(~exp=initial_exp, ~ctx?, root_step);
    (model, Calc.get_value(final_exp), Calc.get_value(validity));
  | None => failwith("Empty step chain")
  };
};

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
        let exp = parse_exp("1 + 2");
        let step =
          mk_test_step(~step_kind=StepKindHelpers.mk_missing_step(), ());
        let (_, final_exp, _) = test_calculate(~exp, step);
        check(
          dhexp_typ,
          "expression unchanged",
          exp,
          Calc.get_value(final_exp),
        );
      },
    ),
    // ============================================================
    // SingleStep tests
    // ============================================================
    test_case(
      "single step: simple arithmetic",
      `Quick,
      () => {
        let exp = parse_exp("1 + 2");
        let result =
          run_single_step(
            ~exp,
            ~step_kind=StepKindHelpers.mk_auto_single_step(exp),
          );
        check(dhexp_typ, "1 + 2 -> 3", parse_exp("3"), result);
      },
    ),
    test_case(
      "single step: beta reduction",
      `Quick,
      () => {
        let exp = parse_exp("(fun x -> x + 1)(5)");
        let elab = elaborate(exp);
        let result =
          run_single_step(
            ~exp=elab,
            ~step_kind=StepKindHelpers.mk_auto_single_step(elab),
          );
        // After beta reduction, should have 5 + 1
        let expected_pattern = parse_exp("5 + 1");
        check(
          dhexp_typ,
          "(fun x -> x + 1)(5) reduces",
          expected_pattern,
          result,
        );
      },
    ),
    test_case(
      "single step: let binding",
      `Quick,
      () => {
        let exp = parse_exp("let x = 5 in x + 1");
        let elab = elaborate(exp);
        let result =
          run_single_step(
            ~exp=elab,
            ~step_kind=StepKindHelpers.mk_auto_single_step(elab),
          );
        // After one step, should substitute x
        let expected = parse_exp("5 + 1");
        check(dhexp_typ, "let x = 5 in x + 1 -> 5 + 1", expected, result);
      },
    ),
    // ============================================================
    // Validity tests
    // ============================================================
    test_case(
      "validity: true expression",
      `Quick,
      () => {
        let exp = parse_exp("true");
        let step =
          mk_test_step(~step_kind=StepKindHelpers.mk_missing_step(), ());
        let (_, _, validity) = test_calculate(~exp, step);
        check(
          option(bool),
          "true is valid",
          Some(true),
          Calc.get_value(validity),
        );
      },
    ),
    test_case(
      "validity: false expression",
      `Quick,
      () => {
        let exp = parse_exp("false");
        let step =
          mk_test_step(~step_kind=StepKindHelpers.mk_missing_step(), ());
        let (_, _, validity) = test_calculate(~exp, step);
        check(
          option(bool),
          "false is refutable",
          Some(false),
          Calc.get_value(validity),
        );
      },
    ),
    test_case(
      "validity: unknown expression",
      `Quick,
      () => {
        let exp = parse_exp("42");
        let step =
          mk_test_step(~step_kind=StepKindHelpers.mk_missing_step(), ());
        let (_, _, validity) = test_calculate(~exp, step);
        check(
          option(bool),
          "42 is not a boolean",
          None,
          Calc.get_value(validity),
        );
      },
    ),
    test_case(
      "validity: single step to true",
      `Quick,
      () => {
        let exp = parse_exp("1 == 1");
        let elab = elaborate(exp);
        let step_kind = StepKindHelpers.mk_auto_single_step(elab);
        let step = mk_test_step(~step_kind, ());
        let (_, final_exp, validity) = test_calculate(~exp=elab, step);
        // After evaluation, 1 == 1 should become true
        check(
          dhexp_typ,
          "1 == 1 evaluates to true",
          parse_exp("true"),
          Calc.get_value(final_exp),
        );
        // But validity on a SingleStep checks the *next* expression
        // So we need to check the model's proof_validity field
        check(
          option(bool),
          "validity is Some(true)",
          Some(true),
          Calc.get_value(validity),
        );
      },
    ),
    // ============================================================
    // Step Chain tests
    // ============================================================
    test_case(
      "step chain: two arithmetic steps",
      `Quick,
      () => {
        let exp = parse_exp("1 + 2 + 3");
        let elab = elaborate(exp);
        // First step: 1 + 2 -> 3
        // Second step: 3 + 3 -> 6
        let step1 = StepKindHelpers.mk_auto_single_step(elab);
        let after_first = run_single_step(~exp=elab, ~step_kind=step1);
        let step2 = StepKindHelpers.mk_auto_single_step(after_first);

        let (_, final_exp, _) =
          run_step_chain(~initial_exp=elab, ~step_kinds=[step1, step2], ());
        check(dhexp_typ, "1 + 2 + 3 -> 6", parse_exp("6"), final_exp);
      },
    ),
    test_case(
      "step chain: validity propagation true",
      `Quick,
      () => {
        // Chain that ends in true should have validity Some(true)
        // Use a simpler expression that requires fewer steps
        let exp = parse_exp("1 == 1");
        let elab = elaborate(exp);

        // Take one step: 1 == 1 -> true
        let step1 = StepKindHelpers.mk_auto_single_step(elab);

        let (_, final_exp, validity) =
          run_step_chain(~initial_exp=elab, ~step_kinds=[step1], ());

        check(dhexp_typ, "reduces to true", parse_exp("true"), final_exp);
        check(option(bool), "chain validity is true", Some(true), validity);
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
        // Test that we can create an InductionStep with a scrutinee
        let scrut_exp = parse_exp("n");
        let step_kind = StepKindHelpers.mk_induction_step(~exp=scrut_exp, ());
        let step = mk_test_step(~step_kind, ());

        // Just verify it calculates without crashing
        let (_, _, _) = test_calculate(~exp=parse_exp("true"), step);
        // If we get here, the test passed
        check(bool, "induction step created", true, true);
      },
    ),
    test_case(
      "induction step: exhaustiveness check empty",
      `Quick,
      () => {
        // An InductionStep with no cases should not be exhaustive
        let scrut_exp = parse_exp("true");
        let elab = elaborate(scrut_exp);
        let step_kind = StepKindHelpers.mk_induction_step(~exp=elab, ());
        let step = mk_test_step(~step_kind, ());

        let (model, _, _) = test_calculate(~exp=parse_exp("true"), step);

        // Extract the InductionStep from the model to check exhaustiveness
        switch (model.step_kind) {
        | StepperBase.InductionStep(ind_model) =>
          let is_exhaustive =
            ind_model.is_exhaustive
            |> Calc.get_saved_exc(~print="is_exhaustive not calculated");
          check(bool, "empty induction not exhaustive", false, is_exhaustive);
        | _ => failwith("Expected InductionStep")
        };
      },
    ),
    test_case(
      "induction step: validity with no cases on concrete type",
      `Quick,
      () => {
        // An InductionStep with no cases on a concrete type (like Int) should be None (not valid)
        // because it's not exhaustive
        let scrut_exp = parse_exp("42"); // Int type - concrete
        let elab = elaborate(scrut_exp);
        let step_kind = StepKindHelpers.mk_induction_step(~exp=elab, ());
        let step = mk_test_step(~step_kind, ());

        let (model, _, _) = test_calculate(~exp=parse_exp("true"), step);

        // Check that validity is None - concrete type with no cases is not valid
        switch (model.step_kind) {
        | StepperBase.InductionStep(ind_model) =>
          let ind_validity =
            ind_model.validity
            |> Calc.get_saved_exc(~print="validity not calculated");
          check(
            option(bool),
            "no cases on concrete type is not valid",
            None,
            ind_validity,
          );
        | _ => failwith("Expected InductionStep")
        };
      },
    ),
    test_case(
      "induction step: calculate result expression",
      `Quick,
      () => {
        // An InductionStep should calculate a result expression (join_exp)
        let scrut_exp = parse_exp("n");
        let step_kind = StepKindHelpers.mk_induction_step(~exp=scrut_exp, ());
        let step = mk_test_step(~step_kind, ());

        let (model, _, _) = test_calculate(~exp=parse_exp("true"), step);

        // Check that result and join_exp are calculated
        switch (model.step_kind) {
        | StepperBase.InductionStep(ind_model) =>
          let result = ind_model.result |> Calc.get_saved_opt;
          let join_exp = ind_model.join_exp |> Calc.get_saved_opt;
          // Both should be calculated (even if the result might be an error)
          check(bool, "result is calculated", true, Option.is_some(result));
          check(
            bool,
            "join_exp is calculated",
            true,
            Option.is_some(join_exp),
          );
        | _ => failwith("Expected InductionStep")
        };
      },
    ),
    // Complete proof of xs == xs by induction with proof steps
    test_case(
      "induction step: complete proof xs == xs",
      `Quick,
      () => {
        // Build induction proof: for all xs, xs == xs
        // The inductive case generates an inductive hypothesis "ih: tl == tl"
        // by substituting xs->tl in the goal "xs == xs".
        // Base case: [] == [] (by reflexivity)
        // Inductive case: hd::tl == hd::tl (using reflexivity, with IH available)
        // Note: The IH exists and could be used in a more complex proof
        let step =
          mk_test_step(
            ~step_kind=
              InductionCaseHelpers.mk_induction_with_cases(
                ~scrut_str="xs",
                ~cases=[
                  InductionCaseHelpers.mk_case_with_steps(
                    ~pattern_str="[]",
                    ~goal_str="[] == []",
                    [
                      StepKindHelpers.mk_axiom_step(
                        ~name="Reflexive(==)",
                        ~equality="Reflexive(==)",
                        parse_exp("[] == []"),
                      ),
                    ],
                  ),
                  InductionCaseHelpers.mk_case_with_steps(
                    ~pattern_str="hd::tl",
                    ~goal_str="hd::tl == hd::tl",
                    [
                      StepKindHelpers.mk_axiom_step(
                        ~name="ih",
                        ~equality="ih",
                        parse_exp("tl"),
                      ),
                      StepKindHelpers.mk_axiom_step(
                        ~name="Reflexive(==)",
                        ~equality="Reflexive(==)",
                        parse_exp("hd::tl == hd::tl"),
                      ),
                    ],
                  ),
                ],
              ),
            (),
          );

        // Run calculate on the entire proof structure
        // Need to add xs to context with List type for IH generation to work
        let xs_entry: Ctx.entry =
          VarEntry({
            name: "xs",
            id: Id.invalid,
            typ: List(Atom(Int) |> Typ.temp) |> Typ.temp,
            custom_statics: None,
          });
        let ctx_with_xs =
          Ctx.extend(Builtins.ctx_init(Some(Int)), xs_entry);
        let (model, _, _) =
          test_calculate(~exp=parse_exp("xs == xs"), ~ctx=ctx_with_xs, step);

        // Verify the proof
        switch (model.step_kind) {
        | InductionStep({cases, is_exhaustive, validity, _}) =>
          check(int, "should have 2 cases", 2, List.length(cases));
          check(
            bool,
            "should be exhaustive",
            true,
            is_exhaustive
            |> Calc.get_saved_opt
            |> Option.value(~default=false),
          );

          // Verify inductive hypothesis IS generated
          let inductive_case = List.nth(cases, 1);
          let hypotheses =
            inductive_case.Web.InductionCase.hypotheses
            |> Calc.get_saved_opt
            |> Option.value(~default=[]);

          check(
            int,
            "inductive case should have 2 hypotheses (case_eq and ih)",
            2,
            List.length(hypotheses),
          );

          let has_ih =
            List.exists(
              ((binding, _)) => binding.Binding.name == "ih",
              hypotheses,
            );
          check(bool, "should have 'ih' hypothesis", true, has_ih);

          // Proof should be valid with reflexivity steps
          check(
            option(option(bool)),
            "proof should be valid",
            Some(Some(true)),
            validity |> Calc.get_saved_opt,
          );
        | _ => Alcotest.fail("Expected InductionStep")
        };
      },
    ),
  ],
);
