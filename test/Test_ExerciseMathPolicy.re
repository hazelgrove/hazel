open Alcotest;
open Language;
open IdTagged.FreshGrammar;
open Util;

let write_text_file = (path, contents) => {
  let channel = open_out(path);
  output_string(channel, contents);
  close_out(channel);
};

let string_contains = (needle, haystack) => {
  let needle_len = String.length(needle);
  let haystack_len = String.length(haystack);
  let rec loop = offset =>
    offset
    + needle_len <= haystack_len
    && (
      String.sub(haystack, offset, needle_len) == needle || loop(offset + 1)
    );
  needle_len == 0 || loop(0);
};

let theorem_spec = exercise =>
  switch (exercise) {
  | Web.Exercise.Theorem(spec) => spec
  | _ => fail("expected a theorem exercise")
  };

let require_policy = exercise =>
  switch (theorem_spec(exercise).math_policy) {
  | Some(policy) => policy
  | None => fail("expected an exercise math policy")
  };

let exp_of_source = source =>
  switch (Haz3lcore.Parser.to_zipper(~root=Exp, source)) {
  | Some(zipper) =>
    Haz3lcore.MakeTerm.from_zip_for_sem(zipper, ~root=Exp).term
  | None => fail("expected expression source to parse")
  };

/* The case-study sources elaborate beneath [use Real].  Build behavioral
 * checker fixtures with the same operators and literal representation that
 * the browser stepper receives, rather than relying on parser-default Ints. */
module BrowserReal = {
  let number = value => Exp.real(Real.of_bigint(Bigint.of_int(value)));
  let plus = (left, right) =>
    Exp.bin_op(Operators.Real(Operators.Plus), left, right);
  let minus = (left, right) =>
    Exp.bin_op(Operators.Real(Operators.Minus), left, right);
  let times = (left, right) =>
    Exp.bin_op(Operators.Real(Operators.Times), left, right);
  let divide = (left, right) =>
    Exp.bin_op(Operators.Real(Operators.Divide), left, right);
  let power = (left, right) =>
    Exp.bin_op(Operators.Real(Operators.Power), left, right);
};

let check_policy = (label, exercise, expected_level, expected_stage) => {
  let policy = require_policy(exercise);
  let profile = Web.ExerciseMathPolicy.resolved_profile(policy);
  check(bool, label ++ " level", true, profile.level == expected_level);
  check(
    bool,
    label ++ " automation stage",
    true,
    policy.automation_stage == expected_stage,
  );
};

let tests = (
  "ExerciseMathPolicy",
  [
    test_case(
      "initial case-study exercises have reproducible locked policies",
      `Quick,
      () => {
        let exercises = [
          Web.Ex_OrderOfOperations.exercise,
          Web.Ex_FoilVerbose.exercise,
          Web.Ex_FoilAutomated.exercise,
          Web.Ex_CompletingTheSquare.exercise,
          Web.Ex_TrigPowerReduction.exercise,
          Web.Ex_PolynomialDerivative.exercise,
          Web.Ex_QuadraticTaylorApproximation.exercise,
          Web.Ex_TrigTaylorApproximation.exercise,
        ];
        check(int, "exercise count", 8, List.length(exercises));
        exercises
        |> List.iter(exercise => {
             let policy = require_policy(exercise);
             check(bool, "profile locked", true, policy.lock_profile);
             check(
               bool,
               "automation locked",
               true,
               policy.lock_automation_stage,
             );
             Web.ExerciseMathPolicy.resolve(policy)
             |> (
               fun
               | Ok(_) => ()
               | Error(error) =>
                 fail(CustomMathMode.resolution_error_message(error))
             );
           });
        check(
          bool,
          "order of operations hides available-step highlights",
          false,
          require_policy(Web.Ex_OrderOfOperations.exercise).
            show_next_step_hints,
        );
        exercises
        |> List.tl
        |> List.iter(exercise =>
             check(
               bool,
               "other case studies retain available-step highlights",
               true,
               require_policy(exercise).show_next_step_hints,
             )
           );
      },
    ),
    test_case(
      "order of operations is an Explore exercise with a hidden target",
      `Quick,
      () => {
        let spec = theorem_spec(Web.Ex_OrderOfOperations.exercise);
        check(
          bool,
          "hidden completion target is configured",
          true,
          Option.is_some(spec.expected_explore_result),
        );
        let model = Web.TheoremExerciseMode.Model.of_spec(spec);
        check(
          bool,
          "Explore target survives the exercise model round trip",
          true,
          Web.TheoremExerciseMode.Model.spec_of_t(model).
            expected_explore_result
          == spec.expected_explore_result,
        );
        Web.TheoremExerciseMode.Model.get_problem_editors(model)
        |> List.rev
        |> List.hd
        |> (
          ((label, _)) =>
            check(
              option(string),
              "problem sidebar label",
              Some("Explore"),
              label,
            )
        );
      },
    ),
    test_case(
      "Explore score requires reaching the configured terminal result",
      `Quick,
      () => {
        let target = exp_of_source("17");
        let (initial_earned, initial_max) =
          Web.Theorems.Model.explore_completion_score(
            ~target,
            exp_of_source("3 + 4 * 2 ** 2 - 6 / 3"),
          );
        let (equivalent_earned, _) =
          Web.Theorems.Model.explore_completion_score(
            ~target,
            exp_of_source("14 + 3"),
          );
        let (finished_earned, finished_max) =
          Web.Theorems.Model.explore_completion_score(
            ~target,
            exp_of_source("17"),
          );
        check(
          bool,
          "initial expression is incomplete",
          true,
          initial_earned == 0.0,
        );
        check(
          bool,
          "unfinished equivalent form is incomplete",
          true,
          equivalent_earned == 0.0,
        );
        check(
          bool,
          "exact terminal result earns credit",
          true,
          finished_earned == 1.0,
        );
        check(
          bool,
          "score maximum is stable",
          true,
          initial_max == 1.0 && finished_max == 1.0,
        );
      },
    ),
    test_case(
      "long theorem exercise statements are structurally line-wrapped",
      `Quick,
      () => {
        let model =
          Web.Ex_FoilAutomated.exercise
          |> theorem_spec
          |> Web.TheoremExerciseMode.Model.of_spec;
        let formatted =
          model.cells.theorem.editor.editor.state.zipper
          |> Haz3lcore.Printer.of_zipper(~holes="", ~indent="");
        check(
          bool,
          "pretty-printed theorem contains a line break",
          true,
          String.contains(formatted, '\n'),
        );
        check(
          bool,
          "pretty-printing preserves theorem identity",
          true,
          string_contains("theorem foil_with_cleanup", formatted),
        );
      },
    ),
    test_case(
      "case-study verbosity matches each teaching goal",
      `Quick,
      () => {
        check_policy(
          "order of operations",
          Web.Ex_OrderOfOperations.exercise,
          Axioms.Arithmetic,
          Axioms.Manual,
        );
        check_policy(
          "written-out FOIL",
          Web.Ex_FoilVerbose.exercise,
          Axioms.Algebra,
          Axioms.Manual,
        );
        check_policy(
          "cleanup FOIL",
          Web.Ex_FoilAutomated.exercise,
          Axioms.Algebra,
          Axioms.Manual,
        );
        check_policy(
          "completing the square",
          Web.Ex_CompletingTheSquare.exercise,
          Axioms.Algebra,
          Axioms.MultiStepCheck,
        );
        check_policy(
          "trig power reduction",
          Web.Ex_TrigPowerReduction.exercise,
          Axioms.Trigonometry,
          Axioms.MultiStepCheck,
        );
        check_policy(
          "polynomial derivative",
          Web.Ex_PolynomialDerivative.exercise,
          Axioms.Calculus,
          Axioms.MultiStepCheck,
        );
        check_policy(
          "introductory Taylor approximation",
          Web.Ex_QuadraticTaylorApproximation.exercise,
          Axioms.Calculus,
          Axioms.MultiStepCheck,
        );
        check_policy(
          "advanced Taylor approximation",
          Web.Ex_TrigTaylorApproximation.exercise,
          Axioms.Calculus,
          Axioms.MultiStepCheck,
        );
      },
    ),
    test_case(
      "compiled case-study rules never inherit disabled cleanup",
      `Quick,
      () => {
        let exercises = [
          Web.Ex_OrderOfOperations.exercise,
          Web.Ex_FoilVerbose.exercise,
          Web.Ex_FoilAutomated.exercise,
          Web.Ex_CompletingTheSquare.exercise,
          Web.Ex_TrigPowerReduction.exercise,
          Web.Ex_PolynomialDerivative.exercise,
          Web.Ex_QuadraticTaylorApproximation.exercise,
          Web.Ex_TrigTaylorApproximation.exercise,
        ];
        exercises
        |> List.iter(exercise => {
             let profile =
               exercise
               |> require_policy
               |> Web.ExerciseMathPolicy.resolved_profile;
             [Axioms.Manual, Axioms.MultiStepCheck, Axioms.AutoEval]
             |> List.iter(stage =>
                  Axioms.stage_plan_for_profile(profile, stage).visible_rules
                  |> List.iter((planned: Axioms.planned_visible_rule) =>
                       planned.allowed_cleanup
                       |> List.iter(cleanup =>
                            check(
                              bool,
                              "compiled cleanup remains globally enabled",
                              true,
                              List.mem(
                                cleanup,
                                profile.step_policy.default_cleanup,
                              ),
                            )
                          )
                     )
                );
             check(
               list(bool),
               "hidden rule receives no cleanup authority",
               [],
               Axioms.cleanup_for_visible_rule(
                 profile.step_policy,
                 "test.rule.not.visible",
               )
               |> List.map(_ => true),
             );
           });
        let derivative_profile =
          Web.Ex_PolynomialDerivative.exercise
          |> require_policy
          |> Web.ExerciseMathPolicy.resolved_profile;
        let power_rule =
          Axioms.stage_plan_for_profile(
            derivative_profile,
            Axioms.MultiStepCheck,
          ).
            visible_rules
          |> List.find((planned: Axioms.planned_visible_rule) =>
               planned.rule.id == "calc.diff_power"
             );
        check(
          bool,
          "disabled derivative cleanup absent from compiled power rule",
          false,
          List.mem(Axioms.DerivativeBasics, power_rule.allowed_cleanup),
        );
        let power_disabled_manually =
          Axioms.profile_with_capability_usage_overrides(
            derivative_profile,
            derivative_profile.capability_usage_overrides
            @ [
              Axioms.{
                capability_id: "calc.diff_power",
                stage: Manual,
                usage: Disabled,
              },
            ],
          );
        let stage_exposes_power = stage =>
          Axioms.stage_plan_for_profile(power_disabled_manually, stage).
            visible_rules
          |> List.exists((planned: Axioms.planned_visible_rule) =>
               planned.rule.id == "calc.diff_power"
             );
        check(
          bool,
          "manual-disabled rule is absent from executable rules",
          false,
          stage_exposes_power(Manual),
        );
        check(
          bool,
          "stage-specific disable does not leak into Check Result",
          true,
          stage_exposes_power(MultiStepCheck),
        );
        check(
          bool,
          "manual Rocq search rejects the disabled rule",
          false,
          Axioms.profile_allows_rocq_rule_id(
            power_disabled_manually,
            Manual,
            "calc.diff_power",
          ),
        );
        check(
          bool,
          "Check Result Rocq search retains the enabled rule",
          true,
          Axioms.profile_allows_rocq_rule_id(
            power_disabled_manually,
            MultiStepCheck,
            "calc.diff_power",
          ),
        );
        let identity_disabled_for_check =
          Axioms.profile_with_capability_usage_overrides(
            derivative_profile,
            derivative_profile.capability_usage_overrides
            @ [
              Axioms.{
                capability_id: "mul.identity",
                stage: MultiStepCheck,
                usage: Disabled,
              },
            ],
          );
        let stage_cleanup = stage =>
          Axioms.stage_plan_for_profile(identity_disabled_for_check, stage).
            pre_cleanup;
        check(
          bool,
          "stage-disabled cleanup is absent from Check Result",
          false,
          List.mem(Axioms.MulIdentity, stage_cleanup(MultiStepCheck)),
        );
        check(
          bool,
          "stage-disabled cleanup remains available in Manual",
          true,
          List.mem(Axioms.MulIdentity, stage_cleanup(Manual)),
        );
        check(
          bool,
          "Check Result Rocq search rejects stage-disabled cleanup",
          false,
          Axioms.profile_allows_rocq_rule_id(
            identity_disabled_for_check,
            MultiStepCheck,
            "mul.identity",
          ),
        );
        check(
          bool,
          "manual Rocq search retains stage-enabled cleanup",
          true,
          Axioms.profile_allows_rocq_rule_id(
            identity_disabled_for_check,
            Manual,
            "mul.identity",
          ),
        );
      },
    ),
    test_case(
      "verbose FOIL allows factor commutation but not collection",
      `Quick,
      () => {
        let policy = require_policy(Web.Ex_FoilVerbose.exercise);
        let profile = Web.ExerciseMathPolicy.resolved_profile(policy);
        check(
          bool,
          "square identity hidden",
          false,
          Axioms.visible_rule_enabled(
            profile.step_policy,
            "alg.square_of_sum",
          ),
        );
        check(
          bool,
          "multiplication commutation enabled",
          true,
          List.mem(Axioms.MulComm, profile.step_policy.default_cleanup),
        );
        check(
          bool,
          "collection disabled",
          false,
          List.mem(
            Axioms.CollectLikeTerms,
            profile.step_policy.default_cleanup,
          ),
        );
        check(
          bool,
          "manual polynomial expansion disabled",
          false,
          Axioms.compiled_capability_enabled(
            Axioms.stage_plan_for_profile(profile, Axioms.Manual),
            "alg.expand_polynomial",
          ),
        );
        check(
          bool,
          "primitive distribution remains enabled",
          true,
          Axioms.visible_rule_enabled(
            profile.step_policy,
            "alg.distribute_mul_add",
          ),
        );
      },
    ),
    test_case(
      "cleanup FOIL permits whole-product expansion in One Step mode",
      `Quick,
      () => {
        let policy = require_policy(Web.Ex_FoilAutomated.exercise);
        let profile = Web.ExerciseMathPolicy.resolved_profile(policy);
        check(
          bool,
          "uses One Step interaction",
          true,
          policy.automation_stage == Axioms.Manual,
        );
        check(
          bool,
          "whole polynomial expansion is enabled",
          true,
          Axioms.compiled_capability_enabled(
            Axioms.stage_plan_for_profile(profile, Axioms.Manual),
            "alg.expand_polynomial",
          ),
        );
        check(
          bool,
          "primitive distribution remains enabled",
          true,
          Axioms.visible_rule_enabled(
            profile.step_policy,
            "alg.distribute_mul_add",
          ),
        );
        check(
          bool,
          "automatic collection is disabled",
          false,
          List.mem(
            Axioms.CollectLikeTerms,
            profile.step_policy.default_cleanup,
          ),
        );
        check(
          bool,
          "collect-like-terms remains a visible algebra rule",
          true,
          Axioms.visible_rule_enabled(
            profile.step_policy,
            "alg.collect_like_terms",
          ),
        );
        let x = Exp.var("x");
        let number = BrowserReal.number;
        let plus = BrowserReal.plus;
        let minus = BrowserReal.minus;
        let times = BrowserReal.times;
        let power = BrowserReal.power;
        let source =
          times(
            minus(times(number(2), x), number(3)),
            plus(x, number(4)),
          );
        let first_distribution =
          minus(
            times(times(number(2), x), plus(x, number(4))),
            times(number(3), plus(x, number(4))),
          );
        let target =
          minus(
            plus(
              times(number(2), power(x, number(2))),
              times(number(5), x),
            ),
            number(12),
          );
        let accepts = (from_, to_) =>
          switch (
            Web.ProfileProofPlan.authorize({
              profile,
              stage: Axioms.Manual,
              candidate_origin: Web.ProfileProofPlan.UserEntered,
              settings: CoreSettings.on,
              env: Environment.empty,
              source: from_,
              target: to_,
              max_depth: 1,
              max_states: 80,
            })
          ) {
          | Authorized(_) => true
          | Rejected(_) => false
          };
        let expanded =
          minus(
            plus(
              minus(
                times(number(2), power(x, number(2))),
                times(number(3), x),
              ),
              times(number(8), x),
            ),
            number(12),
          );
        let expanded_with_repeated_factor =
          minus(
            plus(
              minus(times(times(number(2), x), x), times(number(3), x)),
              times(number(8), x),
            ),
            number(12),
          );
        let cleanup = profile.step_policy.default_cleanup;
        check(
          bool,
          "cleanup FOIL accepts Real x*x to x**2 as a standalone One Step cleanup",
          true,
          accepts(times(x, x), power(x, number(2))),
        );
        check(
          bool,
          "Real repeated factors match power notation cleanup",
          true,
          Web.RewriteChecker.product_term_same_under_cleanup(
            cleanup,
            times(times(number(2), x), x),
            times(number(2), power(x, number(2))),
          ),
        );
        check(
          bool,
          "Real coefficient factors match constant-fold cleanup",
          true,
          Web.RewriteChecker.product_term_same_under_cleanup(
            cleanup,
            times(times(number(2), x), number(4)),
            times(number(8), x),
          ),
        );
        check(
          bool,
          "Real constant products match constant-fold cleanup",
          true,
          Web.RewriteChecker.product_term_same_under_cleanup(
            cleanup,
            times(number(3), number(4)),
            number(12),
          ),
        );
        check(
          bool,
          "Real whole-product terms match before collection",
          true,
          Web.RewriteChecker.uncollected_full_distribution_matches(
            profile,
            source,
            expanded,
          ),
        );
        check(
          bool,
          "Real whole-product expansion remains polynomial-equivalent",
          true,
          Web.RewriteChecker.polynomial_equivalent_exps(source, expanded),
        );
        check(
          bool,
          "whole product can expand to four visible products",
          true,
          accepts(source, expanded),
        );
        check(
          bool,
          "whole product accepts repeated factors before power notation cleanup",
          true,
          accepts(source, expanded_with_repeated_factor),
        );
        check(
          bool,
          "whole expansion cannot absorb the separate collection step",
          false,
          accepts(source, target),
        );
        check(
          bool,
          "first distribution is accepted",
          true,
          accepts(source, first_distribution),
        );
        check(
          bool,
          "second distribution is accepted",
          true,
          accepts(
            times(times(number(2), x), plus(x, number(4))),
            plus(
              times(number(2), power(x, number(2))),
              times(times(number(2), x), number(4)),
            ),
          ),
        );
        check(
          bool,
          "third distribution is accepted",
          true,
          accepts(
            times(number(3), plus(x, number(4))),
            plus(times(number(3), x), times(number(3), number(4))),
          ),
        );
        check(
          bool,
          "numeric coefficient product is simplified",
          true,
          accepts(
            times(times(number(2), x), number(4)),
            times(number(8), x),
          ),
        );
        check(
          bool,
          "constant product is simplified",
          true,
          accepts(times(number(3), number(4)), number(12)),
        );
        let products_folded =
          minus(
            plus(
              times(number(2), power(x, number(2))),
              times(number(8), x),
            ),
            plus(times(number(3), x), number(12)),
          );
        switch (
          Web.ProfileProofPlan.authorize({
            profile,
            stage: Axioms.Manual,
            candidate_origin: Web.ProfileProofPlan.UserEntered,
            settings: CoreSettings.on,
            env: Environment.empty,
            source: products_folded,
            target,
            max_depth: 1,
            max_states: 80,
          })
        ) {
        | Authorized(_) => ()
        | Rejected(rejection) =>
          fail(
            "explicit collection after expansion: "
            ++ Web.ProfileProofPlan.rejection_message(rejection),
          )
        };
        let collect_source =
          plus(times(number(8), x), times(number(3), x));
        let collect_target = times(number(11), x);
        switch (
          Web.RewriteChecker.check_single_step_result_for_profile(
            ~profile,
            ~settings=CoreSettings.on,
            ~env=Environment.empty,
            collect_source,
            collect_target,
          )
        ) {
        | Some(result) =>
          if (List.length(result.prover_steps) != 1) {
            fail(
              "unexpected collection replay: "
              ++ (
                result.prover_steps
                |> List.map((step: Web.ProofTrace.prover_step) =>
                     step.rule_id
                   )
                |> String.concat(" -> ")
              ),
            );
          };
          check(
            bool,
            "collection trace names the enabled rule",
            true,
            result.trace
            |> List.exists((rule: Axioms.rewrite_rule) =>
                 rule.id == "alg.collect_like_terms"
               ),
          );
        | None => fail("collect-like-terms backend rejected equivalent sums")
        };
        switch (
          Web.ProfileProofPlan.authorize({
            profile,
            stage: Axioms.Manual,
            candidate_origin: Web.ProfileProofPlan.UserEntered,
            settings: CoreSettings.on,
            env: Environment.empty,
            source: collect_source,
            target: collect_target,
            max_depth: 1,
            max_states: 80,
          })
        ) {
        | Authorized(_) => ()
        | Rejected(rejection) =>
          fail(Web.ProfileProofPlan.rejection_message(rejection))
        };
        let profile_without_automatic_commute: Axioms.math_profile = {
          ...profile,
          step_policy: {
            ...profile.step_policy,
            default_cleanup:
              profile.step_policy.default_cleanup
              |> List.filter(capability => capability != Axioms.AddComm),
          },
        };
        switch (
          Web.ProfileProofPlan.authorize({
            profile: profile_without_automatic_commute,
            stage: Axioms.Manual,
            candidate_origin: Web.ProfileProofPlan.UserEntered,
            settings: CoreSettings.on,
            env: Environment.empty,
            source: plus(x, number(1)),
            target: plus(number(1), x),
            max_depth: 1,
            max_states: 80,
          })
        ) {
        | Authorized(plan) =>
          check(
            bool,
            "explicit commutation retains its visible-rule identity",
            true,
            List.mem("arith.add_comm", plan.capability_ids)
            && !List.mem("add.comm", plan.capability_ids),
          )
        | Rejected(rejection) =>
          fail(
            "explicit commutation should not require automatic commutation: "
            ++ Web.ProfileProofPlan.rejection_message(rejection),
          )
        };
        check(
          bool,
          "signed linear terms are collected",
          true,
          accepts(
            minus(times(number(8), x), times(number(3), x)),
            times(number(5), x),
          ),
        );
        check(
          bool,
          "quadratic like terms are collected",
          true,
          accepts(
            plus(times(x, x), times(number(2), times(x, x))),
            times(number(3), times(x, x)),
          ),
        );
        check(
          bool,
          "unlike variables are not collected",
          false,
          accepts(
            plus(times(number(8), x), times(number(3), Exp.var("y"))),
            times(number(11), x),
          ),
        );
        check(
          bool,
          "collection cannot change a power",
          false,
          accepts(
            plus(power(x, number(3)), power(x, number(3))),
            times(number(2), power(x, number(2))),
          ),
        );
        let collection_disabled_profile = {
          ...profile,
          capability_usage_overrides: [
            {
              Axioms.capability_id: "alg.collect_like_terms",
              stage: Axioms.Manual,
              usage: Axioms.Disabled,
            },
            ...profile.capability_usage_overrides,
          ],
        };
        let accepts_with_profile = (profile, from_, to_) =>
          switch (
            Web.ProfileProofPlan.authorize({
              profile,
              stage: Axioms.Manual,
              candidate_origin: Web.ProfileProofPlan.UserEntered,
              settings: CoreSettings.on,
              env: Environment.empty,
              source: from_,
              target: to_,
              max_depth: 1,
              max_states: 80,
            })
          ) {
          | Authorized(_) => true
          | Rejected(_) => false
          };
        check(
          bool,
          "disabled collection rule stays disabled",
          false,
          accepts_with_profile(
            collection_disabled_profile,
            collect_source,
            collect_target,
          ),
        );
      },
    ),
    test_case(
      "completing the square rejects unrelated polynomial factoring",
      `Quick,
      () => {
        let policy = require_policy(Web.Ex_CompletingTheSquare.exercise);
        let profile = Web.ExerciseMathPolicy.resolved_profile(policy);
        check(
          bool,
          "arbitrary polynomial factor normalization disabled",
          false,
          Axioms.compiled_capability_enabled(
            Axioms.stage_plan_for_profile(profile, Axioms.MultiStepCheck),
            "alg.factor_polynomial_normalize",
          ),
        );
        check(
          bool,
          "general common/perfect-square factoring remains enabled",
          true,
          Axioms.visible_rule_enabled(
            profile.step_policy,
            "alg.factor_common",
          ),
        );
      },
    ),
    test_case(
      "trig power reduction exposes only lesson-relevant identities",
      `Quick,
      () => {
        let policy = require_policy(Web.Ex_TrigPowerReduction.exercise);
        let profile = Web.ExerciseMathPolicy.resolved_profile(policy);
        check(
          bool,
          "power-reduction identity enabled",
          true,
          Axioms.visible_rule_enabled(
            profile.step_policy,
            "trig.sin_squared_double",
          ),
        );
        check(
          bool,
          "alternative double-angle route enabled",
          true,
          Axioms.visible_rule_enabled(
            profile.step_policy,
            "trig.cos_double_sin",
          ),
        );
        check(
          bool,
          "unrelated angle-sum identity disabled",
          false,
          Axioms.visible_rule_enabled(profile.step_policy, "trig.sin_sum"),
        );
        check(
          bool,
          "unrelated reflection identity disabled",
          false,
          Axioms.visible_rule_enabled(profile.step_policy, "trig.cos_pi_sub"),
        );
      },
    ),
    test_case(
      "polynomial derivative Check Result certifies intermediate calculus rules",
      `Quick,
      () => {
        let policy = require_policy(Web.Ex_PolynomialDerivative.exercise);
        let profile = Web.ExerciseMathPolicy.resolved_profile(policy);
        let x = Language.Exp.fresh(Var("x"));
        let number = BrowserReal.number;
        let plus = BrowserReal.plus;
        let times = BrowserReal.times;
        let minus = BrowserReal.minus;
        let divide = BrowserReal.divide;
        let power = BrowserReal.power;
        let deriv = (body, variable) =>
          DerivativeOperator.expression(~body, ~variable);
        let cubic = power(x, number(3));
        let linear = times(number(2), x);
        let source = deriv(plus(cubic, linear), x);
        let target = plus(deriv(cubic, x), deriv(linear, x));
        let authorize =
            (~profile=profile, ~stage=Axioms.MultiStepCheck, source, target) =>
          Web.ProfileProofPlan.authorize({
            profile,
            stage,
            candidate_origin: Web.ProfileProofPlan.UserEntered,
            settings: CoreSettings.on,
            env: Environment.empty,
            source,
            target,
            max_depth: 6,
            max_states: 300,
          });
        let request = (source, target) =>
          Web.ProofSearchBackend.{
            backend: JSCoqTacticSearch,
            level: Axioms.Calculus,
            max_depth: 6,
            max_states: 300,
            source,
            target,
          };
        let expect_authorized_certificate =
            (~rocq_path=None, label, source, target) =>
          switch (authorize(source, target)) {
          | Authorized(plan) =>
            let program =
              Web.ProofSearchBackend.rocq_program_for_authorized_plan(
                ~profile,
                request(source, target),
                plan,
              );
            check(
              bool,
              label ++ " has a Rocq certificate",
              true,
              String.length(program) > 0,
            );
            switch (rocq_path) {
            | Some(path) => write_text_file(path, program)
            | None => ()
            };
            (plan, program);
          | Rejected(rejection) =>
            fail(Web.ProfileProofPlan.rejection_message(rejection))
          };
        let (linearity_plan, _) =
          expect_authorized_certificate(
            ~rocq_path=Some("/tmp/hazel_polynomial_derivative_linearity.v"),
            "sum linearity",
            source,
            target,
          );
        check(
          bool,
          "linearity route recorded",
          true,
          List.mem("calc.diff_sum", linearity_plan.summary.rule_ids),
        );
        let power_source = deriv(cubic, x);
        let power_target = times(number(3), power(x, number(2)));
        let expect_displayed_action = (label, source, rule_id, target) => {
          let actions =
            Web.AxiomsBox.calculus_actions_for_profile(~profile, source);
          check(
            Alcotest.int,
            label ++ " suggestion count",
            1,
            List.length(actions),
          );
          let action = List.hd(actions);
          check(string, label ++ " suggestion id", rule_id, action.rule_id);
          check(
            bool,
            label ++ " keeps disabled cleanup visible",
            true,
            Web.RewriteChecker.exp_same(target, action.after_exp),
          );
          switch (
            Web.ProfileProofPlan.authorize({
              profile,
              stage: Axioms.Manual,
              candidate_origin: Web.ProfileProofPlan.DisplayedSuggestion,
              settings: CoreSettings.on,
              env: Environment.empty,
              source,
              target: action.after_exp,
              max_depth: 1,
              max_states: 80,
            })
          ) {
          | Authorized(_) => ()
          | Rejected(rejection) =>
            fail(
              label
              ++ " displayed suggestion rejected: "
              ++ Web.ProfileProofPlan.rejection_message(rejection),
            )
          };
        };
        expect_displayed_action(
          "power rule",
          power_source,
          "calc.diff_power",
          times(power_target, deriv(x, x)),
        );
        let linear_derivative = deriv(linear, x);
        expect_displayed_action(
          "product rule",
          linear_derivative,
          "calc.diff_product",
          plus(
            times(deriv(number(2), x), x),
            times(number(2), deriv(x, x)),
          ),
        );
        let suggest = (~profile=profile, source) =>
          Web.RewriteChecker.simplify_for_profile(
            ~profile,
            ~settings=CoreSettings.on,
            ~env=Environment.empty,
            source,
          );
        let expect_suggestion = (~profile=profile, label, source, target) =>
          switch (suggest(~profile, source)) {
          | Some(result) =>
            check(
              bool,
              label,
              true,
              Web.RewriteChecker.exp_same(target, result),
            )
          | None => fail(label ++ " should produce a profile suggestion")
          };
        expect_suggestion(
          "power Check Result suggestion",
          power_source,
          power_target,
        );
        expect_suggestion(
          "constant-times-variable Check Result suggestion",
          linear_derivative,
          number(2),
        );
        expect_suggestion(
          "variable-times-constant Check Result suggestion",
          deriv(times(x, number(5)), x),
          number(5),
        );
        expect_suggestion(
          "complete polynomial Check Result suggestion",
          source,
          plus(power_target, number(2)),
        );
        let (power_plan, power_program) =
          expect_authorized_certificate(
            ~rocq_path=Some("/tmp/hazel_derivative_power_result.v"),
            "completed power-rule result",
            power_source,
            power_target,
          );
        ["calc.diff_power", "calc.diff_variable", "mul.identity"]
        |> List.iter(rule_id =>
             check(
               bool,
               rule_id ++ " route recorded",
               true,
               List.mem(rule_id, power_plan.summary.rule_ids),
             )
           );
        check(
          bool,
          "power cleanup replays the authorized identity lemma",
          true,
          string_contains("rewrite Rmult_1_r", power_program)
          && !string_contains("{ lra. }", power_program),
        );
        check(
          bool,
          "variable derivative remains an explicit One Step rule",
          true,
          switch (authorize(~stage=Axioms.Manual, deriv(x, x), number(1))) {
          | Authorized(_) => true
          | Rejected(_) => false
          },
        );
        check(
          bool,
          "completed power result is not collapsed into One Step",
          true,
          switch (authorize(~stage=Axioms.Manual, power_source, power_target)) {
          | Rejected(_) => true
          | Authorized(_) => false
          },
        );
        let product_source = deriv(times(cubic, linear), x);
        let product_target =
          plus(
            times(deriv(cubic, x), linear),
            times(cubic, deriv(linear, x)),
          );
        expect_authorized_certificate(
          "product rule intermediate",
          product_source,
          product_target,
        )
        |> ignore;
        let denominator = plus(x, number(1));
        let quotient_source = deriv(divide(cubic, denominator), x);
        let quotient_target =
          divide(
            minus(
              times(deriv(cubic, x), denominator),
              times(cubic, deriv(denominator, x)),
            ),
            power(denominator, number(2)),
          );
        expect_authorized_certificate(
          ~rocq_path=Some("/tmp/hazel_derivative_quotient_intermediate.v"),
          "quotient rule intermediate",
          quotient_source,
          quotient_target,
        )
        |> ignore;
        let typo_target =
          plus(deriv(power(x, number(2)), x), deriv(linear, x));
        check(
          bool,
          "changing the cubic exponent is rejected",
          true,
          switch (authorize(source, typo_target)) {
          | Rejected(_) => true
          | Authorized(_) => false
          },
        );
        let without_sum =
          Web.ProfileBoard.profile_without_visible_rule(
            ~rule_id="calc.diff_sum",
            profile,
          );
        check(
          bool,
          "disabled sum rule cannot reach the certificate layer",
          true,
          switch (authorize(~profile=without_sum, source, target)) {
          | Rejected(_) => true
          | Authorized(_) => false
          },
        );
        ["calc.diff_power", "calc.diff_variable"]
        |> List.iter(rule_id => {
             let disabled =
               Web.ProfileBoard.profile_without_visible_rule(
                 ~rule_id,
                 profile,
               );
             check(
               bool,
               "disabled " ++ rule_id ++ " blocks the completed power result",
               true,
               switch (
                 authorize(~profile=disabled, power_source, power_target)
               ) {
               | Rejected(_) => true
               | Authorized(_) => false
               },
             );
             check(
               bool,
               "disabled " ++ rule_id ++ " is not used by suggestions",
               true,
               switch (suggest(~profile=disabled, power_source)) {
               | Some(result) =>
                 Web.DifferentiationRewrite.contains_diff(result)
               | None => true
               },
             );
           });
        let without_mul_identity =
          Web.ProfileBoard.profile_with_cleanup(
            ~cleanup=
              profile.step_policy.default_cleanup
              |> List.filter(capability => capability != Axioms.MulIdentity),
            profile,
          );
        switch (
          authorize(~profile=without_mul_identity, power_source, power_target)
        ) {
        | Rejected(_) => ()
        | Authorized(plan) =>
          fail(
            "disabled mul.identity admitted route: "
            ++ String.concat(", ", plan.summary.rule_ids),
          )
        };
      },
    ),
    test_case(
      "locked stepper ignores profile and automation mutations",
      `Quick,
      () => {
        let policy = require_policy(Web.Ex_FoilVerbose.exercise);
        let model =
          Web.StepperView.Model.init
          |> Web.StepperView.Model.with_math_policy(Some(policy));
        let level_changed =
          Web.StepperView.Update.update(
            ~settings=Web.Settings.Model.init,
            Web.StepperView.Update.SelectRewriteLevel(Axioms.Calculus),
            model,
          ).
            model;
        let automation_changed =
          Web.StepperView.Update.update(
            ~settings=Web.Settings.Model.init,
            Web.StepperView.Update.SelectAutomationStage(Axioms.AutoEval),
            level_changed,
          ).
            model;
        check(
          bool,
          "level unchanged",
          true,
          automation_changed.rewrite_level == Axioms.Algebra,
        );
        check(
          bool,
          "automation unchanged",
          true,
          automation_changed.automation_stage == Axioms.Manual,
        );
        check(
          bool,
          "policy retained",
          true,
          automation_changed.math_policy == Some(policy),
        );
        check(
          bool,
          "fully locked policy hides redundant math automation controls",
          false,
          Web.StepperView.Model.shows_math_automation_controls(
            automation_changed,
          ),
        );
        check(
          bool,
          "ordinary stepper keeps math automation controls",
          true,
          Web.StepperView.Model.shows_math_automation_controls(
            Web.StepperView.Model.init,
          ),
        );
        let locked_result =
          Web.EvalResult.Model.init
          |> Web.EvalResult.Model.with_math_policy(Some(policy));
        check(
          bool,
          "result model retains its exercise policy",
          true,
          locked_result.math_policy == Some(policy),
        );
        let opened_lemma_result =
          Web.EvalResult.Update.update(
            ~settings=Web.Settings.Model.init,
            Web.EvalResult.Update.ToggleStepper,
            locked_result,
          ).
            model;
        switch (opened_lemma_result.display) {
        | Stepper(stepper) =>
          check(
            bool,
            "locked scratch stepper hides math-mode controls",
            false,
            Web.StepperView.Model.shows_math_automation_controls(stepper),
          )
        | Evaluation(_) => fail("expected the scratch stepper to open")
        };
      },
    ),
    test_case(
      "theorem and Explore calculation refreshes the exercise policy",
      `Quick,
      () => {
        let policy = require_policy(Web.Ex_OrderOfOperations.exercise);
        let refreshed =
          Web.Theorems.Update.calculate_stepper(
            ~settings=Calc.NewValue(CoreSettings.on),
            ~math_policy=Some(policy),
            ~sem_ctx=
              Calc.NewValue(
                SemanticCtx.of_ctx_and_env(
                  Builtins.ctx_init(Some(Int)),
                  Builtins.env_init,
                ),
              ),
            ~goal_exp=
              Calc.NewValue(
                Language.Exp.fresh(Atom(Int(Bigint.of_int(3)))),
              ),
            Web.StepperView.Model.init,
          );
        check(
          bool,
          "locked controls remain hidden during calculation",
          false,
          Web.StepperView.Model.shows_math_automation_controls(refreshed),
        );
        check(
          bool,
          "calculation restores One Step",
          true,
          refreshed.automation_stage == Axioms.Manual,
        );
        check(
          bool,
          "calculation restores suppressed next-step hints",
          false,
          Option.get(refreshed.math_policy).show_next_step_hints,
        );
      },
    ),
    test_case(
      "theorem stepper uses elaborated numeric operators",
      `Quick,
      () => {
        let source = exp_of_source("use Real in (x ** 2 + 1)");
        let (info_map, _) =
          Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), source);
        let elaborated =
          Web.Theorems.elaborated_exp_for_stepper(info_map, source);
        let rec has_real_operators = (exp: Language.Exp.t) =>
          switch (exp.term) {
          | Parens(inner) => has_real_operators(inner)
          | BinOp(
              Operators.Real(Operators.Plus),
              {term: BinOp(Operators.Real(Operators.Power), _, _), _},
              _,
            ) =>
            true
          | _ => false
          };
        check(
          bool,
          "Real mode reaches the stepper expression",
          true,
          has_real_operators(elaborated),
        );
      },
    ),
    test_case(
      "Taylor theorem-local functions elaborate in Real mode", `Quick, () => {
      [
        (
          "quadratic Taylor theorem",
          Web.Ex_QuadraticTaylorApproximation.exercise,
        ),
        (
          "trigonometric Taylor theorem",
          Web.Ex_TrigTaylorApproximation.exercise,
        ),
      ]
      |> List.iter(((label, exercise)) => {
           let spec = theorem_spec(exercise);
           let term_of_zipper = zipper =>
             Haz3lcore.MakeTerm.from_zip_for_sem(zipper, ~root=Exp).term;
           let prelude = term_of_zipper(spec.prelude);
           let lemmas = term_of_zipper(spec.lemmas);
           let theorem = term_of_zipper(spec.theorem);
           check(
             bool,
             label ++ " keeps its function definitions out of the prelude",
             true,
             switch (prelude.term) {
             | EmptyHole => true
             | _ => false
             },
           );
           let stitched_scratch = Web.EditorUtil.append_exp(prelude, lemmas);
           let stitched_theorem =
             stitched_scratch
             |> Web.EditorUtil.append_exp(
                  _,
                  prelude
                  |> Language.ProofHacks.strip_theorems
                  |> Language.Exp.replace_all_ids,
                )
             |> Web.EditorUtil.append_exp(_, theorem);
           let prelude_statics =
             Haz3lcore.CachedStatics.init_from_term(
               ~settings=CoreSettings.on,
               ~is_dynamic_term=false,
               prelude,
             );
           let theorem_statics =
             Haz3lcore.CachedStatics.init_from_term(
               ~settings=CoreSettings.on,
               ~is_dynamic_term=false,
               stitched_theorem,
             );
           check(
             list(string),
             label ++ " has no Real/Int consistency errors",
             [],
             prelude_statics.error_ids
             @ theorem_statics.error_ids
             |> List.map(Id.show),
           );
         })
    }),
    test_case(
      "rewrite mini-editor preserves the selected Real numeric mode",
      `Quick,
      () => {
        let real_typ = Language.Typ.temp(Atom(Real));
        let base_ctx =
          Ctx.extend(
            Builtins.ctx_init(Some(Int)),
            Ctx.VarEntry({
              name: "x",
              id: Id.invalid,
              typ: real_typ,
              custom_statics: None,
            }),
          );
        let ctx =
          Web.MissingStep.rewrite_editor_ctx(
            ~ctx=base_ctx,
            ~ana=Some(real_typ),
          );
        let target = exp_of_source("x ** 2 + 6 * x");
        let (info_map, elaborated) =
          Statics.mk(~ana=real_typ, CoreSettings.on, ctx, target);
        let rec has_real_operators = (exp: Language.Exp.t) =>
          switch (exp.term) {
          | BinOp(
              Operators.Real(Operators.Plus),
              {term: BinOp(Operators.Real(Operators.Power), _, _), _},
              {term: BinOp(Operators.Real(Operators.Times), _, _), _},
            ) =>
            true
          | Parens(inner) => has_real_operators(inner)
          | _ => false
          };
        check(
          list(string),
          "target has no Real/Int consistency errors",
          [],
          Statics.Map.error_ids(info_map) |> List.map(Id.show),
        );
        check(
          bool,
          "fresh target operators elaborate as Real",
          true,
          has_real_operators(elaborated),
        );
      },
    ),
    test_case(
      "completing-square profile suggestion remains Real after insertion",
      `Quick,
      () => {
        let real_typ = Language.Typ.temp(Atom(Real));
        let real_ctx =
          Ctx.extend(
            Builtins.ctx_init(Some(Real)),
            Ctx.VarEntry({
              name: "x",
              id: Id.invalid,
              typ: real_typ,
              custom_statics: None,
            }),
          );
        let stepper_ctx =
          Ctx.extend(
            Builtins.ctx_init(Some(Int)),
            Ctx.VarEntry({
              name: "x",
              id: Id.invalid,
              typ: real_typ,
              custom_statics: None,
            }),
          );
        let surface = exp_of_source("(x + 3) ** 2 + -4 == (x + 3) ** 2 - 4");
        let (initial_info, elaborated) =
          Statics.mk(CoreSettings.on, real_ctx, surface);
        check(
          list(string),
          "initial completing-square equality has no type errors",
          [],
          Statics.Map.error_ids(initial_info) |> List.map(Id.show),
        );
        let elaborated_left =
          switch (elaborated.term) {
          | BinOp(Operators.Poly(Operators.Equals), left, _) => left
          | _ => fail("expected an elaborated equality")
          };
        let candidate =
          switch (
            Web.RewriteChecker.simplify_for_profile(
              ~profile=Axioms.math_profile(Algebra),
              ~settings=CoreSettings.on,
              ~env=Environment.empty,
              elaborated_left,
            )
          ) {
          | Some(candidate) => candidate
          | None => fail("expected a completing-square cleanup suggestion")
          };
        let inserted =
          Language.ProofHacks.replace_nth_exp(
            elaborated_left,
            Language.ProofHacks.exp_idx(elaborated_left, elaborated),
            elaborated,
            candidate,
          )
          |> Option.get;
        let (inserted_info, _) =
          Statics.mk(CoreSettings.on, stepper_ctx, inserted);
        check(
          list(string),
          "inserted profile suggestion has no Real/Int type errors",
          [],
          Statics.Map.error_ids(inserted_info) |> List.map(Id.show),
        );
        let parser_default_target =
          exp_of_source("(x + 3) ** 2 - 4")
          |> Web.RewriteChecker.inherit_numeric_mode(~source=elaborated_left);
        let inserted_after_write_boundary =
          Language.ProofHacks.replace_nth_exp(
            elaborated_left,
            Language.ProofHacks.exp_idx(elaborated_left, elaborated),
            elaborated,
            parser_default_target,
          )
          |> Option.get;
        let (write_boundary_info, _) =
          Statics.mk(
            CoreSettings.on,
            stepper_ctx,
            inserted_after_write_boundary,
          );
        check(
          list(string),
          "written-step boundary upgrades parser-default nodes to Real",
          [],
          Statics.Map.error_ids(write_boundary_info) |> List.map(Id.show),
        );
      },
    ),
    test_case(
      "inherited trig-profile cleanup preserves Real mode",
      `Quick,
      () => {
        let x = Exp.var("x");
        let source =
          BrowserReal.plus(
            BrowserReal.plus(
              BrowserReal.plus(
                BrowserReal.number(1),
                BrowserReal.number(2),
              ),
              BrowserReal.number(3),
            ),
            BrowserReal.power(x, BrowserReal.number(2)),
          );
        let candidate =
          switch (
            Web.RewriteChecker.simplify_for_profile(
              ~profile=Axioms.math_profile(Trigonometry),
              ~settings=CoreSettings.on,
              ~env=Environment.empty,
              source,
            )
          ) {
          | Some(candidate) => candidate
          | None => fail("expected inherited arithmetic cleanup")
          };
        check(
          bool,
          "Trig profile performs inherited arithmetic cleanup",
          true,
          Web.RewriteChecker.exp_same(
            BrowserReal.plus(
              BrowserReal.times(x, x),
              BrowserReal.number(6),
            ),
            candidate,
          ),
        );
        check(
          bool,
          "inherited cleanup rebuilds its constants and operators as Real",
          true,
          switch (candidate.term) {
          | BinOp(
              Operators.Real(Operators.Plus),
              {term: Atom(Real(_)), _},
              {
                term:
                  BinOp(
                    Operators.Real(Operators.Power | Operators.Times),
                    _,
                    _,
                  ),
                _,
              },
            )
          | BinOp(
              Operators.Real(Operators.Plus),
              {
                term:
                  BinOp(
                    Operators.Real(Operators.Power | Operators.Times),
                    _,
                    _,
                  ),
                _,
              },
              {term: Atom(Real(_)), _},
            ) =>
            true
          | _ => false
          },
        );
      },
    ),
    test_case(
      "pending exercise evaluation retains its math policy",
      `Quick,
      () => {
        let policy = require_policy(Web.Ex_OrderOfOperations.exercise);
        let locked_result =
          Web.EvalResult.Model.init
          |> Web.EvalResult.Model.with_math_policy(Some(policy));
        let calculated =
          Web.EvalResult.Update.calculate(
            ~settings=CoreSettings.on,
            ~queue_worker=None,
            ~is_edited=false,
            Haz3lcore.CachedStatics.empty,
            locked_result,
          );
        check(
          bool,
          "result reset preserves the exercise policy",
          true,
          calculated.theorems.math_policy == Some(policy),
        );
      },
    ),
    test_case(
      "instructor can change an exercise math level and proof interaction",
      `Quick,
      () => {
        let model =
          Web.Ex_FoilVerbose.exercise
          |> theorem_spec
          |> Web.TheoremExerciseMode.Model.of_spec;
        let instructor_settings = {
          ...Web.Settings.Model.init,
          instructor_mode: true,
        };
        let changed_level =
          Web.TheoremExerciseMode.Update.update(
            ~settings=instructor_settings,
            Web.TheoremExerciseMode.Update.Instructor(
              Web.TheoremExerciseMode.Update.UpdateMathLevel(Axioms.Calculus),
            ),
            model,
          ).
            model;
        let changed =
          Web.TheoremExerciseMode.Update.update(
            ~settings=instructor_settings,
            Web.TheoremExerciseMode.Update.Instructor(
              Web.TheoremExerciseMode.Update.UpdateAutomationStage(
                Axioms.MultiStepCheck,
              ),
            ),
            changed_level,
          ).
            model;
        let policy =
          switch (changed.math_policy) {
          | Some(policy) => policy
          | None => fail("expected instructor-selected exercise policy")
          };
        check(
          bool,
          "changes the exercise profile parent",
          true,
          Web.ExerciseMathPolicy.resolved_profile(policy).level
          == Axioms.Calculus,
        );
        check(
          bool,
          "changes the exercise proof interaction",
          true,
          policy.automation_stage == Axioms.MultiStepCheck,
        );
        [changed.cells.prelude, changed.cells.lemmas, changed.cells.theorem]
        |> List.iter((cell: Web.CellEditor.Model.t) =>
             check(
               bool,
               "propagates the policy to every exercise cell",
               true,
               cell.result.math_policy == Some(policy),
             )
           );
        let student_attempt =
          Web.TheoremExerciseMode.Update.update(
            ~settings=Web.Settings.Model.init,
            Web.TheoremExerciseMode.Update.Instructor(
              Web.TheoremExerciseMode.Update.UpdateMathLevel(
                Axioms.Arithmetic,
              ),
            ),
            changed,
          ).
            model;
        check(
          bool,
          "student mode cannot change the instructor-selected policy",
          true,
          student_attempt.math_policy == Some(policy),
        );
        check(
          bool,
          "the changed policy is included in instructor module export",
          true,
          Web.TheoremExerciseMode.Model.spec_of_t(changed).math_policy
          == Some(policy),
        );
      },
    ),
    test_case(
      "Taylor case studies are Calculus exercises rather than new math levels",
      `Quick,
      () => {
      [
        Web.Ex_QuadraticTaylorApproximation.exercise,
        Web.Ex_TrigTaylorApproximation.exercise,
      ]
      |> List.iter(exercise => {
           let policy = require_policy(exercise);
           let profile = Web.ExerciseMathPolicy.resolved_profile(policy);
           check(
             bool,
             "uses Calculus",
             true,
             profile.level == Axioms.Calculus,
           );
           check(
             bool,
             "uses Check Result",
             true,
             policy.automation_stage == Axioms.MultiStepCheck,
           );
           check(
             bool,
             "derivative rules enabled",
             true,
             Axioms.visible_rule_enabled(
               profile.step_policy,
               "calc.diff_function_value",
             ),
           );
         })
    }),
  ],
);
