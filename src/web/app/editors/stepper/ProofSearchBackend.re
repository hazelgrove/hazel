open Language;

type backend =
  | LocalAxiomSearch
  | JSCoqTacticSearch;

type request = {
  backend,
  level: Axioms.rewrite_level,
  max_depth: int,
  max_states: int,
  source: Exp.t,
  target: Exp.t,
};

type outcome =
  | PrimitiveTrace(RewriteChecker.trace_summary)
  | CollapsedMacro(RewriteChecker.trace_summary)
  | Rejected(string);

let local_axiom_search = request =>
  AxiomSearch.search(
    ~level=request.level,
    ~max_depth=request.max_depth,
    ~max_states=request.max_states,
    request.source,
    request.target,
  )
  |> Option.map(AxiomSearch.trace_summary)
  |> Option.map(summary => PrimitiveTrace(summary))
  |> Option.value(~default=Rejected("local axiom search found no proof"));

let effective_profile_for_request = request =>
  Axioms.effective_profile_for_rewrite(
    ~requested_level=request.level,
    request.source,
    request.target,
  );

let domain_for_request = request => {
  let profile = effective_profile_for_request(request);
  switch (profile.rocq_domain_policy) {
  | Axioms.RealsByDefault => CoqExport.Reals
  | IntegersByDefault =>
    CoqExport.requires_reals(request.source)
    || CoqExport.requires_reals(request.target)
      ? CoqExport.Reals : CoqExport.Integers
  };
};

let vars_for_request = request =>
  CoqExport.unique_vars_in_ast(request.source)
  @ CoqExport.unique_vars_in_ast(request.target)
  |> RewriteChecker.dedup;

let forall_string = (~domain, vars) =>
  switch (vars) {
  | [] => ""
  | vars =>
    let typ =
      switch (domain) {
      | CoqExport.Reals => "R"
      | Integers => "Z"
      };
    "forall " ++ String.concat(" ", vars) ++ " : " ++ typ ++ ",";
  };

let tactic_plan_purpose_for_automation_stage = Axioms.tactic_plan_purpose_for_stage;

let effective_level_for_request = request =>
  effective_profile_for_request(request).level;

let rocq_tactic_step_script = (step: Axioms.rocq_tactic_step) =>
  switch (step.mode) {
  | Axioms.Once => step.tactic
  | TryOnce => "try " ++ step.tactic
  | RepeatUntilStuck => "repeat progress " ++ step.tactic
  | RepeatFuel(fuel) =>
    "hazel_repeat_fuel " ++ string_of_int(fuel) ++ "%nat " ++ step.tactic
  | FinishOnly => "try solve [" ++ step.tactic ++ "]"
  };

let rocq_tactic_plan_script = (plan: Axioms.rocq_tactic_plan) =>
  plan.steps
  |> List.map(rocq_tactic_step_script)
  |> (scripts => scripts @ ["reflexivity"])
  |> String.concat("; ");

let rocq_plan_for_profile_and_purpose = (profile, purpose) =>
  switch (purpose) {
  | Axioms.ValidatePrimitiveStep =>
    Axioms.stage_plan_for_profile(profile, Manual).rocq_plan
  | CheckResult =>
    Axioms.stage_plan_for_profile(profile, MultiStepCheck).rocq_plan
  | AutoSimplify => Axioms.stage_plan_for_profile(profile, AutoEval).rocq_plan
  | ValidateMacroStep => Axioms.rocq_tactic_plan_for_profile(profile, purpose)
  };

let catalog_domain =
  fun
  | CoqExport.Integers => Axioms.RocqIntegers
  | Reals => RocqReals;

let rocq_backend_tactics = (~domain, backend: Axioms.rocq_rule_backend) =>
  Axioms.rocq_tactics_for_domain(
    ~domain=catalog_domain(domain),
    backend.search_tactics,
  );

let tactic_alternatives = (~name, tactics) =>
  switch (tactics |> RewriteChecker.dedup) {
  | [] => "Ltac " ++ name ++ " := fail 0 \"no enabled rules\".\n"
  | tactics =>
    "Ltac "
    ++ name
    ++ " :=\n  first [\n    "
    ++ String.concat("\n  | ", tactics)
    ++ "\n  ].\n"
  };

let profile_allows_affine_constant_reordering = profile =>
  [Axioms.AddAssoc, AddComm, MulAssoc, MulComm]
  |> List.for_all(capability =>
       List.mem(capability, profile.Axioms.step_policy.default_cleanup)
     );

let goal_directed_finishers = (~domain, ~profile, request) =>
  RewriteChecker.rational_affine_equivalent_with_capabilities(
    ~allow_left_distribution=
      Axioms.visible_rule_enabled(
        profile.Axioms.step_policy,
        "alg.distribute_mul_add",
      ),
    ~allow_right_distribution=
      Axioms.visible_rule_enabled(
        profile.Axioms.step_policy,
        "alg.factor_common",
      ),
    ~allow_constant_reordering=
      profile_allows_affine_constant_reordering(profile),
    request.source,
    request.target,
  )
    ? Axioms.guarded_normalization_backend_for_profile(
        profile,
        "arith.affine_normalize",
      )
      |> Option.map(rocq_backend_tactics(~domain))
      |> Option.value(~default=[])
    : [];

let direct_certificate_prelude = domain =>
  switch (domain) {
  | CoqExport.Integers => "From Stdlib Require Import ZArith Lia.\nOpen Scope Z_scope.\n"
  | Reals =>
    "From Stdlib Require Import Rbase Rfunctions Rtrigo1 Lra.\n"
    ++ "Open Scope R_scope.\n"
  };

let direct_certificate_definitions = directed_finishers =>
  switch (directed_finishers |> RewriteChecker.dedup) {
  | [] => "Ltac hazel_profile_search := fail 0 \"no enabled rules\".\n"
  | tactics =>
    "Ltac hazel_profile_search :=\n  solve [\n    "
    ++ String.concat("\n  | ", tactics)
    ++ "\n  ].\n"
  };

type derivative_certificate = {
  raw_derivative: Exp.t,
  proof: string,
  nonzero_denominators: list(Exp.t),
};

let proof_block = proof => "{\n" ++ proof ++ "\n}";

let derivative_function = (~variable, expression) =>
  "(fun "
  ++ variable
  ++ " : R => "
  ++ CoqExport.string_of_d_for_domain(~domain=CoqExport.Reals, expression)
  ++ ")";

let rec derivative_certificate_for_expression = (~variable, expression) => {
  let expression = DifferentiationRewrite.strip(expression);
  let independent = !DifferentiationRewrite.depends_on(variable, expression);
  if (independent) {
    let constant =
      CoqExport.string_of_d_for_domain(~domain=CoqExport.Reals, expression);
    Some({
      raw_derivative: DifferentiationRewrite.int_exp(0),
      proof:
        "change (derivable_pt_lim (fct_cte ("
        ++ constant
        ++ ")) "
        ++ variable
        ++ " 0).\napply derivable_pt_lim_const.",
      nonzero_denominators: [],
    });
  } else {
    let combine_binary = (left, right, raw, lemma, assumptions) =>
      switch (
        derivative_certificate_for_expression(~variable, left),
        derivative_certificate_for_expression(~variable, right),
      ) {
      | (Some(left_proof), Some(right_proof)) =>
        let left_derivative =
          CoqExport.string_of_d_for_domain(
            ~domain=CoqExport.Reals,
            left_proof.raw_derivative,
          );
        let right_derivative =
          CoqExport.string_of_d_for_domain(
            ~domain=CoqExport.Reals,
            right_proof.raw_derivative,
          );
        Some({
          raw_derivative:
            raw(left_proof.raw_derivative, right_proof.raw_derivative),
          proof:
            "eapply "
            ++ "("
            ++ lemma
            ++ " "
            ++ derivative_function(~variable, left)
            ++ " "
            ++ derivative_function(~variable, right)
            ++ " "
            ++ variable
            ++ " ("
            ++ left_derivative
            ++ ") ("
            ++ right_derivative
            ++ ")).\n"
            ++ proof_block(left_proof.proof)
            ++ "\n"
            ++ proof_block(right_proof.proof)
            ++ assumptions,
          nonzero_denominators:
            left_proof.nonzero_denominators @ right_proof.nonzero_denominators,
        });
      | _ => None
      };
    switch (expression.term) {
    | Var(name) when name == variable =>
      Some({
        raw_derivative: DifferentiationRewrite.int_exp(1),
        proof:
          "change (derivable_pt_lim id "
          ++ variable
          ++ " 1).\napply derivable_pt_lim_id.",
        nonzero_denominators: [],
      })
    | BinOp(op, left, right)
        when DifferentiationRewrite.is_operator(Operators.Plus, op) =>
      combine_binary(
        left,
        right,
        DifferentiationRewrite.plus_exp,
        "derivable_pt_lim_plus",
        "",
      )
    | BinOp(op, left, right)
        when DifferentiationRewrite.is_operator(Operators.Minus, op) =>
      combine_binary(
        left,
        right,
        DifferentiationRewrite.minus_exp,
        "derivable_pt_lim_minus",
        "",
      )
    | BinOp(op, left, right)
        when DifferentiationRewrite.is_operator(Operators.Times, op) =>
      combine_binary(
        left,
        right,
        (left_derivative, right_derivative) =>
          DifferentiationRewrite.plus_exp(
            DifferentiationRewrite.times_exp(left_derivative, right),
            DifferentiationRewrite.times_exp(left, right_derivative),
          ),
        "derivable_pt_lim_mult",
        "",
      )
    | BinOp(op, numerator, denominator)
        when DifferentiationRewrite.is_operator(Operators.Divide, op) =>
      switch (
        derivative_certificate_for_expression(~variable, numerator),
        derivative_certificate_for_expression(~variable, denominator),
      ) {
      | (Some(numerator_proof), Some(denominator_proof)) =>
        let numerator_derivative =
          CoqExport.string_of_d_for_domain(
            ~domain=CoqExport.Reals,
            numerator_proof.raw_derivative,
          );
        let denominator_derivative =
          CoqExport.string_of_d_for_domain(
            ~domain=CoqExport.Reals,
            denominator_proof.raw_derivative,
          );
        let raw_derivative =
          DifferentiationRewrite.divide_exp(
            DifferentiationRewrite.minus_exp(
              DifferentiationRewrite.times_exp(
                numerator_proof.raw_derivative,
                denominator,
              ),
              DifferentiationRewrite.times_exp(
                numerator,
                denominator_proof.raw_derivative,
              ),
            ),
            DifferentiationRewrite.power_exp(
              denominator,
              DifferentiationRewrite.int_exp(2),
            ),
          );
        let standard_derivative =
          DifferentiationRewrite.divide_exp(
            DifferentiationRewrite.minus_exp(
              DifferentiationRewrite.times_exp(
                numerator_proof.raw_derivative,
                denominator,
              ),
              DifferentiationRewrite.times_exp(
                denominator_proof.raw_derivative,
                numerator,
              ),
            ),
            DifferentiationRewrite.power_exp(
              denominator,
              DifferentiationRewrite.int_exp(2),
            ),
          );
        let raw_derivative_string =
          CoqExport.string_of_d_for_domain(
            ~domain=CoqExport.Reals,
            raw_derivative,
          );
        let standard_derivative_string =
          CoqExport.string_of_d_for_domain(
            ~domain=CoqExport.Reals,
            standard_derivative,
          );
        let numerator_string =
          CoqExport.string_of_d_for_domain(
            ~domain=CoqExport.Reals,
            numerator,
          );
        Some({
          raw_derivative,
          proof:
            "replace ("
            ++ raw_derivative_string
            ++ ") with ("
            ++ standard_derivative_string
            ++ ").\n{ eapply (derivable_pt_lim_div "
            ++ derivative_function(~variable, numerator)
            ++ " "
            ++ derivative_function(~variable, denominator)
            ++ " "
            ++ variable
            ++ " ("
            ++ numerator_derivative
            ++ ") ("
            ++ denominator_derivative
            ++ ")).\n"
            ++ proof_block(numerator_proof.proof)
            ++ "\n"
            ++ proof_block(denominator_proof.proof)
            ++ "\n{ assumption. } }\n"
            ++ "{ rewrite (Rmult_comm ("
            ++ numerator_string
            ++ ") ("
            ++ denominator_derivative
            ++ ")); reflexivity. }",
          nonzero_denominators:
            numerator_proof.nonzero_denominators
            @ denominator_proof.nonzero_denominators
            @ [denominator],
        });
      | _ => None
      }
    | BinOp(op, base, exponent)
        when DifferentiationRewrite.is_operator(Operators.Power, op) =>
      switch (
        DifferentiationRewrite.integer_constant(exponent),
        derivative_certificate_for_expression(~variable, base),
      ) {
      | (Some(power), Some(base_proof)) when power > 0 =>
        let base_at =
          CoqExport.string_of_d_for_domain(~domain=CoqExport.Reals, base);
        let base_derivative =
          CoqExport.string_of_d_for_domain(
            ~domain=CoqExport.Reals,
            base_proof.raw_derivative,
          );
        let outer_derivative =
          power == 2
            ? DifferentiationRewrite.times_exp(
                DifferentiationRewrite.int_exp(2),
                base,
              )
            : DifferentiationRewrite.times_exp(
                DifferentiationRewrite.int_exp(power),
                DifferentiationRewrite.power_exp(
                  base,
                  DifferentiationRewrite.int_exp(power - 1),
                ),
              );
        let raw_derivative =
          DifferentiationRewrite.times_exp(
            outer_derivative,
            base_proof.raw_derivative,
          );
        let raw_derivative_string =
          CoqExport.string_of_d_for_domain(
            ~domain=CoqExport.Reals,
            raw_derivative,
          );
        Some({
          raw_derivative,
          proof:
            "change (derivable_pt_lim (comp "
            ++ (
              power == 2
                ? "Rsqr" : "(fun y : R => y ^ " ++ string_of_int(power) ++ ")"
            )
            ++ " "
            ++ derivative_function(~variable, base)
            ++ ") "
            ++ variable
            ++ " ("
            ++ raw_derivative_string
            ++ ")).\n"
            ++ "eapply (derivable_pt_lim_comp "
            ++ derivative_function(~variable, base)
            ++ " "
            ++ (
              power == 2
                ? "Rsqr" : "(fun y : R => y ^ " ++ string_of_int(power) ++ ")"
            )
            ++ " "
            ++ variable
            ++ " ("
            ++ base_derivative
            ++ ") ("
            ++ (
              power == 2
                ? "2 * " ++ base_at
                : "INR "
                  ++ string_of_int(power)
                  ++ " * ("
                  ++ base_at
                  ++ ") ^ pred "
                  ++ string_of_int(power)
            )
            ++ ")).\n"
            ++ proof_block(base_proof.proof)
            ++ "\n{ apply "
            ++ (power == 2 ? "derivable_pt_lim_Rsqr" : "derivable_pt_lim_pow")
            ++ ". }",
          nonzero_denominators: base_proof.nonzero_denominators,
        });
      | _ => None
      }
    | UnOp(
        Operators.Int(Operators.Minus) | SInt(Minus) | Float(Minus),
        inner,
      ) =>
      derivative_certificate_for_expression(~variable, inner)
      |> Option.map(inner_proof =>
           {
             raw_derivative:
               DifferentiationRewrite.neg_exp(inner_proof.raw_derivative),
             proof:
               "eapply derivable_pt_lim_opp.\n"
               ++ proof_block(inner_proof.proof),
             nonzero_denominators: inner_proof.nonzero_denominators,
           }
         )
    | Ap(Operators.Forward, fn, inner) =>
      let name = DifferentiationRewrite.function_name(fn);
      switch (name, derivative_certificate_for_expression(~variable, inner)) {
      | (Some("sin"), Some(inner_proof)) =>
        let inner_at =
          CoqExport.string_of_d_for_domain(~domain=CoqExport.Reals, inner);
        let inner_derivative =
          CoqExport.string_of_d_for_domain(
            ~domain=CoqExport.Reals,
            inner_proof.raw_derivative,
          );
        let raw_derivative =
          DifferentiationRewrite.times_exp(
            DifferentiationRewrite.app_exp("cos", inner),
            inner_proof.raw_derivative,
          );
        let raw_derivative_string =
          CoqExport.string_of_d_for_domain(
            ~domain=CoqExport.Reals,
            raw_derivative,
          );
        Some({
          raw_derivative,
          proof:
            "change (derivable_pt_lim (comp sin "
            ++ derivative_function(~variable, inner)
            ++ ") "
            ++ variable
            ++ " ("
            ++ raw_derivative_string
            ++ ")).\n"
            ++ "eapply (derivable_pt_lim_comp "
            ++ derivative_function(~variable, inner)
            ++ " sin "
            ++ variable
            ++ " ("
            ++ inner_derivative
            ++ ") (cos ("
            ++ inner_at
            ++ "))).\n"
            ++ proof_block(inner_proof.proof)
            ++ "\n{ apply derivable_pt_lim_sin. }",
          nonzero_denominators: inner_proof.nonzero_denominators,
        });
      | (Some("cos"), Some(inner_proof)) =>
        let inner_at =
          CoqExport.string_of_d_for_domain(~domain=CoqExport.Reals, inner);
        let inner_derivative =
          CoqExport.string_of_d_for_domain(
            ~domain=CoqExport.Reals,
            inner_proof.raw_derivative,
          );
        let raw_derivative =
          DifferentiationRewrite.times_exp(
            DifferentiationRewrite.neg_exp(
              DifferentiationRewrite.app_exp("sin", inner),
            ),
            inner_proof.raw_derivative,
          );
        let raw_derivative_string =
          CoqExport.string_of_d_for_domain(
            ~domain=CoqExport.Reals,
            raw_derivative,
          );
        Some({
          raw_derivative,
          proof:
            "change (derivable_pt_lim (comp cos "
            ++ derivative_function(~variable, inner)
            ++ ") "
            ++ variable
            ++ " ("
            ++ raw_derivative_string
            ++ ")).\n"
            ++ "eapply (derivable_pt_lim_comp "
            ++ derivative_function(~variable, inner)
            ++ " cos "
            ++ variable
            ++ " ("
            ++ inner_derivative
            ++ ") (- sin ("
            ++ inner_at
            ++ "))).\n"
            ++ proof_block(inner_proof.proof)
            ++ "\n{ apply derivable_pt_lim_cos. }",
          nonzero_denominators: inner_proof.nonzero_denominators,
        });
      | _ => None
      };
    | _ => None
    };
  };
};

let calculus_source = source =>
  switch (DifferentiationRewrite.diff_parts(source)) {
  | Some((expression, variable)) =>
    switch (DifferentiationRewrite.variable_name(variable)) {
    | None => None
    | Some(variable_name) =>
      switch (DifferentiationRewrite.strip(expression).term) {
      | Fun(pattern, body, _, _) =>
        DifferentiationRewrite.function_parameter_name(pattern)
        == Some(variable_name)
          ? Some((DifferentiationRewrite.strip(body), variable_name)) : None
      | _ => Some((DifferentiationRewrite.strip(expression), variable_name))
      }
    }
  | None => None
  };

let calculus_cleanup_script = (~use_affine_finisher) =>
  "cbn [pred INR];\n"
  ++ "repeat first [\n"
  ++ "  progress rewrite Rplus_0_l\n"
  ++ "| progress rewrite Rplus_0_r\n"
  ++ "| progress rewrite Rmult_0_l\n"
  ++ "| progress rewrite Rmult_0_r\n"
  ++ "| progress rewrite Rmult_1_l\n"
  ++ "| progress rewrite Rmult_1_r\n"
  ++ "| progress rewrite Rminus_0_r\n"
  ++ "| progress rewrite Ropp_0\n"
  ++ "| progress rewrite pow_1\n"
  ++ "| progress rewrite pow_O\n"
  ++ "].\n"
  ++ (use_affine_finisher ? "lra." : "reflexivity.");

let calculus_search_program =
    (~profile, ~theorem_name="hazel_rocq_search", request) =>
  switch (calculus_source(request.source)) {
  | None => None
  | Some((expression, variable)) =>
    let rule_enabled = rule_id =>
      Axioms.visible_rule_enabled(profile.Axioms.step_policy, rule_id);
    let cleanup_enabled = capability =>
      List.mem(capability, profile.step_policy.default_cleanup);
    let normalized =
      DifferentiationRewrite.normalize(
        ~rule_enabled,
        ~fuel=128,
        request.source,
      );
    let expected =
      DifferentiationRewrite.cleanup(~cleanup_enabled, normalized.exp);
    let exact_target = TrigRewrite.exp_same(expected, request.target);
    let affine_target =
      RewriteChecker.rational_affine_equivalent(expected, request.target)
      && Axioms.guarded_normalization_backend_for_profile(
           profile,
           "arith.affine_normalize",
         )
      |> Option.is_some;
    if (!normalized.complete
        || DifferentiationRewrite.contains_diff(expected)
        || !exact_target
        && !affine_target) {
      None;
    } else {
      derivative_certificate_for_expression(~variable, expression)
      |> Option.map(certificate => {
           let body =
             CoqExport.string_of_d_for_domain(
               ~domain=CoqExport.Reals,
               expression,
             );
           let target =
             CoqExport.string_of_d_for_domain(
               ~domain=CoqExport.Reals,
               request.target,
             );
           let raw =
             CoqExport.string_of_d_for_domain(
               ~domain=CoqExport.Reals,
               certificate.raw_derivative,
             );
           let vars =
             [variable]
             @ CoqExport.unique_vars_in_ast(expression)
             @ CoqExport.unique_vars_in_ast(request.target)
             |> RewriteChecker.dedup;
           let hypotheses =
             certificate.nonzero_denominators
             |> List.map(denominator =>
                  CoqExport.string_of_d_for_domain(
                    ~domain=CoqExport.Reals,
                    denominator,
                  )
                  ++ " <> 0 -> "
                )
             |> RewriteChecker.dedup
             |> String.concat("");
           let function_expression =
             derivative_function(~variable, expression);
           let finish =
             if (TrigRewrite.exp_same(
                   certificate.raw_derivative,
                   request.target,
                 )) {
               "exact H_hazel_derivative.";
             } else {
               "replace ("
               ++ target
               ++ ") with ("
               ++ raw
               ++ ").\n"
               ++ "- exact H_hazel_derivative.\n"
               ++ "- "
               ++ calculus_cleanup_script(~use_affine_finisher=affine_target);
             };
           Printf.sprintf(
             "From Stdlib Require Import Rbase Rfunctions Ranalysis1 Ranalysis3 Rtrigo_reg Lra.\nOpen Scope R_scope.\n\n(* Hazel profile-directed derivative certificate. *)\nTheorem %s : forall %s : R, %sderivable_pt_lim (fun %s : R => %s) %s (%s).\nProof.\nintros.\nassert (H_hazel_derivative : derivable_pt_lim %s %s (%s)).\n{ %s }\n%s\nQed.",
             theorem_name,
             String.concat(" ", vars),
             hypotheses,
             variable,
             body,
             variable,
             target,
             function_expression,
             variable,
             raw,
             certificate.proof,
             finish,
           );
         });
    };
  };

let calculus_export_program = (source, target) => {
  let request = {
    backend: JSCoqTacticSearch,
    level: Axioms.Calculus,
    max_depth: 4,
    max_states: 80,
    source,
    target,
  };
  calculus_search_program(
    ~profile=Axioms.math_profile(Calculus),
    ~theorem_name="hazel_derivative",
    request,
  );
};

let profile_search_definitions = (~domain, ~profile, ~max_depth, request) => {
  let plan = Axioms.stage_plan_for_profile(profile, MultiStepCheck);
  let directed_finishers =
    goal_directed_finishers(~domain, ~profile, request);
  let visible_tactics =
    plan.visible_rules
    |> List.concat_map((planned: Axioms.planned_visible_rule) =>
         planned.rule.rocq_backend
         |> Option.map(rocq_backend_tactics(~domain))
         |> Option.value(~default=[])
       );
  let cleanup_tactics =
    plan.pre_cleanup
    @ plan.post_cleanup
    @ (
      plan.visible_rules
      |> List.concat_map((planned: Axioms.planned_visible_rule) =>
           planned.allowed_cleanup
         )
    )
    |> List.sort_uniq(compare)
    |> List.concat_map(cleanup =>
         Axioms.rocq_cleanup_tactics(~domain=catalog_domain(domain), cleanup)
       );
  let normalization_finishers =
    plan.normalization_backends
    |> List.filter((backend: Axioms.rocq_rule_backend) =>
         backend.mode == Axioms.FinishOnly
       )
    |> List.concat_map(rocq_backend_tactics(~domain));
  let normalization_steps =
    plan.normalization_backends
    |> List.filter((backend: Axioms.rocq_rule_backend) =>
         backend.mode != Axioms.FinishOnly
       )
    |> List.concat_map(rocq_backend_tactics(~domain));
  let max_depth = max(0, min(12, max_depth));
  let recursive_search_branches =
    visible_tactics
    @ normalization_steps
    @ cleanup_tactics
    |> RewriteChecker.dedup
    |> List.map(tactic =>
         "progress (" ++ tactic ++ "); hazel_profile_search_exact n'"
       );
  tactic_alternatives(~name="hazel_profile_direct_finish", directed_finishers)
  ++ "\n"
  ++ tactic_alternatives(~name="hazel_profile_visible_step", visible_tactics)
  ++ "\n"
  ++ tactic_alternatives(
       ~name="hazel_profile_normalization_step",
       normalization_finishers @ normalization_steps,
     )
  ++ "\n"
  ++ tactic_alternatives(~name="hazel_profile_cleanup_step", cleanup_tactics)
  ++ "\nLtac hazel_profile_search_exact n :=\n"
  ++ "  lazymatch n with\n"
  ++ "  | O => reflexivity\n"
  ++ "  | S ?n' => first [\n"
  ++ "      reflexivity\n"
  ++ "    | "
  ++ (
    switch (recursive_search_branches) {
    | [] => "fail 0 \"no enabled search branches\""
    | branches => String.concat("\n    | ", branches)
    }
  )
  ++ "\n"
  ++ "    ]\n"
  ++ "  end.\n\n"
  ++ "Ltac hazel_profile_search :=\n  first [\n    "
  ++ (
    (directed_finishers == [] ? [] : ["solve [hazel_profile_direct_finish]"])
    @ (
      normalization_finishers
      |> RewriteChecker.dedup
      |> List.map(tactic => "solve [" ++ tactic ++ "]")
    )
    @ ["hazel_profile_search_exact " ++ string_of_int(max_depth) ++ "%nat"]
    |> String.concat("\n  | ")
  )
  ++ "\n  ].\n";
};

let check_result_finisher = (~domain, level) =>
  switch (domain, level) {
  | (CoqExport.Integers, Axioms.Arithmetic) => "lia"
  | (Integers, Algebra)
  | (Integers, Trigonometry) => "hazel_integer_polynomial"
  | (Reals, Arithmetic) => "lra"
  | (Reals, Algebra)
  | (Reals, Trigonometry) => "hazel_real_algebra"
  | (_, FunctionsAndLists) => "hazel_functions"
  | (_, Calculus) => "hazel_calculus"
  };

let profile_check_result_script = "hazel_profile_search";

let equivalence_check_result_script = (~domain, profile) =>
  "first ["
  ++ check_result_finisher(~domain, profile.Axioms.level)
  ++ " | reflexivity]";

let rocq_search_program_for_profile_and_purpose_internal =
    (~profile, ~purpose, ~equivalence_fallback, request) => {
  let calculus_program =
    switch (purpose, equivalence_fallback) {
    | (Axioms.CheckResult, false) =>
      calculus_search_program(~profile, request)
    | _ => None
    };
  switch (calculus_program) {
  | Some(program) => program
  | None =>
    if (DifferentiationRewrite.contains_diff(request.source)
        || DifferentiationRewrite.contains_diff(request.target)) {
      failwith(
        "the active calculus profile cannot certify this derivative candidate",
      );
    };
    let domain = domain_for_request(request);
    let plan = rocq_plan_for_profile_and_purpose(profile, purpose);
    let directed_finishers =
      switch (purpose, equivalence_fallback) {
      | (Axioms.CheckResult, false) =>
        goal_directed_finishers(~domain, ~profile, request)
      | _ => []
      };
    let prelude =
      directed_finishers != []
        ? direct_certificate_prelude(domain)
        : (
          switch (domain) {
          | CoqExport.Reals => CoqProofExport.real_prelude
          | Integers => CoqProofExport.prelude
          }
        );
    let source = CoqExport.string_of_d_for_domain(~domain, request.source);
    let target = CoqExport.string_of_d_for_domain(~domain, request.target);
    let forall_str = forall_string(~domain, vars_for_request(request));
    let (search_definitions, tactic_script) =
      switch (purpose, equivalence_fallback) {
      | (Axioms.CheckResult, false) when directed_finishers != [] => (
          direct_certificate_definitions(directed_finishers),
          profile_check_result_script,
        )
      | (Axioms.CheckResult, false) => (
          profile_search_definitions(
            ~domain,
            ~profile,
            ~max_depth=request.max_depth,
            request,
          ),
          profile_check_result_script,
        )
      | (CheckResult, true) => (
          "",
          equivalence_check_result_script(~domain, profile),
        )
      | _ => ("", rocq_tactic_plan_script(plan))
      };
    Printf.sprintf(
      "%s\n%s\n(* Hazel Rocq tactic-search candidate. *)\nTheorem hazel_rocq_search:%s%s=%s.\nProof.\nintros.\n%s.\nQed.",
      prelude,
      search_definitions,
      forall_str,
      source,
      target,
      tactic_script,
    );
  };
};

let rocq_search_program_for_profile_and_purpose =
    (~profile, ~purpose, request) =>
  rocq_search_program_for_profile_and_purpose_internal(
    ~profile,
    ~purpose,
    ~equivalence_fallback=false,
    request,
  );

let rocq_equivalence_program_for_profile = (~profile, request) =>
  rocq_search_program_for_profile_and_purpose_internal(
    ~profile,
    ~purpose=CheckResult,
    ~equivalence_fallback=true,
    request,
  );

let rocq_search_program_for_purpose = (~purpose, request) =>
  rocq_search_program_for_profile_and_purpose(
    ~profile=effective_profile_for_request(request),
    ~purpose,
    request,
  );

let rocq_search_program = request =>
  rocq_search_program_for_purpose(~purpose=Axioms.CheckResult, request);

let macro_detail_for_plan = (plan: Axioms.rocq_tactic_plan) =>
  "JSCoq/Rocq tactic-search plan: " ++ plan.id;

let collapsed_macro_summary_for_purpose = (~purpose, request) => {
  let profile = effective_profile_for_request(request);
  let plan = rocq_plan_for_profile_and_purpose(profile, purpose);
  let group_name =
    switch (List.rev(profile.groups)) {
    | [group, ..._] => Some(group.name)
    | [] => None
    };
  let rule_id = profile.rocq_macro_rule_id;
  let step =
    RewriteChecker.{
      origin: Normalization,
      rule_id,
      before_full_exp: request.source,
      after_full_exp: request.target,
      before_exp: request.source,
      after_exp: request.target,
      occurrence: 1,
      detail: Some(macro_detail_for_plan(plan)),
    };
  RewriteChecker.{
    justification: "Rocq tactic search",
    group_name,
    from_normal_exp: request.target,
    to_normal_exp: request.target,
    from_rule_ids: [rule_id],
    to_rule_ids: [],
    rule_ids: [rule_id],
    prover_steps: [step],
    exportable: true,
  };
};

let collapsed_macro_summary = request =>
  collapsed_macro_summary_for_purpose(~purpose=Axioms.CheckResult, request);

let search = request =>
  switch (request.backend) {
  | LocalAxiomSearch => local_axiom_search(request)
  | JSCoqTacticSearch =>
    Rejected("JSCoq tactic search backend is not implemented yet")
  };

let trace_summary = outcome =>
  switch (outcome) {
  | PrimitiveTrace(summary)
  | CollapsedMacro(summary) => Some(summary)
  | Rejected(_) => None
  };

let search_trace = request => search(request) |> trace_summary;
