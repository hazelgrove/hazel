module Axioms = Language.Axioms;

let tactic_for_axiom = (~domain, name) => {
  let lemma =
    switch (domain) {
    | CoqExport.Reals =>
      switch (name) {
      | "Iden(+)L" => Some("Rplus_0_l")
      | "Iden(+)R" => Some("Rplus_0_r")
      | "Iden(*)L" => Some("Rmult_1_l")
      | "Iden(*)R" => Some("Rmult_1_r")
      | "Zero(*)L" => Some("Rmult_0_l")
      | "Zero(*)R" => Some("Rmult_0_r")
      | "Comm(+)" => Some("Rplus_comm")
      | "Assoc(+)" => Some("Rplus_assoc")
      | "Comm(*)" => Some("Rmult_comm")
      | "Assoc(*)" => Some("Rmult_assoc")
      | _ => None
      }
    | Integers =>
      switch (name) {
      | "Iden(+)L" => Some("Z.add_0_l")
      | "Iden(+)R" => Some("Z.add_0_r")
      | "Iden(*)L" => Some("Z.mul_1_l")
      | "Iden(*)R" => Some("Z.mul_1_r")
      | "Zero(*)L" => Some("Z.mul_0_l")
      | "Zero(*)R" => Some("Z.mul_0_r")
      | "Comm(+)" => Some("Z.add_comm")
      | "Assoc(+)" => Some("Z.add_assoc")
      | "Comm(*)" => Some("Z.mul_comm")
      | "Assoc(*)" => Some("Z.mul_assoc")
      | _ => None
      }
    };
  switch (lemma) {
  | Some(lemma) => "rewrite " ++ lemma
  | None => "cbv"
  };
};

let is_trig_rule_id = rule_id => {
  let prefix = "trig.";
  let prefix_len = String.length(prefix);
  String.length(rule_id) >= prefix_len
  && String.sub(rule_id, 0, prefix_len) == prefix;
};

let legacy_rewrite_tactics_for_rule_id = rule_id =>
  switch (rule_id) {
  | "arith.simplify_scalar_products" => []
  | _ => []
  };

let catalog_domain =
  fun
  | CoqExport.Integers => Axioms.RocqIntegers
  | Reals => RocqReals;

let rewrite_tactics_for_rule_id = (~domain=CoqExport.Reals, rule_id) =>
  switch (Axioms.catalog_rule_by_id(rule_id)) {
  | Some({rocq_backend: Some(backend), _}) =>
    Axioms.rocq_tactics_for_domain(
      ~domain=catalog_domain(domain),
      backend.replay_tactics,
    )
  | _ =>
    switch (Axioms.cleanup_capability_for_id(rule_id)) {
    | Some(capability) =>
      Axioms.rocq_cleanup_tactics(~domain=catalog_domain(domain), capability)
    | None => legacy_rewrite_tactics_for_rule_id(rule_id)
    }
  };

let tactic_script = tactics =>
  tactics |> List.map(tactic => tactic ++ ".") |> String.concat("\n");

let tactic_sequence_script = tactics =>
  switch (tactics) {
  | [] => "idtac."
  | tactics => String.concat("; ", tactics) ++ "."
  };

let cut_name = index => "H_hazel_step_" ++ string_of_int(index);

let tactic_for_prover_step = (~domain, step: RewriteChecker.prover_step) => {
  switch (Axioms.rocq_tactic_group_for_macro_rule_id(step.rule_id)) {
  | Some(tactic_group) => tactic_script([tactic_group])
  | None =>
    switch (step.rule_id) {
    | "rocq.tactic_search" =>
      tactic_script([
        "first [hazel_power_normalize | hazel_rewrite_search 8%nat | hazel_mul_reorder | reflexivity]",
      ])
    | "arith.simplify_scalar_products" => tactic_script(["hazel_algebra"])
    | _ =>
      rewrite_tactics_for_rule_id(~domain, step.rule_id)
      @ ["cbn", "reflexivity"]
      |> tactic_sequence_script
    }
  };
};

let recorded_transition_replay_script =
    (~domain, steps: list(RewriteChecker.prover_step)) => {
  let same_recorded_transition =
      (left: RewriteChecker.prover_step, right: RewriteChecker.prover_step) =>
    Language.Exp.fast_equal(left.before_full_exp, right.before_full_exp)
    && Language.Exp.fast_equal(left.after_full_exp, right.after_full_exp);
  let rec take_transition_group = (first, grouped, remaining) =>
    switch (remaining) {
    | [step, ...rest] when same_recorded_transition(first, step) =>
      take_transition_group(first, grouped @ [step], rest)
    | _ => (grouped, remaining)
    };
  let rec transition_groups = steps =>
    switch (steps) {
    | [] => []
    | [first, ...rest] =>
      let (grouped, remaining) =
        take_transition_group(first, [first], rest);
      [grouped, ...transition_groups(remaining)];
    };
  let polynomial_transition_replay = group => {
    let includes_algebra_identity =
      group
      |> List.exists((step: RewriteChecker.prover_step) =>
           List.mem(step.rule_id, Axioms.algebra_identity_rule_ids)
         );
    let includes_factoring =
      group
      |> List.exists((step: RewriteChecker.prover_step) =>
           step.rule_id == "alg.factor_common"
         );
    let includes_distribution_family =
      group
      |> List.exists((step: RewriteChecker.prover_step) =>
           List.mem(
             step.rule_id,
             [
               "arith.mul_const",
               "alg.distribute_mul_add",
               "alg.factor_common",
             ],
           )
         );
    let is_direct_distribution_transition =
      switch (group) {
      | [step, ..._] =>
        RewriteChecker.distribute_mul_over_add_candidates(step.before_exp)
        |> List.exists(candidate =>
             Language.Exp.fast_equal(candidate, step.after_exp)
           )
        || RewriteChecker.distribute_mul_over_add_candidates(step.after_exp)
        |> List.exists(candidate =>
             Language.Exp.fast_equal(candidate, step.before_exp)
           )
      | [] => false
      };
    if (includes_algebra_identity) {
      /* Named polynomial identities have already been authorized by the
         active profile. Use a deterministic certificate here, not as a
         broader search fallback. JSCoq's first ring invocation can overflow
         on symbolic subtraction, whereas nra is stable over real goals. */
      Some(
        switch (domain) {
        | CoqExport.Reals => "unfold Rsqr; nra."
        | Integers => "ring."
        },
      );
    } else if (includes_distribution_family
               && (includes_factoring || !is_direct_distribution_transition)) {
      /* Factoring may collect and reorder several polynomial terms, so a
         single reverse-distribution rewrite is not a complete certificate.
         This is exact replay of a profile-authorized transition, not a search
         fallback; nra is used because JSCoq's first ring invocation is not
         stable for all multivariable real polynomials. */
      Some(
        switch (domain) {
        | CoqExport.Reals => "unfold Rsqr; nra."
        | Integers => "ring."
        },
      );
    } else if (!includes_distribution_family) {
      None;
    } else {
      let rewrites =
        switch (domain) {
        | CoqExport.Reals => [
            "rewrite Rmult_plus_distr_l",
            "rewrite Rmult_plus_distr_r",
            "rewrite <- Rmult_plus_distr_l",
            "rewrite <- Rmult_plus_distr_r",
          ]
        | Integers => [
            "rewrite Z.mul_add_distr_l",
            "rewrite Z.mul_add_distr_r",
            "rewrite <- Z.mul_add_distr_l",
            "rewrite <- Z.mul_add_distr_r",
          ]
        };
      Some("first [" ++ String.concat(" | ", rewrites) ++ "]; reflexivity.");
    };
  };
  let tactic_for_transition_group = group =>
    switch (polynomial_transition_replay(group)) {
    | Some(tactic) => tactic
    | None =>
      switch (group) {
      | [step] => tactic_for_prover_step(~domain, step)
      | [_, ..._] =>
        group
        |> List.concat_map((step: RewriteChecker.prover_step) =>
             rewrite_tactics_for_rule_id(~domain, step.rule_id)
           )
        |> (
          tactics =>
            tactics @ ["cbn", "reflexivity"] |> tactic_sequence_script
        )
      | [] => "idtac."
      }
    };
  let assert_for_group = (index, group) => {
    let first: RewriteChecker.prover_step = List.hd(group);
    Printf.sprintf(
      "assert (%s : %s = %s).\n{ %s }",
      cut_name(index),
      CoqExport.string_of_d_for_domain(~domain, first.before_full_exp),
      CoqExport.string_of_d_for_domain(~domain, first.after_full_exp),
      tactic_for_transition_group(group),
    );
  };
  let groups = transition_groups(steps);
  switch (groups) {
  | [group] =>
    /* A single recorded transition already has the theorem's complete source
       and target. Replaying its certificate directly avoids constructing and
       then immediately eliminating a redundant equality proof. Besides being
       smaller, it avoids unnecessary intermediate proof terms in JSCoq. */
    tactic_for_transition_group(group)
  | _ =>
    let replay =
      groups
      |> List.mapi((index, _) => cut_name(index + 1))
      |> List.rev
      |> List.map(name => "try rewrite <- " ++ name ++ ".")
      |> String.concat("\n");
    (
      groups
      |> List.mapi((index, group) => assert_for_group(index + 1, group))
      |> String.concat("\n")
    )
    ++ "\n"
    ++ replay
    ++ "\nreflexivity.";
  };
};

/* Every exported Hazel proof uses one stable numeric semantics. */
let domain_for_summary = (_summary: RewriteChecker.trace_summary) => CoqExport.Reals;

let tactic_group_for_summary = (summary: RewriteChecker.trace_summary) =>
  switch (
    summary.rule_ids
    |> List.filter_map(Axioms.rocq_tactic_group_for_macro_rule_id)
  ) {
  | [tactic_group, ..._] => Some(tactic_group)
  | [] =>
    switch (summary.group_name) {
    | Some(group_name) =>
      Axioms.math_profile_for_group_name(group_name)
      |> Option.map(profile => profile.Axioms.rocq_tactic_group)
    | None => None
    }
  };

let is_rocq_tactic_search_rule_id =
  fun
  | "rocq.tactic_search" => true
  | rule_id => Axioms.is_rocq_macro_rule_id(rule_id);

let summary_uses_rocq_tactic_search = (summary: RewriteChecker.trace_summary) =>
  summary.rule_ids
  |> List.exists(is_rocq_tactic_search_rule_id)
  || summary.prover_steps
  |> List.exists((step: RewriteChecker.prover_step) =>
       is_rocq_tactic_search_rule_id(step.rule_id)
     );

let tactics_for_summary = (~domain, summary: RewriteChecker.trace_summary) => {
  let recorded_tactics =
    summary.rule_ids
    |> List.concat_map(rule_id =>
         rewrite_tactics_for_rule_id(~domain, rule_id)
       );
  switch (tactic_group_for_summary(summary)) {
  | Some(tactic_group) => recorded_tactics @ [tactic_group]
  | None =>
    recorded_tactics
    @ ["cbn", "first [hazel_rewrite_search 8%nat | reflexivity]"]
  };
};

let tactic_for_symbolic_arithmetic_summary =
    (summary: RewriteChecker.trace_summary) =>
  switch (summary.group_name) {
  | Some("arithmetic") => "(* Temporary affine fallback while symbolic normalization emits finer local breadcrumbs. *)\nlia."
  | _ =>
    let domain = domain_for_summary(summary);
    tactics_for_summary(~domain, summary) |> tactic_sequence_script;
  };

let tactic_for_written_summary = (~forall_str as _, ~domain, summary) => {
  switch (summary.RewriteChecker.prover_steps) {
  | [] => tactics_for_summary(~domain, summary) |> tactic_script
  | _ =>
    summary_uses_rocq_tactic_search(summary)
      ? tactics_for_summary(~domain, summary) |> tactic_script
      : summary.exportable
          ? recorded_transition_replay_script(~domain, summary.prover_steps)
          : tactic_for_symbolic_arithmetic_summary(summary)
  };
};

let tactic_for_axiom_step = (~domain=CoqExport.Reals, name) =>
  [
    tactic_for_axiom(~domain, name),
    "cbn",
    "first [hazel_rewrite_search 6%nat | reflexivity]",
  ]
  |> tactic_script;

let default_tactic =
  ["cbn", "first [hazel_rewrite_search 6%nat | reflexivity]"] |> tactic_script;

let default_tactic_for_domain = domain =>
  switch (domain) {
  | CoqExport.Reals => tactic_script(["hazel_arithmetic"])
  | CoqExport.Integers => default_tactic
  };

/* Exact trace replay normally needs standard lemmas and decision procedures,
   not Hazel's complete library of search Ltac definitions. */
let exact_replay_prelude = domain =>
  switch (domain) {
  | CoqExport.Integers => "From Stdlib Require Import ZArith Lia Ring.\nOpen Scope Z_scope.\n"
  | Reals =>
    "From Stdlib Require Import Rbase Rfunctions Rtrigo1 Cos_plus Lra Ring.\n"
    ++ "Open Scope R_scope.\n"
  };

let list_comment = (label, values) =>
  switch (values) {
  | [] => ""
  | values => "(* " ++ label ++ ": " ++ String.concat(", ", values) ++ " *)\n"
  };

let prover_step_origin =
  fun
  | RewriteChecker.ManualRewrite => "manual rewrite"
  | Normalization => "normalization"
  | AutoEvaluation => "auto evaluation";

let prover_step_comment = (~domain, index, step: RewriteChecker.prover_step) =>
  Printf.sprintf(
    "(* Hazel prover step %d: %s (%s), occurrence %d%s\n   local: %s -> %s\n   whole: %s -> %s *)\n",
    index,
    step.rule_id,
    prover_step_origin(step.origin),
    step.occurrence,
    switch (step.detail) {
    | Some(detail) => "\n   detail: " ++ detail
    | None => ""
    },
    CoqExport.string_of_d_for_domain(~domain, step.before_exp),
    CoqExport.string_of_d_for_domain(~domain, step.after_exp),
    CoqExport.string_of_d_for_domain(~domain, step.before_full_exp),
    CoqExport.string_of_d_for_domain(~domain, step.after_full_exp),
  );

let prover_steps_comment = (~domain, steps) =>
  steps
  |> List.mapi((index, step) =>
       prover_step_comment(~domain, index + 1, step)
     )
  |> String.concat("");

let written_trace_comment = (summary: RewriteChecker.trace_summary) => {
  let domain = domain_for_summary(summary);
  "(* Hazel written step: "
  ++ RewriteChecker.trace_summary_label(summary)
  ++ " *)\n"
  ++ (
    switch (summary.group_name) {
    | Some(group_name) => "(* Hazel rewrite group: " ++ group_name ++ " *)\n"
    | None => ""
    }
  )
  ++ list_comment("Hazel applied rule ids", summary.rule_ids)
  ++ list_comment("From-side rule ids", summary.from_rule_ids)
  ++ list_comment("To-side rule ids", summary.to_rule_ids)
  ++ prover_steps_comment(~domain, summary.prover_steps)
  ++ (
    summary.exportable
      ? "(* Export policy: replay Hazel prover steps. Coarse normalizer steps are still a TODO for local-fragment replay. *)\n"
      : "(* Export policy: non-exportable Hazel step. *)\n"
  );
};

let invocation = index =>
  Printf.sprintf("try rewrite <- equiv_exp%d.", index);

let prelude = "(* Generated by Hazel.\n   Export policy: replay Hazel prover steps. The data model now records\n   per-rule proof steps with local and whole-expression before/after terms;\n   the next pass should generate fine local fragments for every math rule. *)\nFrom Stdlib Require Import ZArith Lia Ring.\nOpen Scope Z_scope.\n\nLtac hazel_repeat_fuel n tac :=\n  lazymatch n with\n  | O => idtac\n  | S ?n' => first [progress tac; hazel_repeat_fuel n' tac | idtac]\n  end.\n\nLtac hazel_rewrite_step :=\n  first [\n    rewrite Z.add_0_l\n  | rewrite Z.add_0_r\n  | rewrite Z.mul_0_l\n  | rewrite Z.mul_0_r\n  | rewrite Z.mul_1_l\n  | rewrite Z.mul_1_r\n  | rewrite Z.add_opp_diag_l\n  | rewrite Z.add_opp_diag_r\n  | rewrite Z.mul_add_distr_l\n  | rewrite Z.mul_add_distr_r\n  | rewrite <- Z.mul_add_distr_l\n  | rewrite <- Z.mul_add_distr_r\n  | rewrite Z.add_assoc\n  | rewrite <- Z.add_assoc\n  | rewrite Z.mul_assoc\n  | rewrite <- Z.mul_assoc\n  | rewrite Z.add_comm\n  | rewrite Z.mul_comm\n  ].\n\nLtac hazel_rewrite_search n :=\n  match n with\n  | O => reflexivity\n  | S ?n' => first [reflexivity | hazel_rewrite_step; hazel_rewrite_search n']\n  end.\n\nLtac hazel_power_normalize :=\n  cbn;\n  repeat rewrite Z.mul_1_r;\n  repeat rewrite Z.mul_1_l;\n  repeat rewrite Z.mul_assoc;\n  reflexivity.\n\nLtac hazel_integer_polynomial :=\n  repeat rewrite Z.pow_2_r;\n  first [ring | nia].\n\nLtac hazel_mul_reorder :=\n  repeat rewrite Z.mul_assoc;\n  match goal with\n  | |- ?a * ?b = ?a * ?c =>\n    replace b with c by (rewrite Z.mul_comm; reflexivity); reflexivity\n  | |- context [?a * ?b] =>\n    replace (a * b) with (b * a) by (rewrite Z.mul_comm; reflexivity); reflexivity\n  | |- _ => try rewrite Z.mul_comm; reflexivity\n  end.\n\nLtac hazel_arithmetic :=\n  first [lia | hazel_power_normalize | hazel_rewrite_search 8%nat | hazel_mul_reorder | reflexivity].\n\nLtac hazel_algebra :=\n  first [hazel_integer_polynomial | nia | lia | hazel_power_normalize | hazel_rewrite_search 10%nat | hazel_mul_reorder | reflexivity].\n\nLtac hazel_trigonometry := hazel_algebra.\n\n";

let prelude = prelude ++ "Ltac hazel_affine_arithmetic := lia.\n\n";

let prelude =
  prelude
  ++ "Ltac hazel_factor_polynomial :=\n"
  ++ "  repeat rewrite Z.pow_2_r;\n"
  ++ "  repeat rewrite Z.mul_add_distr_l;\n"
  ++ "  repeat rewrite Z.mul_add_distr_r;\n"
  ++ "  repeat rewrite Z.mul_sub_distr_l;\n"
  ++ "  repeat rewrite Z.mul_sub_distr_r;\n"
  ++ "  lia.\n\n";

let real_prelude =
  "(* Generated by Hazel.\n"
  ++ "   Export policy: replay Hazel trigonometry prover steps over real numbers.\n"
  ++ "   Some trigonometry macros use ring/lra internally and should be lowered later. *)\n"
  ++ "From Stdlib Require Import Rbase Rfunctions Rtrigo1 Cos_plus Lra Ring.\n"
  ++ "Open Scope R_scope.\n\n"
  ++ "Ltac hazel_repeat_fuel n tac :=\n"
  ++ "  lazymatch n with\n"
  ++ "  | O => idtac\n"
  ++ "  | S ?n' => first [progress tac; hazel_repeat_fuel n' tac | idtac]\n"
  ++ "  end.\n\n"
  ++ "Ltac hazel_sin_double_sum_square :=\n"
  ++ "  match goal with\n"
  ++ "  | |- sin (2 * ?x) = Rsqr (sin ?x + cos ?x) - 1 =>\n"
  ++ "    rewrite sin_2a;\n"
  ++ "    replace (Rsqr (sin x + cos x) - 1)\n"
  ++ "      with (2 * sin x * cos x + (Rsqr (sin x) + Rsqr (cos x) - 1));\n"
  ++ "    [ rewrite sin2_cos2; ring | unfold Rsqr; ring ]\n"
  ++ "  | |- Rsqr (sin ?x + cos ?x) - 1 = sin (2 * ?x) =>\n"
  ++ "    symmetry; hazel_sin_double_sum_square\n"
  ++ "  end.\n\n"
  ++ "Ltac hazel_sin_squared_double :=\n"
  ++ "  match goal with\n"
  ++ "  | |- Rsqr (sin ?x) = (1 - cos (2 * ?x)) / 2 =>\n"
  ++ "    rewrite cos_2a_sin; unfold Rsqr; lra\n"
  ++ "  | |- Rsqr (sin ?x) = (1 - cos ?y) / 2 =>\n"
  ++ "    replace y with (2 * x) by lra; hazel_sin_squared_double\n"
  ++ "  | |- (1 - cos (2 * ?x)) / 2 = Rsqr (sin ?x) =>\n"
  ++ "    symmetry; hazel_sin_squared_double\n"
  ++ "  | |- (1 - cos ?y) / 2 = Rsqr (sin ?x) =>\n"
  ++ "    symmetry; hazel_sin_squared_double\n"
  ++ "  end.\n\n"
  ++ "Ltac hazel_cos_squared_double :=\n"
  ++ "  match goal with\n"
  ++ "  | |- Rsqr (cos ?x) = ?rhs =>\n"
  ++ "    replace (Rsqr (cos x)) with ((1 + cos (2 * x)) / 2);\n"
  ++ "    [ try match goal with\n"
  ++ "      | |- ?lhs = ?rhs =>\n"
  ++ "        match lhs with\n"
  ++ "        | context [cos ?a] =>\n"
  ++ "          match rhs with\n"
  ++ "          | context [cos ?b] =>\n"
  ++ "            progress replace (cos a) with (cos b) by (f_equal; lra)\n"
  ++ "          end\n"
  ++ "        end\n"
  ++ "      end;\n"
  ++ "      field\n"
  ++ "    | assert (Hcos : cos (2 * x) = 2 * Rsqr (cos x) - 1) by (rewrite cos_2a_cos at 1; unfold Rsqr; ring);\n"
  ++ "      rewrite Hcos; unfold Rsqr; field ]\n"
  ++ "  | |- ?lhs = Rsqr (cos ?x) =>\n"
  ++ "    symmetry; hazel_cos_squared_double\n"
  ++ "  end.\n\n"
  ++ "Ltac hazel_sin_half_squared :=\n"
  ++ "  match goal with\n"
  ++ "  | |- Rsqr (sin (?x / 2)) = (1 - cos ?x) / 2 =>\n"
  ++ "    replace x with (2 * (x / 2)) by lra;\n"
  ++ "    replace (2 * (x / 2) / 2) with (x / 2) by lra;\n"
  ++ "    rewrite cos_2a_sin; unfold Rsqr; lra\n"
  ++ "  | |- (1 - cos ?x) / 2 = Rsqr (sin (?x / 2)) =>\n"
  ++ "    symmetry; hazel_sin_half_squared\n"
  ++ "  end.\n\n"
  ++ "Ltac hazel_cos_half_squared :=\n"
  ++ "  match goal with\n"
  ++ "  | |- Rsqr (cos (?x / 2)) = (1 + cos ?x) / 2 =>\n"
  ++ "    replace x with (2 * (x / 2)) by lra;\n"
  ++ "    replace (2 * (x / 2) / 2) with (x / 2) by lra;\n"
  ++ "    rewrite cos_2a_cos; unfold Rsqr; lra\n"
  ++ "  | |- (1 + cos ?x) / 2 = Rsqr (cos (?x / 2)) =>\n"
  ++ "    symmetry; hazel_cos_half_squared\n"
  ++ "  end.\n\n"
  ++ "Ltac hazel_cos_pi_sub :=\n"
  ++ "  match goal with\n"
  ++ "  | |- cos (PI - ?x) = - cos ?x =>\n"
  ++ "    rewrite cos_minus; rewrite sin_PI; rewrite cos_PI; ring\n"
  ++ "  | |- - cos ?x = cos (PI - ?x) =>\n"
  ++ "    symmetry; hazel_cos_pi_sub\n"
  ++ "  end.\n\n"
  ++ "Ltac hazel_pythagorean :=\n"
  ++ "  first [\n"
  ++ "    rewrite sin2_cos2; reflexivity\n"
  ++ "  | rewrite Rplus_comm; rewrite sin2_cos2; reflexivity\n"
  ++ "  ].\n\n"
  ++ "Ltac hazel_trig_identity_context :=\n"
  ++ "  match goal with\n"
  ++ "  | |- context [Rsqr (sin ?x)] =>\n"
  ++ "    replace (Rsqr (sin x)) with ((1 - cos (2 * x)) / 2) by hazel_sin_squared_double\n"
  ++ "  | |- context [(1 - cos (2 * ?x)) / 2] =>\n"
  ++ "    replace ((1 - cos (2 * x)) / 2) with (Rsqr (sin x)) by hazel_sin_squared_double\n"
  ++ "  | |- context [Rsqr (cos ?x)] =>\n"
  ++ "    replace (Rsqr (cos x)) with ((1 + cos (2 * x)) / 2) by hazel_cos_squared_double\n"
  ++ "  | |- context [(1 + cos (2 * ?x)) / 2] =>\n"
  ++ "    replace ((1 + cos (2 * x)) / 2) with (Rsqr (cos x)) by hazel_cos_squared_double\n"
  ++ "  end.\n\n"
  ++ "Ltac hazel_trig_argument_algebra :=\n"
  ++ "  first [\n"
  ++ "  match goal with\n"
  ++ "  | |- context [sin (2 * (2 * ?x))] =>\n"
  ++ "    replace (sin (2 * (2 * x))) with (sin (4 * x)) by (f_equal; lra)\n"
  ++ "  | |- context [sin (4 * ?x)] =>\n"
  ++ "    replace (sin (4 * x)) with (sin (2 * (2 * x))) by (f_equal; lra)\n"
  ++ "  | |- context [cos (2 * (2 * ?x))] =>\n"
  ++ "    replace (cos (2 * (2 * x))) with (cos (4 * x)) by (f_equal; lra)\n"
  ++ "  | |- context [cos (4 * ?x)] =>\n"
  ++ "    replace (cos (4 * x)) with (cos (2 * (2 * x))) by (f_equal; lra)\n"
  ++ "  end\n"
  ++ "  | match goal with\n"
  ++ "  | |- ?lhs = ?rhs =>\n"
  ++ "    match lhs with\n"
  ++ "    | context [sin ?a] =>\n"
  ++ "      match rhs with\n"
  ++ "      | context [sin ?b] =>\n"
  ++ "        progress replace (sin a) with (sin b) by (f_equal; lra)\n"
  ++ "      end\n"
  ++ "    | context [cos ?a] =>\n"
  ++ "      match rhs with\n"
  ++ "      | context [cos ?b] =>\n"
  ++ "        progress replace (cos a) with (cos b) by (f_equal; lra)\n"
  ++ "      end\n"
  ++ "    end\n"
  ++ "  end\n"
  ++ "  ].\n\n"
  ++ "Ltac hazel_rewrite_step :=\n"
  ++ "  first [\n"
  ++ "    rewrite sin_plus\n"
  ++ "  | rewrite sin_minus\n"
  ++ "  | rewrite cos_plus\n"
  ++ "  | rewrite cos_minus\n"
  ++ "  | hazel_sin_double_sum_square\n"
  ++ "  | hazel_sin_squared_double\n"
  ++ "  | hazel_cos_squared_double\n"
  ++ "  | hazel_sin_half_squared\n"
  ++ "  | hazel_cos_half_squared\n"
  ++ "  | hazel_cos_pi_sub\n"
  ++ "  | hazel_trig_identity_context\n"
  ++ "  | hazel_trig_argument_algebra\n"
  ++ "  | rewrite sin_2a\n"
  ++ "  | rewrite cos_2a\n"
  ++ "  | rewrite cos_2a_cos\n"
  ++ "  | rewrite cos_2a_sin\n"
  ++ "  | rewrite sin2_cos2\n"
  ++ "  | rewrite cos2\n"
  ++ "  | rewrite sin2\n"
  ++ "  | rewrite Rplus_comm\n"
  ++ "  | rewrite Rmult_comm\n"
  ++ "  | rewrite Rplus_assoc\n"
  ++ "  | rewrite <- Rplus_assoc\n"
  ++ "  | rewrite Rmult_assoc\n"
  ++ "  | rewrite <- Rmult_assoc\n"
  ++ "  ].\n\n"
  ++ "Ltac hazel_rewrite_search n :=\n"
  ++ "  match n with\n"
  ++ "  | O => reflexivity\n"
  ++ "  | S ?n' => first [reflexivity | hazel_rewrite_step; hazel_rewrite_search n']\n"
  ++ "  end.\n\n"
  ++ "Ltac hazel_rational_square_collect :=\n"
  ++ "  rewrite Rsqr_div';\n"
  ++ "  rewrite Rsqr_minus;\n"
  ++ "  unfold Rsqr, Rdiv;\n"
  ++ "  cbn;\n"
  ++ "  rewrite Rinv_mult;\n"
  ++ "  match goal with\n"
  ++ "  | |- 2 * (?a * (/ 2 * / 2)) = _ =>\n"
  ++ "    rewrite <- (Rmult_assoc 2 a (/ 2 * / 2));\n"
  ++ "    rewrite (Rmult_comm 2 a);\n"
  ++ "    rewrite (Rmult_assoc a 2 (/ 2 * / 2));\n"
  ++ "    rewrite <- (Rmult_assoc 2 (/ 2) (/ 2))\n"
  ++ "  end;\n"
  ++ "  rewrite Rinv_r by lra;\n"
  ++ "  repeat rewrite Rmult_1_l;\n"
  ++ "  repeat rewrite Rmult_1_r;\n"
  ++ "  unfold Rminus;\n"
  ++ "  repeat rewrite Rmult_plus_distr_r;\n"
  ++ "  repeat rewrite Rmult_1_l;\n"
  ++ "  repeat rewrite Rmult_1_r;\n"
  ++ "  match goal with\n"
  ++ "  | |- context [-(2 * ?c) * / 2] =>\n"
  ++ "    rewrite Ropp_mult_distr_l_reverse;\n"
  ++ "    replace ((2 * c) * / 2) with c by\n"
  ++ "      (rewrite Rmult_assoc;\n"
  ++ "       rewrite (Rmult_comm c (/ 2));\n"
  ++ "       rewrite <- Rmult_assoc;\n"
  ++ "       rewrite Rinv_r by lra;\n"
  ++ "       rewrite Rmult_1_l;\n"
  ++ "       reflexivity)\n"
  ++ "  end;\n"
  ++ "  match goal with\n"
  ++ "  | |- context [(?c * ?c) * / 2] =>\n"
  ++ "    rewrite (Rmult_comm (c * c) (/ 2))\n"
  ++ "  end;\n"
  ++ "  match goal with\n"
  ++ "  | |- (?a + ?b) + ?c = _ =>\n"
  ++ "    rewrite (Rplus_assoc a b c);\n"
  ++ "    rewrite (Rplus_comm b c);\n"
  ++ "    rewrite <- (Rplus_assoc a c b)\n"
  ++ "  end;\n"
  ++ "  reflexivity.\n\n"
  ++ "Ltac hazel_rational_square_normalize :=\n"
  ++ "  first [\n"
  ++ "    lazymatch goal with\n"
  ++ "    | |- context [Rsqr (?numerator / ?denominator)] =>\n"
  ++ "      unfold Rsqr; field\n"
  ++ "    end\n"
  ++ "  | lazymatch goal with\n"
  ++ "    | |- context [((?left - ?right) * (?left - ?right)) / ?denominator] =>\n"
  ++ "      unfold Rsqr; field\n"
  ++ "    end\n"
  ++ "  | hazel_rational_square_collect\n"
  ++ "  ].\n\n"
  ++ "Ltac hazel_power_normalize :=\n"
  ++ "  cbn;\n"
  ++ "  try unfold Rsqr;\n"
  ++ "  repeat rewrite Rmult_1_r;\n"
  ++ "  repeat rewrite Rmult_1_l;\n"
  ++ "  repeat rewrite Rmult_assoc;\n"
  ++ "  reflexivity.\n\n"
  ++ "Ltac hazel_real_algebra :=\n"
  ++ "  try unfold Rsqr;\n"
  ++ "  first [ring | field; try lra | lra].\n\n"
  ++ "Ltac hazel_trig_context_solve :=\n"
  ++ "  hazel_trig_identity_context;\n"
  ++ "  try hazel_trig_argument_algebra;\n"
  ++ "  first [hazel_real_algebra | reflexivity | hazel_rewrite_search 6%nat].\n\n"
  ++ "Ltac hazel_mul_reorder :=\n"
  ++ "  repeat rewrite Rmult_assoc;\n"
  ++ "  match goal with\n"
  ++ "  | |- ?a * ?b = ?a * ?c =>\n"
  ++ "    replace b with c by (rewrite Rmult_comm; reflexivity); reflexivity\n"
  ++ "  | |- context [?a * ?b] =>\n"
  ++ "    replace (a * b) with (b * a) by (rewrite Rmult_comm; reflexivity); reflexivity\n"
  ++ "  | |- _ => try rewrite Rmult_comm; reflexivity\n"
  ++ "  end.\n\n"
  ++ "Ltac hazel_arithmetic :=\n"
  ++ "  first [lra | hazel_real_algebra | hazel_power_normalize | hazel_rewrite_search 8%nat | hazel_mul_reorder | reflexivity].\n\n"
  ++ "Ltac hazel_algebra :=\n"
  ++ "  first [hazel_power_normalize | hazel_real_algebra | hazel_rewrite_search 10%nat | hazel_mul_reorder | lra | reflexivity].\n\n"
  ++ "Ltac hazel_trigonometry :=\n"
  ++ "  first [hazel_pythagorean | hazel_trig_argument_algebra; hazel_algebra | hazel_sin_half_squared | hazel_cos_half_squared | hazel_cos_pi_sub | hazel_sin_squared_double | hazel_cos_squared_double | hazel_sin_double_sum_square | hazel_algebra | hazel_power_normalize | hazel_trig_context_solve | hazel_rewrite_search 12%nat | hazel_mul_reorder | hazel_real_algebra | reflexivity].\n\n";

let real_prelude = real_prelude ++ "Ltac hazel_affine_arithmetic := lra.\n\n";
