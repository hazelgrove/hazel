let tactic_for_axiom = name =>
  switch (name) {
  | "Iden(+)L" => "rewrite Z.add_0_l"
  | "Iden(+)R" => "rewrite Z.add_0_r"
  | "Iden(*)L" => "rewrite Z.mul_1_l"
  | "Iden(*)R" => "rewrite Z.mul_1_r"
  | "Zero(*)L" => "rewrite Z.mul_0_l"
  | "Zero(*)R" => "rewrite Z.mul_0_r"
  | "Comm(+)" => "rewrite Z.add_comm"
  | "Assoc(+)" => "rewrite Z.add_assoc"
  | "Comm(*)" => "rewrite Z.mul_comm"
  | "Assoc(*)" => "rewrite Z.mul_assoc"
  | _ => "cbv"
  };

let is_trig_rule_id = rule_id => {
  let prefix = "trig.";
  let prefix_len = String.length(prefix);
  String.length(rule_id) >= prefix_len
  && String.sub(rule_id, 0, prefix_len) == prefix;
};

let rewrite_tactics_for_rule_id = (~domain=CoqExport.Integers, rule_id) =>
  switch (domain, rule_id) {
  | (CoqExport.Reals, "arith.add_comm") => ["try rewrite Rplus_comm"]
  | (CoqExport.Reals, "arith.mul_comm") => ["try rewrite Rmult_comm"]
  | (CoqExport.Reals, "arith.mul_assoc") => [
      "try rewrite Rmult_assoc",
      "try rewrite <- Rmult_assoc",
    ]
  | (CoqExport.Reals, "arith.reorder_mul_factors") => []
  | (CoqExport.Reals, "trig.sin_sum") => ["try rewrite sin_plus"]
  | (CoqExport.Reals, "trig.sin_diff") => ["try rewrite sin_minus"]
  | (CoqExport.Reals, "trig.cos_sum") => ["try rewrite cos_plus"]
  | (CoqExport.Reals, "trig.cos_diff") => ["try rewrite cos_minus"]
  | (CoqExport.Reals, "trig.sin_double") => ["try rewrite sin_2a"]
  | (CoqExport.Reals, "trig.cos_double_square") => [
      "try rewrite cos_2a",
      "try unfold Rsqr",
    ]
  | (CoqExport.Reals, "trig.cos_double_cos") => [
      "try rewrite cos_2a_cos",
      "try unfold Rsqr",
      "try rewrite <- Rmult_assoc",
    ]
  | (CoqExport.Reals, "trig.cos_double_sin") => [
      "try rewrite cos_2a_sin",
      "try unfold Rsqr",
      "try rewrite <- Rmult_assoc",
    ]
  | (CoqExport.Reals, "trig.pythagorean_sin_cos") => [
      "try rewrite sin2_cos2",
    ]
  | (CoqExport.Reals, "trig.pythagorean_cos_sin") => [
      "try rewrite Rplus_comm",
      "try rewrite sin2_cos2",
    ]
  | (CoqExport.Reals, "trig.cos_squared_pythagorean") => ["try rewrite cos2"]
  | (CoqExport.Reals, "trig.sin_squared_pythagorean") => ["try rewrite sin2"]
  | (CoqExport.Reals, "trig.sin_cofunction") => ["try rewrite sin_shift"]
  | (CoqExport.Reals, "trig.cos_cofunction") => ["try rewrite cos_shift"]
  | (CoqExport.Reals, "trig.sin_pi_sub") => ["try rewrite sin_PI_x"]
  | (CoqExport.Reals, "trig.sin_neg") => ["try rewrite sin_neg"]
  | (CoqExport.Reals, "trig.cos_neg") => ["try rewrite cos_neg"]
  | (CoqExport.Reals, "trig.tan_neg") => ["try rewrite tan_neg"]
  | (CoqExport.Reals, "alg.power_add") => [
      "cbn",
      "try unfold Rsqr",
      "repeat rewrite Rmult_1_r",
      "repeat rewrite Rmult_1_l",
      "repeat rewrite Rmult_assoc",
    ]
  | (CoqExport.Reals, "alg.power_mul") => [
      "cbn",
      "try unfold Rsqr",
      "repeat rewrite Rmult_1_r",
      "repeat rewrite Rmult_1_l",
      "repeat rewrite Rmult_assoc",
    ]
  | (CoqExport.Reals, _) => []
  | (_, "arith.add_comm") => ["try rewrite Z.add_comm"]
  | (_, "arith.mul_comm") => ["try rewrite Z.mul_comm"]
  | (_, "arith.add_assoc") => ["repeat rewrite Z.add_assoc"]
  | (_, "arith.mul_assoc") => [
      "try rewrite Z.mul_assoc",
      "try rewrite <- Z.mul_assoc",
    ]
  | (_, "arith.add_zero") => [
      "repeat rewrite Z.add_0_l",
      "repeat rewrite Z.add_0_r",
    ]
  | (_, "arith.add_neg") => [
      "repeat rewrite Z.add_opp_diag_l",
      "repeat rewrite Z.add_opp_diag_r",
    ]
  | (_, "arith.const_fold")
  | (_, "arith.mul_const") => ["cbn"]
  | (_, "arith.collect_like_terms") => []
  | (_, "arith.reorder_add_terms") => []
  | (_, "arith.reorder_mul_factors") => []
  | (_, "alg.distribute_mul_add") => [
      "repeat rewrite Z.mul_add_distr_l",
      "repeat rewrite Z.mul_add_distr_r",
    ]
  | (_, "alg.factor_common") => [
      "repeat rewrite <- Z.mul_add_distr_l",
      "repeat rewrite <- Z.mul_add_distr_r",
    ]
  | (_, "alg.expand_polynomial") => [
      "repeat rewrite Z.mul_add_distr_l",
      "repeat rewrite Z.mul_add_distr_r",
      "repeat rewrite Z.add_assoc",
      "cbn",
    ]
  | (_, "alg.power_add") => [
      "cbn",
      "repeat rewrite Z.mul_1_r",
      "repeat rewrite Z.mul_1_l",
      "repeat rewrite Z.mul_assoc",
    ]
  | (_, "alg.power_mul") => [
      "cbn",
      "repeat rewrite Z.mul_1_r",
      "repeat rewrite Z.mul_1_l",
      "repeat rewrite Z.mul_assoc",
    ]
  | (_, "alg.collect_like_terms")
  | (_, "alg.cancel_common_add") => []
  | _ => []
  };

let tactic_script = tactics =>
  tactics |> List.map(tactic => tactic ++ ".") |> String.concat("\n");

let cut_name = index => "H_hazel_step_" ++ string_of_int(index);

let tactic_for_prover_step = (~domain, step: RewriteChecker.prover_step) =>
  switch (step.rule_id) {
  | "arith.reorder_add_terms" => tactic_script(["hazel_rewrite_search 8%nat"])
  | "arith.reorder_mul_factors" => tactic_script(["hazel_mul_reorder"])
  | "arith.collect_like_terms"
  | "alg.collect_like_terms"
  | "alg.cancel_common_add" =>
    tactic_script(["first [hazel_rewrite_search 10%nat | reflexivity]"])
  | _ =>
    rewrite_tactics_for_rule_id(~domain, step.rule_id)
    @ ["cbn", "reflexivity"]
    |> tactic_script
  };

let assertion_replay_script =
    (~domain, steps: list(RewriteChecker.prover_step)) => {
  let assert_for_step = (index, step: RewriteChecker.prover_step) =>
    Printf.sprintf(
      "assert (%s : %s = %s).\n{ %s }",
      cut_name(index),
      CoqExport.string_of_d_for_domain(~domain, step.before_full_exp),
      CoqExport.string_of_d_for_domain(~domain, step.after_full_exp),
      tactic_for_prover_step(~domain, step),
    );
  let replay =
    steps
    |> List.mapi((index, _) => cut_name(index + 1))
    |> List.rev
    |> List.map(name => "try rewrite <- " ++ name ++ ".")
    |> String.concat("\n");
  (
    steps
    |> List.mapi((index, step) => assert_for_step(index + 1, step))
    |> String.concat("\n")
  )
  ++ "\n"
  ++ replay
  ++ "\nreflexivity.";
};

let prover_step_requires_reals = (step: RewriteChecker.prover_step) =>
  CoqExport.requires_reals(step.before_full_exp)
  || CoqExport.requires_reals(step.after_full_exp)
  || CoqExport.requires_reals(step.before_exp)
  || CoqExport.requires_reals(step.after_exp);

let domain_for_summary = (summary: RewriteChecker.trace_summary) =>
  summary.rule_ids
  |> List.exists(is_trig_rule_id)
  || CoqExport.requires_reals(summary.from_normal_exp)
  || CoqExport.requires_reals(summary.to_normal_exp)
  || summary.prover_steps
  |> List.exists(prover_step_requires_reals)
    ? CoqExport.Reals : CoqExport.Integers;

let tactics_for_summary = (~domain, summary: RewriteChecker.trace_summary) => {
  let recorded_tactics =
    summary.rule_ids
    |> List.concat_map(rule_id =>
         rewrite_tactics_for_rule_id(~domain, rule_id)
       );
  recorded_tactics
  @ ["cbn", "first [hazel_rewrite_search 8%nat | reflexivity]"];
};

let tactic_for_symbolic_arithmetic_summary =
    (summary: RewriteChecker.trace_summary) =>
  switch (summary.group_name) {
  | Some("arithmetic") => "(* Temporary affine fallback while symbolic normalization emits finer local breadcrumbs. *)\nlia."
  | _ =>
    let domain = domain_for_summary(summary);
    tactics_for_summary(~domain, summary) |> tactic_script;
  };

let tactic_for_written_summary = (~forall_str, summary) => {
  let domain = domain_for_summary(summary);
  switch (summary.RewriteChecker.prover_steps) {
  | [] => tactics_for_summary(~domain, summary) |> tactic_script
  | _ =>
    (domain == CoqExport.Reals || forall_str == "") && summary.exportable
      ? assertion_replay_script(~domain, summary.prover_steps)
      : tactic_for_symbolic_arithmetic_summary(summary)
  };
};

let tactic_for_axiom_step = name =>
  [
    tactic_for_axiom(name),
    "cbn",
    "first [hazel_rewrite_search 6%nat | reflexivity]",
  ]
  |> tactic_script;

let default_tactic =
  ["cbn", "first [hazel_rewrite_search 6%nat | reflexivity]"] |> tactic_script;

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

let prelude = "(* Generated by Hazel.\n   Export policy: replay Hazel prover steps. The data model now records\n   per-rule proof steps with local and whole-expression before/after terms;\n   the next pass should generate fine local fragments for every math rule.\n   No ring tactic is used. *)\nFrom Stdlib Require Import ZArith Lia.\nOpen Scope Z_scope.\n\nLtac hazel_rewrite_step :=\n  first [\n    rewrite Z.add_0_l\n  | rewrite Z.add_0_r\n  | rewrite Z.mul_0_l\n  | rewrite Z.mul_0_r\n  | rewrite Z.mul_1_l\n  | rewrite Z.mul_1_r\n  | rewrite Z.add_opp_diag_l\n  | rewrite Z.add_opp_diag_r\n  | rewrite Z.mul_add_distr_l\n  | rewrite Z.mul_add_distr_r\n  | rewrite <- Z.mul_add_distr_l\n  | rewrite <- Z.mul_add_distr_r\n  | rewrite Z.add_assoc\n  | rewrite <- Z.add_assoc\n  | rewrite Z.mul_assoc\n  | rewrite <- Z.mul_assoc\n  | rewrite Z.add_comm\n  | rewrite Z.mul_comm\n  ].\n\nLtac hazel_rewrite_search n :=\n  match n with\n  | O => reflexivity\n  | S ?n' => first [reflexivity | hazel_rewrite_step; hazel_rewrite_search n']\n  end.\n\nLtac hazel_mul_reorder :=\n  repeat rewrite Z.mul_assoc;\n  match goal with\n  | |- ?a * ?b = ?a * ?c =>\n    replace b with c by (rewrite Z.mul_comm; reflexivity); reflexivity\n  | |- context [?a * ?b] =>\n    replace (a * b) with (b * a) by (rewrite Z.mul_comm; reflexivity); reflexivity\n  | |- _ => try rewrite Z.mul_comm; reflexivity\n  end.\n\n";

let real_prelude = "(* Generated by Hazel.\n   Export policy: replay Hazel trigonometry prover steps over real numbers.\n   No ring tactic is used. *)\nFrom Stdlib Require Import Rbase Rfunctions Rtrigo1 Cos_plus.\nOpen Scope R_scope.\n\nLtac hazel_rewrite_step :=\n  first [\n    rewrite Rplus_comm\n  | rewrite Rmult_comm\n  | rewrite Rplus_assoc\n  | rewrite <- Rplus_assoc\n  | rewrite Rmult_assoc\n  | rewrite <- Rmult_assoc\n  | rewrite sin_plus\n  | rewrite sin_minus\n  | rewrite cos_plus\n  | rewrite cos_minus\n  | rewrite sin_2a\n  | rewrite cos_2a\n  | rewrite cos_2a_cos\n  | rewrite cos_2a_sin\n  | rewrite sin2_cos2\n  | rewrite cos2\n  | rewrite sin2\n  ].\n\nLtac hazel_rewrite_search n :=\n  match n with\n  | O => reflexivity\n  | S ?n' => first [reflexivity | hazel_rewrite_step; hazel_rewrite_search n']\n  end.\n\nLtac hazel_mul_reorder :=\n  repeat rewrite Rmult_assoc;\n  match goal with\n  | |- ?a * ?b = ?a * ?c =>\n    replace b with c by (rewrite Rmult_comm; reflexivity); reflexivity\n  | |- context [?a * ?b] =>\n    replace (a * b) with (b * a) by (rewrite Rmult_comm; reflexivity); reflexivity\n  | |- _ => try rewrite Rmult_comm; reflexivity\n  end.\n\n";
