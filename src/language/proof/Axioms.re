[@deriving (show({with_path: false}), sexp, yojson)]
type rewrite_level =
  | Arithmetic
  | Algebra
  | Trigonometry
  | FunctionsAndLists
  | Calculus;

[@deriving (show({with_path: false}), sexp, yojson)]
type automation_stage =
  | Manual
  | MultiStepCheck
  | AutoEval;

type prover_hint = {
  prover: string,
  tactic: string,
};

type rewrite_rule = {
  id: string,
  label: string,
  prover_hints: list(prover_hint),
};

type rewrite_group = {
  name: string,
  label: string,
  level: rewrite_level,
  rank: int,
  rules: list(rewrite_rule),
};

let rewrite_levels = [
  Arithmetic,
  Algebra,
  Trigonometry,
  FunctionsAndLists,
  Calculus,
];

let automation_stages = [Manual, MultiStepCheck, AutoEval];

let rewrite_level_rank =
  fun
  | Arithmetic => 0
  | Algebra => 1
  | Trigonometry => 2
  | FunctionsAndLists => 3
  | Calculus => 4;

let rewrite_level_label =
  fun
  | Arithmetic => "Arithmetic"
  | Algebra => "Algebra"
  | Trigonometry => "Trigonometry"
  | FunctionsAndLists => "Functions/lists"
  | Calculus => "Calculus";

let rewrite_level_detail =
  fun
  | Arithmetic => "constants and affine terms"
  | Algebra => "distribution, factoring, and cancellation"
  | Trigonometry => "identities and angle rewrites"
  | FunctionsAndLists => "future: unfold, beta, map, fold"
  | Calculus => "future: derivative and limit rules";

let rewrite_level_enabled =
  fun
  | Arithmetic
  | Algebra
  | Trigonometry => true
  | FunctionsAndLists
  | Calculus => false;

let automation_stage_label =
  fun
  | Manual => "One step"
  | MultiStepCheck => "Check result"
  | AutoEval => "Auto simplify";

let automation_stage_detail =
  fun
  | Manual => "one visible Hazel step"
  | MultiStepCheck => "compare normal forms"
  | AutoEval => "apply rewrite group";

let lean = tactic => {
  prover: "lean",
  tactic,
};

let arithmetic_rewrite_group = {
  name: "arithmetic",
  label: "arithmetic",
  level: Arithmetic,
  rank: rewrite_level_rank(Arithmetic),
  rules: [
    {
      id: "arith.add_comm",
      label: "commute addition",
      prover_hints: [lean("rw [add_comm]")],
    },
    {
      id: "arith.mul_comm",
      label: "commute multiplication",
      prover_hints: [lean("rw [mul_comm]")],
    },
    {
      id: "arith.add_assoc",
      label: "associate addition",
      prover_hints: [lean("rw [add_assoc]")],
    },
    {
      id: "arith.add_zero",
      label: "remove additive identity",
      prover_hints: [lean("rw [add_zero, zero_add]")],
    },
    {
      id: "arith.add_neg",
      label: "cancel additive inverses",
      prover_hints: [lean("rw [add_left_neg, add_right_neg]")],
    },
    {
      id: "arith.const_fold",
      label: "fold constants",
      prover_hints: [lean("norm_num")],
    },
    {
      id: "arith.mul_const",
      label: "scale term by constant",
      prover_hints: [lean("rw [mul_add, add_mul, one_mul]")],
    },
    {
      id: "arith.collect_like_terms",
      label: "collect like terms",
      prover_hints: [lean("rw [← add_mul, ← mul_add]")],
    },
    {
      id: "arith.reorder_add_terms",
      label: "reorder addition terms",
      prover_hints: [lean("ac_rfl")],
    },
  ],
};

let algebra_rewrite_group = {
  name: "algebra",
  label: "algebra",
  level: Algebra,
  rank: rewrite_level_rank(Algebra),
  rules: [
    {
      id: "alg.distribute_mul_add",
      label: "distribute multiplication over addition",
      prover_hints: [lean("rw [mul_add, add_mul]")],
    },
    {
      id: "alg.factor_common",
      label: "factor common term",
      prover_hints: [lean("rw [← mul_add, ← add_mul]")],
    },
    {
      id: "alg.expand_polynomial",
      label: "expand polynomial product",
      prover_hints: [lean("rw [mul_add, add_mul]")],
    },
    {
      id: "alg.collect_like_terms",
      label: "collect polynomial terms",
      prover_hints: [lean("rw [← add_mul, ← mul_add, add_assoc]")],
    },
    {
      id: "alg.cancel_common_add",
      label: "cancel common additive term",
      prover_hints: [lean("rw [add_assoc, add_left_neg, add_right_neg]")],
    },
  ],
};

let trigonometry_rewrite_group = {
  name: "trigonometry",
  label: "trigonometry",
  level: Trigonometry,
  rank: rewrite_level_rank(Trigonometry),
  rules: [
    {
      id: "trig.pythagorean_sin_cos",
      label: "Pythagorean identity",
      prover_hints: [lean("rw [Real.sin_sq_add_cos_sq]")],
    },
    {
      id: "trig.pythagorean_cos_sin",
      label: "Pythagorean identity",
      prover_hints: [lean("rw [add_comm, Real.sin_sq_add_cos_sq]")],
    },
    {
      id: "trig.cos_squared_pythagorean",
      label: "cosine squared Pythagorean form",
      prover_hints: [lean("rw [Real.cos_sq]")],
    },
    {
      id: "trig.sin_squared_pythagorean",
      label: "sine squared Pythagorean form",
      prover_hints: [lean("rw [Real.sin_sq]")],
    },
    {
      id: "trig.tan_squared_pythagorean",
      label: "tangent squared Pythagorean form",
      prover_hints: [lean("rw [Real.one_add_tan_sq_eq_inv_cos_sq]")],
    },
    {
      id: "trig.sin_sum",
      label: "sine sum identity",
      prover_hints: [lean("rw [Real.sin_add]")],
    },
    {
      id: "trig.sin_diff",
      label: "sine difference identity",
      prover_hints: [lean("rw [Real.sin_sub]")],
    },
    {
      id: "trig.cos_sum",
      label: "cosine sum identity",
      prover_hints: [lean("rw [Real.cos_add]")],
    },
    {
      id: "trig.cos_diff",
      label: "cosine difference identity",
      prover_hints: [lean("rw [Real.cos_sub]")],
    },
    {
      id: "trig.sin_double",
      label: "sine double-angle identity",
      prover_hints: [lean("rw [Real.sin_two_mul]")],
    },
    {
      id: "trig.cos_double_square",
      label: "cosine double-angle identity",
      prover_hints: [lean("rw [Real.cos_two_mul]")],
    },
    {
      id: "trig.cos_double_cos",
      label: "cosine double-angle with cos squared",
      prover_hints: [lean("rw [Real.cos_two_mul]")],
    },
    {
      id: "trig.cos_double_sin",
      label: "cosine double-angle with sin squared",
      prover_hints: [lean("rw [Real.cos_two_mul]")],
    },
    {
      id: "trig.sin_half_squared",
      label: "sine squared half-angle identity",
      prover_hints: [lean("rw [Real.sin_sq]")],
    },
    {
      id: "trig.cos_half_squared",
      label: "cosine squared half-angle identity",
      prover_hints: [lean("rw [Real.cos_sq]")],
    },
    {
      id: "trig.sin_cofunction",
      label: "sine cofunction identity",
      prover_hints: [lean("rw [Real.sin_pi_div_two_sub]")],
    },
    {
      id: "trig.cos_cofunction",
      label: "cosine cofunction identity",
      prover_hints: [lean("rw [Real.cos_pi_div_two_sub]")],
    },
    {
      id: "trig.tan_cofunction",
      label: "tangent cofunction identity",
      prover_hints: [lean("rw [Real.tan_pi_div_two_sub]")],
    },
    {
      id: "trig.sin_pi_sub",
      label: "sine reflection identity",
      prover_hints: [lean("rw [Real.sin_pi_sub]")],
    },
    {
      id: "trig.cos_pi_sub",
      label: "cosine reflection identity",
      prover_hints: [lean("rw [Real.cos_pi_sub]")],
    },
    {
      id: "trig.tan_pi_sub",
      label: "tangent reflection identity",
      prover_hints: [lean("rw [Real.tan_pi_sub]")],
    },
    {
      id: "trig.sin_neg",
      label: "sine negative-angle identity",
      prover_hints: [lean("rw [Real.sin_neg]")],
    },
    {
      id: "trig.cos_neg",
      label: "cosine negative-angle identity",
      prover_hints: [lean("rw [Real.cos_neg]")],
    },
    {
      id: "trig.tan_neg",
      label: "tangent negative-angle identity",
      prover_hints: [lean("rw [Real.tan_neg]")],
    },
  ],
};

let rewrite_groups = [
  arithmetic_rewrite_group,
  algebra_rewrite_group,
  trigonometry_rewrite_group,
];

let allowed_groups = level => {
  let max_rank = rewrite_level_rank(level);
  rewrite_groups |> List.filter(group => group.rank <= max_rank);
};

let rewrite_group_by_name = name =>
  rewrite_groups |> List.find_opt(group => group.name == name);

let rewrite_rule_by_id = (group, id) =>
  group.rules |> List.find_opt(rule => rule.id == id);

let v: ProofCtx.t =
  []
  |> ProofCtx.add_exp(
       "Reflexive(==)",
       Forall(
         Var("x") |> Pat.fresh,
         BinOp(
           Poly(Equals),
           BinOp(
             Poly(Equals),
             Var("x") |> Exp.fresh,
             Var("x") |> Exp.fresh,
           )
           |> Exp.fresh,
           Atom(Bool(true)) |> Exp.fresh,
         )
         |> Exp.fresh,
       )
       |> Exp.fresh,
     );
