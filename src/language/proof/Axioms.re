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

type rocq_domain_policy =
  | IntegersByDefault
  | RealsByDefault;

type rocq_tactic_mode =
  | Once
  | TryOnce
  | RepeatUntilStuck
  | RepeatFuel(int)
  | FinishOnly;

type rocq_tactic_step = {
  id: string,
  label: string,
  tactic: string,
  mode: rocq_tactic_mode,
  rule_ids: list(string),
};

type rocq_tactic_plan = {
  id: string,
  label: string,
  steps: list(rocq_tactic_step),
};

type rocq_tactic_plan_purpose =
  | ValidatePrimitiveStep
  | ValidateMacroStep
  | CheckResult
  | AutoSimplify;

type cleanup_capability =
  | AddAssoc
  | AddComm
  | MulAssoc
  | MulComm
  | AddIdentity
  | MulIdentity
  | ConstFold
  | DerivativeBasics
  | PowerIdentity
  | PowerNotation
  | CollectLikeTerms;

type operation_metadata = {
  id: string,
  name: string,
  short_name: string,
  example: string,
};

type visible_step_mode =
  | VisibleOnce
  | VisibleRepeatFuel(int)
  | VisibleRepeatUntilStuck;

type visible_rule_policy = {
  rule_id: string,
  metadata: operation_metadata,
  mode: visible_step_mode,
  allowed_cleanup: list(cleanup_capability),
};

type step_policy = {
  visible_rules: list(visible_rule_policy),
  default_cleanup: list(cleanup_capability),
};

type math_rule_kind =
  | VisibleRule
  | CleanupRule
  | NormalizationRule
  | GuardedNormalizationRule
  | TacticOnlyRule;

type math_rule_direction =
  | Forward
  | Backward
  | BothDirections;

type hazel_rule_backend =
  | ArithmeticAddComm
  | ArithmeticConstFold
  | ArithmeticMulConst
  | ArithmeticMulIdentity
  | AlgebraIdentity
  | AlgebraDistributeMulAdd
  | AlgebraFactorCommon
  | AlgebraCancelCommonAdd
  | TrigIdentity
  | CalculusDerivative;

type rocq_domain =
  | RocqIntegers
  | RocqReals;

type rocq_domain_tactics = {
  integers: list(string),
  reals: list(string),
};

type rocq_rule_backend = {
  tactic: string,
  mode: rocq_tactic_mode,
  search_tactics: rocq_domain_tactics,
  replay_tactics: rocq_domain_tactics,
};

type rocq_cleanup_backend = {
  capability: cleanup_capability,
  search_tactics: rocq_domain_tactics,
};

type math_rule = {
  id: string,
  metadata: operation_metadata,
  level: rewrite_level,
  kind: math_rule_kind,
  direction: math_rule_direction,
  hazel_backend: option(hazel_rule_backend),
  rocq_backend: option(rocq_rule_backend),
  introduced_levels: list(rewrite_level),
  visible_mode: visible_step_mode,
  allowed_cleanup: list(cleanup_capability),
  required_cleanup: list(cleanup_capability),
  required_rule_ids: list(string),
};

type semantic_proof_atom_kind =
  | VisibleAtom
  | CleanupAtom
  | CheckNormalizerAtom;

type semantic_proof_atom = {
  id: string,
  kind: semantic_proof_atom_kind,
  mode: rocq_tactic_mode,
  required_cleanup: list(cleanup_capability),
  required_rule_ids: list(string),
};

type planned_visible_rule = {
  rule: math_rule,
  mode: visible_step_mode,
  allowed_cleanup: list(cleanup_capability),
};

type stage_plan = {
  stage: automation_stage,
  atoms: list(semantic_proof_atom),
  pre_cleanup: list(cleanup_capability),
  visible_rules: list(planned_visible_rule),
  post_cleanup: list(cleanup_capability),
  normalization_backends: list(rocq_rule_backend),
  rocq_plan: rocq_tactic_plan,
};

type distribution_step_policy =
  | StrictDistributedForm
  | DistributionMaySimplify;

type one_step_policy = {
  distribution_step_policy,
  allow_polynomial_expansion: bool,
};

type math_profile = {
  level: rewrite_level,
  rank: int,
  label: string,
  detail: string,
  enabled: bool,
  groups: list(rewrite_group),
  one_step_policy,
  step_policy,
  check_result_rule_ids: list(string),
  rocq_macro_rule_id: string,
  rocq_tactic_group: string,
  rocq_tactic_plan,
  rocq_tactic_plans: list((rocq_tactic_plan_purpose, rocq_tactic_plan)),
  rocq_domain_policy,
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

/* Math levels form a directed acyclic graph. The numeric rank above is only
   presentation order; capability inheritance must always use this graph. */
let rewrite_level_parents =
  fun
  | Arithmetic => []
  | Algebra => [Arithmetic]
  | Trigonometry => [Algebra]
  | FunctionsAndLists => [Algebra]
  | Calculus => [Trigonometry];

let rec collect_rewrite_level_ancestors = (visited, level) =>
  List.mem(level, visited)
    ? visited
    : rewrite_level_parents(level)
      |> List.fold_left(collect_rewrite_level_ancestors, [level, ...visited]);

let inherited_rewrite_levels = level => {
  let ancestors = collect_rewrite_level_ancestors([], level);
  rewrite_levels |> List.filter(level => List.mem(level, ancestors));
};

let rewrite_level_inherits = (~current_level, required_level) =>
  inherited_rewrite_levels(current_level) |> List.mem(required_level);

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
  | Calculus => "differentiation rules over real expressions";

let rewrite_level_enabled =
  fun
  | Arithmetic
  | Algebra
  | Trigonometry
  | Calculus => true
  | FunctionsAndLists => false;

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

type construct_requirement = {
  construct: string,
  required_level: rewrite_level,
  exp_id: Id.t,
};

let required_level_allows = (~current_level, required_level) =>
  rewrite_level_inherits(~current_level, required_level);

let is_float_pi = value => abs_float(value -. Float.pi) < 0.000001;

let is_trig_builtin = name =>
  switch (name) {
  | "sin"
  | "cos"
  | "tan" => true
  | _ => false
  };

let is_calculus_builtin = name => name == "diff";

let require = (construct, required_level, exp) => {
  construct,
  required_level,
  exp_id: Exp.rep_id(exp),
};

let requirement_at_exp = (requirement, exp) => {
  ...requirement,
  exp_id: Exp.rep_id(exp),
};

let rec construct_requirements = exp => {
  let exp = exp |> DHExp.strip_ascriptions;
  switch (exp.term) {
  | Var("pi") => [require("pi", Trigonometry, exp)]
  | Var(name) when is_calculus_builtin(name) => [
      require(name, Calculus, exp),
    ]
  | Var(_) => [require("variables", Algebra, exp)]
  | BuiltinFun(name) when is_trig_builtin(name) => [
      require(name, Trigonometry, exp),
    ]
  | BuiltinFun(name) when is_calculus_builtin(name) => [
      require(name, Calculus, exp),
    ]
  | BuiltinFun(name) => [require(name, FunctionsAndLists, exp)]
  | Atom(Float(value)) when is_float_pi(value) => [
      require("pi", Trigonometry, exp),
    ]
  | BinOp(_, left, right) =>
    construct_requirements(left) @ construct_requirements(right)
  | UnOp(_, inner)
  | Parens(inner)
  | Asc(inner, _) => construct_requirements(inner)
  | Ap(_, fn, arg) =>
    let requirements =
      construct_requirements(fn) @ construct_requirements(arg);
    let call_requirements =
      requirements
      |> List.filter(requirement =>
           requirement.required_level == Trigonometry
           || requirement.required_level == Calculus
           || requirement.required_level == FunctionsAndLists
         )
      |> List.map(requirement => requirement_at_exp(requirement, exp));
    call_requirements @ requirements;
  | _ => []
  };
};

let dedup_requirements = requirements =>
  requirements
  |> List.fold_left(
       (acc, requirement) =>
         acc
         |> List.exists(existing =>
              existing.construct == requirement.construct
              && existing.required_level == requirement.required_level
            )
           ? acc : [requirement, ...acc],
       [],
     )
  |> List.rev;

let unsupported_constructs = (~level, exps) =>
  exps
  |> List.concat_map(construct_requirements)
  |> List.filter(requirement =>
       !
         required_level_allows(
           ~current_level=level,
           requirement.required_level,
         )
     )
  |> dedup_requirements;

let unsupported_construct_ids = (~level, exps) =>
  exps
  |> List.concat_map(construct_requirements)
  |> List.filter(requirement =>
       !
         required_level_allows(
           ~current_level=level,
           requirement.required_level,
         )
     )
  |> List.fold_left(
       (acc, requirement) =>
         List.mem(requirement.exp_id, acc)
           ? acc : [requirement.exp_id, ...acc],
       [],
     )
  |> List.rev;

let maximal_required_levels = requirements => {
  let levels =
    rewrite_levels
    |> List.filter(level =>
         requirements
         |> List.exists(requirement => requirement.required_level == level)
       );
  levels
  |> List.filter(level =>
       !(
         levels
         |> List.exists(other_level =>
              other_level != level
              && rewrite_level_inherits(~current_level=other_level, level)
            )
       )
     );
};

let unsupported_constructs_message_from_requirements = requirements =>
  switch (maximal_required_levels(requirements)) {
  | [] => None
  | levels =>
    Some(
      "Needs "
      ++ (levels |> List.map(rewrite_level_label) |> String.concat(" and ")),
    )
  };

let unsupported_constructs_message = (~level, exps) =>
  unsupported_constructs(~level, exps)
  |> unsupported_constructs_message_from_requirements;

let operation_fingerprint = op => Operators.bin_op_to_string(op);

let application_direction_fingerprint = dir =>
  Operators.show_ap_direction(dir);

let rec exp_fingerprint = exp => {
  let exp = exp |> DHExp.strip_ascriptions;
  switch (exp.term) {
  | Parens(inner)
  | Asc(inner, _) => exp_fingerprint(inner)
  | Atom(Int(value))
  | Atom(Nat(value)) => "int:" ++ Bigint.to_string(value)
  | Atom(SInt(value)) => "int:" ++ string_of_int(value)
  | Atom(Float(value)) => "float:" ++ string_of_float(value)
  | Var(name) => "var:" ++ name
  | BuiltinFun(name) => "builtin:" ++ name
  | BinOp(op, left, right) =>
    "bin:"
    ++ operation_fingerprint(op)
    ++ "("
    ++ exp_fingerprint(left)
    ++ ","
    ++ exp_fingerprint(right)
    ++ ")"
  | UnOp(op, inner) =>
    "un:" ++ Operators.show_op_un(op) ++ "(" ++ exp_fingerprint(inner) ++ ")"
  | Ap(dir, fn, arg) =>
    "ap:"
    ++ application_direction_fingerprint(dir)
    ++ "("
    ++ exp_fingerprint(fn)
    ++ ","
    ++ exp_fingerprint(arg)
    ++ ")"
  | _ =>
    Exp.show_cls(Exp.cls_of_term(exp.term))
    ++ ":"
    ++ Id.to_string(Exp.rep_id(exp))
  };
};

let trig_function_name = exp => {
  let exp = exp |> DHExp.strip_ascriptions;
  switch (exp.term) {
  | Var(("sin" | "cos" | "tan") as name)
  | BuiltinFun(("sin" | "cos" | "tan") as name) => Some(name)
  | _ => None
  };
};

let rec trig_application_fingerprints = exp => {
  let exp = exp |> DHExp.strip_ascriptions;
  let children =
    switch (exp.term) {
    | BinOp(_, left, right)
    | Ap(_, left, right) =>
      trig_application_fingerprints(left)
      @ trig_application_fingerprints(right)
    | UnOp(_, inner)
    | Parens(inner)
    | Asc(inner, _)
    | Projector(_, inner) => trig_application_fingerprints(inner)
    | _ => []
    };
  switch (exp.term) {
  | Ap(Operators.Forward, fn, arg) =>
    switch (trig_function_name(fn)) {
    | Some(name) => [
        name ++ "(" ++ exp_fingerprint(arg) ++ ")",
        ...children,
      ]
    | None => children
    }
  | _ => children
  };
};

let sorted_unique_strings = values =>
  values |> List.sort_uniq(String.compare);

let trig_applications_preserved = (source, target) =>
  sorted_unique_strings(trig_application_fingerprints(source))
  == sorted_unique_strings(trig_application_fingerprints(target));

let is_trig_construct_requirement = requirement =>
  is_trig_builtin(requirement.construct);

let can_treat_trig_applications_as_opaque = (~level, source, target) =>
  rewrite_level_inherits(~current_level=level, Algebra)
  && trig_applications_preserved(source, target);

let unsupported_construct_requirements_for_rewrite =
    (~level, ~source, ~target) => {
  let unsupported = unsupported_constructs(~level, [source, target]);
  can_treat_trig_applications_as_opaque(~level, source, target)
    ? unsupported
      |> List.filter(requirement =>
           !is_trig_construct_requirement(requirement)
         )
    : unsupported;
};

let unsupported_constructs_for_rewrite = unsupported_construct_requirements_for_rewrite;

let unsupported_construct_ids_for_rewrite = (~level, ~source, ~target) =>
  unsupported_construct_requirements_for_rewrite(~level, ~source, ~target)
  |> List.fold_left(
       (acc, requirement) =>
         List.mem(requirement.exp_id, acc)
           ? acc : [requirement.exp_id, ...acc],
       [],
     )
  |> List.rev;

let unsupported_constructs_message_for_rewrite = (~level, ~source, ~target) =>
  unsupported_construct_requirements_for_rewrite(~level, ~source, ~target)
  |> unsupported_constructs_message_from_requirements;

let export_level_for_rewrite = (~requested_level, source, target) =>
  switch (requested_level) {
  | Trigonometry =>
    trig_applications_preserved(source, target) ? Algebra : Trigonometry
  | Calculus => Calculus
  | level => level
  };

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
      id: "arith.mul_assoc",
      label: "associate multiplication",
      prover_hints: [lean("rw [mul_assoc]")],
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
      id: "arith.mul_identity",
      label: "remove multiplicative identity",
      prover_hints: [lean("rw [one_mul, mul_one]")],
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
    {
      id: "arith.reorder_mul_factors",
      label: "reorder multiplication factors",
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
      id: "alg.power_add",
      label: "split power over exponent addition",
      prover_hints: [lean("rw [pow_add]")],
    },
    {
      id: "alg.power_mul",
      label: "split power over exponent multiplication",
      prover_hints: [lean("rw [pow_mul]")],
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
    {
      id: "alg.difference_of_squares",
      label: "difference of squares",
      prover_hints: [lean("ring")],
    },
    {
      id: "alg.square_of_sum",
      label: "square of a sum",
      prover_hints: [lean("ring")],
    },
    {
      id: "alg.square_of_difference",
      label: "square of a difference",
      prover_hints: [lean("ring")],
    },
    {
      id: "alg.difference_of_cubes",
      label: "difference of cubes",
      prover_hints: [lean("ring")],
    },
    {
      id: "alg.sum_of_cubes",
      label: "sum of cubes",
      prover_hints: [lean("ring")],
    },
    {
      id: "alg.cube_of_sum",
      label: "cube of a sum",
      prover_hints: [lean("ring")],
    },
    {
      id: "alg.cube_of_difference",
      label: "cube of a difference",
      prover_hints: [lean("ring")],
    },
  ],
};

let algebra_identity_rule_ids = [
  "alg.difference_of_squares",
  "alg.square_of_sum",
  "alg.square_of_difference",
  "alg.difference_of_cubes",
  "alg.sum_of_cubes",
  "alg.cube_of_sum",
  "alg.cube_of_difference",
];

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
      id: "trig.sin_double_sum_square",
      label: "sine double-angle sum-square form",
      prover_hints: [lean("ring_nf")],
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
      id: "trig.sin_squared_double",
      label: "sine squared double-angle form",
      prover_hints: [lean("rw [Real.cos_two_mul]")],
    },
    {
      id: "trig.cos_squared_double",
      label: "cosine squared double-angle form",
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

let calculus_rewrite_group = {
  name: "calculus",
  label: "calculus",
  level: Calculus,
  rank: rewrite_level_rank(Calculus),
  rules: [
    {
      id: "calc.diff_function",
      label: "differentiate a named function body",
      prover_hints: [lean("simp")],
    },
    {
      id: "calc.diff_constant",
      label: "derivative of a constant",
      prover_hints: [lean("fun_prop")],
    },
    {
      id: "calc.diff_variable",
      label: "derivative of the variable",
      prover_hints: [lean("fun_prop")],
    },
    {
      id: "calc.diff_sum",
      label: "linearity over addition",
      prover_hints: [lean("fun_prop")],
    },
    {
      id: "calc.diff_difference",
      label: "linearity over subtraction",
      prover_hints: [lean("fun_prop")],
    },
    {
      id: "calc.diff_negation",
      label: "linearity over negation",
      prover_hints: [lean("fun_prop")],
    },
    {
      id: "calc.diff_product",
      label: "product rule",
      prover_hints: [lean("fun_prop")],
    },
    {
      id: "calc.diff_quotient",
      label: "quotient rule (denominator nonzero)",
      prover_hints: [lean("fun_prop")],
    },
    {
      id: "calc.diff_power",
      label: "power rule",
      prover_hints: [lean("fun_prop")],
    },
    {
      id: "calc.diff_chain",
      label: "chain rule",
      prover_hints: [lean("fun_prop")],
    },
    {
      id: "calc.diff_chain_sin",
      label: "sine chain rule",
      prover_hints: [lean("fun_prop")],
    },
    {
      id: "calc.diff_chain_cos",
      label: "cosine chain rule",
      prover_hints: [lean("fun_prop")],
    },
  ],
};

let rewrite_groups = [
  arithmetic_rewrite_group,
  algebra_rewrite_group,
  trigonometry_rewrite_group,
  calculus_rewrite_group,
];

let allowed_groups = level => {
  rewrite_groups
  |> List.filter((group: rewrite_group) =>
       rewrite_level_inherits(~current_level=level, group.level)
     );
};

let rocq_tactic_step = (~id, ~label, ~tactic, ~mode, ~rule_ids) => {
  id,
  label,
  tactic,
  mode,
  rule_ids,
};

let rocq_tactic_mode_label =
  fun
  | Once => "once"
  | TryOnce => "try_once"
  | RepeatUntilStuck => "repeat_until_stuck"
  | RepeatFuel(fuel) => "repeat_fuel:" ++ string_of_int(fuel)
  | FinishOnly => "finish_only";

let rocq_tactic_plan_purpose_label =
  fun
  | ValidatePrimitiveStep => "validate_primitive_step"
  | ValidateMacroStep => "validate_macro_step"
  | CheckResult => "check_result"
  | AutoSimplify => "auto_simplify";

let distribution_step_policy_label =
  fun
  | StrictDistributedForm => "strict_distributed_form"
  | DistributionMaySimplify => "distribution_may_simplify";

let cleanup_capability_label =
  fun
  | AddAssoc => "add.assoc"
  | AddComm => "add.comm"
  | MulAssoc => "mul.assoc"
  | MulComm => "mul.comm"
  | AddIdentity => "add.identity"
  | MulIdentity => "mul.identity"
  | ConstFold => "const.fold"
  | DerivativeBasics => "derivative.basics"
  | PowerIdentity => "power.identity"
  | PowerNotation => "power.notation"
  | CollectLikeTerms => "collect.like_terms";

let cleanup_capability_for_id =
  fun
  | "add.assoc"
  | "arith.add_assoc" => Some(AddAssoc)
  | "add.comm"
  | "arith.add_comm" => Some(AddComm)
  | "mul.assoc"
  | "arith.mul_assoc" => Some(MulAssoc)
  | "mul.comm"
  | "arith.mul_comm" => Some(MulComm)
  | "add.identity"
  | "arith.add_zero" => Some(AddIdentity)
  | "mul.identity" => Some(MulIdentity)
  | "const.fold"
  | "arith.const_fold" => Some(ConstFold)
  | "derivative.basics" => Some(DerivativeBasics)
  | "power.identity" => Some(PowerIdentity)
  | "power.notation" => Some(PowerNotation)
  | "collect.like_terms"
  | "arith.collect_like_terms"
  | "alg.collect_like_terms" => Some(CollectLikeTerms)
  | _ => None;

let operation_metadata = (~id, ~name, ~short_name, ~example) => {
  id,
  name,
  short_name,
  example,
};

let cleanup_capability_metadata =
  fun
  | AddAssoc =>
    operation_metadata(
      ~id=cleanup_capability_label(AddAssoc),
      ~name="Reassociate addition",
      ~short_name="Assoc +",
      ~example="(1 + 2) + x = 1 + (2 + x)",
    )
  | AddComm =>
    operation_metadata(
      ~id=cleanup_capability_label(AddComm),
      ~name="Commute addition",
      ~short_name="Comm +",
      ~example="1 + 2 = 2 + 1",
    )
  | MulAssoc =>
    operation_metadata(
      ~id=cleanup_capability_label(MulAssoc),
      ~name="Reassociate multiplication",
      ~short_name="Assoc *",
      ~example="(2 * 3) * x = 2 * (3 * x)",
    )
  | MulComm =>
    operation_metadata(
      ~id=cleanup_capability_label(MulComm),
      ~name="Commute multiplication",
      ~short_name="Comm *",
      ~example="2 * x = x * 2",
    )
  | AddIdentity =>
    operation_metadata(
      ~id=cleanup_capability_label(AddIdentity),
      ~name="Remove additive identity",
      ~short_name="+ 0",
      ~example="x + 0 = x",
    )
  | MulIdentity =>
    operation_metadata(
      ~id=cleanup_capability_label(MulIdentity),
      ~name="Remove multiplicative identity",
      ~short_name="* 1",
      ~example="x * 1 = x",
    )
  | ConstFold =>
    operation_metadata(
      ~id=cleanup_capability_label(ConstFold),
      ~name="Fold constants",
      ~short_name="Fold",
      ~example="3 + 4 = 7",
    )
  | DerivativeBasics =>
    operation_metadata(
      ~id=cleanup_capability_label(DerivativeBasics),
      ~name="Simplify basic derivatives",
      ~short_name="Basic diff",
      ~example="diff(x, x) = 1",
    )
  | PowerIdentity =>
    operation_metadata(
      ~id=cleanup_capability_label(PowerIdentity),
      ~name="Simplify identity powers",
      ~short_name="Power id",
      ~example="x**1 = x",
    )
  | PowerNotation =>
    operation_metadata(
      ~id=cleanup_capability_label(PowerNotation),
      ~name="Use power notation",
      ~short_name="Power",
      ~example="x * x = x**2",
    )
  | CollectLikeTerms =>
    operation_metadata(
      ~id=cleanup_capability_label(CollectLikeTerms),
      ~name="Collect like terms",
      ~short_name="Collect",
      ~example="x + x = 2 * x",
    );

let visible_rule_metadata = rule_id =>
  switch (rule_id) {
  | "arith.add_comm" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Commute addition",
      ~short_name="Comm +",
      ~example="1 + 2 = 2 + 1",
    )
  | "arith.const_fold" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Evaluate constants",
      ~short_name="Eval",
      ~example="3 + 4 = 7",
    )
  | "arith.mul_const" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Distribute constant multiplication",
      ~short_name="Dist",
      ~example="2 * (3 + 4) = 2 * 3 + 2 * 4",
    )
  | "arith.mul_identity" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Remove multiplicative identity",
      ~short_name="* 1",
      ~example="1 * x = x",
    )
  | "alg.distribute_mul_add" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Distribute multiplication over addition",
      ~short_name="Dist",
      ~example="x * (a + b) = x * a + x * b",
    )
  | "alg.factor_common" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Factor a common term",
      ~short_name="Factor",
      ~example="x * a + x * b = x * (a + b)",
    )
  | "alg.cancel_common_add" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Cancel a common additive term",
      ~short_name="Cancel +",
      ~example="x + y - y = x",
    )
  | "alg.difference_of_squares" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Use the difference of squares identity",
      ~short_name="a^2-b^2",
      ~example="a**2 - b**2 = (a + b) * (a - b)",
    )
  | "alg.square_of_sum" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Expand or factor the square of a sum",
      ~short_name="(a+b)^2",
      ~example="(a + b)**2 = a**2 + 2*a*b + b**2",
    )
  | "alg.square_of_difference" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Expand or factor the square of a difference",
      ~short_name="(a-b)^2",
      ~example="(a - b)**2 = a**2 - 2*a*b + b**2",
    )
  | "alg.difference_of_cubes" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Use the difference of cubes identity",
      ~short_name="a^3-b^3",
      ~example="a**3 - b**3 = (a - b) * (a**2 + a*b + b**2)",
    )
  | "alg.sum_of_cubes" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Use the sum of cubes identity",
      ~short_name="a^3+b^3",
      ~example="a**3 + b**3 = (a + b) * (a**2 - a*b + b**2)",
    )
  | "alg.cube_of_sum" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Expand or factor the cube of a sum",
      ~short_name="(a+b)^3",
      ~example="(a + b)**3 = a**3 + 3*a**2*b + 3*a*b**2 + b**3",
    )
  | "alg.cube_of_difference" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Expand or factor the cube of a difference",
      ~short_name="(a-b)^3",
      ~example="(a - b)**3 = a**3 - 3*a**2*b + 3*a*b**2 - b**3",
    )
  | "calc.diff_function" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Differentiate a named function body",
      ~short_name="Function",
      ~example="diff(fun f(x) -> x**2, x) = diff(x**2, x)",
    )
  | "calc.diff_constant" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Derivative of a constant",
      ~short_name="Constant",
      ~example="diff(7, x) = 0",
    )
  | "calc.diff_variable" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Derivative of the variable",
      ~short_name="Variable",
      ~example="diff(x, x) = 1",
    )
  | "calc.diff_sum" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Apply derivative linearity to a sum",
      ~short_name="Sum",
      ~example="diff(u + v, x) = diff(u, x) + diff(v, x)",
    )
  | "calc.diff_difference" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Apply derivative linearity to a difference",
      ~short_name="Difference",
      ~example="diff(u - v, x) = diff(u, x) - diff(v, x)",
    )
  | "calc.diff_negation" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Apply derivative linearity to negation",
      ~short_name="Negation",
      ~example="diff(-u, x) = -diff(u, x)",
    )
  | "calc.diff_product" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Apply the product rule",
      ~short_name="Product",
      ~example="diff(u*v, x) = diff(u, x)*v + u*diff(v, x)",
    )
  | "calc.diff_quotient" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Apply the quotient rule (denominator nonzero)",
      ~short_name="Quotient",
      ~example="v != 0: diff(u/v, x) = (diff(u,x)*v-u*diff(v,x))/v**2",
    )
  | "calc.diff_power" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Apply the power rule",
      ~short_name="Power",
      ~example="diff(u**n, x) = n*u**(n-1)*diff(u, x)",
    )
  | "calc.diff_chain" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Apply the chain rule",
      ~short_name="Chain",
      ~example="diff(f(g(x)), x) = diff(f, g(x))*diff(g(x), x)",
    )
  | "calc.diff_chain_sin" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Apply the sine chain rule",
      ~short_name="Sin chain",
      ~example="diff(sin(u), x) = cos(u)*diff(u, x)",
    )
  | "calc.diff_chain_cos" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Apply the cosine chain rule",
      ~short_name="Cos chain",
      ~example="diff(cos(u), x) = -sin(u)*diff(u, x)",
    )
  | "trig.pythagorean_sin_cos"
  | "trig.pythagorean_cos_sin" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Use the Pythagorean trigonometry identity",
      ~short_name="Pythagorean",
      ~example="sin(x)**2 + cos(x)**2 = 1",
    )
  | "trig.cos_squared_pythagorean" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Rewrite cosine squared with sine",
      ~short_name="Cos^2",
      ~example="cos(x)**2 = 1 - sin(x)**2",
    )
  | "trig.sin_squared_pythagorean" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Rewrite sine squared with cosine",
      ~short_name="Sin^2",
      ~example="sin(x)**2 = 1 - cos(x)**2",
    )
  | "trig.sin_sum" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Expand sine of a sum",
      ~short_name="Sin +",
      ~example="sin(x + y) = sin(x)*cos(y) + cos(x)*sin(y)",
    )
  | "trig.sin_diff" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Expand sine of a difference",
      ~short_name="Sin -",
      ~example="sin(x - y) = sin(x)*cos(y) - cos(x)*sin(y)",
    )
  | "trig.cos_sum" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Expand cosine of a sum",
      ~short_name="Cos +",
      ~example="cos(x + y) = cos(x)*cos(y) - sin(x)*sin(y)",
    )
  | "trig.cos_diff" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Expand cosine of a difference",
      ~short_name="Cos -",
      ~example="cos(x - y) = cos(x)*cos(y) + sin(x)*sin(y)",
    )
  | "trig.sin_double" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Use the sine double-angle identity",
      ~short_name="Sin 2x",
      ~example="sin(2*x) = 2*sin(x)*cos(x)",
    )
  | "trig.sin_double_sum_square" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Use sine double-angle sum-square form",
      ~short_name="Sin 2x sq",
      ~example="sin(2*x)**2 = 4*sin(x)**2*cos(x)**2",
    )
  | "trig.cos_double_square" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Use the cosine double-angle identity",
      ~short_name="Cos 2x",
      ~example="cos(2*x) = cos(x)**2 - sin(x)**2",
    )
  | "trig.cos_double_cos" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Use cosine double-angle with cosine squared",
      ~short_name="Cos 2x cos",
      ~example="cos(2*x) = 2*cos(x)**2 - 1",
    )
  | "trig.cos_double_sin" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Use cosine double-angle with sine squared",
      ~short_name="Cos 2x sin",
      ~example="cos(2*x) = 1 - 2*sin(x)**2",
    )
  | "trig.sin_squared_double" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Rewrite sine squared with double angle",
      ~short_name="Sin^2 half",
      ~example="sin(x)**2 = (1 - cos(2*x)) / 2",
    )
  | "trig.cos_squared_double" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Rewrite cosine squared with double angle",
      ~short_name="Cos^2 half",
      ~example="cos(x)**2 = (1 + cos(2*x)) / 2",
    )
  | "trig.sin_half_squared" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Use sine half-angle identity",
      ~short_name="Sin half",
      ~example="sin(x/2)**2 = (1 - cos(x)) / 2",
    )
  | "trig.cos_half_squared" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Use cosine half-angle identity",
      ~short_name="Cos half",
      ~example="cos(x/2)**2 = (1 + cos(x)) / 2",
    )
  | "trig.sin_cofunction" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Use sine cofunction identity",
      ~short_name="Sin cofn",
      ~example="sin(pi/2 - x) = cos(x)",
    )
  | "trig.cos_cofunction" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Use cosine cofunction identity",
      ~short_name="Cos cofn",
      ~example="cos(pi/2 - x) = sin(x)",
    )
  | "trig.sin_pi_sub" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Use sine reflection identity",
      ~short_name="Sin refl",
      ~example="sin(pi - x) = sin(x)",
    )
  | "trig.cos_pi_sub" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Use cosine reflection identity",
      ~short_name="Cos refl",
      ~example="cos(pi - x) = -cos(x)",
    )
  | "trig.sin_neg" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Use sine negative-angle identity",
      ~short_name="Sin neg",
      ~example="sin(-x) = -sin(x)",
    )
  | "trig.cos_neg" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Use cosine negative-angle identity",
      ~short_name="Cos neg",
      ~example="cos(-x) = cos(x)",
    )
  | "trig.tan_neg" =>
    operation_metadata(
      ~id=rule_id,
      ~name="Use tangent negative-angle identity",
      ~short_name="Tan neg",
      ~example="tan(-x) = -tan(x)",
    )
  | _ =>
    operation_metadata(
      ~id=rule_id,
      ~name=rule_id,
      ~short_name=rule_id,
      ~example="",
    )
  };

let visible_step_mode_label =
  fun
  | VisibleOnce => "once"
  | VisibleRepeatFuel(fuel) => "repeat_fuel:" ++ string_of_int(fuel)
  | VisibleRepeatUntilStuck => "repeat_until_stuck";

let visible_step_mode_display_label =
  fun
  | VisibleOnce => "Counts as one step"
  | VisibleRepeatFuel(fuel) =>
    "May repeat up to " ++ string_of_int(fuel) ++ " times"
  | VisibleRepeatUntilStuck => "May repeat automatically";

let visible_once_rule = (~rule_id, ~allowed_cleanup) => {
  rule_id,
  metadata: visible_rule_metadata(rule_id),
  mode: VisibleOnce,
  allowed_cleanup,
};

let ac_cleanup = [AddAssoc, AddComm, MulAssoc, MulComm];

let structural_identity_cleanup = [AddIdentity, MulIdentity];

let calculus_step_cleanup =
  structural_identity_cleanup @ [DerivativeBasics, PowerIdentity];

let calculus_step_cleanup_for_rule = rule_id =>
  List.mem(rule_id, ["calc.diff_power", "calc.diff_product"])
    ? calculus_step_cleanup : [];

let assoc_cleanup = [AddAssoc, MulAssoc];

let rocq_domain_tactics = (~integers, ~reals) => {
  integers,
  reals,
};

let rocq_rule_backend = (~tactic, ~integers, ~reals) => {
  tactic,
  mode: Once,
  search_tactics: rocq_domain_tactics(~integers, ~reals),
  replay_tactics:
    rocq_domain_tactics(
      ~integers=integers |> List.map(tactic => "try " ++ tactic),
      ~reals=reals |> List.map(tactic => "try " ++ tactic),
    ),
};

let rocq_rule_backend_with_replay =
    (~tactic, ~integers, ~reals, ~replay_integers, ~replay_reals) => {
  tactic,
  mode: Once,
  search_tactics: rocq_domain_tactics(~integers, ~reals),
  replay_tactics:
    rocq_domain_tactics(~integers=replay_integers, ~reals=replay_reals),
};

let factor_tactics = (~left_lemma, ~right_lemma) => [
  "lazymatch goal with | |- ?lhs = _ => lazymatch lhs with | context [?a * ?b + ?a * ?c] => rewrite <- "
  ++ left_lemma
  ++ " end end",
  "lazymatch goal with | |- ?lhs = _ => lazymatch lhs with | context [?a * ?b + ?a * ?c] => rewrite <- "
  ++ right_lemma
  ++ " end end",
  "lazymatch goal with | |- ?lhs = _ => lazymatch lhs with | context [?a * ?c + ?b * ?c] => rewrite <- "
  ++ left_lemma
  ++ " end end",
  "lazymatch goal with | |- ?lhs = _ => lazymatch lhs with | context [?a * ?c + ?b * ?c] => rewrite <- "
  ++ right_lemma
  ++ " end end",
];

let rocq_backend_for_rule_id =
  fun
  | "arith.mul_comm" =>
    Some(
      rocq_rule_backend(
        ~tactic="hazel_rewrite_step",
        ~integers=["rewrite Z.mul_comm"],
        ~reals=["rewrite Rmult_comm"],
      ),
    )
  | "arith.add_assoc" =>
    Some(
      rocq_rule_backend_with_replay(
        ~tactic="hazel_rewrite_step",
        ~integers=["rewrite Z.add_assoc"],
        ~reals=["rewrite Rplus_assoc"],
        ~replay_integers=["repeat rewrite Z.add_assoc"],
        ~replay_reals=["repeat rewrite Rplus_assoc"],
      ),
    )
  | "arith.mul_assoc" =>
    Some(
      rocq_rule_backend_with_replay(
        ~tactic="hazel_rewrite_step",
        ~integers=["rewrite Z.mul_assoc"],
        ~reals=["rewrite Rmult_assoc"],
        ~replay_integers=[
          "try rewrite Z.mul_assoc",
          "try rewrite <- Z.mul_assoc",
        ],
        ~replay_reals=[
          "try rewrite Rmult_assoc",
          "try rewrite <- Rmult_assoc",
        ],
      ),
    )
  | "arith.add_zero" =>
    Some(
      rocq_rule_backend_with_replay(
        ~tactic="hazel_rewrite_step",
        ~integers=["rewrite Z.add_0_l", "rewrite Z.add_0_r"],
        ~reals=["rewrite Rplus_0_l", "rewrite Rplus_0_r"],
        ~replay_integers=[
          "repeat rewrite Z.add_0_l",
          "repeat rewrite Z.add_0_r",
        ],
        ~replay_reals=[
          "repeat rewrite Rplus_0_l",
          "repeat rewrite Rplus_0_r",
        ],
      ),
    )
  | "arith.add_neg" =>
    Some(
      rocq_rule_backend_with_replay(
        ~tactic="hazel_rewrite_step",
        ~integers=["rewrite Z.add_opp_diag_l", "rewrite Z.add_opp_diag_r"],
        ~reals=[],
        ~replay_integers=[
          "repeat rewrite Z.add_opp_diag_l",
          "repeat rewrite Z.add_opp_diag_r",
        ],
        ~replay_reals=[],
      ),
    )
  | "arith.add_comm" =>
    Some(
      rocq_rule_backend(
        ~tactic="hazel_rewrite_step",
        ~integers=["rewrite Z.add_comm"],
        ~reals=["rewrite Rplus_comm"],
      ),
    )
  | "arith.const_fold" =>
    Some(
      rocq_rule_backend(
        ~tactic="hazel_arithmetic",
        ~integers=["cbn"],
        ~reals=["cbn"],
      ),
    )
  | "arith.mul_const"
  | "alg.distribute_mul_add" =>
    Some(
      rocq_rule_backend_with_replay(
        ~tactic="hazel_rewrite_step",
        ~integers=["rewrite Z.mul_add_distr_l", "rewrite Z.mul_add_distr_r"],
        ~reals=["rewrite Rmult_plus_distr_l", "rewrite Rmult_plus_distr_r"],
        ~replay_integers=[
          "first [rewrite Z.mul_add_distr_l | rewrite Z.mul_add_distr_r]",
        ],
        ~replay_reals=[
          "first [rewrite Rmult_plus_distr_l | rewrite Rmult_plus_distr_r]",
        ],
      ),
    )
  | "arith.mul_identity" =>
    Some(
      rocq_rule_backend(
        ~tactic="hazel_rewrite_step",
        ~integers=["rewrite Z.mul_1_l", "rewrite Z.mul_1_r"],
        ~reals=["rewrite Rmult_1_l", "rewrite Rmult_1_r"],
      ),
    )
  | rule_id when List.mem(rule_id, algebra_identity_rule_ids) =>
    Some(
      rocq_rule_backend_with_replay(
        ~tactic="hazel_integer_polynomial",
        ~integers=[],
        ~reals=[],
        ~replay_integers=[
          "first [rewrite <- Z.mul_add_distr_l | rewrite <- Z.mul_add_distr_r]",
        ],
        ~replay_reals=[
          "first [rewrite <- Rmult_plus_distr_l | rewrite <- Rmult_plus_distr_r]",
        ],
      ),
    )
  | "alg.expand_polynomial" =>
    Some(
      rocq_rule_backend_with_replay(
        ~tactic="hazel_rewrite_step",
        ~integers=["rewrite Z.mul_add_distr_l", "rewrite Z.mul_add_distr_r"],
        ~reals=["rewrite Rmult_plus_distr_l", "rewrite Rmult_plus_distr_r"],
        /* This macro is available only when CollectLikeTerms is enabled.
           Its exact replay may therefore use the corresponding deterministic
           polynomial certificate instead of recursive rewrite search. */
        ~replay_integers=["ring"],
        ~replay_reals=["unfold Rsqr; ring"],
      ),
    )
  | "alg.factor_common" =>
    Some(
      rocq_rule_backend_with_replay(
        ~tactic="hazel_rewrite_step",
        ~integers=
          factor_tactics(
            ~left_lemma="Z.mul_add_distr_l",
            ~right_lemma="Z.mul_add_distr_r",
          ),
        ~reals=
          factor_tactics(
            ~left_lemma="Rmult_plus_distr_l",
            ~right_lemma="Rmult_plus_distr_r",
          ),
        ~replay_integers=["ring"],
        ~replay_reals=["unfold Rsqr; ring"],
      ),
    )
  | "alg.cancel_common_add" =>
    Some(
      rocq_rule_backend(
        ~tactic="hazel_rewrite_step",
        ~integers=[
          "rewrite Z.add_simpl_r",
          "rewrite Z.add_simpl_l",
          "rewrite Z.sub_simpl_r",
          "rewrite Z.sub_add",
        ],
        ~reals=[
          "unfold Rminus; rewrite <- Rplus_assoc; rewrite Rplus_opp_r; rewrite Rplus_0_r",
          "unfold Rminus; rewrite Rplus_assoc; rewrite Rplus_opp_l; rewrite Rplus_0_l",
        ],
      ),
    )
  | "alg.power_add"
  | "alg.power_mul" => {
      let backend =
        rocq_rule_backend_with_replay(
          ~tactic="hazel_power_normalize",
          ~integers=["hazel_power_normalize"],
          ~reals=["hazel_power_normalize"],
          ~replay_integers=[
            "cbn",
            "repeat rewrite Z.mul_1_r",
            "repeat rewrite Z.mul_1_l",
            "repeat rewrite Z.mul_assoc",
          ],
          ~replay_reals=[
            "cbn",
            "try unfold Rsqr",
            "repeat rewrite Rmult_1_r",
            "repeat rewrite Rmult_1_l",
            "repeat rewrite Rmult_assoc",
          ],
        );
      Some({
        ...backend,
        mode: FinishOnly,
      });
    }
  | "arith.collect_like_terms"
  | "alg.collect_like_terms" =>
    Some(
      rocq_rule_backend_with_replay(
        ~tactic="hazel_algebra",
        ~integers=[],
        ~reals=[],
        /* Exact replay is not proof search: the normalizer has already
           certified that this enabled cleanup produced the target. */
        ~replay_integers=["lia"],
        ~replay_reals=["lra"],
      ),
    )
  | "arith.reorder_add_terms" =>
    Some(
      rocq_rule_backend_with_replay(
        ~tactic="hazel_rewrite_search 8%nat",
        ~integers=[],
        ~reals=[],
        ~replay_integers=["hazel_rewrite_search 8%nat"],
        ~replay_reals=["hazel_rewrite_search 8%nat"],
      ),
    )
  | "arith.reorder_mul_factors" =>
    Some(
      rocq_rule_backend_with_replay(
        ~tactic="hazel_mul_reorder",
        ~integers=[],
        ~reals=[],
        ~replay_integers=["hazel_mul_reorder"],
        ~replay_reals=["hazel_mul_reorder"],
      ),
    )
  | "trig.pythagorean_sin_cos"
  | "trig.pythagorean_cos_sin" =>
    Some(
      rocq_rule_backend_with_replay(
        ~tactic="hazel_pythagorean",
        ~integers=[],
        ~reals=["hazel_pythagorean"],
        ~replay_integers=[],
        ~replay_reals=["try hazel_pythagorean"],
      ),
    )
  | "trig.cos_squared_pythagorean" =>
    Some(
      rocq_rule_backend(
        ~tactic="hazel_trigonometry",
        ~integers=[],
        ~reals=["rewrite cos2"],
      ),
    )
  | "trig.sin_squared_pythagorean" =>
    Some(
      rocq_rule_backend(
        ~tactic="hazel_trigonometry",
        ~integers=[],
        ~reals=["rewrite sin2"],
      ),
    )
  | "trig.sin_sum" =>
    Some(
      rocq_rule_backend(
        ~tactic="hazel_trigonometry",
        ~integers=[],
        ~reals=["rewrite sin_plus"],
      ),
    )
  | "trig.sin_diff" =>
    Some(
      rocq_rule_backend(
        ~tactic="hazel_trigonometry",
        ~integers=[],
        ~reals=["rewrite sin_minus"],
      ),
    )
  | "trig.cos_sum" =>
    Some(
      rocq_rule_backend(
        ~tactic="hazel_trigonometry",
        ~integers=[],
        ~reals=["rewrite cos_plus"],
      ),
    )
  | "trig.cos_diff" =>
    Some(
      rocq_rule_backend(
        ~tactic="hazel_trigonometry",
        ~integers=[],
        ~reals=["rewrite cos_minus"],
      ),
    )
  | "trig.sin_double" =>
    Some(
      rocq_rule_backend(
        ~tactic="hazel_trigonometry",
        ~integers=[],
        ~reals=["rewrite sin_2a"],
      ),
    )
  | "trig.sin_double_sum_square" =>
    Some(
      rocq_rule_backend(
        ~tactic="hazel_trigonometry",
        ~integers=[],
        ~reals=["hazel_sin_double_sum_square"],
      ),
    )
  | "trig.cos_double_square" =>
    Some(
      rocq_rule_backend_with_replay(
        ~tactic="hazel_trigonometry",
        ~integers=[],
        ~reals=["rewrite cos_2a"],
        ~replay_integers=[],
        ~replay_reals=["try rewrite cos_2a", "try unfold Rsqr"],
      ),
    )
  | "trig.cos_double_cos" =>
    Some(
      rocq_rule_backend_with_replay(
        ~tactic="hazel_trigonometry",
        ~integers=[],
        ~reals=["rewrite cos_2a_cos"],
        ~replay_integers=[],
        ~replay_reals=[
          "try rewrite cos_2a_cos",
          "try unfold Rsqr",
          "try rewrite <- Rmult_assoc",
        ],
      ),
    )
  | "trig.cos_double_sin" =>
    Some(
      rocq_rule_backend_with_replay(
        ~tactic="hazel_trigonometry",
        ~integers=[],
        ~reals=["rewrite cos_2a_sin"],
        ~replay_integers=[],
        ~replay_reals=[
          "try rewrite cos_2a_sin",
          "try unfold Rsqr",
          "try rewrite <- Rmult_assoc",
        ],
      ),
    )
  | "trig.sin_squared_double" =>
    Some(
      rocq_rule_backend(
        ~tactic="hazel_trigonometry",
        ~integers=[],
        ~reals=[
          "first [hazel_sin_squared_double | hazel_trig_identity_context]",
        ],
      ),
    )
  | "trig.cos_squared_double" =>
    Some(
      rocq_rule_backend(
        ~tactic="hazel_trigonometry",
        ~integers=[],
        ~reals=[
          "first [hazel_cos_squared_double | hazel_trig_identity_context]",
        ],
      ),
    )
  | "trig.sin_half_squared" =>
    Some(
      rocq_rule_backend(
        ~tactic="hazel_trigonometry",
        ~integers=[],
        ~reals=["hazel_sin_half_squared"],
      ),
    )
  | "trig.cos_half_squared" =>
    Some(
      rocq_rule_backend(
        ~tactic="hazel_trigonometry",
        ~integers=[],
        ~reals=["hazel_cos_half_squared"],
      ),
    )
  | "trig.sin_cofunction" =>
    Some(
      rocq_rule_backend(
        ~tactic="hazel_trigonometry",
        ~integers=[],
        ~reals=["rewrite sin_shift"],
      ),
    )
  | "trig.cos_cofunction" =>
    Some(
      rocq_rule_backend(
        ~tactic="hazel_trigonometry",
        ~integers=[],
        ~reals=["rewrite cos_shift"],
      ),
    )
  | "trig.sin_pi_sub" =>
    Some(
      rocq_rule_backend(
        ~tactic="hazel_trigonometry",
        ~integers=[],
        ~reals=["rewrite sin_PI_x"],
      ),
    )
  | "trig.cos_pi_sub" =>
    Some(
      rocq_rule_backend(
        ~tactic="hazel_trigonometry",
        ~integers=[],
        ~reals=["hazel_cos_pi_sub"],
      ),
    )
  | "trig.sin_neg" =>
    Some(
      rocq_rule_backend(
        ~tactic="hazel_trigonometry",
        ~integers=[],
        ~reals=["rewrite sin_neg"],
      ),
    )
  | "trig.cos_neg" =>
    Some(
      rocq_rule_backend(
        ~tactic="hazel_trigonometry",
        ~integers=[],
        ~reals=["rewrite cos_neg"],
      ),
    )
  | "trig.tan_neg" =>
    Some(
      rocq_rule_backend(
        ~tactic="hazel_trigonometry",
        ~integers=[],
        ~reals=["rewrite tan_neg"],
      ),
    )
  | "calc.diff_constant" =>
    Some(
      rocq_rule_backend(
        ~tactic="hazel_calculus",
        ~integers=[],
        ~reals=["apply derivable_pt_lim_const"],
      ),
    )
  | "calc.diff_variable" =>
    Some(
      rocq_rule_backend(
        ~tactic="hazel_calculus",
        ~integers=[],
        ~reals=["apply derivable_pt_lim_id"],
      ),
    )
  | "calc.diff_sum" =>
    Some(
      rocq_rule_backend(
        ~tactic="hazel_calculus",
        ~integers=[],
        ~reals=["apply derivable_pt_lim_plus"],
      ),
    )
  | "calc.diff_difference" =>
    Some(
      rocq_rule_backend(
        ~tactic="hazel_calculus",
        ~integers=[],
        ~reals=["apply derivable_pt_lim_minus"],
      ),
    )
  | "calc.diff_negation" =>
    Some(
      rocq_rule_backend(
        ~tactic="hazel_calculus",
        ~integers=[],
        ~reals=["apply derivable_pt_lim_opp"],
      ),
    )
  | "calc.diff_product" =>
    Some(
      rocq_rule_backend(
        ~tactic="hazel_calculus",
        ~integers=[],
        ~reals=["apply derivable_pt_lim_mult"],
      ),
    )
  | "calc.diff_quotient" =>
    Some(
      rocq_rule_backend(
        ~tactic="hazel_calculus",
        ~integers=[],
        ~reals=["apply derivable_pt_lim_div"],
      ),
    )
  | "calc.diff_chain"
  | "calc.diff_chain_sin"
  | "calc.diff_chain_cos"
  | "calc.diff_power" =>
    Some(
      rocq_rule_backend(
        ~tactic="hazel_calculus",
        ~integers=[],
        ~reals=["apply derivable_pt_lim_comp"],
      ),
    )
  | _ => None;

let rocq_tactics_for_domain = (~domain, tactics) =>
  switch (domain) {
  | RocqIntegers => tactics.integers
  | RocqReals => tactics.reals
  };

let rocq_cleanup_backend = (~capability, ~integers, ~reals) => {
  capability,
  search_tactics: rocq_domain_tactics(~integers, ~reals),
};

let rocq_cleanup_catalog = [
  rocq_cleanup_backend(
    ~capability=AddAssoc,
    ~integers=["rewrite Z.add_assoc"],
    ~reals=["rewrite Rplus_assoc"],
  ),
  rocq_cleanup_backend(
    ~capability=AddComm,
    ~integers=["rewrite Z.add_comm"],
    ~reals=["rewrite Rplus_comm"],
  ),
  rocq_cleanup_backend(
    ~capability=MulAssoc,
    ~integers=["rewrite Z.mul_assoc"],
    ~reals=["rewrite Rmult_assoc"],
  ),
  rocq_cleanup_backend(
    ~capability=MulComm,
    ~integers=["rewrite Z.mul_comm"],
    ~reals=["rewrite Rmult_comm"],
  ),
  rocq_cleanup_backend(
    ~capability=AddIdentity,
    ~integers=[
      "repeat rewrite Z.add_0_l",
      "repeat rewrite Z.add_0_r",
      "repeat rewrite Z.sub_0_r",
    ],
    ~reals=[
      "repeat rewrite Rplus_0_l",
      "repeat rewrite Rplus_0_r",
      "repeat rewrite Rminus_0_r",
      "repeat rewrite Ropp_0",
    ],
  ),
  rocq_cleanup_backend(
    ~capability=MulIdentity,
    ~integers=[
      "repeat rewrite Z.mul_0_l",
      "repeat rewrite Z.mul_0_r",
      "repeat rewrite Z.mul_1_l",
      "repeat rewrite Z.mul_1_r",
    ],
    ~reals=[
      "repeat rewrite Rmult_0_l",
      "repeat rewrite Rmult_0_r",
      "repeat rewrite Rmult_1_l",
      "repeat rewrite Rmult_1_r",
    ],
  ),
  rocq_cleanup_backend(
    ~capability=ConstFold,
    ~integers=["cbn"],
    ~reals=["cbn"],
  ),
  rocq_cleanup_backend(~capability=DerivativeBasics, ~integers=[], ~reals=[]),
  rocq_cleanup_backend(
    ~capability=PowerIdentity,
    ~integers=[],
    ~reals=["repeat rewrite pow_1", "repeat rewrite pow_O"],
  ),
  rocq_cleanup_backend(
    ~capability=PowerNotation,
    ~integers=["rewrite Z.pow_2_r"],
    ~reals=["unfold Rsqr; ring"],
  ),
  rocq_cleanup_backend(
    ~capability=CollectLikeTerms,
    ~integers=["ring"],
    ~reals=["ring"],
  ),
];

let rocq_cleanup_tactics = (~domain, capability) =>
  rocq_cleanup_catalog
  |> List.find_opt((backend: rocq_cleanup_backend) =>
       backend.capability == capability
     )
  |> Option.map((backend: rocq_cleanup_backend) =>
       rocq_tactics_for_domain(~domain, backend.search_tactics)
     )
  |> Option.value(~default=[]);

let trig_argument_normalization_backend =
  rocq_rule_backend(
    ~tactic="hazel_trig_argument_algebra",
    ~integers=[],
    ~reals=["hazel_trig_argument_algebra"],
  );

let level_for_rule_id = rule_id =>
  rewrite_groups
  |> List.find_opt((group: rewrite_group) =>
       group.rules |> List.exists((rule: rewrite_rule) => rule.id == rule_id)
     )
  |> Option.map((group: rewrite_group) => group.level);

let catalog_rule_with_kind =
    (
      ~id,
      ~kind,
      ~direction,
      ~hazel_backend,
      ~introduced_levels,
      ~allowed_cleanup,
    ) =>
  level_for_rule_id(id)
  |> Option.map(level =>
       {
         id,
         metadata: visible_rule_metadata(id),
         level,
         kind,
         direction,
         hazel_backend,
         rocq_backend: rocq_backend_for_rule_id(id),
         introduced_levels,
         visible_mode: VisibleOnce,
         allowed_cleanup,
         required_cleanup: [],
         required_rule_ids: [],
       }
     );

let catalog_rule =
    (~id, ~direction, ~hazel_backend, ~introduced_levels, ~allowed_cleanup) =>
  catalog_rule_with_kind(
    ~id,
    ~kind=VisibleRule,
    ~direction,
    ~hazel_backend,
    ~introduced_levels,
    ~allowed_cleanup,
  );

let math_rule_catalog = {
  let affine_normalization_backend =
    rocq_rule_backend(~tactic="lia", ~integers=["lia"], ~reals=["lra"]);
  let affine_normalization_rule = {
    id: "arith.affine_normalize",
    metadata:
      operation_metadata(
        ~id="arith.affine_normalize",
        ~name="Normalize affine arithmetic",
        ~short_name="Affine",
        ~example="1 + x / 2 + x / 2 = x + 1",
      ),
    level: Arithmetic,
    kind: GuardedNormalizationRule,
    direction: BothDirections,
    hazel_backend: None,
    rocq_backend:
      Some({
        ...affine_normalization_backend,
        mode: FinishOnly,
      }),
    introduced_levels: [],
    visible_mode: VisibleOnce,
    allowed_cleanup: [],
    required_cleanup: [
      AddAssoc,
      AddComm,
      MulAssoc,
      MulComm,
      AddIdentity,
      MulIdentity,
      ConstFold,
      CollectLikeTerms,
    ],
    required_rule_ids: [
      "arith.add_comm",
      "arith.const_fold",
      "arith.mul_const",
      "arith.mul_identity",
    ],
  };
  let factor_polynomial_backend =
    rocq_rule_backend(
      ~tactic="hazel_factor_polynomial",
      ~integers=["hazel_factor_polynomial"],
      ~reals=["unfold Rsqr; ring"],
    );
  let factor_polynomial_rule = {
    id: "alg.factor_polynomial_normalize",
    metadata:
      operation_metadata(
        ~id="alg.factor_polynomial_normalize",
        ~name="Verify a factored integer polynomial",
        ~short_name="Factor poly",
        ~example="x**2 + 3*x - 4 = (x - 1) * (x + 4)",
      ),
    level: Algebra,
    kind: NormalizationRule,
    direction: BothDirections,
    hazel_backend: None,
    rocq_backend:
      Some({
        ...factor_polynomial_backend,
        mode: FinishOnly,
      }),
    introduced_levels: [],
    visible_mode: VisibleOnce,
    allowed_cleanup: [],
    required_cleanup: ac_cleanup @ structural_identity_cleanup,
    required_rule_ids: [
      "alg.distribute_mul_add",
      "alg.factor_common",
      "alg.cancel_common_add",
    ],
  };
  let rational_square_backend =
    rocq_rule_backend(
      ~tactic="hazel_rational_square_normalize",
      ~integers=[],
      ~reals=["hazel_rational_square_normalize"],
    );
  let rational_square_rule = {
    id: "alg.rational_square_normalize",
    metadata:
      operation_metadata(
        ~id="alg.rational_square_normalize",
        ~name="Normalize a squared rational binomial",
        ~short_name="Square /",
        ~example="2*((1-x)/2)**2 = 1/2-x+1/2*x**2",
      ),
    level: Algebra,
    kind: NormalizationRule,
    direction: Forward,
    hazel_backend: None,
    rocq_backend:
      Some({
        ...rational_square_backend,
        mode: FinishOnly,
      }),
    introduced_levels: [],
    visible_mode: VisibleOnce,
    allowed_cleanup: [],
    required_cleanup: ac_cleanup @ structural_identity_cleanup,
    required_rule_ids: ["alg.distribute_mul_add"],
  };
  let arithmetic_rules = [
    catalog_rule(
      ~id="arith.add_comm",
      ~direction=BothDirections,
      ~hazel_backend=Some(ArithmeticAddComm),
      ~introduced_levels=[Arithmetic],
      ~allowed_cleanup=[AddAssoc],
    ),
    catalog_rule(
      ~id="arith.const_fold",
      ~direction=Forward,
      ~hazel_backend=Some(ArithmeticConstFold),
      ~introduced_levels=[Arithmetic],
      ~allowed_cleanup=[AddAssoc],
    ),
    catalog_rule(
      ~id="arith.mul_const",
      ~direction=BothDirections,
      ~hazel_backend=Some(ArithmeticMulConst),
      ~introduced_levels=[Arithmetic],
      ~allowed_cleanup=[AddAssoc, MulAssoc],
    ),
    catalog_rule(
      ~id="arith.mul_identity",
      ~direction=Forward,
      ~hazel_backend=Some(ArithmeticMulIdentity),
      ~introduced_levels=[Arithmetic],
      ~allowed_cleanup=[],
    ),
  ];
  let algebra_rules =
    [
      catalog_rule(
        ~id="alg.distribute_mul_add",
        ~direction=BothDirections,
        ~hazel_backend=Some(AlgebraDistributeMulAdd),
        ~introduced_levels=[Algebra],
        ~allowed_cleanup=ac_cleanup @ [PowerNotation],
      ),
      catalog_rule(
        ~id="alg.factor_common",
        ~direction=BothDirections,
        ~hazel_backend=Some(AlgebraFactorCommon),
        ~introduced_levels=[Algebra],
        ~allowed_cleanup=ac_cleanup,
      ),
      catalog_rule(
        ~id="alg.cancel_common_add",
        ~direction=BothDirections,
        ~hazel_backend=Some(AlgebraCancelCommonAdd),
        ~introduced_levels=[Algebra],
        ~allowed_cleanup=[AddAssoc, AddComm],
      ),
      catalog_rule(
        ~id="alg.expand_polynomial",
        ~direction=BothDirections,
        ~hazel_backend=Some(AlgebraDistributeMulAdd),
        ~introduced_levels=[],
        ~allowed_cleanup=ac_cleanup @ structural_identity_cleanup,
      ),
    ]
    @ (
      algebra_identity_rule_ids
      |> List.map(id =>
           catalog_rule(
             ~id,
             ~direction=BothDirections,
             ~hazel_backend=Some(AlgebraIdentity),
             ~introduced_levels=[Algebra],
             ~allowed_cleanup=[],
           )
         )
    );
  let replay_only_rules =
    [
      "arith.mul_comm",
      "arith.add_assoc",
      "arith.mul_assoc",
      "arith.add_zero",
      "arith.add_neg",
      "arith.collect_like_terms",
      "arith.reorder_add_terms",
      "arith.reorder_mul_factors",
      "alg.power_add",
      "alg.power_mul",
      "alg.collect_like_terms",
    ]
    |> List.map(id => {
         let is_power_normalizer =
           List.mem(id, ["alg.power_add", "alg.power_mul"]);
         catalog_rule_with_kind(
           ~id,
           ~kind=is_power_normalizer ? NormalizationRule : CleanupRule,
           ~direction=BothDirections,
           ~hazel_backend=None,
           ~introduced_levels=[],
           ~allowed_cleanup=[],
         )
         |> Option.map((rule: math_rule) =>
              is_power_normalizer
                ? {
                  ...rule,
                  required_cleanup: [
                    PowerIdentity,
                    PowerNotation,
                    MulIdentity,
                  ],
                  required_rule_ids: ["alg.distribute_mul_add"],
                }
                : rule
            );
       });
  let trig_rules =
    trigonometry_rewrite_group.rules
    |> List.map((rule: rewrite_rule) =>
         catalog_rule(
           ~id=rule.id,
           ~direction=BothDirections,
           ~hazel_backend=Some(TrigIdentity),
           ~introduced_levels=[Trigonometry],
           ~allowed_cleanup=[],
         )
       );
  let calculus_rules =
    calculus_rewrite_group.rules
    |> List.map((rule: rewrite_rule) =>
         catalog_rule(
           ~id=rule.id,
           ~direction=Forward,
           ~hazel_backend=Some(CalculusDerivative),
           ~introduced_levels=rule.id == "calc.diff_chain" ? [] : [Calculus],
           ~allowed_cleanup=calculus_step_cleanup_for_rule(rule.id),
         )
       );
  let algebra_rules =
    algebra_rules
    |> List.map((entry: option(math_rule)) =>
         switch (entry) {
         | Some(rule: math_rule) when rule.id == "alg.expand_polynomial" =>
           Some(
             {
               ...rule,
               kind: NormalizationRule,
               required_cleanup:
                 ac_cleanup
                 @ structural_identity_cleanup
                 @ [ConstFold, CollectLikeTerms],
               required_rule_ids: ["alg.distribute_mul_add"],
             }: math_rule,
           )
         | Some(_)
         | None => entry
         }
       );
  arithmetic_rules
  @ algebra_rules
  @ replay_only_rules
  @ trig_rules
  @ calculus_rules
  @ [
    Some(affine_normalization_rule),
    Some(factor_polynomial_rule),
    Some(rational_square_rule),
  ]
  |> List.filter_map(value => value);
};

let affine_normalization_cleanup = [
  AddAssoc,
  AddComm,
  MulAssoc,
  MulComm,
  AddIdentity,
  MulIdentity,
  ConstFold,
  CollectLikeTerms,
];

/* Each entry contains only capabilities introduced at that node. Profiles
   inherit the union from every ancestor, so sibling branches stay isolated. */
let profile_cleanup_introduced = [
  (Arithmetic, affine_normalization_cleanup),
  (Algebra, [PowerIdentity, PowerNotation]),
  (Trigonometry, []),
  (FunctionsAndLists, []),
  (Calculus, [DerivativeBasics]),
];

let dedup_cleanup_capabilities = capabilities =>
  capabilities
  |> List.fold_left(
       (acc, capability) =>
         List.mem(capability, acc) ? acc : [capability, ...acc],
       [],
     )
  |> List.rev;

let profile_default_cleanup_for_level = level =>
  inherited_rewrite_levels(level)
  |> List.concat_map(inherited_level =>
       profile_cleanup_introduced
       |> List.find_opt(((candidate_level, _cleanup)) =>
            candidate_level == inherited_level
          )
       |> Option.map(((_level, cleanup)) => cleanup)
       |> Option.value(~default=[])
     )
  |> dedup_cleanup_capabilities;

let rule_visible_at_level = (rule: math_rule, level) =>
  rule.introduced_levels
  |> List.exists(introduced_level =>
       rewrite_level_inherits(~current_level=level, introduced_level)
     );

let step_policy = level => {
  visible_rules:
    math_rule_catalog
    |> List.filter((rule: math_rule) => rule_visible_at_level(rule, level))
    |> List.map((rule: math_rule) =>
         {
           rule_id: rule.id,
           metadata: rule.metadata,
           mode: rule.visible_mode,
           allowed_cleanup: rule.allowed_cleanup,
         }
       ),
  default_cleanup: profile_default_cleanup_for_level(level),
};

let catalog_rule_by_id = rule_id =>
  math_rule_catalog |> List.find_opt((rule: math_rule) => rule.id == rule_id);

let unresolved_visible_rule_ids = (policy: step_policy) =>
  policy.visible_rules
  |> List.filter_map((rule: visible_rule_policy) =>
       catalog_rule_by_id(rule.rule_id) |> Option.is_some
         ? None : Some(rule.rule_id)
     );

let visible_catalog_rules = (policy: step_policy) =>
  policy.visible_rules
  |> List.filter_map((rule: visible_rule_policy) =>
       catalog_rule_by_id(rule.rule_id)
     );

let planned_visible_rules = (policy: step_policy) =>
  policy.visible_rules
  |> List.filter_map((rule_policy: visible_rule_policy) =>
       catalog_rule_by_id(rule_policy.rule_id)
       |> Option.map(rule =>
            {
              rule,
              mode: rule_policy.mode,
              allowed_cleanup: rule_policy.allowed_cleanup,
            }
          )
     );

let visible_rule_policy_for_rule = (policy: step_policy, rule_id) =>
  policy.visible_rules
  |> List.find_opt((rule_policy: visible_rule_policy) =>
       rule_policy.rule_id == rule_id
     );

let visible_rule_enabled = (policy: step_policy, rule_id) =>
  visible_rule_policy_for_rule(policy, rule_id) |> Option.is_some;

let cleanup_for_visible_rule = (policy: step_policy, rule_id) =>
  visible_rule_policy_for_rule(policy, rule_id)
  |> Option.map((rule_policy: visible_rule_policy) =>
       rule_policy.allowed_cleanup
     )
  |> Option.value(~default=policy.default_cleanup);

let one_step_policy = level => {
  distribution_step_policy: StrictDistributedForm,
  allow_polynomial_expansion:
    rewrite_level_inherits(~current_level=level, Algebra),
};

let rocq_finish_step = (~id, ~label, ~tactic, ~rule_ids) =>
  rocq_tactic_step(~id, ~label, ~tactic, ~mode=FinishOnly, ~rule_ids);

let rocq_try_step = (~id, ~label, ~tactic, ~rule_ids) =>
  rocq_tactic_step(~id, ~label, ~tactic, ~mode=TryOnce, ~rule_ids);

let rocq_once_step = (~id, ~label, ~tactic, ~rule_ids) =>
  rocq_tactic_step(~id, ~label, ~tactic, ~mode=Once, ~rule_ids);

let rocq_repeat_fuel_step = (~id, ~label, ~tactic, ~fuel, ~rule_ids) =>
  rocq_tactic_step(~id, ~label, ~tactic, ~mode=RepeatFuel(fuel), ~rule_ids);

let rocq_check_result_tactic_plan_for_node = level => {
  switch (level) {
  | Arithmetic => {
      id: "hazel_arithmetic_plan",
      label: "Arithmetic tactic plan",
      steps: [
        rocq_try_step(
          ~id="arith_power_normalize",
          ~label="normalize powers and identities",
          ~tactic="hazel_power_normalize",
          ~rule_ids=["arith.mul_const"],
        ),
        rocq_try_step(
          ~id="arith_mul_reorder",
          ~label="reorder multiplication",
          ~tactic="hazel_mul_reorder",
          ~rule_ids=["arith.reorder_mul_factors"],
        ),
        rocq_finish_step(
          ~id="arith_finish",
          ~label="finish arithmetic goal",
          ~tactic="hazel_arithmetic",
          ~rule_ids=[
            "arith.const_fold",
            "arith.reorder_add_terms",
            "arith.reorder_mul_factors",
          ],
        ),
      ],
    }
  | Algebra => {
      id: "hazel_algebra_plan",
      label: "Algebra tactic plan",
      steps: [
        rocq_try_step(
          ~id="alg_power_normalize",
          ~label="normalize powers and identities",
          ~tactic="hazel_power_normalize",
          ~rule_ids=["alg.power_add", "alg.power_mul"],
        ),
        rocq_try_step(
          ~id="alg_bounded_rewrite_search",
          ~label="bounded algebra rewrite search",
          ~tactic="hazel_rewrite_search 10%nat",
          ~rule_ids=[
            "alg.distribute_mul_add",
            "alg.factor_common",
            "alg.cancel_common_add",
          ],
        ),
        rocq_try_step(
          ~id="alg_mul_reorder",
          ~label="reorder multiplication",
          ~tactic="hazel_mul_reorder",
          ~rule_ids=["arith.reorder_mul_factors"],
        ),
        rocq_finish_step(
          ~id="alg_finish",
          ~label="finish algebra goal",
          ~tactic="hazel_algebra",
          ~rule_ids=[
            "alg.expand_polynomial",
            "alg.collect_like_terms",
            "alg.factor_common",
          ],
        ),
      ],
    }
  | Trigonometry => {
      id: "hazel_trigonometry_plan",
      label: "Trigonometry tactic plan",
      steps: [
        rocq_try_step(
          ~id="trig_pythagorean",
          ~label="try Pythagorean identity",
          ~tactic="hazel_pythagorean",
          ~rule_ids=["trig.pythagorean_sin_cos", "trig.pythagorean_cos_sin"],
        ),
        rocq_try_step(
          ~id="trig_argument_algebra",
          ~label="try trig argument algebra",
          ~tactic="hazel_trig_argument_algebra",
          ~rule_ids=["arith.simplify_scalar_products"],
        ),
        rocq_try_step(
          ~id="trig_power_normalize",
          ~label="normalize powers and identities",
          ~tactic="hazel_power_normalize",
          ~rule_ids=["alg.power_add", "alg.power_mul"],
        ),
        rocq_try_step(
          ~id="trig_context_solve",
          ~label="try trig context simplification",
          ~tactic="hazel_trig_context_solve",
          ~rule_ids=["trig.sin_squared_double", "trig.cos_squared_double"],
        ),
        rocq_finish_step(
          ~id="trig_finish",
          ~label="finish trigonometry goal",
          ~tactic="hazel_trigonometry",
          ~rule_ids=["trig.pythagorean_sin_cos"],
        ),
      ],
    }
  | FunctionsAndLists => {
      id: "hazel_functions_plan",
      label: "Functions/lists tactic plan",
      steps: [],
    }
  | Calculus => {
      id: "hazel_calculus_plan",
      label: "Calculus tactic plan",
      steps: [],
    }
  };
};

let rocq_validate_primitive_tactic_plan_for_node = level => {
  switch (level) {
  | Arithmetic => {
      id: "hazel_arithmetic_primitive_plan",
      label: "Arithmetic primitive-step tactic plan",
      steps: [
        rocq_try_step(
          ~id="arith_power_normalize_once",
          ~label="try one arithmetic normalization",
          ~tactic="hazel_power_normalize",
          ~rule_ids=["arith.mul_const"],
        ),
        rocq_once_step(
          ~id="arith_mul_reorder_once",
          ~label="try one multiplication reorder",
          ~tactic="hazel_mul_reorder",
          ~rule_ids=["arith.reorder_mul_factors"],
        ),
      ],
    }
  | Algebra => {
      id: "hazel_algebra_primitive_plan",
      label: "Algebra primitive-step tactic plan",
      steps: [
        rocq_try_step(
          ~id="alg_power_normalize_once",
          ~label="try one algebra normalization",
          ~tactic="hazel_power_normalize",
          ~rule_ids=["alg.power_add", "alg.power_mul"],
        ),
        rocq_once_step(
          ~id="alg_rewrite_once",
          ~label="try one algebra rewrite",
          ~tactic="hazel_rewrite_search 1%nat",
          ~rule_ids=[
            "alg.distribute_mul_add",
            "alg.factor_common",
            "alg.cancel_common_add",
          ],
        ),
      ],
    }
  | Trigonometry => {
      id: "hazel_trigonometry_primitive_plan",
      label: "Trigonometry primitive-step tactic plan",
      steps: [
        rocq_once_step(
          ~id="trig_pythagorean_once",
          ~label="try one trig identity",
          ~tactic="hazel_pythagorean",
          ~rule_ids=["trig.pythagorean_sin_cos", "trig.pythagorean_cos_sin"],
        ),
        rocq_try_step(
          ~id="trig_argument_algebra_once",
          ~label="try one trig argument simplification",
          ~tactic="hazel_trig_argument_algebra",
          ~rule_ids=["arith.simplify_scalar_products"],
        ),
      ],
    }
  | FunctionsAndLists => {
      id: "hazel_functions_primitive_plan",
      label: "Functions/lists primitive-step tactic plan",
      steps: [],
    }
  | Calculus => {
      id: "hazel_calculus_primitive_plan",
      label: "Calculus primitive-step tactic plan",
      steps: [],
    }
  };
};

let rocq_validate_macro_tactic_plan_for_node = level => {
  switch (level) {
  | Arithmetic => {
      id: "hazel_arithmetic_macro_plan",
      label: "Arithmetic macro-step tactic plan",
      steps: [
        rocq_repeat_fuel_step(
          ~id="arith_power_normalize_macro",
          ~label="normalize arithmetic powers",
          ~tactic="hazel_power_normalize",
          ~fuel=4,
          ~rule_ids=["arith.mul_const"],
        ),
        rocq_try_step(
          ~id="arith_mul_reorder_macro",
          ~label="reorder arithmetic multiplication",
          ~tactic="hazel_mul_reorder",
          ~rule_ids=["arith.reorder_mul_factors"],
        ),
      ],
    }
  | Algebra => {
      id: "hazel_algebra_macro_plan",
      label: "Algebra macro-step tactic plan",
      steps: [
        rocq_repeat_fuel_step(
          ~id="alg_rewrite_macro",
          ~label="bounded algebra rewrite macro",
          ~tactic="hazel_rewrite_step",
          ~fuel=10,
          ~rule_ids=[
            "alg.distribute_mul_add",
            "alg.factor_common",
            "alg.cancel_common_add",
          ],
        ),
        rocq_try_step(
          ~id="alg_mul_reorder_macro",
          ~label="reorder multiplication",
          ~tactic="hazel_mul_reorder",
          ~rule_ids=["arith.reorder_mul_factors"],
        ),
      ],
    }
  | Trigonometry => {
      id: "hazel_trigonometry_macro_plan",
      label: "Trigonometry macro-step tactic plan",
      steps: [
        rocq_repeat_fuel_step(
          ~id="trig_rewrite_macro",
          ~label="bounded trigonometry rewrite macro",
          ~tactic="hazel_rewrite_step",
          ~fuel=12,
          ~rule_ids=["trig.pythagorean_sin_cos", "trig.pythagorean_cos_sin"],
        ),
        rocq_try_step(
          ~id="trig_context_macro",
          ~label="simplify trig context",
          ~tactic="hazel_trig_context_solve",
          ~rule_ids=["trig.sin_squared_double", "trig.cos_squared_double"],
        ),
      ],
    }
  | FunctionsAndLists => rocq_validate_primitive_tactic_plan_for_node(level)
  | Calculus => rocq_validate_primitive_tactic_plan_for_node(level)
  };
};

let compose_inherited_rocq_plan = (~level, ~plan_for_node) => {
  let local_plan = plan_for_node(level);
  {
    ...local_plan,
    steps:
      inherited_rewrite_levels(level)
      |> List.concat_map(inherited_level =>
           plan_for_node(inherited_level).steps
         ),
  };
};

let rocq_check_result_tactic_plan = level =>
  compose_inherited_rocq_plan(
    ~level,
    ~plan_for_node=rocq_check_result_tactic_plan_for_node,
  );

let rocq_validate_primitive_tactic_plan = level =>
  compose_inherited_rocq_plan(
    ~level,
    ~plan_for_node=rocq_validate_primitive_tactic_plan_for_node,
  );

let rocq_validate_macro_tactic_plan = level =>
  compose_inherited_rocq_plan(
    ~level,
    ~plan_for_node=rocq_validate_macro_tactic_plan_for_node,
  );

let rocq_auto_simplify_tactic_plan = level => {
  let check_plan = rocq_check_result_tactic_plan(level);
  {
    ...check_plan,
    id: check_plan.id ++ "_auto_simplify",
    label: check_plan.label ++ " for auto simplification",
  };
};

let rocq_tactic_plan = level => rocq_check_result_tactic_plan(level);

let rocq_tactic_plan_for_purpose = (level, purpose) =>
  switch (purpose) {
  | ValidatePrimitiveStep => rocq_validate_primitive_tactic_plan(level)
  | ValidateMacroStep => rocq_validate_macro_tactic_plan(level)
  | CheckResult => rocq_check_result_tactic_plan(level)
  | AutoSimplify => rocq_auto_simplify_tactic_plan(level)
  };

let rocq_tactic_plans = level => [
  (
    ValidatePrimitiveStep,
    rocq_tactic_plan_for_purpose(level, ValidatePrimitiveStep),
  ),
  (
    ValidateMacroStep,
    rocq_tactic_plan_for_purpose(level, ValidateMacroStep),
  ),
  (CheckResult, rocq_tactic_plan_for_purpose(level, CheckResult)),
  (AutoSimplify, rocq_tactic_plan_for_purpose(level, AutoSimplify)),
];

let math_profile = level => {
  let rocq_config =
    switch (level) {
    | Arithmetic => (
        "rocq.arithmetic_tactic_search",
        "hazel_arithmetic",
        IntegersByDefault,
      )
    | Algebra => (
        "rocq.algebra_tactic_search",
        "hazel_algebra",
        IntegersByDefault,
      )
    | Trigonometry => (
        "rocq.trigonometry_tactic_search",
        "hazel_trigonometry",
        RealsByDefault,
      )
    | FunctionsAndLists => (
        "rocq.functions_tactic_search",
        "hazel_functions",
        IntegersByDefault,
      )
    | Calculus => (
        "rocq.calculus_tactic_search",
        "hazel_calculus",
        RealsByDefault,
      )
    };
  let (rocq_macro_rule_id, rocq_tactic_group, rocq_domain_policy) = rocq_config;
  {
    level,
    rank: rewrite_level_rank(level),
    label: rewrite_level_label(level),
    detail: rewrite_level_detail(level),
    enabled: rewrite_level_enabled(level),
    groups: allowed_groups(level),
    one_step_policy: one_step_policy(level),
    step_policy: step_policy(level),
    check_result_rule_ids:
      math_rule_catalog
      |> List.filter((rule: math_rule) =>
           (
             rule.kind == NormalizationRule
             || rule.kind == GuardedNormalizationRule
           )
           && rewrite_level_inherits(~current_level=level, rule.level)
         )
      |> List.map((rule: math_rule) => rule.id),
    rocq_macro_rule_id,
    rocq_tactic_group,
    rocq_tactic_plan: rocq_tactic_plan(level),
    rocq_tactic_plans: rocq_tactic_plans(level),
    rocq_domain_policy,
  };
};

let rocq_tactic_plan_for_profile = (profile, purpose) =>
  profile.rocq_tactic_plans
  |> List.find_opt(((candidate_purpose, _plan)) =>
       candidate_purpose == purpose
     )
  |> Option.map(((_purpose, plan)) => plan)
  |> Option.value(~default=profile.rocq_tactic_plan);

let tactic_plan_purpose_for_stage =
  fun
  | Manual => ValidatePrimitiveStep
  | MultiStepCheck => CheckResult
  | AutoEval => AutoSimplify;

let cleanup_enabled_for_profile = (profile: math_profile, capability) =>
  List.mem(capability, profile.step_policy.default_cleanup);

let rule_prerequisites_satisfied = (profile: math_profile, rule: math_rule) =>
  rule.required_cleanup
  |> List.for_all(cleanup_enabled_for_profile(profile))
  && rule.required_rule_ids
  |> List.for_all(rule_id =>
       visible_rule_enabled(profile.step_policy, rule_id)
     );

let check_result_rule_enabled = (profile: math_profile, rule_id) =>
  List.mem(rule_id, profile.check_result_rule_ids)
  && (
    switch (catalog_rule_by_id(rule_id)) {
    | Some(rule) => rule_prerequisites_satisfied(profile, rule)
    | None => false
    }
  );

let normalization_rules_for_profile = (profile: math_profile) =>
  math_rule_catalog
  |> List.filter((rule: math_rule) =>
       (
         rule.kind == NormalizationRule || rule.kind == GuardedNormalizationRule
       )
       && rewrite_level_inherits(~current_level=profile.level, rule.level)
       && check_result_rule_enabled(profile, rule.id)
     );

let normalization_backends_for_profile = (profile: math_profile) => {
  let catalog_backends =
    normalization_rules_for_profile(profile)
    |> List.filter((rule: math_rule) => rule.kind == NormalizationRule)
    |> List.filter_map((rule: math_rule) => rule.rocq_backend);
  catalog_backends;
};

let guarded_normalization_backend_for_profile =
    (profile: math_profile, rule_id) =>
  switch (catalog_rule_by_id(rule_id)) {
  | Some(rule)
      when
        rule.kind == GuardedNormalizationRule
        && rewrite_level_inherits(~current_level=profile.level, rule.level)
        && check_result_rule_enabled(profile, rule.id) =>
    rule.rocq_backend
  | Some(_)
  | None => None
  };

let profile_allows_rocq_rule_id = (profile: math_profile, rule_id) =>
  visible_rule_enabled(profile.step_policy, rule_id)
  || check_result_rule_enabled(profile, rule_id)
  || (
    switch (cleanup_capability_for_id(rule_id)) {
    | Some(capability) => cleanup_enabled_for_profile(profile, capability)
    | None => false
    }
  );

let active_rocq_tactic_plan_for_profile = (profile, purpose) => {
  let plan = rocq_tactic_plan_for_profile(profile, purpose);
  {
    ...plan,
    steps:
      plan.steps
      |> List.filter((step: rocq_tactic_step) =>
           step.rule_ids != []
           && step.rule_ids
           |> List.for_all(profile_allows_rocq_rule_id(profile))
         ),
  };
};

let stage_plan_for_profile = (profile: math_profile, stage) => {
  let unresolved = unresolved_visible_rule_ids(profile.step_policy);
  switch (unresolved) {
  | [rule_id, ..._] =>
    invalid_arg("Unknown math rule in profile stage plan: " ++ rule_id)
  | [] =>
    let cleanup = profile.step_policy.default_cleanup;
    let cleanup_atoms =
      cleanup
      |> List.map(capability =>
           {
             id: cleanup_capability_label(capability),
             kind: CleanupAtom,
             mode: RepeatUntilStuck,
             required_cleanup: [capability],
             required_rule_ids: [],
           }
         );
    let visible_atoms =
      planned_visible_rules(profile.step_policy)
      |> List.map((planned: planned_visible_rule) =>
           {
             id: planned.rule.id,
             kind: VisibleAtom,
             mode:
               switch (planned.mode) {
               | VisibleOnce => Once
               | VisibleRepeatFuel(fuel) => RepeatFuel(fuel)
               | VisibleRepeatUntilStuck => RepeatUntilStuck
               },
             required_cleanup: planned.allowed_cleanup,
             required_rule_ids: [],
           }
         );
    let normalizer_atoms =
      stage == MultiStepCheck
        ? normalization_rules_for_profile(profile)
          |> List.map((rule: math_rule) =>
               {
                 id: rule.id,
                 kind: CheckNormalizerAtom,
                 mode:
                   rule.rocq_backend
                   |> Option.map((backend: rocq_rule_backend) =>
                        backend.mode
                      )
                   |> Option.value(~default=FinishOnly),
                 required_cleanup: rule.required_cleanup,
                 required_rule_ids: rule.required_rule_ids,
               }
             )
        : [];
    {
      stage,
      atoms: cleanup_atoms @ visible_atoms @ normalizer_atoms,
      pre_cleanup: cleanup,
      visible_rules: planned_visible_rules(profile.step_policy),
      post_cleanup: cleanup,
      normalization_backends: normalization_backends_for_profile(profile),
      rocq_plan:
        active_rocq_tactic_plan_for_profile(
          profile,
          tactic_plan_purpose_for_stage(stage),
        ),
    };
  };
};

let stage_plan_for_level = (level, stage) =>
  stage_plan_for_profile(math_profile(level), stage);

let math_profiles = rewrite_levels |> List.map(math_profile);

let math_profile_for_group_name = name =>
  switch (name) {
  | "arithmetic" => Some(math_profile(Arithmetic))
  | "algebra" => Some(math_profile(Algebra))
  | "trigonometry" => Some(math_profile(Trigonometry))
  | "functions/lists" => Some(math_profile(FunctionsAndLists))
  | "calculus" => Some(math_profile(Calculus))
  | _ => None
  };

let math_profile_for_macro_rule_id = rule_id =>
  math_profiles
  |> List.find_opt(profile => profile.rocq_macro_rule_id == rule_id);

let is_rocq_macro_rule_id = rule_id =>
  math_profile_for_macro_rule_id(rule_id) |> Option.is_some;

let rocq_tactic_group_for_macro_rule_id = rule_id =>
  math_profile_for_macro_rule_id(rule_id)
  |> Option.map(profile => profile.rocq_tactic_group);

let effective_profile_for_rewrite = (~requested_level, source, target) =>
  math_profile(export_level_for_rewrite(~requested_level, source, target));

let rewrite_group_by_name = name =>
  rewrite_groups
  |> List.find_opt((group: rewrite_group) => group.name == name);

let rewrite_rule_by_id = (group, id) =>
  group.rules |> List.find_opt((rule: rewrite_rule) => rule.id == id);

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
