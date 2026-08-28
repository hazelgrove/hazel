open Language;
open Util;

type application = {
  rule: Axioms.rewrite_rule,
  before_full_exp: Exp.t,
  after_full_exp: Exp.t,
  before_exp: Exp.t,
  after_exp: Exp.t,
  occurrence: int,
};

type result = {
  source: Exp.t,
  target: Exp.t,
  steps: list(ProofTrace.prover_step),
  applications: list(application),
};

let strip = exp =>
  exp |> DHExp.strip_ascriptions |> MathRewriteUtil.strip_math_wrappers;

let exp_same = (left, right) =>
  MathRewriteUtil.exp_same(strip(left), strip(right))
  || TrigRewrite.exp_same(strip(left), strip(right));

let int_exp = MathRewriteUtil.int_exp;
let plus_exp = MathRewriteUtil.plus_exp;
let times_exp = MathRewriteUtil.times_exp;

let power_exp_with_op = (op, base, exponent) =>
  Exp.fresh(BinOp(op, base, exponent));

let rule_by_id = rule_id =>
  Axioms.rewrite_groups
  |> List.filter_map(group => Axioms.rewrite_rule_by_id(group, rule_id))
  |> ListUtil.hd_opt;

let allowed_rules = level =>
  Axioms.allowed_groups(level)
  |> List.concat_map((group: Axioms.rewrite_group) => group.rules);

let cleanup_rules = level =>
  Axioms.profile_default_cleanup_for_level(level)
  |> List.map(capability => {
       let metadata = Axioms.cleanup_capability_metadata(capability);
       {
         Axioms.id: Axioms.cleanup_capability_label(capability),
         label: metadata.name,
         prover_hints: [],
       };
     });

let allowed_rule_ids = level =>
  allowed_rules(level) |> List.map((rule: Axioms.rewrite_rule) => rule.id);

let int_constant = exp => MathRewriteUtil.int_constant(strip(exp));

let is_power_op =
  fun
  | Operators.Int(Operators.Power)
  | Nat(Power)
  | SInt(Power)
  | Real(Power)
  | Float(Power) => true
  | _ => false;

let rec has_hole = exp =>
  switch (strip(exp).term) {
  | EmptyHole
  | DynamicErrorHole(_) => true
  | BinOp(_, left, right)
  | Ap(_, left, right) => has_hole(left) || has_hole(right)
  | UnOp(_, inner)
  | Parens(inner)
  | Asc(inner, _)
  | Projector(_, inner) => has_hole(inner)
  | Tuple(entries)
  | ListLit(entries) => List.exists(has_hole, entries)
  | Fun(_, body, _, _) => has_hole(body)
  | _ => false
  };

let rec flatten_plus = exp => {
  let exp = strip(exp);
  switch (exp.term) {
  | BinOp(op, left, right) when MathRewriteUtil.is_plus_op(op) =>
    flatten_plus(left) @ flatten_plus(right)
  | _ => [exp]
  };
};

let rec flatten_times = exp => {
  let exp = strip(exp);
  switch (exp.term) {
  | BinOp(op, left, right) when MathRewriteUtil.is_times_op(op) =>
    flatten_times(left) @ flatten_times(right)
  | _ => [exp]
  };
};

let rec choices = items =>
  switch (items) {
  | [] => []
  | [item, ...rest] => [
      (item, rest),
      ...choices(rest)
         |> List.map(((chosen, remaining)) =>
              (chosen, [item, ...remaining])
            ),
    ]
  };

let rec permutations = items =>
  switch (items) {
  | [] => [[]]
  | _ =>
    choices(items)
    |> List.concat_map(((item, rest)) =>
         permutations(rest)
         |> List.map(permutation => [item, ...permutation])
       )
  };

let build_left_assoc_plus = terms =>
  switch (terms) {
  | [] => None
  | [term] => Some(term)
  | [first, ...rest] =>
    Some(rest |> List.fold_left((acc, term) => plus_exp(acc, term), first))
  };

let build_left_assoc_times = terms =>
  switch (terms) {
  | [] => None
  | [term] => Some(term)
  | [first, ...rest] =>
    Some(
      rest |> List.fold_left((acc, term) => times_exp(acc, term), first),
    )
  };

let small_addition_permutations = exp => {
  let terms = flatten_plus(exp);
  let length = List.length(terms);
  length > 1 && length <= 4
    ? terms
      |> permutations
      |> List.filter(permutation =>
           !
             List.for_all2(
               (left, right) => exp_same(left, right),
               terms,
               permutation,
             )
         )
      |> List.filter_map(build_left_assoc_plus)
    : [];
};

let small_multiplication_permutations = exp => {
  let factors = flatten_times(exp);
  let length = List.length(factors);
  length > 1 && length <= 4
    ? factors
      |> permutations
      |> List.filter(permutation =>
           !
             List.for_all2(
               (left, right) => exp_same(left, right),
               factors,
               permutation,
             )
         )
      |> List.filter_map(build_left_assoc_times)
    : [];
};

let power_literal = (power_op, value) =>
  switch (power_op) {
  | Operators.Nat(Power) => Exp.fresh(Atom(Nat(Bigint.of_int(value))))
  | Operators.SInt(Power) => Exp.fresh(Atom(SInt(value)))
  | Operators.Real(Power) =>
    Exp.fresh(Atom(Real(Real.of_bigint(Bigint.of_int(value)))))
  | Operators.Float(Operators.Power) =>
    Exp.fresh(Atom(Float(float_of_int(value))))
  | _ => int_exp(Bigint.of_int(value))
  };

let times_exp_for_power_op = (power_op, left, right) =>
  switch (power_op) {
  | Operators.Int(Operators.Power) =>
    Exp.fresh(BinOp(Operators.Int(Operators.Times), left, right))
  | Operators.Nat(Power) =>
    Exp.fresh(BinOp(Operators.Nat(Times), left, right))
  | Operators.SInt(Power) =>
    Exp.fresh(BinOp(Operators.SInt(Times), left, right))
  | Operators.Real(Power) =>
    Exp.fresh(BinOp(Operators.Real(Times), left, right))
  | Operators.Float(Power) =>
    Exp.fresh(BinOp(Operators.Float(Times), left, right))
  | _ => times_exp(left, right)
  };

let positive_literal_splits = (power_op, exponent) =>
  switch (int_constant(exponent)) {
  | Some(value) =>
    switch (Bigint.to_int(value)) {
    | Some(value) when value > 1 =>
      List.init(
        value - 1,
        index => {
          let left = index + 1;
          let right = value - left;
          (power_literal(power_op, left), power_literal(power_op, right));
        },
      )
    | _ => []
    }
  | _ => []
  };

let positive_literal_factor_splits = (power_op, exponent) =>
  switch (int_constant(exponent)) {
  | Some(value) =>
    switch (Bigint.to_int(value)) {
    | Some(value) when value > 1 =>
      List.init(value - 2, index => index + 2)
      |> List.filter_map(left =>
           value mod left == 0
             ? {
               let right = value / left;
               right > 1
                 ? Some((
                     power_literal(power_op, left),
                     power_literal(power_op, right),
                   ))
                 : None;
             }
             : None
         )
    | _ => []
    }
  | _ => []
  };

let rec contains_addition = exp =>
  switch (strip(exp).term) {
  | BinOp(op, _, _) when MathRewriteUtil.is_plus_op(op) => true
  | BinOp(_, left, right)
  | Ap(_, left, right) =>
    contains_addition(left) || contains_addition(right)
  | UnOp(_, inner)
  | Parens(inner)
  | Asc(inner, _)
  | Projector(_, inner) => contains_addition(inner)
  | Tuple(entries)
  | ListLit(entries) => List.exists(contains_addition, entries)
  | Fun(_, body, _, _) => contains_addition(body)
  | _ => false
  };

let scalar_product_spans_addition = exp =>
  switch (strip(exp).term) {
  | BinOp(op, left, right) when MathRewriteUtil.is_times_op(op) =>
    contains_addition(left) || contains_addition(right)
  | _ => false
  };

let rec apply_rule_at_root = (rule_id, exp: Exp.t): list(Exp.t) => {
  let exp = strip(exp);
  switch (rule_id, exp.term) {
  | ("arith.add_comm", BinOp(op, left, right))
      when MathRewriteUtil.is_plus_op(op) => [
      MathRewriteUtil.plus_exp_with_op(op, right, left),
    ]
  | ("arith.mul_comm", BinOp(op, left, right))
      when MathRewriteUtil.is_times_op(op) => [
      MathRewriteUtil.times_exp_with_op(op, right, left),
    ]
  | ("arith.add_assoc", BinOp(op, {term: BinOp(inner_op, a, b), _}, c))
      when
        MathRewriteUtil.is_plus_op(op)
        && MathRewriteUtil.is_plus_op(inner_op) => [
      MathRewriteUtil.plus_exp_with_op(
        op,
        a,
        MathRewriteUtil.plus_exp_with_op(op, b, c),
      ),
    ]
  | ("arith.add_assoc", BinOp(op, a, {term: BinOp(inner_op, b, c), _}))
      when
        MathRewriteUtil.is_plus_op(op)
        && MathRewriteUtil.is_plus_op(inner_op) => [
      MathRewriteUtil.plus_exp_with_op(
        op,
        MathRewriteUtil.plus_exp_with_op(op, a, b),
        c,
      ),
    ]
  | ("arith.mul_assoc", BinOp(op, {term: BinOp(inner_op, a, b), _}, c))
      when
        MathRewriteUtil.is_times_op(op)
        && MathRewriteUtil.is_times_op(inner_op) => [
      MathRewriteUtil.times_exp_with_op(
        op,
        a,
        MathRewriteUtil.times_exp_with_op(op, b, c),
      ),
    ]
  | ("arith.mul_assoc", BinOp(op, a, {term: BinOp(inner_op, b, c), _}))
      when
        MathRewriteUtil.is_times_op(op)
        && MathRewriteUtil.is_times_op(inner_op) => [
      MathRewriteUtil.times_exp_with_op(
        op,
        MathRewriteUtil.times_exp_with_op(op, a, b),
        c,
      ),
    ]
  | ("arith.add_zero", BinOp(op, left, right))
      when MathRewriteUtil.is_plus_op(op) =>
    switch (int_constant(left), int_constant(right)) {
    | (Some(value), _) when Bigint.equal(value, Bigint.zero) => [
        strip(right),
      ]
    | (_, Some(value)) when Bigint.equal(value, Bigint.zero) => [
        strip(left),
      ]
    | _ => []
    }
  | ("arith.const_fold", _) =>
    let is_value = (expected, candidate) =>
      int_constant(candidate)
      |> Option.map(value => Bigint.equal(value, expected))
      |> Option.value(~default=false);
    let is_identity_operation =
      switch (exp.term) {
      | BinOp(op, left, right) when MathRewriteUtil.is_plus_op(op) =>
        is_value(Bigint.zero, left) || is_value(Bigint.zero, right)
      | BinOp(op, left, right) when MathRewriteUtil.is_times_op(op) =>
        is_value(Bigint.zero, left)
        || is_value(Bigint.zero, right)
        || is_value(Bigint.one, left)
        || is_value(Bigint.one, right)
      | BinOp(op, _, exponent) when is_power_op(op) =>
        is_value(Bigint.zero, exponent) || is_value(Bigint.one, exponent)
      | _ => false
      };
    if (is_identity_operation) {
      [];
    } else {
      switch (ArithmeticNormalization.fold_rational_constant(exp)) {
      | Some(folded) when !exp_same(exp, folded) => [folded]
      | Some(_)
      | None => []
      };
    };
  | ("arith.reorder_add_terms", _) => small_addition_permutations(exp)
  | ("arith.reorder_mul_factors", _) =>
    small_multiplication_permutations(exp)
  | ("arith.simplify_scalar_products", _) =>
    if (scalar_product_spans_addition(exp)) {
      [];
    } else {
      let simplified = ArithmeticNormalization.simplify_scalar_products(exp);
      exp_same(exp, simplified) ? [] : [simplified];
    }
  | (
      "alg.distribute_mul_add",
      BinOp(op, left, {term: BinOp(plus_op, add_left, add_right), _}),
    )
      when
        MathRewriteUtil.is_times_op(op)
        && MathRewriteUtil.is_plus_op(plus_op) => [
      MathRewriteUtil.plus_exp_with_op(
        plus_op,
        MathRewriteUtil.times_exp_with_op(op, left, add_left),
        MathRewriteUtil.times_exp_with_op(op, left, add_right),
      ),
    ]
  | (
      "alg.distribute_mul_add",
      BinOp(op, {term: BinOp(plus_op, add_left, add_right), _}, right),
    )
      when
        MathRewriteUtil.is_times_op(op)
        && MathRewriteUtil.is_plus_op(plus_op) => [
      MathRewriteUtil.plus_exp_with_op(
        plus_op,
        MathRewriteUtil.times_exp_with_op(op, add_left, right),
        MathRewriteUtil.times_exp_with_op(op, add_right, right),
      ),
    ]
  | ("alg.distribute_div_add", _) =>
    MathRewriteUtil.distribute_div_over_add_candidates(exp)
  | ("alg.factor_common", BinOp(plus_op, left, right))
      when MathRewriteUtil.is_plus_op(plus_op) =>
    switch (
      MathRewriteUtil.factors_of_product(left),
      MathRewriteUtil.factors_of_product(right),
    ) {
    | (Some((times_op, a, b)), Some((right_times_op, c, d)))
        when times_op == right_times_op && exp_same(a, c) => [
        times_exp(a, plus_exp(b, d)),
      ]
    | (Some((times_op, a, b)), Some((right_times_op, c, d)))
        when times_op == right_times_op && exp_same(a, d) => [
        times_exp(a, plus_exp(b, c)),
      ]
    | (Some((times_op, a, b)), Some((right_times_op, c, d)))
        when times_op == right_times_op && exp_same(b, c) => [
        times_exp(b, plus_exp(a, d)),
      ]
    | (Some((times_op, a, b)), Some((right_times_op, c, d)))
        when times_op == right_times_op && exp_same(b, d) => [
        times_exp(b, plus_exp(a, c)),
      ]
    | _ => []
    }
  | ("alg.power_add", BinOp(power_op, base, exponent))
      when is_power_op(power_op) =>
    let syntactic_splits =
      switch (strip(exponent).term) {
      | BinOp(plus_op, left_exp, right_exp)
          when MathRewriteUtil.is_plus_op(plus_op) => [
          (strip(left_exp), strip(right_exp)),
        ]
      | _ => []
      };
    syntactic_splits
    @ positive_literal_splits(power_op, exponent)
    |> List.map(((left_exp, right_exp)) =>
         times_exp_for_power_op(
           power_op,
           power_exp_with_op(power_op, base, left_exp),
           power_exp_with_op(power_op, base, right_exp),
         )
       );
  | ("alg.power_mul", BinOp(power_op, base, exponent))
      when is_power_op(power_op) =>
    positive_literal_factor_splits(power_op, exponent)
    |> List.map(((left_exp, right_exp)) =>
         power_exp_with_op(
           power_op,
           power_exp_with_op(power_op, base, left_exp),
           right_exp,
         )
       )
  | _ =>
    let cleanup_rewrites =
      switch (Axioms.cleanup_capability_for_id(rule_id)) {
      | Some(capability)
          when rule_id == Axioms.cleanup_capability_label(capability) =>
        let primitive_rule_id =
          Axioms.primitive_rule_id_for_cleanup(capability);
        let primitive_rewrites =
          primitive_rule_id == rule_id
            ? [] : apply_rule_at_root(primitive_rule_id, exp);
        switch (
          DifferentiationRewrite.cleanup_once(
            ~cleanup_enabled=candidate => candidate == capability,
            exp,
          )
        ) {
        | Some((after_exp, _)) => [after_exp, ...primitive_rewrites]
        | None => primitive_rewrites
        };
      | Some(_)
      | None => []
      };
    AlgebraIdentityRewrite.apply_rule_at_root(rule_id, exp)
    @ TrigRewrite.apply_rule_at_root(rule_id, exp)
    @ DifferentiationRewrite.applicable_at_root(
        ~rule_enabled=candidate_rule_id => candidate_rule_id == rule_id,
        exp,
      )
    |> List.map((rewrite: TrigRewrite.rewrite) => rewrite.after_exp)
    |> List.append(cleanup_rewrites);
  };
};

let rebuild_bin_op = (op, left, right) => Exp.fresh(BinOp(op, left, right));

let rebuild_un_op = (op, exp) => Exp.fresh(UnOp(op, exp));

let rebuild_parens = exp => Exp.fresh(Parens(exp));

let rebuild_ap = (dir, fn, arg) => Exp.fresh(Ap(dir, fn, arg));

let rebuild_tuple = entries => Exp.fresh(Tuple(entries));

let apply_rule_everywhere = (rule, exp): list(application) => {
  let occurrence = ref(0);
  let rec walk = exp => {
    let exp = strip(exp);
    let root_apps =
      apply_rule_at_root(rule.Axioms.id, exp)
      |> List.map(after_exp => {
           occurrence := occurrence^ + 1;
           {
             rule,
             before_full_exp: exp,
             after_full_exp: after_exp,
             before_exp: exp,
             after_exp,
             occurrence: occurrence^,
           };
         });
    let child_apps =
      switch (exp.term) {
      | BinOp(op, left, right) =>
        let left_apps =
          walk(left)
          |> List.map(app =>
               {
                 ...app,
                 before_full_exp: exp,
                 after_full_exp:
                   rebuild_bin_op(op, app.after_full_exp, right),
               }
             );
        let right_apps =
          walk(right)
          |> List.map(app =>
               {
                 ...app,
                 before_full_exp: exp,
                 after_full_exp: rebuild_bin_op(op, left, app.after_full_exp),
               }
             );
        left_apps @ right_apps;
      | UnOp(op, inner) =>
        walk(inner)
        |> List.map(app =>
             {
               ...app,
               before_full_exp: exp,
               after_full_exp: rebuild_un_op(op, app.after_full_exp),
             }
           )
      | Parens(inner) =>
        walk(inner)
        |> List.map(app =>
             {
               ...app,
               before_full_exp: exp,
               after_full_exp: rebuild_parens(app.after_full_exp),
             }
           )
      | Ap(dir, fn, arg) =>
        let fn_apps =
          walk(fn)
          |> List.map(app =>
               {
                 ...app,
                 before_full_exp: exp,
                 after_full_exp: rebuild_ap(dir, app.after_full_exp, arg),
               }
             );
        let arg_apps =
          walk(arg)
          |> List.map(app =>
               {
                 ...app,
                 before_full_exp: exp,
                 after_full_exp: rebuild_ap(dir, fn, app.after_full_exp),
               }
             );
        fn_apps @ arg_apps;
      | Tuple(entries) =>
        let rec walk_entries = (before, remaining) =>
          switch (remaining) {
          | [] => []
          | [entry, ...rest] =>
            (
              walk(entry)
              |> List.map(app =>
                   {
                     ...app,
                     before_full_exp: exp,
                     after_full_exp:
                       rebuild_tuple(
                         List.rev(before) @ [app.after_full_exp, ...rest],
                       ),
                   }
                 )
            )
            @ walk_entries([entry, ...before], rest)
          };
        walk_entries([], entries);
      | _ => []
      };
    root_apps @ child_apps;
  };
  walk(exp);
};

let application_to_prover_step = (app: application) =>
  ProofTrace.prover_step_at(
    ~origin=ProofTrace.Normalization,
    ~rule_id=app.rule.id,
    ~before_full_exp=app.before_full_exp,
    ~after_full_exp=app.after_full_exp,
    ~before_exp=app.before_exp,
    ~after_exp=app.after_exp,
    ~occurrence=app.occurrence,
    ~detail="bounded axiom search",
  );

let targeted_reorder =
    (~rule_id, ~candidates, source, target): option(result) =>
  switch (rule_by_id(rule_id)) {
  | None => None
  | Some(rule) =>
    let source = strip(source);
    let target = strip(target);
    let root_target_is_small_permutation =
      candidates(source)
      |> List.exists(candidate => exp_same(candidate, target));
    let matching_app =
      apply_rule_everywhere(rule, source)
      |> List.find_opt((app: application) =>
           exp_same(app.after_full_exp, target)
         );
    switch (matching_app) {
    | Some(app) =>
      Some({
        source,
        target,
        steps: [application_to_prover_step(app)],
        applications: [app],
      })
    | None when root_target_is_small_permutation =>
      let app = {
        rule,
        before_full_exp: source,
        after_full_exp: target,
        before_exp: source,
        after_exp: target,
        occurrence: 1,
      };
      Some({
        source,
        target,
        steps: [application_to_prover_step(app)],
        applications: [app],
      });
    | None => None
    };
  };

let targeted_addition_reorder = (source, target): option(result) =>
  targeted_reorder(
    ~rule_id="arith.reorder_add_terms",
    ~candidates=small_addition_permutations,
    source,
    target,
  );

let targeted_multiplication_reorder = (source, target): option(result) =>
  targeted_reorder(
    ~rule_id="arith.reorder_mul_factors",
    ~candidates=small_multiplication_permutations,
    source,
    target,
  );

let targeted_rule_transform = (~rule_id, ~transform, source, target) =>
  switch (rule_by_id(rule_id), transform(source)) {
  | (Some(rule), Some(transformed)) when exp_same(transformed, target) =>
    let source = strip(source);
    let target = strip(target);
    let app = {
      rule,
      before_full_exp: source,
      after_full_exp: target,
      before_exp: source,
      after_exp: target,
      occurrence: 1,
    };
    Some({
      source,
      target,
      steps: [application_to_prover_step(app)],
      applications: [app],
    });
  | _ => None
  };

let string_starts_with = (prefix, value) => {
  let prefix_len = String.length(prefix);
  String.length(value) >= prefix_len
  && String.sub(value, 0, prefix_len) == prefix;
};

let group_name_for_rule_ids = rule_ids =>
  rule_ids |> List.exists(string_starts_with("trig."))
    ? Some("trigonometry")
    : rule_ids |> List.exists(string_starts_with("alg."))
        ? Some("algebra") : Some("arithmetic");

let trace_summary = (result: result): ProofTrace.trace_summary => {
  let rule_ids =
    result.applications
    |> List.map((app: application) => app.rule.id)
    |> MathRewriteUtil.dedup;
  {
    justification: "bounded axiom search",
    group_name: group_name_for_rule_ids(rule_ids),
    from_normal_exp: result.target,
    to_normal_exp: result.target,
    from_rule_ids: rule_ids,
    to_rule_ids: [],
    rule_ids,
    prover_steps: result.steps,
    exportable: result.steps != [],
  };
};

let state_key = exp => Exp.show(strip(exp));

let console_log = message =>
  Js_of_ocaml.Firebug.console##log(Js_of_ocaml.Js.string(message));

let op_string = op =>
  switch (op) {
  | Operators.Int(Operators.Plus)
  | Nat(Plus)
  | SInt(Plus)
  | Float(Plus) => "+"
  | Operators.Int(Operators.Minus)
  | SInt(Minus)
  | Float(Minus) => "-"
  | Operators.Int(Operators.Times)
  | Nat(Times)
  | SInt(Times)
  | Float(Times) => "*"
  | Operators.Int(Operators.Divide)
  | Nat(Divide)
  | SInt(Divide)
  | Float(Divide) => "/"
  | Operators.Int(Operators.Power)
  | Nat(Power)
  | SInt(Power)
  | Float(Power) => "**"
  | _ =>
    Exp.show(
      Exp.fresh(BinOp(op, int_exp(Bigint.zero), int_exp(Bigint.zero))),
    )
  };

let exp_string = exp => {
  let rec loop = exp => {
    let exp = strip(exp);
    switch (exp.term) {
    | EmptyHole => "<hole>"
    | DynamicErrorHole(_) => "<dynamic-error-hole>"
    | Deferral(_) => "<deferral>"
    | Var(name) => name
    | BuiltinFun(name) => name
    | Atom(Int(value))
    | Atom(Nat(value)) => Bigint.to_string(value)
    | Atom(SInt(value)) => string_of_int(value)
    | Atom(Float(value)) => string_of_float(value)
    | BinOp(op, left, right) =>
      "(" ++ loop(left) ++ " " ++ op_string(op) ++ " " ++ loop(right) ++ ")"
    | UnOp(Operators.Int(Operators.Minus), inner)
    | UnOp(Operators.SInt(Operators.Minus), inner)
    | UnOp(Operators.Float(Operators.Minus), inner) => "-" ++ loop(inner)
    | UnOp(_, inner) => "unop(" ++ loop(inner) ++ ")"
    | Ap(Operators.Forward, fn, arg) => loop(fn) ++ "(" ++ loop(arg) ++ ")"
    | Ap(_, fn, arg) => loop(fn) ++ " " ++ loop(arg)
    | Parens(inner) => "(" ++ loop(inner) ++ ")"
    | Asc(inner, _) => loop(inner)
    | _ => Exp.show(exp)
    };
  };
  loop(exp);
};

let level_string = level => Axioms.rewrite_level_label(level);

let rule_ids_string = rule_ids =>
  switch (rule_ids) {
  | [] => "all allowed rules"
  | rule_ids => String.concat(", ", rule_ids)
  };

let log_application = (index, app: application) =>
  console_log(
    Printf.sprintf(
      "  %d. %s occurrence %d: %s  ==>  %s\n     whole: %s  ==>  %s",
      index,
      app.rule.id,
      app.occurrence,
      exp_string(app.before_exp),
      exp_string(app.after_exp),
      exp_string(app.before_full_exp),
      exp_string(app.after_full_exp),
    ),
  );

let log_search_result =
    (~source, ~target, ~level, ~max_depth, ~allowed_rule_ids, result) => {
  console_log(
    Printf.sprintf(
      "[Hazel proof search] %s -> %s; level=%s; max_depth=%d; rules=%s",
      exp_string(source),
      exp_string(target),
      level_string(level),
      max_depth,
      rule_ids_string(allowed_rule_ids),
    ),
  );
  switch (result) {
  | None => console_log("[Hazel proof search] no proof found")
  | Some(result) =>
    console_log(
      Printf.sprintf(
        "[Hazel proof search] found %d rewrite step(s)",
        List.length(result.applications),
      ),
    );
    result.applications
    |> List.iteri((index, app) => log_application(index + 1, app));
  };
};

let unsupported_constructs = Axioms.unsupported_constructs;

let unsupported_construct_ids = Axioms.unsupported_construct_ids;

let unsupported_constructs_message = Axioms.unsupported_constructs_message;

let unsupported_constructs_for_rewrite = Axioms.unsupported_constructs_for_rewrite;

let unsupported_construct_ids_for_rewrite = Axioms.unsupported_construct_ids_for_rewrite;

let unsupported_constructs_message_for_rewrite = Axioms.unsupported_constructs_message_for_rewrite;

type search_node = (Exp.t, list(ProofTrace.prover_step), list(application));

type search_session = {
  source: Exp.t,
  target: Exp.t,
  level: Axioms.rewrite_level,
  max_depth: int,
  max_states: int,
  allowed_rule_ids: list(string),
  rule_use_limits: list((string, int)),
  foreground_rule_ids: list(string),
  max_foreground_uses: int,
  rules: list(Axioms.rewrite_rule),
  depth: int,
  seen: list(string),
  frontier: list(search_node),
  next: list(search_node),
  generated_this_depth: int,
};

type search_progress =
  | SearchPending(search_session)
  | SearchComplete(option(result));

let applications_within_limits =
    (
      ~rule_use_limits,
      ~foreground_rule_ids,
      ~max_foreground_uses,
      applications,
    ) => {
  let count = rule_id =>
    applications
    |> List.fold_left(
         (count, app: application) =>
           app.rule.id == rule_id ? count + 1 : count,
         0,
       );
  let individual_limits_hold =
    rule_use_limits
    |> List.for_all(((rule_id, max_uses)) => count(rule_id) <= max_uses);
  let foreground_uses =
    applications
    |> List.fold_left(
         (count, app: application) =>
           List.mem(app.rule.id, foreground_rule_ids) ? count + 1 : count,
         0,
       );
  individual_limits_hold
  && (max_foreground_uses < 0 || foreground_uses <= max_foreground_uses);
};

let targeted_finish_from = (~level, ~allowed_rule_ids, ~target, exp) => {
  let has_targeted_rule = rule_id =>
    allowed_rules(level)
    |> List.exists((rule: Axioms.rewrite_rule) =>
         rule.id == rule_id
         && (allowed_rule_ids == [] || List.mem(rule.id, allowed_rule_ids))
       );
  /* Literal power splitting has a small, deterministic candidate set. Check
   * those exact catalog transitions before starting the broader incremental
   * search; do not change the scheduling semantics of unrelated rules. */
  let direct_power_result =
    allowed_rules(level)
    @ cleanup_rules(level)
    |> List.filter((rule: Axioms.rewrite_rule) =>
         List.mem(rule.id, ["alg.power_add", "alg.power_mul"])
         && (allowed_rule_ids == [] || List.mem(rule.id, allowed_rule_ids))
       )
    |> List.find_map(rule =>
         apply_rule_everywhere(rule, exp)
         |> List.find_opt((app: application) =>
              exp_same(app.after_full_exp, target)
            )
         |> Option.map(app =>
              {
                source: strip(exp),
                target: strip(target),
                steps: [application_to_prover_step(app)],
                applications: [app],
              }
            )
       );
  let candidates = [
    (
      "arith.const_fold",
      () =>
        targeted_rule_transform(
          ~rule_id="arith.const_fold",
          ~transform=ArithmeticNormalization.fold_rational_constant,
          exp,
          target,
        ),
    ),
    (
      "arith.simplify_scalar_products",
      () => {
        let transformed =
          ArithmeticNormalization.simplify_scalar_products(exp);
        targeted_rule_transform(
          ~rule_id="arith.simplify_scalar_products",
          ~transform=_ => Some(transformed),
          exp,
          target,
        );
      },
    ),
    (
      "arith.reorder_add_terms",
      () => targeted_addition_reorder(exp, target),
    ),
    (
      "arith.reorder_mul_factors",
      () => targeted_multiplication_reorder(exp, target),
    ),
  ];
  switch (direct_power_result) {
  | Some(_) as result => result
  | None =>
    candidates
    |> List.find_map(((rule_id, candidate)) =>
         has_targeted_rule(rule_id) ? candidate() : None
       )
  };
};

let start_search =
    (
      ~level=Axioms.Arithmetic,
      ~max_depth=4,
      ~max_states=250,
      ~allowed_rule_ids=[],
      ~rule_use_limits=[],
      ~foreground_rule_ids=[],
      ~max_foreground_uses=(-1),
      source,
      target,
    ) => {
  let rules =
    allowed_rules(level)
    @ cleanup_rules(level)
    |> List.filter((rule: Axioms.rewrite_rule) =>
         rule.id != "arith.reorder_add_terms"
       )
    |> List.filter((rule: Axioms.rewrite_rule) =>
         rule.id != "arith.reorder_mul_factors"
       )
    |> List.filter((rule: Axioms.rewrite_rule) =>
         allowed_rule_ids == [] || List.mem(rule.id, allowed_rule_ids)
       );
  let source = strip(source);
  let target = strip(target);
  if (has_hole(source)
      || has_hole(target)
      || unsupported_constructs_for_rewrite(~level, ~source, ~target) != []) {
    SearchComplete(None);
  } else {
    switch (
      max_depth > 0
        ? targeted_finish_from(~level, ~allowed_rule_ids, ~target, source)
        : None
    ) {
    | Some(result)
        when
          applications_within_limits(
            ~rule_use_limits,
            ~foreground_rule_ids,
            ~max_foreground_uses,
            result.applications,
          ) =>
      SearchComplete(Some(result))
    | Some(_)
    | None =>
      SearchPending({
        source,
        target,
        level,
        max_depth,
        max_states,
        allowed_rule_ids,
        rule_use_limits,
        foreground_rule_ids,
        max_foreground_uses,
        rules,
        depth: 0,
        seen: [state_key(source)],
        frontier: [(source, [], [])],
        next: [],
        generated_this_depth: 0,
      })
    };
  };
};

let finish_node = (session, (exp, steps, applications): search_node) =>
  if (exp_same(exp, session.target)) {
    Some({
      source: session.source,
      target: exp,
      steps: List.rev(steps),
      applications: List.rev(applications),
    });
  } else {
    switch (
      session.depth < session.max_depth
        ? targeted_finish_from(
            ~level=session.level,
            ~allowed_rule_ids=session.allowed_rule_ids,
            ~target=session.target,
            exp,
          )
        : None
    ) {
    | Some(reorder_result) =>
      let combined_applications = reorder_result.applications @ applications;
      applications_within_limits(
        ~rule_use_limits=session.rule_use_limits,
        ~foreground_rule_ids=session.foreground_rule_ids,
        ~max_foreground_uses=session.max_foreground_uses,
        combined_applications,
      )
        ? Some({
            source: session.source,
            target: session.target,
            steps: List.rev(reorder_result.steps @ steps),
            applications: List.rev(combined_applications),
          })
        : None;
    | None => None
    };
  };

let expand_node = (session, (exp, steps, applications): search_node) => {
  let generation_budget = max(1, session.max_states * 4);
  let capacity = session.max_states - List.length(session.next);
  if (session.depth >= session.max_depth
      || capacity <= 0
      || session.generated_this_depth >= generation_budget) {
    session;
  } else {
    let remaining_generation =
      generation_budget - session.generated_this_depth;
    let candidates =
      session.rules
      |> List.concat_map(rule =>
           apply_rule_everywhere(rule, exp)
           |> List.filter(app =>
                applications_within_limits(
                  ~rule_use_limits=session.rule_use_limits,
                  ~foreground_rule_ids=session.foreground_rule_ids,
                  ~max_foreground_uses=session.max_foreground_uses,
                  [app, ...applications],
                )
              )
           |> List.map(app =>
                (
                  strip(app.after_full_exp),
                  [application_to_prover_step(app), ...steps],
                  [app, ...applications],
                )
              )
         )
      |> ListUtil.take(remaining_generation);
    let generated_this_depth =
      session.generated_this_depth + List.length(candidates);
    let rec add_unseen = (seen, next, remaining, candidates) =>
      switch (candidates) {
      | [] => (seen, next)
      | _ when remaining <= 0 => (seen, next)
      | [(candidate_exp, _, _) as candidate, ...rest] =>
        let key = state_key(candidate_exp);
        List.mem(key, seen)
          ? add_unseen(seen, next, remaining, rest)
          : add_unseen(
              [key, ...seen],
              [candidate, ...next],
              remaining - 1,
              rest,
            );
      };
    let (seen, next) =
      add_unseen(session.seen, session.next, capacity, candidates);
    {
      ...session,
      seen,
      next,
      generated_this_depth,
    };
  };
};

/* Process only a bounded number of frontier nodes. Browser callers can yield
   between slices, while synchronous callers retain the original API below. */
let rec continue_search = (~work_budget=1, progress) =>
  switch (progress) {
  | SearchComplete(_) => progress
  | SearchPending(session) =>
    if (work_budget <= 0) {
      progress;
    } else {
      switch (session.frontier) {
      | [node, ...rest] =>
        switch (finish_node(session, node)) {
        | Some(result) => SearchComplete(Some(result))
        | None =>
          let session =
            expand_node(
              {
                ...session,
                frontier: rest,
              },
              node,
            );
          continue_search(
            ~work_budget=work_budget - 1,
            SearchPending(session),
          );
        }
      | [] =>
        if (session.next == [] || session.depth >= session.max_depth) {
          SearchComplete(None);
        } else {
          continue_search(
            ~work_budget,
            SearchPending({
              ...session,
              depth: session.depth + 1,
              frontier: List.rev(session.next),
              next: [],
              generated_this_depth: 0,
            }),
          );
        }
      };
    }
  };

let search =
    (
      ~level=Axioms.Arithmetic,
      ~max_depth=4,
      ~max_states=250,
      ~allowed_rule_ids=[],
      ~rule_use_limits=[],
      ~foreground_rule_ids=[],
      ~max_foreground_uses=(-1),
      ~log=true,
      source,
      target,
    ) => {
  let rec finish = progress =>
    switch (continue_search(~work_budget=max_states, progress)) {
    | SearchComplete(result) => result
    | SearchPending(_) as progress => finish(progress)
    };
  let result =
    start_search(
      ~level,
      ~max_depth,
      ~max_states,
      ~allowed_rule_ids,
      ~rule_use_limits,
      ~foreground_rule_ids,
      ~max_foreground_uses,
      source,
      target,
    )
    |> finish;
  if (log) {
    log_search_result(
      ~source,
      ~target,
      ~level,
      ~max_depth,
      ~allowed_rule_ids,
      result,
    );
  };
  result;
};
