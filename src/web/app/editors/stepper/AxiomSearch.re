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
  steps: list(RewriteChecker.prover_step),
  applications: list(application),
};

let strip = exp =>
  exp |> DHExp.strip_ascriptions |> RewriteChecker.strip_math_wrappers;

let exp_same = (left, right) =>
  RewriteChecker.exp_same(strip(left), strip(right))
  || TrigRewrite.exp_same(strip(left), strip(right));

let int_exp = RewriteChecker.int_exp;
let plus_exp = RewriteChecker.plus_exp;
let times_exp = RewriteChecker.times_exp;

let power_exp_with_op = (op, base, exponent) =>
  Exp.fresh(BinOp(op, base, exponent));

let rule_by_id = rule_id =>
  Axioms.rewrite_groups
  |> List.filter_map(group => Axioms.rewrite_rule_by_id(group, rule_id))
  |> ListUtil.hd_opt;

let allowed_rules = level =>
  Axioms.allowed_groups(level)
  |> List.concat_map((group: Axioms.rewrite_group) => group.rules);

let allowed_rule_ids = level =>
  allowed_rules(level) |> List.map((rule: Axioms.rewrite_rule) => rule.id);

let int_constant = exp => RewriteChecker.int_constant(strip(exp));

let is_power_op =
  fun
  | Operators.Int(Operators.Power)
  | Nat(Power)
  | SInt(Power)
  | Float(Power) => true
  | _ => false;

let rec has_hole = exp =>
  switch (strip(exp).term) {
  | EmptyHole
  | DynamicErrorHole(_) => true
  | BinOp(_, left, right) => has_hole(left) || has_hole(right)
  | UnOp(_, inner)
  | Parens(inner) => has_hole(inner)
  | Ap(_, fn, arg) => has_hole(fn) || has_hole(arg)
  | _ => false
  };

let rec flatten_plus = exp => {
  let exp = strip(exp);
  switch (exp.term) {
  | BinOp(op, left, right) when RewriteChecker.is_plus_op(op) =>
    flatten_plus(left) @ flatten_plus(right)
  | _ => [exp]
  };
};

let rec flatten_times = exp => {
  let exp = strip(exp);
  switch (exp.term) {
  | BinOp(op, left, right) when RewriteChecker.is_times_op(op) =>
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

let positive_literal_splits = exponent =>
  switch (int_constant(exponent)) {
  | Some(value) =>
    switch (Bigint.to_int(value)) {
    | Some(value) when value > 1 =>
      List.init(
        value - 1,
        index => {
          let left = index + 1;
          let right = value - left;
          (int_exp(Bigint.of_int(left)), int_exp(Bigint.of_int(right)));
        },
      )
    | _ => []
    }
  | _ => []
  };

let positive_literal_factor_splits = exponent =>
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
                     int_exp(Bigint.of_int(left)),
                     int_exp(Bigint.of_int(right)),
                   ))
                 : None;
             }
             : None
         )
    | _ => []
    }
  | _ => []
  };

let apply_rule_at_root = (rule_id, exp: Exp.t): list(Exp.t) => {
  let exp = strip(exp);
  switch (rule_id, exp.term) {
  | ("arith.add_comm", BinOp(op, left, right))
      when RewriteChecker.is_plus_op(op) => [
      RewriteChecker.plus_exp_with_op(op, right, left),
    ]
  | ("arith.mul_comm", BinOp(op, left, right))
      when RewriteChecker.is_times_op(op) => [
      RewriteChecker.times_exp_with_op(op, right, left),
    ]
  | ("arith.add_assoc", BinOp(op, {term: BinOp(inner_op, a, b), _}, c))
      when
        RewriteChecker.is_plus_op(op) && RewriteChecker.is_plus_op(inner_op) => [
      RewriteChecker.plus_exp_with_op(
        op,
        a,
        RewriteChecker.plus_exp_with_op(op, b, c),
      ),
    ]
  | ("arith.add_assoc", BinOp(op, a, {term: BinOp(inner_op, b, c), _}))
      when
        RewriteChecker.is_plus_op(op) && RewriteChecker.is_plus_op(inner_op) => [
      RewriteChecker.plus_exp_with_op(
        op,
        RewriteChecker.plus_exp_with_op(op, a, b),
        c,
      ),
    ]
  | ("arith.mul_assoc", BinOp(op, {term: BinOp(inner_op, a, b), _}, c))
      when
        RewriteChecker.is_times_op(op)
        && RewriteChecker.is_times_op(inner_op) => [
      RewriteChecker.times_exp_with_op(
        op,
        a,
        RewriteChecker.times_exp_with_op(op, b, c),
      ),
    ]
  | ("arith.mul_assoc", BinOp(op, a, {term: BinOp(inner_op, b, c), _}))
      when
        RewriteChecker.is_times_op(op)
        && RewriteChecker.is_times_op(inner_op) => [
      RewriteChecker.times_exp_with_op(
        op,
        RewriteChecker.times_exp_with_op(op, a, b),
        c,
      ),
    ]
  | ("arith.add_zero", BinOp(op, left, right))
      when RewriteChecker.is_plus_op(op) =>
    switch (int_constant(left), int_constant(right)) {
    | (Some(value), _) when Bigint.equal(value, Bigint.zero) => [
        strip(right),
      ]
    | (_, Some(value)) when Bigint.equal(value, Bigint.zero) => [
        strip(left),
      ]
    | _ => []
    }
  | ("arith.const_fold", BinOp(op, left, right))
      when RewriteChecker.is_plus_op(op) =>
    switch (int_constant(left), int_constant(right)) {
    | (Some(left), Some(right)) => [int_exp(Bigint.(+)(left, right))]
    | _ => []
    }
  | ("arith.const_fold", BinOp(op, left, right))
      when RewriteChecker.is_times_op(op) =>
    switch (int_constant(left), int_constant(right)) {
    | (Some(left), Some(right)) => [int_exp(Bigint.( * )(left, right))]
    | _ => []
    }
  | ("arith.reorder_add_terms", _) => small_addition_permutations(exp)
  | ("arith.reorder_mul_factors", _) =>
    small_multiplication_permutations(exp)
  | (
      "alg.distribute_mul_add",
      BinOp(op, left, {term: BinOp(plus_op, add_left, add_right), _}),
    )
      when
        RewriteChecker.is_times_op(op) && RewriteChecker.is_plus_op(plus_op) => [
      RewriteChecker.plus_exp_with_op(
        plus_op,
        RewriteChecker.times_exp_with_op(op, left, add_left),
        RewriteChecker.times_exp_with_op(op, left, add_right),
      ),
    ]
  | (
      "alg.distribute_mul_add",
      BinOp(op, {term: BinOp(plus_op, add_left, add_right), _}, right),
    )
      when
        RewriteChecker.is_times_op(op) && RewriteChecker.is_plus_op(plus_op) => [
      RewriteChecker.plus_exp_with_op(
        plus_op,
        RewriteChecker.times_exp_with_op(op, add_left, right),
        RewriteChecker.times_exp_with_op(op, add_right, right),
      ),
    ]
  | ("alg.distribute_div_add", _) =>
    RewriteChecker.distribute_div_over_add_candidates(exp)
  | ("alg.factor_common", BinOp(plus_op, left, right))
      when RewriteChecker.is_plus_op(plus_op) =>
    switch (
      RewriteChecker.factors_of_product(left),
      RewriteChecker.factors_of_product(right),
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
          when RewriteChecker.is_plus_op(plus_op) => [
          (strip(left_exp), strip(right_exp)),
        ]
      | _ => []
      };
    syntactic_splits
    @ positive_literal_splits(exponent)
    |> List.map(((left_exp, right_exp)) =>
         times_exp(
           power_exp_with_op(power_op, base, left_exp),
           power_exp_with_op(power_op, base, right_exp),
         )
       );
  | ("alg.power_mul", BinOp(power_op, base, exponent))
      when is_power_op(power_op) =>
    positive_literal_factor_splits(exponent)
    |> List.map(((left_exp, right_exp)) =>
         power_exp_with_op(
           power_op,
           power_exp_with_op(power_op, base, left_exp),
           right_exp,
         )
       )
  | _ =>
    AlgebraIdentityRewrite.apply_rule_at_root(rule_id, exp)
    @ TrigRewrite.apply_rule_at_root(rule_id, exp)
    |> List.map((rewrite: TrigRewrite.rewrite) => rewrite.after_exp)
  };
};

let rebuild_bin_op = (op, left, right) => Exp.fresh(BinOp(op, left, right));

let rebuild_un_op = (op, exp) => Exp.fresh(UnOp(op, exp));

let rebuild_parens = exp => Exp.fresh(Parens(exp));

let rebuild_ap = (dir, fn, arg) => Exp.fresh(Ap(dir, fn, arg));

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
      | _ => []
      };
    root_apps @ child_apps;
  };
  walk(exp);
};

let application_to_prover_step = (app: application) =>
  RewriteChecker.prover_step_at(
    ~origin=Normalization,
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

let trace_summary = (result: result): RewriteChecker.trace_summary => {
  let rule_ids =
    result.applications
    |> List.map((app: application) => app.rule.id)
    |> RewriteChecker.dedup;
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

let search =
    (
      ~level=Axioms.Arithmetic,
      ~max_depth=4,
      ~max_states=250,
      ~allowed_rule_ids=[],
      ~log=true,
      source,
      target,
    ) => {
  let rules =
    allowed_rules(level)
    |> List.filter((rule: Axioms.rewrite_rule) =>
         rule.id != "arith.reorder_add_terms"
       )
    |> List.filter((rule: Axioms.rewrite_rule) =>
         rule.id != "arith.reorder_mul_factors"
       )
    |> List.filter((rule: Axioms.rewrite_rule) =>
         allowed_rule_ids == [] || List.mem(rule.id, allowed_rule_ids)
       );
  let has_targeted_rule = rule_id =>
    allowed_rules(level)
    |> List.exists((rule: Axioms.rewrite_rule) =>
         rule.id == rule_id
         && (allowed_rule_ids == [] || List.mem(rule.id, allowed_rule_ids))
       );
  let target = strip(target);
  if (has_hole(source)
      || has_hole(target)
      || unsupported_constructs_for_rewrite(~level, ~source, ~target) != []) {
    None;
  } else {
    let targeted_reorder_from = exp =>
      switch (
        has_targeted_rule("arith.reorder_add_terms"),
        has_targeted_rule("arith.reorder_mul_factors"),
      ) {
      | (true, _) =>
        switch (targeted_addition_reorder(exp, target)) {
        | Some(result) => Some(result)
        | None when has_targeted_rule("arith.reorder_mul_factors") =>
          targeted_multiplication_reorder(exp, target)
        | None => None
        }
      | (false, true) => targeted_multiplication_reorder(exp, target)
      | (false, false) => None
      };
    let targeted_result = targeted_reorder_from(source);
    switch (targeted_result) {
    | Some(result) =>
      if (log) {
        log_search_result(
          ~source,
          ~target,
          ~level,
          ~max_depth,
          ~allowed_rule_ids,
          Some(result),
        );
      };
      Some(result);
    | None =>
      let generated_this_depth = ref(0);
      let generation_budget = max(1, max_states * 4);
      let rec bounded_concat_map = (items, f) =>
        switch (items) {
        | [] => []
        | [item, ...rest] =>
          generated_this_depth^ >= generation_budget
            ? [] : f(item) @ bounded_concat_map(rest, f)
        };
      let rec bfs = (depth, seen, frontier) =>
        if (depth > max_depth) {
          None;
        } else {
          let finish_state = ((exp, steps, applications)) =>
            if (exp_same(exp, target)) {
              Some({
                source: strip(source),
                target: exp,
                steps: List.rev(steps),
                applications: List.rev(applications),
              });
            } else {
              switch (targeted_reorder_from(exp)) {
              | Some(reorder_result) =>
                Some({
                  source: strip(source),
                  target,
                  steps: List.rev(reorder_result.steps @ steps),
                  applications:
                    List.rev(reorder_result.applications @ applications),
                })
              | None => None
              };
            };
          switch (
            frontier |> List.filter_map(finish_state) |> ListUtil.hd_opt
          ) {
          | Some(result) => Some(result)
          | None =>
            generated_this_depth := 0;
            let next =
              bounded_concat_map(frontier, ((exp, steps, applications)) =>
                bounded_concat_map(rules, rule =>
                  apply_rule_everywhere(rule, exp)
                  |> List.filter_map(app =>
                       if (generated_this_depth^ >= generation_budget) {
                         None;
                       } else {
                         generated_this_depth := generated_this_depth^ + 1;
                         Some((
                           strip(app.after_full_exp),
                           [application_to_prover_step(app), ...steps],
                           [app, ...applications],
                         ));
                       }
                     )
                )
              )
              |> List.filter(((exp, _, _)) =>
                   !List.mem(state_key(exp), seen)
                 );
            let next = ListUtil.take(max_states, next);
            let seen =
              seen @ (next |> List.map(((exp, _, _)) => state_key(exp)));
            next == [] ? None : bfs(depth + 1, seen, next);
          };
        };
      let result =
        bfs(0, [state_key(source)], [(strip(source), [], [])]);
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
  };
};
