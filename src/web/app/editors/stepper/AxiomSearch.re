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
  | _ =>
    TrigRewrite.apply_rule_at_root(rule_id, exp)
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

let targeted_addition_reorder = (source, target): option(result) =>
  switch (rule_by_id("arith.reorder_add_terms")) {
  | None => None
  | Some(rule) =>
    let source = strip(source);
    let target = strip(target);
    let target_is_small_permutation =
      small_addition_permutations(source)
      |> List.exists(candidate => exp_same(candidate, target));
    if (target_is_small_permutation) {
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
    } else {
      None;
    };
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
         allowed_rule_ids == [] || List.mem(rule.id, allowed_rule_ids)
       );
  let target = strip(target);
  if (has_hole(source) || has_hole(target)) {
    None;
  } else {
    switch (targeted_addition_reorder(source, target)) {
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
          switch (
            frontier
            |> List.find_opt(((exp, _, _)) => exp_same(exp, target))
          ) {
          | Some((exp, steps, applications)) =>
            Some({
              source: strip(source),
              target: exp,
              steps: List.rev(steps),
              applications: List.rev(applications),
            })
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
