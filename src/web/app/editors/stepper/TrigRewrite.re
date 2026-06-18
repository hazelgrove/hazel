open Language;

type op_kind =
  | Add
  | Sub
  | Mul
  | Div
  | Pow;

type pat =
  | Meta(string)
  | VarName(string)
  | IntLit(int)
  | Pi
  | App(string, pat)
  | Bin(op_kind, pat, pat)
  | Neg(pat);

type spec = {
  rule_id: string,
  label: string,
  left: pat,
  right: pat,
};

type rewrite = {
  rule_id: string,
  label: string,
  before_exp: Exp.t,
  after_exp: Exp.t,
};

let strip = exp => {
  let rec loop = exp =>
    switch ((exp |> DHExp.strip_ascriptions).term) {
    | Parens(inner)
    | Asc(inner, _) => loop(inner)
    | _ => exp |> DHExp.strip_ascriptions
    };
  loop(exp);
};

let int_exp = value => Exp.fresh(Atom(Int(Bigint.of_int(value))));
let float_exp = value => Exp.fresh(Atom(Float(value)));
let var_exp = name => Exp.fresh(Var(name));
let app_exp = (name, arg) =>
  Exp.fresh(Ap(Operators.Forward, var_exp(name), arg));
let plus_exp = (left, right) =>
  Exp.fresh(BinOp(Operators.Int(Operators.Plus), left, right));
let minus_exp = (left, right) =>
  Exp.fresh(BinOp(Operators.Int(Operators.Minus), left, right));
let times_exp = (left, right) =>
  Exp.fresh(BinOp(Operators.Int(Operators.Times), left, right));
let divide_exp = (left, right) =>
  Exp.fresh(BinOp(Operators.Int(Operators.Divide), left, right));
let power_exp = (left, right) =>
  Exp.fresh(BinOp(Operators.Int(Operators.Power), left, right));
let neg_exp = exp => Exp.fresh(UnOp(Operators.Int(Operators.Minus), exp));

let is_float_pi = value => abs_float(value -. Float.pi) < 0.000001;

let standard_bin_op = op =>
  switch (op) {
  | Operators.Int(Plus)
  | Nat(Plus)
  | SInt(Plus)
  | Float(Plus) => Some(Operators.Int(Operators.Plus))
  | Operators.Int(Minus)
  | Nat(Minus)
  | SInt(Minus)
  | Float(Minus) => Some(Operators.Int(Operators.Minus))
  | Operators.Int(Times)
  | Nat(Times)
  | SInt(Times)
  | Float(Times) => Some(Operators.Int(Operators.Times))
  | Operators.Int(Divide)
  | Nat(Divide)
  | SInt(Divide)
  | Float(Divide) => Some(Operators.Int(Operators.Divide))
  | Operators.Int(Power)
  | Nat(Power)
  | SInt(Power)
  | Float(Power) => Some(Operators.Int(Operators.Power))
  | _ => None
  };

let rec canonical = exp => {
  let exp = strip(exp);
  switch (exp.term) {
  | Atom(Float(value)) when is_float_pi(value) => var_exp("pi")
  | Atom(Float(value)) when value == Float.round(value) =>
    int_exp(int_of_float(value))
  | BuiltinFun("sin")
  | BuiltinFun("cos")
  | BuiltinFun("tan") =>
    var_exp(
      switch (exp.term) {
      | BuiltinFun(name) => name
      | _ => ""
      },
    )
  | BinOp(op, left, right) =>
    switch (standard_bin_op(op)) {
    | Some(op) => Exp.fresh(BinOp(op, canonical(left), canonical(right)))
    | None => Exp.fresh(BinOp(op, canonical(left), canonical(right)))
    }
  | UnOp(Operators.Float(Operators.Minus), inner) =>
    neg_exp(canonical(inner))
  | UnOp(op, inner) => Exp.fresh(UnOp(op, canonical(inner)))
  | Ap(dir, fn, arg) => Exp.fresh(Ap(dir, canonical(fn), canonical(arg)))
  | Parens(inner)
  | Asc(inner, _) => canonical(inner)
  | _ => exp
  };
};

let exp_same = (left, right) =>
  Exp.fast_equal(canonical(left), canonical(right));

let rec lookup_meta = (name, env) =>
  switch (env) {
  | [] => None
  | [(existing, exp), ...rest] =>
    existing == name ? Some(exp) : lookup_meta(name, rest)
  };

let bind_meta = (name, exp, env) =>
  switch (lookup_meta(name, env)) {
  | Some(existing) => exp_same(existing, exp) ? Some(env) : None
  | None => Some([(name, canonical(exp)), ...env])
  };

let int_constant = exp => {
  let exp = strip(exp);
  switch (exp.term) {
  | Atom(Int(value))
  | Atom(Nat(value)) => Bigint.to_int(value)
  | Atom(SInt(value)) => Some(value)
  | Atom(Float(value)) when value == Float.round(value) =>
    Some(int_of_float(value))
  | _ => None
  };
};

let is_pi_exp = exp => {
  let exp = strip(exp);
  switch (exp.term) {
  | Var("pi") => true
  | Atom(Float(value)) when is_float_pi(value) => true
  | _ => false
  };
};

let op_matches = (kind, op) =>
  switch (kind, op) {
  | (
      Add,
      Operators.Int(Operators.Plus) | Nat(Plus) | SInt(Plus) | Float(Plus),
    ) =>
    true
  | (Sub, Operators.Int(Operators.Minus) | SInt(Minus) | Float(Minus)) =>
    true
  | (
      Mul,
      Operators.Int(Operators.Times) | Nat(Times) | SInt(Times) |
      Float(Times),
    ) =>
    true
  | (
      Div,
      Operators.Int(Operators.Divide) | Nat(Divide) | SInt(Divide) |
      Float(Divide),
    ) =>
    true
  | (
      Pow,
      Operators.Int(Operators.Power) | Nat(Power) | SInt(Power) |
      Float(Power),
    ) =>
    true
  | _ => false
  };

let function_name = exp => {
  let exp = strip(exp);
  switch (exp.term) {
  | Var(name)
  | BuiltinFun(name) => Some(name)
  | _ => None
  };
};

let rec match_pat = (pat, exp, env) => {
  let exp = canonical(exp);
  switch (pat, exp.term) {
  | (Meta(name), _) => bind_meta(name, exp, env)
  | (VarName(name), Var(actual)) when name == actual => Some(env)
  | (Pi, _) when is_pi_exp(exp) => Some(env)
  | (IntLit(value), _) =>
    switch (int_constant(exp)) {
    | Some(actual) when actual == value => Some(env)
    | _ => None
    }
  | (App(name, arg_pat), Ap(Operators.Forward, fn, arg)) =>
    switch (function_name(fn)) {
    | Some(actual) when name == actual => match_pat(arg_pat, arg, env)
    | _ => None
    }
  | (Bin(kind, left_pat, right_pat), BinOp(op, left, right))
      when op_matches(kind, op) =>
    switch (match_pat(left_pat, left, env)) {
    | Some(env) => match_pat(right_pat, right, env)
    | None => None
    }
  | (
      Neg(inner_pat),
      UnOp(
        Operators.Int(Operators.Minus) | SInt(Minus) |
        Operators.Float(Operators.Minus),
        inner,
      ),
    ) =>
    match_pat(inner_pat, inner, env)
  | _ => None
  };
};

let rec instantiate = (pat, env) =>
  switch (pat) {
  | Meta(name) =>
    lookup_meta(name, env) |> Option.value(~default=var_exp(name))
  | VarName(name) => var_exp(name)
  | IntLit(value) => int_exp(value)
  | Pi => var_exp("pi")
  | App(name, arg) => app_exp(name, instantiate(arg, env))
  | Bin(Add, left, right) =>
    plus_exp(instantiate(left, env), instantiate(right, env))
  | Bin(Sub, left, right) =>
    minus_exp(instantiate(left, env), instantiate(right, env))
  | Bin(Mul, left, right) =>
    times_exp(instantiate(left, env), instantiate(right, env))
  | Bin(Div, left, right) =>
    divide_exp(instantiate(left, env), instantiate(right, env))
  | Bin(Pow, left, right) =>
    power_exp(instantiate(left, env), instantiate(right, env))
  | Neg(inner) => neg_exp(instantiate(inner, env))
  };

let m = name => Meta(name);
let i = value => IntLit(value);
let v = name => VarName(name);
let add = (a, b) => Bin(Add, a, b);
let sub = (a, b) => Bin(Sub, a, b);
let mul = (a, b) => Bin(Mul, a, b);
let div = (a, b) => Bin(Div, a, b);
let pow = (a, b) => Bin(Pow, a, b);
let sin = x => App("sin", x);
let cos = x => App("cos", x);
let tan = x => App("tan", x);
let pi = Pi;
let pi_over_two = div(pi, i(2));

let specs = [
  {
    rule_id: "trig.pythagorean_sin_cos",
    label: "Pythagorean identity",
    left: add(pow(sin(m("x")), i(2)), pow(cos(m("x")), i(2))),
    right: i(1),
  },
  {
    rule_id: "trig.pythagorean_cos_sin",
    label: "Pythagorean identity",
    left: add(pow(cos(m("x")), i(2)), pow(sin(m("x")), i(2))),
    right: i(1),
  },
  {
    rule_id: "trig.cos_squared_pythagorean",
    label: "cosine squared Pythagorean form",
    left: pow(cos(m("x")), i(2)),
    right: sub(i(1), pow(sin(m("x")), i(2))),
  },
  {
    rule_id: "trig.sin_squared_pythagorean",
    label: "sine squared Pythagorean form",
    left: pow(sin(m("x")), i(2)),
    right: sub(i(1), pow(cos(m("x")), i(2))),
  },
  {
    rule_id: "trig.tan_squared_pythagorean",
    label: "tangent squared Pythagorean form",
    left: add(i(1), pow(tan(m("x")), i(2))),
    right: div(i(1), pow(cos(m("x")), i(2))),
  },
  {
    rule_id: "trig.sin_sum",
    label: "sine sum",
    left: sin(add(m("a"), m("b"))),
    right:
      add(
        mul(sin(m("a")), cos(m("b"))),
        mul(cos(m("a")), sin(m("b"))),
      ),
  },
  {
    rule_id: "trig.sin_diff",
    label: "sine difference",
    left: sin(sub(m("a"), m("b"))),
    right:
      sub(
        mul(sin(m("a")), cos(m("b"))),
        mul(cos(m("a")), sin(m("b"))),
      ),
  },
  {
    rule_id: "trig.cos_sum",
    label: "cosine sum",
    left: cos(add(m("a"), m("b"))),
    right:
      sub(
        mul(cos(m("a")), cos(m("b"))),
        mul(sin(m("a")), sin(m("b"))),
      ),
  },
  {
    rule_id: "trig.cos_diff",
    label: "cosine difference",
    left: cos(sub(m("a"), m("b"))),
    right:
      add(
        mul(cos(m("a")), cos(m("b"))),
        mul(sin(m("a")), sin(m("b"))),
      ),
  },
  {
    rule_id: "trig.sin_double",
    label: "sine double-angle",
    left: sin(mul(i(2), m("x"))),
    right: mul(mul(i(2), sin(m("x"))), cos(m("x"))),
  },
  {
    rule_id: "trig.cos_double_square",
    label: "cosine double-angle",
    left: cos(mul(i(2), m("x"))),
    right: sub(pow(cos(m("x")), i(2)), pow(sin(m("x")), i(2))),
  },
  {
    rule_id: "trig.cos_double_cos",
    label: "cosine double-angle with cos squared",
    left: cos(mul(i(2), m("x"))),
    right: sub(mul(i(2), pow(cos(m("x")), i(2))), i(1)),
  },
  {
    rule_id: "trig.cos_double_sin",
    label: "cosine double-angle with sin squared",
    left: cos(mul(i(2), m("x"))),
    right: sub(i(1), mul(i(2), pow(sin(m("x")), i(2)))),
  },
  {
    rule_id: "trig.sin_half_squared",
    label: "sine squared half-angle",
    left: pow(sin(div(m("x"), i(2))), i(2)),
    right: div(sub(i(1), cos(m("x"))), i(2)),
  },
  {
    rule_id: "trig.cos_half_squared",
    label: "cosine squared half-angle",
    left: pow(cos(div(m("x"), i(2))), i(2)),
    right: div(add(i(1), cos(m("x"))), i(2)),
  },
  {
    rule_id: "trig.sin_cofunction",
    label: "sine cofunction",
    left: sin(sub(pi_over_two, m("x"))),
    right: cos(m("x")),
  },
  {
    rule_id: "trig.cos_cofunction",
    label: "cosine cofunction",
    left: cos(sub(pi_over_two, m("x"))),
    right: sin(m("x")),
  },
  {
    rule_id: "trig.tan_cofunction",
    label: "tangent cofunction",
    left: tan(sub(pi_over_two, m("x"))),
    right: div(i(1), tan(m("x"))),
  },
  {
    rule_id: "trig.sin_pi_sub",
    label: "sine reflection",
    left: sin(sub(pi, m("x"))),
    right: sin(m("x")),
  },
  {
    rule_id: "trig.cos_pi_sub",
    label: "cosine reflection",
    left: cos(sub(pi, m("x"))),
    right: Neg(cos(m("x"))),
  },
  {
    rule_id: "trig.tan_pi_sub",
    label: "tangent reflection",
    left: tan(sub(pi, m("x"))),
    right: Neg(tan(m("x"))),
  },
  {
    rule_id: "trig.sin_neg",
    label: "sine negative-angle",
    left: sin(Neg(m("x"))),
    right: Neg(sin(m("x"))),
  },
  {
    rule_id: "trig.cos_neg",
    label: "cosine negative-angle",
    left: cos(Neg(m("x"))),
    right: cos(m("x")),
  },
  {
    rule_id: "trig.tan_neg",
    label: "tangent negative-angle",
    left: tan(Neg(m("x"))),
    right: Neg(tan(m("x"))),
  },
];

let specs_for_rule = rule_id =>
  specs |> List.filter((spec: spec) => spec.rule_id == rule_id);

let apply_spec_direction = (spec: spec, before_pat, after_pat, exp) =>
  switch (match_pat(before_pat, exp, [])) {
  | Some(env) => [
      {
        rule_id: spec.rule_id,
        label: spec.label,
        before_exp: strip(exp),
        after_exp: instantiate(after_pat, env),
      },
    ]
  | None => []
  };

let apply_spec = (spec: spec, exp) =>
  apply_spec_direction(spec, spec.left, spec.right, exp)
  @ apply_spec_direction(spec, spec.right, spec.left, exp);

let apply_rule_at_root = (rule_id, exp) =>
  specs_for_rule(rule_id) |> List.concat_map(spec => apply_spec(spec, exp));

let applicable_at_root = exp =>
  specs |> List.concat_map(spec => apply_spec(spec, exp));

let is_trig_rule_id = rule_id => {
  let prefix = "trig.";
  let prefix_len = String.length(prefix);
  String.length(rule_id) >= prefix_len
  && String.sub(rule_id, 0, prefix_len) == prefix;
};
