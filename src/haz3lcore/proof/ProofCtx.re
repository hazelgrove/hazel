type entry = {
  name: string,
  exp: Exp.t,
  rule: ProofRule.t,
};

type t = list(entry);

let empty = [];

let add_entry = (name: string, exp: Exp.t, ctx: t) => {
  let rule = ProofRule.exp_to_rule(exp);
  [
    {
      name,
      exp,
      rule,
    },
    ...ctx,
  ];
};

let rec get_empty_bindings = (ctx: list(Ctx.entry)) =>
  switch (ctx) {
  | [] => []
  | [VarEntry(var_entry), ...rs] => [
      (var_entry.name, None),
      ...get_empty_bindings(rs),
    ]
  | [_, ...rs] => get_empty_bindings(rs)
  };

let rec get_rewrites = (ctx: t, exp: Exp.t) =>
  switch (ctx) {
  | [] => []
  | [
      {rule: {bindings, assumptions: _, conclusion: Equality(a, b)}, _},
      ...rs,
    ] =>
    let bindings = get_empty_bindings(bindings);
    switch (MatchExp.match_exp([], bindings, a, exp)) {
    | Some(_) => [b, ...get_rewrites(rs, exp)]
    | None =>
      switch (MatchExp.match_exp([], bindings, b, exp)) {
      | Some(_) => [a, ...get_rewrites(rs, exp)]
      | None =>
        print_endline("NOPE");
        get_rewrites(rs, exp);
      }
    };
  | [_, ...rs] => get_rewrites(rs, exp)
  };
