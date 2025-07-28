type entry = {rule: ProofRule.t};

type t = list(entry);

let empty = [];

let add_rule = (rule: ProofRule.t, ctx: t): t => {
  [{rule: rule}, ...ctx];
};

let add_entry = (_name: string, exp: Exp.t, ctx: t) => {
  let rule = ProofRule.exp_to_rule(exp);
  [{rule: rule}, ...ctx];
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
    | Some(m) => [b |> MatchExp.substitute_exp(m), ...get_rewrites(rs, exp)]
    | None =>
      switch (MatchExp.match_exp([], bindings, b, exp)) {
      | Some(m) => [
          a |> MatchExp.substitute_exp(m),
          ...get_rewrites(rs, exp),
        ]
      | None => get_rewrites(rs, exp)
      }
    };
  | [_, ...rs] => get_rewrites(rs, exp)
  };
