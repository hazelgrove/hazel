open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type entry = {
  name: string,
  rule: ProofRule.t,
  typ: Typ.t,
  exp: Exp.t,
  is_captured: bool,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = list(entry);

let add_exp = (name: string, exp: Exp.t, ctx: t) => {
  let rule = ProofRule.exp_to_rule(exp);
  let typ = ProofRule.rule_to_typ(rule);
  [
    {
      name,
      rule,
      typ,
      exp,
      is_captured: false,
    },
    ...ctx,
  ];
};

let of_env = (~builtins, ~ctx: Ctx.t, env: Environment.t(Exp.t)) => {
  let (_, rules) =
    Environment.to_list(env)
    |> List.rev
    |> List.fold_left(
         ((seen_vars, rules), (name, exp)) =>
           switch (Exp.term_of(exp)) {
           | Grammar.ProofObject(e) =>
             let rule = ProofRule.exp_to_rule(e);
             let typ = ProofRule.rule_to_typ(rule);
             let coctx =
               ProofRule.get_coctx(ctx, Typ.temp(Atom(Bool)), rule);
             let is_captured = CoCtx.has_any(coctx, seen_vars);
             print_endline("is captured: " ++ string_of_bool(is_captured));
             let entry = {
               name,
               rule,
               typ,
               exp: e,
               is_captured,
             };
             ([name, ...seen_vars], [entry, ...rules]);
           | _ => ([name, ...seen_vars], rules)
           },
         ([], builtins),
       );
  rules;
};

let lookup_rule = (name: string, ctx: t): option(ProofRule.t) =>
  ctx
  |> List.find_opt(e => e.name == name && e.is_captured == false)
  |> Option.map(e => e.rule);
