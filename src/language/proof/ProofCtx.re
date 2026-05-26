open Util;
open OptUtil.Syntax;

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

let empty = [];

let add_rule = (name: string, rule: ProofRule.t, ctx: t): t => {
  let typ = ProofRule.rule_to_typ(rule);
  let exp = ProofRule.rule_to_exp(rule);
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

let add_typ = (name: string, typ: Typ.t, ctx: t): option(t) => {
  let* rule = ProofRule.typ_to_rule(typ);
  let exp = ProofRule.rule_to_exp(rule);
  Some([
    {
      name,
      typ,
      rule,
      exp,
      is_captured: false,
    },
    ...ctx,
  ]);
};

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

let of_ctx = (~builtins, ctx: Ctx.t): t => {
  let (_, rules) =
    List.fold_left(
      ((seen_vars, rules), entry) =>
        switch (entry) {
        | Ctx.VarEntry({name, typ, _}) =>
          switch (ProofRule.typ_to_rule(typ)) {
          | Some(rule) =>
            let coctx =
              ProofRule.get_coctx(ctx, Typ.temp(Atom(Bool)), rule);
            let is_captured = CoCtx.has_any(coctx, seen_vars);
            let entry = {
              name,
              rule,
              typ,
              exp: ProofRule.rule_to_exp(rule),
              is_captured,
            };
            ([name, ...seen_vars], [entry, ...rules]);
          | None => ([name, ...seen_vars], rules)
          }
        | Ctx.ConstructorEntry(_)
        | Ctx.TVarEntry(_)
        | Ctx.LivelitEntry(_) => (seen_vars, rules)
        },
      ([], builtins),
      ctx.entries,
    );
  rules;
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

let rec get_empty_bindings = (ctx: list(Ctx.entry)) =>
  switch (ctx) {
  | [] => []
  | [Ctx.VarEntry(var_entry), ...rs] => [
      (var_entry.name, (var_entry.typ, None)),
      ...get_empty_bindings(rs),
    ]
  | [_, ...rs] => get_empty_bindings(rs)
  };

let rec get_rewrites = (ctx: t, exp: Exp.t) =>
  switch (ctx) {
  | [] => []
  | [{rule, _}, ...rs] =>
    let rewrites =
      switch (
        ProofRule.can_eq(
          ~info_map=Statics.Map.empty,
          ~env=Environment.empty,
          rule,
          exp,
        )
      ) {
      | (Some(l), Some(r)) => [l, r]
      | (Some(l), None) => [l]
      | (None, Some(r)) => [r]
      | (None, None) => []
      };
    rewrites @ get_rewrites(rs, exp);
  };

let rec get_rewrites_with_names = (ctx: t, exp: Exp.t) =>
  switch (ctx) {
  | [] => []
  | [{name, rule, _}, ...rs] =>
    let rewrites =
      switch (
        ProofRule.can_eq(
          ~info_map=Statics.Map.empty,
          ~env=Environment.empty,
          rule,
          exp,
        )
      ) {
      | (Some(l), Some(r)) => [(name, l), (name, r)]
      | (Some(l), None) => [(name, l)]
      | (None, Some(r)) => [(name, r)]
      | (None, None) => []
      };
    rewrites @ get_rewrites_with_names(rs, exp);
  };
