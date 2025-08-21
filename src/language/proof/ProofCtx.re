open Util;
open OptUtil.Syntax;

[@deriving (show({with_path: false}), sexp, yojson)]
type entry = {
  name: string,
  rule: ProofRule.t,
  typ: Typ.t,
  is_captured: bool,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = list(entry);

let empty = [];

let add_rule = (name: string, rule: ProofRule.t, ctx: t): t => {
  let typ = ProofRule.rule_to_typ(rule);
  [
    {
      name,
      rule,
      typ,
      is_captured: false,
    },
    ...ctx,
  ];
};

let add_typ = (name: string, typ: Typ.t, ctx: t): option(t) => {
  let* rule = ProofRule.typ_to_rule(typ);
  Some([
    {
      name,
      typ,
      rule,
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
