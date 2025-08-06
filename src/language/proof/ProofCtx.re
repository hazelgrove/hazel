open Util;
open OptUtil.Syntax;

[@deriving (show({with_path: false}), sexp, yojson)]
type entry = {
  name: string,
  rule: ProofRule.t,
  typ: Typ.t,
};

type t = list(entry);

let empty = [];

let add_rule = (name: string, rule: ProofRule.t, ctx: t): t => {
  let typ = ProofRule.rule_to_typ(rule);
  [
    {
      name,
      rule,
      typ,
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
    },
    ...ctx,
  ];
};
