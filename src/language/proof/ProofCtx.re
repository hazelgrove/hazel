open Util;

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

let rec get_rewrites = (ctx: t, exp: Exp.t) =>
  ListUtil.flat_map(
    (entry: entry) => {
      let (l, r) = ProofRule.can_eq(entry.rule, exp);
      Option.to_list(l) @ Option.to_list(r);
    },
    ctx,
  );
