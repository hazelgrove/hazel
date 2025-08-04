open Util;
open OptUtil.Syntax;

[@deriving (show({with_path: false}), sexp, yojson)]
type conclusion =
  | Equality(Exp.t, Exp.t)
  | Other(Exp.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  bindings: list(Ctx.entry),
  assumptions: list(Exp.t),
  conclusion,
};

let rec exp_to_rule = (exp: Exp.t): t =>
  switch (exp |> Exp.term_of) {
  | Fun(p, e, Some(t), _) =>
    let bindings' =
      ProofHacks.dhpat_extend_ctx(p, t, Ctx.empty)
      |> Option.map((x: Ctx.t) => x.entries)
      |> OptUtil.get(() => []);
    let {bindings, assumptions, conclusion} = exp_to_rule(e);
    {
      bindings: bindings' @ bindings,
      assumptions,
      conclusion,
    };
  | BinOp(Bool(Or), {term: UnOp(Bool(Not), e1), _}, e2) =>
    // TODO: Negate more generally and implication
    let {bindings, assumptions, conclusion} = exp_to_rule(e2);
    {
      bindings,
      assumptions: [e1] @ assumptions,
      conclusion,
    };
  | BinOp(Poly(Equals), e1, e2) => {
      bindings: [],
      assumptions: [],
      conclusion: Equality(e1, e2),
    }
  | _ => {
      bindings: [],
      assumptions: [],
      conclusion: Other(exp),
    }
  };

let rec typ_to_rule = (typ: Typ.t): option(t) =>
  switch (typ |> Typ.term_of) {
  | Forall(p, t) =>
    let bindings' =
      ProofHacks.dhpat_extend_ctx(p, t, Ctx.empty)
      |> Option.map((x: Ctx.t) => x.entries)
      |> OptUtil.get(() => []);
    let* {bindings, assumptions, conclusion} = typ_to_rule(t);
    Some({
      bindings: bindings' @ bindings,
      assumptions,
      conclusion,
    });
  | Yes(e) => Some(exp_to_rule(e))
  | _ => None
  };

let conclusion_exp = (rule: t): Exp.t =>
  switch (rule.conclusion) {
  | Equality(e1, e2) => Exp.fresh(BinOp(Poly(Equals), e1, e2))
  | Other(e) => e
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

let can_eq =
    (~interfering_bindings as _: list(Var.t)=[], rule: t, exp: Exp.t)
    : (option(Exp.t), option(Exp.t)) => {
  switch (rule.conclusion) {
  | Equality(a, b) =>
    let bindings = get_empty_bindings(rule.bindings);
    (
      MatchExp.match_exp([], bindings, a, exp)
      |> Option.map(MatchExp.substitute_exp(_, b)),
      MatchExp.match_exp([], bindings, b, exp)
      |> Option.map(MatchExp.substitute_exp(_, a)),
    );
  | Other(_) => (None, None)
  };
};
