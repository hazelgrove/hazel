open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type conclusion =
  | Equality(Exp.t, Exp.t)
  | Other(Exp.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  bindings: list(Ctx.entry),
  // assumptions: list(Exp.t),
  conclusion,
};

let rec exp_to_rule = (exp: Exp.t): t =>
  switch (exp |> Exp.term_of) {
  | Forall(p, e) =>
    let bindings' =
      ProofHacks.dhpat_extend_ctx(p, Typ.temp(Unknown(Internal)), Ctx.empty)
      |> Option.map((x: Ctx.t) => x.entries)
      |> OptUtil.get(() => []);
    let {bindings, /* assumptions, */ conclusion} = exp_to_rule(e);
    {
      bindings: bindings' @ bindings,
      // assumptions,
      conclusion,
    };
  // | BinOp(Bool(Or), {term: UnOp(Bool(Not), e1), _}, e2) =>
  //   // TODO: Negate more generally and implication
  //   let {bindings, assumptions, conclusion} = exp_to_rule(e2);
  //   {
  //     bindings,
  //     assumptions: [e1] @ assumptions,
  //     conclusion,
  //   };
  | BinOp(Poly(Equals), e1, e2) => {
      bindings: [],
      // assumptions: [],
      conclusion: Equality(e1, e2),
    }
  | _ => {
      bindings: [],
      // assumptions: [],
      conclusion: Other(exp),
    }
  };

let typ_to_rule = (typ: Typ.t): option(t) =>
  switch (typ |> Typ.term_of) {
  | ProofOf(e) => Some(exp_to_rule(e))
  | _ => None
  };

let rule_to_exp = (rule: t): Exp.t => {
  let rec _wrap_assumptions = (assumptions: list(Exp.t), body: Exp.t): Exp.t =>
    switch (assumptions) {
    | [] => body
    | [a, ...rs] =>
      _wrap_assumptions(
        rs,
        Exp.fresh(BinOp(Bool(Or), Exp.fresh(UnOp(Bool(Not), a)), body)),
      )
    };
  let rec wrap_foralls = (bindings: list(Ctx.entry), body: Exp.t): Exp.t =>
    switch (bindings) {
    | [] => body
    | [VarEntry({name, typ, _}), ...rs] =>
      wrap_foralls(
        rs,
        Exp.fresh(
          Forall(Pat.fresh(Asc(Pat.fresh(Var(name)), typ)), body),
        ),
      )
    | [ConstructorEntry(_) | TVarEntry(_) | LivelitEntry(_), ...rs] =>
      wrap_foralls(rs, body)
    };
  let body =
    switch (rule.conclusion) {
    | Equality(e1, e2) => Exp.fresh(BinOp(Poly(Equals), e1, e2))
    | Other(e) => e
    };
  wrap_foralls(
    rule.bindings,
    /*wrap_assumptions(rule.assumptions,*/ body /*)*/,
  );
};

let rule_to_typ = (rule: t): Typ.t => {
  rule |> rule_to_exp |> (x => Typ.fresh(ProofOf(x)));
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
      (var_entry.name, (var_entry.typ, None)),
      ...get_empty_bindings(rs),
    ]
  | [_, ...rs] => get_empty_bindings(rs)
  };

let can_eq =
    (~info_map, ~env, rule: t, exp: Exp.t): (option(Exp.t), option(Exp.t)) => {
  switch (rule.conclusion) {
  | Equality(a, b) =>
    let bindings = get_empty_bindings(rule.bindings);
    (
      MatchExp.match_exp(~info_map, ~exp_env=env, ~exp_r_ctx=bindings, a, exp)
      |> Option.map(MatchExp.substitute_exp(_, b)),
      MatchExp.match_exp(~info_map, ~exp_env=env, ~exp_r_ctx=bindings, b, exp)
      |> Option.map(MatchExp.substitute_exp(_, a)),
    );
  | Other(_) => (None, None)
  };
};

let is_active = (~info_map, ~env, rule: t, exp: Exp.t): bool =>
  switch (can_eq(~info_map, ~env, rule, exp)) {
  | (Some(_), _)
  | (_, Some(_)) => true
  | _ => false
  };

let get_coctx = (ctx: Ctx.t, ana: Typ.t, rule: t): CoCtx.t => {
  let full_ctx = List.fold_left(Ctx.extend, ctx, rule.bindings);
  let c_exp = conclusion_exp(rule);
  /* TODO[Matt]: using full statics here feels a little overblown
     especially given we need to fake some settings to it, perhaps
     discuss with Andrew */
  let statics = Statics.mk(~ana, CoreSettings.on, full_ctx, c_exp);
  let root_id = Exp.rep_id(c_exp);
  let info = Statics.Map.lookup(root_id, statics);
  let inner_coctx =
    switch (info) {
    | Some(Info.InfoExp(exp)) => Info.exp_co_ctx(exp)
    | _ => []
    };
  CoCtx.mk(ctx, full_ctx, inner_coctx);
};
