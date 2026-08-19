open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type conclusion =
  | Equality(Exp.t, Exp.t)
  | Other(Exp.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  bindings: list(Ctx.entry),
  /* Conditions on the rule, in source order: the `where` guards of
   * restricted binders followed by the peeled top-level `==>` antecedents
   * (`A ==> B ==> concl` contributes [A, B]). A rule with assumptions is
   * a conditional rewrite rule; applying it emits each assumption,
   * instantiated by the match, as an obligation
   * (docs/prover-obligations.md §2.1, §4.1). */
  assumptions: list(Exp.t),
  conclusion,
};

/* Peel only the outer binders (forall / forall-where) off a theorem
 * statement, collecting the binders' ctx entries and any `where` guards.
 * Returns the remaining core, which may retain `==>` antecedents. */
let rec peel_binders = (exp: Exp.t): (list(Ctx.entry), list(Exp.t), Exp.t) =>
  switch (exp |> Exp.term_of) {
  | Forall(p, e) =>
    let bindings' =
      ProofHacks.dhpat_extend_ctx(p, Typ.temp(Unknown(Internal)), Ctx.empty)
      |> Option.map((x: Ctx.t) => x.entries)
      |> OptUtil.get(() => []);
    let (bindings, guards, core) = peel_binders(e);
    (bindings' @ bindings, guards, core);
  | ForallWhere(p, g, e) =>
    let bindings' =
      ProofHacks.dhpat_extend_ctx(p, Typ.temp(Unknown(Internal)), Ctx.empty)
      |> Option.map((x: Ctx.t) => x.entries)
      |> OptUtil.get(() => []);
    let (bindings, guards, core) = peel_binders(e);
    (bindings' @ bindings, [g, ...guards], core);
  | _ => ([], [], exp)
  };

/* Peel the top-level `==>` chain off a core proposition:
 * `A ==> B ==> concl` yields ([A, B], concl). */
let rec peel_implications = (exp: Exp.t): (list(Exp.t), Exp.t) =>
  switch (exp |> Exp.term_of) {
  | BinOp(Bool(Implies), e1, e2) =>
    let (assumptions, core) = peel_implications(e2);
    ([e1, ...assumptions], core);
  | _ => ([], exp)
  };

let classify = (exp: Exp.t): conclusion =>
  switch (exp |> Exp.term_of) {
  | BinOp(Poly(Equals), e1, e2) => Equality(e1, e2)
  | _ => Other(exp)
  };

let exp_to_rule = (exp: Exp.t): t => {
  let (bindings, guards, core) = peel_binders(exp);
  let (antecedents, conclusion_core) = peel_implications(core);
  {
    bindings,
    assumptions: guards @ antecedents,
    conclusion: classify(conclusion_core),
  };
};

let typ_to_rule = (typ: Typ.t): option(t) =>
  switch (typ |> Typ.term_of) {
  | ProofOf(e) => Some(exp_to_rule(e))
  | _ => None
  };

let wrap_assumptions = (assumptions: list(Exp.t), body: Exp.t): Exp.t =>
  List.fold_right(
    (a, acc) => Exp.fresh(BinOp(Bool(Implies), a, acc)),
    assumptions,
    body,
  );

let conclusion_exp = (rule: t): Exp.t =>
  switch (rule.conclusion) {
  | Equality(e1, e2) => Exp.fresh(BinOp(Poly(Equals), e1, e2))
  | Other(e) => e
  };

/* The rule's core proposition: the assumptions re-wrapped as a `==>`
 * chain onto the conclusion. This is the goal a proof of the rule works
 * on after its binders are introduced. */
let core_exp = (rule: t): Exp.t =>
  wrap_assumptions(rule.assumptions, conclusion_exp(rule));

let rule_to_exp = (rule: t): Exp.t => {
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
    | [
        ConstructorEntry(_) | TVarEntry(_) | LivelitEntry(_) |
        HypothesisEntry(_),
        ...rs,
      ] =>
      wrap_foralls(rs, body)
    };
  /* Note: `where` guards and `==>` antecedents are conflated in
   * `assumptions`, so this rebuilds all of them as a `==>` chain —
   * logically equivalent to the source, though not lexically identical
   * for restricted binders. */
  wrap_foralls(rule.bindings, core_exp(rule));
};

let rule_to_typ = (rule: t): Typ.t => {
  rule |> rule_to_exp |> (x => Typ.fresh(ProofOf(x)));
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

/* Like `can_eq` below, but also returns the match context that produced
 * each rewrite, so callers can instantiate the rule's assumptions and
 * detect unresolved metavariables (underdetermined instantiation,
 * docs/prover-obligations.md §4.1). */
let can_eq_inst =
    (~info_map, ~env, rule: t, exp: Exp.t)
    : (
        option((Exp.t, MatchExp.match_ctx)),
        option((Exp.t, MatchExp.match_ctx)),
      ) => {
  switch (rule.conclusion) {
  | Equality(a, b) =>
    let bindings = get_empty_bindings(rule.bindings);
    let via = (from, to_) =>
      MatchExp.match_exp(
        ~info_map,
        ~exp_env=env,
        ~exp_r_ctx=bindings,
        from,
        exp,
      )
      |> Option.map(mctx => (MatchExp.substitute_exp(mctx, to_), mctx));
    (via(b, a), via(a, b));
  | Other(_) => (None, None)
  };
};

let can_eq =
    (~info_map, ~env, rule: t, exp: Exp.t): (option(Exp.t), option(Exp.t)) => {
  let (l, r) = can_eq_inst(~info_map, ~env, rule, exp);
  (Option.map(fst, l), Option.map(fst, r));
};

let is_active = (~info_map, ~env, rule: t, exp: Exp.t): bool =>
  switch (can_eq(~info_map, ~env, rule, exp)) {
  | (Some(_), _)
  | (_, Some(_)) => true
  | _ => false
  };

let get_coctx = (ctx: Ctx.t, ana: Typ.t, rule: t): CoCtx.t => {
  let full_ctx = List.fold_left(Ctx.extend, ctx, rule.bindings);
  /* Use the full core (assumptions ==> conclusion) so free variables of
   * the assumptions are seen too. */
  let c_exp = core_exp(rule);
  /* TODO[Matt]: using full statics here feels a little overblown
     especially given we need to fake some settings to it, perhaps
     discuss with Andrew */
  let (statics, _) = Statics.mk(~ana, CoreSettings.on, full_ctx, c_exp);
  let root_id = Exp.rep_id(c_exp);
  let inner_coctx =
    switch (Statics.Map.lookup_exp(root_id, statics)) {
    | Some(exp) => Info.exp_co_ctx(exp)
    | None => []
    };
  CoCtx.mk(ctx, full_ctx, inner_coctx);
};
