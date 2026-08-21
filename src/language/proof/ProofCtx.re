open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type entry = {
  name: string,
  rule: ProofRule.t,
  exp: Exp.t,
  is_captured: bool,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = list(entry);

let empty = [];

let add_rule = (name: string, rule: ProofRule.t, ctx: t): t => {
  let exp = ProofRule.rule_to_exp(rule);
  [
    {
      name,
      rule,
      exp,
      is_captured: false,
    },
    ...ctx,
  ];
};

let add_exp = (name: string, exp: Exp.t, ctx: t) => {
  let rule = ProofRule.exp_to_rule(exp);
  [
    {
      name,
      rule,
      exp,
      is_captured: false,
    },
    ...ctx,
  ];
};

/* The rules citable in a scope: the built-in axioms plus every fact in
 * the scope's THEOREM NAMESPACE (`Ctx.TheoremEntry`), innermost first.
 *
 * This replaces the old `of_env` / `of_ctx` pair, which scooped
 * `ProofObject(_)` environment values and `ProofOf(_)`-typed var entries
 * respectively (both forms are now deleted) — the Curry-Howard reading the design has since decided
 * against (docs/prover-obligations.md §0.1). Citation is now lookup in
 * the theorem context, full stop.
 *
 * `is_captured` is carried over unchanged: a fact stated in terms of a
 * name that an INNER binder has since rebound is about the old binding
 * and must not be citable. The test is the same free-occurrence check as
 * before (`ProofRule.mentions_any` against the variable names bound
 * strictly inside the fact's own entry), so the capture semantics of
 * generalize/citation are preserved. */
let of_theorem_ctx = (~builtins, ctx: Ctx.t): t => {
  /* Entries are innermost-first, so walking them in that order and
   * accumulating the VARIABLE names seen so far gives, at each fact, the
   * set of variables rebound strictly inside it. */
  let (_, rules) =
    List.fold_left(
      ((seen_vars, rules), entry) =>
        switch (entry) {
        | Ctx.VarEntry({name, _}) => ([name, ...seen_vars], rules)
        | Ctx.TheoremEntry({name, prop: Some(prop), _}) =>
          let rule = ProofRule.exp_to_rule(prop);
          let is_captured = ProofRule.mentions_any(rule, seen_vars);
          let entry = {
            name,
            rule,
            exp: prop,
            is_captured,
          };
          (seen_vars, [entry, ...rules]);
        | Ctx.TheoremEntry({prop: None, _})
        | Ctx.ConstructorEntry(_)
        | Ctx.TVarEntry(_)
        | Ctx.LivelitEntry(_) => (seen_vars, rules)
        },
      ([], builtins),
      ctx.entries,
    );
  rules;
};

let lookup_rule = (name: string, ctx: t): option(ProofRule.t) =>
  ctx
  |> List.find_opt(e => e.name == name && e.is_captured == false)
  |> Option.map(e => e.rule);
