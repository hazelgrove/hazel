open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type entry = {
  name: string,
  rule: ProofRule.t,
  exp: Exp.t,
  is_captured: bool,
  /* An entry a NEARER entry of the same name hides. Hypothesis names are
     fixed and shadow (docs/prover-obligations.md, "Hypothesis naming"),
     so a scope can hold several `case_eq`s or `ih`s and only the
     innermost is reachable by that name. `lookup_rule` takes the first
     match and therefore never returns one of these; it is carried so the
     UI can show the fact — it IS in scope, and its proposition is still
     true — while saying honestly that the name no longer reaches it. */
  is_shadowed: bool,
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
      is_shadowed: false,
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
      is_shadowed: false,
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
   * set of variables rebound strictly inside it.
   *
   * The result must STAY innermost-first, with the built-in axioms last.
   * `lookup_rule` takes the first match by name, and hypothesis names are
   * now FIXED and SHADOW (docs/prover-obligations.md, "Hypothesis
   * naming"), so the order here decides what a shadowed name means to an
   * `axiom` step. It has to be the same thing it means to `revert`, which
   * resolves through `Ctx.lookup_theorem` — also first-match over these
   * same innermost-first entries. (This fold used to PREPEND onto the
   * accumulator, silently reversing the list to outermost-first. That was
   * invisible while auto-names were freshened to be unique; under
   * shadowing it made `axiom case_eq` and `revert case_eq` disagree.) */
  let (_, rev_rules) =
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
            /* Filled in below, once the whole list is known. */
            is_shadowed: false,
          };
          (seen_vars, [entry, ...rules]);
        | Ctx.TheoremEntry({prop: None, _})
        | Ctx.ConstructorEntry(_)
        | Ctx.TVarEntry(_)
        | Ctx.LivelitEntry(_) => (seen_vars, rules)
        },
      ([], []),
      ctx.entries,
    );
  let rules = List.rev(rev_rules) @ builtins;
  /* Mark every entry a nearer one of the same name hides. The list is
     innermost-first, so "nearer" is "earlier". */
  let (_, marked) =
    List.fold_left(
      ((seen, acc), entry) =>
        (
          [entry.name, ...seen],
          [
            {
              ...entry,
              is_shadowed: List.mem(entry.name, seen),
            },
            ...acc,
          ],
        ),
      ([], []),
      rules,
    );
  List.rev(marked);
};

/* First match over an innermost-first list: a shadowed entry is never
   returned, because the entry that shadows it comes first and has the
   same name. (`is_shadowed` is not tested here for that reason — doing so
   would change nothing, and the invariant is clearer stated once.) */
let lookup_rule = (name: string, ctx: t): option(ProofRule.t) =>
  ctx
  |> List.find_opt(e => e.name == name && e.is_captured == false)
  |> Option.map(e => e.rule);
