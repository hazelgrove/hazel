open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type entry = {
  name: string,
  rule: ProofRule.t,
  exp: Exp.t,
  is_captured: bool,
  /* Hidden by a nearer entry of the same name. Hypothesis names are fixed
     and shadow, so a scope can hold several `case_eq`s or `ih_t`s and only
     the innermost is reachable by that name. A shadowed entry is kept so
     the UI can show the fact, which is still in scope and still true,
     while saying that its name does not reach it. */
  is_shadowed: bool,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = list(entry);

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

/* The rules citable in a scope: every fact in the scope's theorem
 * namespace (`Ctx.TheoremEntry`), innermost first, then the built-in
 * axioms.
 *
 * A fact stated in terms of a name that an inner binder has since rebound
 * is about the old binding, so it is marked `is_captured` and is not
 * citable. The test is a free-occurrence check (`ProofRule.mentions_any`)
 * against the variable names bound strictly inside the fact's own entry. */
let of_theorem_ctx = (~builtins, ctx: Ctx.t): t => {
  /* Entries are innermost-first, so walking them in that order and
   * accumulating the variable names seen so far gives, at each fact, the
   * set of variables rebound strictly inside it.
   *
   * The result has to stay innermost-first, with the built-in axioms last.
   * Hypothesis names are fixed and shadow, and `lookup_rule` takes the
   * first match by name, so this order decides what a shadowed name means
   * to a `rewrite` step.
   * It has to agree with `revert`, which resolves through
   * `Ctx.lookup_theorem`, also first-match over these entries. */
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

/* The nearest entry with this name, unless it is captured. A captured
   entry still shadows the outer entries with its name, as it does for
   `revert`. */
let lookup_rule = (name: string, ctx: t): option(ProofRule.t) =>
  switch (List.find_opt(e => e.name == name, ctx)) {
  | Some({is_captured: false, rule, _}) => Some(rule)
  | Some({is_captured: true, _})
  | None => None
  };
