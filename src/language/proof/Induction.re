/* Building the cases of a split (`cases` or `induction`), for the checker
 * and the UI stepper: the goal with the scrutinee replaced by a case's
 * pattern, an induction case's inductive hypotheses, and which facts
 * leave its scope. The statics use it to scope a case body as the checker
 * does. */

/* Returns all sub-pattern of the original pattern that
       1. Have the same type as the original pattern
       2. Are not the original pattern itself.

   `t` has to be normalized, as `statics` types are: for a recursive ADT
   the scrutinee's type is written with the alias (`Term`), while the
   constructor payloads carry the unrolled `Rec` form.
   */

let rec hypotheses = (statics: ProofStatics.t, t, pat) => {
  switch (pat |> Pat.term_of) {
  | _ when Typ.fast_equal(t, Typ.temp(Unknown(Internal))) => []
  | Invalid(_) => []
  | EmptyHole => []
  | MultiHole(_) => []
  | Wild => []
  | Atom(_) => []
  | ListLit(xs) =>
    List.concat(List.map(~f=hypotheses_inner(statics, t, _), xs))
  | Constructor(_) => []
  | Cons(e1, e2) =>
    hypotheses_inner(statics, t, e1) @ hypotheses_inner(statics, t, e2)
  | Var(_) => []
  | Tuple(xs) =>
    List.concat(List.map(~f=hypotheses_inner(statics, t, _), xs))
  /* A wrapper's strict sub-patterns are its contents'. Through
   * `hypotheses_inner` the contents would count themselves, and a case
   * pattern in parens would be its own hypothesis: its goal. */
  | Parens(e)
  | Projector(_, e)
  | Asc(e, _) => hypotheses(statics, t, e)
  | Ap(e1, e2) =>
    hypotheses_inner(statics, t, e1) @ hypotheses_inner(statics, t, e2)
  | Label(_) => []
  | TupLabel(l, e) =>
    hypotheses_inner(statics, t, l) @ hypotheses_inner(statics, t, e)
  | ExplicitNonlabel => []
  };
}
and hypotheses_inner' = (statics: ProofStatics.t, t, pat) => {
  let is_correct_type =
    statics.typ(Pat.rep_id(pat))
    |> Option.map(~f=Typ.fast_equal(t))
    |> Option.value(~default=false);
  (is_correct_type ? [pat] : []) @ hypotheses(statics, t, pat);
}
and hypotheses_inner = (statics: ProofStatics.t, t, pat) =>
  /* Wrappers share their inner pattern's type, so only the innermost one
     counts as a hypothesis. */
  switch (pat |> Pat.term_of) {
  | Parens(x)
  | Projector(_, x)
  | Asc(x, _) => hypotheses_inner(statics, t, x)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | Wild
  | ExplicitNonlabel
  | Atom(_)
  | ListLit(_)
  | Constructor(_)
  | Cons(_)
  | Var(_)
  | Tuple(_)
  | Label(_)
  | TupLabel(_)
  | Ap(_) => hypotheses_inner'(statics, t, pat)
  };

/* The name an inductive hypothesis is cited by: `ih_t` for the
   sub-pattern `t`. A sub-pattern that isn't a variable, like `S(n)` in
   `S(S(n))`, has no variable to name it after, so it is plain `ih`. */
let hypothesis_name = (pat: Pat.t): string =>
  switch (pat |> Pat.term_of) {
  | Var(x) => "ih_" ++ x
  | _ => "ih"
  };

/* Does `fact` mention `e`: free, for a variable, or as a subterm, for
   a computed expression? An induction case is about a fresh value in
   place of its scrutinee, so a fact that mentions the scrutinee is out of
   scope there. Kept beside an inductive hypothesis that doesn't assume
   it, it would prove too much. */
let mentions = (e: Exp.t, fact: Exp.t): bool =>
  switch (ProofSyntax.exp_to_name(e)) {
  | Some(x) => Exp.occurs_free_any([x], fact)
  | None =>
    let found = ref(false);
    let _: Exp.t =
      Exp.map_term(
        ~f_exp=
          (continue, sub) =>
            if (Equality.ignoring_ascriptions.exp(sub, e)) {
              found := true;
              sub;
            } else {
              continue(sub);
            },
        fact,
      );
    found^;
  };

/* Replace all occurrences of `replace` in `in_exp` with `with_exp`.
   The coctx is used to prevent capture inside binders. `None` when a
   binder would capture: the replacement would land under a binder for
   one of `replace`'s or `with_exp`'s variables at a use of one of
   `blacklist_vars`. Occurrences are found up to ascriptions, not up to
   alpha-equivalence. */
let replace =
    (
      statics: ProofStatics.t,
      replace,
      replace_coctx,
      with_exp,
      with_coctx,
      blacklist_vars,
      in_exp,
    )
    : option(Exp.t) => {
  let captured = ref(false);
  let is_bound = (pat: Pat.t): bool => {
    let bvn = pat |> Pat.bindings |> Binding.variable_names;
    CoCtx.has_any(replace_coctx, bvn)
      ? true : CoCtx.has_any(with_coctx, bvn);
  };
  /* At a binder the replacement stops at, `exp` must not use a
     blacklisted variable. */
  let check_capture = (exp: Exp.t, blacklist_vars) => {
    let coctx =
      statics.co_ctx(Exp.rep_id(exp)) |> Option.value(~default=CoCtx.empty);
    if (CoCtx.has_any(coctx, blacklist_vars)) {
      captured := true;
    };
  };
  let restrict_blacklist = (pat: Pat.t, blacklist_vars) => {
    let bvn = pat |> Pat.bindings |> Binding.variable_names;
    List.filter(
      ~f=v => !List.mem(bvn, v, ~equal=String.equal),
      blacklist_vars,
    );
  };
  let rec replace_exp = (in_exp, blacklist_vars): Exp.t =>
    Exp.map_term(
      ~f_exp=
        (continue, exp) => {
          let (term, rewrap) = Exp.unwrap(exp);
          switch (term) {
          | _ when Equality.ignoring_ascriptions.exp(exp, replace) =>
            with_exp |> Exp.replace_all_ids
          /* Forms with binders: check if any bound variables are in the coctx,
             if so, stop. */
          | Fun(p, d1, x, y) =>
            if (is_bound(p)) {
              check_capture(exp, restrict_blacklist(p, blacklist_vars));
              exp;
            } else {
              Fun(
                p,
                replace_exp(d1, restrict_blacklist(p, blacklist_vars)),
                x,
                y,
              )
              |> rewrap;
            }
          | Let(p, e1, e2) =>
            if (is_bound(p)) {
              check_capture(exp, restrict_blacklist(p, blacklist_vars));
              Let(
                p,
                replace_exp(e1, restrict_blacklist(p, blacklist_vars)),
                e2,
              )
              |> rewrap;
            } else {
              continue(exp);
            }
          | FixF(p, e, env) =>
            if (is_bound(p)) {
              check_capture(exp, blacklist_vars);
              exp;
            } else {
              FixF(
                p,
                replace_exp(e, restrict_blacklist(p, blacklist_vars)),
                env,
              )
              |> rewrap;
            }
          | Match(e, cases) =>
            Match(
              replace_exp(e, blacklist_vars),
              List.map(
                ~f=
                  ((p, e)) =>
                    if (is_bound(p)) {
                      check_capture(
                        e,
                        restrict_blacklist(p, blacklist_vars),
                      );
                      (p, e);
                    } else {
                      (
                        p,
                        replace_exp(
                          e,
                          restrict_blacklist(p, blacklist_vars),
                        ),
                      );
                    },
                cases,
              ),
            )
            |> rewrap
          | Theorem(p, e1, pf, e2) =>
            if (is_bound(p)) {
              check_capture(exp, restrict_blacklist(p, blacklist_vars));
              Theorem(p, replace_exp(e1, blacklist_vars), pf, e2) |> rewrap;
            } else {
              continue(exp);
            }
          | Forall(p, e) =>
            if (is_bound(p)) {
              check_capture(exp, restrict_blacklist(p, blacklist_vars));
              exp;
            } else {
              Forall(
                p,
                replace_exp(e, restrict_blacklist(p, blacklist_vars)),
              )
              |> rewrap;
            }
          /* Variables: check if in blacklist */
          | Var(x) when List.mem(blacklist_vars, x, ~equal=String.equal) =>
            captured := true;
            exp;
          | Var(_) => continue(exp)
          /* Forms without binders: continue */
          | EmptyHole
          | Undefined
          | Invalid(_)
          | MultiHole(_)
          | DynamicErrorHole(_, _)
          | Deferral(_)
          | Atom(_)
          | DrvQuote(_, _)
          | ListLit(_)
          | Constructor(_)
          | TypFun(_)
          | Tuple(_)
          | TupleExtension(_)
          | Label(_)
          | TupLabel(_, _)
          | Dot(_, _)
          | LivelitName(_)
          | TyAlias(_)
          | Use(_, _)
          | Ap(_, _, _)
          | TypAp(_, _)
          | DeferredAp(_, _)
          | If(_, _, _)
          | Seq(_, _)
          | Test(_)
          | HintedTest(_, _)
          | Filter(_)
          | Closure(_)
          | Parens(_)
          | Projector(_)
          | Cons(_, _)
          | ListConcat(_, _)
          | UnOp(_, _)
          | Standard(_)
          | BinOp(_, _, _)
          | BuiltinFun(_)
          | Asc(_, _)
          | ExplicitNonlabel
          | Module(_)
          | ModuleExp(_) => continue(exp)
          };
        },
      in_exp,
    );
  let replaced = replace_exp(in_exp, blacklist_vars);
  captured^ ? None : Some(replaced);
};
