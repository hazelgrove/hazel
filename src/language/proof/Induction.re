/* Building an induction's cases, for the checker and the UI stepper:
 * the goal with the scrutinee replaced by a case's pattern, and the
 * case's inductive hypotheses and `where` facts. The statics use it to
 * scope a case body as the checker does, so it reads maps through
 * `StaticsBase.Map`, the same module as `Statics.Map`: naming `Statics`
 * would make the two modules depend on each other. */

/* Returns all sub-pattern of the original pattern that
       1. Have the same type as the original pattern
       2. Are not the original pattern itself.

   `tyctx` is the type context the induction is checked in. For a
   recursive ADT the two types compared are written in different forms:
   the scrutinee carries the alias (`Term`), while the constructor
   payloads the statics assign to sub-patterns carry the unrolled `Rec`
   form, so `Typ.fast_equal` alone would never find an inductive
   hypothesis. Both sides are `Typ.normalize`d first, as `Coverage.check`
   is in `ProofCheck.check_induction`.
   */

let rec hypotheses = (~tyctx, m, t, pat) => {
  switch (pat |> Pat.term_of) {
  | _ when Typ.fast_equal(t, Typ.temp(Unknown(Internal))) => []
  | Invalid(_) => []
  | EmptyHole => []
  | MultiHole(_) => []
  | Wild => []
  | Atom(_) => []
  | ListLit(xs) =>
    List.concat(List.map(hypotheses_inner(~tyctx, m, t, _), xs))
  | Constructor(_) => []
  | Cons(e1, e2) =>
    hypotheses_inner(~tyctx, m, t, e1) @ hypotheses_inner(~tyctx, m, t, e2)
  | Var(_) => []
  | Tuple(xs) =>
    List.concat(List.map(hypotheses_inner(~tyctx, m, t, _), xs))
  | Parens(e)
  | Projector(_, e) => hypotheses_inner(~tyctx, m, t, e)
  | Ap(e1, e2) =>
    hypotheses_inner(~tyctx, m, t, e1) @ hypotheses_inner(~tyctx, m, t, e2)
  | Asc(e, _)
  | Where(e, _) => hypotheses_inner(~tyctx, m, t, e)
  | Label(_) => []
  | TupLabel(l, e) =>
    hypotheses_inner(~tyctx, m, t, l) @ hypotheses_inner(~tyctx, m, t, e)
  | ExplicitNonlabel => []
  };
}
and hypotheses_inner' = (~tyctx, m, t, pat) => {
  /* Compare modulo type-alias unrolling: see the note above. */
  let norm = ty => Typ.normalize(tyctx, ty);
  let is_correct_type =
    StaticsBase.Map.ty_of(Pat.rep_id(pat), m)
    |> Option.map(ty => Typ.fast_equal(norm(ty), norm(t)))
    |> Option.value(~default=false);
  (is_correct_type ? [pat] : []) @ hypotheses(~tyctx, m, t, pat);
}
and hypotheses_inner = (~tyctx, m, t, pat) =>
  switch (pat |> Pat.term_of) {
  | Parens(x) => hypotheses_inner(~tyctx, m, t, x)
  | Asc(e, _) => hypotheses_inner(~tyctx, m, t, e)
  | _ => hypotheses_inner'(~tyctx, m, t, pat)
  };

/* Could `p1` and `p2` match the same value? Conservatively yes unless
   their head constructors are both known and different. */
let pats_overlap = (p1: Pat.t, p2: Pat.t): bool => {
  /* A coarse head-constructor key. `None` means "matches anything"
     (variable / wildcard / hole), which overlaps everything. */
  let rec head_key = (p: Pat.t): option(string) =>
    switch (p |> Pat.term_of) {
    | Where(p, _)
    | Parens(p)
    | Projector(_, p)
    | Asc(p, _)
    | TupLabel(_, p) => head_key(p)
    | Constructor(c, _) => Some("ctr:" ++ c)
    | Ap(c, _) => Pat.ctr_name(c) |> Option.map(c => "ctr:" ++ c)
    | Cons(_, _) => Some("cons")
    | ListLit(ps) => Some("list:" ++ string_of_int(List.length(ps)))
    | Tuple(ps) => Some("tuple:" ++ string_of_int(List.length(ps)))
    | Atom(a) => Some("atom:" ++ Atom.show(a))
    | Label(l) => Some("label:" ++ l)
    | Var(_)
    | Wild
    | EmptyHole
    | MultiHole(_)
    | Invalid(_)
    | ExplicitNonlabel => None
    };
  switch (head_key(p1), head_key(p2)) {
  | (Some(k1), Some(k2)) => k1 == k2
  | _ => true
  };
};

/* The facts the case arm `pat`, below the `earlier` arms, can cite as
   `where`, in the order they are installed, so the last one is what the
   name resolves to. An arm is only reached when every earlier arm
   failed, so an earlier arm guarded by `g` that could have matched this
   value contributes `!g`; a constructor-disjoint one contributes
   nothing, so `| Z =>` doesn't import the `S` arm's negated guard. The
   arm's own guard comes last. A negation is one fact, never split into
   conjuncts: `!(a && b)` doesn't entail `!a`. */
let where_facts = (~earlier: list(Pat.t), pat: Pat.t): list(Exp.t) => {
  let negations =
    List.filter_map(
      prev =>
        switch (Exp.split_pat_guard(prev)) {
        | (prev_base, Some(g)) when pats_overlap(prev_base, pat) =>
          Some(Exp.fresh(UnOp(Bool(Not), g)))
        | _ => None
        },
      earlier,
    );
  negations @ Option.to_list(Exp.pat_guard(pat));
};

/* Replace all occurrences of `replace` in `in_exp` with `with_exp`.
   The coctx is used to prevent capture inside binders. `None` when a
   binder would capture: the replacement would land under a binder for
   one of `replace`'s or `with_exp`'s variables at a use of one of
   `blacklist_vars`. Occurrences are found up to ascriptions, not up to
   alpha-equivalence. */
let replace =
    (
      info_map: StaticsBase.Map.t,
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
      switch (StaticsBase.Map.lookup_exp(Exp.rep_id(exp), info_map)) {
      | Some({co_ctx, _}) => co_ctx
      | None => CoCtx.empty
      };
    if (CoCtx.has_any(coctx, blacklist_vars)) {
      captured := true;
    };
  };
  let restrict_blacklist = (pat: Pat.t, blacklist_vars) => {
    let bvn = pat |> Pat.bindings |> Binding.variable_names;
    List.filter(v => !List.mem(v, bvn), blacklist_vars);
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
                replace_pat(p, restrict_blacklist(p, blacklist_vars)),
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
                ((p, e)) =>
                  if (is_bound(p)) {
                    check_capture(e, restrict_blacklist(p, blacklist_vars));
                    (p, e);
                  } else {
                    (
                      p,
                      replace_exp(e, restrict_blacklist(p, blacklist_vars)),
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
                replace_pat(p, restrict_blacklist(p, blacklist_vars)),
                replace_exp(e, restrict_blacklist(p, blacklist_vars)),
              )
              |> rewrap;
            }
          /* Variables: check if in blacklist */
          | Var(x) when List.mem(x, blacklist_vars) =>
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
          | BinOp(_, _, _)
          | BuiltinFun(_)
          | Asc(_, _)
          | ExplicitNonlabel
          | Module(_)
          | ModuleExp(_) => continue(exp)
          };
        },
      in_exp,
    )
  /* Rewrite inside a binder pattern's `where` guard, which lives under
     the binder and so needs the same rewrite the body gets. */
  and replace_pat = (pat: Pat.t, blacklist_vars): Pat.t => {
    let (pterm, prewrap) = Pat.unwrap(pat);
    switch (pterm) {
    | Where(p1, g) =>
      prewrap(
        Where(
          replace_pat(p1, blacklist_vars),
          replace_exp(g, blacklist_vars),
        ),
      )
    | Parens(p1) => prewrap(Parens(replace_pat(p1, blacklist_vars)))
    | _ => pat
    };
  };
  let replaced = replace_exp(in_exp, blacklist_vars);
  captured^ ? None : Some(replaced);
};
