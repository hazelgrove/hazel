/* Inductive-hypothesis candidates for one case of an induction: the
   sub-patterns of `pat` whose type is the scrutinee's type `t`. Moved here
   UNCHANGED from ProofHacks (where an alias remains for the checker) so
   that Statics can use the same function to make the checker's `ih` name
   visible; the only change is spelling `Statics.Map.ty_of` as
   `StaticsBase.Map.ty_of`, which is the same function. */
/* Returns all sub-pattern of the original pattern that
       1. Have the same type as the original pattern
       2. Are not the original pattern itself.

   `tyctx` is the type context the induction is checked in; it is needed
   because the two types being compared are written in DIFFERENT forms for
   a recursive ADT: the scrutinee carries the alias (`Term`), while the
   constructor payloads statics assigns to sub-patterns carry the unrolled
   `Rec` form. `Typ.fast_equal` on those always fails, so before Phase 4c
   every ADT induction generated ZERO inductive hypotheses (list
   inductions, whose element types compare nominally, were unaffected).
   Both sides are `Typ.normalize`d before comparison — the same
   normalization `Coverage.check` is already given in `check_induction`.
   */

let rec get_inductive_hypotheses = (~tyctx, m, t, pat) => {
  switch (pat |> Pat.term_of) {
  | _ when Typ.fast_equal(t, Typ.temp(Unknown(Internal))) => []
  | Invalid(_) => []
  | EmptyHole => []
  | MultiHole(_) => []
  | Wild => []
  | Atom(_) => []
  | ListLit(xs) =>
    List.concat(
      List.map(get_inductive_hypotheses_inner(~tyctx, m, t, _), xs),
    )
  | Constructor(_) => []
  | Cons(e1, e2) =>
    get_inductive_hypotheses_inner(~tyctx, m, t, e1)
    @ get_inductive_hypotheses_inner(~tyctx, m, t, e2)
  | Var(_) => []
  | Tuple(xs) =>
    List.concat(
      List.map(get_inductive_hypotheses_inner(~tyctx, m, t, _), xs),
    )
  | Parens(e)
  | Projector(_, e) => get_inductive_hypotheses_inner(~tyctx, m, t, e)
  | Ap(e1, e2) =>
    get_inductive_hypotheses_inner(~tyctx, m, t, e1)
    @ get_inductive_hypotheses_inner(~tyctx, m, t, e2)
  | Asc(e, _)
  | Where(e, _) => get_inductive_hypotheses_inner(~tyctx, m, t, e)
  | Label(_) => []
  | TupLabel(l, e) =>
    get_inductive_hypotheses_inner(~tyctx, m, t, l)
    @ get_inductive_hypotheses_inner(~tyctx, m, t, e)
  | ExplicitNonlabel => []
  };
}
and get_inductive_hypotheses_inner' = (~tyctx, m, t, pat) => {
  /* Compare modulo type-alias unrolling: see the note above. */
  let norm = ty => Typ.normalize(tyctx, ty);
  let is_correct_type =
    StaticsBase.Map.ty_of(Pat.rep_id(pat), m)
    |> Option.map(ty => Typ.fast_equal(norm(ty), norm(t)))
    |> Option.value(~default=false);
  (is_correct_type ? [pat] : [])
  @ get_inductive_hypotheses(~tyctx, m, t, pat);
}
and get_inductive_hypotheses_inner = (~tyctx, m, t, pat) =>
  switch (pat |> Pat.term_of) {
  | Parens(x) => get_inductive_hypotheses_inner(~tyctx, m, t, x)
  | Asc(e, _) => get_inductive_hypotheses_inner(~tyctx, m, t, e)
  | _ => get_inductive_hypotheses_inner'(~tyctx, m, t, pat)
  };
