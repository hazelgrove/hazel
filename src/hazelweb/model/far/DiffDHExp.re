/* Temporary; remove when complete */
type error =
  | ClosureInUnevaluatedExp
  | DiffNotImplemented;

exception Exception(error);

[@deriving sexp]
type t =
  | NoDiff
  | NonFillDiff
  | FillDiff(DHExp.t, MetaVar.t);

let get_hole_number_of_dhexp = (d: DHExp.t): option(MetaVar.t) => {
  switch (d) {
  | EmptyHole(u, _)
  | NonEmptyHole(_, u, _, _)
  | Keyword(u, _, _)
  | FreeVar(u, _, _)
  | InvalidText(u, _, _) => Some(u)
  | _ => None
  };
};

let rec diff_dhexp = (d1: DHExp.t, d2: DHExp.t): t => {
  /* First, we compare two expressions of the same form, to see if the current
     node should be the diff root. If the expressions are not of the same form,
     we check if `d1` is a hole expression. */
  switch (d1, d2) {
  /* Closures should not appear in elaborated, non-evaluated result.
     Don't technically need to throw an error since this should never come up.
     But here just in case. */
  | (Closure(_), _)
  | (_, Closure(_)) => raise(Exception(ClosureInUnevaluatedExp))

  /* Diffing leaf expressions: if different, this is the (non-fill)
     root of the diff */
  | (Triv, Triv)
  | (BoolLit(_), BoolLit(_))
  | (IntLit(_), IntLit(_))
  | (FloatLit(_), FloatLit(_))
  | (BoundVar(_), BoundVar(_))
  | (ListNil(_), ListNil(_)) => d1 == d2 ? NoDiff : NonFillDiff

  /* Diffing expressions with one subexpression:
     - If current node is diff, then current node is diff root.
     - Else use child's diff. */
  | (Inj(ty1, side1, d1'), Inj(ty2, side2, d2')) =>
    ty1 == ty2 && side1 == side2 ? diff_dhexp(d1', d2') : NonFillDiff
  | (FixF(x1, ty1, d1'), FixF(x2, ty2, d2')) =>
    x1 == x2 && ty1 == ty2 ? diff_dhexp(d1', d2') : NonFillDiff
  | (Fun(dp1, ty1, d1'), Fun(dp2, ty2, d2')) =>
    dp1 == dp2 && ty1 == ty2 ? diff_dhexp(d1', d2') : NonFillDiff
  | (Cast(d1', ty1, ty1'), Cast(d2', ty2, ty2')) =>
    ty1 == ty2 && ty1' == ty2' ? diff_dhexp(d1', d2') : NonFillDiff
  | (FailedCast(d1', ty1, ty1'), FailedCast(d2', ty2, ty2')) =>
    ty1 == ty2 && ty1' == ty2' ? diff_dhexp(d1', d2') : NonFillDiff
  | (InvalidOperation(d1', reason1), InvalidOperation(d2', reason2)) =>
    reason1 == reason2 ? diff_dhexp(d1', d2') : NonFillDiff

  /* Diffing expressions with more than one subexpression:
     - If current node is diff, then current node is diff root.
     - Else if no children diff, then no diff.
     - Else if multiple children diff, then current node is diff root.
     - Else if one child has non-fill-diff, then current node is diff root.
     - Else one child has fill-diff, carry through one child's diff.
     */
  /*
    TODO: working here
    | Let(DHPat.t, t, t)
    | Ap(t, t)
    | ApBuiltin(string, list(t))
    | BinBoolOp(BinBoolOp.t, t, t)
    | BinIntOp(BinIntOp.t, t, t)
    | BinFloatOp(BinFloatOp.t, t, t)
    | Cons(t, t)
    | Pair(t, t)
    | ConsistentCase(case)
   */

  /* Diffing hole expressions: if the hole is different or if there is a
     non-fill child diff, then this becomes the new root of the diff. */
  | (EmptyHole(u, _), EmptyHole(_))
  | (Keyword(u, _, _), Keyword(_))
  | (FreeVar(u, _, _), FreeVar(_))
  | (InvalidText(u, _, _), InvalidText(_)) =>
    d1 == d2 ? NoDiff : FillDiff(d2, u)
  | (NonEmptyHole(reason1, u1, i1, d1'), NonEmptyHole(reason2, u2, i2, d2')) =>
    if (reason1 != reason2 || u1 != u2 || i1 != i2) {
      FillDiff(d2, u1);
    } else {
      switch (diff_dhexp(d1', d2')) {
      | NoDiff => NoDiff
      | NonFillDiff => FillDiff(d2, u1)
      | FillDiff(_) as diff => diff
      };
    }
  | (
      InconsistentBranches(_u1, _i1, _case1),
      InconsistentBranches(_u2, _i2, _case2),
    ) =>
    /* TODO: implement case diff */
    raise(Exception(DiffNotImplemented))

  /* If non-equal: check if `d1` is a hole <=> fill diff. */
  | _ =>
    switch (get_hole_number_of_dhexp(d1)) {
    | Some(u) => FillDiff(d2, u)
    | None => NonFillDiff
    }
  };
};
