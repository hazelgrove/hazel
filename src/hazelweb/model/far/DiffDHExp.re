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
  | (Triv, Triv) => NoDiff
  | (BoolLit(_), BoolLit(_))
  | (IntLit(_), IntLit(_))
  | (FloatLit(_), FloatLit(_))
  | (BoundVar(_), BoundVar(_))
  | (ListNil(_), ListNil(_)) => d1 == d2 ? NoDiff : NonFillDiff

  /* Diffing expressions with one subexpression:
     - If current node is diff, then current node is diff root.
     - Else use child's diff. */
  | (Inj(ty1, side1, d1'), Inj(ty2, side2, d2')) =>
    ty1 != ty2 || side1 != side2 ? NonFillDiff : diff_dhexp(d1', d2')
  | (FixF(x1, ty1, d1'), FixF(x2, ty2, d2')) =>
    x1 != x2 || ty1 != ty2 ? NonFillDiff : diff_dhexp(d1', d2')
  | (Fun(dp1, ty1, d1'), Fun(dp2, ty2, d2')) =>
    dp1 != dp2 || ty1 != ty2 ? NonFillDiff : diff_dhexp(d1', d2')
  | (Cast(d1', ty1, ty1'), Cast(d2', ty2, ty2'))
  | (FailedCast(d1', ty1, ty1'), FailedCast(d2', ty2, ty2')) =>
    ty1 != ty2 || ty1' != ty2' ? NonFillDiff : diff_dhexp(d1', d2')
  | (InvalidOperation(d1', reason1), InvalidOperation(d2', reason2)) =>
    reason1 != reason2 ? NonFillDiff : diff_dhexp(d1', d2')

  /* Diffing expressions with more than one subexpression:
     - If current node is diff, then current node is diff root.
     - Else check children (see `diff_children2`)
     */
  | (Let(dp1, d11, d12), Let(dp2, d21, d22)) =>
    dp1 != dp2 ? NonFillDiff : diff_children2(d11, d12, d21, d22)
  | (Ap(d11, d12), Ap(d21, d22))
  | (Cons(d11, d12), Cons(d21, d22))
  | (Pair(d11, d12), Pair(d21, d22)) => diff_children2(d11, d12, d21, d22)
  | (BinBoolOp(op1, d11, d12), BinBoolOp(op2, d21, d22)) =>
    op1 != op2 ? NonFillDiff : diff_children2(d11, d12, d21, d22)
  | (BinIntOp(op1, d11, d12), BinIntOp(op2, d21, d22)) =>
    op1 != op2 ? NonFillDiff : diff_children2(d11, d12, d21, d22)
  | (BinFloatOp(op1, d11, d12), BinFloatOp(op2, d21, d22)) =>
    op1 != op2 ? NonFillDiff : diff_children2(d11, d12, d21, d22)

  /* Expression variants with >2 subexpressions */
  | (ApBuiltin(f1, args1), ApBuiltin(f2, args2)) =>
    f1 != f2 ? NonFillDiff : diff_children(args1, args2)
  | (ConsistentCase(case1), ConsistentCase(case2)) =>
    diff_case(case1, case2)

  /* Diffing hole expressions: if the hole is different or if there is a
     non-fill child diff, then this becomes the new root of the diff. */
  | (EmptyHole(u, _), EmptyHole(_))
  | (Keyword(u, _, _), Keyword(_))
  | (FreeVar(u, _, _), FreeVar(_))
  | (InvalidText(u, _, _), InvalidText(_)) =>
    d1 != d2 ? FillDiff(d2, u) : NoDiff
  | (NonEmptyHole(reason1, u1, i1, d1'), NonEmptyHole(reason2, u2, i2, d2')) =>
    if (reason1 != reason2 || u1 != u2 || i1 != i2) {
      FillDiff(d2, u1);
    } else {
      switch (diff_dhexp(d1', d2')) {
      | NonFillDiff => FillDiff(d2, u1)
      | diff => diff
      };
    }
  | (
      InconsistentBranches(u1, i1, case1),
      InconsistentBranches(u2, i2, case2),
    ) =>
    if (u1 != u2 || i1 != i2) {
      FillDiff(d2, u1);
    } else {
      switch (diff_case(case1, case2)) {
      | NonFillDiff => FillDiff(d2, u1)
      | diff => diff
      };
    }

  /* If different variants, then necessarily a diff.
     Check if `d1` is a hole <=> fill diff. */
  | _ =>
    switch (get_hole_number_of_dhexp(d1)) {
    | Some(u) => FillDiff(d2, u)
    | None => NonFillDiff
    }
  };
}

/* Helper for diffing multiple subexpressions:
   - If no children diff, then no diff.
   - Else if multiple children diff, then current node is diff root
     (non-fill-diff).
   - Else carry through child's diff. */
and diff_children = (ds1: list(DHExp.t), ds2: list(DHExp.t)): t => {
  let diffs =
    List.map2(diff_dhexp, ds1, ds2)
    |> List.filter((diff: t) =>
         switch (diff) {
         | NoDiff => false
         | FillDiff(_)
         | NonFillDiff => true
         }
       );
  switch (diffs) {
  | [] => NoDiff
  | [diff] => diff
  | _diffs => NonFillDiff
  };
}

/* This function is for the special case of two children. */
and diff_children2 =
    (d11: DHExp.t, d12: DHExp.t, d21: DHExp.t, d22: DHExp.t): t =>
  switch (diff_dhexp(d11, d21), diff_dhexp(d12, d22)) {
  | (NoDiff, NoDiff) => NoDiff
  | (FillDiff(_) as diff, NoDiff)
  | (NoDiff, FillDiff(_) as diff) => diff
  | _ => NonFillDiff
  }

/* Helper for diffing case expressions. If any of the patterns
   or the `i`s are different, then the case is the root. Otherwise,
   apply `diff_children` to the scrut and rule bodies. */
and diff_case =
    (
      Case(scrut1, rules1, i1): DHExp.case,
      Case(scrut2, rules2, i2): DHExp.case,
    )
    : t => {
  let dp_of_rule = (Rule(dp, _): DHExp.rule): DHPat.t => dp;
  let d_of_rule = (Rule(_, d): DHExp.rule): DHExp.t => d;
  if (i1 != i2
      || rules1
      |> List.map(dp_of_rule) != (rules2 |> List.map(dp_of_rule))) {
    NonFillDiff;
  } else {
    diff_children(
      [scrut1, ...rules1 |> List.map(d_of_rule)],
      [scrut2, ...rules2 |> List.map(d_of_rule)],
    );
  };
};
