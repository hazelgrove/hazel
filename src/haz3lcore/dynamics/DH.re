module rec DHExp: {
  include (module type of Term.UExp);
  module BinBoolOp: {type t = op_bin_bool;};

  module BinIntOp: {type t = op_bin_int;};

  module BinFloatOp: {type t = op_bin_float;};

  module BinStringOp: {type t = op_bin_string;};

  type case = (t, list((Term.UPat.t, t)), int);
  type rule = (DHPat.t, t);

  let constructor_string: t => string;

  let mk_tuple: list(t) => t;

  let cast: (t, Typ.t, Typ.t) => t;

  let apply_casts: (t, list((Typ.t, Typ.t))) => t;
  let strip_casts: t => t;

  let fast_equal: (t, t) => bool;
} = {
  include Term.UExp;

  module BinBoolOp = {
    type t = op_bin_bool;
  };

  module BinIntOp = {
    type t = op_bin_int;
  };

  module BinFloatOp = {
    type t = op_bin_float;
  };

  module BinStringOp = {
    type t = op_bin_string;
  };

  type case = (t, list((Term.UPat.t, t)), int);
  type rule = (Term.UPat.t, t);

  let constructor_string = (d: t): string =>
    switch (d.term) {
    | Error(_) => failwith("constructor_string Error not in Hole")
    | EmptyHole => failwith("constructor_string EmptyHole")
    | MultiHole(_) => failwith("constructor_string MultHole")
    | Hole(_, he) =>
      switch (he.term) {
      | EmptyHole => "EmptyHole"
      | Error(NonEmptyHole(_, _)) => "NonEmptyHole"
      | Error(ExpandingKeyword(_)) => "ExpandingKeyword"
      | Error(FreeVar(_)) => "FreeVar"
      | Error(InvalidText(_)) => "InvalidText"
      | Error(InconsistentBranches(_)) => "InconsistentBranches"
      | Error(FailedCast(_)) => "FailedCast"
      | Error(InvalidOperation(_)) => "InvalidOperation"
      | _ => failwith("constructor_string Hole(_, _)")
      }
    | Triv => "Triv"
    | Var(_) => "BoundVar"
    | Seq(_, _) => "Sequence"
    | Let(_, _, _) => "Let"
    | FixF(_, _, _) => "FixF"
    | Fun(_, _, _, _) => "Fun"
    | Closure(_, _) => "Closure"
    | Ap(_, _) => "Ap"
    | If(_, _, _) =>
      failwith("constructor_string If, should elaborated to Match")
    | Parens(_) => "Parens"
    | Test(_) => "TestLit"
    | Bool(_) => "BoolLit"
    | Int(_) => "IntLit"
    | Float(_) => "FloatLit"
    | String(_) => "StringLit"
    | BinOp(_, _, _) => "BinOp"
    | UnOp(_, _) => "UnOp"
    | ListLit(_) => "ListLit"
    | Cons(_, _) => "Cons"
    | Tuple(_) => "Tuple"
    | Prj(_) => "Prj"
    | Inj(_, _, _) => "Inj"
    | Tag(_) => "Tag"
    | Match(_) => "ConsistentCase"
    | Cast(_, _, _) => "Cast"
    };

  let mk_tuple: list(t) => t =
    fun
    | []
    | [_] => failwith("mk_tuple: expected at least 2 elements")
    | xs => {
        ids: List.fold_left((acc, ts) => acc @ ts.ids, [], xs),
        term: Tuple(xs),
      };

  let cast = (d: t, t1: Typ.t, t2: Typ.t): t =>
    switch (d.term, t2) {
    | (ListLit([], _), List(_)) =>
      //HACK(andrew, cyrus)
      d
    | _ =>
      if (Typ.eq(t1, t2) || t2 == Unknown(SynSwitch)) {
        d;
      } else {
        {ids: d.ids, term: Cast(d, t1, t2)};
      }
    };

  let apply_casts = (d: t, casts: list((Typ.t, Typ.t))): t =>
    List.fold_left(
      (d, c: (Typ.t, Typ.t)) => {
        let (ty1, ty2) = c;
        cast(d, ty1, ty2);
      },
      d,
      casts,
    );

  let rec strip_casts = (dexp: t): t => {
    ids: dexp.ids,
    term:
      switch (dexp.term) {
      | Error(_) => failwith("strip_casts on Error outside of Hole")
      | EmptyHole => failwith("strip_casts on EmptyHole")
      | MultiHole(_) => failwith("strip_casts on MultiHole")
      | Closure(ei, d) => Closure(ei, strip_casts(d))
      | Cast(d, _, _) => strip_casts(d).term
      | Hole(_, {ids: _, term: Error(FailedCast(d, _, _))}) =>
        strip_casts(d).term
      | Inj(ty, side, d) => Inj(ty, side, strip_casts(d))
      | Tuple(ds) => Tuple(ds |> List.map(strip_casts))
      | Prj(d, n) => Prj(strip_casts(d), n)
      | Parens(d) => Parens(strip_casts(d))
      | Cons(d1, d2) => Cons(strip_casts(d1), strip_casts(d2))
      | ListLit(ds, info) => ListLit(List.map(strip_casts, ds), info)
      | Hole(hi, {ids, term: Error(NonEmptyHole(err, d))}) =>
        Hole(hi, {ids, term: Error(NonEmptyHole(err, strip_casts(d)))})
      | Seq(a, b) => Seq(strip_casts(a), strip_casts(b))
      | Let(dp, b, c) => Let(dp, strip_casts(b), strip_casts(c))
      | FixF(a, b, c) => FixF(a, b, strip_casts(c))
      | Fun(a, b, c, d) => Fun(a, b, strip_casts(c), d)
      | Ap(a, b) => Ap(strip_casts(a), strip_casts(b))
      // | ApBuiltin(fn, args) => ApBuiltin(fn, List.map(strip_casts, args))
      | BinOp(a, b, c) => BinOp(a, strip_casts(b), strip_casts(c))
      | UnOp(a, b) => UnOp(a, strip_casts(b))
      | If(_, _, _) =>
        failwith("strip_casts on If, which should be elaborated to Match")
      | Match(a, rs, b) =>
        Match(strip_casts(a), List.map(strip_casts_rule, rs), b)
      | Hole(hi, {ids, term: Error(InconsistentBranches(scrut, rules, n))}) =>
        Hole(
          hi,
          {
            ids,
            term:
              Error(
                InconsistentBranches(
                  strip_casts(scrut),
                  List.map(strip_casts_rule, rules),
                  n,
                ),
              ),
          },
        )
      | Hole(_, {ids: _, term: EmptyHole}) as d
      | Hole(_, {ids: _, term: Error(ExpandingKeyword(_))}) as d
      | Hole(_, {ids: _, term: Error(FreeVar(_))}) as d
      | Hole(_, {ids: _, term: Error(InvalidText(_))}) as d
      | Triv as d
      | Var(_) as d
      | Test(_) as d
      | Bool(_) as d
      | Int(_) as d
      | Float(_) as d
      | String(_) as d
      | Tag(_) as d
      | Hole(_, {ids: _, term: Error(InvalidOperation(_))}) as d => d
      | Hole(_) => failwith("strip_casts on Invalid Hole")
      },
  }
  and strip_casts_rule = ((a, d)) => (a, strip_casts(d));

  let rec fast_equal = (d1: t, d2: t): bool => {
    switch (d1.term, d2.term) {
    | (Error(_), _)
    | (_, Error(_)) => failwith("fast_equal on Error outside of Hole")
    | (EmptyHole, _)
    | (_, EmptyHole) => failwith("fast_equal on EmptyHole")
    | (MultiHole(_), _)
    | (_, MultiHole(_)) => failwith("fast_equal on MultiHole")
    | (If(_), _)
    | (_, If(_)) => failwith("fast_equal on If")
    /* Primitive forms: regular structural equality */
    | (Triv, _)
    | (Var(_), _)
    /* TODO: Not sure if this is right... */
    | (Test(_), _)
    | (Bool(_), _)
    | (Int(_), _)
    | (Float(_), _)
    | (Tag(_), _) => d1 == d2
    | (String(s1), String(s2)) => String.equal(s1, s2)
    | (String(_), _) => false

    /* Non-hole forms: recurse */
    | (Seq(d11, d21), Seq(d12, d22)) =>
      fast_equal(d11, d12) && fast_equal(d21, d22)
    | (Let(dp1, d11, d21), Let(dp2, d12, d22)) =>
      dp1 == dp2 && fast_equal(d11, d12) && fast_equal(d21, d22)
    | (FixF(f1, ty1, d1), FixF(f2, ty2, d2)) =>
      f1 == f2 && ty1 == ty2 && fast_equal(d1, d2)
    | (Fun(dp1, ty1, d1, s1), Fun(dp2, ty2, d2, s2)) =>
      dp1 == dp2 && ty1 == ty2 && fast_equal(d1, d2) && s1 == s2
    | (Ap(d11, d21), Ap(d12, d22))
    | (Cons(d11, d21), Cons(d12, d22)) =>
      fast_equal(d11, d12) && fast_equal(d21, d22)
    | (Parens(d1), Parens(d2)) => fast_equal(d1, d2)
    | (Tuple(ds1), Tuple(ds2)) =>
      List.length(ds1) == List.length(ds2)
      && List.for_all2(fast_equal, ds1, ds2)
    | (Prj(d1, n), Prj(d2, m)) => n == m && fast_equal(d1, d2)
    // | (ApBuiltin(f1, args1), ApBuiltin(f2, args2)) =>
    //   f1 == f2 && List.for_all2(fast_equal, args1, args2)
    | (ListLit(ds1, _), ListLit(ds2, _)) =>
      List.for_all2(fast_equal, ds1, ds2)
    | (BinOp(op1, d11, d21), BinOp(op2, d12, d22)) =>
      op1 == op2 && fast_equal(d11, d12) && fast_equal(d21, d22)
    | (UnOp(op1, d11), UnOp(op2, d12)) =>
      op1 == op2 && fast_equal(d11, d12)
    | (Inj(ty1, side1, d1), Inj(ty2, side2, d2)) =>
      ty1 == ty2 && side1 == side2 && fast_equal(d1, d2)
    | (Cast(d1, ty11, ty21), Cast(d2, ty12, ty22))
    | (
        Hole(_, {ids: _, term: Error(FailedCast(d1, ty11, ty21))}),
        Hole(_, {ids: _, term: Error(FailedCast(d2, ty12, ty22))}),
      ) =>
      fast_equal(d1, d2) && ty11 == ty12 && ty21 == ty22
    | (
        Hole(_, {ids: _, term: Error(InvalidOperation(reason1, d1))}),
        Hole(_, {ids: _, term: Error(InvalidOperation(reason2, d2))}),
      ) =>
      fast_equal(d1, d2) && reason1 == reason2
    | (Match(scrut1, rule1, i1), Match(scrut2, rule2, i2)) =>
      let case1 = (scrut1, rule1, i1);
      let case2 = (scrut2, rule2, i2);
      fast_equal_case(case1, case2);
    /* We can group these all into a `_ => false` clause; separating
       these so that we get exhaustiveness checking. */
    | (Seq(_), _)
    | (Let(_), _)
    | (FixF(_), _)
    | (Fun(_), _)
    | (Ap(_), _)
    // | (ApBuiltin(_), _)
    | (Parens(_), _)
    | (Cons(_), _)
    | (ListLit(_), _)
    | (Tuple(_), _)
    | (Prj(_), _)
    | (BinOp(_), _)
    | (UnOp(_), _)
    | (Inj(_), _)
    | (Cast(_), _)
    | (Hole(_, {ids: _, term: Error(FailedCast(_))}), _)
    | (Hole(_, {ids: _, term: Error(InvalidOperation(_))}), _)
    | (Match(_), _) => false

    /* Hole forms: when checking environments, only check that
       environment ID's are equal, don't check structural equality.

       (This resolves a performance issue with many nested holes.) */
    | (
        Hole((u1, i1), {ids: _, term: EmptyHole}),
        Hole((u2, i2), {ids: _, term: EmptyHole}),
      ) =>
      u1 == u2 && i1 == i2
    | (
        Hole((u1, i1), {ids: _, term: Error(NonEmptyHole(reason1, d1))}),
        Hole((u2, i2), {ids: _, term: Error(NonEmptyHole(reason2, d2))}),
      ) =>
      reason1 == reason2 && u1 == u2 && i1 == i2 && fast_equal(d1, d2)
    | (
        Hole((u1, i1), {ids: _, term: Error(ExpandingKeyword(kw1))}),
        Hole((u2, i2), {ids: _, term: Error(ExpandingKeyword(kw2))}),
      ) =>
      u1 == u2 && i1 == i2 && kw1 == kw2
    | (
        Hole((u1, i1), {ids: _, term: Error(FreeVar(x1))}),
        Hole((u2, i2), {ids: _, term: Error(FreeVar(x2))}),
      ) =>
      u1 == u2 && i1 == i2 && x1 == x2
    | (
        Hole((u1, i1), {ids: _, term: Error(InvalidText(text1))}),
        Hole((u2, i2), {ids: _, term: Error(InvalidText(text2))}),
      ) =>
      u1 == u2 && i1 == i2 && text1 == text2
    | (Closure(sigma1, d1), Closure(sigma2, d2)) =>
      ClosureEnvironment.id_equal(sigma1, sigma2) && fast_equal(d1, d2)
    | (
        Hole(
          (u1, hi1),
          {ids: _, term: Error(InconsistentBranches(s1, r1, i1))},
        ),
        Hole(
          (u2, hi2),
          {ids: _, term: Error(InconsistentBranches(s2, r2, i2))},
        ),
      ) =>
      let case1 = (s1, r1, i1);
      let case2 = (s2, r2, i2);
      u1 == u2 && hi1 == hi2 && fast_equal_case(case1, case2);
    | (Hole(_, {ids: _, term: EmptyHole}), _)
    | (Hole(_, {ids: _, term: Error(NonEmptyHole(_))}), _)
    | (Hole(_, {ids: _, term: Error(ExpandingKeyword(_))}), _)
    | (Hole(_, {ids: _, term: Error(FreeVar(_))}), _)
    | (Hole(_, {ids: _, term: Error(InvalidText(_))}), _)
    | (Hole(_, {ids: _, term: Error(InconsistentBranches(_))}), _)
    | (Hole(_), _)
    | (Closure(_), _) => false
    };
  }
  and fast_equal_case = ((d1, rules1, i1), (d2, rules2, i2)) => {
    fast_equal(d1, d2)
    && List.length(rules1) == List.length(rules2)
    && List.for_all2(
         ((dp1, d1), (dp2, d2)) => dp1 == dp2 && fast_equal(d1, d2),
         rules1,
         rules2,
       )
    && i1 == i2;
  };
}

and Environment: {
  include
     (module type of VarBstMap.Ordered) with
      type t_('a) = VarBstMap.Ordered.t_('a);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = t_(DHExp.t);
} = {
  include VarBstMap.Ordered;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = t_(DHExp.t);
}
and ClosureEnvironment: {include (module type of Term.ClosureEnvironment);} = {
  include Term.ClosureEnvironment;
};
