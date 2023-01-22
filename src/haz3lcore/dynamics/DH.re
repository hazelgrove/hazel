module rec DHExp: {
  include (module type of CH.CExp);

  module UnIntOp: {type t = op_un_int;};

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

  let mk: (CH.Ids.t, term) => t;
} = {
  include CH.CExp;

  module UnIntOp = {
    type t = op_un_int;
  };

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
    | Hole(_, _) => "Hole"
    | Triv => "Triv"
    | Var(_) => "BoundVar"
    | Seq(_, _) => "Sequence"
    | Let(_, _, _) => "Let"
    | FixF(_, _, _) => "FixF"
    | Fun(_, _, _, _) => "Fun"
    | Closure(_, _) => "Closure"
    | Ap(_, _) => "Ap"
    | ApBuiltin(_, _) => "ApBuiltin"
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
      | Closure(ei, d) => Closure(ei, strip_casts(d))
      | Cast(d, _, _) => strip_casts(d).term
      | Hole(_, FailedCast(d, _, _)) => strip_casts(d).term
      | Inj(ty, side, d) => Inj(ty, side, strip_casts(d))
      | Tuple(ds) => Tuple(ds |> List.map(strip_casts))
      | Prj(d, n) => Prj(strip_casts(d), n)
      | Parens(d) => Parens(strip_casts(d))
      | Cons(d1, d2) => Cons(strip_casts(d1), strip_casts(d2))
      | ListLit(ds, info) => ListLit(List.map(strip_casts, ds), info)
      | Hole(hi, NonEmptyHole(err, d)) => Hole(hi, NonEmptyHole(err, d))
      | Seq(a, b) => Seq(strip_casts(a), strip_casts(b))
      | Let(dp, b, c) => Let(dp, strip_casts(b), strip_casts(c))
      | FixF(a, b, c) => FixF(a, b, strip_casts(c))
      | Fun(a, b, c, d) => Fun(a, b, strip_casts(c), d)
      | Ap(a, b) => Ap(strip_casts(a), strip_casts(b))
      | ApBuiltin(fn, args) => ApBuiltin(fn, List.map(strip_casts, args))
      | BinOp(a, b, c) => BinOp(a, strip_casts(b), strip_casts(c))
      | UnOp(a, b) => UnOp(a, strip_casts(b))
      | If(c, t, e) => If(strip_casts(c), strip_casts(t), strip_casts(e))
      | Match(a, rs, b) =>
        Match(strip_casts(a), List.map(strip_casts_rule, rs), b)
      | Hole(hi, InconsistentBranches(scrut, rules, n)) =>
        Hole(
          hi,
          InconsistentBranches(
            strip_casts(scrut),
            List.map(strip_casts_rule, rules),
            n,
          ),
        )
      | Hole(_, _) as d
      | Triv as d
      | Var(_) as d
      | Test(_) as d
      | Bool(_) as d
      | Int(_) as d
      | Float(_) as d
      | String(_) as d
      | Tag(_) as d => d
      },
  }
  and strip_casts_rule = ((a, d)) => (a, strip_casts(d));

  let rec fast_equal = (d1, d2) => {
    switch (d1.term, d2.term) {
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
    | (ApBuiltin(f1, args1), ApBuiltin(f2, args2)) =>
      f1 == f2 && List.for_all2(fast_equal, args1, args2)
    | (ListLit(ds1, _), ListLit(ds2, _)) =>
      List.for_all2(fast_equal, ds1, ds2)
    | (BinOp(op1, d11, d21), BinOp(op2, d12, d22)) =>
      op1 == op2 && fast_equal(d11, d12) && fast_equal(d21, d22)
    | (UnOp(op1, d11), UnOp(op2, d12)) =>
      op1 == op2 && fast_equal(d11, d12)
    | (Inj(ty1, side1, d1), Inj(ty2, side2, d2)) =>
      ty1 == ty2 && side1 == side2 && fast_equal(d1, d2)
    | (Cast(d1, ty11, ty21), Cast(d2, ty12, ty22)) =>
      fast_equal(d1, d2) && ty11 == ty12 && ty21 == ty22
    | (
        Hole(ohi1, FailedCast(d1, ty11, ty21)),
        Hole(ohi2, FailedCast(d2, ty12, ty22)),
      ) =>
      ohi1 == ohi2 && fast_equal(d1, d2) && ty11 == ty12 && ty21 == ty22
    | (
        Hole(ohi1, InvalidOperation(reason1, d1)),
        Hole(ohi2, InvalidOperation(reason2, d2)),
      ) =>
      ohi1 == ohi2 && fast_equal(d1, d2) && reason1 == reason2
    | (If(c1, t1, e1), If(c2, t2, e2)) =>
      fast_equal(c1, c2) && fast_equal(t1, t2) && fast_equal(e1, e2)
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
    | (ApBuiltin(_), _)
    | (Parens(_), _)
    | (Cons(_), _)
    | (ListLit(_), _)
    | (Tuple(_), _)
    | (Prj(_), _)
    | (BinOp(_), _)
    | (UnOp(_), _)
    | (Inj(_), _)
    | (Cast(_), _)
    | (Hole(_, FailedCast(_)), _)
    | (Hole(_, InvalidOperation(_)), _)
    | (If(_), _)
    | (Match(_), _) => false

    /* Hole forms: when checking environments, only check that
       environment ID's are equal, don't check structural equality.

       (This resolves a performance issue with many nested holes.) */
    | (Hole(ohi1, EmptyHole), Hole(ohi2, EmptyHole)) => ohi1 == ohi2
    | (Hole(h1, MultiHole(_tms1)), Hole(h2, MultiHole(_tms2))) => h1 == h2
    | (
        Hole(ohi1, NonEmptyHole(reason1, d1)),
        Hole(ohi2, NonEmptyHole(reason2, d2)),
      ) =>
      reason1 == reason2 && ohi1 == ohi2 && fast_equal(d1, d2)
    | (Hole(h1, ExpandingKeyword(kw1)), Hole(h2, ExpandingKeyword(kw2))) =>
      h1 == h2 && kw1 == kw2
    | (Hole(h1, FreeVar(x1)), Hole(h2, FreeVar(x2))) => h1 == h2 && x1 == x2
    | (Hole(h1, InvalidText(text1)), Hole(h2, InvalidText(text2))) =>
      h1 == h2 && text1 == text2
    | (Closure(sigma1, d1), Closure(sigma2, d2)) =>
      ClosureEnvironment.id_equal(sigma1, sigma2) && fast_equal(d1, d2)
    | (
        Hole(h1, InconsistentBranches(s1, r1, i1)),
        Hole(h2, InconsistentBranches(s2, r2, i2)),
      ) =>
      let case1 = (s1, r1, i1);
      let case2 = (s2, r2, i2);
      h1 == h2 && fast_equal_case(case1, case2);
    | (Hole(_, EmptyHole), _)
    | (Hole(_, MultiHole(_)), _)
    | (Hole(_, NonEmptyHole(_)), _)
    | (Hole(_, ExpandingKeyword(_)), _)
    | (Hole(_, FreeVar(_)), _)
    | (Hole(_, InvalidText(_)), _)
    | (Hole(_, InconsistentBranches(_)), _)
    | (Hole(_, _), _)
    | (Closure(_), _) => false
    };
  }
  and fast_equal_case = ((d1, rules1, i1), (d2, rules2, i2)) => {
    let fast_equal_t = (d1: t, d2: t): bool => fast_equal(d1, d2);
    fast_equal_t(d1, d2)
    && List.length(rules1) == List.length(rules2)
    && List.for_all2(
         ((dp1, d1), (dp2, d2)) => dp1 == dp2 && fast_equal_t(d1, d2),
         rules1,
         rules2,
       )
    && i1 == i2;
  };

  let mk = (ids: CH.Ids.t, term: term): t => {ids, term};
};

module Environment = Environment;

module ClosureEnvironment = ClosureEnvironment;
