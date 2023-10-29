open Sexplib.Std;

module rec DHExp: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | EmptyHole(MetaVar.t, HoleInstanceId.t)
    | NonEmptyHole(ErrStatus.HoleReason.t, MetaVar.t, HoleInstanceId.t, t)
    | ExpandingKeyword(MetaVar.t, HoleInstanceId.t, ExpandingKeyword.t)
    | FreeVar(MetaVar.t, HoleInstanceId.t, Var.t)
    | InvalidText(MetaVar.t, HoleInstanceId.t, string)
    | InconsistentBranches(MetaVar.t, HoleInstanceId.t, case)
    | Closure(
        [@opaque] ClosureEnvironment.t,
        [@opaque] FilterEnvironment.t,
        t,
      )
    | BoundVar(Var.t)
    | Sequence(t, t)
    | Let(DHPat.t, t, t)
    | Filter(Filter.t, t)
    | FixF(Var.t, Typ.t, t)
    | Fun(DHPat.t, Typ.t, t, option(Var.t))
    | Ap(t, t)
    | ApBuiltin(string, list(t))
    | TestLit(KeywordID.t)
    | BoolLit(bool)
    | IntLit(int)
    | FloatLit(float)
    | StringLit(string)
    | BinBoolOp(TermBase.UExp.op_bin_bool, t, t)
    | BinIntOp(TermBase.UExp.op_bin_int, t, t)
    | BinFloatOp(TermBase.UExp.op_bin_float, t, t)
    | BinStringOp(TermBase.UExp.op_bin_string, t, t)
    | ListLit(MetaVar.t, MetaVarInst.t, Typ.t, list(t))
    | Cons(t, t)
    | ListConcat(t, t)
    | Tuple(list(t))
    | Prj(t, int)
    | Constructor(string)
    | ConsistentCase(case)
    | Cast(t, Typ.t, Typ.t)
    | FailedCast(t, Typ.t, Typ.t)
    | InvalidOperation(t, InvalidOperationError.t)
  and case =
    | Case(t, list(rule), int)
  and rule =
    | Rule(DHPat.t, t);

  let constructor_string: t => string;

  let mk_tuple: list(t) => t;

  let cast: (t, Typ.t, Typ.t) => t;

  let apply_casts: (t, list((Typ.t, Typ.t))) => t;
  let strip_casts: t => t;

  let fast_equal: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    /* Hole types */
    | EmptyHole(MetaVar.t, HoleInstanceId.t)
    | NonEmptyHole(ErrStatus.HoleReason.t, MetaVar.t, HoleInstanceId.t, t)
    | ExpandingKeyword(MetaVar.t, HoleInstanceId.t, ExpandingKeyword.t)
    | FreeVar(MetaVar.t, HoleInstanceId.t, Var.t)
    | InvalidText(MetaVar.t, HoleInstanceId.t, string)
    | InconsistentBranches(MetaVar.t, HoleInstanceId.t, case)
    /* Generalized closures */
    | Closure(
        [@opaque] ClosureEnvironment.t,
        [@opaque] FilterEnvironment.t,
        t,
      )
    /* Other expressions forms */
    | BoundVar(Var.t)
    | Sequence(t, t)
    | Let(DHPat.t, t, t)
    | Filter(Filter.t, t)
    | FixF(Var.t, Typ.t, t)
    | Fun(DHPat.t, Typ.t, t, option(Var.t))
    | Ap(t, t)
    | ApBuiltin(string, list(t))
    | TestLit(KeywordID.t)
    | BoolLit(bool)
    | IntLit(int)
    | FloatLit(float)
    | StringLit(string)
    | BinBoolOp(TermBase.UExp.op_bin_bool, t, t)
    | BinIntOp(TermBase.UExp.op_bin_int, t, t)
    | BinFloatOp(TermBase.UExp.op_bin_float, t, t)
    | BinStringOp(TermBase.UExp.op_bin_string, t, t)
    | ListLit(MetaVar.t, MetaVarInst.t, Typ.t, list(t))
    | Cons(t, t)
    | ListConcat(t, t)
    | Tuple(list(t))
    | Prj(t, int)
    | Constructor(string)
    | ConsistentCase(case)
    | Cast(t, Typ.t, Typ.t)
    | FailedCast(t, Typ.t, Typ.t)
    | InvalidOperation(t, InvalidOperationError.t)
  and case =
    | Case(t, list(rule), int)
  and rule =
    | Rule(DHPat.t, t);

  let constructor_string = (d: t): string =>
    switch (d) {
    | EmptyHole(_, _) => "EmptyHole"
    | NonEmptyHole(_, _, _, _) => "NonEmptyHole"
    | ExpandingKeyword(_, _, _) => "ExpandingKeyword"
    | FreeVar(_, _, _) => "FreeVar"
    | InvalidText(_) => "InvalidText"
    | BoundVar(_) => "BoundVar"
    | Sequence(_, _) => "Sequence"
    | Filter(_, _) => "Filter"
    | Let(_, _, _) => "Let"
    | FixF(_, _, _) => "FixF"
    | Fun(_, _, _, _) => "Fun"
    | Closure(_, _, _) => "Closure"
    | Ap(_, _) => "Ap"
    | ApBuiltin(_, _) => "ApBuiltin"
    | TestLit(_) => "TestLit"
    | BoolLit(_) => "BoolLit"
    | IntLit(_) => "IntLit"
    | FloatLit(_) => "FloatLit"
    | StringLit(_) => "StringLit"
    | BinBoolOp(_, _, _) => "BinBoolOp"
    | BinIntOp(_, _, _) => "BinIntOp"
    | BinFloatOp(_, _, _) => "BinFloatOp"
    | BinStringOp(_, _, _) => "BinStringOp"
    | ListLit(_) => "ListLit"
    | Cons(_, _) => "Cons"
    | ListConcat(_, _) => "ListConcat"
    | Tuple(_) => "Tuple"
    | Prj(_) => "Prj"
    | Constructor(_) => "Constructor"
    | ConsistentCase(_) => "ConsistentCase"
    | InconsistentBranches(_, _, _) => "InconsistentBranches"
    | Cast(_, _, _) => "Cast"
    | FailedCast(_, _, _) => "FailedCast"
    | InvalidOperation(_) => "InvalidOperation"
    };

  let mk_tuple: list(t) => t =
    fun
    | []
    | [_] => failwith("mk_tuple: expected at least 2 elements")
    | xs => Tuple(xs);

  let cast = (d: t, t1: Typ.t, t2: Typ.t): t =>
    if (Typ.eq(t1, t2) || t2 == Unknown(SynSwitch)) {
      d;
    } else {
      Cast(d, t1, t2);
    };

  let apply_casts = (d: t, casts: list((Typ.t, Typ.t))): t =>
    List.fold_left((d, (ty1, ty2)) => cast(d, ty1, ty2), d, casts);

  let rec strip_casts =
    fun
    | Closure(ei, ef, d) =>
      Closure(ei, FilterEnvironment.strip_casts(ef), strip_casts(d))
    | Cast(d, _, _) => strip_casts(d)
    | FailedCast(d, _, _) => strip_casts(d)
    | Tuple(ds) => Tuple(ds |> List.map(strip_casts))
    | Prj(d, n) => Prj(strip_casts(d), n)
    | Cons(d1, d2) => Cons(strip_casts(d1), strip_casts(d2))
    | ListConcat(d1, d2) => ListConcat(strip_casts(d1), strip_casts(d2))
    | ListLit(a, b, c, ds) => ListLit(a, b, c, List.map(strip_casts, ds))
    | NonEmptyHole(err, u, i, d) => NonEmptyHole(err, u, i, strip_casts(d))
    | Sequence(a, b) => Sequence(strip_casts(a), strip_casts(b))
    | Filter(f, b) => Filter(Filter.strip_casts(f), strip_casts(b))
    | Let(dp, b, c) => Let(dp, strip_casts(b), strip_casts(c))
    | FixF(a, b, c) => FixF(a, b, strip_casts(c))
    | Fun(a, b, c, d) => Fun(a, b, strip_casts(c), d)
    | Ap(a, b) => Ap(strip_casts(a), strip_casts(b))
    | ApBuiltin(fn, args) => ApBuiltin(fn, List.map(strip_casts, args))
    | BinBoolOp(a, b, c) => BinBoolOp(a, strip_casts(b), strip_casts(c))
    | BinIntOp(a, b, c) => BinIntOp(a, strip_casts(b), strip_casts(c))
    | BinFloatOp(a, b, c) => BinFloatOp(a, strip_casts(b), strip_casts(c))
    | BinStringOp(a, b, c) =>
      BinStringOp(a, strip_casts(b), strip_casts(c))
    | ConsistentCase(Case(a, rs, b)) =>
      ConsistentCase(
        Case(strip_casts(a), List.map(strip_casts_rule, rs), b),
      )
    | InconsistentBranches(u, i, Case(scrut, rules, n)) =>
      InconsistentBranches(
        u,
        i,
        Case(strip_casts(scrut), List.map(strip_casts_rule, rules), n),
      )
    | EmptyHole(_) as d
    | ExpandingKeyword(_) as d
    | FreeVar(_) as d
    | InvalidText(_) as d
    | BoundVar(_) as d
    | TestLit(_) as d
    | BoolLit(_) as d
    | IntLit(_) as d
    | FloatLit(_) as d
    | StringLit(_) as d
    | Constructor(_) as d
    | InvalidOperation(_) as d => d
  and strip_casts_rule = (Rule(a, d)) => Rule(a, strip_casts(d));

  let rec fast_equal = (d1: t, d2: t): bool => {
    switch (d1, d2) {
    /* Primitive forms: regular structural equality */
    | (BoundVar(_), _)
    /* TODO: Not sure if this is right... */
    | (TestLit(_), _)
    | (BoolLit(_), _)
    | (IntLit(_), _)
    | (FloatLit(_), _)
    | (Constructor(_), _) => d1 == d2
    | (StringLit(s1), StringLit(s2)) => String.equal(s1, s2)
    | (StringLit(_), _) => false

    /* Non-hole forms: recurse */
    | (Sequence(d11, d21), Sequence(d12, d22)) =>
      fast_equal(d11, d12) && fast_equal(d21, d22)
    | (Filter(f1, d1), Filter(f2, d2)) =>
      Filter.fast_equal(f1, f2) && fast_equal(d1, d2)
    | (Let(dp1, d11, d21), Let(dp2, d12, d22)) =>
      dp1 == dp2 && fast_equal(d11, d12) && fast_equal(d21, d22)
    | (FixF(f1, ty1, d1), FixF(f2, ty2, d2)) =>
      f1 == f2 && ty1 == ty2 && fast_equal(d1, d2)
    | (Fun(dp1, ty1, d1, s1), Fun(dp2, ty2, d2, s2)) =>
      dp1 == dp2 && ty1 == ty2 && fast_equal(d1, d2) && s1 == s2
    | (Ap(d11, d21), Ap(d12, d22))
    | (Cons(d11, d21), Cons(d12, d22)) =>
      fast_equal(d11, d12) && fast_equal(d21, d22)
    | (ListConcat(d11, d21), ListConcat(d12, d22)) =>
      fast_equal(d11, d12) && fast_equal(d21, d22)
    | (Tuple(ds1), Tuple(ds2)) =>
      List.length(ds1) == List.length(ds2)
      && List.for_all2(fast_equal, ds1, ds2)
    | (Prj(d1, n), Prj(d2, m)) => n == m && fast_equal(d1, d2)
    | (ApBuiltin(f1, args1), ApBuiltin(f2, args2)) =>
      f1 == f2 && List.for_all2(fast_equal, args1, args2)
    | (ListLit(_, _, _, ds1), ListLit(_, _, _, ds2)) =>
      List.for_all2(fast_equal, ds1, ds2)
    | (BinBoolOp(op1, d11, d21), BinBoolOp(op2, d12, d22)) =>
      op1 == op2 && fast_equal(d11, d12) && fast_equal(d21, d22)
    | (BinIntOp(op1, d11, d21), BinIntOp(op2, d12, d22)) =>
      op1 == op2 && fast_equal(d11, d12) && fast_equal(d21, d22)
    | (BinFloatOp(op1, d11, d21), BinFloatOp(op2, d12, d22)) =>
      op1 == op2 && fast_equal(d11, d12) && fast_equal(d21, d22)
    | (BinStringOp(op1, d11, d21), BinStringOp(op2, d12, d22)) =>
      op1 == op2 && fast_equal(d11, d12) && fast_equal(d21, d22)
    | (Cast(d1, ty11, ty21), Cast(d2, ty12, ty22))
    | (FailedCast(d1, ty11, ty21), FailedCast(d2, ty12, ty22)) =>
      fast_equal(d1, d2) && ty11 == ty12 && ty21 == ty22
    | (InvalidOperation(d1, reason1), InvalidOperation(d2, reason2)) =>
      fast_equal(d1, d2) && reason1 == reason2
    | (ConsistentCase(case1), ConsistentCase(case2)) =>
      fast_equal_case(case1, case2)
    /* We can group these all into a `_ => false` clause; separating
       these so that we get exhaustiveness checking. */
    | (Sequence(_), _)
    | (Filter(_), _)
    | (Let(_), _)
    | (FixF(_), _)
    | (Fun(_), _)
    | (Ap(_), _)
    | (ApBuiltin(_), _)
    | (Cons(_), _)
    | (ListConcat(_), _)
    | (ListLit(_), _)
    | (Tuple(_), _)
    | (Prj(_), _)
    | (BinBoolOp(_), _)
    | (BinIntOp(_), _)
    | (BinFloatOp(_), _)
    | (BinStringOp(_), _)
    | (Cast(_), _)
    | (FailedCast(_), _)
    | (InvalidOperation(_), _)
    | (ConsistentCase(_), _) => false

    /* Hole forms: when checking environments, only check that
       environment ID's are equal, don't check structural equality.

       (This resolves a performance issue with many nested holes.) */
    | (EmptyHole(u1, i1), EmptyHole(u2, i2)) => u1 == u2 && i1 == i2
    | (NonEmptyHole(reason1, u1, i1, d1), NonEmptyHole(reason2, u2, i2, d2)) =>
      reason1 == reason2 && u1 == u2 && i1 == i2 && fast_equal(d1, d2)
    | (ExpandingKeyword(u1, i1, kw1), ExpandingKeyword(u2, i2, kw2)) =>
      u1 == u2 && i1 == i2 && kw1 == kw2
    | (FreeVar(u1, i1, x1), FreeVar(u2, i2, x2)) =>
      u1 == u2 && i1 == i2 && x1 == x2
    | (InvalidText(u1, i1, text1), InvalidText(u2, i2, text2)) =>
      u1 == u2 && i1 == i2 && text1 == text2
    | (Closure(sigma1, fenv1, d1), Closure(sigma2, fenv2, d2)) =>
      ClosureEnvironment.id_equal(sigma1, sigma2)
      && FilterEnvironment.fast_equal(fenv1, fenv2)
      && fast_equal(d1, d2)
    | (
        InconsistentBranches(u1, i1, case1),
        InconsistentBranches(u2, i2, case2),
      ) =>
      u1 == u2 && i1 == i2 && fast_equal_case(case1, case2)
    | (EmptyHole(_), _)
    | (NonEmptyHole(_), _)
    | (ExpandingKeyword(_), _)
    | (FreeVar(_), _)
    | (InvalidText(_), _)
    | (Closure(_), _)
    | (InconsistentBranches(_), _) => false
    };
  }
  and fast_equal_case = (Case(d1, rules1, i1), Case(d2, rules2, i2)) => {
    fast_equal(d1, d2)
    && List.length(rules1) == List.length(rules2)
    && List.for_all2(
         (Rule(dp1, d1), Rule(dp2, d2)) =>
           dp1 == dp2 && fast_equal(d1, d2),
         rules1,
         rules2,
       )
    && i1 == i2;
  };
}

and Filter: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    pat: DHExp.t,
    act: FilterAction.t,
  };

  let matches: (ClosureEnvironment.t, DHExp.t, t) => option(FilterAction.t);

  let fast_equal: (t, t) => bool;

  let strip_casts: t => t;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    pat: DHExp.t,
    act: FilterAction.t,
  };

  let matches =
      (env: ClosureEnvironment.t, d: DHExp.t, f: t): option(FilterAction.t) => {
    print_endline("======== Filter.matches ========");
    print_endline("exp = " ++ DHExp.show(d));
    print_endline("pat = " ++ DHExp.show(f.pat));
    let rec matches_exp = (d: DHExp.t, f: DHExp.t): bool => {
      print_endline("======== Filter.matches_exp ========");
      print_endline("d = " ++ DHExp.show(d));
      print_endline("f = " ++ DHExp.show(f));
      switch (d, f) {
      | (Constructor("$Expr"), _) => failwith("$Expr in matched expression")
      | (Constructor("$Value"), _) =>
        failwith("$Value in matched expression")

      | (_, Constructor("$Expr")) => true
      | (_, EmptyHole(_)) => true
      | (EmptyHole(_), _) => false

      | (Closure(_, _, d), _) => matches_exp(d, f)
      | (_, Closure(_, _, f)) => matches_exp(d, f)

      | (Cast(d, _, _), _) => matches_exp(d, f)
      | (_, Cast(f, _, _)) => matches_exp(d, f)
      | (FailedCast(d, _, _), _) => matches_exp(d, f)
      | (_, FailedCast(f, _, _)) => matches_exp(d, f)

      | (BoundVar(dx), BoundVar(fx))
      | (BoundVar(dx), FreeVar(_, _, fx))
      | (FreeVar(_, _, dx), BoundVar(fx))
      | (FreeVar(_, _, dx), FreeVar(_, _, fx)) => dx == fx
      | (BoundVar(dx), _) =>
        print_endline(
          "FilterEnvironment.matches: Var(" ++ dx ++ ") =? " ++ DHExp.show(f),
        );
        switch (ClosureEnvironment.lookup(env, dx)) {
        | Some(d) => matches_exp(d, f)
        | None => false
        };
      | (_, BoundVar(fx)) =>
        print_endline(
          "FilterEnvironment.matches: "
          ++ DHExp.show(d)
          ++ " =? Var("
          ++ fx
          ++ ")",
        );
        switch (ClosureEnvironment.lookup(env, fx)) {
        | Some(f) => matches_exp(d, f)
        | None => false
        };

      | (Filter(_, d1), _) => matches_exp(d1, f)

      | (
          BoolLit(_) | IntLit(_) | FloatLit(_) | StringLit(_),
          Constructor("$Value"),
        ) =>
        true

      | (BoolLit(dv), BoolLit(fv)) => dv == fv
      | (BoolLit(_), _) => false

      | (IntLit(dv), IntLit(fv)) => dv == fv
      | (IntLit(_), _) => false

      | (FloatLit(dv), FloatLit(fv)) => dv == fv
      | (FloatLit(_), _) => false

      | (StringLit(dv), StringLit(fv)) => dv == fv
      | (StringLit(_), _) => false

      | (Constructor(_), Ap(Constructor("~MVal"), Tuple([]))) => true
      | (Constructor(dt), Constructor(ft)) => dt == ft
      | (Constructor(_), _) => false

      | (Fun(dp1, dty1, d1, dname1), Fun(fp1, fty1, f1, fname1)) =>
        matches_pat(dp1, fp1)
        && dty1 == fty1
        && matches_exp(d1, f1)
        && dname1 == fname1
      | (Fun(_), _) => false

      | (FreeVar(_), _) => false

      | (Let(dp, d1, d2), Let(fp, f1, f2)) =>
        matches_pat(dp, fp) && matches_exp(d1, f1) && matches_exp(d2, f2)
      | (Let(_), _)
      | (_, Let(_)) => false

      | (Ap(d1, d2), Ap(f1, f2)) =>
        matches_exp(d1, f1) && matches_exp(d2, f2)
      | (Ap(_), _) => false

      | (Sequence(d1, d2), Sequence(f1, f2)) =>
        matches_exp(d1, f1) && matches_exp(d2, f2)
      | (Sequence(_), _) => false

      | (TestLit(d1), TestLit(f1)) => d1 == f1
      | (TestLit(_), _) => false

      | (Cons(d1, d2), Cons(f1, f2)) =>
        matches_exp(d1, f1) && matches_exp(d2, f2)
      | (Cons(_), _) => false

      | (ListLit(_, _, dt, dv), ListLit(_, _, ft, fv)) =>
        dt == ft
        && List.fold_left2(
             (acc, d, f) => acc && matches_exp(d, f),
             true,
             dv,
             fv,
           )
      | (ListLit(_), _) => false

      | (Tuple(dv), Tuple(fv)) =>
        List.fold_left2(
          (acc, d, f) => acc && matches_exp(d, f),
          true,
          dv,
          fv,
        )
      | (Tuple(_), _) => false

      | (BinBoolOp(d_op_bin, d1, d2), BinBoolOp(f_op_bin, f1, f2)) =>
        d_op_bin == f_op_bin && matches_exp(d1, f1) && matches_exp(d2, f2)
      | (BinBoolOp(_), _) => false

      | (BinIntOp(d_op_bin, d1, d2), BinIntOp(f_op_bin, f1, f2)) =>
        d_op_bin == f_op_bin && matches_exp(d1, f1) && matches_exp(d2, f2)
      | (BinIntOp(_), _) => false

      | (BinFloatOp(d_op_bin, d1, d2), BinFloatOp(f_op_bin, f1, f2)) =>
        d_op_bin == f_op_bin && matches_exp(d1, f1) && matches_exp(d2, f2)
      | (BinFloatOp(_), _) => false

      | (BinStringOp(d_op_bin, d1, d2), BinStringOp(f_op_bin, f1, f2)) =>
        d_op_bin == f_op_bin && matches_exp(d1, f1) && matches_exp(d2, f2)
      | (BinStringOp(_), _) => false

      | (ListConcat(_), _) => false

      | (
          ConsistentCase(Case(dscrut, drule, _)),
          ConsistentCase(Case(fscrut, frule, _)),
        )
      | (
          InconsistentBranches(_, _, Case(dscrut, drule, _)),
          InconsistentBranches(_, _, Case(fscrut, frule, _)),
        ) =>
        matches_exp(dscrut, fscrut)
        && (
          switch (
            List.fold_left2(
              (res, drule, frule) => res && matches_rul(drule, frule),
              true,
              drule,
              frule,
            )
          ) {
          | exception (Invalid_argument(_)) => false
          | res => res
          }
        )
      | (ConsistentCase(_), _)
      | (InconsistentBranches(_), _) => false

      | (NonEmptyHole(_), _) => false
      | (ExpandingKeyword(_), _) => false
      | (InvalidText(_), _) => false
      | (InvalidOperation(_), _) => false

      | (FixF(dv, dt, dc), FixF(fv, ft, fc)) =>
        dv == fv && dt == ft && matches_exp(dc, fc)
      | (FixF(_), _) => false

      | (ApBuiltin(dname, dargs), ApBuiltin(fname, fargs)) =>
        dname == fname
        && List.fold_left2(
             (acc, d, f) => {acc && matches_exp(d, f)},
             true,
             dargs,
             fargs,
           )
      | (ApBuiltin(_), _) => false

      | (Prj(dv, di), Prj(fv, fi)) => matches_exp(dv, fv) && di == fi
      | (Prj(_), _) => false
      };
    }
    and matches_pat = (d: DHPat.t, f: DHPat.t): bool => {
      switch (d, f) {
      | (_, EmptyHole(_)) => true
      | (Wild, Wild) => true
      | (IntLit(dv), IntLit(fv)) => dv == fv
      | (FloatLit(dv), FloatLit(fv)) => dv == fv
      | (BoolLit(dv), BoolLit(fv)) => dv == fv
      | (StringLit(dv), StringLit(fv)) => dv == fv
      | (ListLit(dty1, dl), ListLit(fty1, fl)) =>
        switch (
          List.fold_left2(
            (res, d, f) => res && matches_pat(d, f),
            true,
            dl,
            fl,
          )
        ) {
        | exception (Invalid_argument(_)) => false
        | res => matches_typ(dty1, fty1) && res
        }
      | (Constructor(dt), Constructor(ft)) => dt == ft
      | (Var(dx), Var(fx)) => dx == fx
      | (Tuple(dl), Tuple(fl)) =>
        switch (
          List.fold_left2(
            (res, d, f) => res && matches_pat(d, f),
            true,
            dl,
            fl,
          )
        ) {
        | exception (Invalid_argument(_)) => false
        | res => res
        }
      | (Ap(d1, d2), Ap(f1, f2)) =>
        matches_pat(d1, f1) && matches_pat(d2, f2)
      | (_, _) => false
      };
    }
    and matches_typ = (d: Typ.t, f: Typ.t) => {
      switch (d, f) {
      | (_, _) => false
      };
    }
    and matches_rul = (_d: DHExp.rule, _f: DHExp.rule) => {
      false;
    };

    if (matches_exp(d, f.pat)) {
      Some(f.act);
    } else {
      None;
    };
  };

  let fast_equal = (f1: t, f2: t): bool => {
    DHExp.fast_equal(f1.pat, f2.pat) && f1.act == f2.act;
  };

  let strip_casts = (flt: t) => {...flt, pat: flt.pat |> DHExp.strip_casts};
}

and FilterEnvironment: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(Filter.t);

  let empty: t;

  let fast_equal: (t, t) => bool;

  let strip_casts: t => t;

  let extends: (Filter.t, t) => t;

  let matches: (ClosureEnvironment.t, DHExp.t, t) => option(FilterAction.t);
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(Filter.t);

  let empty = [];

  let fast_equal = (f1: t, f2: t): bool => {
    let go = () => {
      List.fold_left2(
        (r, f1, f2) => r && Filter.fast_equal(f1, f2),
        true,
        f1,
        f2,
      );
    };
    switch (go()) {
    | exception (Invalid_argument(_)) => false
    | result => result
    };
  };

  let strip_casts = (env: t) => {
    env |> List.map(Filter.strip_casts);
  };

  let extends = (f: Filter.t, env: t) => {
    [f, ...env];
  };

  let matches =
      (var: ClosureEnvironment.t, d: DHExp.t, env: t): option(FilterAction.t) => {
    env |> List.find_map(Filter.matches(var, d));
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

and ClosureEnvironment: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t;

  let wrap: (EnvironmentId.t, Environment.t) => t;

  let id_of: t => EnvironmentId.t;
  let map_of: t => Environment.t;

  let to_list: t => list((Var.t, DHExp.t));

  let of_environment:
    (Environment.t, EnvironmentIdGen.t) => (t, EnvironmentIdGen.t);

  let id_equal: (t, t) => bool;

  let empty: EnvironmentIdGen.t => (t, EnvironmentIdGen.t);
  let is_empty: t => bool;
  let length: t => int;

  let lookup: (t, Var.t) => option(DHExp.t);
  let contains: (t, Var.t) => bool;
  let update:
    (Environment.t => Environment.t, t, EnvironmentIdGen.t) =>
    (t, EnvironmentIdGen.t);
  let update_keep_id: (Environment.t => Environment.t, t) => t;
  let extend:
    (t, (Var.t, DHExp.t), EnvironmentIdGen.t) => (t, EnvironmentIdGen.t);
  let extend_keep_id: (t, (Var.t, DHExp.t)) => t;
  let union: (t, t, EnvironmentIdGen.t) => (t, EnvironmentIdGen.t);
  let union_keep_id: (t, t) => t;
  let map:
    (((Var.t, DHExp.t)) => DHExp.t, t, EnvironmentIdGen.t) =>
    (t, EnvironmentIdGen.t);
  let map_keep_id: (((Var.t, DHExp.t)) => DHExp.t, t) => t;
  let filter:
    (((Var.t, DHExp.t)) => bool, t, EnvironmentIdGen.t) =>
    (t, EnvironmentIdGen.t);
  let filter_keep_id: (((Var.t, DHExp.t)) => bool, t) => t;
  let fold: (((Var.t, DHExp.t), 'b) => 'b, 'b, t) => 'b;

  let placeholder: t;
} = {
  module Inner: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t;

    let wrap: (EnvironmentId.t, Environment.t) => t;

    let id_of: t => EnvironmentId.t;
    let map_of: t => Environment.t;
  } = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = (EnvironmentId.t, Environment.t);

    let wrap = (ei, map): t => (ei, map);

    let id_of = ((ei, _)) => ei;
    let map_of = ((_, map)) => map;
  };
  include Inner;

  let to_list = env => env |> map_of |> Environment.to_listo;

  let of_environment = (map, eig) => {
    let (ei, eig) = EnvironmentIdGen.next(eig);
    (wrap(ei, map), eig);
  };

  /* Equals only needs to check environment id's (faster than structural equality
   * checking.) */
  let id_equal = (env1, env2) => id_of(env1) == id_of(env2);

  let empty = Environment.empty |> of_environment;

  let is_empty = env => env |> map_of |> Environment.is_empty;

  let length = env => Environment.length(map_of(env));

  let lookup = (env, x) =>
    env |> map_of |> (map => Environment.lookup(map, x));

  let contains = (env, x) =>
    env |> map_of |> (map => Environment.contains(map, x));

  let update = (f, env) => env |> map_of |> f |> of_environment;

  let update_keep_id = (f, env) => env |> map_of |> f |> wrap(env |> id_of);

  let extend = (env, xr) =>
    env |> update(map => Environment.extend(map, xr));

  let extend_keep_id = (env, xr) =>
    env |> update_keep_id(map => Environment.extend(map, xr));

  let union = (env1, env2) =>
    env2 |> update(map2 => Environment.union(env1 |> map_of, map2));

  let union_keep_id = (env1, env2) =>
    env2 |> update_keep_id(map2 => Environment.union(env1 |> map_of, map2));

  let map = (f, env) => env |> update(Environment.mapo(f));

  let map_keep_id = (f, env) => env |> update_keep_id(Environment.mapo(f));

  let filter = (f, env) => env |> update(Environment.filtero(f));

  let filter_keep_id = (f, env) =>
    env |> update_keep_id(Environment.filtero(f));

  let fold = (f, init, env) => env |> map_of |> Environment.foldo(f, init);

  let placeholder = wrap(EnvironmentId.invalid, Environment.empty);
};
