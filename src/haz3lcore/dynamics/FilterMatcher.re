let rec find_keys = (dp: DHPat.t): list(string) => {
  switch (dp) {
  | DHPat.EmptyHole(_, _)
  | DHPat.NonEmptyHole(_, _, _, _)
  | DHPat.Wild
  | DHPat.ExpandingKeyword(_, _, _)
  | DHPat.InvalidText(_, _, _)
  | DHPat.BadConstructor(_, _, _) => []
  | DHPat.Var(x) => [x]
  | DHPat.IntLit(_)
  | DHPat.FloatLit(_)
  | DHPat.BoolLit(_)
  | DHPat.StringLit(_)
  | DHPat.Constructor(_) => []
  | DHPat.ListLit(_, dps)
  | DHPat.Tuple(dps) => List.concat_map(find_keys, dps)
  | DHPat.Cons(dp1, dp2)
  | DHPat.Ap(dp1, dp2) => find_keys(dp1) @ find_keys(dp2)
  };
};

let rec matches_exp =
        (
          ~denv: ClosureEnvironment.t,
          d: DHExp.t,
          ~fenv: ClosureEnvironment.t,
          f: DHExp.t,
        )
        : bool => {
  let matches_exp = (~denv=denv, ~fenv=fenv, d, f) =>
    matches_exp(~denv, d, ~fenv, f);
  if (d == f) {
    true;
  } else {
    switch (d, f) {
    | (Constructor("$e"), _) => failwith("$e in matched expression")
    | (Constructor("$v"), _) => failwith("$v in matched expression")

    // HACK[Matt]: ignore fixpoints in comparison, to allow pausing on fixpoint steps
    | (FixF(dp, dt, dc), FixF(fp, ft, fc)) =>
      dp == fp
      && dt == ft
      && matches_exp(
           ~denv=denv |> ClosureEnvironment.without_keys([dp]),
           dc,
           ~fenv=fenv |> ClosureEnvironment.without_keys([fp]),
           fc,
         )
    | (FixF(dp, _, dc), f) =>
      matches_exp(~denv=denv |> ClosureEnvironment.without_keys([dp]), dc, f)
    | (d, FixF(fp, _, fc)) =>
      matches_exp(d, ~fenv=fenv |> ClosureEnvironment.without_keys([fp]), fc)

    | (_, Constructor("$v")) =>
      switch (ValueChecker.check_value(denv, d)) {
      | Indet
      | Value => true
      | Expr => false
      }

    | (_, EmptyHole(_))
    | (_, Constructor("$e")) => true

    | (Cast(d, _, _), Cast(f, _, _)) => matches_exp(d, f)
    | (Closure(denv, d), Closure(fenv, f)) =>
      matches_exp(~denv, d, ~fenv, f)

    | (_, Closure(fenv, f)) => matches_exp(~fenv, d, f)
    | (_, Cast(f, _, _)) => matches_exp(d, f)
    | (_, FailedCast(f, _, _)) => matches_exp(d, f)

    | (Closure(denv, d), _) => matches_exp(~denv, d, f)
    | (Cast(d, _, _), _) => matches_exp(d, f)
    | (FailedCast(d, _, _), _) => matches_exp(d, f)
    | (Filter(Residue(_), d), _) => matches_exp(d, f)

    | (BoundVar(dx), BoundVar(fx))
        when String.starts_with(dx, ~prefix="__mutual__") =>
      String.starts_with(fx, ~prefix="__mutual__") && dx == fx
    | (BoundVar(dx), BoundVar(fx)) =>
      switch (
        ClosureEnvironment.lookup(denv, dx),
        ClosureEnvironment.lookup(fenv, fx),
      ) {
      | (
          Some(Fun(_, _, Closure(denv, _), Some(dname)) as d),
          Some(Fun(_, _, Closure(fenv, _), Some(fname)) as f),
        )
          when
            ClosureEnvironment.lookup(denv, dname) == Some(d)
            && ClosureEnvironment.lookup(fenv, fname) == Some(f) =>
        matches_exp(
          ~denv=ClosureEnvironment.without_keys([dname], denv),
          d,
          ~fenv=ClosureEnvironment.without_keys([fname], fenv),
          f,
        )
      | (
          Some(Fun(_, _, Closure(denv, _), Some(dname)) as d),
          Some(Fun(_, _, _, Some(fname)) as f),
        )
          when
            ClosureEnvironment.lookup(denv, dname) == Some(d)
            && ClosureEnvironment.lookup(fenv, fname) == Some(f) =>
        matches_exp(
          ~denv=ClosureEnvironment.without_keys([dname], denv),
          d,
          ~fenv=ClosureEnvironment.without_keys([fname], fenv),
          f,
        )
      | (
          Some(Fun(_, _, _, Some(dname)) as d),
          Some(Fun(_, _, _, Some(fname)) as f),
        )
          when
            ClosureEnvironment.lookup(denv, dname) == Some(d)
            && ClosureEnvironment.lookup(fenv, fname) == Some(f) =>
        matches_exp(
          ~denv=ClosureEnvironment.without_keys([dname], denv),
          d,
          ~fenv=ClosureEnvironment.without_keys([fname], fenv),
          f,
        )
      | (
          Some(Fun(_, _, _, Some(dname)) as d),
          Some(Fun(_, _, _, Some(fname)) as f),
        )
          when
            ClosureEnvironment.lookup(denv, dname) == Some(d)
            && ClosureEnvironment.lookup(fenv, fname) == Some(f) =>
        matches_exp(
          ~denv=ClosureEnvironment.without_keys([dname], denv),
          d,
          ~fenv=ClosureEnvironment.without_keys([fname], denv),
          f,
        )
      | (Some(d), Some(f)) => matches_exp(d, f)
      | (Some(_), None) => false
      | (None, Some(_)) => false
      | (None, None) => true
      }
    | (BoundVar(dx), _) =>
      switch (ClosureEnvironment.lookup(denv, dx)) {
      | Some(d) => matches_exp(d, f)
      | None => false
      }
    | (_, BoundVar(fx)) =>
      switch (ClosureEnvironment.lookup(fenv, fx)) {
      | Some(f) => matches_exp(d, f)
      | None => false
      }

    | (EmptyHole(_), _) => false

    | (Filter(df, dd), Filter(ff, fd)) =>
      DH.DHFilter.fast_equal(df, ff) && matches_exp(dd, fd)
    | (Filter(_), _) => false

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

    | (BuiltinFun(dn), BuiltinFun(fn)) => dn == fn
    | (BuiltinFun(_), _) => false

    | (
        Fun(dp1, _, Closure(denv, d1), _),
        Fun(fp1, _, Closure(fenv, f1), _),
      ) =>
      matches_fun(~denv, dp1, d1, ~fenv, fp1, f1)
    | (Fun(dp1, _, Closure(denv, d1), _), Fun(fp1, _, f1, _)) =>
      matches_fun(~denv, dp1, d1, ~fenv, fp1, f1)
    | (Fun(dp1, _, d1, _), Fun(fp1, _, Closure(fenv, f1), _)) =>
      matches_fun(~denv, dp1, d1, ~fenv, fp1, f1)
    | (Fun(dp1, _, d1, _), Fun(fp1, _, f1, _)) =>
      matches_fun(~denv, dp1, d1, ~fenv, fp1, f1)
    | (Fun(_), _) => false

    | (FreeVar(du, di, dx), FreeVar(fu, fi, fx)) =>
      du == fu && di == fi && dx == fx
    | (FreeVar(_), _) => false

    | (Let(dp, d1, d2), Let(fp, f1, f2)) =>
      matches_pat(dp, fp) && matches_exp(d1, f1) && matches_exp(d2, f2)
    | (Let(_), _) => false

    | (Ap(d1, d2), Ap(f1, f2)) =>
      matches_exp(d1, f1) && matches_exp(d2, f2)
    | (Ap(_), _) => false

    | (IfThenElse(dc, d1, d2, d3), IfThenElse(fc, f1, f2, f3)) =>
      dc == fc
      && matches_exp(d1, f1)
      && matches_exp(d2, f2)
      && matches_exp(d3, f3)
    | (IfThenElse(_), _) => false

    | (Sequence(d1, d2), Sequence(f1, f2)) =>
      matches_exp(d1, f1) && matches_exp(d2, f2)
    | (Sequence(_), _) => false

    | (Test(id1, d2), Test(id2, f2)) => id1 == id2 && matches_exp(d2, f2)
    | (Test(_), _) => false

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
      List.fold_left2((acc, d, f) => acc && matches_exp(d, f), true, dv, fv)
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
            (res, drule, frule) =>
              res && matches_rul(~denv, drule, ~fenv, frule),
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

    | (ApBuiltin(dname, darg), ApBuiltin(fname, farg)) =>
      dname == fname && matches_exp(darg, farg)
    | (ApBuiltin(_), _) => false

    | (Prj(dv, di), Prj(fv, fi)) => matches_exp(dv, fv) && di == fi
    | (Prj(_), _) => false
    };
  };
}
and matches_fun =
    (
      ~denv: ClosureEnvironment.t,
      dp: DHPat.t,
      d: DHExp.t,
      ~fenv: ClosureEnvironment.t,
      fp: DHPat.t,
      f: DHExp.t,
    ) => {
  matches_pat(dp, fp)
  && matches_exp(
       ~denv=ClosureEnvironment.without_keys(find_keys(dp), denv),
       d,
       ~fenv=ClosureEnvironment.without_keys(find_keys(fp), fenv),
       f,
     );
}
and matches_pat = (d: DHPat.t, f: DHPat.t): bool => {
  switch (d, f) {
  | (_, EmptyHole(_)) => true
  | (Wild, Wild) => true
  | (Wild, _) => false
  | (IntLit(dv), IntLit(fv)) => dv == fv
  | (IntLit(_), _) => false
  | (FloatLit(dv), FloatLit(fv)) => dv == fv
  | (FloatLit(_), _) => false
  | (BoolLit(dv), BoolLit(fv)) => dv == fv
  | (BoolLit(_), _) => false
  | (StringLit(dv), StringLit(fv)) => dv == fv
  | (StringLit(_), _) => false
  | (ListLit(dty1, dl), ListLit(fty1, fl)) =>
    switch (
      List.fold_left2((res, d, f) => res && matches_pat(d, f), true, dl, fl)
    ) {
    | exception (Invalid_argument(_)) => false
    | res => matches_typ(dty1, fty1) && res
    }
  | (ListLit(_), _) => false
  | (Constructor(dt), Constructor(ft)) => dt == ft
  | (Constructor(_), _) => false
  | (Var(_), Var(_)) => true
  | (Var(_), _) => false
  | (Tuple(dl), Tuple(fl)) =>
    switch (
      List.fold_left2((res, d, f) => res && matches_pat(d, f), true, dl, fl)
    ) {
    | exception (Invalid_argument(_)) => false
    | res => res
    }
  | (Tuple(_), _) => false
  | (Ap(d1, d2), Ap(f1, f2)) => matches_pat(d1, f1) && matches_pat(d2, f2)
  | (Ap(_), _) => false
  | (BadConstructor(_, _, dt), BadConstructor(_, _, ft)) => dt == ft
  | (BadConstructor(_), _) => false
  | (Cons(d1, d2), Cons(f1, f2)) =>
    matches_pat(d1, f1) && matches_pat(d2, f2)
  | (Cons(_), _) => false
  | (EmptyHole(_), _) => false
  | (NonEmptyHole(_), _) => false
  | (ExpandingKeyword(_), _) => false
  | (InvalidText(_), _) => false
  };
}
and matches_typ = (d: Typ.t, f: Typ.t) => {
  Typ.eq(d, f);
}
and matches_rul = (~denv, d: DHExp.rule, ~fenv, f: DHExp.rule) => {
  switch (d, f) {
  | (Rule(dp, d), Rule(fp, f)) =>
    matches_pat(dp, fp) && matches_exp(~denv, d, ~fenv, f)
  };
};

let matches =
    (~env: ClosureEnvironment.t, ~exp: DHExp.t, ~flt: Filter.t)
    : option(FilterAction.t) =>
  if (matches_exp(~denv=env, exp, ~fenv=env, flt.pat)) {
    Some(flt.act);
  } else {
    None;
  };

let matches =
    (~env: ClosureEnvironment.t, ~exp: DHExp.t, ~act: FilterAction.t, flt_env)
    : (FilterAction.t, int) => {
  let len = List.length(flt_env);
  let rec matches' = (~env, ~exp, ~act, flt_env, idx) => {
    switch (flt_env) {
    | [] => (act, idx)
    | [hd, ...tl] =>
      switch (matches(~env, ~exp, ~flt=hd)) {
      | Some(act) => (act, idx)
      | None => matches'(~env, ~exp, ~act, tl, idx + 1)
      }
    };
  };
  let (act, idx) = matches'(~env, ~exp, ~act, flt_env, 0);
  (act, len - idx);
};
