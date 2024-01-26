let rec matches_exp =
        (env: ClosureEnvironment.t, d: DHExp.t, f: DHExp.t): bool => {
  switch (d, f) {
  | (Constructor("$e"), _) => failwith("$e in matched expression")
  | (Constructor("$v"), _) => failwith("$v in matched expression")

  // HACK[Matt]: ignore fixpoints in comparison, to allow pausing on fixpoint steps
  | (FixF(dp, _, dc), f) =>
    matches_exp(
      env,
      Closure(
        Transition.evaluate_extend_env(Environment.singleton((dp, dc)), env),
        dc,
      ),
      f,
    )
  | (d, FixF(fp, _, fc)) =>
    matches_exp(
      env,
      d,
      Closure(
        Transition.evaluate_extend_env(Environment.singleton((fp, fc)), env),
        fc,
      ),
    )

  | (_, Constructor("$v")) =>
    switch (ValueChecker.check_value(env, d)) {
    | Indet
    | Value => true
    | Expr => false
    }

  | (_, EmptyHole(_))
  | (_, Constructor("$e")) => true

  | (_, Closure(env, f)) => matches_exp(env, d, f)
  | (_, Cast(f, _, _)) => matches_exp(env, d, f)
  | (_, FailedCast(f, _, _)) => matches_exp(env, d, f)

  | (Closure(env, d), _) => matches_exp(env, d, f)
  | (Cast(d, _, _), _) => matches_exp(env, d, f)
  | (FailedCast(d, _, _), _) => matches_exp(env, d, f)

  | (Var(dx), Var(fx)) => dx == fx
  | (Var(dx), _) =>
    let d =
      ClosureEnvironment.lookup(env, dx)
      |> Util.OptUtil.get(() => {
           print_endline("FreeInvalidVar:" ++ dx);
           raise(EvaluatorError.Exception(FreeInvalidVar(dx)));
         });
    matches_exp(env, d, f);
  | (_, Var(fx)) =>
    switch (ClosureEnvironment.lookup(env, fx)) {
    | Some(f) => matches_exp(env, d, f)
    | None => false
    }

  | (EmptyHole(_), _) => false

  | (Filter(df, dd), Filter(ff, fd)) =>
    DH.DHFilter.fast_equal(df, ff) && matches_exp(env, dd, fd)
  | (Filter(_), _) => false

  | (Bool(dv), Bool(fv)) => dv == fv
  | (Bool(_), _) => false

  | (Int(dv), Int(fv)) => dv == fv
  | (Int(_), _) => false

  | (Float(dv), Float(fv)) => dv == fv
  | (Float(_), _) => false

  | (String(dv), String(fv)) => dv == fv
  | (String(_), _) => false

  | (Constructor(_), Ap(Constructor("~MVal"), Tuple([]))) => true
  | (Constructor(dt), Constructor(ft)) => dt == ft
  | (Constructor(_), _) => false

  | (BuiltinFun(dn), BuiltinFun(fn)) => dn == fn
  | (BuiltinFun(_), _) => false

  | (Fun(dp1, dty1, d1, dname1), Fun(fp1, fty1, f1, fname1)) =>
    matches_pat(dp1, fp1)
    && dty1 == fty1
    && matches_exp(env, d1, f1)
    && dname1 == fname1
  | (Fun(_), _) => false

  | (FreeVar(du, di, dx), FreeVar(fu, fi, fx)) =>
    du == fu && di == fi && dx == fx
  | (FreeVar(_), _) => false

  | (Let(dp, d1, d2), Let(fp, f1, f2)) =>
    matches_pat(dp, fp)
    && matches_exp(env, d1, f1)
    && matches_exp(env, d2, f2)
  | (Let(_), _) => false

  | (Ap(d1, d2), Ap(f1, f2)) =>
    matches_exp(env, d1, f1) && matches_exp(env, d2, f2)
  | (Ap(_), _) => false

  | (If(dc, d1, d2, d3), If(fc, f1, f2, f3)) =>
    dc == fc
    && matches_exp(env, d1, f1)
    && matches_exp(env, d2, f2)
    && matches_exp(env, d3, f3)
  | (If(_), _) => false

  | (Seq(d1, d2), Seq(f1, f2)) =>
    matches_exp(env, d1, f1) && matches_exp(env, d2, f2)
  | (Seq(_), _) => false

  | (Test(id1, d2), Test(id2, f2)) =>
    id1 == id2 && matches_exp(env, d2, f2)
  | (Test(_), _) => false

  | (Cons(d1, d2), Cons(f1, f2)) =>
    matches_exp(env, d1, f1) && matches_exp(env, d2, f2)
  | (Cons(_), _) => false

  | (ListLit(_, _, dt, dv), ListLit(_, _, ft, fv)) =>
    dt == ft
    && List.fold_left2(
         (acc, d, f) => acc && matches_exp(env, d, f),
         true,
         dv,
         fv,
       )
  | (ListLit(_), _) => false

  | (Tuple(dv), Tuple(fv)) =>
    List.fold_left2(
      (acc, d, f) => acc && matches_exp(env, d, f),
      true,
      dv,
      fv,
    )
  | (Tuple(_), _) => false

  | (BinOp(d_op, d1, d2), BinOp(f_op, f1, f2)) =>
    d_op == f_op && matches_exp(env, d1, f1) && matches_exp(env, d2, f2)
  | (BinOp(_), _) => false

  | (ListConcat(d1, d2), ListConcat(f1, f2)) =>
    matches_exp(env, d1, f1) && matches_exp(env, d2, f2)
  | (ListConcat(_), _) => false

  | (
      ConsistentCase(Case(dscrut, drule, _)),
      ConsistentCase(Case(fscrut, frule, _)),
    )
  | (
      InconsistentBranches(_, _, Case(dscrut, drule, _)),
      InconsistentBranches(_, _, Case(fscrut, frule, _)),
    ) =>
    matches_exp(env, dscrut, fscrut)
    && (
      switch (
        List.fold_left2(
          (res, drule, frule) => res && matches_rul(env, drule, frule),
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
    dname == fname && matches_exp(env, darg, farg)
  | (ApBuiltin(_), _) => false

  | (Prj(dv, di), Prj(fv, fi)) => matches_exp(env, dv, fv) && di == fi
  | (Prj(_), _) => false
  };
}
and matches_pat = (d: DHPat.t, f: DHPat.t): bool => {
  switch (d, f) {
  | (_, EmptyHole(_)) => true
  | (Wild, Wild) => true
  | (Wild, _) => false
  | (Int(dv), Int(fv)) => dv == fv
  | (Int(_), _) => false
  | (Float(dv), Float(fv)) => dv == fv
  | (Float(_), _) => false
  | (Bool(dv), Bool(fv)) => dv == fv
  | (Bool(_), _) => false
  | (String(dv), String(fv)) => dv == fv
  | (String(_), _) => false
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
  | (Var(dx), Var(fx)) => dx == fx
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
and matches_rul = (env, d: DHExp.rule, f: DHExp.rule) => {
  switch (d, f) {
  | (Rule(dp, d), Rule(fp, f)) =>
    matches_pat(dp, fp) && matches_exp(env, d, f)
  };
};

let matches =
    (~env: ClosureEnvironment.t, ~exp: DHExp.t, ~flt: Filter.t)
    : option(FilterAction.t) =>
  if (matches_exp(env, exp, flt.pat)) {
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
