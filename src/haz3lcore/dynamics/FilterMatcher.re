let rec matches_exp =
        (
          info_map: Statics.Map.t,
          env: ClosureEnvironment.t,
          d: DHExp.t,
          f: DHExp.t,
        )
        : bool => {
  let matches_exp = matches_exp(info_map);
  switch (DHExp.term_of(d), DHExp.term_of(f)) {
  | (Constructor("$e"), _) => failwith("$e in matched expression")
  | (Constructor("$v"), _) => failwith("$v in matched expression")
  | (_, Constructor("$v")) =>
    switch (ValueChecker.check_value(info_map, env, d)) {
    | Indet
    | Value => true
    | Expr => false
    }

  | (_, EmptyHole)
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

  | (EmptyHole, _) => false

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

  | (Constructor(_), Ap(_, d1, d2)) =>
    switch (DHExp.term_of(d1), DHExp.term_of(d2)) {
    | (Constructor("~MVal"), Tuple([])) => true
    | _ => false
    }
  | (Constructor(dt), Constructor(ft)) => dt == ft
  | (Constructor(_), _) => false

  | (BuiltinFun(dn), BuiltinFun(fn)) => dn == fn
  | (BuiltinFun(_), _) => false

  // Not sure if we should be checking functions for closures here
  | (Fun(dp1, dty1, d1, _, dname1), Fun(fp1, fty1, f1, _, fname1)) =>
    matches_pat(dp1, fp1)
    && dty1 == fty1
    && matches_exp(env, d1, f1)
    && dname1 == fname1
  | (Fun(_), _) => false

  | (FixF(dp, dt, d1), FixF(fp, ft, f1)) =>
    matches_pat(dp, fp) && dt == ft && matches_exp(env, d1, f1)
  | (FixF(_), _) => false

  | (Let(dp, d1, d2), Let(fp, f1, f2)) =>
    matches_pat(dp, fp)
    && matches_exp(env, d1, f1)
    && matches_exp(env, d2, f2)
  | (Let(_), _) => false

  // TODO: do we want f(x) to match x |> f ???
  | (Ap(_, d1, d2), Ap(_, f1, f2)) =>
    matches_exp(env, d1, f1) && matches_exp(env, d2, f2)
  | (Ap(_), _) => false

  | (If(d1, d2, d3), If(f1, f2, f3)) =>
    matches_exp(env, d1, f1)
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

  | (ListLit(dt, dv), ListLit(ft, fv)) =>
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

  | (UnOp(d_op, d1), UnOp(f_op, f1)) =>
    d_op == f_op && matches_exp(env, d1, f1)
  | (UnOp(_), _) => false

  | (BinOp(d_op, d1, d2), BinOp(f_op, f1, f2)) =>
    d_op == f_op && matches_exp(env, d1, f1) && matches_exp(env, d2, f2)
  | (BinOp(_), _) => false

  | (ListConcat(d1, d2), ListConcat(f1, f2)) =>
    matches_exp(env, d1, f1) && matches_exp(env, d2, f2)
  | (ListConcat(_), _) => false

  | (Match(dscrut, drule), Match(fscrut, frule)) =>
    matches_exp(env, dscrut, fscrut)
    && (
      switch (
        List.for_all2(
          ((dk, dv), (fk, fv)) =>
            matches_pat(dk, fk) && matches_exp(env, dv, fv),
          drule,
          frule,
        )
      ) {
      | exception (Invalid_argument(_)) => false
      | res => res
      }
    )
  | (Match(_), _) => false
  // TODO: should these not default to false?
  | (MultiHole(_), _) => false
  | (StaticErrorHole(_), _) => false
  | (Invalid(_), _) => false
  | (DynamicErrorHole(_), _) => false

  | (ApBuiltin(dname, darg), ApBuiltin(fname, farg)) =>
    dname == fname && matches_exp(env, darg, farg)
  | (ApBuiltin(_), _) => false

  | (TyAlias(dtp, dut, dd), TyAlias(ftp, fut, fd)) =>
    dtp == ftp && dut == fut && matches_exp(env, dd, fd)
  | (TyAlias(_), _) => false
  };
}
and matches_pat = (d: DHPat.t, f: DHPat.t): bool => {
  switch (d |> DHPat.term_of, f |> DHPat.term_of) {
  | (_, EmptyHole) => true
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
  | (ListLit(dl), ListLit(fl)) =>
    switch (
      List.fold_left2((res, d, f) => res && matches_pat(d, f), true, dl, fl)
    ) {
    | exception (Invalid_argument(_)) => false
    | res => res
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
  | (EmptyHole, _) => false
  | (NonEmptyHole(_), _) => false
  | (ExpandingKeyword(_), _) => false
  | (Invalid(_), _) => false
  };
}
and matches_typ = (d: Typ.t, f: Typ.t) => {
  Typ.eq(d, f);
}
and matches_rul = (info_map, env, (dp, d), (fp, f)) => {
  matches_pat(dp, fp) && matches_exp(info_map, env, d, f);
};

let matches =
    (info_map, ~env: ClosureEnvironment.t, ~exp: DHExp.t, ~flt: Filter.t)
    : option(FilterAction.t) =>
  if (matches_exp(info_map, env, exp, flt.pat)) {
    Some(flt.act);
  } else {
    None;
  };

let matches =
    (
      ~env: ClosureEnvironment.t,
      ~exp: DHExp.t,
      ~exp_info_map: Statics.Map.t,
      ~act: FilterAction.t,
      flt_env,
    )
    : (FilterAction.t, int) => {
  let len = List.length(flt_env);
  let rec matches' = (~env, ~exp, ~act, flt_env, idx) => {
    switch (flt_env) {
    | [] => (act, idx)
    | [hd, ...tl] =>
      switch (matches(exp_info_map, ~env, ~exp, ~flt=hd)) {
      | Some(act) => (act, idx)
      | None => matches'(~env, ~exp, ~act, tl, idx + 1)
      }
    };
  };
  let (act, idx) = matches'(~env, ~exp, ~act, flt_env, 0);
  (act, len - idx);
};
