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

  | (BoundVar(dx), BoundVar(fx)) => dx == fx
  | (BoundVar(dx), _) =>
    let d =
      ClosureEnvironment.lookup(env, dx)
      |> Util.OptUtil.get(() => {
           print_endline("FreeInvalidVar:" ++ dx);
           raise(EvaluatorError.Exception(FreeInvalidVar(dx)));
         });
    matches_exp(env, d, f);
  | (_, BoundVar(fx)) =>
    switch (ClosureEnvironment.lookup(env, fx)) {
    | Some(f) => matches_exp(env, d, f)
    | None => false
    }

  | (EmptyHole(_), _) => false

  | (Filter(df, dd), Filter(ff, fd)) =>
    DH.DHFilter.fast_equal(df, ff) && matches_exp(env, dd, fd)
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

  | (TypFun(pat1, d1), TypFun(pat2, d2)) =>
    matches_utpat(pat1, pat2) && matches_exp(env, d1, d2)
  | (TypFun(_), _) => false

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

  | (TypAp(d1, t1), TypAp(d2, t2)) =>
    matches_exp(env, d1, d2) && matches_typ(t1, t2)
  | (TypAp(_), _) => false

  | (Ap(d1, d2), Ap(f1, f2)) =>
    matches_exp(env, d1, f1) && matches_exp(env, d2, f2)
  | (Ap(_), _) => false

  | (IfThenElse(dc, d1, d2, d3), IfThenElse(fc, f1, f2, f3)) =>
    dc == fc
    && matches_exp(env, d1, f1)
    && matches_exp(env, d2, f2)
    && matches_exp(env, d3, f3)
  | (IfThenElse(_), _) => false

  | (Sequence(d1, d2), Sequence(f1, f2)) =>
    matches_exp(env, d1, f1) && matches_exp(env, d2, f2)
  | (Sequence(_), _) => false

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

  | (BinBoolOp(d_op_bin, d1, d2), BinBoolOp(f_op_bin, f1, f2)) =>
    d_op_bin == f_op_bin
    && matches_exp(env, d1, f1)
    && matches_exp(env, d2, f2)

  | (BinBoolOp(_), _) => false

  | (BinIntOp(d_op_bin, d1, d2), BinIntOp(f_op_bin, f1, f2)) =>
    d_op_bin == f_op_bin
    && matches_exp(env, d1, f1)
    && matches_exp(env, d2, f2)
  | (BinIntOp(_), _) => false

  | (BinFloatOp(d_op_bin, d1, d2), BinFloatOp(f_op_bin, f1, f2)) =>
    d_op_bin == f_op_bin
    && matches_exp(env, d1, f1)
    && matches_exp(env, d2, f2)
  | (BinFloatOp(_), _) => false

  | (BinStringOp(d_op_bin, d1, d2), BinStringOp(f_op_bin, f1, f2)) =>
    d_op_bin == f_op_bin
    && matches_exp(env, d1, f1)
    && matches_exp(env, d2, f2)
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
}
and matches_utpat = (d: Term.UTPat.t, f: Term.UTPat.t): bool => {
  switch (d.term, f.term) {
  | (Invalid(_), _) => false
  | (_, Invalid(_)) => false
  | (_, EmptyHole) => true
  | (MultiHole(l1), MultiHole(l2)) => List.length(l1) == List.length(l2) /* TODO: probably should define a matches_any and recurse in here...? */
  | (Var(t1), Var(t2)) => t1 == t2
  | _ => false
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
