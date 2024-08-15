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
    switch (d |> DHExp.term_of, f |> DHExp.term_of) {
    | (Parens(d), _) => matches_exp(d, f)
    | (_, Parens(f)) => matches_exp(d, f)

    | (Constructor("$e", _), _) => failwith("$e in matched expression")
    | (Constructor("$v", _), _) => failwith("$v in matched expression")

    // HACK[Matt]: ignore fixpoints in comparison, to allow pausing on fixpoint steps
    | (FixF(dp, dc, _), FixF(fp, fc, _)) =>
      dp == fp
      && matches_exp(
           ~denv=
             denv |> ClosureEnvironment.without_keys(dp |> DHPat.bound_vars),
           dc,
           ~fenv=
             fenv |> ClosureEnvironment.without_keys(fp |> DHPat.bound_vars),
           fc,
         )
    | (FixF(dp, dc, _), _) =>
      matches_exp(
        ~denv=denv |> ClosureEnvironment.without_keys(DHPat.bound_vars(dp)),
        dc,
        f,
      )
    | (_, FixF(fp, fc, _)) =>
      matches_exp(
        d,
        ~fenv=fenv |> ClosureEnvironment.without_keys(DHPat.bound_vars(fp)),
        fc,
      )

    | (_, Constructor("$v", _)) =>
      switch (ValueChecker.check_value((), denv, d)) {
      | Indet
      | Value => true
      | Expr => false
      }

    | (_, EmptyHole)
    | (_, Constructor("$e", _)) => true

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

    | (Var(dx), Var(fx)) when String.starts_with(dx, ~prefix="__mutual__") =>
      String.starts_with(fx, ~prefix="__mutual__") && dx == fx
    | (Var(dx), Var(fx)) =>
      switch (
        ClosureEnvironment.lookup(denv, dx) |> Option.map(DHExp.term_of),
        ClosureEnvironment.lookup(fenv, fx) |> Option.map(DHExp.term_of),
      ) {
      | (
          Some(Fun(_, _, Some(denv), Some(dname)) as d),
          Some(Fun(_, _, Some(fenv), Some(fname)) as f),
        )
          when
            ClosureEnvironment.lookup(denv, dname)
            |> Option.map(DHExp.term_of) == Some(d)
            && ClosureEnvironment.lookup(fenv, fname)
            |> Option.map(DHExp.term_of) == Some(f) =>
        matches_exp(
          ~denv=ClosureEnvironment.without_keys([dname], denv),
          d |> Exp.fresh,
          ~fenv=ClosureEnvironment.without_keys([fname], fenv),
          f |> Exp.fresh,
        )
      | (
          Some(Fun(_, _, Some(denv), Some(dname)) as d),
          Some(Fun(_, _, _, Some(fname)) as f),
        )
          when
            ClosureEnvironment.lookup(denv, dname)
            |> Option.map(DHExp.term_of) == Some(d)
            && ClosureEnvironment.lookup(fenv, fname)
            |> Option.map(DHExp.term_of) == Some(f) =>
        matches_exp(
          ~denv=ClosureEnvironment.without_keys([dname], denv),
          d |> DHExp.fresh,
          ~fenv=ClosureEnvironment.without_keys([fname], fenv),
          f |> DHExp.fresh,
        )
      | (
          Some(Fun(_, _, _, Some(dname)) as d),
          Some(Fun(_, _, _, Some(fname)) as f),
        )
          when
            ClosureEnvironment.lookup(denv, dname)
            |> Option.map(DHExp.term_of) == Some(d)
            && ClosureEnvironment.lookup(fenv, fname)
            |> Option.map(DHExp.term_of) == Some(f) =>
        matches_exp(
          ~denv=ClosureEnvironment.without_keys([dname], denv),
          d |> DHExp.fresh,
          ~fenv=ClosureEnvironment.without_keys([fname], fenv),
          f |> DHExp.fresh,
        )
      | (
          Some(Fun(_, _, _, Some(dname)) as d),
          Some(Fun(_, _, _, Some(fname)) as f),
        )
          when
            ClosureEnvironment.lookup(denv, dname)
            |> Option.map(DHExp.term_of) == Some(d)
            && ClosureEnvironment.lookup(fenv, fname)
            |> Option.map(DHExp.term_of) == Some(f) =>
        matches_exp(
          ~denv=ClosureEnvironment.without_keys([dname], denv),
          d |> DHExp.fresh,
          ~fenv=ClosureEnvironment.without_keys([fname], denv),
          f |> DHExp.fresh,
        )
      | (Some(d), Some(f)) => matches_exp(d |> Exp.fresh, f |> Exp.fresh)
      | (Some(_), None) => false
      | (None, Some(_)) => false
      | (None, None) => true
      }
    | (Var(dx), _) =>
      switch (ClosureEnvironment.lookup(denv, dx)) {
      | Some(d) => matches_exp(d, f)
      | None => false
      }
    | (_, Var(fx)) =>
      switch (ClosureEnvironment.lookup(fenv, fx)) {
      | Some(f) => matches_exp(d, f)
      | None => false
      }

    | (EmptyHole, _) => false

    | (Deferral(x), Deferral(y)) => x == y
    | (Deferral(_), _) => false

    | (Filter(df, dd), Filter(ff, fd)) =>
      TermBase.StepperFilterKind.fast_equal(df, ff) && matches_exp(dd, fd)
    | (Filter(_), _) => false

    | (Bool(dv), Bool(fv)) => dv == fv
    | (Bool(_), _) => false

    | (Int(dv), Int(fv)) => dv == fv
    | (Int(_), _) => false

    | (Float(dv), Float(fv)) => dv == fv
    | (Float(_), _) => false

    | (String(dv), String(fv)) => dv == fv
    | (String(_), _) => false

    | (
        Constructor(_),
        Ap(_, {term: Constructor("~MVal", _), _}, {term: Tuple([]), _}),
      ) =>
      true
    | (Constructor(dt, _), Constructor(ft, _)) => dt == ft
    | (Constructor(_), _) => false

    | (BuiltinFun(dn), BuiltinFun(fn)) => dn == fn
    | (BuiltinFun(_), _) => false

    | (TypFun(pat1, d1, s1), TypFun(pat2, d2, s2)) =>
      s1 == s2 && matches_utpat(pat1, pat2) && matches_exp(d1, d2)
    | (TypFun(_), _) => false

    | (Fun(dp1, d1, Some(denv), _), Fun(fp1, f1, Some(fenv), _)) =>
      matches_fun(~denv, dp1, d1, ~fenv, fp1, f1)
    | (Fun(dp1, d1, Some(denv), _), Fun(fp1, f1, None, _)) =>
      matches_fun(~denv, dp1, d1, ~fenv, fp1, f1)
    | (Fun(dp1, d1, None, _), Fun(fp1, f1, Some(fenv), _)) =>
      matches_fun(~denv, dp1, d1, ~fenv, fp1, f1)
    | (Fun(dp1, d1, None, _), Fun(fp1, f1, None, _)) =>
      matches_fun(~denv, dp1, d1, ~fenv, fp1, f1)
    | (Fun(_), _) => false

    | (Let(dp, d1, d2), Let(fp, f1, f2)) =>
      matches_pat(dp, fp) && matches_exp(d1, f1) && matches_exp(d2, f2)
    | (Let(_), _) => false

    | (Theorem(dp, d1, d2), Theorem(fp, f1, f2)) =>
      matches_pat(dp, fp) && matches_exp(d1, f1) && matches_exp(d2, f2)
    | (Theorem(_), _) => false

    | (TypAp(d1, t1), TypAp(d2, t2)) =>
      matches_exp(d1, d2) && matches_typ(t1, t2)
    | (TypAp(_), _) => false

    // TODO: do we want f(x) to match x |> f ???
    | (Ap(_, d1, d2), Ap(_, f1, f2)) =>
      matches_exp(d1, f1) && matches_exp(d2, f2)
    | (Ap(_), _) => false

    | (DeferredAp(d1, d2), DeferredAp(f1, f2)) =>
      matches_exp(d1, f1)
      && List.fold_left2(
           (acc, d, f) => acc && matches_exp(d, f),
           true,
           d2,
           f2,
         )
    | (DeferredAp(_), _) => false

    | (If(d1, d2, d3), If(f1, f2, f3)) =>
      matches_exp(d1, f1) && matches_exp(d2, f2) && matches_exp(d3, f3)
    | (If(_), _) => false

    | (Seq(d1, d2), Seq(f1, f2)) =>
      matches_exp(d1, f1) && matches_exp(d2, f2)
    | (Seq(_), _) => false

    | (Test(d2), Test(f2)) => matches_exp(d2, f2)
    | (Test(_), _) => false

    | (Cons(d1, d2), Cons(f1, f2)) =>
      matches_exp(d1, f1) && matches_exp(d2, f2)
    | (Cons(_), _) => false

    | (ListLit(dv), ListLit(fv)) =>
      List.fold_left2((acc, d, f) => acc && matches_exp(d, f), true, dv, fv)
    | (ListLit(_), _) => false

    | (Tuple(dv), Tuple(fv)) =>
      List.fold_left2((acc, d, f) => acc && matches_exp(d, f), true, dv, fv)
    | (Tuple(_), _) => false

    | (UnOp(d_op, d1), UnOp(f_op, f1)) =>
      d_op == f_op && matches_exp(d1, f1)
    | (UnOp(_), _) => false

    | (BinOp(d_op, d1, d2), BinOp(f_op, f1, f2)) =>
      d_op == f_op && matches_exp(d1, f1) && matches_exp(d2, f2)
    | (BinOp(_), _) => false

    | (ListConcat(d1, d2), ListConcat(f1, f2)) =>
      matches_exp(d1, f1) && matches_exp(d2, f2)
    | (ListConcat(_), _) => false

    | (Match(dscrut, drule), Match(fscrut, frule)) =>
      matches_exp(dscrut, fscrut)
      && (
        switch (
          List.for_all2(
            ((dk, dv), (fk, fv)) =>
              matches_pat(dk, fk) && matches_exp(dv, fv),
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
    | (Invalid(_), _) => false
    | (DynamicErrorHole(_), _) => false

    | (Undefined, _) => false

    | (TyAlias(dtp, dut, dd), TyAlias(ftp, fut, fd)) =>
      dtp == ftp && dut == fut && matches_exp(dd, fd)
    | (TyAlias(_), _) => false
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
       ~denv=ClosureEnvironment.without_keys(DHPat.bound_vars(dp), denv),
       d,
       ~fenv=ClosureEnvironment.without_keys(DHPat.bound_vars(fp), fenv),
       f,
     );
}

and matches_pat = (d: Pat.t, f: Pat.t): bool => {
  switch (d |> DHPat.term_of, f |> DHPat.term_of) {
  // Matt: I'm not sure what the exact semantics of matching should be here.
  | (Parens(x), _) => matches_pat(x, f)
  | (_, Parens(x)) => matches_pat(d, x)
  | (Cast(x, _, _), _) => matches_pat(x, f)
  | (_, Cast(x, _, _)) => matches_pat(d, x)
  | (_, EmptyHole) => true
  | (MultiHole(_), MultiHole(_)) => true
  | (MultiHole(_), _) => false
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
  | (Constructor(dt, _), Constructor(ft, _)) => dt == ft
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
  | (Cons(d1, d2), Cons(f1, f2)) =>
    matches_pat(d1, f1) && matches_pat(d2, f2)
  | (Cons(_), _) => false
  | (EmptyHole, _) => false
  | (Invalid(_), _) => false
  };
}
and matches_typ = (d: Typ.t, f: Typ.t) => {
  Typ.eq(d, f);
}
and matches_utpat = (d: TPat.t, f: TPat.t): bool => {
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
    (
      ~env: ClosureEnvironment.t,
      ~exp: DHExp.t,
      ~flt: TermBase.StepperFilterKind.filter,
    )
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
