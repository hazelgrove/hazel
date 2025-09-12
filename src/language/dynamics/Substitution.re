/**
 * Whether dp contains the variable x outside of a hole.
 */
let rec binds_var = (x: Var.t, dp: DHPat.t): bool =>
  switch (dp |> Pat.term_of) {
  | EmptyHole
  | MultiHole(_)
  | Wild
  | Invalid(_)
  | Atom(_)
  | Label(_)
  | Constructor(_) => false
  | Asc(y, _)
  | Parens(y)
  | Probe(y, _) => binds_var(x, y)
  | Var(y) => Var.equal(x, y)
  | TupLabel(_, dp) => binds_var(x, dp)
  | Tuple(dps) => dps |> List.exists(binds_var(x))
  | Cons(dp1, dp2) => binds_var(x, dp1) || binds_var(x, dp2)
  | ListLit(d_list) =>
    let new_list = List.map(binds_var(x), d_list);
    List.fold_left((||), false, new_list);
  | Ap(_, _) => false
  };
let rec expr_contains_var = (d: DHExp.t, x: Var.t): bool => {
  let (term, _) = DHExp.unwrap(d);
  switch (term) {
  | Var(y) => Var.equal(x, y)
  | Invalid(_) => false
  | Undefined => false
  | Seq(d3, d4) => expr_contains_var(d3, x) || expr_contains_var(d4, x)
  | Filter(_, _) =>
    //unimplemented
    false
  | Let(dp, d3, d4) =>
    if (expr_contains_var(d3, x)) {
      true;
    } else if (binds_var(x, dp)) {
      false;
    } else {
      expr_contains_var(d4, x);
    }
  | FixF(_, _, _) =>
    //unimplemnted
    false
  | Fun(dp, d3, _, _) =>
    if (binds_var(x, dp)) {
      false;
    } else {
      expr_contains_var(d3, x);
    }
  | TypFun(_, d3, _) => expr_contains_var(d3, x)
  | Closure(_, d3) =>
    /* Closure shouldn't appear during substitution (which
       only is called from elaboration currently) */
    expr_contains_var(d3, x)
  | Ap(_, d3, d4) => expr_contains_var(d3, x) || expr_contains_var(d4, x)
  | BuiltinFun(_) => false
  | Test(d)
  | HintedTest(d, _) => expr_contains_var(d, x)
  | Atom(_)
  | Label(_)
  | LivelitName(_)
  | Constructor(_) => false
  | ListLit(ds) =>
    List.fold_left((acc, d) => acc || expr_contains_var(d, x), false, ds)
  | Cons(d3, d4) => expr_contains_var(d3, x) || expr_contains_var(d4, x)
  | ListConcat(d3, d4) =>
    expr_contains_var(d3, x) || expr_contains_var(d4, x)
  | TupLabel(_, d) => expr_contains_var(d, x)
  | Dot(d3, d4) => expr_contains_var(d3, x) || expr_contains_var(d4, x)
  | Tuple(ds) =>
    List.fold_left((acc, d) => acc || expr_contains_var(d, x), false, ds)
  | TupleExtension(d3, d4) =>
    expr_contains_var(d3, x) || expr_contains_var(d4, x)
  | UnOp(_, d3) => expr_contains_var(d3, x)
  | BinOp(_, d3, d4) => expr_contains_var(d3, x) || expr_contains_var(d4, x)
  | Match(_, _) => false
  /* Unimplemented
     let ds = subst_var(d1, x, ds);
     let rules =
       List.map(
         ((p, v)) =>
           if (binds_var(x, p)) {
             (p, v);
           } else {
             (p, subst_var(d1, x, v));
           },
         rules,
       );
     Match(ds, rules) |> rewrap;*/
  | EmptyHole => false
  // TODO: handle multihole
  | MultiHole(_d2) => false //MultiHole(List.map(subst_var(m, d1, x), ds)) |> rewrap
  | Asc(d, _) => expr_contains_var(d, x)
  | DynamicErrorHole(d, _) => expr_contains_var(d, x)
  | If(d4, d5, d6) =>
    expr_contains_var(d4, x)
    || expr_contains_var(d5, x)
    || expr_contains_var(d6, x)
  | TyAlias(_, _, d4) => expr_contains_var(d4, x)
  | Use(_, d) => expr_contains_var(d, x)
  | Parens(d4) => expr_contains_var(d4, x)
  | Probe(d4, _) => expr_contains_var(d4, x)
  | Deferral(_) => false
  | DeferredAp(d3, d4s) =>
    expr_contains_var(d3, x)
    || List.fold_left(
         (acc, d) => acc || expr_contains_var(d, x),
         false,
         d4s,
       )
  | TypAp(d3, _) => expr_contains_var(d3, x)
  };
};

/* closed substitution [d1/x]d2 */
let rec subst_var = (d1: DHExp.t, x: Var.t, d2: DHExp.t): DHExp.t => {
  let (term, rewrap) = DHExp.unwrap(d2);
  switch (term) {
  | Var(y) =>
    if (Var.equal(x, y)) {
      d1;
    } else {
      d2;
    }
  | Invalid(_) => d2
  | Undefined => d2
  | Seq(d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 = subst_var(d1, x, d4);
    Seq(d3, d4) |> rewrap;
  | Filter(filter, dbody) =>
    let dbody = subst_var(d1, x, dbody);
    let filter = subst_var_filter(d1, x, filter);
    Filter(filter, dbody) |> rewrap;
  | Let(dp, d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 =
      if (binds_var(x, dp)) {
        d4;
      } else {
        subst_var(d1, x, d4);
      };
    Let(dp, d3, d4) |> rewrap;
  | FixF(y, d3, env) =>
    let env' = Option.map(subst_var_env(d1, x), env);
    let d3 =
      if (binds_var(x, y)) {
        d3;
      } else {
        subst_var(d1, x, d3);
      };
    FixF(y, d3, env') |> rewrap;
  | Fun(dp, d3, ty, s) =>
    if (binds_var(x, dp)) {
      Fun(dp, d3, ty, s) |> rewrap;
    } else {
      let d3 = subst_var(d1, x, d3);
      Fun(dp, d3, ty, s) |> rewrap;
    }
  | TypFun(tpat, d3, s) => TypFun(tpat, subst_var(d1, x, d3), s) |> rewrap
  | Closure(env, d3) =>
    /* Closure shouldn't appear during substitution (which
       only is called from elaboration currently) */
    let env' = subst_var_env(d1, x, env);
    let d3' = subst_var(d1, x, d3);
    Closure(env', d3') |> rewrap;
  | Ap(dir, d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 = subst_var(d1, x, d4);
    Ap(dir, d3, d4) |> rewrap;
  | BuiltinFun(_) => d2
  | Test(d3) => Test(subst_var(d1, x, d3)) |> rewrap
  | HintedTest(d3, h) => HintedTest(subst_var(d1, x, d3), h) |> rewrap
  | Atom(_)
  | Label(_)
  | LivelitName(_)
  | Constructor(_) => d2
  | ListLit(ds) => ListLit(List.map(subst_var(d1, x), ds)) |> rewrap
  | Cons(d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 = subst_var(d1, x, d4);
    Cons(d3, d4) |> rewrap;
  | ListConcat(d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 = subst_var(d1, x, d4);
    ListConcat(d3, d4) |> rewrap;
  | TupLabel(label, d) => TupLabel(label, subst_var(d1, x, d)) |> rewrap
  | Dot(d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 = subst_var(d1, x, d4);
    Dot(d3, d4) |> rewrap;
  | Tuple(ds) => Tuple(List.map(subst_var(d1, x), ds)) |> rewrap
  | TupleExtension(d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 = subst_var(d1, x, d4);
    TupleExtension(d3, d4) |> rewrap;
  | UnOp(op, d3) =>
    let d3 = subst_var(d1, x, d3);
    UnOp(op, d3) |> rewrap;
  | BinOp(op, d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 = subst_var(d1, x, d4);
    BinOp(op, d3, d4) |> rewrap;
  | Match(ds, rules) =>
    let ds = subst_var(d1, x, ds);
    let rules =
      List.map(
        ((p, v)) =>
          if (binds_var(x, p)) {
            (p, v);
          } else {
            (p, subst_var(d1, x, v));
          },
        rules,
      );
    Match(ds, rules) |> rewrap;
  | EmptyHole => EmptyHole |> rewrap
  // TODO: handle multihole
  | MultiHole(_d2) => d2 //MultiHole(List.map(subst_var(m, d1, x), ds)) |> rewrap
  | Asc(d, ty) =>
    let d' = subst_var(d1, x, d);
    Asc(d', ty) |> rewrap;
  | DynamicErrorHole(d, err) =>
    let d' = subst_var(d1, x, d);
    DynamicErrorHole(d', err) |> rewrap;
  | If(d4, d5, d6) =>
    let d4' = subst_var(d1, x, d4);
    let d5' = subst_var(d1, x, d5);
    let d6' = subst_var(d1, x, d6);
    If(d4', d5', d6') |> rewrap;
  | TyAlias(tp, ut, d4) =>
    let d4' = subst_var(d1, x, d4);
    TyAlias(tp, ut, d4') |> rewrap;
  | Use(t, d) =>
    let d' = subst_var(d1, x, d);
    Use(t, d') |> rewrap;
  | Parens(d4) =>
    let d4' = subst_var(d1, x, d4);
    Parens(d4') |> rewrap;
  | Probe(d4, pr) =>
    let d4' = subst_var(d1, x, d4);
    Probe(d4', pr) |> rewrap;
  | Deferral(_) => d2
  | DeferredAp(d3, d4s) =>
    let d3 = subst_var(d1, x, d3);
    let d4s = List.map(subst_var(d1, x), d4s);
    DeferredAp(d3, d4s) |> rewrap;
  | TypAp(d3, ut) =>
    let d3 = subst_var(d1, x, d3);
    TypAp(d3, ut) |> rewrap;
  };
}

and subst_var_env =
    (d1: DHExp.t, x: Var.t, env: ClosureEnvironment.t): ClosureEnvironment.t => {
  Environment.foldo(
    ((x', d': DHExp.t), map) => {
      let d' =
        switch (DHExp.term_of(d')) {
        /* Substitute each previously substituted binding into the
         * fixpoint. */
        | FixF(_) =>
          map
          |> Environment.foldo(
               ((x'', d''), d) => subst_var(d'', x'', d),
               d',
             )
        | _ => d'
        };

      /* Substitute. */
      let d' = subst_var(d1, x, d');
      Environment.extend(map, (x', d'));
    },
    Environment.empty,
  )
  |> ClosureEnvironment.update_env(_, env);
}

and subst_var_filter =
    (d1: DHExp.t, x: Var.t, flt: TermBase.StepperFilterKind.t)
    : TermBase.StepperFilterKind.t => {
  flt |> TermBase.StepperFilterKind.map(subst_var(d1, x));
};

let subst = (env: Environment.t, d: DHExp.t): DHExp.t =>
  env
  |> Environment.foldo(
       (xd: (Var.t, DHExp.t), d2) => {
         let (x, d1) = xd;
         subst_var(d1, x, d2);
       },
       d,
     );
