/* closed substitution [d1/x]d2 */
let rec subst_var = (m, d1: DExp.t, x: Var.t, d2: DExp.t): DExp.t => {
  let (term, rewrap) = DExp.unwrap(d2);
  switch (term) {
  | Var(y) =>
    if (Var.eq(x, y)) {
      d1;
    } else {
      d2;
    }
  | Invalid(_) => d2
  | Seq(d3, d4) =>
    let d3 = subst_var(m, d1, x, d3);
    let d4 = subst_var(m, d1, x, d4);
    Seq(d3, d4) |> rewrap;
  | Filter(filter, dbody) =>
    let dbody = subst_var(m, d1, x, dbody);
    let filter = subst_var_filter(m, d1, x, filter);
    Filter(filter, dbody) |> rewrap;
  | Let(dp, d3, d4) =>
    let d3 = subst_var(m, d1, x, d3);
    let d4 =
      if (DHPat.binds_var(m, x, dp)) {
        d4;
      } else {
        subst_var(m, d1, x, d4);
      };
    Let(dp, d3, d4) |> rewrap;
  | FixF(y, d3, env) =>
    let env' = Option.map(subst_var_env(m, d1, x), env);
    let d3 =
      if (DHPat.binds_var(m, x, y)) {
        d3;
      } else {
        subst_var(m, d1, x, d3);
      };
    FixF(y, d3, env') |> rewrap;
  | Fun(dp, d3, env, s) =>
    /* Function closure shouldn't appear during substitution
       (which only is called from elaboration currently) */
    let env' = Option.map(subst_var_env(m, d1, x), env);
    if (DHPat.binds_var(m, x, dp)) {
      Fun(dp, d3, env', s) |> rewrap;
    } else {
      let d3 = subst_var(m, d1, x, d3);
      Fun(dp, d3, env', s) |> rewrap;
    };
  | Closure(env, d3) =>
    /* Closure shouldn't appear during substitution (which
       only is called from elaboration currently) */
    let env' = subst_var_env(m, d1, x, env);
    let d3' = subst_var(m, d1, x, d3);
    Closure(env', d3') |> rewrap;
  | Ap(dir, d3, d4) =>
    let d3 = subst_var(m, d1, x, d3);
    let d4 = subst_var(m, d1, x, d4);
    Ap(dir, d3, d4) |> rewrap;
  | BuiltinFun(_) => d2
  | Test(d3) => Test(subst_var(m, d1, x, d3)) |> rewrap
  | Bool(_)
  | Int(_)
  | Float(_)
  | String(_)
  | Constructor(_) => d2
  | ListLit(ds) => ListLit(List.map(subst_var(m, d1, x), ds)) |> rewrap
  | Cons(d3, d4) =>
    let d3 = subst_var(m, d1, x, d3);
    let d4 = subst_var(m, d1, x, d4);
    Cons(d3, d4) |> rewrap;
  | ListConcat(d3, d4) =>
    let d3 = subst_var(m, d1, x, d3);
    let d4 = subst_var(m, d1, x, d4);
    ListConcat(d3, d4) |> rewrap;
  | Tuple(ds) => Tuple(List.map(subst_var(m, d1, x), ds)) |> rewrap
  | UnOp(op, d3) =>
    let d3 = subst_var(m, d1, x, d3);
    UnOp(op, d3) |> rewrap;
  | BinOp(op, d3, d4) =>
    let d3 = subst_var(m, d1, x, d3);
    let d4 = subst_var(m, d1, x, d4);
    BinOp(op, d3, d4) |> rewrap;
  | Match(ds, rules) =>
    let ds = subst_var(m, d1, x, ds);
    let rules =
      List.map(
        ((p, v)) =>
          if (DHPat.binds_var(m, x, p)) {
            (p, v);
          } else {
            (p, subst_var(m, d1, x, v));
          },
        rules,
      );
    Match(ds, rules) |> rewrap;
  | EmptyHole => EmptyHole |> rewrap
  // TODO: handle multihole
  | MultiHole(_d2) => d2 //MultiHole(List.map(subst_var(m, d1, x), ds)) |> rewrap
  | StaticErrorHole(u, d3) =>
    let d3' = subst_var(m, d1, x, d3);
    StaticErrorHole(u, d3') |> rewrap;
  | Cast(d, ty1, ty2) =>
    let d' = subst_var(m, d1, x, d);
    Cast(d', ty1, ty2) |> rewrap;
  | FailedCast(d, ty1, ty2) =>
    let d' = subst_var(m, d1, x, d);
    FailedCast(d', ty1, ty2) |> rewrap;
  | DynamicErrorHole(d, err) =>
    let d' = subst_var(m, d1, x, d);
    DynamicErrorHole(d', err) |> rewrap;
  | If(d4, d5, d6) =>
    let d4' = subst_var(m, d1, x, d4);
    let d5' = subst_var(m, d1, x, d5);
    let d6' = subst_var(m, d1, x, d6);
    If(d4', d5', d6') |> rewrap;
  | TyAlias(tp, ut, d4) =>
    let d4' = subst_var(m, d1, x, d4);
    TyAlias(tp, ut, d4') |> rewrap;
  | Parens(d4) =>
    let d4' = subst_var(m, d1, x, d4);
    Parens(d4') |> rewrap;
  };
}

and subst_var_env =
    (m, d1: DExp.t, x: Var.t, env: ClosureEnvironment.t): ClosureEnvironment.t => {
  let id = env |> ClosureEnvironment.id_of;
  let map =
    env
    |> ClosureEnvironment.map_of
    |> Environment.foldo(
         ((x', d': DExp.t), map) => {
           let d' =
             switch (DExp.term_of(d')) {
             /* Substitute each previously substituted binding into the
              * fixpoint. */
             | FixF(_) =>
               map
               |> Environment.foldo(
                    ((x'', d''), d) => subst_var(m, d'', x'', d),
                    d',
                  )
             | _ => d'
             };

           /* Substitute. */
           let d' = subst_var(m, d1, x, d');
           Environment.extend(map, (x', d'));
         },
         Environment.empty,
       );

  ClosureEnvironment.wrap(id, map);
}

and subst_var_filter =
    (m, d1: DExp.t, x: Var.t, flt: TermBase.StepperFilterKind.t)
    : TermBase.StepperFilterKind.t => {
  flt |> TermBase.StepperFilterKind.map(subst_var(m, d1, x));
};

let subst = (m, env: Environment.t, d: DExp.t): DExp.t =>
  env
  |> Environment.foldo(
       (xd: (Var.t, DExp.t), d2) => {
         let (x, d1) = xd;
         subst_var(m, d1, x, d2);
       },
       d,
     );
