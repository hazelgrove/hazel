/* closed substitution [d1/x]d2 */
let rec subst_var = (d1: DHExp.t, x: Var.t, d2: DHExp.t): DHExp.t => {
  let (term, rewrap) = DHExp.unwrap(d2);
  switch (term) {
  | Var(y) =>
    if (Var.eq(x, y)) {
      d1;
    } else {
      d2;
    }
  | FreeVar(_) => d2
  | InvalidText(_) => d2
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
      if (DHPat.binds_var(x, dp)) {
        d4;
      } else {
        subst_var(d1, x, d4);
      };
    Let(dp, d3, d4) |> rewrap;
  | FixF(y, ty, d3) =>
    let d3 =
      if (Var.eq(x, y)) {
        d3;
      } else {
        subst_var(d1, x, d3);
      };
    FixF(y, ty, d3) |> rewrap;
  | Fun(dp, ty, d3, env, s) =>
    /* Function closure shouldn't appear during substitution
       (which only is called from elaboration currently) */
    let env' = Option.map(subst_var_env(d1, x), env);
    if (DHPat.binds_var(x, dp)) {
      Fun(dp, ty, d3, env', s) |> rewrap;
    } else {
      let d3 = subst_var(d1, x, d3);
      Fun(dp, ty, d3, env', s) |> rewrap;
    };
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
  | ApBuiltin(ident, d1) =>
    let d2 = subst_var(d1, x, d1);
    ApBuiltin(ident, d2) |> rewrap;
  | BuiltinFun(_) => d2
  | Test(id, d3) => Test(id, subst_var(d1, x, d3)) |> rewrap
  | Bool(_)
  | Int(_)
  | Float(_)
  | String(_)
  | Constructor(_) => d2
  | ListLit(a, b, c, ds) =>
    ListLit(a, b, c, List.map(subst_var(d1, x), ds)) |> rewrap
  | Cons(d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 = subst_var(d1, x, d4);
    Cons(d3, d4) |> rewrap;
  | ListConcat(d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 = subst_var(d1, x, d4);
    ListConcat(d3, d4) |> rewrap;
  | Tuple(ds) => Tuple(List.map(subst_var(d1, x), ds)) |> rewrap
  | Prj(d, n) => Prj(subst_var(d1, x, d), n) |> rewrap
  | BinOp(op, d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 = subst_var(d1, x, d4);
    BinOp(op, d3, d4) |> rewrap;
  | Match(c, ds, rules) =>
    let ds = subst_var(d1, x, ds);
    let rules =
      List.map(
        ((p, v)) =>
          if (DHPat.binds_var(x, p)) {
            (p, v);
          } else {
            (p, subst_var(d1, x, v));
          },
        rules,
      );
    Match(c, ds, rules) |> rewrap;
  | EmptyHole => EmptyHole |> rewrap
  | NonEmptyHole(reason, u, i, d3) =>
    let d3' = subst_var(d1, x, d3);
    NonEmptyHole(reason, u, i, d3') |> rewrap;
  | Cast(d, ty1, ty2) =>
    let d' = subst_var(d1, x, d);
    Cast(d', ty1, ty2) |> rewrap;
  | FailedCast(d, ty1, ty2) =>
    let d' = subst_var(d1, x, d);
    FailedCast(d', ty1, ty2) |> rewrap;
  | InvalidOperation(d, err) =>
    let d' = subst_var(d1, x, d);
    InvalidOperation(d', err) |> rewrap;
  | If(d3, d4, d5, d6) =>
    let d4' = subst_var(d1, x, d4);
    let d5' = subst_var(d1, x, d5);
    let d6' = subst_var(d1, x, d6);
    If(d3, d4', d5', d6') |> rewrap;
  };
}

and subst_var_env =
    (d1: DHExp.t, x: Var.t, env: ClosureEnvironment.t): ClosureEnvironment.t => {
  let id = env |> ClosureEnvironment.id_of;
  let map =
    env
    |> ClosureEnvironment.map_of
    |> Environment.foldo(
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
       );

  ClosureEnvironment.wrap(id, map);
}

and subst_var_filter =
    (d1: DHExp.t, x: Var.t, flt: DH.DHFilter.t): DH.DHFilter.t => {
  flt |> DH.DHFilter.map(subst_var(d1, x));
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
