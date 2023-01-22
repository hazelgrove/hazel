/* closed substitution [d1/x]d2 */
let rec subst_var = (d1: DHExp.t, x: Var.t, d2: DHExp.t): DHExp.t =>
  switch (d2.term) {
  | Var(y) =>
    if (Var.eq(x, y)) {
      d1;
    } else {
      d2;
    }
  | Hole(_, FreeVar(_)) => d2
  | Hole(_, Invalid(_)) => d2
  | Hole(_, InvalidText(_)) => d2
  | Hole(_, ExpandingKeyword(_)) => d2
  | Seq(d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 = subst_var(d1, x, d4);
    DHExp.{ids: d2.ids, term: Seq(d3, d4)};
  | Let(dp, d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 =
      if (DHPat.binds_var(x, dp)) {
        d4;
      } else {
        subst_var(d1, x, d4);
      };
    DHExp.{ids: d2.ids, term: Let(dp, d3, d4)};
  | FixF(y, ty, d3) =>
    let d3 =
      if (Var.eq(x, y)) {
        d3;
      } else {
        subst_var(d1, x, d3);
      };
    DHExp.{ids: d2.ids, term: FixF(y, ty, d3)};
  | Fun(dp, ty, d3, s) =>
    if (DHPat.binds_var(x, dp)) {
      DHExp.{ids: d2.ids, term: Fun(dp, ty, d3, s)};
    } else {
      let d3 = subst_var(d1, x, d3);
      DHExp.{ids: d2.ids, term: Fun(dp, ty, d3, s)};
    }
  | Closure(env, d3) =>
    /* Closure shouldn't appear during substitution (which
       only is called from elaboration currently) */
    let env' = subst_var_env(d1, x, env);
    let d3' = subst_var(d1, x, d3);
    DHExp.{ids: d2.ids, term: Closure(env', d3')};
  | Ap(d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 = subst_var(d1, x, d4);
    DHExp.{ids: d2.ids, term: Ap(d3, d4)};
  | ApBuiltin(ident, args) =>
    let args = List.map(subst_var(d1, x), args);
    DHExp.{ids: d2.ids, term: ApBuiltin(ident, args)};
  | Triv
  | Test(_)
  | Bool(_)
  | Int(_)
  | Float(_)
  | String(_)
  | Tag(_) => d2
  | ListLit(ds, info) =>
    DHExp.{
      ids: d2.ids,
      term: ListLit(List.map(subst_var(d1, x), ds), info),
    }
  | Parens(d3) =>
    let d3 = subst_var(d1, x, d3);
    DHExp.{ids: d2.ids, term: Parens(d3)};
  | Cons(d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 = subst_var(d1, x, d4);
    DHExp.{ids: d2.ids, term: Cons(d3, d4)};
  | Tuple(ds) =>
    DHExp.{ids: d2.ids, term: Tuple(List.map(subst_var(d1, x), ds))}
  | Prj(d, n) => DHExp.{ids: d2.ids, term: Prj(subst_var(d1, x, d), n)}
  | BinOp(op, d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 = subst_var(d1, x, d4);
    DHExp.{ids: d2.ids, term: BinOp(op, d3, d4)};
  | UnOp(op, d3) =>
    let d3 = subst_var(d1, x, d3);
    DHExp.{ids: d2.ids, term: UnOp(op, d3)};
  | Inj(ty, side, d3) =>
    let d3 = subst_var(d1, x, d3);
    DHExp.{ids: d2.ids, term: Inj(ty, side, d3)};
  | If(_) =>
    failwith("subst_var on If, which should be elaborated into Match")
  | Match(d3, rules, n) =>
    let d3 = subst_var(d1, x, d3);
    let rules = subst_var_rules(d1, x, rules);
    DHExp.{ids: d2.ids, term: Match(d3, rules, n)};
  | Hole(hi, InconsistentBranches(d3, rules, n)) =>
    let d3 = subst_var(d1, x, d3);
    let rules = subst_var_rules(d1, x, rules);
    DHExp.{ids: d2.ids, term: Hole(hi, InconsistentBranches(d3, rules, n))};
  | Hole(_, EmptyHole) as dterm => DHExp.{ids: d2.ids, term: dterm}
  | Hole(_, MultiHole(_)) as dterm => DHExp.{ids: d2.ids, term: dterm}
  | Hole(hi, NonEmptyHole(reason, d3)) =>
    let d3' = subst_var(d1, x, d3);
    DHExp.{ids: d2.ids, term: Hole(hi, NonEmptyHole(reason, d3'))};
  | Cast(d, ty1, ty2) =>
    let d' = subst_var(d1, x, d);
    DHExp.{ids: d2.ids, term: Cast(d', ty1, ty2)};
  | Hole(hi, FailedCast(d, ty1, ty2)) =>
    let d' = subst_var(d1, x, d);
    DHExp.{ids: d2.ids, term: Hole(hi, FailedCast(d', ty1, ty2))};
  | Hole(hi, InvalidOperation(err, d)) =>
    let d' = subst_var(d1, x, d);
    DHExp.{ids: d2.ids, term: Hole(hi, InvalidOperation(err, d'))};
  }

and subst_var_rules =
    (d1: DHExp.t, x: Var.t, rules: list(DHExp.rule)): list(DHExp.rule) =>
  rules
  |> List.map(((dp, d2): DHExp.rule) =>
       if (DHPat.binds_var(x, dp)) {
         (dp, d2);
       } else {
         (dp, subst_var(d1, x, d2));
       }
     )

and subst_var_env =
    (d1: DHExp.t, x: Var.t, env: ClosureEnvironment.t): ClosureEnvironment.t => {
  let id = env |> ClosureEnvironment.id_of;
  let map =
    env
    |> ClosureEnvironment.map_of
    |> Environment.foldo(
         ((x', d': DHExp.t), map) => {
           let d' =
             switch (d'.term) {
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
