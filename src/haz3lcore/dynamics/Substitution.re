/* closed substitution [d1/x]d2 */
let rec subst_var = (d1: DHExp.t, x: Var.t, d2: DHExp.t): DHExp.t =>
  switch (d2) {
  | BoundVar(y) =>
    if (Var.eq(x, y)) {
      d1;
    } else {
      d2;
    }
  | FreeVar(_) => d2
  | InvalidText(_) => d2
  | ExpandingKeyword(_) => d2
  | Sequence(d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 = subst_var(d1, x, d4);
    Sequence(d3, d4);
  | Let(dp, d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 =
      if (DHPat.binds_var(x, dp)) {
        d4;
      } else {
        subst_var(d1, x, d4);
      };
    Let(dp, d3, d4);
  | Module(dp, d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 =
      if (DHPat.binds_var(x, dp)) {
        d4;
      } else {
        subst_var(d1, x, d4);
      };
    Module(dp, d3, d4);
  | Dot(u, i, d, n) =>
    let d = subst_var(d1, x, d);
    Dot(u, i, d, n);
  | FreeDot(u, i, d, n) =>
    let d = subst_var(d1, x, d);
    FreeDot(u, i, d, n);
  | FixF(y, ty, d3) =>
    let d3 =
      if (Var.eq(x, y)) {
        d3;
      } else {
        subst_var(d1, x, d3);
      };
    FixF(y, ty, d3);
  | Fun(dp, ty, d3, s) =>
    if (DHPat.binds_var(x, dp)) {
      Fun(dp, ty, d3, s);
    } else {
      let d3 = subst_var(d1, x, d3);
      Fun(dp, ty, d3, s);
    }
  | Closure(env, d3) =>
    /* Closure shouldn't appear during substitution (which
       only is called from elaboration currently) */
    let env' = subst_var_env(d1, x, env);
    let d3' = subst_var(d1, x, d3);
    Closure(env', d3');
  | Ap(d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 = subst_var(d1, x, d4);
    Ap(d3, d4);
  | ApBuiltin(ident, args) =>
    let args = List.map(subst_var(d1, x), args);
    ApBuiltin(ident, args);
  | TestLit(_)
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | StringLit(_)
  | ModuleVal(_)
  | Tag(_) => d2
  | ListLit(a, b, c, ds) =>
    ListLit(a, b, c, List.map(subst_var(d1, x), ds))
  | Cons(d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 = subst_var(d1, x, d4);
    Cons(d3, d4);
  | TupLabel(_) => d2
  | Tuple(ds) => Tuple(List.map(subst_var(d1, x), ds))
  | Prj(d, n) => Prj(subst_var(d1, x, d), n)
  | BinBoolOp(op, d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 = subst_var(d1, x, d4);
    BinBoolOp(op, d3, d4);
  | BinIntOp(op, d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 = subst_var(d1, x, d4);
    BinIntOp(op, d3, d4);
  | BinFloatOp(op, d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 = subst_var(d1, x, d4);
    BinFloatOp(op, d3, d4);
  | BinStringOp(op, d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 = subst_var(d1, x, d4);
    BinStringOp(op, d3, d4);
  | ConsistentCase(Case(d3, rules, n)) =>
    let d3 = subst_var(d1, x, d3);
    let rules = subst_var_rules(d1, x, rules);
    ConsistentCase(Case(d3, rules, n));
  | InconsistentBranches(u, i, Case(d3, rules, n)) =>
    let d3 = subst_var(d1, x, d3);
    let rules = subst_var_rules(d1, x, rules);
    InconsistentBranches(u, i, Case(d3, rules, n));
  | EmptyHole(u, i) => EmptyHole(u, i)
  | NonEmptyHole(reason, u, i, d3) =>
    let d3' = subst_var(d1, x, d3);
    NonEmptyHole(reason, u, i, d3');
  | Cast(d, ty1, ty2) =>
    let d' = subst_var(d1, x, d);
    Cast(d', ty1, ty2);
  | FailedCast(d, ty1, ty2) =>
    let d' = subst_var(d1, x, d);
    FailedCast(d', ty1, ty2);
  | InvalidOperation(d, err) =>
    let d' = subst_var(d1, x, d);
    InvalidOperation(d', err);
  }

and subst_var_rules =
    (d1: DHExp.t, x: Var.t, rules: list(DHExp.rule)): list(DHExp.rule) =>
  rules
  |> List.map((r: DHExp.rule) =>
       switch (r) {
       | Rule(dp, d2) =>
         if (DHPat.binds_var(x, dp)) {
           r;
         } else {
           Rule(dp, subst_var(d1, x, d2));
         }
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
             switch (d') {
             /* Substitute each previously substituted binding into the
              * fixpoint. */
             | FixF(_) as d =>
               map
               |> Environment.foldo(
                    ((x'', d''), d) => subst_var(d'', x'', d),
                    d,
                  )
             | d => d
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
