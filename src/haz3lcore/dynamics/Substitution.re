open Util;

/**
 * Whether dp contains the variable x outside of a hole.
 */
let rec binds_var = (x: Var.t, dp: DHPat.t): bool =>
  switch (dp |> Pat.term_of) {
  | EmptyHole
  | MultiHole(_)
  | Wild
  | Invalid(_)
  | Int(_)
  | Float(_)
  | Bool(_)
  | String(_)
  | Label(_)
  | Constructor(_) => false
  | Cast(y, _, _)
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

let rec free_variables = (d: DHExp.t): Sets.StringSet.t => {
  switch (d |> DHExp.term_of) {
  /* Closures: exception */
  | Closure(_, _) => failwith("Called free_variables on a Closure")
  | FixF(_, _, Some(_)) =>
    failwith("Called free_variables on a FixF with an environment")
  | Fun(_, _, Some(_), _) =>
    failwith("Called free_variables on a Fun with an environment")

  /* Variables: add to fv */
  | Var(x) => Sets.StringSet.singleton(x)

  /* Binders: remove bound variables */
  | Let(dp, d1, d2) =>
    let bound_vars = Sets.StringSet.of_list(DHPat.bound_vars(dp));
    let fvs1 = free_variables(d1);
    let fvs2 = free_variables(d2);
    let fvs = Sets.StringSet.union(fvs1, fvs2);
    Sets.StringSet.diff(fvs, bound_vars);
  | FixF(dp, d1, None) =>
    let bound_vars = Sets.StringSet.of_list(DHPat.bound_vars(dp));
    let fvs1 = free_variables(d1);
    Sets.StringSet.diff(fvs1, bound_vars);
  | Fun(dp, d1, None, _) =>
    let bound_vars = Sets.StringSet.of_list(DHPat.bound_vars(dp));
    let fvs1 = free_variables(d1);
    Sets.StringSet.diff(fvs1, bound_vars);

  /* Atomic Forms: no fvs*/
  | EmptyHole
  | Undefined
  | Invalid(_)
  | MultiHole(_)
  | Deferral(_)
  | Bool(_)
  | Int(_)
  | Float(_)
  | String(_)
  | Constructor(_)
  | Label(_)
  | BuiltinFun(_) => Sets.StringSet.empty

  /* Compound Forms: combine fvs */
  | Filter(_, d)
  | Test(d)
  | Cast(d, _, _)
  | FailedCast(d, _, _)
  | TypFun(_, d, _)
  | TypAp(d, _)
  | Probe(d, _)
  | TupLabel(_, d)
  | TyAlias(_, _, d)
  | Parens(d)
  | UnOp(_, d)
  | DynamicErrorHole(d, _) => free_variables(d)

  | Seq(d1, d2)
  | Ap(_, d1, d2)
  | Cons(d1, d2)
  | ListConcat(d1, d2)
  | BinOp(_, d1, d2)
  | Dot(d1, d2) =>
    let fvs1 = free_variables(d1);
    let fvs2 = free_variables(d2);
    Sets.StringSet.union(fvs1, fvs2);

  | If(d1, d2, d3) =>
    let fvs1 = free_variables(d1);
    let fvs2 = free_variables(d2);
    let fvs3 = free_variables(d3);
    Sets.StringSet.union(fvs1, Sets.StringSet.union(fvs2, fvs3));

  | ListLit(ds)
  | Tuple(ds) =>
    ds
    |> List.map(free_variables)
    |> List.fold_left(Sets.StringSet.union, Sets.StringSet.empty)

  | DeferredAp(d, d2) =>
    let fvs1 = free_variables(d);
    let fvs2 =
      d2
      |> List.map(free_variables)
      |> List.fold_left(Sets.StringSet.union, Sets.StringSet.empty);
    Sets.StringSet.union(fvs1, fvs2);

  | Match(d, rules) =>
    let fvs1 = free_variables(d);
    let fvs2 =
      rules
      |> List.map(((p, v)) => {
           let bound_vars = Sets.StringSet.of_list(DHPat.bound_vars(p));
           let fvs = free_variables(v);
           Sets.StringSet.diff(fvs, bound_vars);
         })
      |> List.fold_left(Sets.StringSet.union, Sets.StringSet.empty);
    Sets.StringSet.union(fvs1, fvs2);
  };
};

/* does alpha-renaming to avoid capture. note: in the case where a pattern has multiple
   of the same variable, this will rename all of them */
let rec pattern_capture =
        (p: DHPat.t, fv: Sets.StringSet.t): (DHPat.t, Environment.t) => {
  let (term, rewrap) = DHPat.unwrap(p);
  switch (term) {
  /* Variables: rename if in fv */
  | Var(x) =>
    if (Sets.StringSet.mem(x, fv)) {
      let x' = Var.free_name(x, Sets.StringSet.to_list(fv));
      (
        Var(x') |> rewrap,
        Environment.singleton((x, Var(x') |> Exp.fresh)),
      );
    } else {
      (p, Environment.empty);
    }

  /* Invidisible forms: unchanged */
  | EmptyHole
  | MultiHole(_)
  | Wild
  | Invalid(_)
  | Int(_)
  | Float(_)
  | Bool(_)
  | String(_)
  | Label(_)
  | TupLabel(_)
  | Constructor(_) => (p, Environment.empty)

  /* Compound forms: recursively rename */
  | Parens(p) =>
    let (p', m) = pattern_capture(p, fv);
    (Parens(p') |> rewrap, m);
  | Probe(p, pr) =>
    let (p', m) = pattern_capture(p, fv);
    (Probe(p', pr) |> rewrap, m);
  | Ap(p1, p2) =>
    let (p1', m1) = pattern_capture(p1, fv);
    let (p2', m2) = pattern_capture(p2, fv);
    (Ap(p1', p2') |> rewrap, Environment.union(m1, m2));
  | Cast(p, t1, t2) =>
    let (p', m) = pattern_capture(p, fv);
    (Cast(p', t1, t2) |> rewrap, m);
  | Cons(p1, p2) =>
    let (p1', m1) = pattern_capture(p1, fv);
    let (p2', m2) = pattern_capture(p2, fv);
    (Cons(p1', p2') |> rewrap, Environment.union(m1, m2));
  | ListLit(ps) =>
    let (ps', ms) =
      ps |> List.map(p => pattern_capture(p, fv)) |> ListUtil.unzip;
    (
      ListLit(ps') |> rewrap,
      List.fold_left(Environment.union, Environment.empty, ms),
    );
  | Tuple(ps) =>
    let (ps', ms) =
      ps |> List.map(p => pattern_capture(p, fv)) |> ListUtil.unzip;
    (
      Tuple(ps') |> rewrap,
      List.fold_left(Environment.union, Environment.empty, ms),
    );
  };
};

/* closed substitution [d1/x]d2 */
let rec subst_var =
        (d1: DHExp.t, d1_fv: Sets.StringSet.t, x: Var.t, d2: DHExp.t): DHExp.t => {
  let (term, rewrap) = DHExp.unwrap(d2);
  switch (term) {
  /* Variables: substitute if equal */
  | Var(y) =>
    if (Var.equal(x, y)) {
      d1;
    } else {
      d2;
    }

  /* Binders: alpha-rename to avoid capture if necessary */
  | Let(dp, d3, d4) =>
    let d3' = subst_var(d1, d1_fv, x, d3);
    let (dp', d4') =
      if (binds_var(x, dp)) {
        (dp, d4);
      } else {
        let (dp', alphas) = pattern_capture(dp, d1_fv);
        (dp', subst(alphas, d3) |> subst_var(d1, d1_fv, x));
      };
    Let(dp', d3', d4') |> rewrap;
  | FixF(y, d3, env) =>
    let env' =
      Option.map(
        ClosureEnvironment.update_env(subst_var_env(d1, d1_fv, x)),
        env,
      );
    let (dp', d3') =
      if (binds_var(x, y)) {
        (y, d3);
      } else {
        let (dp', alphas) = pattern_capture(y, d1_fv);
        (dp', subst(alphas, d3) |> subst_var(d1, d1_fv, x));
      };
    FixF(dp', d3', env') |> rewrap;
  | Fun(dp, d3, ty, s) =>
    if (binds_var(x, dp)) {
      Fun(dp, d3, ty, s) |> rewrap;
    } else {
      let (dp', alphas) = pattern_capture(dp, d1_fv);
      let d3' = subst(alphas, d3) |> subst_var(d1, d1_fv, x);
      Fun(dp', d3', ty, s) |> rewrap;
    }

  /* Invidisible forms: unchanged */
  | Invalid(_) => d2
  | Undefined => d2
  | BuiltinFun(_) => d2
  | Bool(_)
  | Int(_)
  | Float(_)
  | String(_)
  | Label(_)
  | Deferral(_) => d2
  | EmptyHole => EmptyHole |> rewrap
  // TODO: handle multihole
  | MultiHole(_d2) => d2 //MultiHole(List.map(subst_var(m, d1, x), ds)) |> rewrap
  | Constructor(_) => d2

  /* Compound forms: recursively substitute */
  | Closure(env, d3) =>
    /* Closure shouldn't appear during substitution (which
       only is called from elaboration currently) */
    let env' =
      ClosureEnvironment.update_env(subst_var_env(d1, d1_fv, x), env);
    let d3' = subst_var(d1, d1_fv, x, d3);
    Closure(env', d3') |> rewrap;
  | Filter(filter, dbody) =>
    let dbody = subst_var(d1, d1_fv, x, dbody);
    let filter = subst_var_filter(d1, d1_fv, x, filter);
    Filter(filter, dbody) |> rewrap;
  | TypFun(tpat, d3, s) =>
    TypFun(tpat, subst_var(d1, d1_fv, x, d3), s) |> rewrap
  | Test(d3) => Test(subst_var(d1, d1_fv, x, d3)) |> rewrap
  | TupLabel(label, d) =>
    TupLabel(label, subst_var(d1, d1_fv, x, d)) |> rewrap
  | UnOp(op, d3) =>
    let d3 = subst_var(d1, d1_fv, x, d3);
    UnOp(op, d3) |> rewrap;
  | Cast(d, ty1, ty2) =>
    let d' = subst_var(d1, d1_fv, x, d);
    Cast(d', ty1, ty2) |> rewrap;
  | FailedCast(d, ty1, ty2) =>
    let d' = subst_var(d1, d1_fv, x, d);
    FailedCast(d', ty1, ty2) |> rewrap;
  | DynamicErrorHole(d, err) =>
    let d' = subst_var(d1, d1_fv, x, d);
    DynamicErrorHole(d', err) |> rewrap;
  | TyAlias(tp, ut, d4) =>
    let d4' = subst_var(d1, d1_fv, x, d4);
    TyAlias(tp, ut, d4') |> rewrap;
  | Parens(d4) =>
    let d4' = subst_var(d1, d1_fv, x, d4);
    Parens(d4') |> rewrap;
  | Probe(d4, pr) =>
    let d4' = subst_var(d1, d1_fv, x, d4);
    Probe(d4', pr) |> rewrap;
  | TypAp(d3, ut) =>
    let d3 = subst_var(d1, d1_fv, x, d3);
    TypAp(d3, ut) |> rewrap;

  | Seq(d3, d4) =>
    let d3 = subst_var(d1, d1_fv, x, d3);
    let d4 = subst_var(d1, d1_fv, x, d4);
    Seq(d3, d4) |> rewrap;
  | Ap(dir, d3, d4) =>
    let d3 = subst_var(d1, d1_fv, x, d3);
    let d4 = subst_var(d1, d1_fv, x, d4);
    Ap(dir, d3, d4) |> rewrap;
  | Cons(d3, d4) =>
    let d3 = subst_var(d1, d1_fv, x, d3);
    let d4 = subst_var(d1, d1_fv, x, d4);
    Cons(d3, d4) |> rewrap;
  | ListConcat(d3, d4) =>
    let d3 = subst_var(d1, d1_fv, x, d3);
    let d4 = subst_var(d1, d1_fv, x, d4);
    ListConcat(d3, d4) |> rewrap;
  | Dot(d3, d4) =>
    let d3 = subst_var(d1, d1_fv, x, d3);
    let d4 = subst_var(d1, d1_fv, x, d4);
    Dot(d3, d4) |> rewrap;
  | BinOp(op, d3, d4) =>
    let d3 = subst_var(d1, d1_fv, x, d3);
    let d4 = subst_var(d1, d1_fv, x, d4);
    BinOp(op, d3, d4) |> rewrap;

  | If(d4, d5, d6) =>
    let d4' = subst_var(d1, d1_fv, x, d4);
    let d5' = subst_var(d1, d1_fv, x, d5);
    let d6' = subst_var(d1, d1_fv, x, d6);
    If(d4', d5', d6') |> rewrap;

  | ListLit(ds) => ListLit(List.map(subst_var(d1, d1_fv, x), ds)) |> rewrap
  | Tuple(ds) => Tuple(List.map(subst_var(d1, d1_fv, x), ds)) |> rewrap

  | DeferredAp(d3, d4s) =>
    let d3 = subst_var(d1, d1_fv, x, d3);
    let d4s = List.map(subst_var(d1, d1_fv, x), d4s);
    DeferredAp(d3, d4s) |> rewrap;
  | Match(ds, rules) =>
    let ds = subst_var(d1, d1_fv, x, ds);
    let rules =
      List.map(
        ((p, v)) =>
          if (binds_var(x, p)) {
            (p, v);
          } else {
            (p, subst_var(d1, d1_fv, x, v));
          },
        rules,
      );
    Match(ds, rules) |> rewrap;
  };
}

and subst_var_env =
    (d1: DHExp.t, d1_fv, x: Var.t, env: Environment.t): Environment.t => {
  Environment.foldo(
    ((x', d': DHExp.t), map) => {
      let d' =
        switch (DHExp.term_of(d')) {
        /* Substitute each previously substituted binding into the
         * fixpoint. */
        | FixF(_) =>
          map
          |> Environment.foldo(
               ((x'', d''), d) =>
                 subst_var(d'', free_variables(d''), x'', d),
               d',
             )
        | _ => d'
        };

      /* Substitute. */
      let d' = subst_var(d1, d1_fv, x, d');
      Environment.extend(map, (x', d'));
    },
    Environment.empty,
    env,
  );
}

and subst_var_filter =
    (d1: DHExp.t, d1_fv, x: Var.t, flt: TermBase.StepperFilterKind.t)
    : TermBase.StepperFilterKind.t => {
  flt |> TermBase.StepperFilterKind.map(subst_var(d1, d1_fv, x));
}

and subst = (env: Environment.t, d: DHExp.t): DHExp.t =>
  env
  |> Environment.foldo(
       (xd: (Var.t, DHExp.t), d2) => {
         let (x, d1) = xd;
         let d1_fv = free_variables(d1);
         subst_var(d1, d1_fv, x, d2);
       },
       d,
     );
