[@deriving sexp]
type error =
  | ClosureInsideClosure
  | UnevalOutsideClosure
  | InvalidClosureBody
  | PostprocessedNonHoleInClosure
  | PostprocessedHoleOutsideClosure;

[@deriving sexp]
exception Exception(error);

type t = EnvironmentIdMap.t(ClosureEnvironment.t);

let rec pp_uneval =
        (
          pe: t,
          hii: HoleInstanceInfo_.t,
          env: ClosureEnvironment.t,
          d: DHExp.t,
        )
        : (t, HoleInstanceInfo_.t, DHExp.t) =>
  switch (d) {
  /* Bound variables should be looked up within the closure
     environment. If lookup fails, then variable is not bound. */
  | BoundVar(x) =>
    switch (ClosureEnvironment.lookup(env, x)) {
    | Some(d') => (pe, hii, d')
    | None => (pe, hii, d)
    }

  /* Non-hole expressions: expand recursively */
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | ListNil(_)
  | Triv => (pe, hii, d)
  | Let(dp, d1, d2) =>
    let (pe, hii, d1') = pp_uneval(pe, hii, env, d1);
    let (pe, hii, d2') = pp_uneval(pe, hii, env, d2);
    (pe, hii, Let(dp, d1', d2'));
  | FixF(f, ty, d1) =>
    let (pe, hii, d1') = pp_uneval(pe, hii, env, d1);
    (pe, hii, FixF(f, ty, d1'));
  | Fun(dp, ty, d') =>
    let (pe, hii, d'') = pp_uneval(pe, hii, env, d');
    (pe, hii, Fun(dp, ty, d''));
  | Ap(d1, d2) =>
    let (pe, hii, d1') = pp_uneval(pe, hii, env, d1);
    let (pe, hii, d2') = pp_uneval(pe, hii, env, d2);
    (pe, hii, Ap(d1', d2'));
  | ApBuiltin(f, args) =>
    let (pe, hii, args') =
      List.fold_right(
        (arg, (pe, hii, args)) => {
          let (pe, hii, arg') = pp_uneval(pe, hii, env, arg);
          (pe, hii, [arg', ...args]);
        },
        args,
        (pe, hii, []),
      );
    (pe, hii, ApBuiltin(f, args'));
  | BinBoolOp(op, d1, d2) =>
    let (pe, hii, d1') = pp_uneval(pe, hii, env, d1);
    let (pe, hii, d2') = pp_uneval(pe, hii, env, d2);
    (pe, hii, BinBoolOp(op, d1', d2'));
  | BinIntOp(op, d1, d2) =>
    let (pe, hii, d1') = pp_uneval(pe, hii, env, d1);
    let (pe, hii, d2') = pp_uneval(pe, hii, env, d2);
    (pe, hii, BinIntOp(op, d1', d2'));
  | BinFloatOp(op, d1, d2) =>
    let (pe, hii, d1') = pp_uneval(pe, hii, env, d1);
    let (pe, hii, d2') = pp_uneval(pe, hii, env, d2);
    (pe, hii, BinFloatOp(op, d1', d2'));
  | Cons(d1, d2) =>
    let (pe, hii, d1') = pp_uneval(pe, hii, env, d1);
    let (pe, hii, d2') = pp_uneval(pe, hii, env, d2);
    (pe, hii, Cons(d1', d2'));
  | Inj(ty, side, d') =>
    let (pe, hii, d'') = pp_uneval(pe, hii, env, d');
    (pe, hii, Inj(ty, side, d''));
  | Pair(d1, d2) =>
    let (pe, hii, d1') = pp_uneval(pe, hii, env, d1);
    let (pe, hii, d2') = pp_uneval(pe, hii, env, d2);
    (pe, hii, Pair(d1', d2'));
  | Cast(d', ty1, ty2) =>
    let (pe, hii, d'') = pp_uneval(pe, hii, env, d');
    (pe, hii, Cast(d'', ty1, ty2));
  | FailedCast(d', ty1, ty2) =>
    let (pe, hii, d'') = pp_uneval(pe, hii, env, d');
    (pe, hii, FailedCast(d'', ty1, ty2));
  | InvalidOperation(d', reason) =>
    let (pe, hii, d'') = pp_uneval(pe, hii, env, d');
    (pe, hii, InvalidOperation(d'', reason));
  | ConsistentCase(Case(scrut, rules, i)) =>
    let (pe, hii, scrut') = pp_uneval(pe, hii, env, scrut);
    let (pe, hii, rules') = pp_uneval_rules(pe, hii, env, rules);
    (pe, hii, ConsistentCase(Case(scrut', rules', i)));

  /* Closures shouldn't exist inside other closures */
  | Closure(_) => raise(Exception(ClosureInsideClosure))

  /* Hole expressions:
     - Use the closure environment as the hole environment.
     - Number the hole closure appropriately.
     - Recurse through inner expression (if any).
     */
  | EmptyHole(u, _) =>
    let (hii, i) = HoleInstanceInfo_.add_instance(hii, u, env);
    (pe, hii, Closure(env, EmptyHole(u, i)));
  | NonEmptyHole(reason, u, _, d') =>
    let (pe, hii, d') = pp_uneval(pe, hii, env, d');
    let (hii, i) = HoleInstanceInfo_.add_instance(hii, u, env);
    (pe, hii, Closure(env, NonEmptyHole(reason, u, i, d')));
  | ExpandingKeyword(u, _, kw) =>
    let (hii, i) = HoleInstanceInfo_.add_instance(hii, u, env);
    (pe, hii, Closure(env, ExpandingKeyword(u, i, kw)));
  | FreeVar(u, _, x) =>
    let (hii, i) = HoleInstanceInfo_.add_instance(hii, u, env);
    (pe, hii, Closure(env, FreeVar(u, i, x)));
  | InvalidText(u, _, text) =>
    let (hii, i) = HoleInstanceInfo_.add_instance(hii, u, env);
    (pe, hii, Closure(env, InvalidText(u, i, text)));
  | InconsistentBranches(u, _, Case(scrut, rules, case_i)) =>
    let (pe, hii, scrut) = pp_uneval(pe, hii, env, scrut);
    let (pe, hii, rules) = pp_uneval_rules(pe, hii, env, rules);
    let (hii, i) = HoleInstanceInfo_.add_instance(hii, u, env);
    (
      pe,
      hii,
      Closure(env, InconsistentBranches(u, i, Case(scrut, rules, case_i))),
    );
  }

and pp_uneval_rules =
    (
      pe: t,
      hii: HoleInstanceInfo_.t,
      env: ClosureEnvironment.t,
      rules: list(DHExp.rule),
    )
    : (t, HoleInstanceInfo_.t, list(DHExp.rule)) =>
  List.fold_right(
    (DHExp.Rule(dp, d), (pe, hii, rules)) => {
      let (pe, hii, d') = pp_uneval(pe, hii, env, d);
      (pe, hii, [DHExp.Rule(dp, d'), ...rules]);
    },
    rules,
    (pe, hii, []),
  )

and pp_eval =
    (pe: t, hii: HoleInstanceInfo_.t, d: DHExp.t)
    : (t, HoleInstanceInfo_.t, DHExp.t) =>
  switch (d) {
  /* Non-hole expressions: recurse through subexpressions */
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | ListNil(_)
  | Triv => (pe, hii, d)
  | FixF(f, ty, d1) =>
    let (pe, hii, d1') = pp_eval(pe, hii, d1);
    (pe, hii, FixF(f, ty, d1'));
  | Ap(d1, d2) =>
    let (pe, hii, d1') = pp_eval(pe, hii, d1);
    let (pe, hii, d2') = pp_eval(pe, hii, d2);
    (pe, hii, Ap(d1', d2'));
  | ApBuiltin(f, args) =>
    let (pe, hii, args') =
      List.fold_right(
        (arg, (pe, hii, args)) => {
          let (pe, hii, arg') = pp_eval(pe, hii, arg);
          (pe, hii, [arg', ...args]);
        },
        args,
        (pe, hii, []),
      );
    (pe, hii, ApBuiltin(f, args'));
  | BinBoolOp(op, d1, d2) =>
    let (pe, hii, d1') = pp_eval(pe, hii, d1);
    let (pe, hii, d2') = pp_eval(pe, hii, d2);
    (pe, hii, BinBoolOp(op, d1', d2'));
  | BinIntOp(op, d1, d2) =>
    let (pe, hii, d1') = pp_eval(pe, hii, d1);
    let (pe, hii, d2') = pp_eval(pe, hii, d2);
    (pe, hii, BinIntOp(op, d1', d2'));
  | BinFloatOp(op, d1, d2) =>
    let (pe, hii, d1') = pp_eval(pe, hii, d1);
    let (pe, hii, d2') = pp_eval(pe, hii, d2);
    (pe, hii, BinFloatOp(op, d1', d2'));
  | Cons(d1, d2) =>
    let (pe, hii, d1') = pp_eval(pe, hii, d1);
    let (pe, hii, d2') = pp_eval(pe, hii, d2);
    (pe, hii, Cons(d1', d2'));
  | Inj(ty, side, d') =>
    let (pe, hii, d'') = pp_eval(pe, hii, d');
    (pe, hii, Inj(ty, side, d''));
  | Pair(d1, d2) =>
    let (pe, hii, d1') = pp_eval(pe, hii, d1);
    let (pe, hii, d2') = pp_eval(pe, hii, d2);
    (pe, hii, Pair(d1', d2'));
  | Cast(d', ty1, ty2) =>
    let (pe, hii, d'') = pp_eval(pe, hii, d');
    (pe, hii, Cast(d'', ty1, ty2));
  | FailedCast(d', ty1, ty2) =>
    let (pe, hii, d'') = pp_eval(pe, hii, d');
    (pe, hii, FailedCast(d'', ty1, ty2));
  | InvalidOperation(d', reason) =>
    let (pe, hii, d'') = pp_eval(pe, hii, d');
    (pe, hii, InvalidOperation(d'', reason));

  /* These expression forms should not exist outside closure in evaluated result */
  | BoundVar(_)
  | Let(_)
  | ConsistentCase(_)
  | Fun(_)
  | EmptyHole(_)
  | NonEmptyHole(_)
  | ExpandingKeyword(_)
  | FreeVar(_)
  | InvalidText(_)
  | InconsistentBranches(_) => raise(Exception(UnevalOutsideClosure))

  /* Closure: postprocess environment, then postprocess `d'`.

     Some parts of `d'` may lie inside and outside the evaluation boundary,
     use `pp_eval` and `pp_uneval` as necessary.
     */
  | Closure(env, d) =>
    let (pe, hii, env) = pp_eval_env(pe, hii, env);
    switch (d) {
    /* Non-hole constructs inside closures. */
    | Fun(dp, ty, d) =>
      let (pe, hii, d) = pp_uneval(pe, hii, env, d);
      (pe, hii, Fun(dp, ty, d));
    | Let(dp, d1, d2) =>
      /* d1 should already be evaluated, d2 is not */
      let (pe, hii, d1) = pp_eval(pe, hii, d1);
      let (pe, hii, d2) = pp_uneval(pe, hii, env, d2);
      (pe, hii, Let(dp, d1, d2));
    | ConsistentCase(Case(scrut, rules, i)) =>
      /* scrut should already be evaluated, rule bodies are not */
      let (pe, hii, scrut) = pp_eval(pe, hii, scrut);
      let (pe, hii, rules) = pp_uneval_rules(pe, hii, env, rules);
      (pe, hii, ConsistentCase(Case(scrut, rules, i)));

    /* Hole constructs inside closures.

       `NonEmptyHole` and `InconsistentBranches` have subexpressions that
       lie inside the evaluation boundary, and need to be handled differently
       than in `pp_uneval`. The other hole types don't have any evaluated
       subexpressions and we can use `pp_uneval`.
       */
    | NonEmptyHole(reason, u, _, d) =>
      let (pe, hii, d) = pp_eval(pe, hii, d);
      let (hii, i) = HoleInstanceInfo_.add_instance(hii, u, env);
      (pe, hii, Closure(env, NonEmptyHole(reason, u, i, d)));
    | InconsistentBranches(u, _, Case(scrut, rules, case_i)) =>
      let (pe, hii, scrut) = pp_eval(pe, hii, scrut);
      let (hii, i) = HoleInstanceInfo_.add_instance(hii, u, env);
      (pe, hii, InconsistentBranches(u, i, Case(scrut, rules, case_i)));
    | EmptyHole(_)
    | ExpandingKeyword(_)
    | FreeVar(_)
    | InvalidText(_) => pp_uneval(pe, hii, env, d)

    /* Other expression forms cannot be directly in a closure. */
    | _ => raise(Exception(InvalidClosureBody))
    };
  }

and pp_eval_env =
    (pe: t, hii: HoleInstanceInfo_.t, env: ClosureEnvironment.t)
    : (t, HoleInstanceInfo_.t, ClosureEnvironment.t) => {
  let ei = env |> ClosureEnvironment.id_of;
  switch (pe |> EnvironmentIdMap.find_opt(ei)) {
  | Some(env) => (pe, hii, env)
  | None =>
    let (pe, hii, result_map) =
      Environment.fold(
        ((x, d), (pe, hii, new_env)) => {
          let (pe, hii, d) = pp_eval(pe, hii, d);
          (pe, hii, Environment.extend(new_env, (x, d)));
        },
        (pe, hii, Environment.empty),
        env |> ClosureEnvironment.map_of,
      );
    let env = (ei, result_map);
    (pe |> EnvironmentIdMap.add(ei, env), hii, env);
  };
};

let rec track_children_of_hole =
        (hii: HoleInstanceInfo.t, parent: HoleInstanceParents.t_, d: DHExp.t)
        : HoleInstanceInfo.t =>
  switch (d) {
  | Triv
  | ListNil(_)
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | BoundVar(_) => hii
  | FixF(_, _, d)
  | Fun(_, _, d)
  | Inj(_, _, d)
  | Cast(d, _, _)
  | FailedCast(d, _, _)
  | InvalidOperation(d, _) => track_children_of_hole(hii, parent, d)
  | Let(_, d1, d2)
  | Ap(d1, d2)
  | BinBoolOp(_, d1, d2)
  | BinIntOp(_, d1, d2)
  | BinFloatOp(_, d1, d2)
  | Cons(d1, d2)
  | Pair(d1, d2) =>
    let hii = track_children_of_hole(hii, parent, d1);
    track_children_of_hole(hii, parent, d2);

  | ConsistentCase(Case(scrut, rules, _)) =>
    let hii = track_children_of_hole(hii, parent, scrut);
    track_children_of_hole_rules(hii, parent, rules);

  | ApBuiltin(_, args) =>
    List.fold_right(
      (arg, hii) => track_children_of_hole(hii, parent, arg),
      args,
      hii,
    )

  /* Hole types */
  | NonEmptyHole(_, u, i, d) =>
    let hii = track_children_of_hole(hii, parent, d);
    hii |> HoleInstanceInfo.add_parent((u, i), parent);
  | InconsistentBranches(u, i, Case(scrut, rules, _)) =>
    let hii = track_children_of_hole(hii, parent, scrut);
    let hii = track_children_of_hole_rules(hii, parent, rules);
    hii |> HoleInstanceInfo.add_parent((u, i), parent);
  | EmptyHole(u, i)
  | ExpandingKeyword(u, i, _)
  | FreeVar(u, i, _)
  | InvalidText(u, i, _) =>
    hii |> HoleInstanceInfo.add_parent((u, i), parent)

  /* The only thing that should exist in closures at this point
     are holes. Ignore the hole environment, not necessary for
     parent tracking. */
  | Closure(_, d) => track_children_of_hole(hii, parent, d)
  }

and track_children_of_hole_rules =
    (
      hii: HoleInstanceInfo.t,
      parent: HoleInstanceParents.t_,
      rules: list(DHExp.rule),
    )
    : HoleInstanceInfo.t =>
  List.fold_right(
    (DHExp.Rule(_, d), hii) => track_children_of_hole(hii, parent, d),
    rules,
    hii,
  );

/* Driver for hole parent tracking; iterate through all hole closures
   in the HoleInstanceInfo, and call `track_children_of_hole` on them. */
let track_children = (hii: HoleInstanceInfo.t): HoleInstanceInfo.t =>
  MetaVarMap.fold(
    (u, his, hii) =>
      List.fold_right(
        ((i, (env, _)), hii) =>
          Environment.fold(
            ((x, d), hii) => track_children_of_hole(hii, (x, (u, i)), d),
            hii,
            env |> ClosureEnvironment.map_of,
          ),
        his |> List.mapi((i, hc) => (i, hc)),
        hii,
      ),
    hii,
    hii,
  );

let postprocess = (d: DHExp.t): (HoleInstanceInfo.t, DHExp.t) => {
  /* Substitution and hole numbering postprocessing */
  let (_, hii, d) =
    pp_eval(EnvironmentIdMap.empty, HoleInstanceInfo_.empty, d);

  /* Convert HoleInstanceInfo_.t to HoleInstanceInfo.t */
  let hii = hii |> HoleInstanceInfo_.to_hole_instance_info;

  /* Add special hole acting as top-level expression (to act as parent
     for holes directly in the result) */
  let (u_result, _) = HoleInstance.result;
  let hii =
    MetaVarMap.add(
      u_result,
      [(((-1), Environment.singleton(("", d))), [])],
      hii,
    );

  /* Perform hole parent tracking */
  (hii |> track_children, d);
};
