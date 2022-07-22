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
          hci: HoleInstanceInfo_.t,
          env: ClosureEnvironment.t,
          d: DHExp.t,
        )
        : (t, HoleInstanceInfo_.t, DHExp.t) =>
  switch (d) {
  /* Bound variables should be looked up within the closure
     environment. If lookup fails, then variable is not bound. */
  | BoundVar(x) =>
    switch (ClosureEnvironment.lookup(env, x)) {
    | Some(Indet(d'))
    | Some(BoxedValue(d')) => (pe, hci, d')
    | None => (pe, hci, d)
    }

  /* Non-hole expressions: expand recursively */
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | ListNil(_)
  | Triv => (pe, hci, d)
  | Let(dp, d1, d2) =>
    let (pe, hci, d1') = pp_uneval(pe, hci, env, d1);
    let (pe, hci, d2') = pp_uneval(pe, hci, env, d2);
    (pe, hci, Let(dp, d1', d2'));
  | FixF(f, ty, d1) =>
    let (pe, hci, d1') = pp_uneval(pe, hci, env, d1);
    (pe, hci, FixF(f, ty, d1'));
  | Fun(dp, ty, d') =>
    let (pe, hci, d'') = pp_uneval(pe, hci, env, d');
    (pe, hci, Fun(dp, ty, d''));
  | Ap(d1, d2) =>
    let (pe, hci, d1') = pp_uneval(pe, hci, env, d1);
    let (pe, hci, d2') = pp_uneval(pe, hci, env, d2);
    (pe, hci, Ap(d1', d2'));
  | ApBuiltin(f, args) =>
    let (pe, hci, args') =
      List.fold_right(
        (arg, (pe, hci, args)) => {
          let (pe, hci, arg') = pp_uneval(pe, hci, env, arg);
          (pe, hci, [arg', ...args]);
        },
        args,
        (pe, hci, []),
      );
    (pe, hci, ApBuiltin(f, args'));
  | BinBoolOp(op, d1, d2) =>
    let (pe, hci, d1') = pp_uneval(pe, hci, env, d1);
    let (pe, hci, d2') = pp_uneval(pe, hci, env, d2);
    (pe, hci, BinBoolOp(op, d1', d2'));
  | BinIntOp(op, d1, d2) =>
    let (pe, hci, d1') = pp_uneval(pe, hci, env, d1);
    let (pe, hci, d2') = pp_uneval(pe, hci, env, d2);
    (pe, hci, BinIntOp(op, d1', d2'));
  | BinFloatOp(op, d1, d2) =>
    let (pe, hci, d1') = pp_uneval(pe, hci, env, d1);
    let (pe, hci, d2') = pp_uneval(pe, hci, env, d2);
    (pe, hci, BinFloatOp(op, d1', d2'));
  | Cons(d1, d2) =>
    let (pe, hci, d1') = pp_uneval(pe, hci, env, d1);
    let (pe, hci, d2') = pp_uneval(pe, hci, env, d2);
    (pe, hci, Cons(d1', d2'));
  | Inj(ty, side, d') =>
    let (pe, hci, d'') = pp_uneval(pe, hci, env, d');
    (pe, hci, Inj(ty, side, d''));
  | Pair(d1, d2) =>
    let (pe, hci, d1') = pp_uneval(pe, hci, env, d1);
    let (pe, hci, d2') = pp_uneval(pe, hci, env, d2);
    (pe, hci, Pair(d1', d2'));
  | Cast(d', ty1, ty2) =>
    let (pe, hci, d'') = pp_uneval(pe, hci, env, d');
    (pe, hci, Cast(d'', ty1, ty2));
  | FailedCast(d', ty1, ty2) =>
    let (pe, hci, d'') = pp_uneval(pe, hci, env, d');
    (pe, hci, FailedCast(d'', ty1, ty2));
  | InvalidOperation(d', reason) =>
    let (pe, hci, d'') = pp_uneval(pe, hci, env, d');
    (pe, hci, InvalidOperation(d'', reason));
  | ConsistentCase(Case(scrut, rules, i)) =>
    let (pe, hci, scrut') = pp_uneval(pe, hci, env, scrut);
    let (pe, hci, rules') = pp_uneval_rules(pe, hci, env, rules);
    (pe, hci, ConsistentCase(Case(scrut', rules', i)));

  /* Closures shouldn't exist inside other closures */
  | Closure(_) => raise(Exception(ClosureInsideClosure))

  /* Hole expressions:
     - Use the closure environment as the hole environment.
     - Number the hole closure appropriately.
     - Recurse through inner expression (if any).
     */
  | EmptyHole(u, _) =>
    let (hci, i) = HoleInstanceInfo_.number_hole_closure(hci, u, env);
    (pe, hci, Closure(env, EmptyHole(u, i)));
  | NonEmptyHole(reason, u, _, d') =>
    let (pe, hci, d') = pp_uneval(pe, hci, env, d');
    let (hci, i) = HoleInstanceInfo_.number_hole_closure(hci, u, env);
    (pe, hci, Closure(env, NonEmptyHole(reason, u, i, d')));
  | ExpandingKeyword(u, _, kw) =>
    let (hci, i) = HoleInstanceInfo_.number_hole_closure(hci, u, env);
    (pe, hci, Closure(env, ExpandingKeyword(u, i, kw)));
  | FreeVar(u, _, x) =>
    let (hci, i) = HoleInstanceInfo_.number_hole_closure(hci, u, env);
    (pe, hci, Closure(env, FreeVar(u, i, x)));
  | InvalidText(u, _, text) =>
    let (hci, i) = HoleInstanceInfo_.number_hole_closure(hci, u, env);
    (pe, hci, Closure(env, InvalidText(u, i, text)));
  | InconsistentBranches(u, _, Case(scrut, rules, case_i)) =>
    let (pe, hci, scrut) = pp_uneval(pe, hci, env, scrut);
    let (pe, hci, rules) = pp_uneval_rules(pe, hci, env, rules);
    let (hci, i) = HoleInstanceInfo_.number_hole_closure(hci, u, env);
    (
      pe,
      hci,
      Closure(env, InconsistentBranches(u, i, Case(scrut, rules, case_i))),
    );
  }

and pp_uneval_rules =
    (
      pe: t,
      hci: HoleInstanceInfo_.t,
      env: ClosureEnvironment.t,
      rules: list(DHExp.rule),
    )
    : (t, HoleInstanceInfo_.t, list(DHExp.rule)) =>
  List.fold_right(
    (DHExp.Rule(dp, d), (pe, hci, rules)) => {
      let (pe, hci, d') = pp_uneval(pe, hci, env, d);
      (pe, hci, [DHExp.Rule(dp, d'), ...rules]);
    },
    rules,
    (pe, hci, []),
  )

and pp_eval =
    (pe: t, hci: HoleInstanceInfo_.t, d: DHExp.t)
    : (t, HoleInstanceInfo_.t, DHExp.t) =>
  switch (d) {
  /* Non-hole expressions: recurse through subexpressions */
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | ListNil(_)
  | Triv => (pe, hci, d)
  | FixF(f, ty, d1) =>
    let (pe, hci, d1') = pp_eval(pe, hci, d1);
    (pe, hci, FixF(f, ty, d1'));
  | Ap(d1, d2) =>
    let (pe, hci, d1') = pp_eval(pe, hci, d1);
    let (pe, hci, d2') = pp_eval(pe, hci, d2);
    (pe, hci, Ap(d1', d2'));
  | ApBuiltin(f, args) =>
    let (pe, hci, args') =
      List.fold_right(
        (arg, (pe, hci, args)) => {
          let (pe, hci, arg') = pp_eval(pe, hci, arg);
          (pe, hci, [arg', ...args]);
        },
        args,
        (pe, hci, []),
      );
    (pe, hci, ApBuiltin(f, args'));
  | BinBoolOp(op, d1, d2) =>
    let (pe, hci, d1') = pp_eval(pe, hci, d1);
    let (pe, hci, d2') = pp_eval(pe, hci, d2);
    (pe, hci, BinBoolOp(op, d1', d2'));
  | BinIntOp(op, d1, d2) =>
    let (pe, hci, d1') = pp_eval(pe, hci, d1);
    let (pe, hci, d2') = pp_eval(pe, hci, d2);
    (pe, hci, BinIntOp(op, d1', d2'));
  | BinFloatOp(op, d1, d2) =>
    let (pe, hci, d1') = pp_eval(pe, hci, d1);
    let (pe, hci, d2') = pp_eval(pe, hci, d2);
    (pe, hci, BinFloatOp(op, d1', d2'));
  | Cons(d1, d2) =>
    let (pe, hci, d1') = pp_eval(pe, hci, d1);
    let (pe, hci, d2') = pp_eval(pe, hci, d2);
    (pe, hci, Cons(d1', d2'));
  | Inj(ty, side, d') =>
    let (pe, hci, d'') = pp_eval(pe, hci, d');
    (pe, hci, Inj(ty, side, d''));
  | Pair(d1, d2) =>
    let (pe, hci, d1') = pp_eval(pe, hci, d1);
    let (pe, hci, d2') = pp_eval(pe, hci, d2);
    (pe, hci, Pair(d1', d2'));
  | Cast(d', ty1, ty2) =>
    let (pe, hci, d'') = pp_eval(pe, hci, d');
    (pe, hci, Cast(d'', ty1, ty2));
  | FailedCast(d', ty1, ty2) =>
    let (pe, hci, d'') = pp_eval(pe, hci, d');
    (pe, hci, FailedCast(d'', ty1, ty2));
  | InvalidOperation(d', reason) =>
    let (pe, hci, d'') = pp_eval(pe, hci, d');
    (pe, hci, InvalidOperation(d'', reason));

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
    let (pe, hci, env) = pp_eval_env(pe, hci, env);
    switch (d) {
    /* Non-hole constructs inside closures. */
    | Fun(dp, ty, d) =>
      let (pe, hci, d) = pp_uneval(pe, hci, env, d);
      (pe, hci, Fun(dp, ty, d));
    | Let(dp, d1, d2) =>
      /* d1 should already be evaluated, d2 is not */
      let (pe, hci, d1) = pp_eval(pe, hci, d1);
      let (pe, hci, d2) = pp_uneval(pe, hci, env, d2);
      (pe, hci, Let(dp, d1, d2));
    | ConsistentCase(Case(scrut, rules, i)) =>
      /* scrut should already be evaluated, rule bodies are not */
      let (pe, hci, scrut) = pp_eval(pe, hci, scrut);
      let (pe, hci, rules) = pp_uneval_rules(pe, hci, env, rules);
      (pe, hci, ConsistentCase(Case(scrut, rules, i)));

    /* Hole constructs inside closures.

       `NonEmptyHole` and `InconsistentBranches` have subexpressions that
       lie inside the evaluation boundary, and need to be handled differently
       than in `pp_uneval`. The other hole types don't have any evaluated
       subexpressions and we can use `pp_uneval`.
       */
    | NonEmptyHole(reason, u, _, d) =>
      let (pe, hci, d) = pp_eval(pe, hci, d);
      let (hci, i) = HoleInstanceInfo_.number_hole_closure(hci, u, env);
      (pe, hci, Closure(env, NonEmptyHole(reason, u, i, d)));
    | InconsistentBranches(u, _, Case(scrut, rules, case_i)) =>
      let (pe, hci, scrut) = pp_eval(pe, hci, scrut);
      let (hci, i) = HoleInstanceInfo_.number_hole_closure(hci, u, env);
      (pe, hci, InconsistentBranches(u, i, Case(scrut, rules, case_i)));
    | EmptyHole(_)
    | ExpandingKeyword(_)
    | FreeVar(_)
    | InvalidText(_) => pp_uneval(pe, hci, env, d)

    /* Other expression forms cannot be directly in a closure. */
    | _ => raise(Exception(InvalidClosureBody))
    };
  }

and pp_eval_env =
    (pe: t, hci: HoleInstanceInfo_.t, env: ClosureEnvironment.t)
    : (t, HoleInstanceInfo_.t, ClosureEnvironment.t) => {
  let ei = env |> ClosureEnvironment.id_of;
  switch (pe |> EnvironmentIdMap.find_opt(ei)) {
  | Some(env) => (pe, hci, env)
  | None =>
    let (pe, hci, result_map) =
      VarBstMap.fold(
        ((x, r: EvaluatorResult.t), (pe, hci, new_env)) => {
          let (pe, hci, r: EvaluatorResult.t) =
            switch (r) {
            | BoxedValue(d) =>
              let (pe, hci, d) = pp_eval(pe, hci, d);
              (pe, hci, BoxedValue(d));
            | Indet(d) =>
              let (pe, hci, d) = pp_eval(pe, hci, d);
              (pe, hci, Indet(d));
            };
          (pe, hci, VarBstMap.extend(new_env, (x, r)));
        },
        (pe, hci, VarBstMap.empty),
        env |> ClosureEnvironment.map_of,
      );
    let env = (ei, result_map);
    (pe |> EnvironmentIdMap.add(ei, env), hci, env);
  };
};

let rec track_children_of_hole =
        (hci: HoleInstanceInfo.t, parent: HoleInstanceParents.t_, d: DHExp.t)
        : HoleInstanceInfo.t =>
  switch (d) {
  | Triv
  | ListNil(_)
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | BoundVar(_) => hci
  | FixF(_, _, d)
  | Fun(_, _, d)
  | Inj(_, _, d)
  | Cast(d, _, _)
  | FailedCast(d, _, _)
  | InvalidOperation(d, _) => track_children_of_hole(hci, parent, d)
  | Let(_, d1, d2)
  | Ap(d1, d2)
  | BinBoolOp(_, d1, d2)
  | BinIntOp(_, d1, d2)
  | BinFloatOp(_, d1, d2)
  | Cons(d1, d2)
  | Pair(d1, d2) =>
    let hci = track_children_of_hole(hci, parent, d1);
    track_children_of_hole(hci, parent, d2);

  | ConsistentCase(Case(scrut, rules, _)) =>
    let hci = track_children_of_hole(hci, parent, scrut);
    track_children_of_hole_rules(hci, parent, rules);

  | ApBuiltin(_, args) =>
    List.fold_right(
      (arg, hci) => track_children_of_hole(hci, parent, arg),
      args,
      hci,
    )

  /* Hole types */
  | NonEmptyHole(_, u, i, d) =>
    let hci = track_children_of_hole(hci, parent, d);
    hci |> HoleInstanceInfo.add_parent((u, i), parent);
  | InconsistentBranches(u, i, Case(scrut, rules, _)) =>
    let hci = track_children_of_hole(hci, parent, scrut);
    let hci = track_children_of_hole_rules(hci, parent, rules);
    hci |> HoleInstanceInfo.add_parent((u, i), parent);
  | EmptyHole(u, i)
  | ExpandingKeyword(u, i, _)
  | FreeVar(u, i, _)
  | InvalidText(u, i, _) =>
    hci |> HoleInstanceInfo.add_parent((u, i), parent)

  /* The only thing that should exist in closures at this point
     are holes. Ignore the hole environment, not necessary for
     parent tracking. */
  | Closure(_, d) => track_children_of_hole(hci, parent, d)
  }

and track_children_of_hole_rules =
    (
      hci: HoleInstanceInfo.t,
      parent: HoleInstanceParents.t_,
      rules: list(DHExp.rule),
    )
    : HoleInstanceInfo.t =>
  List.fold_right(
    (DHExp.Rule(_, d), hci) => track_children_of_hole(hci, parent, d),
    rules,
    hci,
  );

/* Driver for hole parent tracking; iterate through all hole closures
   in the HoleInstanceInfo, and call `track_children_of_hole` on them. */
let track_children = (hci: HoleInstanceInfo.t): HoleInstanceInfo.t =>
  MetaVarMap.fold(
    (u, hcs, hci) =>
      List.fold_right(
        ((i, (env, _)), hci) =>
          VarBstMap.fold(
            ((x, r: EvaluatorResult.t), hci) => {
              let d =
                switch (r) {
                | BoxedValue(d) => d
                | Indet(d) => d
                };
              track_children_of_hole(hci, (x, (u, i)), d);
            },
            hci,
            env |> ClosureEnvironment.map_of,
          ),
        hcs |> List.mapi((i, hc) => (i, hc)),
        hci,
      ),
    hci,
    hci,
  );

let postprocess = (d: DHExp.t): (HoleInstanceInfo.t, DHExp.t) => {
  /* Substitution and hole numbering postprocessing */
  let (_, hci, d) =
    pp_eval(EnvironmentIdMap.empty, HoleInstanceInfo_.empty, d);

  /* Convert HoleInstanceInfo_.t to HoleInstanceInfo.t */
  let hci = hci |> HoleInstanceInfo_.to_hole_closure_info;

  /* Add special hole acting as top-level expression (to act as parent
     for holes directly in the result) */
  let (u_result, _) = HoleInstance.result_hc;
  let hci =
    MetaVarMap.add(
      u_result,
      [(((-1), VarBstMap.singleton(("", DHExp.BoxedValue(d)))), [])],
      hci,
    );

  /* Perform hole parent tracking */
  (hci |> track_children, d);
};
