module PpMonad = {
  include Util.StateMonad.Make({
    [@deriving sexp]
    type t = (EnvironmentIdMap.t(ClosureEnvironment.t), HoleInstanceInfo_.t);
  });

  open Syntax;

  let get_pe = get >>| (((pe, _)) => pe);
  let pe_add = (ei, env) =>
    modify(((pe, hii)) => (pe |> EnvironmentIdMap.add(ei, env), hii));

  let hii_add_instance = (u, env) =>
    modify'(((pe, hii)) => {
      let (hii, i) = HoleInstanceInfo_.add_instance(hii, u, env);
      (i, (pe, hii));
    });
};

open PpMonad;
open PpMonad.Syntax;
open DHExp;

type m('a) = PpMonad.t('a);

[@deriving (show({with_path: false}), sexp, yojson)]
type error =
  | ClosureInsideClosure
  | FixFOutsideClosureEnv
  | UnevalOutsideClosure
  | InvalidClosureBody
  | PostprocessedNonHoleInClosure
  | PostprocessedHoleOutsideClosure;

[@deriving (show({with_path: false}), sexp, yojson)]
exception Exception(error);

/**
  Postprocess inside evaluation boundary.
 */
let rec pp_eval = (d: DHExp.t): m(DHExp.t) =>
  switch (d) {
  /* Non-hole expressions: recurse through subexpressions */
  | Test(_)
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | StringLit(_)
  | Constructor(_) => d |> return

  | Sequence(d1, d2) =>
    let* d1' = pp_eval(d1);
    let+ d2' = pp_eval(d2);
    Sequence(d1', d2');

  | Filter(f, dbody) =>
    let+ dbody' = pp_eval(dbody);
    Filter(f, dbody');

  | Ap(d1, d2) =>
    let* d1' = pp_eval(d1);
    let* d2' = pp_eval(d2);
    Ap(d1', d2') |> return;

  | TypAp(d1, ty) =>
    let* d1' = pp_eval(d1);
    TypAp(d1', ty) |> return;

  | ApBuiltin(f, d1) =>
    let* d1' = pp_eval(d1);
    ApBuiltin(f, d1') |> return;

  | BinBoolOp(op, d1, d2) =>
    let* d1' = pp_eval(d1);
    let* d2' = pp_eval(d2);
    BinBoolOp(op, d1', d2') |> return;

  | BuiltinFun(f) => BuiltinFun(f) |> return

  | BinIntOp(op, d1, d2) =>
    let* d1' = pp_eval(d1);
    let* d2' = pp_eval(d2);
    BinIntOp(op, d1', d2') |> return;

  | BinFloatOp(op, d1, d2) =>
    let* d1' = pp_eval(d1);
    let* d2' = pp_eval(d2);
    BinFloatOp(op, d1', d2') |> return;

  | BinStringOp(op, d1, d2) =>
    let* d1' = pp_eval(d1);
    let* d2' = pp_eval(d2);
    BinStringOp(op, d1', d2') |> return;

  | Cons(d1, d2) =>
    let* d1' = pp_eval(d1);
    let* d2' = pp_eval(d2);
    Cons(d1', d2') |> return;

  | ListConcat(d1, d2) =>
    let* d1' = pp_eval(d1);
    let* d2' = pp_eval(d2);
    ListConcat(d1', d2') |> return;

  | ListLit(a, b, c, ds) =>
    let+ ds =
      ds
      |> List.fold_left(
           (ds, d) => {
             let* ds = ds;
             let+ d = pp_eval(d);
             ds @ [d];
           },
           return([]),
         );
    ListLit(a, b, c, ds);

  | Tuple(ds) =>
    let+ ds =
      ds
      |> List.fold_left(
           (ds, d) => {
             let* ds = ds;
             let+ d = pp_eval(d);
             ds @ [d];
           },
           return([]),
         );
    Tuple(ds);

  | Prj(d, n) =>
    let+ d = pp_eval(d);
    Prj(d, n);

  | Cast(d', ty1, ty2) =>
    let* d'' = pp_eval(d');
    Cast(d'', ty1, ty2) |> return;

  | FailedCast(d', ty1, ty2) =>
    let* d'' = pp_eval(d');
    FailedCast(d'', ty1, ty2) |> return;

  | InvalidOperation(d', reason) =>
    let* d'' = pp_eval(d');
    InvalidOperation(d'', reason) |> return;

  | IfThenElse(consistent, c, d1, d2) =>
    let* c' = pp_eval(c);
    let* d1' = pp_eval(d1);
    let* d2' = pp_eval(d2);
    IfThenElse(consistent, c', d1', d2') |> return;

  /* These expression forms should not exist outside closure in evaluated result */
  | BoundVar(_)
  | Let(_)
  | ConsistentCase(_)
  | Fun(_)
  | TypFun(_)
  | EmptyHole(_)
  | NonEmptyHole(_)
  | ExpandingKeyword(_)
  | FreeVar(_)
  | InvalidText(_)
  | InconsistentBranches(_) => raise(Exception(UnevalOutsideClosure))

  | FixF(_) => raise(Exception(FixFOutsideClosureEnv))

  /* Closure: postprocess environment, then postprocess `d'`.

     Some parts of `d'` may lie inside and outside the evaluation boundary,
     use `pp_eval` and `pp_uneval` as necessary.
     */
  | Closure(env, d) =>
    let* env =
      Util.TimeUtil.measure_time("pp_eval_env/Closure", true, () =>
        pp_eval_env(env)
      );
    switch (d) {
    /* Non-hole constructs inside closures. */
    | Fun(dp, ty, d, s) =>
      let* d = pp_uneval(env, d);
      Fun(dp, ty, d, s) |> return;

    | TypFun(tpat, d1) =>
      let* d1' = pp_uneval(env, d1);
      TypFun(tpat, d1') |> return;

    | Let(dp, d1, d2) =>
      /* d1 should already be evaluated, d2 is not */
      let* d1 = pp_eval(d1);
      let* d2 = pp_uneval(env, d2);
      Let(dp, d1, d2) |> return;

    | ConsistentCase(Case(scrut, rules, i)) =>
      /* scrut should already be evaluated, rule bodies are not */
      let* scrut =
        Util.TimeUtil.measure_time("pp_eval(scrut)", true, () =>
          pp_eval(scrut)
        );
      let* rules =
        Util.TimeUtil.measure_time("pp_uneval_rules", true, () =>
          pp_uneval_rules(env, rules)
        );
      ConsistentCase(Case(scrut, rules, i)) |> return;

    /* Hole constructs inside closures.

       `NonEmptyHole` and `InconsistentBranches` have subexpressions that
       lie inside the evaluation boundary, and need to be handled differently
       than in `pp_uneval`. The other hole types don't have any evaluated
       subexpressions and we can use `pp_uneval`.
       */
    | NonEmptyHole(reason, u, _, d) =>
      let* d = pp_eval(d);
      let* i = hii_add_instance(u, env);
      Closure(env, NonEmptyHole(reason, u, i, d)) |> return;

    | InconsistentBranches(u, _, Case(scrut, rules, case_i)) =>
      let* scrut = pp_eval(scrut);
      let* i = hii_add_instance(u, env);
      Closure(env, InconsistentBranches(u, i, Case(scrut, rules, case_i)))
      |> return;

    | EmptyHole(_)
    | ExpandingKeyword(_)
    | FreeVar(_)
    | InvalidText(_) => pp_uneval(env, d)

    /* Other expression forms cannot be directly in a closure. */
    | _ => raise(Exception(InvalidClosureBody))
    };
  }

/* Recurse through environments, using memoized result if available. */
and pp_eval_env = (env: ClosureEnvironment.t): m(ClosureEnvironment.t) => {
  let ei = env |> ClosureEnvironment.id_of;

  let* pe = get_pe;
  switch (pe |> EnvironmentIdMap.find_opt(ei)) {
  | Some(env) => env |> return
  | None =>
    let* env =
      env
      |> ClosureEnvironment.fold(
           ((x, d), env') => {
             let* env' = env';
             let* d' =
               switch (d) {
               | FixF(f, ty, d1) =>
                 let+ d1 = pp_uneval(env', d1);
                 FixF(f, ty, d1);
               | d => pp_eval(d)
               };
             ClosureEnvironment.extend(env', (x, d')) |> return;
           },
           Environment.empty |> ClosureEnvironment.wrap(ei) |> return,
         );

    let* () = pe_add(ei, env);
    env |> return;
  };
}

/**
  Postprocess inside evaluation boundary. Environment should already be
  postprocessed.
 */
and pp_uneval = (env: ClosureEnvironment.t, d: DHExp.t): m(DHExp.t) =>
  switch (d) {
  /* Bound variables should be looked up within the closure
     environment. If lookup fails, then variable is not bound. */
  | BoundVar(x) =>
    switch (ClosureEnvironment.lookup(env, x)) {
    | Some(d') => d' |> return
    | None => d |> return
    }

  /* Non-hole expressions: expand recursively */
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | StringLit(_)
  | Constructor(_) => d |> return

  | Test(id, d1) =>
    let+ d1' = pp_uneval(env, d1);
    Test(id, d1');

  | Sequence(d1, d2) =>
    let* d1' = pp_uneval(env, d1);
    let+ d2' = pp_uneval(env, d2);
    Sequence(d1', d2');

  | Filter(flt, dbody) =>
    let+ dbody' = pp_uneval(env, dbody);
    Filter(flt, dbody');
  | Let(dp, d1, d2) =>
    let* d1' = pp_uneval(env, d1);
    let* d2' = pp_uneval(env, d2);
    Let(dp, d1', d2') |> return;

  | FixF(f, ty, d1) =>
    let* d1' = pp_uneval(env, d1);
    FixF(f, ty, d1') |> return;

  | Fun(dp, ty, d', s) =>
    let* d'' = pp_uneval(env, d');
    Fun(dp, ty, d'', s) |> return;

  | TypFun(tpat, d1) =>
    let* d1' = pp_uneval(env, d1);
    TypFun(tpat, d1') |> return;

  | Ap(d1, d2) =>
    let* d1' = pp_uneval(env, d1);
    let* d2' = pp_uneval(env, d2);
    Ap(d1', d2') |> return;

  | TypAp(d1, ty) =>
    let* d1' = pp_uneval(env, d1);
    TypAp(d1', ty) |> return;

  | ApBuiltin(f, d1) =>
    let* d1' = pp_uneval(env, d1);
    ApBuiltin(f, d1') |> return;

  | BuiltinFun(f) => BuiltinFun(f) |> return

  | BinBoolOp(op, d1, d2) =>
    let* d1' = pp_uneval(env, d1);
    let* d2' = pp_uneval(env, d2);
    BinBoolOp(op, d1', d2') |> return;
  | BinIntOp(op, d1, d2) =>
    let* d1' = pp_uneval(env, d1);
    let* d2' = pp_uneval(env, d2);
    BinIntOp(op, d1', d2') |> return;

  | BinFloatOp(op, d1, d2) =>
    let* d1' = pp_uneval(env, d1);
    let* d2' = pp_uneval(env, d2);
    BinFloatOp(op, d1', d2') |> return;

  | BinStringOp(op, d1, d2) =>
    let* d1' = pp_uneval(env, d1);
    let* d2' = pp_uneval(env, d2);
    BinStringOp(op, d1', d2') |> return;

  | IfThenElse(consistent, c, d1, d2) =>
    let* c' = pp_uneval(env, c);
    let* d1' = pp_uneval(env, d1);
    let* d2' = pp_uneval(env, d2);
    IfThenElse(consistent, c', d1', d2') |> return;

  | Cons(d1, d2) =>
    let* d1' = pp_uneval(env, d1);
    let* d2' = pp_uneval(env, d2);
    Cons(d1', d2') |> return;

  | ListConcat(d1, d2) =>
    let* d1' = pp_uneval(env, d1);
    let* d2' = pp_uneval(env, d2);
    ListConcat(d1', d2') |> return;

  | ListLit(a, b, c, ds) =>
    let+ ds =
      ds
      |> List.fold_left(
           (ds, d) => {
             let* ds = ds;
             let+ d = pp_uneval(env, d);
             ds @ [d];
           },
           return([]),
         );
    ListLit(a, b, c, ds);

  | Tuple(ds) =>
    let+ ds =
      ds
      |> List.fold_left(
           (ds, d) => {
             let* ds = ds;
             let+ d = pp_uneval(env, d);
             ds @ [d];
           },
           return([]),
         );
    Tuple(ds);

  | Prj(d, n) =>
    let+ d = pp_uneval(env, d);
    Prj(d, n);

  | Cast(d', ty1, ty2) =>
    let* d'' = pp_uneval(env, d');
    Cast(d'', ty1, ty2) |> return;

  | FailedCast(d', ty1, ty2) =>
    let* d'' = pp_uneval(env, d');
    FailedCast(d'', ty1, ty2) |> return;

  | InvalidOperation(d', reason) =>
    let* d'' = pp_uneval(env, d');
    InvalidOperation(d'', reason) |> return;

  | ConsistentCase(Case(scrut, rules, i)) =>
    let* scrut' = pp_uneval(env, scrut);
    let* rules' = pp_uneval_rules(env, rules);
    ConsistentCase(Case(scrut', rules', i)) |> return;

  /* Closures shouldn't exist inside other closures */
  | Closure(_) => raise(Exception(ClosureInsideClosure))

  /* Hole expressions:
     - Use the closure environment as the hole environment.
     - Number the hole instance appropriately.
     - Recurse through inner expression (if any).
     */
  | EmptyHole(u, _) =>
    let* i = hii_add_instance(u, env);
    Closure(env, EmptyHole(u, i)) |> return;

  | NonEmptyHole(reason, u, _, d') =>
    let* d' = pp_uneval(env, d');
    let* i = hii_add_instance(u, env);
    Closure(env, NonEmptyHole(reason, u, i, d')) |> return;

  | ExpandingKeyword(u, _, kw) =>
    let* i = hii_add_instance(u, env);
    Closure(env, ExpandingKeyword(u, i, kw)) |> return;

  | FreeVar(u, _, x) =>
    let* i = hii_add_instance(u, env);
    Closure(env, FreeVar(u, i, x)) |> return;

  | InvalidText(u, _, text) =>
    let* i = hii_add_instance(u, env);
    Closure(env, InvalidText(u, i, text)) |> return;

  | InconsistentBranches(u, _, Case(scrut, rules, case_i)) =>
    let* scrut = pp_uneval(env, scrut);
    let* rules = pp_uneval_rules(env, rules);
    let* i = hii_add_instance(u, env);
    Closure(env, InconsistentBranches(u, i, Case(scrut, rules, case_i)))
    |> return;
  }

and pp_uneval_rules =
    (env: ClosureEnvironment.t, rules: list(DHExp.rule))
    : m(list(DHExp.rule)) => {
  rules
  |> List.map((Rule(dp, d)) => {
       let* d' = pp_uneval(env, d);
       Rule(dp, d') |> return;
     })
  |> sequence;
};

/**
  Tracking children of hole instances. A hole instance is a child of another hole
  instance if it exists in the hole environment of the parent.

  This is the second stage of postprocessing, separate from hole numbering and
  substitution, since memoization becomes much more convoluted if these two
  stages are combined.

  This works by simply iterating over all the (postprocessed) hole instance
  environments in the HoleInstanceInfo_.t and looking for "child" holes.
 */
let rec track_children_of_hole =
        (hii: HoleInstanceInfo.t, parent: HoleInstanceParents.t_, d: DHExp.t)
        : HoleInstanceInfo.t =>
  switch (d) {
  | Constructor(_)
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | StringLit(_)
  | BuiltinFun(_)
  | BoundVar(_) => hii
  | Test(_, d)
  | FixF(_, _, d)
  | Fun(_, _, d, _)
  | TypFun(_, d)
  | TypAp(d, _)
  | Prj(d, _)
  | Cast(d, _, _)
  | FailedCast(d, _, _)
  | InvalidOperation(d, _) => track_children_of_hole(hii, parent, d)
  | Sequence(d1, d2)
  | Let(_, d1, d2)
  | Ap(d1, d2)
  | BinBoolOp(_, d1, d2)
  | BinIntOp(_, d1, d2)
  | BinFloatOp(_, d1, d2)
  | BinStringOp(_, d1, d2)
  | Cons(d1, d2) =>
    let hii = track_children_of_hole(hii, parent, d1);
    track_children_of_hole(hii, parent, d2);
  | ListConcat(d1, d2) =>
    let hii = track_children_of_hole(hii, parent, d1);
    track_children_of_hole(hii, parent, d2);

  | ListLit(_, _, _, ds) =>
    List.fold_right(
      (d, hii) => track_children_of_hole(hii, parent, d),
      ds,
      hii,
    )

  | Tuple(ds) =>
    List.fold_right(
      (d, hii) => track_children_of_hole(hii, parent, d),
      ds,
      hii,
    )
  | IfThenElse(_, c, d1, d2) =>
    let hii = track_children_of_hole(hii, parent, c);
    let hii = track_children_of_hole(hii, parent, d1);
    track_children_of_hole(hii, parent, d2);

  | ConsistentCase(Case(scrut, rules, _)) =>
    let hii =
      Util.TimeUtil.measure_time("track_children_of_hole(scrut)", true, () =>
        track_children_of_hole(hii, parent, scrut)
      );
    Util.TimeUtil.measure_time("track_children_of_hole_rules", true, () =>
      track_children_of_hole_rules(hii, parent, rules)
    );

  | ApBuiltin(_, d) => track_children_of_hole(hii, parent, d)

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
  | Filter(_, d)
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

/**
  Driver for hole parent tracking; iterate through all hole instances in the
  [HoleInstanceInfo.t], and call [track_children_of_hole] on them.
 */
let track_children = (hii: HoleInstanceInfo.t): HoleInstanceInfo.t =>
  MetaVarMap.fold(
    (u, his, hii) =>
      List.fold_right(
        ((i, (env, _)), hii) =>
          Environment.foldo(
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
  let ((_, hii), d) =
    Util.TimeUtil.measure_time("pp_eval", true, () =>
      pp_eval(d, (EnvironmentIdMap.empty, HoleInstanceInfo_.empty))
    );

  /* Build hole instance info. */
  let hii =
    Util.TimeUtil.measure_time("to_hii", true, () =>
      hii |> HoleInstanceInfo_.to_hole_instance_info
    );

  /* Add special hole acting as top-level expression (to act as parent
     for holes directly in the result) */
  /* FIXME: Better way to do this? */
  let (u_result, _) = HoleInstance.result;
  let hii =
    MetaVarMap.add(
      u_result,
      [
        (
          ClosureEnvironment.wrap(
            EnvironmentId.invalid,
            Environment.singleton(("", d)),
          ),
          [],
        ),
      ],
      hii,
    );

  let hii =
    Util.TimeUtil.measure_time("track_children", true, () =>
      hii |> track_children
    );

  /* Perform hole parent tracking. */
  (hii, d);
};
