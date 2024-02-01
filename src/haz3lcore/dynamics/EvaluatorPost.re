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

[@deriving sexp]
type error =
  | ClosureInsideClosure
  | FixFOutsideClosureEnv
  | UnevalOutsideClosure
  | InvalidClosureBody
  | PostprocessedNonHoleInClosure
  | PostprocessedHoleOutsideClosure;

[@deriving sexp]
exception Exception(error);

/**
  Postprocess inside evaluation boundary.
 */
let rec pp_eval = (d: DHExp.t): m(DHExp.t) => {
  let (term, rewrap) = DHExp.unwrap(d);
  switch (term) {
  /* Non-hole expressions: recurse through subexpressions */
  | Test(_)
  | Bool(_)
  | Int(_)
  | Float(_)
  | String(_)
  | Constructor(_) => d |> return

  | Seq(d1, d2) =>
    let* d1' = pp_eval(d1);
    let+ d2' = pp_eval(d2);
    Seq(d1', d2') |> rewrap;

  | Filter(f, dbody) =>
    let+ dbody' = pp_eval(dbody);
    Filter(f, dbody') |> rewrap;

  | Ap(d1, d2) =>
    let* d1' = pp_eval(d1);
    let* d2' = pp_eval(d2);
    Ap(d1', d2') |> rewrap |> return;

  | ApBuiltin(f, d1) =>
    let* d1' = pp_eval(d1);
    ApBuiltin(f, d1') |> rewrap |> return;

  | BinOp(op, d1, d2) =>
    let* d1' = pp_eval(d1);
    let* d2' = pp_eval(d2);
    BinOp(op, d1', d2') |> rewrap |> return;

  | BuiltinFun(f) => BuiltinFun(f) |> rewrap |> return

  | Cons(d1, d2) =>
    let* d1' = pp_eval(d1);
    let* d2' = pp_eval(d2);
    Cons(d1', d2') |> rewrap |> return;

  | ListConcat(d1, d2) =>
    let* d1' = pp_eval(d1);
    let* d2' = pp_eval(d2);
    ListConcat(d1', d2') |> rewrap |> return;

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
    ListLit(a, b, c, ds) |> rewrap;

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
    Tuple(ds) |> rewrap;

  | Prj(d, n) =>
    let+ d = pp_eval(d);
    Prj(d, n) |> rewrap;

  | Cast(d', ty1, ty2) =>
    let* d'' = pp_eval(d');
    Cast(d'', ty1, ty2) |> rewrap |> return;

  | FailedCast(d', ty1, ty2) =>
    let* d'' = pp_eval(d');
    FailedCast(d'', ty1, ty2) |> rewrap |> return;

  | InvalidOperation(d', reason) =>
    let* d'' = pp_eval(d');
    InvalidOperation(d'', reason) |> rewrap |> return;

  | If(consistent, c, d1, d2) =>
    let* c' = pp_eval(c);
    let* d1' = pp_eval(d1);
    let* d2' = pp_eval(d2);
    If(consistent, c', d1', d2') |> rewrap |> return;

  // TODO: Add consistent case

  /* These expression forms should not exist outside closure in evaluated result */
  | Var(_)
  | Let(_)
  | Fun(_, _, _, None, _)
  | Match(_)
  | EmptyHole(_)
  | NonEmptyHole(_)
  | ExpandingKeyword(_)
  | FreeVar(_)
  | InvalidText(_) => raise(Exception(UnevalOutsideClosure))

  | FixF(_) => raise(Exception(FixFOutsideClosureEnv))

  /* Closure: postprocess environment, then postprocess `d'`.

     Some parts of `d'` may lie inside and outside the evaluation boundary,
     use `pp_eval` and `pp_uneval` as necessary.
     */
  | Fun(dp, ty, d, Some(env), s) =>
    let* env =
      Util.TimeUtil.measure_time("pp_eval_env/FunClosure", true, () =>
        pp_eval_env(env)
      );
    let* d = pp_uneval(env, d);
    Fun(dp, ty, d, Some(env), s) |> rewrap |> return;

  | Closure(env, d) =>
    let* env =
      Util.TimeUtil.measure_time("pp_eval_env/Closure", true, () =>
        pp_eval_env(env)
      );
    let (term, rewrap) = DHExp.unwrap(d);
    switch (term) {
    /* Non-hole constructs inside closures. */
    | Let(dp, d1, d2) =>
      /* d1 should already be evaluated, d2 is not */
      let* d1 = pp_eval(d1);
      let* d2 = pp_uneval(env, d2);
      Let(dp, d1, d2) |> rewrap |> return;

    | Match(Consistent, scrut, rules) =>
      /* scrut should already be evaluated, rule bodies are not */
      let* scrut =
        Util.TimeUtil.measure_time("pp_eval(scrut)", true, () =>
          pp_eval(scrut)
        );
      let* rules =
        Util.TimeUtil.measure_time("pp_uneval_rules", true, () =>
          pp_uneval_rules(env, rules)
        );
      Match(Consistent, scrut, rules) |> rewrap |> return;

    /* Hole constructs inside closures.

       `NonEmptyHole` and `InconsistentBranches` have subexpressions that
       lie inside the evaluation boundary, and need to be handled differently
       than in `pp_uneval`. The other hole types don't have any evaluated
       subexpressions and we can use `pp_uneval`.
       */
    | NonEmptyHole(reason, u, _, d) =>
      let* d = pp_eval(d);
      let* i = hii_add_instance(u, env);
      Closure(env, NonEmptyHole(reason, u, i, d) |> rewrap)
      |> fresh
      |> return;

    | Match(Inconsistent(u, _), scrut, rules) =>
      let* scrut = pp_eval(scrut);
      let* i = hii_add_instance(u, env);
      Closure(env, Match(Inconsistent(u, i), scrut, rules) |> rewrap)
      |> fresh
      |> return;

    | EmptyHole(_)
    | ExpandingKeyword(_)
    | FreeVar(_)
    | InvalidText(_) => pp_uneval(env, d)

    /* Other expression forms cannot be directly in a closure. */
    | _ => raise(Exception(InvalidClosureBody))
    };
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
             let* d' = {
               let (term, rewrap) = DHExp.unwrap(d);
               switch (term) {
               | FixF(f, ty, d1) =>
                 let+ d1 = pp_uneval(env', d1);
                 FixF(f, ty, d1) |> rewrap;
               | _ => pp_eval(d)
               };
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
and pp_uneval = (env: ClosureEnvironment.t, d: DHExp.t): m(DHExp.t) => {
  let (term, rewrap) = DHExp.unwrap(d);
  switch (term) {
  /* Bound variables should be looked up within the closure
     environment. If lookup fails, then variable is not bound. */
  | Var(x) =>
    switch (ClosureEnvironment.lookup(env, x)) {
    | Some(d') => d' |> return
    | None => d |> return
    }

  /* Non-hole expressions: expand recursively */
  | Bool(_)
  | Int(_)
  | Float(_)
  | String(_)
  | Constructor(_) => d |> return

  | Test(id, d1) =>
    let+ d1' = pp_uneval(env, d1);
    Test(id, d1') |> rewrap;

  | Seq(d1, d2) =>
    let* d1' = pp_uneval(env, d1);
    let+ d2' = pp_uneval(env, d2);
    Seq(d1', d2') |> rewrap;

  | Filter(flt, dbody) =>
    let+ dbody' = pp_uneval(env, dbody);
    Filter(flt, dbody') |> rewrap;
  | Let(dp, d1, d2) =>
    let* d1' = pp_uneval(env, d1);
    let* d2' = pp_uneval(env, d2);
    Let(dp, d1', d2') |> rewrap |> return;

  | FixF(f, ty, d1) =>
    let* d1' = pp_uneval(env, d1);
    FixF(f, ty, d1') |> rewrap |> return;

  | Fun(dp, ty, d', None, s) =>
    let* d'' = pp_uneval(env, d');
    Fun(dp, ty, d'', None, s) |> rewrap |> return;

  | Ap(d1, d2) =>
    let* d1' = pp_uneval(env, d1);
    let* d2' = pp_uneval(env, d2);
    Ap(d1', d2') |> rewrap |> return;

  | ApBuiltin(f, d1) =>
    let* d1' = pp_uneval(env, d1);
    ApBuiltin(f, d1') |> rewrap |> return;
  | BuiltinFun(f) => BuiltinFun(f) |> rewrap |> return

  | BinOp(op, d1, d2) =>
    let* d1' = pp_uneval(env, d1);
    let* d2' = pp_uneval(env, d2);
    BinOp(op, d1', d2') |> rewrap |> return;

  | If(consistent, c, d1, d2) =>
    let* c' = pp_uneval(env, c);
    let* d1' = pp_uneval(env, d1);
    let* d2' = pp_uneval(env, d2);
    If(consistent, c', d1', d2') |> rewrap |> return;

  | Cons(d1, d2) =>
    let* d1' = pp_uneval(env, d1);
    let* d2' = pp_uneval(env, d2);
    Cons(d1', d2') |> rewrap |> return;

  | ListConcat(d1, d2) =>
    let* d1' = pp_uneval(env, d1);
    let* d2' = pp_uneval(env, d2);
    ListConcat(d1', d2') |> rewrap |> return;

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
    ListLit(a, b, c, ds) |> rewrap;

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
    Tuple(ds) |> rewrap;

  | Prj(d, n) =>
    let+ d = pp_uneval(env, d);
    Prj(d, n) |> rewrap;

  | Cast(d', ty1, ty2) =>
    let* d'' = pp_uneval(env, d');
    Cast(d'', ty1, ty2) |> rewrap |> return;

  | FailedCast(d', ty1, ty2) =>
    let* d'' = pp_uneval(env, d');
    FailedCast(d'', ty1, ty2) |> rewrap |> return;

  | InvalidOperation(d', reason) =>
    let* d'' = pp_uneval(env, d');
    InvalidOperation(d'', reason) |> rewrap |> return;

  | Match(Consistent, scrut, rules) =>
    let* scrut' = pp_uneval(env, scrut);
    let* rules' = pp_uneval_rules(env, rules);
    Match(Consistent, scrut', rules') |> rewrap |> return;

  /* Closures shouldn't exist inside other closures */
  | Fun(_, _, _, Some(_), _)
  | Closure(_) => raise(Exception(ClosureInsideClosure))

  /* Hole expressions:
     - Use the closure environment as the hole environment.
     - Number the hole instance appropriately.
     - Recurse through inner expression (if any).
     */
  | EmptyHole(u, _) =>
    let* i = hii_add_instance(u, env);
    Closure(env, EmptyHole(u, i) |> rewrap) |> fresh |> return;

  | NonEmptyHole(reason, u, _, d') =>
    let* d' = pp_uneval(env, d');
    let* i = hii_add_instance(u, env);
    Closure(env, NonEmptyHole(reason, u, i, d') |> rewrap) |> fresh |> return;

  | ExpandingKeyword(u, _, kw) =>
    let* i = hii_add_instance(u, env);
    Closure(env, ExpandingKeyword(u, i, kw) |> rewrap) |> fresh |> return;

  | FreeVar(u, _, x) =>
    let* i = hii_add_instance(u, env);
    Closure(env, FreeVar(u, i, x) |> rewrap) |> fresh |> return;

  | InvalidText(u, _, text) =>
    let* i = hii_add_instance(u, env);
    Closure(env, InvalidText(u, i, text) |> rewrap) |> fresh |> return;

  | Match(Inconsistent(u, _), scrut, rules) =>
    let* scrut = pp_uneval(env, scrut);
    let* rules = pp_uneval_rules(env, rules);
    let* i = hii_add_instance(u, env);
    Closure(env, Match(Inconsistent(u, i), scrut, rules) |> rewrap)
    |> fresh
    |> return;
  };
}

and pp_uneval_rules =
    (env: ClosureEnvironment.t, rules: list((DHPat.t, DHExp.t)))
    : m(list((DHPat.t, DHExp.t))) => {
  rules
  |> List.map(((dp, d)) => {
       let* d' = pp_uneval(env, d);
       (dp, d') |> return;
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
  switch (DHExp.term_of(d)) {
  | Constructor(_)
  | Bool(_)
  | Int(_)
  | Float(_)
  | String(_)
  | BuiltinFun(_)
  | Var(_) => hii
  | Test(_, d)
  | FixF(_, _, d)
  | Fun(_, _, d, _, _)
  | Prj(d, _)
  | Cast(d, _, _)
  | FailedCast(d, _, _)
  | InvalidOperation(d, _) => track_children_of_hole(hii, parent, d)
  | Seq(d1, d2)
  | Let(_, d1, d2)
  | Ap(d1, d2)
  | BinOp(_, d1, d2)
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
  | If(_, c, d1, d2) =>
    let hii = track_children_of_hole(hii, parent, c);
    let hii = track_children_of_hole(hii, parent, d1);
    track_children_of_hole(hii, parent, d2);

  | Match(Consistent, scrut, rules) =>
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
  | Match(Inconsistent(u, i), scrut, rules) =>
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
      rules: list((DHPat.t, DHExp.t)),
    )
    : HoleInstanceInfo.t =>
  List.fold_right(
    ((_, d), hii) => track_children_of_hole(hii, parent, d),
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
