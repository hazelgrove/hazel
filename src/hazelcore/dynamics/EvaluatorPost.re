module PpMonad = {
  include StateMonad.Make({
    [@deriving sexp]
    type t = (EnvironmentIdMap.t(ClosureEnvironment.t), HoleInstanceInfo_.t);
  });

  open Syntax;

  let modify_pe = f => modify(((pe, hii)) => (pe |> f, hii));
  /* let modify_hii = f => modify(((pe, hii)) => (pe, hii |> f)); */

  let get_pe = get >>| (((pe, _)) => pe);
  /* let get_hii = get >>| (((_, hii)) => hii); */

  let pe_add = (ei, env) => modify_pe(EnvironmentIdMap.add(ei, env));

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
  | UnevalOutsideClosure
  | InvalidClosureBody
  | PostprocessedNonHoleInClosure
  | PostprocessedHoleOutsideClosure;

[@deriving sexp]
exception Exception(error);

/**
  Postprocess inside evaluation boundary.
 */
let rec pp_eval = (d: DHExp.t): m(DHExp.t) =>
  switch (d) {
  /* Non-hole expressions: recurse through subexpressions */
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | ListNil(_)
  | Triv => d |> return

  | FixF(f, ty, d1) =>
    let* d1' = pp_eval(d1);
    FixF(f, ty, d1') |> return;

  | Ap(d1, d2) =>
    let* d1' = pp_eval(d1);
    let* d2' = pp_eval(d2);
    Ap(d1', d2') |> return;

  | ApBuiltin(f, args) =>
    let* args' = args |> List.map(pp_eval) |> sequence;
    ApBuiltin(f, args') |> return;

  | BinBoolOp(op, d1, d2) =>
    let* d1' = pp_eval(d1);
    let* d2' = pp_eval(d2);
    BinBoolOp(op, d1', d2') |> return;

  | BinIntOp(op, d1, d2) =>
    let* d1' = pp_eval(d1);
    let* d2' = pp_eval(d2);
    BinIntOp(op, d1', d2') |> return;

  | BinFloatOp(op, d1, d2) =>
    let* d1' = pp_eval(d1);
    let* d2' = pp_eval(d2);
    BinFloatOp(op, d1', d2') |> return;

  | Cons(d1, d2) =>
    let* d1' = pp_eval(d1);
    let* d2' = pp_eval(d2);
    Cons(d1', d2') |> return;

  | Inj(ty, side, d') =>
    let* d'' = pp_eval(d');
    Inj(ty, side, d'') |> return;

  | Pair(d1, d2) =>
    let* d1' = pp_eval(d1);
    let* d2' = pp_eval(d2);
    Pair(d1', d2') |> return;

  | Cast(d', ty1, ty2) =>
    let* d'' = pp_eval(d');
    Cast(d'', ty1, ty2) |> return;

  | FailedCast(d', ty1, ty2) =>
    let* d'' = pp_eval(d');
    FailedCast(d'', ty1, ty2) |> return;

  | InvalidOperation(d', reason) =>
    let* d'' = pp_eval(d');
    InvalidOperation(d'', reason) |> return;

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
    let* env = pp_eval_env(env);
    switch (d) {
    /* Non-hole constructs inside closures. */
    | Fun(dp, ty, d) =>
      let* d = pp_uneval(env, d);
      Fun(dp, ty, d) |> return;

    | Let(dp, d1, d2) =>
      /* d1 should already be evaluated, d2 is not */
      let* d1 = pp_eval(d1);
      let* d2 = pp_uneval(env, d2);
      Let(dp, d1, d2) |> return;

    | ConsistentCase(Case(scrut, rules, i)) =>
      /* scrut should already be evaluated, rule bodies are not */
      let* scrut = pp_eval(scrut);
      let* rules = pp_uneval_rules(env, rules);
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
      InconsistentBranches(u, i, Case(scrut, rules, case_i)) |> return;

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
  /* FIXME: Have a ClosureEnvironment.update_keep_id or something? */
  let ei = env |> ClosureEnvironment.id_of;

  let* pe = get_pe;
  switch (pe |> EnvironmentIdMap.find_opt(ei)) {
  | Some(env) => env |> return
  | None =>
    let* env_bindings =
      env
      |> ClosureEnvironment.to_list
      |> List.map(((x, d)) => {
           let* d' = pp_eval(d);
           (x, d') |> return;
         })
      |> sequence;
    let env =
      env_bindings |> Environment.of_list |> ClosureEnvironment.wrap(ei);

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
  | ListNil(_)
  | Triv => d |> return

  | Let(dp, d1, d2) =>
    let* d1' = pp_uneval(env, d1);
    let* d2' = pp_uneval(env, d2);
    Let(dp, d1', d2') |> return;

  | FixF(f, ty, d1) =>
    let* d1' = pp_uneval(env, d1);
    FixF(f, ty, d1') |> return;

  | Fun(dp, ty, d') =>
    let* d'' = pp_uneval(env, d');
    Fun(dp, ty, d'') |> return;

  | Ap(d1, d2) =>
    let* d1' = pp_uneval(env, d1);
    let* d2' = pp_uneval(env, d2);
    Ap(d1', d2') |> return;

  | ApBuiltin(f, args) =>
    let* args' = args |> List.map(pp_uneval(env)) |> sequence;
    ApBuiltin(f, args') |> return;

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

  | Cons(d1, d2) =>
    let* d1' = pp_uneval(env, d1);
    let* d2' = pp_uneval(env, d2);
    Cons(d1', d2') |> return;

  | Inj(ty, side, d') =>
    let* d'' = pp_uneval(env, d');
    Inj(ty, side, d'') |> return;

  | Pair(d1, d2) =>
    let* d1' = pp_uneval(env, d1);
    let* d2' = pp_uneval(env, d2);
    Pair(d1', d2') |> return;

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

/**
  Driver for hole parent tracking; iterate through all hole instances in the
  [HoleInstanceInfo.t], and call [track_children_of_hole] on them.
 */
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
  let ((_, hii), d) =
    pp_eval(d, (EnvironmentIdMap.empty, HoleInstanceInfo_.empty));

  /* Build hole instance info. */
  let hii = hii |> HoleInstanceInfo_.to_hole_instance_info;

  /* Add special hole acting as top-level expression (to act as parent
     for holes directly in the result) */
  /* FIXME: Better way to do this? */
  let (u_result, _) = HoleInstance.result;
  let hii =
    MetaVarMap.add(
      u_result,
      /* FIXME: Don't hardcode -1. */
      [(ClosureEnvironment.wrap(-1, Environment.singleton(("", d))), [])],
      hii,
    );

  /* Perform hole parent tracking. */
  (hii |> track_children, d);
};
