open Sexplib.Std;
open EvaluatorMonad;
open EvaluatorMonad.Syntax;

module EvalCtx = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Mark
    | Closure
    | Filter
    | Sequence
    | Let
    | Ap1
    | Ap2
    | BinBoolOp1
    | BinBoolOp2
    | BinIntOp1
    | BinIntOp2
    | BinFloatOp1
    | BinFloatOp2
    | BinStringOp1
    | BinStringOp2
    | Tuple(int)
    | ListLit(int)
    | Cons1
    | Cons2
    | Prj
    | Inj
    | NonEmptyHole
    | Cast
    | FailedCast
    | InvalidOperation
    | ConsistentCase
    | InconsistentBranches;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Mark
    | Closure(ClosureEnvironment.t, t)
    | Filter(DHExp.Filter.t, t)
    | Sequence(t, DHExp.t)
    | Let(DHPat.t, t, DHExp.t)
    | Ap1(t, DHExp.t)
    | Ap2(DHExp.t, t)
    | BinBoolOp1(DHExp.BinBoolOp.t, t, DHExp.t)
    | BinBoolOp2(DHExp.BinBoolOp.t, DHExp.t, t)
    | BinIntOp1(DHExp.BinIntOp.t, t, DHExp.t)
    | BinIntOp2(DHExp.BinIntOp.t, DHExp.t, t)
    | BinFloatOp1(DHExp.BinFloatOp.t, t, DHExp.t)
    | BinFloatOp2(DHExp.BinFloatOp.t, DHExp.t, t)
    | BinStringOp1(DHExp.BinStringOp.t, t, DHExp.t)
    | BinStringOp2(DHExp.BinStringOp.t, DHExp.t, t)
    | Tuple(t, (list(DHExp.t), list(DHExp.t)))
    | ListLit(
        MetaVar.t,
        MetaVarInst.t,
        ListErrStatus.t,
        Typ.t,
        t,
        (list(DHExp.t), list(DHExp.t)),
      )
    | Cons1(t, DHExp.t)
    | Cons2(DHExp.t, t)
    | Prj(t, int)
    | Inj(Typ.t, InjSide.t, t)
    | NonEmptyHole(ErrStatus.HoleReason.t, MetaVar.t, HoleInstanceId.t, t)
    | Cast(t, Typ.t, Typ.t)
    | FailedCast(t, Typ.t, Typ.t)
    | InvalidOperation(t, InvalidOperationError.t)
    | ConsistentCase(case)
    | InconsistentBranches(MetaVar.t, HoleInstanceId.t, case)
  and case =
    | Case(t, list(rule), int)
  and rule = DHExp.rule;
};

let unbox: EvaluatorResult.t => DHExp.t =
  fun
  | BoxedValue(d)
  | Indet(d) => d;

/**
  Alias for EvaluatorMonad.
 */
type m('a) = EvaluatorMonad.t('a);

module DecomposeResult = {
  type t = {
    env: ClosureEnvironment.t,
    flt: DHExp.FilterEnvironment.t,
    ctx: EvalCtx.t,
    exp: DHExp.t,
  };

  let mk = (env, flt, ctx, exp) => {env, flt, ctx, exp};
};

module EvalObj = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    env: ClosureEnvironment.t,
    act: DHExp.FilterAction.t,
    ctx: EvalCtx.t,
    exp: DHExp.t,
  };

  let mk = (env, act, ctx, exp) => {env, act, ctx, exp};

  let init = (exp: DHExp.t): t => {
    let (env, _) =
      Builtins.Pervasives.builtins_as_environment
      |> ClosureEnvironment.of_environment
      |> EvaluatorState.with_eig(_, EvaluatorState.init);
    {env, ctx: Mark, exp, act: Step};
  };

  let get_ctx = (obj: t): EvalCtx.t => obj.ctx;
  let get_exp = (obj: t): DHExp.t => obj.exp;

  let rec unwrap = (obj: t, sel: EvalCtx.cls): option(t) => {
    switch (sel, obj.ctx) {
    | (Mark, _) =>
      print_endline(
        "Mark does not match with "
        ++ Sexplib.Sexp.to_string_hum(EvalCtx.sexp_of_t(obj.ctx)),
      );
      raise(EvaluatorError.Exception(StepDoesNotMatch));
    | (NonEmptyHole, NonEmptyHole(_, _, _, c))
    | (Closure, Closure(_, c))
    | (Filter, Filter(_, c))
    | (Sequence, Sequence(c, _))
    | (Let, Let(_, c, _))
    | (Ap1, Ap1(c, _))
    | (Ap2, Ap2(_, c))
    | (BinBoolOp1, BinBoolOp1(_, c, _))
    | (BinBoolOp2, BinBoolOp2(_, _, c))
    | (BinIntOp1, BinIntOp1(_, c, _))
    | (BinIntOp2, BinIntOp2(_, _, c))
    | (BinFloatOp1, BinFloatOp1(_, c, _))
    | (BinFloatOp2, BinFloatOp2(_, _, c))
    | (BinStringOp1, BinStringOp1(_, c, _))
    | (BinStringOp2, BinStringOp2(_, _, c))
    | (Cons1, Cons1(c, _))
    | (Cons2, Cons2(_, c))
    | (Prj, Prj(c, _))
    | (Inj, Inj(_, _, c)) => Some({...obj, ctx: c})
    | (Tuple(n), Tuple(c, (ld, _))) =>
      if (List.length(ld) == n) {
        Some({...obj, ctx: c});
      } else {
        None;
      }
    | (ListLit(n), ListLit(_, _, _, _, c, (ld, _))) =>
      if (List.length(ld) == n) {
        Some({...obj, ctx: c});
      } else {
        None;
      }
    | (InconsistentBranches, InconsistentBranches(_, _, Case(scrut, _, _))) =>
      Some({...obj, ctx: scrut})
    | (ConsistentCase, ConsistentCase(Case(scrut, _, _))) =>
      Some({...obj, ctx: scrut})
    | (Cast, Cast(c, _, _))
    | (FailedCast, FailedCast(c, _, _)) => Some({...obj, ctx: c})
    | (Ap1, Ap2(_, _))
    | (Ap2, Ap1(_, _))
    | (BinBoolOp1, BinBoolOp2(_))
    | (BinBoolOp2, BinBoolOp1(_))
    | (BinIntOp1, BinIntOp2(_))
    | (BinIntOp2, BinIntOp1(_))
    | (BinFloatOp1, BinFloatOp2(_))
    | (BinFloatOp2, BinFloatOp1(_))
    | (BinStringOp1, BinStringOp2(_))
    | (BinStringOp2, BinStringOp1(_))
    | (Cons1, Cons2(_))
    | (Cons2, Cons1(_)) => None
    | (Closure, _) => Some(obj)
    | (tag, Closure(_, c)) => unwrap({...obj, ctx: c}, tag)
    | (Filter, _) => Some(obj)
    | (tag, Filter(_, c)) => unwrap({...obj, ctx: c}, tag)
    | (Cast, _) => Some(obj)
    | (tag, Cast(c, _, _)) => unwrap({...obj, ctx: c}, tag)
    | (tag, ctx) =>
      print_endline(
        Sexplib.Sexp.to_string_hum(EvalCtx.sexp_of_cls(tag))
        ++ " does not match with "
        ++ Sexplib.Sexp.to_string_hum(EvalCtx.sexp_of_t(ctx)),
      );
      raise(EvaluatorError.Exception(StepDoesNotMatch));
    };
  };
};

let rec decompose =
        (
          env: ClosureEnvironment.t,
          fenv: DHExp.FilterEnvironment.t,
          d: DHExp.t,
        )
        : m(list(DecomposeResult.t)) => {
  let wrap = (fctx: EvalCtx.t => EvalCtx.t, ld: list(DecomposeResult.t)) =>
    List.map((obj: DecomposeResult.t) => {...obj, ctx: fctx(obj.ctx)}, ld);

  let go = (dcs: list((DHExp.t, EvalCtx.t => EvalCtx.t))) => {
    List.fold_left(
      (rc, (d, fc)) => {
        let* c = decompose(env, fenv, d);
        let* rc = rc;
        rc @ wrap(fc, c) |> return;
      },
      return([]),
      dcs,
    );
  };

  let mk = DecomposeResult.mk;

  switch (d) {
  | TestLit(_)
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | StringLit(_)
  | Closure(_, Filter(_, Fun(_)))
  | Tag(_)
  | FreeVar(_)
  | InvalidText(_)
  | EmptyHole(_)
  | ExpandingKeyword(_) => [] |> return
  | Closure(_, Fun(_))
  | Fun(_)
  | ApBuiltin(_)
  | FixF(_, _, _)
  | BoundVar(_) => [mk(env, fenv, Mark, d)] |> return
  | Ap(d1, d2) =>
    let* sc = go([(d1, c => Ap1(c, d2)), (d2, c => Ap2(d1, c))]);
    [mk(env, fenv, Mark, d), ...sc] |> return;
  | Closure(env', d) =>
    let* env = env |> ClosureEnvironment.union(env') |> with_eig;
    let* ld = decompose(env, fenv, d);
    wrap(c => Closure(env', c), ld) |> return;
  | Filter(f, d) =>
    let fenv' = DHExp.FilterEnvironment.extends(f, fenv);
    let* ld = decompose(env, fenv', d);
    wrap(c => Filter(f, c), ld) |> return;
  | Cast(d, ty, ty') =>
    let* ld = decompose(env, fenv, d);
    wrap(c => Cast(c, ty, ty'), ld) |> return;
  | NonEmptyHole(reason, u, i, d1) =>
    go([(d1, c => NonEmptyHole(reason, u, i, c))])
  | BinBoolOp(op, d1, d2) =>
    go([
      (d1, c => BinBoolOp1(op, c, d2)),
      (d2, c => BinBoolOp2(op, d1, c)),
    ])
  | BinIntOp(op, d1, d2) =>
    let* sc =
      go([
        (d1, c => BinIntOp1(op, c, d2)),
        (d2, c => BinIntOp2(op, d1, c)),
      ]);
    [mk(env, fenv, Mark, d), ...sc] |> return;
  | BinFloatOp(op, d1, d2) =>
    go([
      (d1, c => BinFloatOp1(op, c, d2)),
      (d2, c => BinFloatOp2(op, d1, c)),
    ])
  | BinStringOp(op, d1, d2) =>
    go([
      (d1, c => BinStringOp1(op, c, d2)),
      (d2, c => BinStringOp2(op, d1, c)),
    ])
  | Cons(d1, d2) =>
    go([(d1, c => Cons1(c, d2)), (d2, c => Cons2(d1, c))])
  | FailedCast(d1, ty1, ty2) => go([(d1, c => FailedCast(c, ty1, ty2))])
  | Tuple(ds) =>
    let rec walk = (ld, rd, rc) =>
      switch (rd) {
      | [] => rc
      | [hd, ...tl] =>
        let rc = rc @ [(hd, (c => EvalCtx.Tuple(c, (ld, tl))))];
        walk(ld @ [hd], tl, rc);
      };
    go(walk([], ds, []));
  | ListLit(m, i, e, t, ds) =>
    let rec walk = (ld, rd, rc) =>
      switch (rd) {
      | [] => rc
      | [hd, ...tl] =>
        let rc =
          rc @ [(hd, (c => EvalCtx.ListLit(m, i, e, t, c, (ld, tl))))];
        walk(ld @ [hd], tl, rc);
      };
    go(walk([], ds, []));
  | Sequence(d1, d2) => go([(d1, c => Sequence(c, d2))])
  | Let(dp, d1, d2) => go([(d1, c => Let(dp, c, d2))])
  | Prj(d, n) => go([(d, c => Prj(c, n))])
  | Inj(ty, side, d1) => go([(d1, c => Inj(ty, side, c))])
  | InvalidOperation(d1, err) => go([(d1, c => InvalidOperation(c, err))])
  | ConsistentCase(Case(d1, rule, n)) =>
    go([(d1, c => ConsistentCase(Case(c, rule, n)))])
  | InconsistentBranches(u, i, Case(d1, rule, n)) =>
    go([(d1, c => InconsistentBranches(u, i, Case(c, rule, n)))])
  };
};

let rec compose = (ctx: EvalCtx.t, d: DHExp.t): DHExp.t => {
  switch (ctx) {
  | Mark => d
  | Closure(env, ctx) => Closure(env, compose(ctx, d))
  | Filter(f, ctx) => Filter(f, compose(ctx, d))
  | Sequence(ctx, d2) => Sequence(compose(ctx, d), d2)
  | Ap1(ctx1, d1) => Ap(compose(ctx1, d), d1)
  | Ap2(d1, ctx1) => Ap(d1, compose(ctx1, d))
  | BinBoolOp1(op, ctx1, d1) => BinBoolOp(op, compose(ctx1, d), d1)
  | BinBoolOp2(op, d1, ctx1) => BinBoolOp(op, d1, compose(ctx1, d))
  | BinIntOp1(op, ctx1, d1) => BinIntOp(op, compose(ctx1, d), d1)
  | BinIntOp2(op, d1, ctx1) => BinIntOp(op, d1, compose(ctx1, d))
  | BinFloatOp1(op, ctx1, d1) => BinFloatOp(op, compose(ctx1, d), d1)
  | BinFloatOp2(op, d1, ctx1) => BinFloatOp(op, d1, compose(ctx1, d))
  | BinStringOp1(op, ctx1, d1) => BinStringOp(op, compose(ctx1, d), d1)
  | BinStringOp2(op, d1, ctx1) => BinStringOp(op, d1, compose(ctx1, d))
  | Cons1(ctx1, d1) => Cons(compose(ctx1, d), d1)
  | Cons2(d1, ctx1) => Cons(d1, compose(ctx1, d))
  | Tuple(ctx, (ld, rd)) => Tuple(ld @ [compose(ctx, d), ...rd])
  | ListLit(m, i, e, t, ctx, (ld, rd)) =>
    ListLit(m, i, e, t, ld @ [compose(ctx, d), ...rd])
  | Let(dp, ctx1, d1) => Let(dp, compose(ctx1, d), d1)
  | Prj(ctx, n) => Prj(compose(ctx, d), n)
  | Inj(ty, side, ctx1) => Inj(ty, side, compose(ctx1, d))
  | Cast(ctx1, ty1, ty2) => Cast(compose(ctx1, d), ty1, ty2)
  | FailedCast(ctx1, ty1, ty2) => FailedCast(compose(ctx1, d), ty1, ty2)
  | InvalidOperation(ctx1, err) => InvalidOperation(compose(ctx1, d), err)
  | NonEmptyHole(reason, u, i, ctx1) =>
    NonEmptyHole(reason, u, i, compose(ctx1, d))
  | ConsistentCase(Case(ctx1, rule, n)) =>
    ConsistentCase(Case(compose(ctx1, d), rule, n))
  | InconsistentBranches(u, i, Case(ctx1, rule, n)) =>
    InconsistentBranches(u, i, Case(compose(ctx1, d), rule, n))
  };
};

let step = (~pause: bool, obj: EvalObj.t): m(EvaluatorResult.t) => {
  let* r =
    if (pause) {
      EvaluatorResult.Indet(obj.exp) |> return;
    } else {
      Evaluator.evaluate_closure(obj.env, obj.exp);
    };
  let d = compose(obj.ctx, unbox(r));
  switch (r) {
  | BoxedValue(_) => EvaluatorResult.BoxedValue(d) |> return
  | Indet(_) => EvaluatorResult.Indet(d) |> return
  };
};

let init = (d: DHExp.t) => {
  print_endline("======== init BEGIN =========");
  let (es, r) = step(~pause=true, EvalObj.init(d), EvaluatorState.init);
  print_endline("======== init END =========");
  r
  |> EvaluatorResult.sexp_of_t
  |> Sexplib.Sexp.to_string_hum
  |> (s => print_endline("init returns r = " ++ s));
  (es, r);
};

let step = (obj: EvalObj.t) => {
  print_endline("======== step BEGIN =========");
  let r = step(~pause=false, obj, EvaluatorState.init);
  print_endline("======== step END =========");
  r;
};

let decompose = (d: DHExp.t) => {
  print_endline("======== decompose BEGIN =========");
  let (env, es) =
    Environment.empty
    |> ClosureEnvironment.of_environment
    |> EvaluatorState.with_eig(_, EvaluatorState.init);
  let (es, rs) = decompose(env, [], d, es);
  let rs =
    rs
    |> List.filter_map((obj: DecomposeResult.t) => {
         obj.exp
         |> DHExp.sexp_of_t
         |> Sexplib.Sexp.to_string_hum
         |> (s => print_endline("obj.exp = " ++ s));
         obj.flt
         |> DHExp.FilterEnvironment.sexp_of_t
         |> Sexplib.Sexp.to_string_hum
         |> (s => print_endline("obj.flt = " ++ s));
         DHExp.FilterEnvironment.matches(obj.exp, obj.flt)
         |> sexp_of_option(DHExp.FilterAction.sexp_of_t)
         |> Sexplib.Sexp.to_string_hum
         |> (s => print_endline("match result = " ++ s));
         switch (DHExp.FilterEnvironment.matches(obj.exp, obj.flt)) {
         | Some(Eval) => Some(EvalObj.mk(obj.env, Eval, obj.ctx, obj.exp))
         | Some(Step)
         | Some(Keep)
         | None =>
           let rec is_final = (d: DHExp.t) => {
             switch (d) {
             | TestLit(_)
             | BoolLit(_)
             | IntLit(_)
             | FloatLit(_)
             | StringLit(_)
             | Closure(_, Filter(_, Fun(_)))
             | Tag(_)
             | FreeVar(_)
             | InvalidText(_)
             | EmptyHole(_)
             | Closure(_, Fun(_))
             | ExpandingKeyword(_) => true
             | Fun(_)
             | ApBuiltin(_)
             | FixF(_, _, _)
             | BoundVar(_)
             | Ap(_, _)
             | BinBoolOp(_, _, _)
             | BinIntOp(_, _, _)
             | BinFloatOp(_, _, _)
             | BinStringOp(_, _, _)
             | Cons(_, _)
             | Sequence(_, _)
             | Let(_, _, _)
             | Prj(_, _)
             | Inj(_, _, _)
             | InvalidOperation(_, _)
             | ConsistentCase(_)
             | InconsistentBranches(_) => false
             | NonEmptyHole(_, _, _, d)
             | Closure(_, d)
             | Filter(_, d)
             | Cast(d, _, _)
             | FailedCast(d, _, _) => is_final(d)
             | ListLit(_, _, _, _, ds)
             | Tuple(ds) =>
               ds |> List.fold_left((res, d) => res && is_final(d), true)
             };
           };
           switch (obj.exp) {
           | TestLit(_)
           | BoolLit(_)
           | IntLit(_)
           | FloatLit(_)
           | StringLit(_)
           | Closure(_, Filter(_, Fun(_)))
           | Tag(_)
           | FreeVar(_)
           | InvalidText(_)
           | EmptyHole(_)
           | Closure(_, Fun(_))
           | ExpandingKeyword(_) => None
           | Fun(_)
           | ApBuiltin(_)
           | FixF(_, _, _)
           | BoundVar(_) => Some(EvalObj.mk(obj.env, Eval, obj.ctx, obj.exp))
           | Ap(d1, d2) =>
             if (is_final(d1) && is_final(d2)) {
               Some(EvalObj.mk(obj.env, Eval, obj.ctx, obj.exp));
             } else {
               None;
             }
           | BinIntOp(_, d1, d2) =>
             if (is_final(d1) && is_final(d2)) {
               Some(EvalObj.mk(obj.env, Eval, obj.ctx, obj.exp));
             } else {
               None;
             }
           | _ => None
           };
         };
       });
  rs
  |> Sexplib.Std.sexp_of_list(EvalObj.sexp_of_t)
  |> Sexplib.Sexp.to_string_hum
  |> (s => print_endline("decomposed: r = " ++ s));
  print_endline("======== decompose END =========");
  (es, rs);
};
