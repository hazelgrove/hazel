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

  let wrap = (f: EvalCtx.t => EvalCtx.t, obj: t) => {
    ...obj,
    ctx: obj.ctx |> f,
  };
};

let rec decompose =
        (
          env: ClosureEnvironment.t,
          flt: DHExp.FilterEnvironment.t,
          act: DHExp.FilterAction.t,
          exp: DHExp.t,
        )
        : m((DHExp.FilterAction.t, list(EvalObj.t))) => {
  let return = (r: (DHExp.FilterAction.t, list(EvalObj.t))) => r |> return;
  let mk = EvalObj.mk;
  let act =
    switch (DHExp.FilterEnvironment.matches(exp, flt)) {
    | Some(act) => act
    | None => act
    };

  switch (exp) {
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
  | ExpandingKeyword(_) => (Keep, []) |> return
  | Closure(_, Fun(_))
  | Fun(_)
  | ApBuiltin(_)
  | FixF(_, _, _)
  | BoundVar(_) => (Eval, [mk(env, Eval, Mark, exp)]) |> return
  | Ap(d1, d2) =>
    let* (a1, r1) = decompose(env, flt, act, d1);
    let* (a2, r2) = decompose(env, flt, act, d2);
    switch (act, a1, a2) {
    | (Eval, Eval, Eval) =>
      (Eval, [EvalObj.mk(env, Eval, Mark, exp)]) |> return
    | _ =>
      (
        Step,
        List.map(EvalObj.wrap(c => Ap1(c, d2)), r1)
        @ List.map(EvalObj.wrap(c => Ap2(d1, c)), r2),
      )
      |> return
    };
  | Closure(env', d) =>
    let* env = env |> ClosureEnvironment.union(env') |> with_eig;
    let* (a, r) = decompose(env, flt, act, d);
    (a, List.map(EvalObj.wrap(c => Closure(env', c)), r)) |> return;
  | Filter(f, d) =>
    let flt' = DHExp.FilterEnvironment.extends(f, flt);
    let* (a, r) = decompose(env, flt', act, d);
    (a, List.map(EvalObj.wrap(c => Filter(f, c)), r)) |> return;
  | BinIntOp(op, d1, d2) =>
    let* (a1, r1) = decompose(env, flt, act, d1);
    let* (a2, r2) = decompose(env, flt, act, d2);
    switch (act, a1, a2) {
    | (Eval, Eval | Keep, Eval | Keep) =>
      (Eval, [EvalObj.mk(env, Eval, Mark, exp)]) |> return
    | (Step, Keep, Keep) =>
      (Step, [EvalObj.mk(env, Step, Mark, exp)]) |> return
    | _ =>
      (
        Step,
        List.map(EvalObj.wrap(c => BinIntOp1(op, c, d2)), r1)
        @ List.map(EvalObj.wrap(c => BinIntOp2(op, d1, c)), r2),
      )
      |> return
    };
  | _ => (Eval, []) |> return
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
  let (es, (_, rs)) = decompose(env, [], Eval, d, es);
  rs
  |> Sexplib.Std.sexp_of_list(EvalObj.sexp_of_t)
  |> Sexplib.Sexp.to_string_hum
  |> (s => print_endline("decomposed: r = " ++ s));
  print_endline("======== decompose END =========");
  (es, rs);
};
