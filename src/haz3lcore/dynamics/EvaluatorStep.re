open Sexplib.Std;
module Monad = EvaluatorMonad;
open Monad;
// open Monad.Syntax;
open Transition;

module Filter = DHExp.Filter;

module FilterAction = DHExp.FilterAction;

module FilterEnvironment = DHExp.FilterEnvironment;

/**
  Alias for EvaluatorMonad.
 */
// type m('a) = EvaluatorMonad.t('a);
module EvalObj = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    env: ClosureEnvironment.t,
    ctx: EvalCtx.t,
    act: FilterAction.t,
    exp: DHExp.t,
    knd: step_kind,
  };

  let mk = (env, ctx, act, exp, knd) => {env, ctx, act, exp, knd};

  // let mark = (env, act, exp) => {env, ctx: Mark, act, exp};

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
    | (Sequence1, Sequence1(c, _))
    | (Sequence2, Sequence2(_, c))
    | (Let1, Let1(_, c, _))
    | (Let2, Let2(_, _, c))
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
    | (ListConcat1, ListConcat1(c, _))
    | (ListConcat2, ListConcat2(_, c))
    | (Prj, Prj(c, _)) => Some({...obj, ctx: c})
    | (Tuple(n), Tuple(c, (ld, _))) =>
      if (List.length(ld) == n) {
        Some({...obj, ctx: c});
      } else {
        None;
      }
    | (ListLit(n), ListLit(_, _, _, c, (ld, _))) =>
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
    | (Cons2, Cons1(_))
    | (ListConcat1, ListConcat2(_))
    | (ListConcat2, ListConcat1(_)) => None
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

module Capture = {
  module Environment: {
    include
       (module type of VarBstMap.Ordered) with
        type t_('a) = VarBstMap.Ordered.t_('a);
    // type t = t_(unit);
    // let strip: DH.Environment.t => t;
  } = {
    include VarBstMap.Ordered;
    // type t = t_(unit);
    // let strip = (env: DH.Environment.t) => {
    //   DH.Environment.mapo(_ => (), env);
    // };
  };
  // type t = Environment.t;
  // let rec provides = (dp: DHPat.t): t => {
  //   switch (dp) {
  //   | Var(x) => Environment.extend(Environment.empty, (x, ()))
  //   | Ap(dp1, dp2) => Environment.union(provides(dp2), provides(dp1))
  //   | Tuple(dps) =>
  //     dps
  //     |> List.fold_left(
  //          (acc, dp) => Environment.union(provides(dp), acc),
  //          Environment.empty,
  //        )
  //   | ListLit(_, dps) =>
  //     dps
  //     |> List.fold_left(
  //          (acc, dp) => Environment.union(provides(dp), acc),
  //          Environment.empty,
  //        )
  //   | Cons(dp1, dp2) => Environment.union(provides(dp2), provides(dp1))
  //   | _ => Environment.empty
  //   };
  // };
  //   let rec analyze = (env: Environment.t, d: DHExp.t): Environment.t => {
  //     switch (d) {
  //     | BoundVar(x) =>
  //       let r = x |> Environment.lookup(env);
  //       switch (r) {
  //       | Some(_) => Environment.empty
  //       | None => Environment.singleton((x, ()))
  //       };
  //     | Sequence(d1, d2) =>
  //       Environment.union(analyze(env, d2), analyze(env, d1))
  //     | Let(dp, d1, d2) =>
  //       let env' = Environment.union(provides(dp), env);
  //       Environment.union(analyze(env, d1), analyze(env', d2));
  //     | FixF(f, _, d') =>
  //       let env' = Environment.extend(env, (f, ()));
  //       analyze(env', d');
  //     | Fun(dp, _, d1, _) =>
  //       let env' = Environment.union(provides(dp), env);
  //       analyze(env', d1);
  //     | Ap(d1, d2) => Environment.union(analyze(env, d1), analyze(env, d2))
  //     | ApBuiltin(_, args) =>
  //       args
  //       |> List.fold_left(
  //            (acc, arg) => Environment.union(analyze(env, arg), acc),
  //            Environment.empty,
  //          )
  //     | Test(_, d) => analyze(env, d)
  //     | BoolLit(_)
  //     | IntLit(_)
  //     | FloatLit(_)
  //     | StringLit(_)
  //     | Constructor(_) => Environment.empty
  //     | BinBoolOp(_, d1, d2) =>
  //       Environment.union(analyze(env, d1), analyze(env, d2))
  //     | BinIntOp(_, d1, d2) =>
  //       Environment.union(analyze(env, d1), analyze(env, d2))
  //     | BinFloatOp(_, d1, d2) =>
  //       Environment.union(analyze(env, d1), analyze(env, d2))
  //     | BinStringOp(_, d1, d2) =>
  //       Environment.union(analyze(env, d1), analyze(env, d2))
  //     | ListConcat(d1, d2) =>
  //       Environment.union(analyze(env, d1), analyze(env, d2))
  //     | Tuple(ds) =>
  //       ds
  //       |> List.fold_left(
  //            (acc, d) => {acc |> Environment.union(analyze(env, d))},
  //            Environment.empty,
  //          )
  //     | Prj(targ, _) => analyze(env, targ)
  //     | Cons(d1, d2) => Environment.union(analyze(env, d2), analyze(env, d1))
  //     | ListLit(_, _, _, lst) =>
  //       lst
  //       |> List.fold_left(
  //            (acc, d) => {acc |> Environment.union(analyze(env, d))},
  //            Environment.empty,
  //          )
  //     | ConsistentCase(Case(d1, rules, _)) =>
  //       rules
  //       |> List.fold_left(
  //            (acc: Environment.t, DHExp.Rule(dp, d)) => {
  //              let env' = Environment.union(provides(dp), env);
  //              acc |> Environment.union(analyze(env', d));
  //            },
  //            analyze(env, d1),
  //          )
  //     | Closure(env', d1) =>
  //       let env' = env' |> ClosureEnvironment.map_of |> Environment.strip;
  //       let env'' = Environment.union(env', env);
  //       analyze(env'', d1);
  //     | Filter(_, d1) => analyze(env, d1)
  //     | InconsistentBranches(_, _, Case(d1, rules, _)) =>
  //       rules
  //       |> List.fold_left(
  //            (acc: Environment.t, DHExp.Rule(dp, d)) => {
  //              let env' = Environment.union(provides(dp), env);
  //              acc |> Environment.union(analyze(env', d));
  //            },
  //            analyze(env, d1),
  //          )
  //     | EmptyHole(_) => Environment.empty
  //     | NonEmptyHole(_, _, _, d1) => analyze(env, d1)
  //     | FreeVar(_)
  //     | ExpandingKeyword(_)
  //     | InvalidText(_) => Environment.empty
  //     | Cast(d1, _, _) => analyze(env, d1)
  //     | FailedCast(d1, _, _) => analyze(env, d1)
  //     | InvalidOperation(d1, _) => analyze(env, d1)
  //     };
  //   };
  //   /*let is_closed = (d: DHExp.t): bool => {
  //       d |> analyze(Environment.empty) |> Environment.is_empty;
  //     };
  //     let capture = (d: DHExp.t, env: ClosureEnvironment.t): ClosureEnvironment.t => {
  //         let renv = analyze(Environment.empty, d);
  //         env
  //         |> ClosureEnvironment.filter_keep_id(((var, _)) =>
  //              Environment.lookup(renv, var) |> Option.is_some
  //            );
  //       };*/
};

module Decompose = {
  module Result = {
    type t =
      | Indet
      | BoxedValue
      | Eval(EvalObj.t)
      | Step(list(EvalObj.t));

    /*let map = (f: EvalObj.t => EvalObj.t, r: t) => {
        switch (r) {
        | Indet => Indet
        | BoxedValue => BoxedValue
        | Eval(obj) => Eval(f(obj))
        | Step(objs) => Step(List.map(f, objs))
        };
      };*/

    let unbox = (r: t): list(EvalObj.t) => {
      switch (r) {
      | Indet
      | BoxedValue => []
      | Eval(obj) => [obj]
      | Step(objs) => objs
      };
    };
    /*let return = (act: DHExp.FilterAction.t, obj: EvalObj.t): t => {
        switch (act) {
        | Eval => Eval(obj)
        | Step => Step([obj])
        };
      };*/
  };

  // TODO[Matt]: Add Skip/Step back to this

  module DecomposeEVMode: {
    include EV_MODE with type result = Result.t and type state = ref(state);
  } = {
    type state = ref(EvaluatorState.t); // TODO[Matt]: Make sure this gets passed around correctly
    type requirement('a) = (Result.t, 'a);
    type requirements('a, 'b) = (Result.t, ClosureEnvironment.t, 'a);
    type result = Result.t;

    let req_value = (cont, wr, d) => {
      switch (cont(d)) {
      | Result.Indet => (Result.Indet, d)
      | Result.BoxedValue => (Result.BoxedValue, d)
      | Result.Eval(obj) => (Result.Eval(EvalObj.wrap(wr, obj)), d)
      | Result.Step(objs) => (
          Result.Step(List.map(EvalObj.wrap(wr), objs)),
          d,
        )
      };
    };

    let (&&): (Result.t, Result.t) => Result.t =
      (u, v) =>
        switch (u, v) {
        | (Eval(_), _)
        | (_, Eval(_)) => failwith("TODO[Matt]: Fix Eval case")
        | (Step(ss1), Step(ss2)) => Step(ss1 @ ss2)
        | (Step(ss), _)
        | (_, Step(ss)) => Step(ss)
        | (Indet, BoxedValue)
        | (BoxedValue, Indet)
        | (Indet, Indet) => Indet
        | (BoxedValue, BoxedValue) => BoxedValue
        };

    let rec req_all_value' = (cont, wr, ds') =>
      fun
      | [] => (Result.BoxedValue, [])
      | [d, ...ds] => {
          let (r1, v) = req_value(cont, wr(_, (ds', ds)), d);
          let (r2, vs) = req_all_value'(cont, wr, [d, ...ds'], ds);
          (r1 && r2, [v, ...vs]);
        };
    let req_all_value = (cont, wr, ds) => {
      req_all_value'(cont, wr, [], ds);
    };

    let req_final = (cont, wr, d) => {
      (
        switch (cont(d)) {
        | Result.Indet => Result.BoxedValue
        | Result.BoxedValue => Result.BoxedValue
        | Result.Eval(obj) => Result.Eval(EvalObj.wrap(wr, obj))
        | Result.Step(objs) =>
          Result.Step(List.map(EvalObj.wrap(wr), objs))
        },
        d,
      );
    };

    let rec req_all_final' = (cont, wr, ds') =>
      fun
      | [] => (Result.BoxedValue, [])
      | [d, ...ds] => {
          let (r1, v) = req_final(cont, wr(_, (ds', ds)), d);
          let (r2, vs) = req_all_final'(cont, wr, [d, ...ds'], ds);
          (r1 && r2, [v, ...vs]);
        };

    let req_all_final = (cont, wr, ds) => {
      req_all_final'(cont, wr, [], ds);
    };

    let do_not_req = (_, _, d) => {
      (Result.BoxedValue, d);
    };

    let (let.): (requirements('a, DHExp.t), 'a => rule) => result =
      (rq, rl) =>
        switch (rq) {
        | (Result.Indet, _, _) => Result.Indet
        | (Result.BoxedValue, env, v) =>
          switch (rl(v)) {
          | Constructor => Result.BoxedValue
          | Indet => Result.Indet
          | Step(s) =>
            Result.Step([EvalObj.mk(env, Mark, Step, s.apply(), s.kind)])
          }
        | (Result.Step(_) as r, _, _)
        | (Result.Eval(_) as r, _, _) => r
        };

    let (and.):
      (requirements('a, 'c => 'b), requirement('c)) =>
      requirements(('a, 'c), 'b) =
      ((r1, env, v1), (r2, v2)) => (r1 && r2, env, (v1, v2));

    let otherwise = (env, _) => (Result.BoxedValue, env, ());
    let update_test = (state, id, v) =>
      state := EvaluatorState.add_test(state^, id, v);
  };

  module Decomp = Transition(DecomposeEVMode);
  let rec decompose = (state, env, exp) => {
    Decomp.transition(decompose, state, env, exp);
  };
  /*
   let rec decompose =
           (
             env: ClosureEnvironment.t,
             flt: FilterEnvironment.t,
             act: FilterAction.t,
             exp: DHExp.t,
           )
           : Monad.t(Result.t) => {
     let act = FilterEnvironment.matches(exp, act, flt);
     let decompose = (~env=env, ~flt=flt, ~act=act, exp) =>
       decompose(env, flt, act, exp);

     module Return = {
       type t =
         | Operator
         | Constructor;

       let merge =
           (cat: t, rs: list((Result.t, EvalCtx.t => EvalCtx.t))): Result.t => {
         let merge_cls = (rc: Result.cls, r: Result.t): Result.cls => {
           switch (rc, r) {
           | (_, Step(_))
           | (Step, _) => Step
           | (_, Eval(_))
           | (Eval, _) => Eval
           | (_, Indet)
           | (Indet, _) => Indet
           | (BoxedValue, BoxedValue) => BoxedValue
           };
         };

         let cls =
           rs
           |> List.fold_left((rc, (r, _)) => merge_cls(rc, r), BoxedValue);

         switch (act, cls) {
         | (Step, Step)
         | (Step, Eval)
         | (Eval, Step) =>
           let folder = (ac, (r: Result.t, f)) =>
             switch (r) {
             | Indet
             | BoxedValue => ac
             | Eval(obj) => [obj |> EvalObj.wrap(f), ...ac]
             | Step(objs) => List.map(EvalObj.wrap(f), objs) @ ac
             };
           let rs = rs |> List.fold_left(folder, []);
           Step(rs);
         | (Eval, Eval)
         | (Eval, BoxedValue) =>
           let env = env |> Capture.capture(exp);
           switch (cat) {
           | Operator => Eval(EvalObj.mark(env, Eval, exp))
           | Constructor => BoxedValue
           };
         | (Step, BoxedValue) =>
           let env = env |> Capture.capture(exp);
           switch (cat) {
           | Operator => Step([EvalObj.mark(env, Step, exp)])
           | Constructor => BoxedValue
           };
         | (Eval, Indet)
         | (Step, Indet) => Indet
         };
       };

       let wrap = f => Result.map(EvalObj.wrap(f));

       let mark = act => {
         let env = env |> Capture.capture(exp);
         EvalObj.mark(env, act, exp) |> Result.return(act) |> Monad.return;
       };

       let merge = (cat: t, rs) => rs |> merge(cat) |> Monad.return;

       let boxed = Result.BoxedValue |> Monad.return;

       let indet = Result.Indet |> Monad.return;
     };

     let* r = Transition.transition(env, exp);
     switch (r) {
     | BoxedValue(_) => Return.boxed
     | Indet(_) => Return.indet
     | Step(_) =>
       switch (exp) {
       | EmptyHole(_) => Return.indet
       | NonEmptyHole(_) => Return.indet
       | ExpandingKeyword(_) => Return.indet
       | FreeVar(_) => Return.indet
       | InvalidText(_) => Return.indet
       | InconsistentBranches(_) => Return.indet
       | FailedCast(_) => Return.indet
       | InvalidOperation(_) => Return.indet
       | Closure(_, Fun(_)) => Return.boxed
       | Closure(env', d1) =>
         let* env = ClosureEnvironment.union(env', env) |> with_eig;
         decompose(~env, d1) >>| Return.wrap(c => Closure(env', c));
       | Filter(flt', d1) =>
         let flt = DHExp.FilterEnvironment.extends(flt', flt);
         decompose(~flt, d1) >>| Return.wrap(c => Filter(flt', c));
       | Cast(d1, ty, ty') =>
         decompose(d1) >>| Return.wrap(c => Cast(c, ty, ty'))
       | BoundVar(_) => Return.mark(act)
       | Sequence(d1, d2) =>
         let* r1 = decompose(d1);
         switch (r1) {
         | BoxedValue
         | Eval(_) => Return.mark(act)
         | Indet
         | Step(_) => r1 |> Return.wrap(c => Sequence(c, d2)) |> Monad.return
         };
       | Let(dp, d1, d2) =>
         let* r1 = decompose(d1);
         switch (r1) {
         | BoxedValue
         | Eval(_) => Return.mark(act)
         | Indet
         | Step(_) => r1 |> Return.wrap(c => Let(dp, c, d2)) |> Monad.return
         };
       | FixF(_) => Return.mark(act)
       | Fun(_) => Return.mark(act)
       | Ap(d1, d2) =>
         let* r1 = decompose(d1);
         let* r2 = decompose(d2);
         switch (r1, r2) {
         | (BoxedValue, BoxedValue)
         | (BoxedValue, Eval(_))
         | (Eval(_), BoxedValue)
         | (Eval(_), Eval(_)) => Return.mark(act)
         | (Indet, _)
         | (_, Indet)
         | (Step(_), _)
         | (_, Step(_)) =>
           [(r1, (c => Ap1(c, d2))), (r2, (c => Ap2(d1, c)))]
           |> Return.merge(Return.Operator)
         };
       | ApBuiltin(_) => Return.mark(act)
       | TestLit(_)
       | BoolLit(_)
       | IntLit(_)
       | FloatLit(_)
       | StringLit(_)
       | Constructor(_) => Return.boxed
       | BinBoolOp(op, d1, d2) =>
         let* r1 = decompose(d1);
         let* r2 = decompose(d2);
         [
           (r1, (c => BinBoolOp1(op, c, d2))),
           (r2, (c => BinBoolOp2(op, d1, c))),
         ]
         |> Return.merge(Return.Operator);
       | BinIntOp(op, d1, d2) =>
         let* r1 = decompose(d1);
         let* r2 = decompose(d2);
         [
           (r1, (c => BinIntOp1(op, c, d2))),
           (r2, (c => BinIntOp2(op, d1, c))),
         ]
         |> Return.merge(Return.Operator);
       | BinFloatOp(op, d1, d2) =>
         let* r1 = decompose(d1);
         let* r2 = decompose(d2);
         [
           (r1, (c => BinFloatOp1(op, c, d2))),
           (r2, (c => BinFloatOp2(op, d1, c))),
         ]
         |> Return.merge(Return.Operator);
       | BinStringOp(op, d1, d2) =>
         let* r1 = decompose(d1);
         let* r2 = decompose(d2);
         [
           (r1, (c => BinStringOp1(op, c, d2))),
           (r2, (c => BinStringOp2(op, d1, c))),
         ]
         |> Return.merge(Return.Operator);
       | Tuple(ds) =>
         let rec walk = (ld, rd, rs) => {
           switch (rd) {
           | [] => rs
           | [d, ...rd] =>
             let* r = decompose(d);
             let* rs = rs;
             let rs = [(r, (c => EvalCtx.Tuple(c, (ld, rd)))), ...rs];
             walk([d, ...ld], rd, return(rs));
           };
         };
         let* rs = walk([], ds, return([]));
         rs |> Return.merge(Return.Constructor);
       | Prj(targ, n) => decompose(targ) >>| Return.wrap(c => Prj(c, n))
       | Cons(d1, d2) =>
         let* r1 = decompose(d1);
         let* r2 = decompose(d2);
         [(r1, (c => Cons1(c, d2))), (r2, (c => Cons2(d1, c)))]
         |> Return.merge(Return.Operator);
       | ListConcat(d1, d2) =>
         let* r1 = decompose(d1);
         let* r2 = decompose(d2);
         [(r1, (c => ListConcat1(c, d2))), (r2, (c => ListConcat2(d1, c)))]
         |> Return.merge(Return.Operator);
       | ListLit(u, i, ty, lst) =>
         let rec walk = (ld, rd, rs) => {
           switch (rd) {
           | [] => rs
           | [d, ...rd] =>
             let* r = decompose(d);
             let* rs = rs;
             let rs = [
               (r, (c => EvalCtx.ListLit(u, i, ty, c, (ld, rd)))),
               ...rs,
             ];
             walk([d, ...ld], rd, return(rs));
           };
         };
         let* rs = walk([], lst, return([]));
         rs |> Return.merge(Return.Constructor);
       | ConsistentCase(Case(d1, rules, i)) =>
         let* r1 = decompose(d1);
         switch (r1) {
         | BoxedValue
         | Eval(_) => Return.mark(act)
         | Indet
         | Step(_) =>
           r1
           |> Return.wrap(c => ConsistentCase(Case(c, rules, i)))
           |> Monad.return
         };
       }
     };
   };*/
};

let rec compose = (ctx: EvalCtx.t, d: DHExp.t): DHExp.t => {
  open DHExp;
  let rec rev_concat = (ls: list('a), rs: list('a)) => {
    switch (ls) {
    | [] => rs
    | [hd, ...tl] => rev_concat(tl, [hd, ...rs])
    };
  };
  switch (ctx) {
  | Mark => d
  | Closure(env, ctx) =>
    let d = compose(ctx, d);
    // let flist = Capture.analyze(Environment.empty, d);
    // if (Environment.is_empty(flist)) {
    //   d |> return;
    // } else {
    Closure(env, d); //;
  // };
  | Filter(fenv, ctx) =>
    let d = compose(ctx, d);
    switch (d) {
    | Filter(fenv', d) =>
      let fenv'' = FilterEnvironment.extends(fenv', fenv);
      Filter(fenv'', d);
    | _ => Filter(fenv, d)
    };
  | Sequence1(ctx, d2) =>
    let d1 = compose(ctx, d);
    Sequence(d1, d2);
  | Sequence2(d1, ctx) =>
    let d2 = compose(ctx, d);
    Sequence(d1, d2);
  | Ap1(ctx, d2) =>
    let d1 = compose(ctx, d);
    Ap(d1, d2);
  | Ap2(d1, ctx) =>
    let d2 = compose(ctx, d);
    Ap(d1, d2);
  | ApBuiltin(s, ctx, (ds1, ds2)) =>
    let d' = compose(ctx, d);
    ApBuiltin(s, rev_concat(ds1, [d', ...ds2]));
  | Test(lit, ctx) =>
    let d1 = compose(ctx, d);
    Test(lit, d1);
  | BinBoolOp1(op, ctx, d2) =>
    let d1 = compose(ctx, d);
    BinBoolOp(op, d1, d2);
  | BinBoolOp2(op, d1, ctx) =>
    let d2 = compose(ctx, d);
    BinBoolOp(op, d1, d2);
  | BinIntOp1(op, ctx, d2) =>
    let d1 = compose(ctx, d);
    BinIntOp(op, d1, d2);
  | BinIntOp2(op, d1, ctx) =>
    let d2 = compose(ctx, d);
    BinIntOp(op, d1, d2);
  | BinFloatOp1(op, ctx, d2) =>
    let d1 = compose(ctx, d);
    BinFloatOp(op, d1, d2);
  | BinFloatOp2(op, d1, ctx) =>
    let d2 = compose(ctx, d);
    BinFloatOp(op, d1, d2);
  | BinStringOp1(op, ctx, d2) =>
    let d1 = compose(ctx, d);
    BinStringOp(op, d1, d2);
  | BinStringOp2(op, d1, ctx) =>
    let d2 = compose(ctx, d);
    BinStringOp(op, d1, d2);
  | Cons1(ctx, d2) =>
    let d1 = compose(ctx, d);
    Cons(d1, d2);
  | Cons2(d1, ctx) =>
    let d2 = compose(ctx, d);
    Cons(d1, d2);
  | ListConcat1(ctx, d2) =>
    let d1 = compose(ctx, d);
    ListConcat(d1, d2);
  | ListConcat2(d1, ctx) =>
    let d2 = compose(ctx, d);
    ListConcat(d1, d2);
  | Tuple(ctx, (ld, rd)) =>
    let d = compose(ctx, d);
    Tuple(rev_concat(ld, [d, ...rd]));
  | ListLit(m, i, t, ctx, (ld, rd)) =>
    let d = compose(ctx, d);
    ListLit(m, i, t, rev_concat(ld, [d, ...rd]));
  | Let1(dp, ctx, d2) =>
    let d = compose(ctx, d);
    Let(dp, d, d2);
  | Let2(dp, d1, ctx) =>
    let d = compose(ctx, d);
    Let(dp, d1, d);
  | Prj(ctx, n) =>
    let d = compose(ctx, d);
    Prj(d, n);
  | Cast(ctx, ty1, ty2) =>
    let d = compose(ctx, d);
    Cast(d, ty1, ty2);
  | FailedCast(ctx, ty1, ty2) =>
    let d = compose(ctx, d);
    FailedCast(d, ty1, ty2);
  | InvalidOperation(ctx, err) =>
    let d = compose(ctx, d);
    InvalidOperation(d, err);
  | NonEmptyHole(reason, u, i, ctx) =>
    let d = compose(ctx, d);
    NonEmptyHole(reason, u, i, d);
  | ConsistentCase(Case(ctx, rule, n)) =>
    let d = compose(ctx, d);
    ConsistentCase(Case(d, rule, n));
  | InconsistentBranches(u, i, Case(ctx, rule, n)) =>
    let d = compose(ctx, d);
    InconsistentBranches(u, i, Case(d, rule, n));
  };
};
/*
 let step = (obj: EvalObj.t): m(EvaluatorResult.t) => {
   let* r =
     switch (obj.act) {
     | Eval => Evaluator.evaluate_closure(obj.env, obj.exp)
     | Step =>
       let* r = Transition.transition(obj.env, obj.exp);
       switch (r) {
       | Step(d)
       | BoxedValue(d) => EvaluatorResult.BoxedValue(d) |> return
       | Indet(d) => EvaluatorResult.Indet(d) |> return
       };
     };
   let* d = compose(obj.ctx, EvaluatorResult.unbox(r));
   switch (r) {
   | BoxedValue(_) => EvaluatorResult.BoxedValue(d) |> return
   | Indet(_) => EvaluatorResult.Indet(d) |> return
   };
 };

 let evaluate_with_history = (d: DHExp.t) => {
   let rec go =
           (env: ClosureEnvironment.t, d: DHExp.t, rs: m(list(DHExp.t)))
           : m(list(DHExp.t)) => {
     let* rs = rs;
     let* r = Transition.transition(env, d);
     switch (r) {
     | Step(d) => go(env, d, [d, ...rs] |> return)
     | BoxedValue(_) => rs |> return
     | Indet(_) => rs |> return
     };
   };
   let (env, es) =
     Environment.empty
     |> ClosureEnvironment.of_environment
     |> EvaluatorState.with_eig(_, EvaluatorState.init);
   let (_, rs) = go(env, d, [] |> return, es);
   rs;
 };

 let step = (obj: EvalObj.t) => {
   step(obj, EvaluatorState.init);
 };

 let step = Core.Memo.general(~cache_size_bound=1000, step);

 let step = (obj: EvalObj.t): ProgramResult.t => {
   let (es, d) = step(obj);
   switch (d) {
   | BoxedValue(d) => (BoxedValue(d), es)
   | Indet(d) => (Indet(d), es)
   | exception (EvaluatorError.Exception(_reason)) =>
     //HACK(andrew): supress exceptions for release
     //raise(EvalError(reason))
     print_endline("Interface.step EXCEPTION");
     (Indet(InvalidText(Id.invalid, 0, "EXCEPTION")), EvaluatorState.init);
   | exception _ =>
     print_endline("Other evaluation exception raised (stack overflow?)");
     (Indet(InvalidText(Id.invalid, 0, "EXCEPTION")), EvaluatorState.init);
   };
 };
 */
let decompose = (d: DHExp.t) => {
  let es = EvaluatorState.init;
  let env = ClosureEnvironment.of_environment(Builtins.env_init);
  let rs = Decompose.decompose(ref(es), env, d);
  Decompose.Result.unbox(rs);
};

let rec evaluate_with_history = d =>
  switch (decompose(d)) {
  | [] => []
  | [x, ..._] =>
    let next = x.exp;
    [next, ...evaluate_with_history(next)];
  };

module Stepper = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type step = {
    d: DHExp.t,
    step: EvalObj.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    current: DHExp.t,
    previous: list(step),
    next: list(EvalObj.t),
  };

  let rec step_forward = (e: EvalObj.t, s: t) => {
    let current = compose(e.ctx, e.exp);
    skip_steps({
      current,
      previous: [{d: s.current, step: e}, ...s.previous],
      next: decompose(current),
    });
  }
  and skip_steps = s => {
    switch (
      List.find_opt((x: EvalObj.t) => should_hide_step(x.knd), s.next)
    ) {
    | None => s
    | Some(e) => step_forward(e, s)
    };
  };

  let mk = d => {
    skip_steps({current: d, previous: [], next: decompose(d)});
  };

  let step_backward = (s: t) =>
    switch (s.previous) {
    | [] => failwith("cannot step backwards")
    | [x, ...xs] => {current: x.d, previous: xs, next: decompose(x.d)}
    };

  let update_expr = (d: DHExp.t, _: t) => {
    current: d,
    previous: [],
    next: decompose(d),
  };

  let get_justification: step_kind => string =
    fun
    | LetBind => "let binding"
    | Sequence => "sequence"
    | FixUnwrap => "unroll fixpoint"
    | UpdateTest => "update test"
    | FunAp => "apply function"
    | Builtin(_) => "builtin application" // TODO[Matt]: Make more specific
    | BinBoolOp(_) => "boolean logic"
    | BinIntOp(_) => "arithmetic"
    | BinFloatOp(_) => "arithmetic"
    | BinStringOp(_) => "string manipulation"
    | ListCons => "list manipulation"
    | ListConcat => "list manipulation"
    | CaseApply => "case selection"
    | CaseNext => "case discarding"
    | Projection => "projection" // TODO(Matt): We don't want to show projection to the user
    | InvalidStep => "error"
    | VarLookup => "variable lookup"
    | CastAp
    | Cast => "cast calculus"
    | CompleteFilter => "unidentified step"
    | CompleteClosure => "unidentified step"
    | FunClosure => "unidentified step";

  let get_history = stepper =>
    List.filter(x => !should_hide_step(x.step.knd), stepper.previous);
};
