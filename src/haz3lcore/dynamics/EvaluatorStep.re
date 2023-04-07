open Sexplib.Std;
open Util;
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
    | Filter(list((DHExp.t, TermBase.UExp.filter_action)), t)
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

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | BoxedValue(DHExp.t)
  | Indet(DHExp.t)
  | Step(DHExp.t);

let unbox =
  fun
  | Step(d)
  | BoxedValue(d)
  | Indet(d) => d;

/**
  Alias for EvaluatorMonad.
 */
type m('a) = EvaluatorMonad.t('a);

let matches = Evaluator.matches;

let evaluate_extend_env = Evaluator.evaluate_extend_env;

let ground_cases_of = Evaluator.ground_cases_of;

let eval_bin_bool_op = Evaluator.eval_bin_bool_op;

let eval_bin_bool_op_short_circuit = Evaluator.eval_bin_bool_op_short_circuit;

let eval_bin_int_op = Evaluator.eval_bin_int_op;

let eval_bin_float_op = Evaluator.eval_bin_float_op;

let eval_bin_string_op = Evaluator.eval_bin_string_op;

let evaluate_ap_builtin = Evaluator.evaluate_ap_builtin;

module EvalFilter = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = TermBase.UExp.filter_action;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list((DHExp.t, action));

  let init: t = [];

  let rec matches_exp = (d: DHExp.t, f: DHExp.t): bool => {
    switch (d, f) {
    | (_, EmptyHole(_)) => true
    | (EmptyHole(_), _) => false

    | (BoundVar(dx), BoundVar(fx)) => dx == fx
    | (BoundVar(dx), FreeVar(_, _, fx)) => dx == fx
    | (FreeVar(_, _, dx), BoundVar(fx)) => dx == fx
    | (FreeVar(_, _, dx), FreeVar(_, _, fx)) => dx == fx

    | (Sequence(d1, d2), Sequence(f1, f2)) =>
      matches_exp(d1, f1) && matches_exp(d2, f2)

    | (Let(dp, d1, d2), Let(fp, f1, f2)) =>
      matches_pat(dp, fp) && matches_exp(d1, f1) && matches_exp(d2, f2)

    | (FixF(dname, dt, dbody), FixF(fname, ft, fbody)) =>
      dname == fname && matches_typ(dt, ft) && matches_exp(dbody, fbody)

    | (Fun(_, _, _, dname), Fun(_, _, _, fname)) => dname == fname

    | (Ap(d1, d2), Ap(f1, f2)) =>
      matches_exp(d1, f1) && matches_exp(d2, f2)

    | (ApBuiltin(dname, dargs), ApBuiltin(fname, fargs)) =>
      dname == fname
      && List.fold_left2(
           (r, d, f) => r && matches_exp(d, f),
           true,
           dargs,
           fargs,
         )

    | (TestLit(did), TestLit(fid)) => did == fid

    | (BoolLit(db), BoolLit(fb)) => db == fb

    | (IntLit(db), IntLit(fb)) => db == fb

    | (FloatLit(db), FloatLit(fb)) => db == fb

    | (StringLit(db), StringLit(fb)) => db == fb

    | (Tag(db), Tag(fb)) => db == fb

    | (BinBoolOp(dop, d1, d2), BinBoolOp(fop, f1, f2)) =>
      dop == fop && matches_exp(d1, f1) && matches_exp(d2, f2)

    | (BinIntOp(dop, d1, d2), BinIntOp(fop, f1, f2)) =>
      dop == fop && matches_exp(d1, f1) && matches_exp(d2, f2)

    | (BinFloatOp(dop, d1, d2), BinFloatOp(fop, f1, f2)) =>
      dop == fop && matches_exp(d1, f1) && matches_exp(d2, f2)

    | (BinStringOp(dop, d1, d2), BinStringOp(fop, f1, f2)) =>
      dop == fop && matches_exp(d1, f1) && matches_exp(d2, f2)

    | (Inj(dt, ds, d1), Inj(ft, fs, f1)) =>
      dt == ft && ds == fs && matches_exp(d1, f1)

    | (Tuple(ds), Tuple(fs)) =>
      List.fold_left2((r, d, f) => r && matches_exp(d, f), true, ds, fs)

    | (Prj(d1, dn), Prj(f1, fn)) => matches_exp(d1, f1) && dn == fn

    | (ListLit(_, _, _, dt, ds), ListLit(_, _, _, ft, fs)) =>
      dt == ft
      && List.fold_left2((r, d, f) => r && matches_exp(d, f), true, ds, fs)

    | (
        ConsistentCase(Case(d1, drules, dn)),
        ConsistentCase(Case(f1, frules, fn)),
      ) =>
      matches_exp(d1, f1)
      && matches_rul(List.nth(drules, dn), List.nth(frules, fn))

    | (Closure(_, d1), Closure(_, f1)) => matches_exp(d1, f1)
    | (Closure(_, d1), _) => matches_exp(d1, f)
    | (_, Closure(_, f1)) => matches_exp(d, f1)

    | (NonEmptyHole(_, _, _, d1), NonEmptyHole(_, _, _, f1)) =>
      matches_exp(d1, f1)
    | (NonEmptyHole(_, _, _, d1), _) => matches_exp(d1, f)
    | (_, NonEmptyHole(_, _, _, f1)) => matches_exp(d, f1)

    | (
        InconsistentBranches(_, _, Case(d1, drules, dn)),
        InconsistentBranches(_, _, Case(f1, frules, fn)),
      ) =>
      matches_exp(d1, f1)
      && matches_rul(List.nth(drules, dn), List.nth(frules, fn))

    | (InvalidText(_, _, dtext), InvalidText(_, _, ftext)) => dtext == ftext

    | (Cast(d1, _, _), _) => matches_exp(d1, f)
    | (_, Cast(f1, _, _)) => matches_exp(d, f1)

    | (FailedCast(d1, dty, dty'), FailedCast(f1, fty, fty')) =>
      matches_exp(d1, f1) && dty == fty && dty' == fty'

    | (InvalidOperation(d1, derr), InvalidOperation(f1, ferr)) =>
      matches_exp(d1, f1) && derr == ferr

    | (_, _) => false
    };
  }
  and matches_pat = (d: DHPat.t, f: DHPat.t): bool => {
    switch (d, f) {
    | (_, EmptyHole(_)) => true
    | (Wild, Wild) => true
    | (ExpandingKeyword(_, _, dkw), ExpandingKeyword(_, _, fkw)) =>
      dkw == fkw
    | (InvalidText(_, _, dt), InvalidText(_, _, df)) => dt == df
    | (Var(dx), Var(fx)) => dx == fx
    | (IntLit(di), IntLit(fi)) => di == fi
    | (_, _) => false
    };
  }
  and matches_typ = (d: Typ.t, f: Typ.t) => {
    switch (d, f) {
    | (_, _) => false
    };
  }
  and matches_rul = (_d: DHExp.rule, _f: DHExp.rule) => {
    false;
  };

  let matches = (d: DHExp.t, f: t): action => {
    let match_single = (oact: action, f: (DHExp.t, action)) => {
      let (f, nact) = f;
      if (matches_exp(d, f)) {
        nact;
      } else {
        oact;
      };
    };
    f |> List.fold_left(match_single, Eval);
  };

  let extends = (old_filter: t, new_filter: t) => {
    let equal = ((f1, act1: action), (f2, act2: action)) =>
      DHExp.fast_equal(f1, f2) && act1 == act2;
    let not_in_new = f => !List.exists(equal(f, _), new_filter);
    List.filter(not_in_new, old_filter) @ new_filter;
  };
};

let rec transition =
        (env: ClosureEnvironment.t, d: DHExp.t, f: EvalFilter.t): m(t) => {
  let act = EvalFilter.matches(d, f);
  /* TODO: Investigate */
  /* Increment number of evaluation steps (calls to `evaluate`). */
  let* () = take_step;
  switch (d) {
  | BoundVar(x) =>
    let d =
      x
      |> ClosureEnvironment.lookup(env)
      |> OptUtil.get(() => {
           print_endline("FreeInvalidVar: " ++ x);
           raise(EvaluatorError.Exception(FreeInvalidVar(x)));
         });
    /* We need to call [evaluate] on [d] again since [env] does not store
     * final expressions. */
    switch (act) {
    | Step => Step(Closure(env, Filter(f, d))) |> return
    | Eval => transition(env, d, f)
    };

  | Sequence(d1, d2) =>
    let* r1 = transition(env, d1, f);
    switch (r1) {
    | Step(d1') =>
      Step(Sequence(d1', Closure(env, Filter(f, d2)))) |> return
    | BoxedValue(_d1)
    /* FIXME THIS IS A HACK FOR 490; for now, just return evaluated d2 even
     * if evaluated d1 is indet. */
    | Indet(_d1) =>
      /* let* r2 = step(env, d2, opt); */
      /* switch (r2) { */
      /* | BoxedValue(d2) */
      /* | Indet(d2) => Indet(Sequence(d1, d2)) |> return */
      /* }; */
      switch (act) {
      | Step => Step(Closure(env, Filter(f, d2))) |> return
      | Eval => transition(env, d2, f)
      }
    };

  | Filter(fs, dbody) =>
    let f = EvalFilter.extends(f, fs);
    let* r = transition(env, dbody, f);
    switch (r) {
    | Step(_) => Step(Closure(env, Filter(f, dbody))) |> return
    | BoxedValue(_)
    | Indet(_) => r |> return
    };

  | Let(dp, d1, d2) =>
    let* r1 = transition(env, d1, f);
    switch (r1) {
    | Step(d1') => Step(Let(dp, d1', d2)) |> return
    | BoxedValue(d1')
    | Indet(d1') =>
      switch (matches(dp, d1')) {
      | IndetMatch
      | DoesNotMatch => Indet(Let(dp, d1', d2)) |> return
      | Matches(env') =>
        let* env = evaluate_extend_env(env', env);
        switch (act) {
        | Step => Step(Closure(env, Filter(f, d2))) |> return
        | Eval => transition(env, d2, f)
        };
      }
    };

  | FixF(name, _, d') =>
    let* env' = evaluate_extend_env(Environment.singleton((name, d)), env);
    switch (act) {
    | Step => Step(Closure(env', Filter(f, d'))) |> return
    | Eval => transition(env', d', f)
    };

  | Fun(_) => BoxedValue(Closure(env, Filter(f, d))) |> return

  | Ap(d1, d2) =>
    let* r1 = transition(env, d1, f);
    switch (r1) {
    | Step(d1') => Step(Ap(d1', d2)) |> return
    | BoxedValue(TestLit(id)) => evaluate_test(env, id, d2, f)
    | BoxedValue(Tag(_)) =>
      let* r2 = transition(env, d2, f);
      switch (r2) {
      | Step(d2) => Step(Ap(d1, d2)) |> return
      | BoxedValue(d2) => BoxedValue(Ap(d1, d2)) |> return
      | Indet(d2) => Indet(Ap(d1, d2)) |> return
      };
    | BoxedValue(Closure(closure_env, Filter(f', Fun(dp, _, d3, _))) as d1) =>
      let f = EvalFilter.extends(f, f');
      let* r2 = transition(env, d2, f);
      switch (r2) {
      | Step(d2) => Step(Ap(d1, d2)) |> return
      | BoxedValue(d2)
      | Indet(d2) =>
        switch (matches(dp, d2)) {
        | DoesNotMatch
        | IndetMatch => Indet(Ap(d1, d2)) |> return
        | Matches(env') =>
          /* evaluate a closure: extend the closure environment with the
           * new bindings introduced by the function application. */
          let* env = evaluate_extend_env(env', closure_env);
          switch (act) {
          | Step => Step(Closure(env, Filter(f, d3))) |> return
          | Eval => transition(env, d3, f)
          };
        }
      };
    | BoxedValue(Cast(d1', Arrow(ty1, ty2), Arrow(ty1', ty2')))
    | Indet(Cast(d1', Arrow(ty1, ty2), Arrow(ty1', ty2'))) =>
      let* r2 = transition(env, d2, f);
      switch (r2) {
      | Step(d2') => Step(Ap(d1, d2')) |> return
      | BoxedValue(d2')
      | Indet(d2') =>
        /* ap cast rule */
        let d = DHExp.Cast(Ap(d1', Cast(d2', ty1', ty1)), ty2, ty2');
        switch (act) {
        | Step => Step(Closure(env, Filter(f, d))) |> return
        | Eval => transition(env, d, f)
        };
      };
    | BoxedValue(d1') =>
      print_endline("InvalidBoxedFun");
      raise(EvaluatorError.Exception(InvalidBoxedFun(d1')));
    | Indet(d1') =>
      let* r2 = transition(env, d2, f);
      switch (r2) {
      | Step(d2') => Step(Ap(d1, d2')) |> return
      | BoxedValue(d2')
      | Indet(d2') => Indet(Ap(d1', d2')) |> return
      };
    };

  | ApBuiltin(ident, args) =>
    let* r = evaluate_ap_builtin(env, ident, args);
    switch (r) {
    | BoxedValue(d) =>
      switch (act) {
      | Step => Step(d) |> return
      | Eval => BoxedValue(d) |> return
      }
    | Indet(d) => Indet(d) |> return
    };

  | TestLit(_)
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | StringLit(_)
  | Tag(_) => BoxedValue(d) |> return

  | BinBoolOp(op, d1, d2) =>
    let* r1 = transition(env, d1, f);
    switch (r1) {
    | Step(d1') => Step(BinBoolOp(op, d1', d2)) |> return
    | BoxedValue(BoolLit(b1) as d1') =>
      switch (eval_bin_bool_op_short_circuit(op, b1)) {
      | Some(b3) => Step(b3) |> return
      | None =>
        let* r2 = transition(env, d2, f);
        switch (r2) {
        | Step(d2') => Step(BinBoolOp(op, d1, d2')) |> return
        | BoxedValue(BoolLit(b2)) =>
          switch (act) {
          | Step => Step(eval_bin_bool_op(op, b1, b2)) |> return
          | Eval => BoxedValue(eval_bin_bool_op(op, b1, b2)) |> return
          }
        | BoxedValue(d2') =>
          print_endline("InvalidBoxedBoolLit");
          raise(EvaluatorError.Exception(InvalidBoxedBoolLit(d2')));
        | Indet(d2') => Indet(BinBoolOp(op, d1', d2')) |> return
        };
      }
    | BoxedValue(d1') =>
      print_endline("InvalidBoxedBoolLit");
      raise(EvaluatorError.Exception(InvalidBoxedBoolLit(d1')));
    | Indet(d1') =>
      let* r2 = transition(env, d2, f);
      switch (r2) {
      | Step(d2') => Step(BinBoolOp(op, d1, d2')) |> return
      | BoxedValue(d2')
      | Indet(d2') => Indet(BinBoolOp(op, d1', d2')) |> return
      };
    };

  | BinIntOp(op, d1, d2) =>
    let* r1 = transition(env, d1, f);
    switch (r1) {
    | Step(d1') => Step(BinIntOp(op, d1', d2)) |> return
    | BoxedValue(IntLit(n1) as d1') =>
      let* r2 = transition(env, d2, f);
      switch (r2) {
      | Step(d2') => Step(BinIntOp(op, d1, d2')) |> return
      | BoxedValue(IntLit(n2)) =>
        switch (op, n1, n2) {
        | (Divide, _, 0) =>
          Indet(
            InvalidOperation(
              BinIntOp(op, IntLit(n1), IntLit(n2)),
              DivideByZero,
            ),
          )
          |> return
        | (Power, _, _) when n2 < 0 =>
          Indet(
            InvalidOperation(
              BinIntOp(op, IntLit(n1), IntLit(n2)),
              NegativeExponent,
            ),
          )
          |> return
        | _ =>
          switch (act) {
          | Step => Step(eval_bin_int_op(op, n1, n2)) |> return
          | Eval => BoxedValue(eval_bin_int_op(op, n1, n2)) |> return
          }
        }
      | BoxedValue(d2') =>
        print_endline("InvalidBoxedIntLit1");
        raise(EvaluatorError.Exception(InvalidBoxedIntLit(d2')));
      | Indet(d2') => Indet(BinIntOp(op, d1', d2')) |> return
      };
    | BoxedValue(d1') =>
      print_endline("InvalidBoxedIntLit2");
      raise(EvaluatorError.Exception(InvalidBoxedIntLit(d1')));
    | Indet(d1') =>
      let* r2 = transition(env, d2, f);
      switch (r2) {
      | Step(d2') => Step(BinIntOp(op, d1, d2')) |> return
      | BoxedValue(d2')
      | Indet(d2') => Indet(BinIntOp(op, d1', d2')) |> return
      };
    };

  | BinFloatOp(op, d1, d2) =>
    let* r1 = transition(env, d1, f);
    switch (r1) {
    | Step(d1') => Step(BinFloatOp(op, d1', d2)) |> return
    | BoxedValue(FloatLit(f1) as d1') =>
      let* r2 = transition(env, d2, f);
      switch (r2) {
      | Step(d2') => Step(BinFloatOp(op, d1, d2')) |> return
      | BoxedValue(FloatLit(f2)) =>
        switch (act) {
        | Step => Step(eval_bin_float_op(op, f1, f2)) |> return
        | Eval => BoxedValue(eval_bin_float_op(op, f1, f2)) |> return
        }
      | BoxedValue(d2') =>
        print_endline("InvalidBoxedFloatLit");
        raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d2')));
      | Indet(d2') => Indet(BinFloatOp(op, d1', d2')) |> return
      };
    | BoxedValue(d1') =>
      print_endline("InvalidBoxedFloatLit");
      raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d1')));
    | Indet(d1') =>
      let* r2 = transition(env, d2, f);
      switch (r2) {
      | Step(d2') => Step(BinFloatOp(op, d1, d2')) |> return
      | BoxedValue(d2')
      | Indet(d2') => Indet(BinFloatOp(op, d1', d2')) |> return
      };
    };

  | BinStringOp(op, d1, d2) =>
    let* r1 = transition(env, d1, f);
    switch (r1) {
    | Step(d1') => Step(BinStringOp(op, d1', d2)) |> return
    | BoxedValue(StringLit(s1) as d1') =>
      let* r2 = transition(env, d2, f);
      switch (r2) {
      | Step(d2') => Step(BinStringOp(op, d1, d2')) |> return
      | BoxedValue(StringLit(s2)) =>
        switch (act) {
        | Step => Step(eval_bin_string_op(op, s1, s2)) |> return
        | Eval => BoxedValue(eval_bin_string_op(op, s1, s2)) |> return
        }
      | BoxedValue(d2') =>
        print_endline("InvalidBoxedStringLit");
        raise(EvaluatorError.Exception(InvalidBoxedStringLit(d2')));
      | Indet(d2') => Indet(BinStringOp(op, d1', d2')) |> return
      };
    | BoxedValue(d1') =>
      print_endline("InvalidBoxedStringLit");
      raise(EvaluatorError.Exception(InvalidBoxedStringLit(d1')));
    | Indet(d1') =>
      let* r2 = transition(env, d2, f);
      switch (r2) {
      | Step(d2') => Step(BinStringOp(op, d1, d2')) |> return
      | BoxedValue(d2')
      | Indet(d2') => Indet(BinStringOp(op, d1', d2')) |> return
      };
    };

  | Inj(ty, side, d1) =>
    let* r1 = transition(env, d1, f);
    switch (r1) {
    | Step(d1') => Step(Inj(ty, side, d1')) |> return
    | BoxedValue(d1') => BoxedValue(Inj(ty, side, d1')) |> return
    | Indet(d1') => Indet(Inj(ty, side, d1')) |> return
    };

  | Tuple(ds) =>
    let+ drs =
      ds
      |> List.map(d => transition(env, d, f) >>| (r => (d, r)))
      |> sequence;

    let empty = DHExp.Tuple([]);
    let (tag, ds') =
      List.fold_right(
        ((el, r), (tag, dst)) => {
          switch (tag, r) {
          | (Step(_), _) => (Step(empty), [el, ...dst])
          | (_, Step(el')) => (Step(empty), [el', ...dst])
          | (Indet(_), _) => (Indet(empty), [el, ...dst])
          | (_, Indet(el')) => (Indet(empty), [el', ...dst])
          | (BoxedValue(_), BoxedValue(el')) => (
              BoxedValue(empty),
              [el', ...dst],
            )
          }
        },
        drs,
        (BoxedValue(empty), []),
      );

    let d' = DHExp.Tuple(ds');

    switch (tag) {
    | Step(_) => Step(d')
    | Indet(_) => Indet(d')
    | BoxedValue(_) => BoxedValue(d')
    };

  | Prj(targ, n) =>
    // TODO:
    if (n < 0) {
      return(
        Indet(InvalidOperation(d, InvalidOperationError.InvalidProjection)),
      );
    } else {
      let* r = transition(env, targ, f);
      switch (r) {
      | Step(Tuple(ds) as rv) =>
        if (n >= List.length(ds)) {
          Step(InvalidOperation(rv, InvalidOperationError.InvalidProjection))
          |> return;
        } else {
          Step(List.nth(ds, n)) |> return;
        }
      | BoxedValue(Tuple(ds) as rv) =>
        if (n >= List.length(ds)) {
          return(
            Indet(
              InvalidOperation(rv, InvalidOperationError.InvalidProjection),
            ),
          );
        } else {
          return(BoxedValue(List.nth(ds, n)));
        }
      | Indet(Closure(_, Filter(_, Tuple(ds))) as rv)
      | Indet(Tuple(ds) as rv) =>
        if (n >= List.length(ds)) {
          return(
            Indet(
              InvalidOperation(rv, InvalidOperationError.InvalidProjection),
            ),
          );
        } else {
          return(Indet(List.nth(ds, n)));
        }
      | Step(Cast(targ', Prod(tys), Prod(tys')) as rv)
      | BoxedValue(Cast(targ', Prod(tys), Prod(tys')) as rv)
      | Indet(Cast(targ', Prod(tys), Prod(tys')) as rv) =>
        if (n >= List.length(tys)) {
          return(
            Indet(
              InvalidOperation(rv, InvalidOperationError.InvalidProjection),
            ),
          );
        } else {
          let ty = List.nth(tys, n);
          let ty' = List.nth(tys', n);
          Step(Closure(env, Filter(f, Cast(Prj(targ', n), ty, ty'))))
          |> return;
        }
      | Step(d) => Step(Prj(d, n)) |> return
      | _ => return(Indet(d))
      };
    }
  | Cons(d1, d2) =>
    let* r1 = transition(env, d1, f);
    let* r2 = transition(env, d2, f);
    switch (r1, r2) {
    | (Step(d1'), _) => Step(Cons(d1', d2)) |> return
    | (_, Step(d2')) => Step(Cons(d1, d2')) |> return
    | (Indet(d1), Indet(d2))
    | (Indet(d1), BoxedValue(d2))
    | (BoxedValue(d1), Indet(d2)) => Indet(Cons(d1, d2)) |> return
    | (BoxedValue(d1), BoxedValue(d2)) =>
      switch (d2) {
      | ListLit(x1, x2, x3, x4, lst) =>
        BoxedValue(ListLit(x1, x2, x3, x4, [d1, ...lst])) |> return
      | Cast(ListLit(x1, x2, x3, x4, lst), List(ty), List(ty')) =>
        BoxedValue(
          Cast(
            ListLit(x1, x2, x3, x4, [d1, ...lst]),
            List(ty),
            List(ty'),
          ),
        )
        |> return
      | _ =>
        print_endline("InvalidBoxedListLit");
        raise(EvaluatorError.Exception(InvalidBoxedListLit(d2)));
      }
    };

  | ListLit(u, i, err, ty, ds) =>
    let+ drs =
      ds
      |> List.map(d => transition(env, d, f) >>| (r => (d, r)))
      |> sequence;

    let empty = DHExp.Tuple([]);
    let (tag, ds') =
      List.fold_right(
        ((el, r), (tag, dst)) => {
          switch (tag, r) {
          | (Step(_), _) => (Step(empty), [el, ...dst])
          | (_, Step(el')) => (Step(empty), [el', ...dst])
          | (Indet(_), _) => (Indet(empty), [el, ...dst])
          | (_, Indet(el')) => (Indet(empty), [el', ...dst])
          | (BoxedValue(_), BoxedValue(el')) => (
              BoxedValue(empty),
              [el', ...dst],
            )
          }
        },
        drs,
        (BoxedValue(empty), []),
      );

    let d' = DHExp.ListLit(u, i, err, ty, ds');

    switch (tag) {
    | Step(_) => Step(d')
    | Indet(_) => Indet(d')
    | BoxedValue(_) => BoxedValue(d')
    };

  | ConsistentCase(Case(d1, rules, n)) =>
    evaluate_case(env, None, d1, rules, n, f, act)

  /* Generalized closures evaluate to themselves. Only
     lambda closures are BoxedValues; other closures are all Indet. */
  | Closure(env', d') =>
    switch (d') {
    | Filter(_, Fun(_)) => BoxedValue(d) |> return
    | Fun(_) => BoxedValue(d) |> return
    | Filter(fs, d') =>
      /* We merge the outside env, and closure env here to avoid closure
         inside closure. */
      let f = EvalFilter.extends(f, fs);
      let* env = ClosureEnvironment.union(env', env) |> with_eig;
      transition(env, d', f);
    | d' =>
      let* env = ClosureEnvironment.union(env', env) |> with_eig;
      transition(env, d', f);
    }

  /* Hole expressions */
  | InconsistentBranches(u, i, Case(d1, rules, n)) =>
    evaluate_case(env, Some((u, i)), d1, rules, n, f, act)

  | EmptyHole(u, i) =>
    Indet(Closure(env, Filter(f, EmptyHole(u, i)))) |> return

  | NonEmptyHole(reason, u, i, d1) =>
    let* r1 = transition(env, d1, f);
    switch (r1) {
    | Step(d1') =>
      Step(Closure(env, Filter(f, NonEmptyHole(reason, u, i, d1'))))
      |> return
    | BoxedValue(d1')
    | Indet(d1') =>
      Indet(Closure(env, Filter(f, NonEmptyHole(reason, u, i, d1'))))
      |> return
    };

  | FreeVar(u, i, x) =>
    Indet(Closure(env, Filter(f, FreeVar(u, i, x)))) |> return

  | ExpandingKeyword(u, i, kw) =>
    Indet(Closure(env, Filter(f, ExpandingKeyword(u, i, kw)))) |> return

  | InvalidText(u, i, text) =>
    Indet(Closure(env, Filter(f, InvalidText(u, i, text)))) |> return

  /* Cast calculus */
  | Cast(d1, ty, ty') =>
    let* r1 = transition(env, d1, f);
    switch (r1) {
    | Step(d1') => Step(Cast(d1', ty, ty')) |> return
    | BoxedValue(d1') as result =>
      switch (ground_cases_of(ty), ground_cases_of(ty')) {
      | (Hole, Hole) => result |> return
      | (Ground, Ground) =>
        /* if two types are ground and consistent, then they are eq */
        result |> return
      | (Ground, Hole) =>
        /* can't remove the cast or do anything else here, so we're done */
        BoxedValue(Cast(d1', ty, ty')) |> return
      | (Hole, Ground) =>
        /* by canonical forms, d1' must be of the form d<ty'' -> ?> */
        switch (d1') {
        | Cast(d1'', ty'', Unknown(_)) =>
          if (Typ.eq(ty'', ty')) {
            BoxedValue(d1'') |> return;
          } else {
            Indet(FailedCast(d1', ty, ty')) |> return;
          }
        | _ =>
          print_endline("CastBVHoleGround");
          raise(EvaluatorError.Exception(CastBVHoleGround(d1')));
        }
      | (Hole, NotGroundOrHole(ty'_grounded)) =>
        /* ITExpand rule */
        let d' = DHExp.Cast(Cast(d1', ty, ty'_grounded), ty'_grounded, ty');
        transition(env, d', f);
      | (NotGroundOrHole(ty_grounded), Hole) =>
        /* ITGround rule */
        let d' = DHExp.Cast(Cast(d1', ty, ty_grounded), ty_grounded, ty');
        transition(env, d', f);
      | (Ground, NotGroundOrHole(_))
      | (NotGroundOrHole(_), Ground) =>
        /* can't do anything when casting between diseq, non-hole types */
        BoxedValue(Cast(d1', ty, ty')) |> return
      | (NotGroundOrHole(_), NotGroundOrHole(_)) =>
        /* they might be eq in this case, so remove cast if so */
        if (Typ.eq(ty, ty')) {
          result |> return;
        } else {
          BoxedValue(Cast(d1', ty, ty')) |> return;
        }
      }
    | Indet(d1') as result =>
      switch (ground_cases_of(ty), ground_cases_of(ty')) {
      | (Hole, Hole) => result |> return
      | (Ground, Ground) =>
        /* if two types are ground and consistent, then they are eq */
        result |> return
      | (Ground, Hole) =>
        /* can't remove the cast or do anything else here, so we're done */
        Indet(Cast(d1', ty, ty')) |> return
      | (Hole, Ground) =>
        switch (d1') {
        | Cast(d1'', ty'', Unknown(_)) =>
          if (Typ.eq(ty'', ty')) {
            Indet(d1'') |> return;
          } else {
            Indet(FailedCast(d1', ty, ty')) |> return;
          }
        | _ => Indet(Cast(d1', ty, ty')) |> return
        }
      | (Hole, NotGroundOrHole(ty'_grounded)) =>
        /* ITExpand rule */
        let d' = DHExp.Cast(Cast(d1', ty, ty'_grounded), ty'_grounded, ty');
        transition(env, d', f);
      | (NotGroundOrHole(ty_grounded), Hole) =>
        /* ITGround rule */
        let d' = DHExp.Cast(Cast(d1', ty, ty_grounded), ty_grounded, ty');
        transition(env, d', f);
      | (Ground, NotGroundOrHole(_))
      | (NotGroundOrHole(_), Ground) =>
        /* can't do anything when casting between diseq, non-hole types */
        Indet(Cast(d1', ty, ty')) |> return
      | (NotGroundOrHole(_), NotGroundOrHole(_)) =>
        /* it might be eq in this case, so remove cast if so */
        if (Typ.eq(ty, ty')) {
          result |> return;
        } else {
          Indet(Cast(d1', ty, ty')) |> return;
        }
      }
    };

  | FailedCast(d1, ty, ty') =>
    let* r1 = transition(env, d1, f);
    switch (r1) {
    | Step(d1') => Step(FailedCast(d1', ty, ty')) |> return
    | BoxedValue(d1')
    | Indet(d1') => Indet(FailedCast(d1', ty, ty')) |> return
    };

  | InvalidOperation(d, err) => Indet(InvalidOperation(d, err)) |> return
  };
}

and evaluate_case =
    (
      env: ClosureEnvironment.t,
      inconsistent_info: option(HoleInstance.t),
      scrut: DHExp.t,
      rules: list(DHExp.rule),
      current_rule_index: int,
      f: EvalFilter.t,
      act: EvalFilter.action,
    )
    : m(t) => {
  let* rscrut = transition(env, scrut, f);
  switch (rscrut) {
  | Step(scrut) =>
    let case = DHExp.Case(scrut, rules, current_rule_index);
    switch (inconsistent_info) {
    | None => Step(Closure(env, Filter(f, ConsistentCase(case)))) |> return
    | Some((u, i)) =>
      Step(Closure(env, Filter(f, InconsistentBranches(u, i, case))))
      |> return
    };
  | BoxedValue(scrut)
  | Indet(scrut) =>
    eval_rule(
      env,
      inconsistent_info,
      scrut,
      rules,
      current_rule_index,
      f,
      act,
    )
  };
}
and eval_rule =
    (
      env: ClosureEnvironment.t,
      inconsistent_info: option(HoleInstance.t),
      scrut: DHExp.t,
      rules: list(DHExp.rule),
      current_rule_index: int,
      f: EvalFilter.t,
      act: EvalFilter.action,
    )
    : m(t) => {
  switch (List.nth_opt(rules, current_rule_index)) {
  | None =>
    let case = DHExp.Case(scrut, rules, current_rule_index);
    (
      switch (inconsistent_info) {
      | None => Indet(Closure(env, Filter(f, ConsistentCase(case))))
      | Some((u, i)) =>
        Indet(Closure(env, Filter(f, InconsistentBranches(u, i, case))))
      }
    )
    |> return;
  | Some(Rule(dp, d)) =>
    switch (matches(dp, scrut)) {
    | IndetMatch =>
      let case = DHExp.Case(scrut, rules, current_rule_index);
      (
        switch (inconsistent_info) {
        | None => Indet(Closure(env, Filter(f, ConsistentCase(case))))
        | Some((u, i)) =>
          Indet(Closure(env, Filter(f, InconsistentBranches(u, i, case))))
        }
      )
      |> return;
    | Matches(env') =>
      // extend environment with new bindings introduced
      let* env = evaluate_extend_env(env', env);
      switch (act) {
      | Step => Step(Closure(env, Filter(f, d))) |> return
      | Eval => transition(env, d, f)
      };
    // by the rule and evaluate the expression.
    | DoesNotMatch =>
      eval_rule(
        env,
        inconsistent_info,
        scrut,
        rules,
        current_rule_index + 1,
        f,
        act,
      )
    }
  };
}

and evaluate_test =
    (env: ClosureEnvironment.t, n: KeywordID.t, arg: DHExp.t, f: EvalFilter.t)
    : m(t) => {
  let* (arg_show, arg_result) =
    switch (DHExp.strip_casts(arg)) {
    | BinBoolOp(op, arg_d1, arg_d2) =>
      let mk_op = (arg_d1, arg_d2) => DHExp.BinBoolOp(op, arg_d1, arg_d2);
      evaluate_test_eq(env, mk_op, arg_d1, arg_d2, f);
    | BinIntOp(op, arg_d1, arg_d2) =>
      let mk_op = (arg_d1, arg_d2) => DHExp.BinIntOp(op, arg_d1, arg_d2);
      evaluate_test_eq(env, mk_op, arg_d1, arg_d2, f);
    | BinFloatOp(op, arg_d1, arg_d2) =>
      let mk_op = (arg_d1, arg_d2) => DHExp.BinFloatOp(op, arg_d1, arg_d2);
      evaluate_test_eq(env, mk_op, arg_d1, arg_d2, f);

    | Ap(Ap(arg_d1, arg_d2), arg_d3) =>
      let* arg_d1 = transition(env, arg_d1, f);
      let* arg_d2 = transition(env, arg_d2, f);
      let* arg_d3 = transition(env, arg_d3, f);
      let arg_show =
        DHExp.Ap(Ap(unbox(arg_d1), unbox(arg_d2)), unbox(arg_d3));
      let* arg_result = transition(env, arg_show, f);
      (arg_show, arg_result) |> return;

    | Ap(arg_d1, arg_d2) =>
      let mk = (arg_d1, arg_d2) => DHExp.Ap(arg_d1, arg_d2);
      evaluate_test_eq(env, mk, arg_d1, arg_d2, f);

    | _ =>
      let* arg = transition(env, arg, f);
      (unbox(arg), arg) |> return;
    };

  let test_status: TestStatus.t =
    switch (arg_result) {
    | BoxedValue(BoolLit(true)) => Pass
    | BoxedValue(BoolLit(false)) => Fail
    | _ => Indet
    };

  let* _ = add_test(n, (arg_show, test_status));
  let r: t =
    switch (arg_result) {
    | BoxedValue(BoolLit(_)) => BoxedValue(Tuple([]))
    // TODO:
    | Step(arg)
    | BoxedValue(arg)
    | Indet(arg) => Indet(Ap(TestLit(n), arg))
    };
  r |> return;
}

and evaluate_test_eq =
    (
      env: ClosureEnvironment.t,
      mk_arg_op: (DHExp.t, DHExp.t) => DHExp.t,
      arg_d1: DHExp.t,
      arg_d2: DHExp.t,
      f: EvalFilter.t,
    )
    : m((DHExp.t, t)) => {
  let* arg_d1 = transition(env, arg_d1, f);
  let* arg_d2 = transition(env, arg_d2, f);

  let arg_show = mk_arg_op(unbox(arg_d1), unbox(arg_d2));
  let* arg_result = transition(env, arg_show, f);

  (arg_show, arg_result) |> return;
};

// Fig.11 Final Forms
// d final
let is_final = (d: t): bool =>
  switch (d) {
  | Step(_) => false
  | BoxedValue(_)
  | Indet(_) => true
  };

module EvalObj = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    env: ClosureEnvironment.t,
    ctx: EvalCtx.t,
    exp: DHExp.t,
    flt: EvalFilter.t,
  };

  let mk = (env, ctx, exp, flt) => {env, ctx, exp, flt};

  let init = (exp: DHExp.t): t => {
    let (env, _) =
      Builtins.Pervasives.builtins_as_environment
      |> ClosureEnvironment.of_environment
      |> EvaluatorState.with_eig(_, EvaluatorState.init);
    {env, ctx: Mark, exp, flt: EvalFilter.init};
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
        (env: ClosureEnvironment.t, d: DHExp.t, f: EvalFilter.t)
        : m(list(EvalObj.t)) => {
  let wrap = (fctx: EvalCtx.t => EvalCtx.t, ld: list(EvalObj.t)) =>
    List.map((obj: EvalObj.t) => {...obj, ctx: fctx(obj.ctx)}, ld);

  let go = (dcs: list((DHExp.t, EvalCtx.t => EvalCtx.t))) => {
    let* is_final = {
      let f = (pr, d) => {
        let* r = transition(env, d, f);
        let* pr = pr;
        (pr && is_final(r)) |> return;
      };
      dcs |> List.map(fst) |> List.fold_left(f, true |> return);
    };
    if (is_final) {
      [EvalObj.mk(env, Mark, d, f)] |> return;
    } else {
      List.fold_left(
        (rc, (d, fc)) => {
          let* c = decompose(env, d, f);
          let* rc = rc;
          rc @ wrap(fc, c) |> return;
        },
        return([]),
        dcs,
      );
    };
  };

  switch (d) {
  | Closure(env', d) =>
    let* env = env |> ClosureEnvironment.union(env') |> with_eig;
    let* ld = decompose(env, d, f);
    wrap(c => Closure(env', c), ld) |> return;
  | Filter(fs, d) =>
    let f = EvalFilter.extends(f, fs);
    let* ld = decompose(env, d, f);
    wrap(c => Filter(fs, c), ld) |> return;
  | Cast(d, ty, ty') =>
    let* ld = decompose(env, d, f);
    wrap(c => Cast(c, ty, ty'), ld) |> return;
  | TestLit(_)
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | StringLit(_)
  | Fun(_)
  | Tag(_) => [] |> return
  | FreeVar(_)
  | InvalidText(_)
  | EmptyHole(_)
  | ExpandingKeyword(_) => [] |> return
  | ApBuiltin(_)
  | FixF(_, _, _)
  | BoundVar(_) => [EvalObj.mk(env, Mark, d, f)] |> return
  | Ap(d1, d2) => go([(d1, c => Ap1(c, d2)), (d2, c => Ap2(d1, c))])
  | NonEmptyHole(reason, u, i, d1) =>
    go([(d1, c => NonEmptyHole(reason, u, i, c))])
  | BinBoolOp(op, d1, d2) =>
    go([
      (d1, c => BinBoolOp1(op, c, d2)),
      (d2, c => BinBoolOp2(op, d1, c)),
    ])
  | BinIntOp(op, d1, d2) =>
    go([(d1, c => BinIntOp1(op, c, d2)), (d2, c => BinIntOp2(op, d1, c))])
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

let step = (obj: EvalObj.t): m(t) => {
  let* r = transition(obj.env, obj.exp, obj.flt);
  let d = compose(obj.ctx, unbox(r));
  switch (r) {
  | Step(_) => Step(d) |> return
  | BoxedValue(_) => BoxedValue(d) |> return
  | Indet(_) => Indet(d) |> return
  };
};

let step = (obj: EvalObj.t) => {
  step(obj, EvaluatorState.init);
};

let decompose = (d: DHExp.t) => {
  let f = EvalFilter.init;
  let (env, es) =
    Environment.empty
    |> ClosureEnvironment.of_environment
    |> EvaluatorState.with_eig(_, EvaluatorState.init);
  decompose(env, d, f, es);
};
