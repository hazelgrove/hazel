open Sexplib.Std;
open Util;
open PatternMatch;
open DH;

module EvalCtx = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Mark
    | Closure
    | Filter
    | Sequence1
    | Sequence2
    | Let1
    | Let2
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
    | ApBuiltin(int)
    | Test
    | Cons1
    | Cons2
    | ListConcat1
    | ListConcat2
    | Prj
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
    | Filter(DHExp.FilterEnvironment.t, t)
    | Sequence1(t, DHExp.t)
    | Sequence2(DHExp.t, t)
    | Let1(DHPat.t, t, DHExp.t)
    | Let2(DHPat.t, DHExp.t, t)
    | Ap1(t, DHExp.t)
    | Ap2(DHExp.t, t)
    | BinBoolOp1(TermBase.UExp.op_bin_bool, t, DHExp.t)
    | BinBoolOp2(TermBase.UExp.op_bin_bool, DHExp.t, t)
    | BinIntOp1(TermBase.UExp.op_bin_int, t, DHExp.t)
    | BinIntOp2(TermBase.UExp.op_bin_int, DHExp.t, t)
    | BinFloatOp1(TermBase.UExp.op_bin_float, t, DHExp.t)
    | BinFloatOp2(TermBase.UExp.op_bin_float, DHExp.t, t)
    | BinStringOp1(TermBase.UExp.op_bin_string, t, DHExp.t)
    | BinStringOp2(TermBase.UExp.op_bin_string, DHExp.t, t)
    | Tuple(t, (list(DHExp.t), list(DHExp.t)))
    | ApBuiltin(string, t, (list(DHExp.t), list(DHExp.t)))
    | Test(KeywordID.t, t)
    | ListLit(
        MetaVar.t,
        MetaVarInst.t,
        Typ.t,
        t,
        (list(DHExp.t), list(DHExp.t)),
      )
    | Cons1(t, DHExp.t)
    | Cons2(DHExp.t, t)
    | ListConcat1(t, DHExp.t)
    | ListConcat2(DHExp.t, t)
    | Prj(t, int)
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
type step_kind =
  | InvalidStep
  | VarLookup
  | Sequence
  | LetBind
  | FunClosure
  | FixUnwrap
  | UpdateTest
  | FunAp
  | CastAp
  | Builtin(string)
  | BinBoolOp(TermBase.UExp.op_bin_bool)
  | BinIntOp(TermBase.UExp.op_bin_int)
  | BinFloatOp(TermBase.UExp.op_bin_float)
  | BinStringOp(TermBase.UExp.op_bin_string)
  | Projection // TODO(Matt): This would be horrible if we showed this to the user
  | ListCons
  | ListConcat
  | CaseApply
  | CaseNext
  | CompleteClosure
  | CompleteFilter
  | Cast
  | Skip;

[@deriving sexp]
type ground_cases =
  | Hole
  | Ground
  | NotGroundOrHole(Typ.t) /* the argument is the corresponding ground type */;

let const_unknown: 'a => Typ.t = _ => Unknown(Internal);

let grounded_Arrow =
  NotGroundOrHole(Arrow(Unknown(Internal), Unknown(Internal)));
let grounded_Prod = length =>
  NotGroundOrHole(Prod(ListUtil.replicate(length, Typ.Unknown(Internal))));
let grounded_Sum = (sm: Typ.sum_map): ground_cases => {
  let sm' = sm |> ConstructorMap.map(Option.map(const_unknown));
  NotGroundOrHole(Sum(sm'));
};
let grounded_List = NotGroundOrHole(List(Unknown(Internal)));

let rec ground_cases_of = (ty: Typ.t): ground_cases => {
  let is_ground_arg: option(Typ.t) => bool =
    fun
    | None
    | Some(Typ.Unknown(_)) => true
    | Some(ty) => ground_cases_of(ty) == Ground;
  switch (ty) {
  | Unknown(_) => Hole
  | Bool
  | Int
  | Float
  | String
  | Var(_)
  | Rec(_)
  | Arrow(Unknown(_), Unknown(_))
  | List(Unknown(_)) => Ground
  | Prod(tys) =>
    if (List.for_all(
          fun
          | Typ.Unknown(_) => true
          | _ => false,
          tys,
        )) {
      Ground;
    } else {
      tys |> List.length |> grounded_Prod;
    }
  | Sum(sm) =>
    sm |> ConstructorMap.is_ground(is_ground_arg) ? Ground : grounded_Sum(sm)
  | Arrow(_, _) => grounded_Arrow
  | List(_) => grounded_List
  };
};

let evaluate_extend_env =
    (new_bindings: Environment.t, to_extend: ClosureEnvironment.t)
    : ClosureEnvironment.t => {
  to_extend
  |> ClosureEnvironment.map_of
  |> Environment.union(new_bindings)
  |> ClosureEnvironment.of_environment;
};

// TODO(Matt): Check Casts, Fix Cast Calculus Error on ADT Statics example
// TODO(Matt): PURGE THE ABOVE

type rule =
  // If anything going into it is indet, the result should be indet
  | Step({
      apply: unit => DHExp.t,
      kind: step_kind,
      final: bool,
    })
  | Constructor
  | Indet;

module type EV_MODE = {
  type state;
  type result;
  type requirement('a);
  type requirements('a, 'b);

  let req_value:
    (DHExp.t => result, EvalCtx.t => EvalCtx.t, DHExp.t) =>
    requirement(DHExp.t);
  let req_all_value:
    (
      DHExp.t => result,
      (EvalCtx.t, (list(DHExp.t), list(DHExp.t))) => EvalCtx.t,
      list(DHExp.t)
    ) =>
    requirement(list(DHExp.t));
  let req_final:
    (DHExp.t => result, EvalCtx.t => EvalCtx.t, DHExp.t) =>
    requirement(DHExp.t);
  let req_all_final:
    (
      DHExp.t => result,
      (EvalCtx.t, (list(DHExp.t), list(DHExp.t))) => EvalCtx.t,
      list(DHExp.t)
    ) =>
    requirement(list(DHExp.t));
  let do_not_req:
    (DHExp.t => result, EvalCtx.t => EvalCtx.t, DHExp.t) =>
    requirement(DHExp.t);

  let (let.): (requirements('a, DHExp.t), 'a => rule) => result;
  let (and.):
    (requirements('a, 'c => 'b), requirement('c)) =>
    requirements(('a, 'c), 'b);
  let otherwise: (ClosureEnvironment.t, 'a) => requirements(unit, 'a);

  let update_test: (state, KeywordID.t, TestMap.instance_report) => unit;
};

module Transition = (EV: EV_MODE) => {
  open EV;
  open DHExp;
  let (let.match) = ((env, match_result), r) =>
    switch (match_result) {
    | IndetMatch
    | DoesNotMatch => Indet
    | Matches(env') => r(evaluate_extend_env(env', env))
    };

  let transition = (req, state, env, d): 'a =>
    switch (d) {
    | BoundVar(x) =>
      let. _ = otherwise(env, BoundVar(x));
      let d =
        ClosureEnvironment.lookup(env, x)
        |> OptUtil.get(() => {
             print_endline("FreeInvalidVar:" ++ x);
             raise(EvaluatorError.Exception(FreeInvalidVar(x)));
           });
      Step({
        apply: () => d,
        kind: VarLookup,
        final: false // TODO(Matt): Can we make this true?
      });
    | Sequence(d1, d2) =>
      let. _ = otherwise(env, (d1, d2) => Sequence(d1, d2))
      and. _ = req_final(req(state, env), d1 => Sequence1(d1, d2), d1)
      and. d2' = do_not_req(req(state, env), d2 => Sequence2(d1, d2), d2);
      Step({apply: () => d2', kind: Sequence, final: false});
    | Let(dp, d1, d2) =>
      let. _ = otherwise(env, d1 => Let(dp, d1, d2))
      and. d1' = req_final(req(state, env), d1 => Let1(dp, d1, d2), d1);
      let.match env' = (env, matches(dp, d1'));
      Step({apply: () => Closure(env', d2), kind: LetBind, final: false});
    | Fun(_) =>
      let. _ = otherwise(env, d);
      Step({apply: () => Closure(env, d), kind: FunClosure, final: true});
    | FixF(f, t, d1) =>
      let. _ = otherwise(env, FixF(f, t, d1));
      // TODO(Matt): Would it be safer to have a fourth argument to FixF?
      // TODO(Matt): is this a step?
      // TODO(Matt): is t needed here?
      Step({
        apply: () =>
          Closure(
            evaluate_extend_env(Environment.singleton((f, d1)), env),
            d1,
          ),
        kind: FixUnwrap,
        final: false,
      });
    | Test(id, d) =>
      let. _ = otherwise(env, d => Test(id, d))
      and. d' = req_final(req(state, env), d => Test(id, d), d);
      Step({
        apply: () =>
          switch (d') {
          | BoolLit(true) =>
            update_test(state, id, (d', Pass));
            d;
          | BoolLit(false) =>
            update_test(state, id, (d', Fail));
            d;
          /* Hack: assume if final and not Bool, then Indet; this won't catch errors in statics */
          | _ =>
            update_test(state, id, (d', Indet));
            d;
          },
        kind: UpdateTest,
        final: true,
      });
    | Ap(d1, d2) =>
      let. _ = otherwise(env, (d1, d2) => Ap(d1, d2))
      and. d1' = req_value(req(state, env), d1 => Ap1(d1, d2), d1)
      and. d2' = req_final(req(state, env), d2 => Ap2(d1, d2), d2);
      switch (d1') {
      | Constructor(_) => Constructor // TODO(Matt): Two different "Constructor" constructors is confusing
      | Closure(env', Fun(dp, _, d3, _)) =>
        let.match env'' = (env', matches(dp, d2'));
        Step({apply: () => Closure(env'', d3), kind: FunAp, final: false});
      | Cast(d3', Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
        Step({
          apply: () => Cast(Ap(d3', Cast(d2', ty1', ty1)), ty2, ty2'),
          kind: CastAp,
          final: false,
        })
      | _ =>
        Step({
          apply: () => {
            print_endline("InvalidBoxedFun");
            raise(EvaluatorError.Exception(InvalidBoxedFun(d1')));
          },
          kind: InvalidStep,
          final: true,
        }) // TODO(Matt): Check that when I implement Indet it won't indet a partially-run program
      };
    | ApBuiltin(ident, args) =>
      let. _ = otherwise(env, args => ApBuiltin(ident, args))
      and. _args' =
        req_all_final(
          req(state, env),
          (d', ds) => ApBuiltin(ident, d', ds),
          args,
        );
      Step({
        apply: () => failwith("Builtins not implemented yet"), // TODO(Matt): Add builtins back.
        // VarMap.lookup(Builtins.forms_init, ident)
        // |> OptUtil.get(() => {
        //      print_endline("InvalidBuiltin");
        //      raise(EvaluatorError.Exception(InvalidBuiltin(ident)));
        //    }),
        kind: Builtin(ident),
        final: false,
      });
    | BoolLit(_)
    | IntLit(_)
    | FloatLit(_)
    | StringLit(_)
    | Constructor(_) =>
      let. _ = otherwise(env, d);
      Constructor;
    | BinBoolOp(And, d1, d2) =>
      let. _ = otherwise(env, (d1, d2) => BinBoolOp(And, d1, d2))
      and. d1' =
        req_value(req(state, env), d1 => BinBoolOp1(And, d1, d2), d1)
      and. d2' =
        do_not_req(req(state, env), d2 => BinBoolOp2(And, d1, d2), d2); // TODO(Matt): This might lead to some unexpected behaviour with not evaluating o | (5 == 3)
      Step({
        apply: () =>
          switch (d1') {
          | BoolLit(true) => d2'
          | BoolLit(false) => BoolLit(false)
          | _ => raise(EvaluatorError.Exception(InvalidBoxedBoolLit(d1'))) // TODO(Matt): Make it consistent whether we print
          },
        kind: BinBoolOp(And),
        final: false,
      });
    | BinBoolOp(Or, d1, d2) =>
      let. _ = otherwise(env, (d1, d2) => BinBoolOp(Or, d1, d2))
      and. d1' =
        req_value(req(state, env), d1 => BinBoolOp1(Or, d1, d2), d1)
      and. d2' =
        do_not_req(req(state, env), d2 => BinBoolOp2(Or, d1, d2), d2); // TODO(Matt): This might lead to some unexpected behaviour with not evaluating o | (5 == 3)
      Step({
        apply: () =>
          switch (d1') {
          | BoolLit(true) => BoolLit(true)
          | BoolLit(false) => d2'
          | _ => raise(EvaluatorError.Exception(InvalidBoxedBoolLit(d2')))
          },
        kind: BinBoolOp(Or),
        final: false,
      });
    | BinIntOp(op, d1, d2) =>
      let. _ = otherwise(env, (d1, d2) => BinIntOp(op, d1, d2))
      and. d1' = req_value(req(state, env), d1 => BinIntOp1(op, d1, d2), d1)
      and. d2' =
        req_value(req(state, env), d2 => BinIntOp2(op, d1, d2), d2);
      Step({
        apply: () =>
          switch (d1', d2') {
          | (IntLit(n1), IntLit(n2)) =>
            switch (op) {
            | Plus => IntLit(n1 + n2)
            | Minus => IntLit(n1 - n2)
            | Power when n2 < 0 =>
              InvalidOperation(
                BinIntOp(op, IntLit(n1), IntLit(n2)),
                NegativeExponent,
              )
            | Power => IntLit(IntUtil.ipow(n1, n2))
            | Times => IntLit(n1 * n2)
            | Divide when n2 == 0 =>
              InvalidOperation(
                BinIntOp(op, IntLit(n1), IntLit(n2)),
                DivideByZero,
              )
            | Divide => IntLit(n1 / n2)
            | LessThan => BoolLit(n1 < n2)
            | LessThanOrEqual => BoolLit(n1 <= n2)
            | GreaterThan => BoolLit(n1 > n2)
            | GreaterThanOrEqual => BoolLit(n1 >= n2)
            | Equals => BoolLit(n1 == n2)
            | NotEquals => BoolLit(n1 != n2)
            }
          | (IntLit(_), _) =>
            raise(EvaluatorError.Exception(InvalidBoxedIntLit(d2')))
          | _ => raise(EvaluatorError.Exception(InvalidBoxedIntLit(d1')))
          },
        kind: BinIntOp(op),
        final: false // False so that InvalidOperations are caught and made indet
      });
    | BinFloatOp(op, d1, d2) =>
      let. _ = otherwise(env, (d1, d2) => BinFloatOp(op, d1, d2))
      and. d1' =
        req_value(req(state, env), d1 => BinFloatOp1(op, d1, d2), d1)
      and. d2' =
        req_value(req(state, env), d2 => BinFloatOp2(op, d1, d2), d2);
      Step({
        apply: () =>
          switch (d1', d2') {
          | (FloatLit(n1), FloatLit(n2)) =>
            switch (op) {
            | Plus => FloatLit(n1 +. n1)
            | Minus => FloatLit(n1 -. n2)
            | Power => FloatLit(n1 ** n2)
            | Times => FloatLit(n1 *. n2)
            | Divide => FloatLit(n1 /. n2) // Do we like that this behavior is not consistent w/ integers?
            | LessThan => BoolLit(n1 < n2)
            | LessThanOrEqual => BoolLit(n1 <= n2)
            | GreaterThan => BoolLit(n1 > n2)
            | GreaterThanOrEqual => BoolLit(n1 >= n2)
            | Equals => BoolLit(n1 == n2)
            | NotEquals => BoolLit(n1 != n2)
            }
          | (FloatLit(_), _) =>
            raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d2')))
          | _ => raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d1')))
          },
        kind: BinFloatOp(op),
        final: true,
      });
    | BinStringOp(op, d1, d2) =>
      let. _ = otherwise(env, (d1, d2) => BinStringOp(op, d1, d2))
      and. d1' =
        req_value(req(state, env), d1 => BinStringOp1(op, d1, d2), d1)
      and. d2' =
        req_value(req(state, env), d2 => BinStringOp2(op, d1, d2), d2);
      Step({
        apply: () =>
          switch (d1', d2') {
          | (StringLit(s1), StringLit(s2)) =>
            switch (op) {
            | Concat => StringLit(s1 ++ s2)
            | Equals => BoolLit(s1 == s2)
            }
          | (StringLit(_), _) =>
            raise(EvaluatorError.Exception(InvalidBoxedStringLit(d2')))
          | _ => raise(EvaluatorError.Exception(InvalidBoxedStringLit(d1')))
          },
        kind: BinStringOp(op),
        final: true,
      });
    | Tuple(ds) =>
      let. _ = otherwise(env, ds => Tuple(ds))
      and. _ =
        req_all_value(req(state, env), (d1, ds) => Tuple(d1, ds), ds);
      Constructor;
    // TODO(Matt): As far as I can tell this case is only used for mutual recursion - could we cut it and replace w/ let?
    | Prj(d1, n) =>
      let. _ = otherwise(env, d1 => Prj(d1, n))
      and. d1' = req_final(req(state, env), d1 => Prj(d1, n), d1);
      Step({
        apply: () =>
          switch (d1') {
          | Tuple(ds) when n < 0 || List.length(ds) <= n =>
            InvalidOperation(d1', InvalidOperationError.InvalidProjection) // TODO(Matt): This is iconsistent - we don't deal with any other static error like this
          | Tuple(ds) => List.nth(ds, n)
          | Cast(_, Prod(ts), Prod(_)) when n < 0 || List.length(ts) <= n =>
            InvalidOperation(d1', InvalidOperationError.InvalidProjection) // TODO(Matt): This is iconsistent - we don't deal with any other static error like this
          | Cast(d2, Prod(ts1), Prod(ts2)) =>
            Cast(Prj(d2, n), List.nth(ts1, n), List.nth(ts2, n))
          | _ =>
            InvalidOperation(d1', InvalidOperationError.InvalidProjection) // TODO(Matt): This is iconsistent - we don't deal with any other static error like this
          },
        kind: Projection,
        final: false,
      });
    // TODO(Matt): Can we do something cleverer when the list structure is complete but the contents aren't?
    | Cons(d1, d2) =>
      let. _ = otherwise(env, (d1, d2) => Cons(d1, d2))
      and. d1' = req_final(req(state, env), d1 => Cons1(d1, d2), d1)
      and. d2' = req_value(req(state, env), d2 => Cons2(d1, d2), d2);
      Step({
        apply: () =>
          switch (d2') {
          | ListLit(u, i, ty, ds) => ListLit(u, i, ty, [d1', ...ds])
          | _ => raise(EvaluatorError.Exception(InvalidBoxedListLit(d2')))
          },
        kind: ListCons,
        final: true,
      });
    | ListConcat(d1, d2) =>
      // TODO(Matt): Can we do something cleverer when the list structure is complete but the contents aren't?
      let. _ = otherwise(env, (d1, d2) => ListConcat(d1, d2))
      and. d1' = req_value(req(state, env), d1 => ListConcat1(d1, d2), d1)
      and. d2' = req_value(req(state, env), d2 => ListConcat2(d1, d2), d2);
      Step({
        apply: () =>
          switch (d1', d2') {
          | (ListLit(u1, i1, t1, ds1), ListLit(_, _, _, ds2)) =>
            ListLit(u1, i1, t1, ds1 @ ds2)
          | (ListLit(_), _) =>
            raise(EvaluatorError.Exception(InvalidBoxedListLit(d2')))
          | (_, _) =>
            raise(EvaluatorError.Exception(InvalidBoxedListLit(d1')))
          },
        kind: ListConcat,
        final: true,
      });
    | ListLit(u, i, ty, ds) =>
      let. _ = otherwise(env, ds => ListLit(u, i, ty, ds))
      and. _ =
        req_all_final(
          req(state, env),
          (d1, ds) => ListLit(u, i, ty, d1, ds),
          ds,
        );
      Constructor;
    // TODO(Matt): This will currently re-traverse d1
    | ConsistentCase(Case(d1, rules, n)) =>
      let. _ = otherwise(env, d1 => ConsistentCase(Case(d1, rules, n)))
      and. d1' =
        req_final(
          req(state, env),
          d1 => ConsistentCase(Case(d1, rules, n)),
          d1,
        );
      switch (List.nth_opt(rules, n)) {
      | None => Indet // TODO: Why do we need a closure when everything is final?
      | Some(Rule(dp, d2)) =>
        switch (matches(dp, d1')) {
        | Matches(env') =>
          Step({
            apply: () => Closure(evaluate_extend_env(env', env), d2),
            kind: CaseApply,
            final: false,
          })
        | DoesNotMatch =>
          Step({
            apply: () => ConsistentCase(Case(d1', rules, n + 1)),
            kind: CaseNext,
            final: false,
          })
        | IndetMatch => Indet // TODO(Matt): Is Closure necessary?
        }
      };
    | InconsistentBranches(_) as d =>
      let. _ = otherwise(env, d);
      Indet; // TODO(Matt): Closure
    | Closure(_, Fun(_)) =>
      let. _ = otherwise(env, d);
      Constructor;
    | Closure(env', d) =>
      let. _ = otherwise(env, d => Closure(env', d))
      and. d' = req_value(req(state, env'), d1 => Closure(env', d1), d);
      Step({apply: () => d', kind: CompleteClosure, final: true});
    | NonEmptyHole(reason, u, i, d1) =>
      let. _ = otherwise(env, d1 => NonEmptyHole(reason, u, i, d1))
      and. _ =
        req_final(
          req(state, env),
          d1 => NonEmptyHole(reason, u, i, d1),
          d1,
        );
      Indet;
    | EmptyHole(_)
    | FreeVar(_)
    | InvalidText(_)
    | InvalidOperation(_)
    | ExpandingKeyword(_) =>
      let. _ = otherwise(env, d);
      Indet;
    | Cast(d, t1, t2) =>
      /* Cast calculus */
      let. _ = otherwise(env, d => Cast(d, t1, t2))
      and. d' = req_final(req(state, env), d => Cast(d, t1, t2), d);
      switch (ground_cases_of(t1), ground_cases_of(t2)) {
      | (Hole, Hole)
      | (Ground, Ground) =>
        /* if two types are ground and consistent, then they are eq */
        Step({apply: () => d', kind: Cast, final: true})
      | (Ground, Hole) =>
        /* can't remove the cast or do anything else here, so we're done */
        Constructor
      | (Hole, Ground) =>
        switch (d') {
        | Cast(d2, t3, Unknown(_)) =>
          /* by canonical forms, d1' must be of the form d<ty'' -> ?> */
          if (Typ.eq(t3, t2)) {
            Step({apply: () => d2, kind: Cast, final: true});
          } else {
            Step({
              apply: () => FailedCast(d', t1, t2),
              kind: Cast,
              final: false,
            });
          }
        | _ => Indet
        }
      | (Hole, NotGroundOrHole(t2_grounded)) =>
        /* ITExpand rule */
        Step({
          apply: () =>
            DHExp.Cast(Cast(d', t1, t2_grounded), t2_grounded, t2),
          kind: Cast,
          final: false,
        })
      | (NotGroundOrHole(t1_grounded), Hole) =>
        /* ITGround rule */
        Step({
          apply: () =>
            DHExp.Cast(Cast(d', t1, t1_grounded), t1_grounded, t2),
          kind: Cast,
          final: false,
        })
      | (Ground, NotGroundOrHole(_))
      | (NotGroundOrHole(_), Ground) =>
        /* can't do anything when casting between diseq, non-hole types */
        Constructor
      | (NotGroundOrHole(_), NotGroundOrHole(_)) =>
        /* they might be eq in this case, so remove cast if so */
        if (Typ.eq(t1, t2)) {
          Step({apply: () => d', kind: Cast, final: true});
        } else {
          Constructor;
        }
      };
    | FailedCast(d1, t1, t2) =>
      let. _ = otherwise(env, d1 => FailedCast(d1, t1, t2))
      and. _ = req_final(req(state, env), d1 => FailedCast(d1, t1, t2), d1);
      Indet;
    | Filter(f, d1) =>
      let. _ = otherwise(env, d1 => Filter(f, d1))
      and. d1 = req_final(req(state, env), d1 => Filter(f, d1), d1);
      Step({apply: () => d1, kind: CompleteFilter, final: true});
    };
};

let should_hide_step = (~env=true, ~casts=true) =>
  fun
  | LetBind
  | Sequence
  | UpdateTest
  | FunAp
  | Builtin(_)
  | BinBoolOp(_)
  | BinIntOp(_)
  | BinFloatOp(_)
  | BinStringOp(_)
  | ListCons
  | ListConcat
  | CaseApply
  | CaseNext
  | Projection // TODO(Matt): We don't want to show projection to the user
  | Skip
  | InvalidStep => false
  | VarLookup => env
  | CastAp
  | Cast => casts
  | CompleteClosure
  | CompleteFilter
  | FixUnwrap
  | FunClosure => true;
