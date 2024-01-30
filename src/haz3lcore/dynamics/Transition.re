open Sexplib.Std;
open Util;
open PatternMatch;
open DH;

/* Transition.re

   This module defines the evaluation semantics of Hazel in terms of small step
   evaluation. These small steps are wrapped up into a big step in Evaluator.re.

   I'll use the Seq case as an example:

    | Seq(d1, d2) =>
        let. _ = otherwise(d1 => Seq(d1, d2))
        and. _ = req_final(req(state, env), 0, d1);
        Step({apply: () => d2, kind: Seq, final: false});


    Each step semantics starts with a `let. () = otherwise(...)` that defines how
    to wrap the expression back up if the step couldn't be evaluated.

    This is followed by a series of `and. d1' = req_final(req(state, env), <i>, <d1>)`
    which indicate that in order to evaluate the step, <d1> must be final. (req_value
    is also available if it needs to be a value). Note that if successful, d1' will
    be the fully-evaluated version of d1. The sub-expressions are all enumerated by
    the <i> field, so i=0 indicates that it is the first sub-expression, i=1 the
    second etc.

    Finally, we have the Step construct that defines the actual step. Note "Step"s
    should be used if and only if they change the expression. If they do not change
    the expression, use `Constructor` or `Indet`.

    The step defines firstly, a `() => ...` function giving the result of the step,
    secondly a `kind`, that describes the step (which will be used in the stepper)

    Lastly, the `value` field allows for some speeding up of the evaluator. If you
    are unsure, it is always safe to put `value: false`.

    `value: true` guarantees:
      - if all requirements are values, then the output will be a value
      - if some requirements are indet, then the output will be indet

    A value is either a literal, or a function with a closure. (functions without
    closures immediately inside them do not count as values).
   */

[@deriving (show({with_path: false}), sexp, yojson)]
type step_kind =
  | InvalidStep
  | VarLookup
  | Seq
  | LetBind
  | FunClosure
  | FixUnwrap
  | UpdateTest
  | FunAp
  | CastAp
  | BuiltinWrap
  | BuiltinAp(string)
  | BinBoolOp(TermBase.UExp.op_bin_bool)
  | BinIntOp(TermBase.UExp.op_bin_int)
  | BinFloatOp(TermBase.UExp.op_bin_float)
  | BinStringOp(TermBase.UExp.op_bin_string)
  | Conditional(bool)
  | Projection
  | ListCons
  | ListConcat
  | CaseApply
  | CompleteClosure
  | CompleteFilter
  | Cast
  | Skip;

module CastHelpers = {
  [@deriving sexp]
  type ground_cases =
    | Hole
    | Ground
    | NotGroundOrHole(Typ.t) /* the argument is the corresponding ground type */;

  let const_unknown: 'a => Typ.t = _ => Unknown(Internal);

  let grounded_Arrow =
    NotGroundOrHole(Arrow(Unknown(Internal), Unknown(Internal)));
  let grounded_Prod = length =>
    NotGroundOrHole(
      Prod(ListUtil.replicate(length, Typ.Unknown(Internal))),
    );
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
      sm |> ConstructorMap.is_ground(is_ground_arg)
        ? Ground : grounded_Sum(sm)
    | Arrow(_, _) => grounded_Arrow
    | List(_) => grounded_List
    };
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

type rule =
  | Step({
      apply: unit => DHExp.t,
      kind: step_kind,
      value: bool,
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
    | Var(x) =>
      let. _ = otherwise(env, Var(x));
      let d =
        ClosureEnvironment.lookup(env, x)
        |> OptUtil.get(() => {
             raise(EvaluatorError.Exception(FreeInvalidVar(x)))
           });
      Step({apply: () => d, kind: VarLookup, value: false});
    | Seq(d1, d2) =>
      let. _ = otherwise(env, d1 => Seq(d1, d2))
      and. _ = req_final(req(state, env), d1 => Seq1(d1, d2), d1);
      Step({apply: () => d2, kind: Seq, value: false});
    | Let(dp, d1, d2) =>
      let. _ = otherwise(env, d1 => Let(dp, d1, d2))
      and. d1' = req_final(req(state, env), d1 => Let1(dp, d1, d2), d1);
      let.match env' = (env, matches(dp, d1'));
      Step({apply: () => Closure(env', d2), kind: LetBind, value: false});
    | Fun(_, _, Closure(_), _) =>
      let. _ = otherwise(env, d);
      Constructor;
    | Fun(p, t, d, v) =>
      let. _ = otherwise(env, Fun(p, t, d, v));
      Step({
        apply: () => Fun(p, t, Closure(env, d), v),
        kind: FunClosure,
        value: true,
      });
    | FixF(f, t, d1) =>
      let. _ = otherwise(env, FixF(f, t, d1));
      Step({
        apply: () =>
          Closure(
            evaluate_extend_env(Environment.singleton((f, d1)), env),
            d1,
          ),
        kind: FixUnwrap,
        value: false,
      });
    | Test(id, d) =>
      let. _ = otherwise(env, d => Test(id, d))
      and. d' = req_final(req(state, env), d => Test(id, d), d);
      Step({
        apply: () =>
          switch (d') {
          | Bool(true) =>
            update_test(state, id, (d', Pass));
            Tuple([]);
          | Bool(false) =>
            update_test(state, id, (d', Fail));
            Tuple([]);
          /* Hack: assume if final and not Bool, then Indet; this won't catch errors in statics */
          | _ =>
            update_test(state, id, (d', Indet));
            Tuple([]);
          },
        kind: UpdateTest,
        value: true,
      });
    | Ap(d1, d2) =>
      let. _ = otherwise(env, (d1, d2) => Ap(d1, d2))
      and. d1' = req_value(req(state, env), d1 => Ap1(d1, d2), d1)
      and. d2' = req_final(req(state, env), d2 => Ap2(d1, d2), d2);
      switch (d1') {
      | Constructor(_) => Constructor
      | Fun(dp, _, Closure(env', d3), _) =>
        let.match env'' = (env', matches(dp, d2'));
        Step({apply: () => Closure(env'', d3), kind: FunAp, value: false});
      | Cast(d3', Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
        Step({
          apply: () => Cast(Ap(d3', Cast(d2', ty1', ty1)), ty2, ty2'),
          kind: CastAp,
          value: false,
        })
      | BuiltinFun(ident) =>
        Step({
          apply: () => {
            //HACK[Matt]: This step is just so we can check that d2' is not indet
            ApBuiltin(
              ident,
              d2',
            );
          },
          kind: BuiltinWrap,
          value: false // Not necessarily a value because of InvalidOperations
        })
      | _ =>
        Step({
          apply: () => {
            raise(EvaluatorError.Exception(InvalidBoxedFun(d1')));
          },
          kind: InvalidStep,
          value: true,
        })
      };
    | ApBuiltin(ident, arg) =>
      let. _ = otherwise(env, arg => ApBuiltin(ident, arg))
      and. arg' =
        req_value(req(state, env), arg => ApBuiltin(ident, arg), arg);
      Step({
        apply: () => {
          let builtin =
            VarMap.lookup(Builtins.forms_init, ident)
            |> OptUtil.get(() => {
                 raise(EvaluatorError.Exception(InvalidBuiltin(ident)))
               });
          builtin(arg');
        },
        kind: BuiltinAp(ident),
        value: false // Not necessarily a value because of InvalidOperations
      });
    | Bool(_)
    | Int(_)
    | Float(_)
    | String(_)
    | Constructor(_)
    | BuiltinFun(_) =>
      let. _ = otherwise(env, d);
      Constructor;
    | If(consistent, c, d1, d2) =>
      let. _ = otherwise(env, c => If(consistent, c, d1, d2))
      and. c' =
        req_value(req(state, env), c => If1(consistent, c, d1, d2), c);
      switch (consistent, c') {
      | (Consistent, Bool(b)) =>
        Step({
          apply: () => {
            b ? d1 : d2;
          },
          // Attach c' to indicate which branch taken.
          kind: Conditional(b),
          value: false,
        })
      // Use a seperate case for invalid conditionals. Makes extracting the bool from BoolLit (above) easier.
      | (Consistent, _) =>
        Step({
          apply: () => {
            raise(EvaluatorError.Exception(InvalidBoxedBoolLit(c')));
          },
          kind: InvalidStep,
          value: true,
        })
      // Inconsistent branches should be Indet
      | (Inconsistent(_), _) => Indet
      };
    | BinOp(Bool(And), d1, d2) =>
      let. _ = otherwise(env, d1 => BinOp(Bool(And), d1, d2))
      and. d1' =
        req_value(req(state, env), d1 => BinOp1(Bool(And), d1, d2), d1);
      Step({
        apply: () =>
          switch (d1') {
          | Bool(true) => d2
          | Bool(false) => Bool(false)
          | _ => raise(EvaluatorError.Exception(InvalidBoxedBoolLit(d1')))
          },
        kind: BinBoolOp(And),
        value: false,
      });
    | BinOp(Bool(Or), d1, d2) =>
      let. _ = otherwise(env, d1 => BinOp(Bool(Or), d1, d2))
      and. d1' =
        req_value(req(state, env), d1 => BinOp1(Bool(Or), d1, d2), d1);
      Step({
        apply: () =>
          switch (d1') {
          | Bool(true) => Bool(true)
          | Bool(false) => d2
          | _ => raise(EvaluatorError.Exception(InvalidBoxedBoolLit(d2)))
          },
        kind: BinBoolOp(Or),
        value: false,
      });
    | BinOp(Int(op), d1, d2) =>
      let. _ = otherwise(env, (d1, d2) => BinOp(Int(op), d1, d2))
      and. d1' =
        req_value(req(state, env), d1 => BinOp1(Int(op), d1, d2), d1)
      and. d2' =
        req_value(req(state, env), d2 => BinOp2(Int(op), d1, d2), d2);
      Step({
        apply: () =>
          switch (d1', d2') {
          | (Int(n1), Int(n2)) =>
            switch (op) {
            | Plus => Int(n1 + n2)
            | Minus => Int(n1 - n2)
            | Power when n2 < 0 =>
              InvalidOperation(
                BinOp(Int(op), Int(n1), Int(n2)),
                NegativeExponent,
              )
            | Power => Int(IntUtil.ipow(n1, n2))
            | Times => Int(n1 * n2)
            | Divide when n2 == 0 =>
              InvalidOperation(
                BinOp(Int(op), Int(n1), Int(n2)),
                DivideByZero,
              )
            | Divide => Int(n1 / n2)
            | LessThan => Bool(n1 < n2)
            | LessThanOrEqual => Bool(n1 <= n2)
            | GreaterThan => Bool(n1 > n2)
            | GreaterThanOrEqual => Bool(n1 >= n2)
            | Equals => Bool(n1 == n2)
            | NotEquals => Bool(n1 != n2)
            }
          | (Int(_), _) =>
            raise(EvaluatorError.Exception(InvalidBoxedIntLit(d2')))
          | _ => raise(EvaluatorError.Exception(InvalidBoxedIntLit(d1')))
          },
        kind: BinIntOp(op),
        // False so that InvalidOperations are caught and made indet by the next step
        value: false,
      });
    | BinOp(Float(op), d1, d2) =>
      let. _ = otherwise(env, (d1, d2) => BinOp(Float(op), d1, d2))
      and. d1' =
        req_value(req(state, env), d1 => BinOp1(Float(op), d1, d2), d1)
      and. d2' =
        req_value(req(state, env), d2 => BinOp2(Float(op), d1, d2), d2);
      Step({
        apply: () =>
          switch (d1', d2') {
          | (Float(n1), Float(n2)) =>
            switch (op) {
            | Plus => Float(n1 +. n2)
            | Minus => Float(n1 -. n2)
            | Power => Float(n1 ** n2)
            | Times => Float(n1 *. n2)
            | Divide => Float(n1 /. n2)
            | LessThan => Bool(n1 < n2)
            | LessThanOrEqual => Bool(n1 <= n2)
            | GreaterThan => Bool(n1 > n2)
            | GreaterThanOrEqual => Bool(n1 >= n2)
            | Equals => Bool(n1 == n2)
            | NotEquals => Bool(n1 != n2)
            }
          | (Float(_), _) =>
            raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d2')))
          | _ => raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d1')))
          },
        kind: BinFloatOp(op),
        value: true,
      });
    | BinOp(String(op), d1, d2) =>
      let. _ = otherwise(env, (d1, d2) => BinOp(String(op), d1, d2))
      and. d1' =
        req_value(req(state, env), d1 => BinOp1(String(op), d1, d2), d1)
      and. d2' =
        req_value(req(state, env), d2 => BinOp2(String(op), d1, d2), d2);
      Step({
        apply: () =>
          switch (d1', d2') {
          | (String(s1), String(s2)) =>
            switch (op) {
            | Concat => String(s1 ++ s2)
            | Equals => Bool(s1 == s2)
            }
          | (String(_), _) =>
            raise(EvaluatorError.Exception(InvalidBoxedStringLit(d2')))
          | _ => raise(EvaluatorError.Exception(InvalidBoxedStringLit(d1')))
          },
        kind: BinStringOp(op),
        value: true,
      });
    | Tuple(ds) =>
      let. _ = otherwise(env, ds => Tuple(ds))
      and. _ =
        req_all_final(req(state, env), (d1, ds) => Tuple(d1, ds), ds);
      Constructor;
    | Prj(d1, n) =>
      let. _ = otherwise(env, d1 => Prj(d1, n))
      and. d1' = req_final(req(state, env), d1 => Prj(d1, n), d1);
      Step({
        apply: () =>
          switch (d1') {
          | Tuple(ds) when n < 0 || List.length(ds) <= n =>
            raise(EvaluatorError.Exception(InvalidProjection(n)))
          | Tuple(ds) => List.nth(ds, n)
          | Cast(_, Prod(ts), Prod(_)) when n < 0 || List.length(ts) <= n =>
            raise(EvaluatorError.Exception(InvalidProjection(n)))
          | Cast(d2, Prod(ts1), Prod(ts2)) =>
            Cast(Prj(d2, n), List.nth(ts1, n), List.nth(ts2, n))
          | _ => raise(EvaluatorError.Exception(InvalidProjection(n)))
          },
        kind: Projection,
        value: false,
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
        value: true,
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
        value: true,
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
    | Match(Consistent, d1, rules) =>
      let. _ = otherwise(env, d1 => Match(Consistent, d1, rules))
      and. d1 =
        req_final(
          req(state, env),
          d1 => MatchScrut(Consistent, d1, rules),
          d1,
        );
      let rec next_rule = (
        fun
        | [] => None
        | [(dp, d2), ...rules] =>
          switch (matches(dp, d1)) {
          | Matches(env') => Some((env', d2))
          | DoesNotMatch => next_rule(rules)
          | IndetMatch => None
          }
      );
      switch (next_rule(rules)) {
      | Some((env', d2)) =>
        Step({
          apply: () => Closure(evaluate_extend_env(env', env), d2),
          kind: CaseApply,
          value: false,
        })
      | None => Indet
      };
    | Match(Inconsistent(_, _), _, _) as d =>
      let. _ = otherwise(env, d);
      Indet;
    | Closure(env', d) =>
      let. _ = otherwise(env, d => Closure(env', d))
      and. d' = req_value(req(state, env'), d1 => Closure(env', d1), d);
      Step({apply: () => d', kind: CompleteClosure, value: true});
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
      open CastHelpers; /* Cast calculus */

      let. _ = otherwise(env, d => Cast(d, t1, t2))
      and. d' = req_final(req(state, env), d => Cast(d, t1, t2), d);
      switch (ground_cases_of(t1), ground_cases_of(t2)) {
      | (Hole, Hole)
      | (Ground, Ground) =>
        /* if two types are ground and consistent, then they are eq */
        Step({apply: () => d', kind: Cast, value: true})
      | (Ground, Hole) =>
        /* can't remove the cast or do anything else here, so we're done */
        Constructor
      | (Hole, Ground) =>
        switch (d') {
        | Cast(d2, t3, Unknown(_)) =>
          /* by canonical forms, d1' must be of the form d<ty'' -> ?> */
          if (Typ.eq(t3, t2)) {
            Step({apply: () => d2, kind: Cast, value: true});
          } else {
            Step({
              apply: () => FailedCast(d', t1, t2),
              kind: Cast,
              value: false,
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
          value: false,
        })
      | (NotGroundOrHole(t1_grounded), Hole) =>
        /* ITGround rule */
        Step({
          apply: () =>
            DHExp.Cast(Cast(d', t1, t1_grounded), t1_grounded, t2),
          kind: Cast,
          value: false,
        })
      | (Ground, NotGroundOrHole(_))
      | (NotGroundOrHole(_), Ground) =>
        /* can't do anything when casting between diseq, non-hole types */
        Constructor
      | (NotGroundOrHole(_), NotGroundOrHole(_)) =>
        /* they might be eq in this case, so remove cast if so */
        if (Typ.eq(t1, t2)) {
          Step({apply: () => d', kind: Cast, value: true});
        } else {
          Constructor;
        }
      };
    | FailedCast(d1, t1, t2) =>
      let. _ = otherwise(env, d1 => FailedCast(d1, t1, t2))
      and. _ = req_final(req(state, env), d1 => FailedCast(d1, t1, t2), d1);
      Indet;
    | Filter(f1, d1) =>
      let. _ = otherwise(env, d1 => Filter(f1, d1))
      and. d1 = req_final(req(state, env), d1 => Filter(f1, d1), d1);
      Step({apply: () => d1, kind: CompleteFilter, value: true});
    };
};

let should_hide_step = (~settings: CoreSettings.Evaluation.t) =>
  fun
  | LetBind
  | Seq
  | UpdateTest
  | FunAp
  | BuiltinAp(_)
  | BinBoolOp(_)
  | BinIntOp(_)
  | BinFloatOp(_)
  | BinStringOp(_)
  | ListCons
  | ListConcat
  | CaseApply
  | Projection // TODO(Matt): We don't want to show projection to the user
  | Skip
  | Conditional(_)
  | InvalidStep => false
  | VarLookup => !settings.show_lookup_steps
  | CastAp
  | Cast => !settings.show_casts
  | CompleteClosure
  | CompleteFilter
  | FixUnwrap
  | BuiltinWrap
  | FunClosure => true;
