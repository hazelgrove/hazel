open Util;
open Sexplib.Std;
open PatternMatch;

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
  | FixClosure
  | UpdateTest
  | FunAp
  | CastAp
  | BuiltinWrap
  | BuiltinAp(string)
  | UnOp(Operators.op_un)
  | BinBoolOp(Operators.op_bin_bool)
  | BinIntOp(Operators.op_bin_int)
  | BinFloatOp(Operators.op_bin_float)
  | BinStringOp(Operators.op_bin_string)
  | Conditional(bool)
  | Projection
  | ListCons
  | ListConcat
  | CaseApply
  | CompleteClosure
  | CompleteFilter
  | Cast
  | RemoveTypeAlias
  | RemoveParens;
    | Unknown(_) => Hole
    | Bool
    | Int
    | Float
    | String
    | Var(_)
    | Rec(_)
    | Arrow({term: Unknown(_), _}, {term: Unknown(_), _})
    | List({term: Unknown(_), _}) => Ground
    | Parens(ty) => ground_cases_of(ty)
    | Prod(tys) =>
      if (List.for_all(
            fun
            | ({term: Typ.Unknown(_), _}: Typ.t) => true
            | _ => false,
            tys,
          )) {
        Ground;
      } else {
        tys |> List.length |> grounded_Prod;
      }
    | Sum(sm) =>
      sm |> ConstructorMap.is_ground(is_hole)
        ? Ground : NotGroundOrHole(Sum(grounded_Sum()) |> Typ.fresh)
    | Arrow(_, _) => grounded_Arrow
    | List(_) => grounded_List
    | Ap(_) => failwith("type application in dynamics")
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

let (let-unbox) = ((request, v), f) =>
  switch (Unboxing.unbox(request, v)) {
  | IndetMatch
  | DoesNotMatch => Indet
  | Matches(n) => f(n)
  };

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
  let req_final_or_value:
    (DHExp.t => result, EvalCtx.t => EvalCtx.t, DHExp.t) =>
    requirement((DHExp.t, bool));

  let (let.): (requirements('a, DHExp.t), 'a => rule) => result;
  let (and.):
    (requirements('a, 'c => 'b), requirement('c)) =>
    requirements(('a, 'c), 'b);
  let otherwise: (ClosureEnvironment.t, 'a) => requirements(unit, 'a);

  let update_test: (state, Id.t, TestMap.instance_report) => unit;

  let get_info_map: state => Statics.Map.t;
};

module Transition = (EV: EV_MODE) => {
  open EV;
  open DHExp;
  let (let.match) = ((env, match_result: PatternMatch.match_result), r) =>
    switch (match_result) {
    | IndetMatch
    | DoesNotMatch => Indet
    | Matches(env') => r(evaluate_extend_env(env', env))
    };

  /* Note[Matt]: For IDs, I'm currently using a fresh id
     if anything about the current node changes, if only its
     children change, we use rewrap */

  let transition = (req, state, env, d): 'a => {
    // If there is an error at this location, swap out the rule for indet.
    let info_map = get_info_map(state);
    let err_info = Statics.get_error_at(info_map, DHExp.rep_id(d));
    let (let.) =
      switch (err_info) {
      | Some(
          FreeVariable(_) | Common(NoType(_) | Inconsistent(Internal(_))),
        ) => (
          (x, _) => {
            let. _ = x;
            Indet;
          }
        )
      | Some(Common(Inconsistent(Expectation(_) | WithArrow(_))))
      | None => (let.)
      };

    // Split DHExp into term and id information
    let (term, rewrap) = DHExp.unwrap(d);
    let wrap_ctx = (term): EvalCtx.t => Term({term, ids: [rep_id(d)]});

    // Transition rules
    switch (term) {
    | Var(x) =>
      let. _ = otherwise(env, Var(x) |> rewrap);
      let to_id = Id.mk();
      Step({
        apply: () => {
          let d =
            ClosureEnvironment.lookup(env, x)
            |> OptUtil.get(() => {
                 raise(EvaluatorError.Exception(FreeInvalidVar(x)))
               });
          d |> fast_copy(to_id);
        },
        kind: VarLookup,
        value: false,
      });
    | Seq(d1, d2) =>
      let. _ = otherwise(env, d1 => Seq(d1, d2) |> rewrap)
      and. _ =
        req_final(req(state, env), d1 => Seq1(d1, d2) |> wrap_ctx, d1);
      Step({apply: () => d2, kind: Seq, value: false});
    | Let(dp, d1, d2) =>
      let. _ = otherwise(env, d1 => Let(dp, d1, d2) |> rewrap)
      and. d1' =
        req_final(req(state, env), d1 => Let1(dp, d1, d2) |> wrap_ctx, d1);
      let.match env' = (env, matches(info_map, dp, d1'));
      Step({
        apply: () => Closure(env', d2) |> fresh,
        kind: LetBind,
        value: false,
      });
    | Fun(_, _, Some(_), _) =>
      let. _ = otherwise(env, d);
      Constructor;
    | Fun(p, d1, None, v) =>
      let. _ = otherwise(env, d);
      Step({
        apply: () => Fun(p, d1, Some(env), v) |> rewrap,
        kind: FunClosure,
        value: true,
      });
    | FixF(dp, d1, None) =>
      let. _ = otherwise(env, FixF(dp, d1, None) |> rewrap);
      Step({
        apply: () => FixF(dp, d1, Some(env)) |> rewrap,
        kind: FixClosure,
        value: false,
      });
    | FixF(dp, d1, Some(env)) =>
      switch (DHPat.get_var(dp)) {
      // Simple Recursion case
      | Some(f) =>
        let. _ = otherwise(env, d);
        let env'' =
          evaluate_extend_env(
            Environment.singleton((f, FixF(dp, d1, Some(env)) |> rewrap)),
            env,
          );
        Step({
          apply: () => Closure(env'', d1) |> fresh,
          kind: FixUnwrap,
          value: false,
        });
      // Mutual Recursion case
      | None =>
        let. _ = otherwise(env, d);
        let bindings = DHPat.bound_vars(info_map, dp);
        let substitutions =
          List.map(
            binding =>
              (
                binding,
                Let(
                  dp,
                  FixF(dp, d1, Some(env)) |> rewrap,
                  Var(binding) |> fresh,
                )
                |> fresh,
              ),
            bindings,
          );
        let env'' =
          evaluate_extend_env(Environment.of_list(substitutions), env);
        Step({
          apply: () => Closure(env'', d1) |> fresh,
          kind: FixUnwrap,
          value: false,
        });
      }
    | Test(d) =>
      let. _ = otherwise(env, d => Test(d) |> rewrap)
      and. d' = req_final(req(state, env), d => Test(d) |> wrap_ctx, d);
      Step({
        apply: () =>
          switch (DHExp.term_of(d')) {
          | Bool(true) =>
            update_test(state, DHExp.rep_id(d), (d', info_map, Pass));
            Tuple([]) |> fresh;
          | Bool(false) =>
            update_test(state, DHExp.rep_id(d), (d', info_map, Fail));
            Tuple([]) |> fresh;
          /* Hack: assume if final and not Bool, then Indet; this won't catch errors in statics */
          | _ =>
            update_test(state, DHExp.rep_id(d), (d', info_map, Indet));
            Tuple([]) |> fresh;
          },
        kind: UpdateTest,
        value: true,
      });
    | Ap(dir, d1, d2) =>
      let. _ = otherwise(env, (d1, (d2, _)) => Ap(dir, d1, d2) |> rewrap)
      and. d1' =
        req_value(req(state, env), d1 => Ap1(dir, d1, d2) |> wrap_ctx, d1)
      and. (d2', d2_is_value) =
        req_final_or_value(
          req(state, env),
          d2 => Ap2(dir, d1, d2) |> wrap_ctx,
          d2,
        );
      switch (DHExp.term_of(d1')) {
      | Constructor(_) => Constructor
      | Fun(dp, d3, Some(env'), _) =>
        let.match env'' = (env', matches(info_map, dp, d2'));
        Step({
          apply: () => Closure(env'', d3) |> fresh,
          kind: FunAp,
          value: false,
        });
      | Cast(
          d3',
          {term: Arrow(ty1, ty2), _},
          {term: Arrow(ty1', ty2'), _},
        ) =>
        Step({
          apply: () =>
            Cast(
              Ap(dir, d3', Cast(d2', ty1', ty1) |> fresh) |> fresh,
              ty2,
              ty2',
            )
            |> fresh,
          kind: CastAp,
          value: false,
        })
      | BuiltinFun(ident) =>
        if (d2_is_value) {
          Step({
            apply: () => {
              let builtin =
                VarMap.lookup(Builtins.forms_init, ident)
                |> OptUtil.get(() => {
                     raise(EvaluatorError.Exception(InvalidBuiltin(ident)))
                   });
              builtin(d2);
            },
            kind: BuiltinAp(ident),
            value: false // Not necessarily a value because of InvalidOperations
          });
        } else {
          Indet;
        }
      | _ =>
        Step({
          apply: () => {
            raise(EvaluatorError.Exception(InvalidBoxedFun(d1')));
          },
          kind: InvalidStep,
          value: true,
        })
      };
    | Bool(_)
    | Int(_)
    | Float(_)
    | String(_)
    | Constructor(_)
    | BuiltinFun(_) =>
      let. _ = otherwise(env, d);
      Constructor;
    | If(c, d1, d2) =>
      let. _ = otherwise(env, c => If(c, d1, d2) |> rewrap)
      and. c' =
        req_value(req(state, env), c => If1(c, d1, d2) |> wrap_ctx, c);
      switch (DHExp.term_of(c')) {
      | Bool(b) =>
        Step({
          apply: () => {
            b ? d1 : d2;
          },
          // Attach c' to indicate which branch taken.
          kind: Conditional(b),
          value: false,
        })
      // Use a seperate case for invalid conditionals. Makes extracting the bool from BoolLit (above) easier.
      | _ =>
        Step({
          apply: () => {
            raise(EvaluatorError.Exception(InvalidBoxedBoolLit(c')));
          },
          kind: InvalidStep,
          value: true,
        })
      };
    | UnOp(Meta(Unquote), _) =>
      let. _ = otherwise(env, d);
      Indet;
    | UnOp(Int(Minus), d1) =>
      let. _ = otherwise(env, d1 => UnOp(Int(Minus), d1) |> rewrap)
      and. d1' =
        req_value(
          req(state, env),
          c => UnOp(Int(Minus), c) |> wrap_ctx,
          d1,
        );
      let-unbox n = (Int, d1');
      Step({
        apply: () => Int(- n) |> fresh,
        kind: UnOp(Int(Minus)),
        value: true,
      });
    | UnOp(Bool(Not), d1) =>
      let. _ = otherwise(env, d1 => UnOp(Bool(Not), d1) |> rewrap)
      and. d1' =
        req_value(
          req(state, env),
          c => UnOp(Bool(Not), c) |> wrap_ctx,
          d1,
        );
      let-unbox b = (Bool, d1');
      Step({
        apply: () => Bool(!b) |> fresh,
        kind: UnOp(Bool(Not)),
        value: true,
      });
    | BinOp(Bool(And), d1, d2) =>
      let. _ = otherwise(env, d1 => BinOp(Bool(And), d1, d2) |> rewrap)
      and. d1' =
        req_value(
          req(state, env),
          d1 => BinOp1(Bool(And), d1, d2) |> wrap_ctx,
          d1,
        );
      let-unbox b1 = (Bool, d1');
      Step({
        apply: () => b1 ? d2 : Bool(false) |> fresh,
        kind: BinBoolOp(And),
        value: false,
      });
    | BinOp(Bool(Or), d1, d2) =>
      let. _ = otherwise(env, d1 => BinOp(Bool(Or), d1, d2) |> rewrap)
      and. d1' =
        req_value(
          req(state, env),
          d1 => BinOp1(Bool(Or), d1, d2) |> wrap_ctx,
          d1,
        );
      let-unbox b1 = (Bool, d1');
      Step({
        apply: () => b1 ? Bool(true) |> fresh : d2,
        kind: BinBoolOp(Or),
        value: false,
      });
    | BinOp(Int(op), d1, d2) =>
      let. _ = otherwise(env, (d1, d2) => BinOp(Int(op), d1, d2) |> rewrap)
      and. d1' =
        req_value(
          req(state, env),
          d1 => BinOp1(Int(op), d1, d2) |> wrap_ctx,
          d1,
        )
      and. d2' =
        req_value(
          req(state, env),
          d2 => BinOp2(Int(op), d1, d2) |> wrap_ctx,
          d2,
        );
      let-unbox n1 = (Int, d1');
      let-unbox n2 = (Int, d2');
      Step({
        apply: () =>
          (
            switch (op) {
            | Plus => Int(n1 + n2)
            | Minus => Int(n1 - n2)
            | Power when n2 < 0 =>
              DynamicErrorHole(
                BinOp(Int(op), d1', d2') |> rewrap,
                NegativeExponent,
              )
            | Power => Int(IntUtil.ipow(n1, n2))
            | Times => Int(n1 * n2)
            | Divide when n2 == 0 =>
              DynamicErrorHole(
                BinOp(Int(op), d1', d1') |> rewrap,
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
          )
          |> fresh,
        kind: BinIntOp(op),
        // False so that InvalidOperations are caught and made indet by the next step
        value: false,
      });
    | BinOp(Float(op), d1, d2) =>
      let. _ =
        otherwise(env, (d1, d2) => BinOp(Float(op), d1, d2) |> rewrap)
      and. d1' =
        req_value(
          req(state, env),
          d1 => BinOp1(Float(op), d1, d2) |> wrap_ctx,
          d1,
        )
      and. d2' =
        req_value(
          req(state, env),
          d2 => BinOp2(Float(op), d1, d2) |> wrap_ctx,
          d2,
        );
      let-unbox n1 = (Float, d1');
      let-unbox n2 = (Float, d2');
      Step({
        apply: () =>
          (
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
          )
          |> fresh,

        kind: BinFloatOp(op),
        value: true,
      });
    | BinOp(String(op), d1, d2) =>
      let. _ =
        otherwise(env, (d1, d2) => BinOp(String(op), d1, d2) |> rewrap)
      and. d1' =
        req_value(
          req(state, env),
          d1 => BinOp1(String(op), d1, d2) |> wrap_ctx,
          d1,
        )
      and. d2' =
        req_value(
          req(state, env),
          d2 => BinOp2(String(op), d1, d2) |> wrap_ctx,
          d2,
        );
      let-unbox s1 = (String, d1');
      let-unbox s2 = (String, d2');
      Step({
        apply: () =>
          switch (op) {
          | Concat => String(s1 ++ s2) |> fresh
          | Equals => Bool(s1 == s2) |> fresh
          },
        kind: BinStringOp(op),
        value: true,
      });
    | Tuple(ds) =>
      let. _ = otherwise(env, ds => Tuple(ds) |> rewrap)
      and. _ =
        req_all_final(
          req(state, env),
          (d1, ds) => Tuple(d1, ds) |> wrap_ctx,
          ds,
        );
      Constructor;
    | Cons(d1, d2) =>
      let. _ = otherwise(env, (d1, d2) => Cons(d1, d2) |> rewrap)
      and. d1' =
        req_final(req(state, env), d1 => Cons1(d1, d2) |> wrap_ctx, d1)
      and. d2' =
        req_value(req(state, env), d2 => Cons2(d1, d2) |> wrap_ctx, d2);
      let-unbox ds = (List, d2');
      Step({
        apply: () => ListLit([d1', ...ds]) |> fresh,
        kind: ListCons,
        value: true,
      });
    | ListConcat(d1, d2) =>
      let. _ = otherwise(env, (d1, d2) => ListConcat(d1, d2) |> rewrap)
      and. d1' =
        req_value(
          req(state, env),
          d1 => ListConcat1(d1, d2) |> wrap_ctx,
          d1,
        )
      and. d2' =
        req_value(
          req(state, env),
          d2 => ListConcat2(d1, d2) |> wrap_ctx,
          d2,
        );
      let-unbox ds1 = (List, d1');
      let-unbox ds2 = (List, d2');
      Step({
        apply: () => ListLit(ds1 @ ds2) |> fresh,
        kind: ListConcat,
        value: true,
      });
    | ListLit(ds) =>
      let. _ = otherwise(env, ds => ListLit(ds) |> rewrap)
      and. _ =
        req_all_final(
          req(state, env),
          (d1, ds) => ListLit(d1, ds) |> wrap_ctx,
          ds,
        );
      Constructor;
    | Match(d1, rules) =>
      let. _ = otherwise(env, d1 => Match(d1, rules) |> rewrap)
      and. d1 =
        req_final(
          req(state, env),
          d1 => MatchScrut(d1, rules) |> wrap_ctx,
          d1,
        );
      let rec next_rule = (
        fun
        | [] => None
        | [(dp, d2), ...rules] =>
          switch (matches(info_map, dp, d1)) {
          | Matches(env') => Some((env', d2))
          | DoesNotMatch => next_rule(rules)
          | IndetMatch => None
          }
      );
      switch (next_rule(rules)) {
      | Some((env', d2)) =>
        Step({
          apply: () => Closure(evaluate_extend_env(env', env), d2) |> fresh,
          kind: CaseApply,
          value: false,
        })
      | None => Indet
      };
    | Closure(env', d) =>
      let. _ = otherwise(env, d => Closure(env', d) |> rewrap)
      and. d' =
        req_value(req(state, env'), d1 => Closure(env', d1) |> wrap_ctx, d);
      Step({apply: () => d', kind: CompleteClosure, value: true});
    | StaticErrorHole(sid, d1) =>
      let. _ = otherwise(env, d1 => StaticErrorHole(sid, d1) |> rewrap)
      and. _ =
        req_final(
          req(state, env),
          d1 => StaticErrorHole(sid, d1) |> wrap_ctx,
          d1,
        );
      Indet;
    | MultiHole(_) =>
      let. _ = otherwise(env, d);
      // and. _ =
      //   req_all_final(
      //     req(state, env),
      //     (d1, ds) => MultiHole(d1, ds) |> wrap_ctx,
      //     ds,
      //   );
      Indet;
    | EmptyHole
    | Invalid(_)
    | DynamicErrorHole(_) =>
      let. _ = otherwise(env, d);
      Indet;
    | Cast(d, t1, t2) =>
      let. _ = otherwise(env, d => Cast(d, t1, t2) |> rewrap)
      and. d' =
        req_final(req(state, env), d => Cast(d, t1, t2) |> wrap_ctx, d);
      switch (Casts.transition(Cast(d', t1, t2) |> rewrap)) {
      | Some(d) => Step({apply: () => d, kind: Cast, value: false})
      | None => Constructor
      };
    | FailedCast(d1, t1, t2) =>
      let. _ = otherwise(env, d1 => FailedCast(d1, t1, t2) |> rewrap)
      and. _ =
        req_final(
          req(state, env),
          d1 => FailedCast(d1, t1, t2) |> wrap_ctx,
          d1,
        );
      Indet;
    | Parens(d) =>
      let. _ = otherwise(env, d);
      Step({apply: () => d, kind: RemoveParens, value: false});
    | TyAlias(_, _, d) =>
      let. _ = otherwise(env, d);
      Step({apply: () => d, kind: RemoveTypeAlias, value: false});
    | Filter(f1, d1) =>
      let. _ = otherwise(env, d1 => Filter(f1, d1) |> rewrap)
      and. d1 =
        req_final(req(state, env), d1 => Filter(f1, d1) |> wrap_ctx, d1);
      Step({apply: () => d1, kind: CompleteFilter, value: true});
    };
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
  | UnOp(_)
  | ListCons
  | ListConcat
  | CaseApply
  | Projection // TODO(Matt): We don't want to show projection to the user
  | Conditional(_)
  | RemoveTypeAlias
  | InvalidStep => false
  | VarLookup => !settings.show_lookup_steps
  | CastAp
  | Cast => !settings.show_casts
  | FixUnwrap => !settings.show_fixpoints
  | CompleteClosure
  | CompleteFilter
  | BuiltinWrap
  | FunClosure
  | FixClosure
  | RemoveParens => true;
