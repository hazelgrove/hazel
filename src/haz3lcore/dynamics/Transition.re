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
        Step({expr: d2, state, kind: Seq, final: false});


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
    are unsure, it is always safe to put `is_value: false`.

    `is_value: true` guarantees:
      - if all requirements are values, then the output will be a value
      - if some requirements are indet, then the output will be indet

    A value is either a literal, or a function with a closure, or a type function.
    (functions without closures immediately inside them do not count as values).
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
  | TypFunAp
  | FunAp
  | DeferredAp
  | CastTypAp
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
      expr: DHExp.t,
      state_update: unit => unit,
      kind: step_kind,
      is_value: bool,
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
};

module Transition = (EV: EV_MODE) => {
  open EV;
  open DHExp;

  // Default state update
  let state_update = () => ();

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
    // Split DHExp into term and id information
    let (term, rewrap) = DHExp.unwrap(d);
    let wrap_ctx = (term): EvalCtx.t => Term({term, ids: [rep_id(d)]});

    // Transition rules
    switch (term) {
    | Var(x) =>
      let. _ = otherwise(env, Var(x) |> rewrap);
      switch (ClosureEnvironment.lookup(env, x)) {
      | Some(d) =>
        Step({
          expr: d |> fast_copy(Id.mk()),
          state_update,
          kind: VarLookup,
          is_value: false,
        })
      | None => Indet
      };
    | Seq(d1, d2) =>
      let. _ = otherwise(env, d1 => Seq(d1, d2) |> rewrap)
      and. _ =
        req_final(req(state, env), d1 => Seq1(d1, d2) |> wrap_ctx, d1);
      Step({expr: d2, state_update, kind: Seq, is_value: false});
    | Let(dp, d1, d2) =>
      let. _ = otherwise(env, d1 => Let(dp, d1, d2) |> rewrap)
      and. d1' =
        req_final(req(state, env), d1 => Let1(dp, d1, d2) |> wrap_ctx, d1);
      let.match env' = (env, matches(dp, d1'));
      Step({
        expr: Closure(env', d2) |> fresh,
        state_update,
        kind: LetBind,
        is_value: false,
      });
    | Theorem(dp, d1, d2) =>
      let. _ = otherwise(env, d1 => Theorem(dp, d1, d2) |> rewrap)
      and. d1' =
        req_final(req(state, env), d1 => Let1(dp, d1, d2) |> wrap_ctx, d1);
      let.match env' = (env, matches(dp, d1'));
      Step({
        expr: Closure(env', d2) |> fresh,
        state_update,
        kind: LetBind,
        is_value: false,
      });
    | TypFun(_)
    | Fun(_, _, Some(_), _) =>
      let. _ = otherwise(env, d);
      Constructor;
    | Fun(p, d1, None, v) =>
      let. _ = otherwise(env, d);
      Step({
        expr: Fun(p, d1, Some(env), v) |> rewrap,
        state_update,
        kind: FunClosure,
        is_value: true,
      });
    | FixF(dp, d1, None) =>
      let. _ = otherwise(env, FixF(dp, d1, None) |> rewrap);
      Step({
        expr: FixF(dp, d1, Some(env)) |> rewrap,
        state_update,
        kind: FixClosure,
        is_value: false,
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
          expr: Closure(env'', d1) |> fresh,
          state_update,
          kind: FixUnwrap,
          is_value: false,
        });
      // Mutual Recursion case
      | None =>
        let. _ = otherwise(env, d);
        let bindings = DHPat.bound_vars(dp);
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
          expr: Closure(env'', d1) |> fresh,
          state_update,
          kind: FixUnwrap,
          is_value: false,
        });
      }
    | Test(d'') =>
      let. _ = otherwise(env, ((d, _)) => Test(d) |> rewrap)
      and. (d', is_value) =
        req_final_or_value(req(state, env), d => Test(d) |> wrap_ctx, d'');
      let result: TestStatus.t =
        if (is_value) {
          switch (Unboxing.unbox(Bool, d')) {
          | DoesNotMatch
          | IndetMatch => Indet
          | Matches(b) => b ? Pass : Fail
          };
        } else {
          Indet;
        };
      Step({
        expr: Tuple([]) |> fresh,
        state_update: () =>
          update_test(state, DHExp.rep_id(d), (d', result)),
        kind: UpdateTest,
        is_value: true,
      });
    | TypAp(d, tau) =>
      let. _ = otherwise(env, d => TypAp(d, tau) |> rewrap)
      and. d' =
        req_value(req(state, env), d => TypAp(d, tau) |> wrap_ctx, d);
      switch (DHExp.term_of(d')) {
      | TypFun(utpat, tfbody, name) =>
        /* Rule ITTLam */
        Step({
          expr:
            DHExp.assign_name_if_none(
              /* Inherit name for user clarity */
              DHExp.ty_subst(tau, utpat, tfbody),
              Option.map(
                x => x ++ "@<" ++ Typ.pretty_print(tau) ++ ">",
                name,
              ),
            ),
          state_update,
          kind: TypFunAp,
          is_value: false,
        })
      | Cast(
          d'',
          {term: Forall(tp1, _), _} as t1,
          {term: Forall(tp2, _), _} as t2,
        ) =>
        /* Rule ITTApCast */
        Step({
          expr:
            Cast(
              TypAp(d'', tau) |> Exp.fresh,
              Typ.subst(tau, tp1, t1),
              Typ.subst(tau, tp2, t2),
            )
            |> Exp.fresh,
          state_update,
          kind: CastTypAp,
          is_value: false,
        })
      | _ => raise(EvaluatorError.Exception(InvalidBoxedTypFun(d')))
      };
    | DeferredAp(d1, ds) =>
      let. _ = otherwise(env, (d1, ds) => DeferredAp(d1, ds) |> rewrap)
      and. _ =
        req_final(
          req(state, env),
          d1 => DeferredAp1(d1, ds) |> wrap_ctx,
          d1,
        )
      and. _ =
        req_all_final(
          req(state, env),
          (d2, ds) => DeferredAp2(d1, d2, ds) |> wrap_ctx,
          ds,
        );
      Constructor;
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
        let.match env'' = (env', matches(dp, d2'));
        Step({
          expr: Closure(env'', d3) |> fresh,
          state_update,
          kind: FunAp,
          is_value: false,
        });
      | Cast(
          d3',
          {term: Arrow(ty1, ty2), _},
          {term: Arrow(ty1', ty2'), _},
        ) =>
        Step({
          expr:
            Cast(
              Ap(dir, d3', Cast(d2', ty1', ty1) |> fresh) |> fresh,
              ty2,
              ty2',
            )
            |> fresh,
          state_update,
          kind: CastAp,
          is_value: false,
        })
      | BuiltinFun(ident) =>
        if (d2_is_value) {
          Step({
            expr: {
              let builtin =
                VarMap.lookup(Builtins.forms_init, ident)
                |> OptUtil.get(() => {
                     /* This exception should never be raised because there is
                        no way for the user to create a BuiltinFun. They are all
                        inserted into the context before evaluation. */
                     raise(
                       EvaluatorError.Exception(InvalidBuiltin(ident)),
                     )
                   });
              builtin(d2');
            },
            state_update,
            kind: BuiltinAp(ident),
            is_value: false // Not necessarily a value because of InvalidOperations
          });
        } else {
          Indet;
        }
      /* This case isn't currently used because deferrals are elaborated away */
      | DeferredAp(d3, d4s) =>
        let n_args =
          List.length(
            List.map(
              fun
              | {term: Deferral(_), _} => true
              | _ => false: Exp.t => bool,
              d4s,
            ),
          );
        let-unbox args = (Tuple(n_args), d2);
        let new_args = {
          let rec go = (deferred, args) =>
            switch ((deferred: list(Exp.t))) {
            | [] => []
            | [{term: Deferral(_), _}, ...deferred] =>
              /* I can use List.hd and List.tl here because let-unbox ensure that
                 there are the correct number of args */
              [List.hd(args), ...go(deferred, List.tl(args))]
            | [x, ...deferred] => [x, ...go(deferred, args)]
            };
          go(d4s, args);
        };
        Step({
          expr: Ap(Forward, d3, Tuple(new_args) |> fresh) |> fresh,
          state_update,
          kind: DeferredAp,
          is_value: false,
        });
      | Cast(_)
      | FailedCast(_) => Indet
      | FixF(_) =>
        print_endline(Exp.show(d1));
        print_endline(Exp.show(d1'));
        print_endline("FIXF");
        failwith("FixF in Ap");
      | _ =>
        Step({
          expr: {
            raise(EvaluatorError.Exception(InvalidBoxedFun(d1')));
          },
          state_update,
          kind: InvalidStep,
          is_value: true,
        })
      };
    | Deferral(_) =>
      let. _ = otherwise(env, d);
      Indet;
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
      let-unbox b = (Bool, c');
      Step({
        expr: {
          b ? d1 : d2;
        },
        state_update,
        // Attach c' to indicate which branch taken.
        kind: Conditional(b),
        is_value: false,
      });
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
        expr: Int(- n) |> fresh,
        state_update,
        kind: UnOp(Int(Minus)),
        is_value: true,
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
        expr: Bool(!b) |> fresh,
        state_update,
        kind: UnOp(Bool(Not)),
        is_value: true,
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
        expr: b1 ? d2 : Bool(false) |> fresh,
        state_update,
        kind: BinBoolOp(And),
        is_value: false,
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
        expr: b1 ? Bool(true) |> fresh : d2,
        state_update,
        kind: BinBoolOp(Or),
        is_value: false,
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
        expr:
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
                BinOp(Int(op), d1', d2') |> rewrap,
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
        state_update,
        kind: BinIntOp(op),
        // False so that InvalidOperations are caught and made indet by the next step
        is_value: false,
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
        expr:
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
        state_update,
        kind: BinFloatOp(op),
        is_value: true,
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
        expr:
          switch (op) {
          | Concat => String(s1 ++ s2) |> fresh
          | Equals => Bool(s1 == s2) |> fresh
          },
        state_update,
        kind: BinStringOp(op),
        is_value: true,
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
        expr: ListLit([d1', ...ds]) |> fresh,
        state_update,
        kind: ListCons,
        is_value: true,
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
        expr: ListLit(ds1 @ ds2) |> fresh,
        state_update,
        kind: ListConcat,
        is_value: true,
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
          switch (matches(dp, d1)) {
          | Matches(env') => Some((env', d2))
          | DoesNotMatch => next_rule(rules)
          | IndetMatch => None
          }
      );
      switch (next_rule(rules)) {
      | Some((env', d2)) =>
        Step({
          expr: Closure(evaluate_extend_env(env', env), d2) |> fresh,
          state_update,
          kind: CaseApply,
          is_value: false,
        })
      | None => Indet
      };
    | Closure(env', d) =>
      let. _ = otherwise(env, d => Closure(env', d) |> rewrap)
      and. d' =
        req_final(req(state, env'), d1 => Closure(env', d1) |> wrap_ctx, d);
      Step({expr: d', state_update, kind: CompleteClosure, is_value: true});
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
      | Some(d) => Step({expr: d, state_update, kind: Cast, is_value: false})
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
      Step({expr: d, state_update, kind: RemoveParens, is_value: false});
    | TyAlias(_, _, d) =>
      let. _ = otherwise(env, d);
      Step({expr: d, state_update, kind: RemoveTypeAlias, is_value: false});
    | Filter(f1, d1) =>
      let. _ = otherwise(env, d1 => Filter(f1, d1) |> rewrap)
      and. d1 =
        req_final(req(state, env), d1 => Filter(f1, d1) |> wrap_ctx, d1);
      Step({expr: d1, state_update, kind: CompleteFilter, is_value: true});
    };
  };
};

let should_hide_step_kind = (~settings: CoreSettings.Evaluation.t) =>
  fun
  | LetBind
  | Seq
  | UpdateTest
  | TypFunAp
  | FunAp
  | DeferredAp
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
  | CastTypAp
  | CastAp
  | Cast => !settings.show_casts
  | FixUnwrap => !settings.show_fixpoints
  | CompleteClosure
  | CompleteFilter
  | BuiltinWrap
  | FunClosure
  | FixClosure
  | RemoveParens => true;

let stepper_justification: step_kind => string =
  fun
  | LetBind => "substitution"
  | Seq => "sequence"
  | FixUnwrap => "unroll fixpoint"
  | UpdateTest => "update test"
  | TypFunAp => "apply type function"
  | FunAp => "apply function"
  | DeferredAp => "deferred application"
  | BuiltinWrap => "wrap builtin"
  | BuiltinAp(s) => "evaluate " ++ s
  | UnOp(Int(Minus))
  | BinIntOp(Plus | Minus | Times | Power | Divide)
  | BinFloatOp(Plus | Minus | Times | Power | Divide) => "arithmetic"
  | BinIntOp(LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual)
  | BinFloatOp(LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual) => "comparison"
  | BinIntOp(Equals | NotEquals)
  | BinFloatOp(Equals | NotEquals)
  | BinStringOp(Equals) => "check equality"
  | BinStringOp(Concat) => "string manipulation"
  | UnOp(Bool(Not))
  | BinBoolOp(_) => "boolean logic"
  | Conditional(_) => "conditional"
  | ListCons => "list manipulation"
  | ListConcat => "list manipulation"
  | CaseApply => "case selection"
  | Projection => "projection" // TODO(Matt): We don't want to show projection to the user
  | InvalidStep => "error"
  | VarLookup => "variable lookup"
  | CastTypAp
  | CastAp
  | Cast => "cast calculus"
  | FixClosure => "fixpoint closure"
  | CompleteFilter => "complete filter"
  | CompleteClosure => "complete closure"
  | FunClosure => "function closure"
  | RemoveTypeAlias => "define type"
  | RemoveParens => "remove parentheses"
  | UnOp(Meta(Unquote)) => failwith("INVALID STEP");
