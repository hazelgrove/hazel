open Util;
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
    which indicate that in order to evaluate the step, <d1> must be final. Note that
    if successful, d1' will be the fully-evaluated version of d1. The sub-expressions
    are all enumerated by the <i> field, so i=0 indicates that it is the first
    sub-expression, i=1 the second etc.

    If there are any sub-expressions that are not requirements, and therefore not
    guaranteed to be run, you should add a `let.wrap_closure () = env` to ensure that
    the closure isn't lost if the expression is indet.

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
  | LetBind(string)
  | WrapClosure
  | FixUnwrap
  | FixClosure
  | UpdateTest
  | TypFunAp
  | FunAp
  | DeferredAp
  | AscriptionTypAp
  | AscriptionAp
  | BuiltinWrap
  | BuiltinAp(string)
  | UnOp(Operators.op_un)
  | BinOp(Operators.op_bin)
  | MarkIncomparable
  | Dot
  | Conditional(bool)
  | Projection
  | TupleExtension
  | ListCons
  | ListConcat
  | CaseApply
  | CompleteClosure
  | CompleteFilter
  | Ascription
  | RemoveTypeAlias
  | RemoveUse
  | RemoveParens;
let evaluate_extend_env = ClosureEnvironment.extend_eval;

type rule =
  | Step({
      expr: DHExp.t,
      state_update: unit => unit,
      kind: step_kind,
      is_value: bool,
    })
  | Constructor
  | Indet
  | Value;

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

  let update_test: (state, Id.t, TestMap.instance_report) => unit;

  let update_probe: (state, Dynamics.Probe.Closure.t) => unit;
};

module Transition = (EV: EV_MODE) => {
  open EV;
  open DHExp;
  open IdTagged.FreshGrammar.Exp;

  // Default state update
  let state_update = () => ();

  let (let.match) = ((env, match_result: match_result, call_stack), r) =>
    switch (match_result) {
    | IndetMatch
    | DoesNotMatch => Indet
    | Matches(env') => r(evaluate_extend_env(env', env, ~call_stack))
    };

  let capture_closures =
      (env: ClosureEnvironment.t, state: state, closures, ()): unit =>
    List.iter(
      closure => update_probe(state, closure(env.call_stack)),
      closures,
    );

  /* Helper function to wrap a closure around an expression. Required for functions, but also for
     things like if-then-else expressions where the scrutinee is indet, and for hole closures */
  let wrap_closure_when_done = (~in_closure, expr, env, r: rule) =>
    switch (in_closure, r) {
    | (_, Step(_)) => r
    | (None, Constructor | Indet | Value) =>
      Step({
        expr: closure(env, expr),
        state_update,
        kind: WrapClosure,
        is_value: false,
      })
    | (Some(f), Constructor | Indet | Value) =>
      f();
      r;
    };

  /* Note(zhiyao): The purpose of transition for derivation terms is much useless
     compared to that for hazel expressions. The only thing here is to calculate
     1) variables reference and
     2) contexts terms cons and concat */

  let drv_transition = (env, d: t): t => {
    let rec go_exp = exp => {
      let (term, rewrap) = Drv.Exp.unwrap(exp);
      let term: Drv.Exp.term =
        switch (term) {
        | Hole(s) => Hole(s)
        | Var(x) => Var(x)
        | Quote(x) =>
          switch (ClosureEnvironment.lookup(env, x)) {
          | Some(d) =>
            switch (DHExp.term_of(d)) {
            | DrvExp(Exp({term, _}), _) => term
            | _ => Hole(AbbrNotDrvTerm)
            }
          | None => Hole(AbbrNotFound)
          }
        | Parens(e) => Drv.Exp.term_of(go_exp(e))
        | Val(e) => Val(go_exp(e))
        | Eval(e1, e2) => Eval(go_exp(e1), go_exp(e2))
        | Entail(ctx, p) => Entail(go_exp(ctx), go_exp(p))
        | Consistent(t1, t2) => Consistent(go_typ(t1), go_typ(t2))
        | MatchedArrow(t1, t2) => MatchedArrow(go_typ(t1), go_typ(t2))
        | MatchedProd(t1, t2) => MatchedProd(go_typ(t1), go_typ(t2))
        | MatchedSum(t1, t2) => MatchedSum(go_typ(t1), go_typ(t2))
        | Ctx(es) => Ctx(List.map(go_exp, es))
        | Cons(p, ctx) =>
          switch (Drv.Exp.term_of(go_exp(ctx))) {
          | Ctx(es) => Ctx(Drv.Exp.cons_ctx(es, go_exp(p)))
          | _ => Cons(p, ctx)
          }
        | Concat(e1, e2) =>
          switch (
            Drv.Exp.term_of(go_exp(e1)),
            Drv.Exp.term_of(go_exp(e2)),
          ) {
          | (Ctx(es1), Ctx(es2)) =>
            Ctx(List.fold_left(Drv.Exp.cons_ctx, es2, es1))
          | _ => Concat(go_exp(e1), go_exp(e2))
          }
        | Type(t) => Type(go_typ(t))
        | HasType(e, t) => HasType(go_exp(e), go_typ(t))
        | Syn(e, t) => Syn(go_exp(e), go_typ(t))
        | Ana(e, t) => Ana(go_exp(e), go_typ(t))
        | And(p1, p2) => And(go_exp(p1), go_exp(p2))
        | Or(p1, p2) => Or(go_exp(p1), go_exp(p2))
        | Impl(p1, p2) => Impl(go_exp(p1), go_exp(p2))
        | Truth => Truth
        | Falsity => Falsity
        | NumLit(n) => NumLit(n)
        | Neg(e) => Neg(go_exp(e))
        | BinOp(op, e1, e2) => BinOp(op, go_exp(e1), go_exp(e2))
        | True => True
        | False => False
        | If(e1, e2, e3) => If(go_exp(e1), go_exp(e2), go_exp(e3))
        | Let(x, e1, e2) => Let(go_pat(x), go_exp(e1), go_exp(e2))
        | Fix(x, e) => Fix(go_pat(x), go_exp(e))
        | Fun(x, e) => Fun(go_pat(x), go_exp(e))
        | Ap(e1, e2) => Ap(go_exp(e1), go_exp(e2))
        | Tuple(es) => Tuple(List.map(go_exp, es))
        | Pair(e1, e2) => Pair(go_exp(e1), go_exp(e2))
        | Triv => Triv
        | PrjL(e) => PrjL(go_exp(e))
        | PrjR(e) => PrjR(go_exp(e))
        | InjL(e) => InjL(go_exp(e))
        | InjR(e) => InjR(go_exp(e))
        | Case(e, x, e1, y, e2) =>
          Case(go_exp(e), go_pat(x), go_exp(e1), go_pat(y), go_exp(e2))
        | Roll(e) => Roll(go_exp(e))
        | Unroll(e) => Unroll(go_exp(e))
        | ExpHole => ExpHole
        };
      term |> rewrap;
    }
    and go_typ = typ => {
      let (term, rewrap) = Drv.Typ.unwrap(typ);
      let term: Drv.Typ.term =
        switch (term) {
        | Hole(s) => Hole(s)
        | Quote(x) =>
          switch (ClosureEnvironment.lookup(env, x)) {
          | Some(d) =>
            switch (DHExp.term_of(d)) {
            | DrvExp(Typ({term, _}), _) => term
            | _ => Hole(AbbrNotDrvTerm)
            }
          | None => Hole(AbbrNotFound)
          }
        | Num => Num
        | Bool => Bool
        | Arrow(t1, t2) => Arrow(go_typ(t1), go_typ(t2))
        | Prod(t1, t2) => Prod(go_typ(t1), go_typ(t2))
        | Unit => Unit
        | Sum(t1, t2) => Sum(go_typ(t1), go_typ(t2))
        | Var(x) => Var(x)
        | Rec(x, t) => Rec(x, go_typ(t))
        | Parens(t) => Drv.Typ.term_of(go_typ(t))
        | TypHole => TypHole
        };
      term |> rewrap;
    }
    and go_pat = pat => {
      let (term, rewrap) = Drv.Pat.unwrap(pat);
      let term: Drv.Pat.term =
        switch (term) {
        | Hole(s) => Hole(s)
        | Quote(x) =>
          switch (ClosureEnvironment.lookup(env, x)) {
          | Some(d) =>
            switch (DHExp.term_of(d)) {
            | DrvExp(Pat({term, _}), _) => term
            | _ => Hole(AbbrNotDrvTerm)
            }
          | None => Hole(AbbrNotFound)
          }
        | Var(x) => Var(x)
        | Cast(p, t) => Cast(go_pat(p), go_typ(t))
        | InjL(p) => InjL(go_pat(p))
        | InjR(p) => InjR(go_pat(p))
        | Pair(p1, p2) => Pair(go_pat(p1), go_pat(p2))
        | Parens(p) => Drv.Pat.term_of(go_pat(p))
        };
      term |> rewrap;
    }
    and go_tpat = tpat => {
      let (term, rewrap) = Drv.TPat.unwrap(tpat);
      let term: Drv.TPat.term =
        switch (term) {
        | Hole(s) => Hole(s)
        | Quote(x) =>
          switch (ClosureEnvironment.lookup(env, x)) {
          | Some(d) =>
            switch (DHExp.term_of(d)) {
            | DrvExp(TPat({term, _}), _) => term
            | _ => Hole(AbbrNotDrvTerm)
            }
          | None => Hole(AbbrNotFound)
          }
        | Var(x) => Var(x)
        };
      term |> rewrap;
    };
    let (term, rewrap) = IdTagged.unwrap(d);
    let term: term =
      switch (term) {
      | DrvExp(drv, s) =>
        switch (drv) {
        | Exp(e) => DrvExp(Exp(go_exp(e)), s)
        | Typ(t) => DrvExp(Typ(go_typ(t)), s)
        | Pat(p) => DrvExp(Pat(go_pat(p)), s)
        | TPat(t) => DrvExp(TPat(go_tpat(t)), s)
        }
      | _ => term
      };
    term |> rewrap;
  };

  /* Note[Matt]: For IDs, I'm currently using a fresh id
     if anything about the current node changes, if only its
     children change, we use rewrap */

  let transition =
      (
        req:
          (
            ~in_closure: unit => unit=?,
            state,
            ClosureEnvironment.t,
            DHExp.t
          ) =>
          'a,
        ~mode: [
           | `Substitution
           | `Environment
         ],
        ~in_closure=?,
        state,
        env, // Empty in substitution mode
        d,
      )
      : EV.result => {
    // Split DHExp into term and id information
    let (term, rewrap) = DHExp.unwrap(d);
    let wrap_ctx = (term): EvalCtx.t =>
      Term({
        term,
        ids: [rep_id(d)],
      });

    let (let.wrap_closure) = (env, f: unit => rule) =>
      switch (mode) {
      | `Environment => wrap_closure_when_done(~in_closure, d, env, f())
      | `Substitution => f()
      };

    let subst_env = (env, d) =>
      switch (mode) {
      | `Environment => Closure(env, d) |> fresh
      | `Substitution => d |> Substitution.subst(env.env)
      };

    // Transition rules
    switch (term) {
    | Var(x) =>
      switch (mode) {
      | `Environment =>
        let. _ = otherwise(env, Var(x) |> rewrap);
        switch (ClosureEnvironment.lookup(env, x)) {
        | Some(d) =>
          let is_value =
            switch (d |> Exp.term_of) {
            | FixF(_, _, _) => false // fixpoints aren't final
            | Let(_, _, _) => false // could be mutually-recursive fixpoint
            | _ => true // all other closure entries should be final
            };
          Step({
            expr: d |> fast_copy(Id.mk()),
            state_update,
            kind: VarLookup,
            is_value,
          });
        | None =>
          let.wrap_closure _ = env;
          Indet;
        };
      | `Substitution =>
        let. _ = otherwise(env, d);
        Indet;
      }
    | Seq(d1, d2) =>
      let. _ = otherwise(env, d1 => Seq(d1, d2) |> rewrap)
      and. _ =
        req_final(req(state, env), d1 => Seq1(d1, d2) |> wrap_ctx, d1);
      Step({
        expr: d2,
        state_update,
        kind: Seq,
        is_value: false,
      });
    | Let(dp, d1, d2) =>
      let. _ = otherwise(env, d1 => Let(dp, d1, d2) |> rewrap)
      and. d1' =
        req_final(req(state, env), d1 => Let1(dp, d1, d2) |> wrap_ctx, d1);
      let.wrap_closure _ = env;
      let {matches, closures} = matches(dp, d1');
      let matches_str = {
        switch (matches) {
        | IndetMatch
        | DoesNotMatch => ""
        | Matches(env) =>
          VarBstMap.Ordered.to_listo(env)
          |> List.rev
          |> List.map(((s, _)) => s)
          |> String.concat(", ")
        };
      };
      let.match env' = (env, matches, env.call_stack);
      Step({
        expr: subst_env(env', d2),
        state_update: capture_closures(env, state, closures),
        kind: LetBind(matches_str),
        is_value: false,
      });
    | TypFun(_)
    | Fun(_, _, _, _) =>
      let. _ = otherwise(env, d);
      let.wrap_closure _ = env;
      Value;
    | FixF(dp, d1, None) when mode == `Environment =>
      let. _ = otherwise(env, FixF(dp, d1, None) |> rewrap);
      Step({
        expr: FixF(dp, d1, Some(env)) |> rewrap,
        state_update,
        kind: FixClosure,
        is_value: false,
      });
    | FixF(dp, d1, env) =>
      let. _ =
        otherwise(env |> Option.value(~default=ClosureEnvironment.empty), d);
      switch (matches(dp, d1).matches) {
      | IndetMatch
      | DoesNotMatch => Indet
      | Matches(env') =>
        let env'' =
          VarBstMap.Ordered.mapo(
            ((p, exp)) =>
              if (VarBstMap.Ordered.length(env') > 1) {
                let_(dp, FixF(dp, d1, env) |> rewrap, var(p));
              } else {
                FixF(Var(p) |> Pat.fresh, exp, env) |> rewrap;
              },
            env',
          );
        let env''' =
          evaluate_extend_env(
            ~call_stack=
              (env |> Option.value(~default=ClosureEnvironment.empty)).
                call_stack,
            env'',
            env |> Option.value(~default=ClosureEnvironment.empty),
          );
        Step({
          expr: subst_env(env''', d1),
          state_update,
          kind: FixUnwrap,
          is_value: false,
        });
      };
    | Test(d'') =>
      let. _ = otherwise(env, d => Test(d) |> rewrap)
      and. d' = req_final(req(state, env), d => Test(d) |> wrap_ctx, d'');
      let result: TestStatus.t =
        switch (Unboxing.unbox(Atom(Bool), d')) {
        | DoesNotMatch
        | IndetMatch => Indet
        | Matches(b) => b ? Pass : Fail
        };
      Step({
        expr: tuple([]),
        state_update: () =>
          update_test(
            state,
            DHExp.rep_id(d),
            {
              exp: d,
              status: result,
              hint: "No hint available.",
            },
          ),
        // update_test(state, DHExp.rep_id(d), (d', result)),
        kind: UpdateTest,
        is_value: true,
      });
    | HintedTest(d'', h) =>
      let. _ = otherwise(env, d => HintedTest(d, h) |> rewrap)
      and. d' =
        req_final(req(state, env), d => HintedTest(d, h) |> wrap_ctx, d'');
      let result: TestStatus.t =
        switch (Unboxing.unbox(Atom(Bool), d')) {
        | DoesNotMatch
        | IndetMatch => Indet
        | Matches(b) => b ? Pass : Fail
        };
      let h: string =
        switch (h.term) {
        | Atom(String(s)) => s
        | _ => "No hint available."
        };
      Step({
        expr: Tuple([]) |> fresh,
        state_update: () =>
          update_test(
            state,
            DHExp.rep_id(d),
            {
              exp: d,
              status: result,
              hint: h,
            },
          ),
        kind: UpdateTest,
        is_value: true,
      });
    | TypAp(d, tau) =>
      let. _ = otherwise(env, d => TypAp(d, tau) |> rewrap)
      and. d' =
        req_final(req(state, env), d => TypAp(d, tau) |> wrap_ctx, d);
      let-unbox typfun = (TypFun, d');
      switch (typfun) {
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
      Value;
    | Ap(dir, d1, d2) =>
      let. _ = otherwise(env, (d1, d2) => Ap(dir, d1, d2) |> rewrap)
      and. d1' =
        req_final(req(state, env), d1 => Ap1(dir, d1, d2) |> wrap_ctx, d1)
      and. d2' =
        req_final(req(state, env), d2 => Ap2(dir, d1, d2) |> wrap_ctx, d2);
      switch (d1'.term) {
      | Asc(d1'', {term: Arrow(t1, t2), _}) =>
        Step({
          expr:
            Asc(Ap(Forward, d1'', Asc(d2', t1) |> fresh) |> fresh, t2)
            |> fresh,
          state_update,
          kind: Ascription,
          is_value: false,
        })
      | _ =>
        let-unbox unboxed_fun = (Fun, d1');
        switch (unboxed_fun) {
        | Constructor(_) => Constructor
        | FunEnv(dp, d3, function_lexical_env) =>
          let matches = matches(dp, d2');
          switch (matches.matches) {
          | IndetMatch
          | DoesNotMatch => Indet
          | Matches(function_arg_env) =>
            let env'' =
              evaluate_extend_env(
                ~ap_id=Term.Exp.rep_id(d),
                ~call_stack=env.call_stack,
                function_arg_env,
                function_lexical_env,
              );
            Step({
              expr: subst_env(env'', d3),
              state_update: capture_closures(env'', state, matches.closures),
              kind: FunAp,
              is_value: false,
            });
          };
        | FunNoEnv(dp, d3) when mode == `Substitution =>
          let matches = matches(dp, d2');
          switch (matches.matches) {
          | IndetMatch
          | DoesNotMatch => Indet
          | Matches(function_arg_env) =>
            Step({
              expr:
                subst_env(
                  function_arg_env |> ClosureEnvironment.of_environment,
                  d3,
                ),
              state_update:
                capture_closures(
                  function_arg_env |> ClosureEnvironment.of_environment,
                  state,
                  matches.closures,
                ),
              kind: FunAp,
              is_value: false,
            })
          };
        | FunNoEnv(_) => Indet
        | BuiltinFun(ident) =>
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
          switch (builtin(d2')) {
          | Some(expr) =>
            Step({
              expr,
              state_update,
              kind: BuiltinAp(ident),
              is_value: false,
            })
          | None => Indet
          };
        | DeferredAp(d3, d4s) =>
          let n_args =
            List.length(
              List.filter(
                fun
                | {term: Deferral(_), _} => true
                | _ => false: Exp.t => bool,
                d4s,
              ),
            );
          let-unbox args =
            if (n_args == 1) {
              (
                Tuple(n_args),
                tuple([d2]) // TODO Should we not be going to a tuple?
              );
            } else {
              (Tuple(n_args), d2);
            };
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
            expr: ap(Forward, d3, tuple(new_args)),
            state_update,
            kind: DeferredAp,
            is_value: false,
          });
        };
      };
    | Deferral(_) =>
      let. _ = otherwise(env, d);
      Indet;
    | Atom(_)
    | LivelitName(_)
    | Label(_)
    | Constructor(_)
    | BuiltinFun(_) =>
      let. _ = otherwise(env, d);
      Constructor;
    | DrvExp(_) =>
      let. _ = otherwise(env, d);
      let d' = drv_transition(env, d);
      if (DHExp.fast_equal(d, d')) {
        Constructor;
      } else {
        Step({
          expr: d',
          state_update,
          kind: CompleteClosure,
          is_value: true,
        });
      };
    | If(c, d1, d2) =>
      let. _ = otherwise(env, c => If(c, d1, d2) |> rewrap)
      and. c' =
        req_final(req(state, env), c => If1(c, d1, d2) |> wrap_ctx, c);
      let.wrap_closure _ = env;
      let-unbox b = (Atom(Bool), c');
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
    | UnOp(op, d1) =>
      let. _ = otherwise(env, d1 => UnOp(op, d1) |> rewrap)
      and. d1' =
        req_final(req(state, env), d1 => UnOp(op, d1) |> wrap_ctx, d1);
      switch (Operators.semantics_of_un_op(op)) {
      | Undefined(_) => Indet
      | Defined(in_ty, out_ty, f) =>
        let-unbox n = (Atom(in_ty), d1');
        let expr =
          switch (f(n)) {
          | Either.L(return_value) =>
            // operator was successful
            Atom(Atom.repack(out_ty, return_value)) |> Exp.fresh
          | Either.R(error) =>
            // e.g. divide by zero
            dynamic_error_hole(UnOp(op, d1) |> rewrap, error)
          };
        Step({
          expr,
          state_update,
          kind: UnOp(op),
          is_value: true,
        });
      };
    | BinOp(Bool(And), d1, d2) =>
      let. _ = otherwise(env, d1 => BinOp(Bool(And), d1, d2) |> rewrap)
      and. d1' =
        req_final(
          req(state, env),
          d1 => BinOp1(Bool(And), d1, d2) |> wrap_ctx,
          d1,
        );
      let.wrap_closure _ = env;
      let-unbox b1 = (Atom(Bool), d1');
      Step({
        expr: b1 ? asc(d2, IdTagged.FreshGrammar.Typ.bool()) : bool(false),
        state_update,
        kind: BinOp(Bool(And)),
        is_value: false,
      });
    | BinOp(Bool(Or), d1, d2) =>
      let. _ = otherwise(env, d1 => BinOp(Bool(Or), d1, d2) |> rewrap)
      and. d1' =
        req_final(
          req(state, env),
          d1 => BinOp1(Bool(Or), d1, d2) |> wrap_ctx,
          d1,
        );
      let.wrap_closure _ = env;
      let-unbox b1 = (Atom(Bool), d1');
      Step({
        expr: b1 ? bool(true) : asc(d2, IdTagged.FreshGrammar.Typ.bool()),
        state_update,
        kind: BinOp(Bool(Or)),
        is_value: false,
      });
    | BinOp(op, d1, d2) =>
      let. _ = otherwise(env, (d1, d2) => BinOp(op, d1, d2) |> rewrap)
      and. d1 =
        req_final(req(state, env), d1 => BinOp1(op, d1, d2) |> wrap_ctx, d1)
      and. d2 =
        req_final(
          req(state, env),
          d2 => BinOp2(op, d1, d2) |> wrap_ctx,
          d2,
        );
      // Operator semantics are defined in Operators.re
      switch (Operators.semantics_of_bin_op(op)) {
      | Undefined(_) => Indet
      | DefinedPoly(poly_op) =>
        if (!DHExp.ty_comparable(d1, d2)) {
          let expr =
            DynamicErrorHole(BinOp(op, d1, d2) |> rewrap, Incomparable)
            |> fresh;
          Step({
            expr,
            state_update,
            kind: MarkIncomparable,
            is_value: false,
          });
        } else {
          let res = DHExp.poly_equal(d1, d2);
          let expr = Atom(Bool(poly_op == Equals ? res : !res)) |> fresh;
          Step({
            expr,
            state_update,
            kind: BinOp(op),
            is_value: false,
          });
        }
      | Defined(in_ty1, in_ty2, out_ty, f) =>
        let-unbox n1 = (Atom(in_ty1), d1);
        let-unbox n2 = (Atom(in_ty2), d2);
        let expr =
          switch (f(n1, n2)) {
          | Either.L(return_value) =>
            // operator was successful
            Atom(Atom.repack(out_ty, return_value)) |> Exp.fresh
          | Either.R(error) =>
            // e.g. divide by zero
            dynamic_error_hole(BinOp(op, d1, d2) |> rewrap, error)
          };
        Step({
          expr,
          state_update,
          kind: BinOp(op),
          is_value: true,
        });
      };
    | Dot(d1, d2) =>
      let. _ = otherwise(env, (d1, d2) => Dot(d1, d2) |> rewrap)
      and. d1' =
        req_final(req(state, env), d1 => Dot1(d1, d2) |> wrap_ctx, d1)
      and. d2' =
        req_final(req(state, env), d2 => Dot2(d1, d2) |> wrap_ctx, d2);
      switch (DHExp.term_of(d2')) {
      | Label(name) as lab =>
        switch (Unboxing.unbox(LabeledTupleProjection(name), d1')) {
        | Matches(d1'') =>
          switch (DHExp.term_of(d1'')) {
          | Tuple(ds) =>
            let projected =
              List.filter_map(
                d => {
                  switch (Exp.match_tup_label(d)) {
                  | Some((s, e)) when name == s => Some(e)
                  | _ => None
                  }
                },
                ds,
              );

            switch (projected) {
            | [exp] =>
              Step({
                expr: exp,
                state_update,
                kind: Dot,
                is_value: false,
              })
            | _ => Indet
            };
          | TupLabel(_, d) =>
            LabeledTuple.has_same_labels(
              Exp.match_tup_label(d1'),
              Some((name, d)),
            )
              ? Step({
                  expr: d,
                  state_update,
                  kind: Dot,
                  is_value: false,
                })
              : Indet
          | ListLit(ds) =>
            let mapped =
              List.map(d => Dot(d, lab |> Exp.fresh) |> Exp.fresh, ds);
            let ls = ListLit(mapped) |> Exp.fresh;
            Step({
              expr: ls,
              state_update,
              kind: Dot,
              is_value: false,
            });
          | _ => Indet
          }
        | _ => Indet
        }

      | _ => Indet
      };
    | TupLabel(label, d1) =>
      let. _ = otherwise(env, d1 => TupLabel(label, d1) |> rewrap)
      and. _ =
        req_final(
          req(state, env),
          d1 => TupLabel(label, d1) |> wrap_ctx,
          d1,
        );
      Constructor;
    | Tuple(ds) =>
      let. _ = otherwise(env, ds => Tuple(ds) |> rewrap)
      and. _ =
        req_all_final(
          req(state, env),
          (d1, ds) => Tuple(d1, ds) |> wrap_ctx,
          ds,
        );
      Constructor;
    | TupleExtension(e1, e2) =>
      let. _ = otherwise(env, (e1, e2) => TupleExtension(e1, e2) |> rewrap)
      and. e1' =
        req_final(
          req(state, env),
          e1 => TupleExtension1(e1, e2) |> wrap_ctx,
          e1,
        )
      and. e2' =
        req_final(
          req(state, env),
          e2 => TupleExtension2(e1, e2) |> wrap_ctx,
          e2,
        );
      let-unbox e1_entries = (LabeledTupleEntries, e1');
      let-unbox e2_entries = (LabeledTupleEntries, e2');

      let tuple: Grammar.exp_t(IdTagged.IdTag.t) =
        tuple(
          List.map(
            ((lab, d)) =>
              switch (lab) {
              | Some(l) => tup_label(label(l), d)
              | None => d
              },
            LabeledTuple.extension(e1_entries, e2_entries),
          ),
        );

      Step({
        expr: tuple,
        state_update,
        kind: TupleExtension,
        is_value: true,
      });
    | Cons(d1, d2) =>
      let. _ = otherwise(env, (d1, d2) => Cons(d1, d2) |> rewrap)
      and. d1' =
        req_final(req(state, env), d1 => Cons1(d1, d2) |> wrap_ctx, d1)
      and. d2' =
        req_final(req(state, env), d2 => Cons2(d1, d2) |> wrap_ctx, d2);
      switch (Unboxing.unbox(ListLit, d2')) {
      | Matches(ds) =>
        Step({
          expr: list_lit([d1', ...ds]),
          state_update,
          kind: ListCons,
          is_value: true,
        })
      | DoesNotMatch => Indet
      | IndetMatch => Constructor // Treat list cons with indet tail as constructors
      };
    | ListConcat(d1, d2) =>
      let. _ = otherwise(env, (d1, d2) => ListConcat(d1, d2) |> rewrap)
      and. d1' =
        req_final(
          req(state, env),
          d1 => ListConcat1(d1, d2) |> wrap_ctx,
          d1,
        )
      and. d2' =
        req_final(
          req(state, env),
          d2 => ListConcat2(d1, d2) |> wrap_ctx,
          d2,
        );
      let-unbox ds1 = (ListLit, d1');
      let-unbox ds2 = (ListLit, d2');
      Step({
        expr: list_lit(ds1 @ ds2),
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
        | [(dp, d2), ...rules] => {
            let matches = matches(dp, d1);
            switch (matches.matches) {
            | Matches(env') => Some((env', d2, matches.closures))
            | DoesNotMatch => next_rule(rules)
            | IndetMatch => None
            };
          }
      );
      switch (next_rule(rules)) {
      | Some((env', d2, closures)) =>
        Step({
          expr:
            subst_env(
              evaluate_extend_env(env', env, ~call_stack=env.call_stack),
              d2,
            ),

          state_update: capture_closures(env, state, closures),
          kind: CaseApply,
          is_value: false,
        })
      | None =>
        let.wrap_closure _ = env;
        Indet;
      };
    | Closure(env', d) =>
      // HACK [Matt] This ref is a hack to ensure that we don't get into an infinite loop
      // where we keep deleting and re-adding closures around forms that need closures
      // e.g. functions.
      let needs_closure = ref(false);
      let in_closure = () => needs_closure := true;
      let. _ = otherwise(env, d => Closure(env', d) |> rewrap)
      and. d' =
        req_final(
          req(~in_closure, state, env'),
          d1 => Closure(env', d1) |> wrap_ctx,
          d,
        );
      if (needs_closure^) {
        Constructor;
      } else {
        Step({
          expr: d',
          state_update,
          kind: CompleteClosure,
          is_value: true,
        });
      };
    | MultiHole(_) =>
      let. _ = otherwise(env, d);
      let.wrap_closure _ = env;
      Indet;
    | EmptyHole
    | Invalid(_) =>
      let. _ = otherwise(env, d);
      // let.wrap_closure _ = env;  // uncomment for hole closures
      Indet;
    | DynamicErrorHole(_) =>
      let. _ = otherwise(env, d);
      let.wrap_closure _ = env;
      Indet;
    | Asc(d', t) =>
      switch (Ascriptions.transition(d)) {
      | Some(d') =>
        let. _ = otherwise(env, d);
        Step({
          expr: d',
          state_update,
          kind: Ascription,
          is_value: false,
        });
      | None =>
        let. _ = otherwise(env, d => Asc(d, t) |> rewrap)
        and. d' =
          req_final(req(state, env), d => Asc(d, t) |> wrap_ctx, d');
        switch (Ascriptions.transition(Asc(d', t) |> rewrap)) {
        | Some(d) =>
          Step({
            expr: d,
            state_update,
            kind: Ascription,
            is_value: false,
          })
        | None => Constructor
        };
      }
    | Undefined =>
      let. _ = otherwise(env, d);
      Indet;
    | Probe(d'', pr) =>
      /* When evaluated, a probe adds a dynamics info entry
       * reflecting the evaluation of the contained expression */
      let. _ = otherwise(env, d => Probe(d, pr) |> rewrap)
      and. d' =
        req_final(req(state, env), d => Probe(d, pr) |> wrap_ctx, d'');
      Step({
        expr: d',
        state_update: () => {
          let call_stack = ClosureEnvironment.call_stack_of(env);
          let map = ClosureEnvironment.map_of(env);
          let id = DHExp.rep_id(d);
          let closure =
            Dynamics.Probe.Closure.mk(id, d', map, call_stack, pr);
          update_probe(state, closure);
        },
        kind: RemoveParens,
        is_value: false,
      });
    | Parens(d) =>
      let. _ = otherwise(env, d);
      Step({
        expr: d,
        state_update,
        kind: RemoveParens,
        is_value: false,
      });
    | TyAlias(_, _, d) =>
      let. _ = otherwise(env, d);
      Step({
        expr: d,
        state_update,
        kind: RemoveTypeAlias,
        is_value: false,
      });
    | Use(_, d) =>
      let. _ = otherwise(env, d);
      Step({
        expr: d,
        state_update,
        kind: RemoveUse,
        is_value: true,
      });
    | Filter(f1, d1) =>
      let. _ = otherwise(env, d1 => Filter(f1, d1) |> rewrap)
      and. d1 =
        req_final(req(state, env), d1 => Filter(f1, d1) |> wrap_ctx, d1);
      Step({
        expr: d1,
        state_update,
        kind: CompleteFilter,
        is_value: true,
      });
    };
  };
};

let should_hide_step_kind = (~settings: CoreSettings.Evaluation.t) =>
  fun
  | LetBind(_)
  | Seq
  | UpdateTest
  | TypFunAp
  | FunAp
  | DeferredAp
  | BuiltinAp(_)
  | BinOp(_)
  | Dot
  | UnOp(_)
  | ListCons
  | ListConcat
  | TupleExtension
  | CaseApply
  | Projection // TODO(Matt): We don't want to show projection to the user
  | Conditional(_)
  | RemoveTypeAlias
  | RemoveUse
  | InvalidStep => false
  | VarLookup => !settings.show_lookup_steps
  | AscriptionTypAp
  | AscriptionAp
  | Ascription => !settings.show_ascription_steps
  | FixUnwrap => !settings.show_fixpoints
  | CompleteClosure
  | CompleteFilter
  | BuiltinWrap
  | WrapClosure
  | FixClosure
  | MarkIncomparable
  | RemoveParens => true;

let stepper_justification: step_kind => string =
  fun
  | LetBind(s) => String.cat("substitution for ", s)
  | Seq => "sequence"
  | FixUnwrap => "unroll fixpoint"
  | UpdateTest => "update test"
  | TypFunAp => "apply type function"
  | FunAp => "apply function"
  | DeferredAp => "deferred application"
  | BuiltinWrap => "wrap builtin"
  | BuiltinAp(s) => "evaluate " ++ s
  | UnOp(Int(Minus) | Nat(Minus) | Float(Minus) | SInt(Minus))
  | BinOp(SInt(Plus | Minus | Times | Power | Divide))
  | BinOp(Nat(Plus | Minus | Times | Power | Divide))
  | BinOp(Float(Plus | Minus | Times | Power | Divide))
  | BinOp(Int(Plus | Minus | Times | Power | Divide)) => "arithmetic"
  | BinOp(Nat(LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual))
  | BinOp(Int(LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual))
  | BinOp(
      SInt(LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual),
    )
  | BinOp(
      Float(LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual),
    ) => "comparison"
  | BinOp(String(Equals))
  | BinOp(Float(Equals | NotEquals))
  | BinOp(Poly(Equals | NotEquals)) => "check equality"
  | BinOp(String(Concat)) => "string manipulation"
  | UnOp(Bool(Not))
  | BinOp(Bool(_)) => "boolean logic"
  | Conditional(_) => "conditional"
  | ListCons => "list manipulation"
  | ListConcat => "list manipulation"
  | CaseApply => "case selection"
  | Projection => "projection" // TODO(Matt): We don't want to show projection to the user
  | InvalidStep => "error"
  | VarLookup => "variable lookup"
  | AscriptionTypAp
  | AscriptionAp
  | Ascription => "ascription transition"
  | FixClosure => "fixpoint closure"
  | CompleteFilter => "complete filter"
  | CompleteClosure => "complete closure"
  | WrapClosure => "wrap closure"
  | RemoveTypeAlias => "define type"
  | RemoveUse => "set use type"
  | RemoveParens => "remove parentheses"
  | Dot => "Labeled tuple access"
  | TupleExtension => "Tuple extension"
  | MarkIncomparable => "mark equality as incomparable"
  | UnOp(Meta(Unquote)) => failwith("INVALID STEP");
