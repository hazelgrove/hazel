open Util;
open PatternMatch;

/* Transition.re

   ##########################################
   #   How to add / edit transition rules   #
   ##########################################

   This module defines the evaluation semantics of Hazel in terms of small step
   evaluation. These small steps are wrapped up into a big step in Evaluator.re.

   I'll use the Seq case as an example:

    | Seq(d1, d2) =>
        let. _ = otherwise(d1 => Seq(d1, d2))
        and. _ = req_final(req(env), 0, d1);
        Step({expr: d2, state, kind: Seq, final: false});


    Each step semantics starts with a `let. () = otherwise(...)` that defines how
    to wrap the expression back up if the step couldn't be evaluated.

    This is followed by a series of `and. d1' = req_final(req(env), <i>, <d1>)`
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


    ############################
    #   How Transition works   #
    ############################

    The following are technical details that for most purposes can be ignored.

    ## Motivation

    Most of the complication in Transition.re comes from combining small-step and
    big-step evaluation. This is annoying because they have different requirements:
      - big-step needs to be fast
          - big-step cannot re-traverse expressions (used to just be for perf,
            but also now prevents double-probes)
          - big-step is not a sequence of small-steps - big-step should never
            re-wrap substeps in their expression context
      - small-step is nondeterministic, and needs to be able to happen in any subexpression

    ## High-Level Idea

    At each point in evaluation we need to
      - check what class of expression we're looking at
      - evaluate all subexpressions
      - make a call whether to take a step or not take a step
          - If we take a step, we might get a new form that needs to take more steps
            so we go back to the top
          - If we don't need to take a step, subexpressions might have taken a step,
            so we can't just stop, we need to rewrap the result of subexpression evaluation

     Because each expression could either take a step or not, this means we always need
     two continuations making it feel like a monadic double-bind which idk if that's something
     that exists. For example, we could consider the type of the plus_case to be:

        list_concat_case: ((result(exp), result(exp)), (exp, exp) -> result(exp), (exp, exp) -> result(exp) )

    which is almost a monadic bind, but not quite.

    The fallback expression wrapper is fed in using `otherwise`, and because this fallback
    expression wrapper needs to be passed through the whole applicative, and at each step needs
    to be filled with the values of d1' and d2' , the types in the applicative become a little
    horrendous. requirements('a, 'b), 'a represents the type of the arguments for the step, normal
    monad style. 'b  represents the type of f2, after it has had all the arguments passed in so far.

    let. which is called last, is expecting 'b  to be a DHExp.t, as in it's expecting f2 to have
    been fully applied by this point, and (and.) is expecting to be able to apply an argument and
    turn f2 from 'c => 'b  into 'b.
   */

[@deriving (show({with_path: false}), sexp, yojson)]
type step_kind =
  | InvalidStep
  | VarLookup
  | Seq
  | LetBind(string)
  | TheoremBind
  | RecordTheorem
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

type rule =
  | Step({
      expr: DHExp.t,
      side_effects: list(EvaluatorState.effect),
      kind: step_kind,
      is_value: bool,
    })
  | Constructor
  | Indet
  | Value;

let (let-unboxed) = (unboxed: Unboxing.unboxed('a), f) =>
  switch (unboxed) {
  | IndetMatch
  | DoesNotMatch => Indet
  | Matches(n) => f(n)
  };

let (let-unbox) = ((request, v), f) => {
  let-unboxed result = Unboxing.unbox(request, v);
  f(result);
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
  let otherwise: (Environment.t(Exp.t), 'a) => requirements(unit, 'a);
};

module Transition = (EV: EV_MODE) => {
  open EV;
  open DHExp;
  open IdTagged.FreshGrammar.Exp;

  /* Helper function to wrap a closure around an expression. Required for functions, but also for
     things like if-then-else expressions where the scrutinee is indet, and for hole closures */
  let wrap_closure_when_done = (~in_closure, expr, env, r: rule) =>
    switch (in_closure, r) {
    | (_, Step(_)) => r
    | (None, Constructor | Indet | Value) =>
      Step({
        expr: closure(env, expr),
        side_effects: [],
        kind: WrapClosure,
        is_value: false,
      })
    | (Some(f), Constructor | Indet | Value) =>
      f();
      r;
    };

  /* Note[Matt]: For IDs, I'm currently using a fresh id
     if anything about the current node changes, if only its
     children change, we use rewrap */

  let transition =
      (
        req:
          (~in_closure: unit => unit=?, Environment.t(Exp.t), DHExp.t) => 'a,
        ~mode: [
           | `Substitution
           | `Environment
         ],
        ~targets: Sample.targets=Sample.no_targets,
        ~in_closure=?,
        env: Environment.t(Exp.t), // Environment is empty in substitution mode
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

    let (let.wrap_closure) = ((env, d'), f: unit => rule) =>
      switch (mode) {
      | `Environment => wrap_closure_when_done(~in_closure, d', env, f())
      | `Substitution => f()
      };

    let subst_env = (env, d) =>
      switch (mode) {
      | `Environment => Closure(env, d) |> fresh
      | `Substitution => d |> Substitution.in_exp(env)
      };

    // Transition rules
    switch (term) {
    | Var(x) =>
      // switch (mode) {
      // | `Environment =>
      let. _ = otherwise(env, Var(x) |> rewrap);
      switch (Environment.lookup(env, x)) {
      | Some({term: Var(y), _}) when x == y => Indet // Used in proof to refer to a bound variable.
      | Some(d) =>
        let is_value =
          switch (d |> Exp.term_of) {
          | FixF(_, _, _) => false // fixpoints aren't final
          | Let(_, _, _) => false // could be mutually-recursive fixpoint
          | _ => true // all other closure entries should be final
          };
        Step({
          expr: d |> fast_copy(Id.mk()),
          side_effects: [],
          kind: VarLookup,
          is_value,
        });
      | None =>
        let.wrap_closure _ = (env, d);
        Indet;
      };
    // | `Substitution =>
    //   let. _ = otherwise(env, d);
    //   Indet;
    // }
    | Seq(d1, d2) =>
      let. _ = otherwise(env, d1 => Seq(d1, d2) |> rewrap)
      and. _ = req_final(req(env), d1 => Seq1(d1, d2) |> wrap_ctx, d1);
      Step({
        expr: d2,
        side_effects: [],
        kind: Seq,
        is_value: false,
      });
    | Let(dp, d1, d2) =>
      let. _ = otherwise(env, d1 => Let(dp, d1, d2) |> rewrap)
      and. d1' =
        req_final(req(env), d1 => Let1(dp, d1, d2) |> wrap_ctx, d1);
      let.wrap_closure _ = (env, Let(dp, d1', d2) |> rewrap);
      let {matches, samples} = matches(targets, dp, d1');
      let matches_str = {
        switch (matches) {
        | IndetMatch
        | DoesNotMatch => ""
        | Matches(env) =>
          env |> List.rev |> List.map(((s, _)) => s) |> String.concat(", ")
        };
      };

      switch (matches) {
      | IndetMatch
      | DoesNotMatch => Indet
      | Matches(env') =>
        let env' = Environment.add_bindings(env, env');
        Step({
          expr: subst_env(env', d2),
          side_effects: [RecordPatProbes(samples)],
          kind: LetBind(matches_str),
          is_value: false,
        });
      };

    | Theorem({term: Var(n), _}, e, d1) =>
      let. _ = otherwise(env, d);
      let e' = Substitution.in_exp(env, e);
      let env' = Environment.extend(env, (n, ProofObject(e') |> Exp.fresh));
      Step({
        expr: subst_env(env', d1),
        side_effects: [RecordTheorem(DHExp.rep_id(d), n, env, e')],
        kind: TheoremBind,
        is_value: false,
      });
    | Theorem(_) =>
      let. _ = otherwise(env, d);
      Indet;
    | ProofObject(e) =>
      let. _ = otherwise(env, d);
      let e' = Substitution.in_exp(env, e);
      switch (mode) {
      | `Substitution => Value
      | `Environment =>
        Step({
          expr: d,
          side_effects: [
            RecordTheorem(DHExp.rep_id(d), "<anon theorem>", env, e'),
          ],
          kind: RecordTheorem,
          is_value: true,
        })
      };
    // Note[Matt]: we could make this spin, but for now it's indet
    | Forall(_) =>
      let. _ = otherwise(env, d);
      Indet;
    | TypFun(_)
    | Fun(_, _, _, _) =>
      let. _ = otherwise(env, d);
      let.wrap_closure _ = (env, d);
      Value;
    | FixF(dp, d1, None) when mode == `Environment =>
      let. _ = otherwise(env, FixF(dp, d1, None) |> rewrap);
      Step({
        expr: FixF(dp, d1, Some(env)) |> rewrap,
        side_effects: [],
        kind: FixClosure,
        is_value: false,
      });
    | FixF(dp, d1, fix_env) =>
      let. _ = otherwise(env, d);
      let env' =
        switch (Pat.is_var(dp)) {
        | Some(v) => [
            (v, FixF(Var(v) |> Pat.fresh, d1, fix_env) |> rewrap),
          ]
        | None =>
          // If the pattern is not a single variable, we need to unpack the results of inner evaulations
          List.map(
            (Binding.{name: v, id: _}) =>
              (v, let_(dp, FixF(dp, d1, fix_env) |> rewrap, var(v))),
            Pat.bindings(dp),
          )
        };
      let env'' = Environment.add_bindings(env, env');
      Step({
        expr: subst_env(env'', d1),
        side_effects: [],
        kind: FixUnwrap,
        is_value: false,
      });
    | Test(d'') =>
      let. _ = otherwise(env, d => Test(d) |> rewrap)
      and. d' = req_final(req(env), d => Test(d) |> wrap_ctx, d'');
      let result: TestStatus.t =
        switch (Unboxing.unbox(Atom(Bool), d')) {
        | DoesNotMatch
        | IndetMatch => Indet
        | Matches(b) => b ? Pass : Fail
        };
      Step({
        expr: tuple([]),
        side_effects: [
          RecordTest({
            exp: d,
            status: result,
            hint: "No hint available.",
          }),
        ],
        kind: UpdateTest,
        is_value: true,
      });
    | HintedTest(d'', h) =>
      let. _ = otherwise(env, d => HintedTest(d, h) |> rewrap)
      and. d' = req_final(req(env), d => HintedTest(d, h) |> wrap_ctx, d'');
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
        side_effects: [
          RecordTest({
            exp: d,
            status: result,
            hint: h,
          }),
        ],
        kind: UpdateTest,
        is_value: true,
      });
    | TypAp(d, tau) =>
      let. _ = otherwise(env, d => TypAp(d, tau) |> rewrap)
      and. d' = req_final(req(env), d => TypAp(d, tau) |> wrap_ctx, d);
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
          side_effects: [],
          kind: TypFunAp,
          is_value: false,
        })
      };
    | DeferredAp(d1, ds) =>
      let. _ = otherwise(env, (d1, ds) => DeferredAp(d1, ds) |> rewrap)
      and. _ =
        req_final(req(env), d1 => DeferredAp1(d1, ds) |> wrap_ctx, d1)
      and. _ =
        req_all_final(
          req(env),
          (d2, ds) => DeferredAp2(d1, d2, ds) |> wrap_ctx,
          ds,
        );
      Value;
    | Ap(dir, d1, d2) =>
      let. _ = otherwise(env, (d1, d2) => Ap(dir, d1, d2) |> rewrap)
      and. d1' = req_final(req(env), d1 => Ap1(dir, d1, d2) |> wrap_ctx, d1)
      and. d2' =
        req_final(req(env), d2 => Ap2(dir, d1, d2) |> wrap_ctx, d2);
      switch (d1'.term) {
      | Asc(d1'', {term: Arrow(t1, t2), _}) =>
        Step({
          expr:
            Asc(Ap(Forward, d1'', Asc(d2', t1) |> fresh) |> fresh, t2)
            |> fresh,
          side_effects: [],
          kind: Ascription,
          is_value: false,
        })
      | _ =>
        let-unbox unboxed_fun = (Fun, d1');
        switch (unboxed_fun) {
        | Constructor(_) => Constructor
        | FunEnv(dp, d3, replacement_env) =>
          let matches = matches(targets, dp, d2');
          switch (matches.matches) {
          | IndetMatch
          | DoesNotMatch => Indet
          | Matches(added_env) =>
            let env'' = Environment.add_bindings(replacement_env, added_env);
            Step({
              expr: subst_env(env'', d3),
              side_effects: [
                RecordStackFrame,
                RecordPatProbes(matches.samples),
              ],
              kind: FunAp,
              is_value: false,
            });
          };
        | FunNoEnv(dp, d3) when mode == `Substitution =>
          let matches = matches(targets, dp, d2');
          switch (matches.matches) {
          | IndetMatch
          | DoesNotMatch => Indet
          | Matches(added_env) =>
            Step({
              expr:
                subst_env(
                  Environment.add_bindings(Environment.empty, added_env),
                  d3,
                ),
              side_effects: [
                RecordStackFrame,
                RecordPatProbes(matches.samples),
              ],
              kind: FunAp,
              is_value: false,
            })
          };
        | FunNoEnv(_) => Indet
        | BuiltinFun(ident) =>
          if (ident == "print") {
            /* Println for probes study */
            Step({
              expr: tuple([]),
              side_effects: [RecordStackFrame, RecordPrint(d2')],
              kind: BuiltinAp(ident),
              is_value: true,
            });
          } else {
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
                side_effects: [RecordStackFrame],
                kind: BuiltinAp(ident),
                is_value: false,
              })
            | None => Indet
            };
          }
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
          let-unboxed args =
            if (n_args == 1) {
              Matches([d2']);
            } else {
              Unboxing.unbox(Tuple(n_args), d2');
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
            expr:
              ap(
                Forward,
                d3,
                switch (new_args) {
                | [{term: TupLabel(_, _), _}] => tuple(new_args)
                | [d] => d
                | _ => tuple(new_args)
                },
              ),
            side_effects: [],
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
    | ExplicitNonlabel
    | Constructor(_)
    | BuiltinFun(_) =>
      let. _ = otherwise(env, d);
      Constructor;
    | If(c, d1, d2) =>
      let. _ = otherwise(env, c => If(c, d1, d2) |> rewrap)
      and. c' = req_final(req(env), c => If1(c, d1, d2) |> wrap_ctx, c);
      let.wrap_closure _ = (env, If(c', d1, d2) |> rewrap);
      let-unbox b = (Atom(Bool), c');
      Step({
        expr: {
          b ? d1 : d2;
        },
        side_effects: [],
        // Attach c' to indicate which branch taken.
        kind: Conditional(b),
        is_value: false,
      });
    | UnOp(Meta(Unquote), _) =>
      let. _ = otherwise(env, d);
      Indet;
    | UnOp(op, d1) =>
      let. _ = otherwise(env, d1 => UnOp(op, d1) |> rewrap)
      and. d1' = req_final(req(env), d1 => UnOp(op, d1) |> wrap_ctx, d1);
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
          side_effects: [],
          kind: UnOp(op),
          is_value: true,
        });
      };
    | BinOp(Bool(And), d1, d2) =>
      let. _ = otherwise(env, d1 => BinOp(Bool(And), d1, d2) |> rewrap)
      and. d1' =
        req_final(
          req(env),
          d1 => BinOp1(Bool(And), d1, d2) |> wrap_ctx,
          d1,
        );
      let.wrap_closure _ = (env, BinOp(Bool(And), d1', d2) |> rewrap);
      let-unbox b1 = (Atom(Bool), d1');
      Step({
        expr: b1 ? asc(d2, IdTagged.FreshGrammar.Typ.bool()) : bool(false),
        side_effects: [],
        kind: BinOp(Bool(And)),
        is_value: false,
      });
    | BinOp(Bool(Or), d1, d2) =>
      let. _ = otherwise(env, d1 => BinOp(Bool(Or), d1, d2) |> rewrap)
      and. d1' =
        req_final(
          req(env),
          d1 => BinOp1(Bool(Or), d1, d2) |> wrap_ctx,
          d1,
        );
      let.wrap_closure _ = (env, BinOp(Bool(Or), d1', d2) |> rewrap);
      let-unbox b1 = (Atom(Bool), d1');
      Step({
        expr: b1 ? bool(true) : asc(d2, IdTagged.FreshGrammar.Typ.bool()),
        side_effects: [],
        kind: BinOp(Bool(Or)),
        is_value: false,
      });
    | BinOp(op, d1, d2) =>
      let. _ = otherwise(env, (d1, d2) => BinOp(op, d1, d2) |> rewrap)
      and. d1 =
        req_final(req(env), d1 => BinOp1(op, d1, d2) |> wrap_ctx, d1)
      and. d2 =
        req_final(req(env), d2 => BinOp2(op, d1, d2) |> wrap_ctx, d2);
      // Operator semantics are defined in Operators.re
      switch (Operators.semantics_of_bin_op(op)) {
      | Undefined(_) => Indet
      | DefinedPoly(poly_op) =>
        if (!DHExp.ty_comparable(d1, d2)) {
          Indet;
        } else {
          switch (DHExp.poly_equal(d1, d2)) {
          | None => Indet
          | Some(true) =>
            Step({
              expr: Atom(Bool(poly_op == Equals)) |> fresh,
              side_effects: [],
              kind: BinOp(op),
              is_value: true,
            })
          | Some(false) =>
            Step({
              expr: Atom(Bool(poly_op != Equals)) |> fresh,
              side_effects: [],
              kind: BinOp(op),
              is_value: false,
            })
          };
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
          side_effects: [],
          kind: BinOp(op),
          is_value: true,
        });
      };
    | Dot(d1, d2) =>
      let. _ = otherwise(env, (d1, d2) => Dot(d1, d2) |> rewrap)
      and. d1' = req_final(req(env), d1 => Dot1(d1, d2) |> wrap_ctx, d1)
      and. d2' = req_final(req(env), d2 => Dot2(d1, d2) |> wrap_ctx, d2);
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
                side_effects: [],
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
                  side_effects: [],
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
              side_effects: [],
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
        req_final(req(env), d1 => TupLabel(label, d1) |> wrap_ctx, d1);
      Constructor;
    | Tuple(ds) =>
      let. _ = otherwise(env, ds => Tuple(ds) |> rewrap)
      and. _ =
        req_all_final(req(env), (d1, ds) => Tuple(d1, ds) |> wrap_ctx, ds);
      Constructor;
    | TupleExtension(e1, e2) =>
      let. _ = otherwise(env, (e1, e2) => TupleExtension(e1, e2) |> rewrap)
      and. e1' =
        req_final(req(env), e1 => TupleExtension1(e1, e2) |> wrap_ctx, e1)
      and. e2' =
        req_final(req(env), e2 => TupleExtension2(e1, e2) |> wrap_ctx, e2);
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
        side_effects: [],
        kind: TupleExtension,
        is_value: true,
      });
    | Cons(d1, d2) =>
      let. _ = otherwise(env, (d1, d2) => Cons(d1, d2) |> rewrap)
      and. d1' = req_final(req(env), d1 => Cons1(d1, d2) |> wrap_ctx, d1)
      and. d2' = req_final(req(env), d2 => Cons2(d1, d2) |> wrap_ctx, d2);
      switch (Unboxing.unbox(ListLit, d2')) {
      | Matches(ds) =>
        Step({
          expr: list_lit([d1', ...ds]),
          side_effects: [],
          kind: ListCons,
          is_value: true,
        })
      | DoesNotMatch => Indet
      | IndetMatch => Constructor // Treat list cons with indet tail as constructors
      };
    | ListConcat(d1, d2) =>
      let. _ = otherwise(env, (d1, d2) => ListConcat(d1, d2) |> rewrap)
      and. d1' =
        req_final(req(env), d1 => ListConcat1(d1, d2) |> wrap_ctx, d1)
      and. d2' =
        req_final(req(env), d2 => ListConcat2(d1, d2) |> wrap_ctx, d2);
      let-unbox ds1 = (ListLit, d1');
      let-unbox ds2 = (ListLit, d2');
      Step({
        expr: list_lit(ds1 @ ds2),
        side_effects: [],
        kind: ListConcat,
        is_value: true,
      });
    | ListLit(ds) =>
      let. _ = otherwise(env, ds => ListLit(ds) |> rewrap)
      and. _ =
        req_all_final(
          req(env),
          (d1, ds) => ListLit(d1, ds) |> wrap_ctx,
          ds,
        );
      Constructor;
    | Match(d1, rules) =>
      let. _ = otherwise(env, d1 => Match(d1, rules) |> rewrap)
      and. d1 =
        req_final(req(env), d1 => MatchScrut(d1, rules) |> wrap_ctx, d1);
      let rec next_rule = (
        fun
        | [] => None
        | [(dp, d2), ...rules] => {
            let matches = matches(targets, dp, d1);
            switch (matches.matches) {
            | Matches(env') => Some((env', d2, matches.samples))
            | DoesNotMatch => next_rule(rules)
            | IndetMatch => None
            };
          }
      );
      switch (next_rule(rules)) {
      | Some((env', d2, samples)) =>
        Step({
          expr: subst_env(Environment.add_bindings(env, env'), d2),
          side_effects: [RecordPatProbes(samples)],
          kind: CaseApply,
          is_value: false,
        })
      | None =>
        let.wrap_closure _ = (env, Match(d1, rules) |> rewrap);
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
          req(~in_closure, env'),
          d1 => Closure(env', d1) |> wrap_ctx,
          d,
        );
      if (needs_closure^ || mode == `Substitution) {
        Constructor;
      } else {
        Step({
          expr: d',
          side_effects: [],
          kind: CompleteClosure,
          is_value: true,
        });
      };
    | MultiHole(_) =>
      let. _ = otherwise(env, d);
      let.wrap_closure _ = (env, d);
      Indet;
    | EmptyHole
    | Invalid(_) =>
      let. _ = otherwise(env, d);
      // let.wrap_closure _ = env;  // uncomment for hole closures
      Indet;
    | DynamicErrorHole(d, err) =>
      let. _ = otherwise(env, d => DynamicErrorHole(d, err) |> rewrap)
      and. _ =
        req_final(req(env), d1 => DynamicErrorHole(d1, err) |> wrap_ctx, d);
      let.wrap_closure _ = (env, d);
      Indet;
    | Asc(d', t) =>
      switch (Ascriptions.transition(d)) {
      | Some(d') =>
        let. _ = otherwise(env, d);
        Step({
          expr: d',
          side_effects: [],
          kind: Ascription,
          is_value: false,
        });
      | None =>
        let. _ = otherwise(env, d => Asc(d, t) |> rewrap)
        and. d' = req_final(req(env), d => Asc(d, t) |> wrap_ctx, d');
        switch (Ascriptions.transition(Asc(d', t) |> rewrap)) {
        | Some(d) =>
          Step({
            expr: d,
            side_effects: [],
            kind: Ascription,
            is_value: false,
          })
        | None => Constructor
        };
      }
    | Undefined =>
      let. _ = otherwise(env, d);
      Indet;
    | Parens(d') =>
      let. _ = otherwise(env, d);
      Step({
        expr: d',
        side_effects: [],
        kind: RemoveParens,
        is_value: false,
      });
    | TyAlias(_, _, d) =>
      let. _ = otherwise(env, d);
      Step({
        expr: d,
        side_effects: [],
        kind: RemoveTypeAlias,
        is_value: false,
      });
    | Use(_, d) =>
      let. _ = otherwise(env, d);
      Step({
        expr: d,
        side_effects: [],
        kind: RemoveUse,
        is_value: true,
      });
    | Filter(f1, d1) =>
      let. _ = otherwise(env, d1 => Filter(f1, d1) |> rewrap)
      and. d1 = req_final(req(env), d1 => Filter(f1, d1) |> wrap_ctx, d1);
      Step({
        expr: d1,
        side_effects: [],
        kind: CompleteFilter,
        is_value: true,
      });
    };
  };
};

let should_hide_step_kind = (~settings: CoreSettings.Evaluation.t) =>
  fun
  | LetBind(_)
  | TheoremBind
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
  | TupleExtension => false
  | CaseApply => !settings.show_case_steps
  | Projection // TODO(Matt): We don't want to show projection to the user
  | Conditional(_)
  | RemoveTypeAlias
  | RemoveUse
  | InvalidStep
  | VarLookup => false
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
  | RecordTheorem
  | RemoveParens => true;

let stepper_justification: step_kind => string =
  fun
  | LetBind(s) => String.cat("substitution for ", s)
  | TheoremBind => "theorem substitution"
  | RecordTheorem => "record theorem"
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
