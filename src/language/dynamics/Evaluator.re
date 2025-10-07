open Transition;

[@deriving (show({with_path: false}), eq)]
type step_constrained('a) =
  | StepLimitExceeded
  | Completed('a);

// This module defines the stack machine for the evaluator.
module Trampoline = {
  type t('a) =
    | Bind(t('b), 'b => t('a)): t('a)
    | Next(unit => t('a)): t('a)
    | Done('a): t('a);

  type callstack('a, 'b) =
    | Finished: callstack('a, 'a)
    | Continue('a => t('b), callstack('b, 'c)): callstack('a, 'c);
  let rec run:
    type a b.
      (~step_limit: int=?, ~step_counter: int=?, t(b), callstack(b, a)) =>
      step_constrained(a) =
    (
      ~step_limit: option(int)=?,
      ~step_counter=0,
      t: t(b),
      callstack: callstack(b, a),
    ) => {
      switch (step_limit) {
      | Some(x) when x <= step_counter => StepLimitExceeded
      | _ =>
        switch (t) {
        | Bind(t, f) =>
          run(
            ~step_limit?,
            ~step_counter=step_counter + 1,
            t,
            Continue(f, callstack),
          )
        | Next(f) =>
          run(~step_limit?, ~step_counter=step_counter + 1, f(), callstack)
        | Done(x) =>
          switch (callstack) {
          | Finished => Completed(x)
          | Continue(f, callstack) =>
            run(
              ~step_limit?,
              ~step_counter=step_counter + 1,
              f(x),
              callstack,
            )
          }
        }
      };
    };

  let run = (~step_limit: option(int)=?, t) =>
    run(~step_limit?, t, Finished);

  let return = x => Done(x);

  let bind = (t, f) => Bind(t, f);

  module Syntax = {
    let (let.trampoline) = (x, f) => bind(x, f);
  };
};

module EvaluatorEVMode: {
  type status =
    | Final
    | Uneval;

  include
    EV_MODE with
      type state = ref(EvaluatorState.t) and
      type result = Trampoline.t((status, list(side_effect), DHExp.t));
} = {
  open Trampoline.Syntax;

  type status =
    | Final
    | Uneval;

  type result = Trampoline.t((status, list(side_effect), DHExp.t));
  type requirement('a) = Trampoline.t('a);
  type requirements('a, 'b) = Trampoline.t(('a, 'b));

  type state = ref(EvaluatorState.t);
  let update_test = (state, id, v) =>
    state := EvaluatorState.add_test(state^, id, v);

  let update_probe = (state, closure: Dynamics.Probe.Closure.t) =>
    state := EvaluatorState.add_closure(state^, closure);

  let req_final = (f, _, x) => {
    let.trampoline (_, _, x) = Next(() => f(x));
    Trampoline.return(x);
  };
  let rec req_all_final = (f, i, xs) =>
    switch (xs) {
    | [] => Trampoline.return([])
    | [x, ...xs] =>
      let.trampoline x' = req_final(f, x => x, x);
      let.trampoline xs' = req_all_final(f, i, xs);
      Trampoline.return([x', ...xs']);
    };

  let otherwise = (_, c) => Trampoline.return(((), c));
  let (and.) = (t1, t2) => {
    let.trampoline (x1, c1) = t1;
    let.trampoline x2 = t2;
    Trampoline.return(((x1, x2), c1(x2)));
  };
  let (let.) = (t1, s) => {
    let.trampoline (x, c) = t1;
    let id = DHExp.rep_id(c); // Assuming that c has the original id
    switch (s(x)) {
    | Step({expr, side_effects, is_value: true, _}) =>
      Trampoline.return((Final, side_effects, expr))
    | Step({expr, side_effects, is_value: false, _}) =>
      Trampoline.return((Uneval, side_effects, expr))
    | Constructor
    | Value
    | Indet => Trampoline.return((Final, [], c))
    };
  };
};

module Eval = Transition(EvaluatorEVMode);

let rec evaluate = (~in_closure=?, ~call_stack, state, env, d) => {
  open Trampoline.Syntax;
  let.trampoline (is_finished, side_effects, d') =
    Eval.transition(
      evaluate(~call_stack),
      ~mode=`Environment,
      ~in_closure?,
      state,
      env,
      d,
    );
  let (call_stack, state) = List.fold_left(
    fun 
      | ((call_stack, state), RecordTest(instance_report))
      | ((call_stack, state), RecordProbe)
      | ((call_stack, state), AddToCallStack)
      | ((call_stack, state), BindingProbe(closure_closures))
  )

  switch (u) {
  | (Final, x) => (EvaluatorEVMode.Final, x) |> Trampoline.return
  | (Uneval, x) =>
    Trampoline.Next(() => evaluate(~call_stack, state, env, x))
  | (AddToCallStack, x) =>
    Trampoline.Next(
      () =>
        evaluate(
          ~call_stack=[DHExp.rep_id(d), ...call_stack],
          state,
          env,
          x,
        ),
    )
  | (RecordProbe, x) =>
    Trampoline.Next(
      () => {
        let map = ClosureEnvironment.map_of(env);
        let id = DHExp.rep_id(d);
        let closure = Dynamics.Probe.Closure.mk(id, x, map, call_stack, pr);
        update_probe(state, closure);
        evaluate(
          ~call_stack=[DHExp.rep_id(d), ...call_stack],
          state,
          env,
          x,
        );
      },
    )
  };
};

let evaluate_and_limit =
    (~step_limit: option(int)=?, ~env, d: DHExp.t)
    : step_constrained((Exp.t, EvaluatorState.t)) => {
  let state = ref(EvaluatorState.init);
  let env = ClosureEnvironment.of_environment(env);
  let result = evaluate(state, env, d);
  let result = Trampoline.run(~step_limit?, result);
  switch (result) {
  | Completed((_, x)) =>
    Completed((
      x
      |> Exp.replace_all_ids
      |> Exp.substitute_closures(env |> ClosureEnvironment.map_of),
      state^,
    ))
  | StepLimitExceeded => StepLimitExceeded
  };
};

let evaluate = (~env, d: DHExp.t): (Exp.t, EvaluatorState.t) => {
  switch (evaluate_and_limit(~env, d)) {
  | Completed((x, state)) => (x, state)
  | StepLimitExceeded =>
    raise(Failure("Impossible: Step limit exceeded when not set"))
  };
};
