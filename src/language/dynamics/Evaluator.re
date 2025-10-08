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

let handle_effects =
    (
      state: ref(EvaluatorState.t),
      call_stack: list(Id.t),
      env,
      init: DHExp.t,
      next: DHExp.t,
      side_effects,
    ) =>
  List.fold_left(
    ((call_stack, state), side_effect) =>
      switch (side_effect) {
      | RecordTest(instance_report) =>
        let id = DHExp.rep_id(instance_report.exp);
        state := EvaluatorState.add_test(state^, id, instance_report);
        (call_stack, state);
      | RecordProbe(pr) =>
        let id = DHExp.rep_id(init);
        let closure =
          Dynamics.Probe.Closure.mk(id, next, env, call_stack, pr);
        state := EvaluatorState.add_closure(state^, closure);
        (call_stack, state);
      | AddToCallStack => ([DHExp.rep_id(init), ...call_stack], state)
      | BindingProbe(closure_closures) =>
        List.iter(
          closure =>
            state := EvaluatorState.add_closure(state^, closure(call_stack)),
          closure_closures,
        );
        (call_stack, state);
      },
    (call_stack, state),
    side_effects,
  );

let rec evaluate =
        (
          ~in_closure=?,
          ~call_stack: list(Id.t),
          state: EvaluatorEVMode.state,
          env,
          init: DHExp.t,
        ) => {
  //TODO: unref state
  open Trampoline.Syntax;
  let.trampoline (is_finished, side_effects, next) =
    Eval.transition(
      evaluate(~call_stack),
      ~mode=`Environment,
      ~in_closure?,
      state,
      env,
      init,
    );
  let (call_stack, state) =
    handle_effects(state, call_stack, env, init, next, side_effects);
  switch (is_finished) {
  | Final => Trampoline.return((EvaluatorEVMode.Final, [], next))
  | Uneval => Trampoline.Next(() => evaluate(~call_stack, state, env, next))
  };
};

let evaluate_and_limit =
    (~step_limit: option(int)=?, ~env, d: DHExp.t)
    : step_constrained((Exp.t, EvaluatorState.t)) => {
  let state = ref(EvaluatorState.init);
  let result = evaluate(~call_stack=[], state, env, d);
  let result = Trampoline.run(~step_limit?, result);
  switch (result) {
  | Completed((_, _, x)) =>
    Completed((
      x |> Exp.replace_all_ids |> Exp.substitute_closures(env),
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
