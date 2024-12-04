open Transition;

open ProgramResult.Result;

// This module defines the stack machine for the evaluator.
module Trampoline = {
  type t('a) =
    | Bind(t('b), 'b => t('a)): t('a)
    | Next(unit => t('a)): t('a)
    | Done('a): t('a);

  type callstack('a, 'b) =
    | Finished: callstack('a, 'a)
    | Continue('a => t('b), callstack('b, 'c)): callstack('a, 'c);

  let rec run: type a b. (t(b), callstack(b, a)) => a =
    (t: t(b), callstack: callstack(b, a)) => (
      switch (t) {
      | Bind(t, f) => (run(t, Continue(f, callstack)): a)
      | Next(f) => run(f(), callstack)
      | Done(x) =>
        switch (callstack) {
        | Finished => x
        | Continue(f, callstack) => run(f(x), callstack)
        }
      }: a
    );

  let run = run(_, Finished);

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
      type result = Trampoline.t((status, DHExp.t));
} = {
  open Trampoline.Syntax;

  type status =
    | Final
    | Uneval;

  type result = Trampoline.t((status, DHExp.t));
  type requirement('a) = Trampoline.t('a);
  type requirements('a, 'b) = Trampoline.t(('a, 'b));

  type state = ref(EvaluatorState.t);
  let update_test = (state, id, v) =>
    state := EvaluatorState.add_test(state^, id, v);

  let req_final = (f, _, x) => {
    let.trampoline x' = Next(() => f(x));
    Trampoline.return(x' |> snd);
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
    | Step({expr, state_update, is_value: true, _}) =>
      state_update();
      Trampoline.return((Final, expr));
    | Step({expr, state_update, is_value: false, _}) =>
      state_update();
      Trampoline.return((Uneval, expr));
    | Constructor
    | Value
    | Indet => Trampoline.return((Final, c))
    };
  };
};

module Eval = Transition(EvaluatorEVMode);

let rec evaluate = (state, env, d) => {
  open Trampoline.Syntax;
  let.trampoline u = Eval.transition(evaluate, state, env, d);
  switch (u) {
  | (Final, x) => (EvaluatorEVMode.Final, x) |> Trampoline.return
  | (Uneval, x) => Trampoline.Next(() => evaluate(state, env, x))
  };
};

let evaluate' = (env, {d, _}: Elaborator.Elaboration.t) => {
  let state = ref(EvaluatorState.init);
  let env = ClosureEnvironment.of_environment(env);
  let result = evaluate(state, env, d);
  let result = Trampoline.run(result);
  let result =
    switch (result) {
    | (Final, x) => BoxedValue(x |> DHExp.repair_ids)
    | (Uneval, x) => Indet(x |> DHExp.repair_ids)
    };
  (state^, result);
};

let evaluate =
    (~settings: CoreSettings.t, ~env=Builtins.env_init, elab: DHExp.t)
    : ProgramResult.t(ProgramResult.inner) =>
  switch () {
  | _ when !settings.dynamics => Off({d: elab})
  | _ =>
    switch (evaluate'(env, {d: elab})) {
    | exception (EvaluatorError.Exception(reason)) =>
      print_endline("EvaluatorError:" ++ EvaluatorError.show(reason));
      ResultFail(EvaulatorError(reason));
    | exception exn =>
      print_endline("EXN:" ++ Printexc.to_string(exn));
      ResultFail(UnknownException(Printexc.to_string(exn)));
    | (state, result) => ResultOk({result, state})
    }
  };
