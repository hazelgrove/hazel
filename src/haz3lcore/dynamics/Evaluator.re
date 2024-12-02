open Transition;

open ProgramResult.Result;

module EvaluatorEVMode: {
  type status =
    | Final
    | Uneval;

  include
    EV_MODE with
      type state = ref(EvaluatorState.t) and type result = (status, DHExp.t);
} = {
  type status =
    | Final
    | Uneval;

  type result = (status, DHExp.t);

  type requirement('a) = 'a;

  type requirements('a, 'b) = ('a, 'b); // cumulative arguments, cumulative 'undo'

  type state = ref(EvaluatorState.t);
  let update_test = (state, id, v) =>
    state := EvaluatorState.add_test(state^, id, v);

  let req_final = (f, _, x) => f(x) |> snd;

  let rec req_all_final = (f, i) =>
    fun
    | [] => []
    | [x, ...xs] => {
        let x' = req_final(f, x => x, x);
        let xs' = req_all_final(f, i, xs);
        [x', ...xs'];
      };

  let otherwise = (_, c) => ((), c);

  let (and.) = ((x1, c1), x2) => ((x1, x2), c1(x2));

  let (let.) = ((x, c), s) =>
    switch (s(x)) {
    | Step({expr, state_update, is_value: true, _}) =>
      state_update();
      (Final, expr);
    | Step({expr, state_update, is_value: false, _}) =>
      state_update();
      (Uneval, expr);
    | Constructor
    | Value
    | Indet => (Final, c)
    };
};
module Eval = Transition(EvaluatorEVMode);

let rec evaluate = (state, env, d) => {
  let u = Eval.transition(evaluate, state, env, d);
  switch (u) {
  | (Final, x) => (Final, x)
  | (Uneval, x) => evaluate(state, env, x)
  };
};

let evaluate' = (env, {d, _}: Elaborator.Elaboration.t) => {
  let state = ref(EvaluatorState.init);
  let env = ClosureEnvironment.of_environment(env);
  let result = evaluate(state, env, d);
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
