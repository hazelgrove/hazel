open Transition;

module Result = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | BoxedValue(DHExp.t)
    | Indet(DHExp.t);

  let unbox =
    fun
    | BoxedValue(d)
    | Indet(d) => d;

  let fast_equal = (r1, r2) =>
    switch (r1, r2) {
    | (BoxedValue(d1), BoxedValue(d2))
    | (Indet(d1), Indet(d2)) => DHExp.fast_equal(d1, d2)
    | _ => false
    };
};

open Result;

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

let evaluate = (env, {d}: Elaborator.Elaboration.t) => {
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
