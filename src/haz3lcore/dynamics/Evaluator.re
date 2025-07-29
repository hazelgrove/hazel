open Transition;
open Trampoline;

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

  let update_probe = (state, closure: Dynamics.Probe.Closure.t) =>
    state := EvaluatorState.add_closure(state^, closure);

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

let rec evaluate = (~in_closure=?, state, env, d) => {
  open Trampoline.Syntax;
  let.trampoline u =
    Eval.transition(
      evaluate,
      ~mode=`Environment,
      ~in_closure?,
      state,
      env,
      d,
    );
  switch (u) {
  | (Final, x) => (EvaluatorEVMode.Final, x) |> Trampoline.return
  | (Uneval, x) => Trampoline.Next(() => evaluate(state, env, x))
  };
};

let evaluate' = (~env, d: DHExp.t) => {
  let state = ref(EvaluatorState.init);
  let env = ClosureEnvironment.of_environment(env);
  let result = evaluate(state, env, d);
  (
    switch (Trampoline.run(result)) {
    | (Final, x) => x |> Exp.replace_all_ids
    | (Uneval, x) => x |> Exp.replace_all_ids
    },
    state^,
  );
};

let evaluate = (~env, d: DHExp.t) => {
  let state = ref(EvaluatorState.init);
  let env = ClosureEnvironment.of_environment(env);
  let result = evaluate(state, env, d);
  let result = Trampoline.run(result);
  let result =
    switch (result) {
    | (Final, x) => x |> Exp.replace_all_ids
    | (Uneval, x) => x |> Exp.replace_all_ids
    };
  let result =
    result |> Exp.substitute_closures(env |> ClosureEnvironment.map_of);
  (result, state^);
};
