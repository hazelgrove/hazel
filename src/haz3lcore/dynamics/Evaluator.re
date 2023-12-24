open EvaluatorResult;
open Transition;

module Evaluator: {
  type result_unfinished =
    | BoxedValue(DHExp.t)
    | Indet(DHExp.t)
    | Uneval(DHExp.t);
  include
    EV_MODE with
      type state = ref(EvaluatorState.t) and type result = result_unfinished;
} = {
  type reqstate =
    | BoxedReady
    | IndetReady
    | IndetBlocked;

  let (&&) = (x, y) =>
    switch (x, y) {
    | (IndetBlocked, _) => IndetBlocked
    | (_, IndetBlocked) => IndetBlocked
    | (IndetReady, _) => IndetReady
    | (_, IndetReady) => IndetReady
    | (BoxedReady, BoxedReady) => BoxedReady
    };

  type requirement('a) = (reqstate, 'a);

  type requirements('a, 'b) = (reqstate, 'a, 'b); // thing, satisfies, indet, otherwise

  type state = ref(EvaluatorState.t);
  let update_test = (state, id, v) =>
    state := EvaluatorState.add_test(state^, id, v);

  type result_unfinished =
    | BoxedValue(DHExp.t)
    | Indet(DHExp.t)
    | Uneval(DHExp.t);

  type result = result_unfinished;

  let req_value = (f, _, x) =>
    switch (f(x)) {
    | BoxedValue(x) => (BoxedReady, x)
    | Indet(x) => (IndetBlocked, x)
    | Uneval(_) => failwith("Unexpected Uneval")
    };

  let rec req_all_value = (f, i) =>
    fun
    | [] => (BoxedReady, [])
    | [x, ...xs] => {
        let (r1, x') = req_value(f, i, x);
        let (r2, xs') = req_all_value(f, i + 1, xs);
        (r1 && r2, [x', ...xs']);
      };

  let req_final = (f, _, x) =>
    switch (f(x)) {
    | BoxedValue(x) => (BoxedReady, x)
    | Indet(x) => (IndetReady, x)
    | Uneval(_) => failwith("Unexpected Uneval")
    };

  let rec req_all_final = (f, i) =>
    fun
    | [] => (BoxedReady, [])
    | [x, ...xs] => {
        let (r1, x') = req_final(f, i, x);
        let (r2, xs') = req_all_final(f, i + 1, xs);
        (r1 && r2, [x', ...xs']);
      };

  let otherwise = c => (BoxedReady, (), c);

  let (and.) = ((r1, x1, c1), (r2, x2)) => (r1 && r2, (x1, x2), c1(x2));

  let (let.) = ((r, x, c), s) =>
    switch (r, s(x)) {
    | (BoxedReady, Step({apply, value: true, _})) => BoxedValue(apply())
    | (IndetReady, Step({apply, value: true, _})) => Indet(apply())
    | (BoxedReady, Step({apply, value: false, _}))
    | (IndetReady, Step({apply, value: false, _})) => Uneval(apply())
    | (BoxedReady, Constructor) => BoxedValue(c)
    | (IndetReady, Constructor) => Indet(c)
    | (IndetBlocked, _) => Indet(c)
    | (_, Indet) => Indet(c)
    };
};
module Eval = Transition(Evaluator);

let rec module_evaluate = (state, env, env_in, d: DHExp.t): Evaluator.result => {
  switch (d) {
  | Let(dp, d1, d2) =>
    let r1: Evaluator.result_unfinished = evaluate(state, env, d1);
    switch (r1) {
    | BoxedValue(d1)
    | Indet(d1)
    | Uneval(d1) =>
      switch (PatternMatch.matches(dp, d1)) {
      | IndetMatch
      | DoesNotMatch => Indet(Closure(env, Let(dp, d1, d2)))
      | Matches(env') =>
        let env = evaluate_extend_env(env', env);
        let env_in = evaluate_extend_env(env', env_in);
        module_evaluate(state, env, env_in, d2);
      }
    };
  | Module(dp, d1, d2) =>
    let empty_env = ClosureEnvironment.empty;
    let r1: Evaluator.result_unfinished =
      module_evaluate(state, env, empty_env, d1);
    switch (r1) {
    | BoxedValue(d1)
    | Indet(d1)
    | Uneval(d1) =>
      switch (PatternMatch.matches(dp, d1)) {
      | IndetMatch
      | DoesNotMatch => Indet(Closure(env, Module(dp, d1, d2)))
      | Matches(env') =>
        let env = evaluate_extend_env(env', env);
        let env_in = evaluate_extend_env(env', env_in);
        module_evaluate(state, env, env_in, d2);
      }
    };
  | _ => BoxedValue(ModuleVal(env_in))
  };
}

and evaluate = (state, env, d) => {
  let u = Eval.transition(evaluate, module_evaluate, state, env, d);
  switch (u) {
  | BoxedValue(x) => BoxedValue(x)
  | Indet(x) => Indet(x)
  | Uneval(x) => evaluate(state, env, x)
  };
};

let evaluate = (env, d): (EvaluatorState.t, EvaluatorResult.t) => {
  let state = ref(EvaluatorState.init);
  let env = ClosureEnvironment.of_environment(env);
  let result = evaluate(state, env, d);
  let result =
    switch (result) {
    | BoxedValue(x) => BoxedValue(x)
    | Indet(x) => Indet(x)
    | Uneval(x) => Indet(x)
    };
  (state^, result);
};
