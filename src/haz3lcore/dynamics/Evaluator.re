open EvaluatorResult;
open Transition;

module EvaluatorEVMode: {
  type result_unfinished =
    | BoxedValue(DHExp.t)
    | Indet(DHExp.t)
    | Uneval(DHExp.t);
  let unbox: result_unfinished => DHExp.t;

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

  type requirements('a, 'b) = (reqstate, 'a, 'b); // cumulative state, cumulative arguments, cumulative 'undo'

  type state = ref(EvaluatorState.t);
  let update_test = (state, id, v) =>
    state := EvaluatorState.add_test(state^, id, v);

  let get_info_map = (state: state) => EvaluatorState.get_info_map(state^);

  type result_unfinished =
    | BoxedValue(DHExp.t)
    | Indet(DHExp.t)
    | Uneval(DHExp.t);

  type result = result_unfinished;

  let unbox =
    fun
    | BoxedValue(x)
    | Indet(x)
    | Uneval(x) => x;

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
        let (r1, x') = req_value(f, x => x, x);
        let (r2, xs') = req_all_value(f, i, xs);
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
        let (r1, x') = req_final(f, x => x, x);
        let (r2, xs') = req_all_final(f, i, xs);
        (r1 && r2, [x', ...xs']);
      };

  let otherwise = (_, c) => (BoxedReady, (), c);

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
module Eval = Transition(EvaluatorEVMode);

let rec evaluate = (state, env, d) => {
  let u = Eval.transition(evaluate, state, env, d);
  switch (u) {
  | BoxedValue(x) => BoxedValue(x)
  | Indet(x) => Indet(x)
  | Uneval(x) => evaluate(state, env, x)
  };
};

let evaluate = (env, {d, info_map}: Elaborator.Elaboration.t) => {
  let state = ref(EvaluatorState.init(info_map));
  let env = ClosureEnvironment.of_environment(env);
  let result = evaluate(state, env, d);
  let result =
    switch (result) {
    | BoxedValue(x) => BoxedValue(x |> DHExp.repair_ids)
    | Indet(x) => Indet(x |> DHExp.repair_ids)
    | Uneval(x) => Indet(x |> DHExp.repair_ids)
    };
  (state^, result);
};
