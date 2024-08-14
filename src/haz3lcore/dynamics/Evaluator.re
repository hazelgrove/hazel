open Transition;
open Sexplib.Conv;
open Ppx_yojson_conv_lib.Yojson_conv;

module Result = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | BoxedValue(DHExp.t(list(Id.t)))
    | Indet(DHExp.t(list(Id.t)));

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
    | BoxedValue
    | Indet
    | Uneval;

  include
    EV_MODE with
      type state = ref(EvaluatorState.t) and
      type result = (status, DHExp.t(list(Id.t)));
} = {
  type status =
    | BoxedValue
    | Indet
    | Uneval;

  type result = (status, DHExp.t(list(Id.t)));

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

  let req_value = (f, _, x) =>
    switch (f(x)) {
    | (BoxedValue, x) => (BoxedReady, x)
    | (Indet, x) => (IndetBlocked, x)
    | (Uneval, _) => failwith("Unexpected Uneval")
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
    | (BoxedValue, x) => (BoxedReady, x)
    | (Indet, x) => (IndetReady, x)
    | (Uneval, _) => failwith("Unexpected Uneval")
    };

  let rec req_all_final = (f, i) =>
    fun
    | [] => (BoxedReady, [])
    | [x, ...xs] => {
        let (r1, x') = req_final(f, x => x, x);
        let (r2, xs') = req_all_final(f, i, xs);
        (r1 && r2, [x', ...xs']);
      };

  let req_final_or_value = (f, _, x) =>
    switch (f(x)) {
    | (BoxedValue, x) => (BoxedReady, (x, true))
    | (Indet, x) => (IndetReady, (x, false))
    | (Uneval, _) => failwith("Unexpected Uneval")
    };

  let otherwise = (_, c) => (BoxedReady, (), c);

  let (and.) = ((r1, x1, c1), (r2, x2)) => (r1 && r2, (x1, x2), c1(x2));

  let (let.) = ((r, x, c), s) =>
    switch (r, s(x)) {
    | (BoxedReady, Step({expr, state_update, is_value: true, _})) =>
      state_update();
      (BoxedValue, expr);
    | (IndetReady, Step({expr, state_update, is_value: true, _})) =>
      state_update();
      (Indet, expr);
    | (BoxedReady, Step({expr, state_update, is_value: false, _}))
    | (IndetReady, Step({expr, state_update, is_value: false, _})) =>
      state_update();
      (Uneval, expr);
    | (BoxedReady, Constructor) => (BoxedValue, c)
    | (IndetReady, Constructor) => (Indet, c)
    | (IndetBlocked, _) => (Indet, c)
    | (_, Indet) => (Indet, c)
    };
};
module Eval = Transition(EvaluatorEVMode);

let rec evaluate = (state, env, d) => {
  let u = Eval.transition(evaluate, state, env, d);
  switch (u) {
  | (BoxedValue, x) => (BoxedValue, x)
  | (Indet, x) => (Indet, x)
  | (Uneval, x) => evaluate(state, env, x)
  };
};

let evaluate = (env, {d}: Elaborator.Elaboration.t) => {
  let state = ref(EvaluatorState.init);
  let env = ClosureEnvironment.of_environment(env);
  let result = evaluate(state, env, d);
  let result =
    switch (result) {
    | (BoxedValue, x) => BoxedValue(x |> DHExp.repair_ids)
    | (Indet, x) => Indet(x |> DHExp.repair_ids)
    | (Uneval, x) => Indet(x |> DHExp.repair_ids)
    };
  (state^, result);
};
