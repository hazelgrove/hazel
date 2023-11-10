include Util.StateMonad.Make(EvaluatorState);

open Syntax;

let take_step = get >>= (state => put(EvaluatorState.take_step(state)));

let add_test = (id, report) =>
  get >>= (state => put(EvaluatorState.add_test(state, id, report)));
