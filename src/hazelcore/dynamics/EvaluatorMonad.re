include StateMonad.Make(EvaluatorState);

open Syntax;

let take_step = get >>= (state => put(EvaluatorState.take_step(state)));
