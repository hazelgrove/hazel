include StateMonad.Make(EvaluatorState);

open Syntax;

let get_eig = get >>| EvaluatorState.get_eig;
let put_eig = eig => update(EvaluatorState.put_eig(eig));
let with_eig = f => modify'(EvaluatorState.with_eig(f));

let take_step = get >>= (state => put(EvaluatorState.take_step(state)));
