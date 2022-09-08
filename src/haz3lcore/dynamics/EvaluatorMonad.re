include Util.StateMonad.Make(EvaluatorState);

open Syntax;

let get_eig = get >>| EvaluatorState.get_eig;
let put_eig = eig => modify(EvaluatorState.put_eig(eig));
let with_eig = f => modify'(EvaluatorState.with_eig(f));

let take_step = get >>= (state => put(EvaluatorState.take_step(state)));

let add_test = (id, report) =>
  get >>= (state => put(EvaluatorState.add_test(state, id, report)));
