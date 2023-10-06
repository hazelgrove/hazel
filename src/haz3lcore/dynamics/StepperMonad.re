include Util.StateMonad.Make(StepperState);

open Syntax;

let get_eval = get >>| StepperState.get_eval;

let put_eval = eval => modify(StepperState.put_eval(eval));

let with_eval = f => modify'(StepperState.with_eval(f));

let take_step =
  get_eval >>= (state => put_eval(EvaluatorState.take_step(state)));

let add_test = (id, report) =>
  get_eval
  >>= (state => put_eval(EvaluatorState.add_test(state, id, report)));

let get_inj = get >>| StepperState.get_inj;

let put_inj = inj => modify(StepperState.put_inj(inj));

let with_inj = f => modify'(StepperState.with_inj(f));
