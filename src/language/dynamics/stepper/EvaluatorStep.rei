/* This .rei file is here to protect you. The get_status function in particular
   does more bookkeeping than you might expect it to - particularly around stepper
   filters. Expose other functions from within EvaluatorStep.re at your own peril. */

[@deriving (show({with_path: false}), sexp, yojson)]
type step;

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent;

[@deriving (show({with_path: false}), sexp, yojson)]
type status =
  | AutoStep(step)
  | AvailableSteps(list(step)); // No automatic steps

//[Matt] Long-term we probably don't need state in the stepper.

let get_status:
  (~settings: CoreSettings.t, Exp.t, ClosureEnvironment.t, EvaluatorState.t) =>
  status; //[Matt] This should probably take an env argument eventually

let refresh_step:
  (
    ~settings: CoreSettings.t,
    Exp.t,
    ClosureEnvironment.t,
    EvaluatorState.t,
    persistent
  ) =>
  option((FilterAction.action, step));

let persist: step => persistent;

// INVARIANT: this take_step function should never return an expression with closures.
let take_step: step => option((Exp.t, EvaluatorState.t));

let get_step_id: step => Id.t;

let get_step_kind: step => Transition.step_kind;
