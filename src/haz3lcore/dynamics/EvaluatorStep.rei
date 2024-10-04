[@deriving (show({with_path: false}), sexp, yojson)]
type step = {
  d: DHExp.t, // technically can be calculated from d_loc and ctx
  state: EvaluatorState.t,
  d_loc: DHExp.t, // the expression at the location given by ctx
  d_loc': DHExp.t,
  ctx: EvalCtx.t,
  knd: Transition.step_kind,
};

module EvalObj: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    env: ClosureEnvironment.t, // technically can be calculated from ctx
    d_loc: DHExp.t,
    ctx: EvalCtx.t,
    knd: Transition.step_kind,
  };

  let mk:
    (EvalCtx.t, ClosureEnvironment.t, DHExp.t, Transition.step_kind) => t;
  let get_ctx: t => EvalCtx.t;
  let get_kind: t => Transition.step_kind;
  let wrap: (EvalCtx.t => EvalCtx.t, t) => t;
};

let decompose:
  (~settings: CoreSettings.Evaluation.t, DHExp.t, EvaluatorState.t) =>
  list((FilterAction.action, EvalObj.t));

module TakeStep: {
  module TakeStepEVMode: {
    include
      Transition.EV_MODE with
        type result = option(DHExp.t) and type state = ref(EvaluatorState.t);
  };
};

let take_step:
  (TakeStep.TakeStepEVMode.state, ClosureEnvironment.t, DHExp.t) =>
  TakeStep.TakeStepEVMode.result;

let matches:
  (
    ClosureEnvironment.t,
    FilterEnvironment.t,
    EvalCtx.t,
    DHExp.t,
    FilterAction.t,
    int
  ) =>
  (FilterAction.t, int, EvalCtx.t);

let should_hide_eval_obj:
  (~settings: CoreSettings.Evaluation.t, EvalObj.t) =>
  (FilterAction.action, EvalObj.t);

let should_hide_step:
  (~settings: CoreSettings.Evaluation.t, step) => (FilterAction.action, step);
