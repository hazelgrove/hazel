open Transition;

module EvalObj: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    ctx: EvalCtx.t,
    apply: unit => DHExp.t,
    undo: DHExp.t,
    knd: step_kind,
  };

  let mk: (EvalCtx.t, unit => DHExp.t, DHExp.t, step_kind) => t;

  let get_ctx: t => EvalCtx.t;
  let get_exp: t => DHExp.t;
  let get_kind: t => step_kind;

  let unwrap: (t, EvalCtx.cls) => option(t);
};

let decompose: DHExp.t => list(EvalObj.t);

module Stepper: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type step = {
    d: DHExp.t,
    step: EvalObj.t,
  };

  type step_with_previous = {
    step,
    previous: option(step),
    hidden: list(step),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    current: DHExp.t,
    previous: list(step),
    next: list(EvalObj.t),
  };
  let mk: (~settings: CoreSettings.Evaluation.t, DHExp.t) => t;
  let step_forward: (~settings: CoreSettings.Evaluation.t, EvalObj.t, t) => t;
  let step_backward: (~settings: CoreSettings.Evaluation.t, t) => t;
  let evaluate: (~settings: CoreSettings.Evaluation.t, t) => t;
  let update_expr: (DHExp.t, t) => t;
  let get_history:
    (~settings: CoreSettings.Evaluation.t, t) =>
    (list(step), list(step_with_previous));
  let get_justification: step_kind => string;
  let undo_point:
    (~settings: CoreSettings.Evaluation.t, list(step)) =>
    option((step, list(step)));
};
