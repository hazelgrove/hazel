open Transition;

module Filter = DHExp.Filter;

module FilterAction = DHExp.FilterAction;

module FilterEnvironment = DHExp.FilterEnvironment;

module EvalObj: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    ctx: EvalCtx.t,
    apply: unit => DHExp.t,
    knd: step_kind,
  };

  let mk: (EvalCtx.t, unit => DHExp.t, step_kind) => t;

  let get_ctx: t => EvalCtx.t;
  let get_exp: t => DHExp.t;

  let unwrap: (t, EvalCtx.cls) => option(t);
};

let evaluate_with_history: DHExp.t => list(DHExp.t);

let decompose: DHExp.t => list(EvalObj.t);

module Stepper: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type step = {
    d: DHExp.t,
    step: EvalObj.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    current: DHExp.t,
    previous: list(step),
    next: list(EvalObj.t),
  };
  let mk: DHExp.t => t;
  let step_forward: (EvalObj.t, t) => t;
  let step_backward: t => t;
  let update_expr: (DHExp.t, t) => t;
  let get_history: t => list(step);
  let get_justification: step_kind => string;
};
