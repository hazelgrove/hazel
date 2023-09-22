open Sexplib.Std;
open EvaluatorStep;

[@deriving (show({with_path: false}), sexp, yojson)]
type previous = ProgramResult.t;

[@deriving (show({with_path: false}), sexp, yojson)]
type current =
  | ResultOk(ProgramResult.t)
  | ResultFail(ProgramEvaluatorError.t)
  | ResultTimeout
  | ResultPending;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  d: DHExp.t,
  current,
  previous,
  stepper: option(Stepper.t),
};

let init = (d, is_stepped) => {
  d,
  current: ResultPending,
  previous: Interface.evaluate(d),
  stepper: is_stepped ? Some(Stepper.mk(d)) : None,
};

let step_forward = (x: EvalObj.t, mr: t) =>
  switch (mr.stepper) {
  | Some(s) => {...mr, stepper: Some(Stepper.step_forward(x, s))}
  | None => mr
  };

let step_backward = (mr: t) =>
  switch (mr.stepper) {
  | Some(s) => {...mr, stepper: Some(Stepper.step_backward(s))}
  | None => mr
  };

let get_last_result = mr =>
  switch (mr.current) {
  | ResultOk(r) => r
  | ResultFail(_)
  | ResultTimeout
  | ResultPending => mr.previous
  };

let update_d = (d: DHExp.t, mr: t): t => {
  d,
  current: ResultPending,
  previous: get_last_result(mr),
  stepper: Option.map(Stepper.update_expr(d), mr.stepper),
};

let update_result = (r: current, mr: t): t => {
  d: mr.d,
  current: r,
  previous: get_last_result(mr),
  stepper: mr.stepper,
};

let stepper_off = (mr): t => {...mr, stepper: None};

let stepper_on = (mr): t => {...mr, stepper: Some(Stepper.mk(mr.d))};

let get_simple = (mr: t): TestResults.simple_data => {
  let p_result =
    switch (mr.current) {
    | ResultOk(r) => r
    | ResultFail(_)
    | ResultTimeout
    | ResultPending => mr.previous
    };
  {
    eval_result: ProgramResult.get_dhexp(p_result),
    test_results:
      p_result
      |> ProgramResult.get_state
      |> EvaluatorState.get_tests
      |> Interface.mk_results,
  };
};

let get_simple: option(t) => TestResults.simple = Option.map(get_simple);
