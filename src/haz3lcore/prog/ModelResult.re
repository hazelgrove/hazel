open EvaluatorStep;

[@deriving (show({with_path: false}), sexp, yojson)]
type eval_result = {
  elab: DHExp.t,
  evaluation: ProgramResult.t,
  previous: ProgramResult.t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | NoElab
  | Evaluation(eval_result)
  | Stepper(Stepper.t);

let init_eval = elab =>
  Evaluation({elab, evaluation: ResultPending, previous: ResultPending});

let update_elab = elab =>
  fun
  | NoElab =>
    Evaluation({elab, evaluation: ResultPending, previous: ResultPending})
  | Evaluation({evaluation, _}) =>
    Evaluation({elab, evaluation: ResultPending, previous: evaluation})
  | Stepper({elab: elab2, _}) as s when DHExp.fast_equal(elab, elab2) => s
  | Stepper(_) => Stepper(Stepper.init(elab));

let update_stepper = f =>
  fun
  | NoElab as e
  | Evaluation(_) as e => e
  | Stepper(s) => Stepper(f(s));

let step_forward = (x: EvalObj.t, mr: t) =>
  mr |> update_stepper(Stepper.step_pending(x));

let step_backward = (~settings, mr: t) =>
  mr |> update_stepper(Stepper.step_backward(~settings));

let run_pending = (~settings: CoreSettings.t) =>
  fun
  | NoElab => NoElab
  | Evaluation({elab, evaluation: ResultPending, previous}) =>
    Evaluation({
      elab,
      previous,
      evaluation: Interface.evaluate(~settings, elab),
    })
  | Evaluation(_) as e => e
  | Stepper(s) =>
    Stepper(Stepper.evaluate_pending(~settings=settings.evaluation, s));

let timeout: t => t =
  fun
  | NoElab => NoElab
  | Evaluation({evaluation, _} as e) =>
    Evaluation({...e, evaluation: ResultFail(Timeout), previous: evaluation})
  | Stepper(s) => Stepper(Stepper.timeout(s));

let toggle_stepper =
  fun
  | NoElab => NoElab
  | Evaluation({elab, _}) => Stepper(Stepper.init(elab))
  | Stepper({elab, _}) =>
    Evaluation({elab, evaluation: ResultPending, previous: ResultPending});

let test_results = (result: t) =>
  switch (result) {
  | Evaluation({evaluation: ResultOk(pr), _})
  | Evaluation({
      evaluation: Off(_) | ResultFail(_) | ResultPending,
      previous: ResultOk(pr),
      _,
    }) =>
    pr
    |> ProgramResult.get_state
    |> EvaluatorState.get_tests
    |> TestResults.mk_results
    |> Option.some
  | Evaluation({evaluation: Off(_) | ResultFail(_) | ResultPending, _})
  | NoElab
  | Stepper(_) => None
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent =
  | Evaluation
  | Stepper(Stepper.persistent);

let to_persistent: t => persistent =
  fun
  | NoElab
  | Evaluation(_) => Evaluation
  | Stepper(s) => Stepper(Stepper.to_persistent(s));

let of_persistent: persistent => t =
  fun
  | Evaluation => NoElab
  | Stepper(s) => Stepper(Stepper.from_persistent(s));
