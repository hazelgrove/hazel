open EvaluatorStep;

/**
  This exists mainly because exceptions on a web worker thread are not
  forwarded back to the main thread.
 */
[@deriving (show({with_path: false}), sexp, yojson)]
type program_eval_error =
  | Program_EvalError(EvaluatorError.t)
  | Program_DoesNotElaborate;

[@deriving (show({with_path: false}), sexp, yojson)]
type evaluation =
  | ResultOk(ProgramResult.t)
  | ResultFail(program_eval_error)
  | ResultTimeout
  | ResultPending;

[@deriving (show({with_path: false}), sexp, yojson)]
type eval_result = {
  elab: DHExp.t,
  evaluation,
  previous: evaluation,
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

let run_pending = (~settings) =>
  fun
  | NoElab => NoElab
  | Evaluation({elab, evaluation: ResultPending, previous}) =>
    Evaluation({
      elab,
      previous,
      evaluation:
        switch (Interface.evaluate(~settings, elab)) {
        | r => ResultOk(r)
        | exception (Interface.EvalError(error)) =>
          let serialized =
            error |> EvaluatorError.sexp_of_t |> Sexplib.Sexp.to_string_hum;
          print_endline(
            "[Program.EvalError(EvaluatorError.Exception("
            ++ serialized
            ++ "))]",
          );
          ResultFail(Program_EvalError(error));
        | exception Interface.DoesNotElaborate =>
          ResultFail(Program_DoesNotElaborate)
        },
    })
  | Evaluation(_) as e => e
  | Stepper(s) =>
    Stepper(Stepper.evaluate_pending(~settings=settings.evaluation, s));

let timeout: t => t =
  fun
  | NoElab => NoElab
  | Evaluation({evaluation, _} as e) =>
    Evaluation({...e, evaluation: ResultTimeout, previous: evaluation})
  | Stepper(s) => Stepper(Stepper.timeout(s));

let toggle_stepper =
  fun
  | NoElab => NoElab
  | Evaluation({elab, _}) => Stepper(Stepper.init(elab))
  | Stepper({elab, _}) =>
    Evaluation({elab, evaluation: ResultPending, previous: ResultPending});

let get_simple =
  fun
  | Evaluation({evaluation: ResultOk(pr), _}) =>
    Some(
      {
        eval_result: pr |> ProgramResult.get_dhexp,
        test_results:
          pr
          |> ProgramResult.get_state
          |> EvaluatorState.get_tests
          |> TestResults.mk_results,
      }: TestResults.simple_data,
    )
  | Evaluation({evaluation: ResultFail(_) | ResultTimeout | ResultPending, _})
  | NoElab
  | Stepper(_) => None;

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
