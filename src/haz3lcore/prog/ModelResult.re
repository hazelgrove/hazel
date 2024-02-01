open EvaluatorStep;

[@deriving (show({with_path: false}), sexp, yojson)]
type evaluation =
  | ResultOk(ProgramResult.t)
  | ResultFail(ProgramEvaluatorError.t)
  | ResultTimeout
  | ResultPending;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | NoElab
  | Evaluation({
      elab: DHExp.t,
      evaluation,
    })
  | Stepper(Stepper.t);

let init_eval = elab => Evaluation({elab, evaluation: ResultPending});
let update_elab = elab =>
  fun
  | NoElab
  | Evaluation(_) => Evaluation({elab, evaluation: ResultPending})
  | Stepper(s) as s' when DHExp.fast_equal(elab, Stepper.get_elab(s)) => s'
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
  | Evaluation({elab, evaluation: ResultPending}) =>
    Evaluation({
      elab,
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

let timeout =
  fun
  | NoElab => NoElab
  | Evaluation({elab, evaluation: ResultPending}) =>
    Evaluation({elab, evaluation: ResultTimeout})
  | Evaluation({evaluation: ResultFail(_) | ResultOk(_) | ResultTimeout, _}) as r => r
  | Stepper(s) => Stepper(Stepper.timeout(s));

let toggle_stepper =
  fun
  | NoElab => NoElab
  | Evaluation({elab, _}) => Stepper(Stepper.init(elab))
  | Stepper(s) =>
    Evaluation({elab: Stepper.get_elab(s), evaluation: ResultPending});

let get_simple =
  fun
  | NoElab => None
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
  | Evaluation({evaluation: ResultFail(_), _})
  | Evaluation({evaluation: ResultTimeout, _})
  | Evaluation({evaluation: ResultPending, _})
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
