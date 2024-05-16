open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type eval_result = {
  elab: Elaborator.Elaboration.t,
  evaluation: ProgramResult.t,
  previous: ProgramResult.t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | NoElab
  | Evaluation(eval_result)
  | Stepper(Stepper.t);

let init_eval = (elab: Elaborator.Elaboration.t) =>
  Evaluation({elab, evaluation: ResultPending, previous: ResultPending});

let update_elab = elab =>
  fun
  | NoElab =>
    Evaluation({elab, evaluation: ResultPending, previous: ResultPending})
  | Evaluation({evaluation, _}) =>
    Evaluation({elab, evaluation: ResultPending, previous: evaluation})
  | Stepper(s) as s' when DHExp.fast_equal(elab.d, Stepper.get_elab(s).d) => s'
  | Stepper(_) => {
      Stepper(Stepper.init(elab));
    };

let update_stepper = f =>
  fun
  | NoElab as e
  | Evaluation(_) as e => e
  | Stepper(s) => Stepper(f(s));

let step_forward = (idx: int, mr: t) =>
  mr |> update_stepper(Stepper.step_pending(idx));

let step_backward = (~settings, mr: t) =>
  mr |> update_stepper(Stepper.step_backward(~settings));

let run_pending = (~settings: CoreSettings.t) =>
  fun
  | NoElab => NoElab
  | Evaluation({elab, evaluation: ResultPending, previous}) =>
    Evaluation({
      elab,
      previous,
      evaluation: Interface.evaluate(~settings, elab.d),
    })
  | Evaluation(_) as e => e
  | Stepper(s) =>
    Stepper(Stepper.evaluate_pending(~settings=settings.evaluation, s));

let update_evaluation =
    (
      eval:
        Result.t(
          (Evaluator.Result.t, EvaluatorState.t),
          ProgramResult.error,
        ),
      result: t,
    )
    : t => {
  switch (result) {
  | Stepper(_)
  | NoElab => result
  | Evaluation({elab, previous, _}) =>
    Evaluation({
      elab,
      previous,
      evaluation:
        switch (eval) {
        | Ok((exp, state)) =>
          ResultOk({
            result: exp,
            editor:
              ExpToSegment.exp_to_editor(
                ~inline=false,
                exp |> Evaluator.Result.unbox,
              ),
            state,
          })
        | Error(reason) => ResultFail(reason)
        },
    })
  };
};

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
  | Stepper(s) =>
    Evaluation({
      elab: Stepper.get_elab(s),
      evaluation: ResultPending,
      previous: ResultPending,
    });

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

[@deriving (show({with_path: false}), sexp, yojson)]
type selection = int;

[@deriving (show({with_path: false}), sexp, yojson)]
type action = {
  selection,
  action: Action.t,
};

let get_selected_editor = (~selection: selection, mr: t) =>
  switch (mr) {
  | NoElab => None
  | Evaluation({evaluation: ResultOk({editor, _}), _}) => Some(editor)
  | Evaluation({previous: ResultOk({editor, _}), _}) => Some(editor)
  | Evaluation(_) => None
  | Stepper(s) => Stepper.get_selected_editor(~selection, s)
  };

let put_selected_editor = (~selection: selection, mr: t, editor) =>
  switch (mr) {
  | NoElab => NoElab
  | Evaluation({evaluation: ResultOk(pr), elab, previous}) =>
    Evaluation({elab, evaluation: ResultOk({...pr, editor}), previous})
  | Evaluation({previous: ResultOk(pr), elab, evaluation}) =>
    Evaluation({elab, evaluation, previous: ResultOk({...pr, editor})})
  | Evaluation(_) => mr
  | Stepper(s) => Stepper(Stepper.put_selected_editor(~selection, s, editor))
  };
