// open Sexplib.Std;
// module Model = {
//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type eval_result = {
//     elab: Elaborator.Elaboration.t,
//     evaluation: ProgramResult.t(ProgramResult.inner),
//     previous: ProgramResult.t(ProgramResult.inner),
//   };
//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type t =
//     | NoElab
//     | Evaluation(eval_result)
//     | Stepper(Stepper.t);
//   let get_elaboration = mr =>
//     switch (mr) {
//     | NoElab => None
//     | Evaluation({elab, _}) => Some(elab)
//     | Stepper(s) => Some(Stepper.get_elab(s))
//     };
//   let init_eval = (elab: Elaborator.Elaboration.t) =>
//     Evaluation({elab, evaluation: ResultPending, previous: ResultPending});
//   let test_results = (result: t) =>
//     switch (result) {
//     | Evaluation({evaluation: ResultOk(pr), _})
//     | Evaluation({
//         evaluation: Off(_) | ResultFail(_) | ResultPending,
//         previous: ResultOk(pr),
//         _,
//       }) =>
//       pr
//       |> ProgramResult.get_state
//       |> EvaluatorState.get_tests
//       |> TestResults.mk_results
//       |> Option.some
//     | Evaluation({evaluation: Off(_) | ResultFail(_) | ResultPending, _})
//     | NoElab
//     | Stepper(_) => None
//     };
//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type persistent =
//     | Evaluation
//     | Stepper(Stepper.persistent);
//   let to_persistent: t => persistent =
//     fun
//     | NoElab
//     | Evaluation(_) => Evaluation
//     | Stepper(s) => Stepper(Stepper.to_persistent(s));
//   let of_persistent = (~settings) =>
//     fun
//     | Evaluation => NoElab
//     | Stepper(s) => Stepper(Stepper.from_persistent(~settings, s));
// };
// module Update = {
//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type stepper_action =
//     | StepForward(int) // Int indicates which step option was taken
//     | StepBackward
//     | HideStepper;
//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type t =
//     | ToggleStepper
//     | Evaluation(Action.t)
//     | StepperAction(stepper_action)
//     | StepperEditorAction(int, Action.t); // Int indicates which step is selected
//   let perform = (~settings, action: Action.t, editor: Editor.t): Editor.t =>
//     editor
//     |> Perform.go(~settings, action)
//     |> (
//       fun
//       | Ok(editor) => editor
//       | Error(e) => raise(Action.Failure.Exception(e))
//     );
//   let update = (~settings, update: t, mr: Model.t): Model.t =>
//     switch (update, mr) {
//     | (_, NoElab) => mr
//     | (
//         Evaluation(action),
//         Evaluation({
//           elab,
//           evaluation: ResultOk({result, editor, state}),
//           previous,
//         }),
//       ) =>
//       Evaluation({
//         elab,
//         evaluation:
//           ResultOk({
//             result,
//             editor: perform(~settings, action, editor),
//             state,
//           }),
//         previous,
//       })
//     | (
//         Evaluation(action),
//         Evaluation({
//           elab,
//           evaluation,
//           previous: ResultOk({result, editor, state}),
//         }),
//       ) =>
//       Evaluation({
//         elab,
//         evaluation,
//         previous:
//           ResultOk({
//             result,
//             editor: perform(~settings, action, editor),
//             state,
//           }),
//       })
//     | (ToggleStepper, Evaluation({elab, _})) =>
//       Stepper(Stepper.init(~settings, elab))
//     | (_, Evaluation(_)) => mr
//     | (StepperEditorAction(idx, action), Stepper(s)) =>
//       Stepper.get_selected_editor(~selection=idx, s)
//       |> Option.map((editor) =>
//            (
//              {
//                let s =
//                  Stepper.put_selected_editor(
//                    ~selection=idx,
//                    s,
//                    perform(~settings, action, editor),
//                  );
//                Stepper(s);
//              }: Model.t
//            )
//          )
//       |> Option.value(~default=mr)
//     | (StepperAction(stepper_action), Stepper(s)) =>
//       switch (stepper_action) {
//       | StepForward(idx) => Stepper(Stepper.step_pending(idx, s))
//       | StepBackward =>
//         Stepper(Stepper.step_backward(~settings=settings.evaluation, s))
//       | HideStepper =>
//         Evaluation({
//           elab: Stepper.get_elab(s),
//           evaluation: ResultPending,
//           previous: ResultPending,
//         })
//       }
//     | (ToggleStepper, Stepper(s)) =>
//       Evaluation({
//         elab: Stepper.get_elab(s),
//         evaluation: ResultPending,
//         previous: ResultPending,
//       })
//     | (Evaluation(_), Stepper(_)) => mr
//     };
//   let update_elab = (elab, ~settings): (Model.t => Model.t) =>
//     fun
//     | NoElab =>
//       Evaluation({elab, evaluation: ResultPending, previous: ResultPending})
//     | Evaluation({evaluation, _}) =>
//       Evaluation({elab, evaluation: ResultPending, previous: evaluation})
//     | Stepper(s) as s' when DHExp.fast_equal(elab.d, Stepper.get_elab(s).d) => s'
//     | Stepper(_) => {
//         Stepper(Stepper.init(elab, ~settings));
//       };
//   let run_pending = (~settings: CoreSettings.t): (Model.t => Model.t) =>
//     fun
//     | NoElab => NoElab
//     | Evaluation({elab, evaluation: ResultPending, previous}) =>
//       Evaluation({
//         elab,
//         previous,
//         evaluation: Interface.evaluate(~settings, elab.d),
//       })
//     | Evaluation(_) as e => e
//     | Stepper(s) => Stepper(Stepper.evaluate_pending(~settings, s));
//   let update_evaluation =
//       (
//         eval:
//           Result.t(
//             (Evaluator.Result.t, EvaluatorState.t),
//             ProgramResult.error,
//           ),
//         result: Model.t,
//       )
//       : Model.t => {
//     switch (result) {
//     | Stepper(_)
//     | NoElab => result
//     | Evaluation({elab, previous, _}) =>
//       Evaluation({
//         elab,
//         previous,
//         evaluation:
//           switch (eval) {
//           | Ok((exp, state)) =>
//             ResultOk({
//               result: exp,
//               editor:
//                 ExpToSegment.exp_to_editor(
//                   ~inline=false,
//                   exp |> Evaluator.Result.unbox,
//                 ),
//               state,
//             })
//           | Error(reason) => ResultFail(reason)
//           },
//       })
//     };
//   };
//   let timeout: Model.t => Model.t =
//     fun
//     | NoElab => NoElab
//     | Evaluation({evaluation, _} as e) =>
//       Evaluation({
//         ...e,
//         evaluation: ResultFail(Timeout),
//         previous: evaluation,
//       })
//     | Stepper(s) => Stepper(Stepper.timeout(s));
// };
