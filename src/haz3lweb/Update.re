// open Haz3lcore;
// include UpdateAction; // to prevent circularity
// let schedule_evaluation = (~schedule_action, model: Model.t): unit =>
//   if (model.globals.settings.core.dynamics) {
//     let elabs =
//       Editors.get_spliced_elabs(
//         ~settings=model.globals.settings,
//         model.statics,
//         model.editors,
//       );
//     let eval_rs =
//       List.map(((n, {d}: Elaborator.Elaboration.t)) => (n, d), elabs);
//     if (List.length(eval_rs) != 0) {
//       WorkerClient.request(
//         eval_rs,
//         ~handler=rs => schedule_action(UpdateEvals(rs)),
//         ~timeout=
//           rqs =>
//             schedule_action(
//               UpdateEvals(
//                 List.map(
//                   ((n, _)) => (n, Error(ProgramResult.Timeout)),
//                   rqs,
//                 ),
//               ),
//             ),
//       );
//     };
//     /* Not sending stepper to worker for now bc closure perf */
//     let new_rs =
//       model.results
//       |> ModelResults.update_elabs(
//            ~settings=model.globals.settings.core,
//            elabs,
//          );
//     let step_rs = ModelResults.to_step(new_rs);
//     if (!ModelResults.is_empty(step_rs)) {
//       let new_rs =
//         step_rs
//         |> ModelResults.run_pending(~settings=model.globals.settings.core);
//       schedule_action(UpdateResult(new_rs));
//     } else {
//       schedule_action(UpdateResult(new_rs));
//     };
//   };
// let update_cached_data = (~schedule_action, update, m: Model.t): Model.t => {
//   let update_statics = is_edit(update) || reevaluate_post_update(update);
//   let update_dynamics = reevaluate_post_update(update);
//   let m =
//     update_statics || update_dynamics && m.globals.settings.core.statics
//       ? {
//         ...m,
//         statics: Editors.mk_statics(~settings=m.globals.settings, m.editors),
//       }
//       : m;
//   if (update_dynamics && m.globals.settings.core.dynamics) {
//     schedule_evaluation(~schedule_action, m);
//     m;
//   } else {
//     m;
//   };
// };
// let perform_action = (model: Model.t, a: Action.t): Result.t(Model.t) =>
//   switch (
//     Editors.get_selected_editor(
//       ~selection=model.active_editor |> Option.get,
//       model.editors,
//       model.results,
//     )
//     |> Option.get
//     |> Haz3lcore.Perform.go(~settings=model.globals.settings.core, a)
//   ) {
//   | exception (Invalid_argument(_)) => Ok(model)
//   | Error(err) => Error(FailedToPerform(err))
//   | Ok(ed) =>
//     let (editors, results) =
//       Editors.put_selected_editor(
//         ~selection=model.active_editor |> Option.get,
//         model.editors,
//         model.results,
//         ed,
//       );
//     let model = {...model, editors, results};
//     /* Note: Not saving here as saving is costly to do each keystroke,
//        we wait a second after the last edit action (see Main.re) */
//     Ok(model);
//   };
// let rec perform_actions = (model: Model.t) =>
//   fun
//   | [] => Ok(model)
//   | [a, ..._as] =>
//     switch (perform_action(model, a)) {
//     | Ok(model) => perform_actions(model, _as)
//     | Error(_) as err => err
//     };
// let switch_scratch_slide =
//     (editors: Editors.t, ~instructor_mode, idx: int): option(Editors.t) =>
//   switch (editors) {
//   | Documentation(_) => None
//   | Scratch(n, _) when n == idx => None
//   | Scratch(_, slides) when idx >= List.length(slides) => None
//   | Scratch(_, slides) => Some(Scratch(idx, slides))
//   | Exercises(_, specs, _) when idx >= List.length(specs) => None
//   | Exercises(_, specs, _) =>
//     let spec = List.nth(specs, idx);
//     let key = Exercise.key_of(spec);
//     let exercise = Store.Exercise.load_exercise(key, spec, ~instructor_mode);
//     Some(Exercises(idx, specs, exercise));
//   };
// /* This action saves a file which serializes all current editor
//    settings, including the states of all Scratch and Example slides.
//    This saved file can directly replace Haz3lweb/Init.ml, allowing
//    you to make your current state the default startup state.
//    This does NOT save any Exercises mode state or any langdocs
//    state. The latter is intentional as we don't want to persist
//    this between users. The former is a TODO, currently difficult
//    due to the more complex architecture of Exercises. */
// let export_persistent_data = () => {
//   let data: PersistentData.t = {
//     documentation:
//       Store.Documentation.load(~settings=CoreSettings.off)
//       |> Store.Documentation.to_persistent,
//     scratch:
//       Store.Scratch.load(~settings=CoreSettings.off)
//       |> Store.Scratch.to_persistent,
//     settings: Store.Settings.load(),
//   };
//   let contents =
//     "let startup : PersistentData.t = " ++ PersistentData.show(data);
//   JsUtil.download_string_file(
//     ~filename="Init.ml",
//     ~content_type="text/plain",
//     ~contents,
//   );
//   print_endline("INFO: Persistent data exported to Init.ml");
// };
// let rec apply =
//         (model: Model.t, update: t, state: State.t, ~schedule_action)
//         : Result.t(Model.t) => {
//   let m: Result.t(Model.t) =
//     switch (update) {
//     | Reset => Ok(Model.reset(model))
//     | Globals(SetMousedown(b)) =>
//       Ok({
//         ...model,
//         globals: {
//           ...model.globals,
//           mousedown: b,
//         },
//       })
//     | Globals(SetShowBackpackTargets(b)) =>
//       Ok({
//         ...model,
//         globals: {
//           ...model.globals,
//           show_backpack_targets: b,
//         },
//       })
//     | Globals(SetFontMetrics(fm)) =>
//       Ok({
//         ...model,
//         globals: {
//           ...model.globals,
//           font_metrics: fm,
//         },
//       })
//     | Globals(Set(s)) =>
//       Ok({
//         ...model,
//         globals: {
//           ...model.globals,
//           settings: Settings.Update.update(s, model.globals.settings),
//         },
//       })
//     | Globals(JumpToTile(tile)) =>
//       // TODO: jump to tiles in other editors
//       perform_actions(model, [Jump(TileId(tile))])
//     | UpdateExplainThisModel(u) =>
//       let explainThisModel =
//         ExplainThisUpdate.set_update(model.explainThisModel, u);
//       Model.save_and_return({...model, explainThisModel});
//     | DebugConsole(key) =>
//       DebugConsole.print(model, key);
//       Ok(model);
//     | Save => Model.save_and_return(model)
//     | InitImportAll(file) =>
//       JsUtil.read_file(file, data => schedule_action(FinishImportAll(data)));
//       Ok(model);
//     | FinishImportAll(data) =>
//       switch (data) {
//       | None => Ok(model)
//       | Some(data) =>
//         Export.import_all(data, ~specs=ExerciseSettings.exercises);
//         Ok(Model.load());
//       }
//     | InitImportScratchpad(file) =>
//       JsUtil.read_file(file, data =>
//         schedule_action(FinishImportScratchpad(data))
//       );
//       Ok(model);
//     | FinishImportScratchpad(data) =>
//       let editors = Editors.import_current(model.editors, data);
//       Model.save_and_return({...model, editors});
//     | ExportPersistentData =>
//       export_persistent_data();
//       Ok(model);
//     | ResetCurrentEditor =>
//       let instructor_mode = model.globals.settings.instructor_mode;
//       let editors = Editors.reset_current(model.editors, ~instructor_mode);
//       Model.save_and_return({...model, editors});
//     | SwitchScratchSlide(n) =>
//       let instructor_mode = model.globals.settings.instructor_mode;
//       switch (switch_scratch_slide(model.editors, ~instructor_mode, n)) {
//       | None => Error(FailedToSwitch)
//       | Some(editors) => Model.save_and_return({...model, editors})
//       };
//     | SwitchDocumentationSlide(name) =>
//       switch (Editors.switch_example_slide(model.editors, name)) {
//       | None => Error(FailedToSwitch)
//       | Some(editors) => Model.save_and_return({...model, editors})
//       }
//     | MakeActive(pos) => Ok({...model, active_editor: Some(pos)})
//     | PerformAction(a)
//         when
//           model.globals.settings.core.assist
//           && model.globals.settings.core.statics =>
//       open Result.Syntax;
//       let* model = perform_actions(model, [ResetSuggestion, a]);
//       // let actions =
//       //   UpdateAssistant.assistant_action_to_editor_actions(
//       //     model,
//       //     Prompt(TyDi),
//       //   );
//       // perform_actions(model, actions);
//       Ok(model);
//     | PerformAction(a) => perform_action(model, a)
//     | ReparseCurrentEditor =>
//       /* This serializes the current editor to text, resets the current
//          editor, and then deserializes. It is intended as a (tactical)
//          nuclear option for weird backpack states */
//       switch (model.active_editor) {
//       | None => Ok(model)
//       | Some(selection) =>
//         switch (
//           Editors.get_selected_editor(
//             ~selection,
//             model.editors,
//             model.results,
//           )
//         ) {
//         | Some(ed) =>
//           let zipper_init = Zipper.init();
//           let ed_str = Printer.to_string_editor(ed);
//           switch (Printer.zipper_of_string(~zipper_init, ed_str)) {
//           | None => Error(CantReset)
//           | Some(z) =>
//             //TODO: add correct action to history (Pick_up is wrong)
//             let editor = Haz3lcore.Editor.new_state(Pick_up, z, ed);
//             let (editors, results) =
//               Editors.put_selected_editor(
//                 ~selection,
//                 model.editors,
//                 model.results,
//                 editor,
//               );
//             Ok({...model, editors, results});
//           };
//         | None => Ok(model)
//         }
//       }
//     | Cut =>
//       // system clipboard handling itself is done in Page.view handlers
//       perform_action(model, Destruct(Left))
//     | Copy =>
//       // system clipboard handling itself is done in Page.view handlers
//       // doesn't change the state but including as an action for logging purposes
//       Ok(model)
//     | MoveToNextHole(d) =>
//       perform_action(model, Move(Goal(Piece(Grout, d))))
//     | Assistant(action) =>
//       perform_actions(
//         model,
//         UpdateAssistant.assistant_action_to_editor_actions(model, action),
//       )
//     | Benchmark(Start) =>
//       List.iter(schedule_action, Benchmark.actions_1);
//       Benchmark.start();
//       Ok(model);
//     | Benchmark(Finish) =>
//       Benchmark.finish();
//       Ok(model);
//     | StepperAction(key, StepForward(idx)) =>
//       let r =
//         model.results
//         |> ModelResults.find(key)
//         |> ModelResult.step_forward(idx);
//       Ok({...model, results: model.results |> ModelResults.add(key, r)});
//     | StepperAction(key, StepBackward) =>
//       let r =
//         model.results
//         |> ModelResults.find(key)
//         |> ModelResult.step_backward(
//              ~settings=model.globals.settings.core.evaluation,
//            );
//       Ok({...model, results: model.results |> ModelResults.add(key, r)});
//     };
//   m |> Result.map(~f=update_cached_data(~schedule_action, update));
// };
