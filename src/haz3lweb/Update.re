open Util;
open Js_of_ocaml;
open Haz3lcore;

include UpdateAction; // to prevent circularity

let observe_font_specimen = (id, update) =>
  ResizeObserver.observe(
    ~node=JsUtil.get_elem_by_id(id),
    ~f=
      (entries, _) => {
        let specimen = Js.to_array(entries)[0];
        let rect = specimen##.contentRect;
        update(
          FontMetrics.{
            row_height: rect##.bottom -. rect##.top,
            col_width: rect##.right -. rect##.left,
          },
        );
      },
    (),
  );

let update_settings =
    (a: settings_action, {settings, _} as model: Model.t): Model.t =>
  switch (a) {
  | Statics =>
    /* NOTE: dynamics depends on statics, so if dynamics is on and
       we're turning statics off, turn dynamics off as well */
    {
      ...model,
      settings: {
        ...settings,
        core: {
          statics: !settings.core.statics,
          assist: !settings.core.statics,
          elaborate: settings.core.elaborate,
          dynamics: !settings.core.statics && settings.core.dynamics,
          evaluation: settings.core.evaluation,
        },
      },
    }
  | Elaborate => {
      ...model,
      settings: {
        ...settings,
        core: {
          statics: !settings.core.elaborate || settings.core.statics,
          assist: settings.core.assist,
          elaborate: !settings.core.elaborate,
          dynamics: settings.core.dynamics,
          evaluation: settings.core.evaluation,
        },
      },
    }
  | Dynamics => {
      ...model,
      settings: {
        ...settings,
        core: {
          statics: !settings.core.dynamics || settings.core.statics,
          assist: settings.core.assist,
          elaborate: settings.core.elaborate,
          dynamics: !settings.core.dynamics,
          evaluation: settings.core.evaluation,
        },
      },
    }
  | Assist => {
      ...model,
      settings: {
        ...settings,
        core: {
          statics: !settings.core.assist || settings.core.statics,
          assist: !settings.core.assist,
          elaborate: settings.core.elaborate,
          dynamics: settings.core.dynamics,
          evaluation: settings.core.evaluation,
        },
      },
    }
  | Evaluation(u) =>
    let evaluation = settings.core.evaluation;
    let evaluation' = {
      switch (u) {
      | ShowRecord => {
          ...evaluation,
          stepper_history: !evaluation.stepper_history,
        }
      | ShowCaseClauses => {
          ...evaluation,
          show_case_clauses: !evaluation.show_case_clauses,
        }
      | ShowFnBodies => {
          ...evaluation,
          show_fn_bodies: !evaluation.show_fn_bodies,
        }
      | ShowCasts => {...evaluation, show_casts: !evaluation.show_casts}
      | ShowFixpoints => {
          ...evaluation,
          show_fixpoints: !evaluation.show_fixpoints,
        }
      | ShowLookups => {
          ...evaluation,
          show_lookup_steps: !evaluation.show_lookup_steps,
        }
      | ShowFilters => {
          ...evaluation,
          show_stepper_filters: !evaluation.show_stepper_filters,
        }
      | ShowSettings => {
          ...evaluation,
          show_settings: !evaluation.show_settings,
        }
      | ShowHiddenSteps => {
          ...evaluation,
          show_hidden_steps: !evaluation.show_hidden_steps,
        }
      };
    };
    {
      ...model,
      settings: {
        ...settings,
        core: {
          ...settings.core,
          evaluation: evaluation',
        },
      },
    };
  | ExplainThis(ToggleShow) =>
    let explainThis = {
      ...settings.explainThis,
      show: !settings.explainThis.show,
    };
    let settings = {...settings, explainThis};
    {...model, settings};
  | ExplainThis(ToggleShowFeedback) =>
    let explainThis = {
      ...settings.explainThis,
      show_feedback: !settings.explainThis.show_feedback,
    };
    let settings = {...settings, explainThis};
    {...model, settings};
  | ExplainThis(SetHighlight(a)) =>
    let highlight: ExplainThisModel.Settings.highlight =
      switch (a, settings.explainThis.highlight) {
      | (Toggle, All) => NoHighlight
      | (Toggle, _) => All
      | (Hover(_), All) => All
      | (Hover(id), _) => One(id)
      | (UnsetHover, All) => All
      | (UnsetHover, _) => NoHighlight
      };
    let explainThis = {...settings.explainThis, highlight};
    let settings = {...settings, explainThis};
    {...model, settings};
  | Benchmark => {
      ...model,
      settings: {
        ...settings,
        benchmark: !settings.benchmark,
      },
    }
  | Captions => {
      ...model,
      settings: {
        ...settings,
        captions: !settings.captions,
      },
    }
  | SecondaryIcons => {
      ...model,
      settings: {
        ...settings,
        secondary_icons: !settings.secondary_icons,
      },
    }
  | ContextInspector => {
      ...model,
      settings: {
        ...settings,
        context_inspector: !settings.context_inspector,
      },
    }
  | InstructorMode =>
    let new_mode = !settings.instructor_mode;
    let editors = Editors.set_editing_title(model.editors, false);
    let editors = Editors.set_instructor_mode(editors, new_mode);
    {
      ...model,
      editors,
      settings: {
        ...settings,
        instructor_mode: !settings.instructor_mode,
        editing_title: false,
      },
    };
  | EditingTitle =>
    let editing = !settings.editing_title;
    {
      ...model,
      editors: Editors.set_editing_title(model.editors, editing),
      settings: {
        ...settings,
        editing_title: editing,
      },
    };
  | Mode(mode) => {
      ...model,
      settings: {
        ...settings,
        mode,
      },
    }
  };

let schedule_evaluation = (~schedule_action, model: Model.t): unit =>
  if (model.settings.core.dynamics) {
    let elabs =
      Editors.get_spliced_elabs(~settings=model.settings.core, model.editors);
    let eval_rs = ModelResults.to_evaluate(model.results, elabs);
    if (!ModelResults.is_empty(eval_rs)) {
      schedule_action(UpdateResult(eval_rs));
      WorkerClient.request(
        eval_rs,
        ~handler=rs => schedule_action(UpdateResult(rs)),
        ~timeout=
          rqs =>
            schedule_action(UpdateResult(ModelResults.timeout_all(rqs))),
      );
    };
    /* Not sending stepper to worker for now bc closure perf */
    let step_rs = ModelResults.to_step(model.results);
    if (!ModelResults.is_empty(step_rs)) {
      let new_rs =
        step_rs
        |> ModelResults.update_elabs(
             ~settings=model.settings.core.evaluation,
             elabs,
           )
        |> ModelResults.run_pending(~settings=model.settings.core);
      schedule_action(UpdateResult(new_rs));
    };
  };

let on_startup =
    (~schedule_action: UpdateAction.t => unit, m: Model.t): Model.t => {
  let _ =
    observe_font_specimen("font-specimen", fm =>
      schedule_action(UpdateAction.SetMeta(FontMetrics(fm)))
    );
  NinjaKeys.initialize(NinjaKeys.options(schedule_action));
  JsUtil.focus_clipboard_shim();
  /* initialize state. */
  /* Initial evaluation on a worker */
  schedule_evaluation(~schedule_action, m);
  Os.is_mac :=
    Dom_html.window##.navigator##.platform##toUpperCase##indexOf(
      Js.string("MAC"),
    )
    >= 0;
  m;
};

let update_cached_data = (~schedule_action, update, m: Model.t): Model.t => {
  let update_dynamics = reevaluate_post_update(update);
  /* If we switch editors, or change settings which require statics
   * when statics was previously off, we may need updated statics */
  let non_edit_action_requiring_statics_refresh =
    update_dynamics
    && (
      switch (update) {
      | PerformAction(_) => false
      | _ => true
      }
    );
  let m =
    if (non_edit_action_requiring_statics_refresh) {
      {
        ...m,
        editors:
          Editors.update_current_editor_statics(m.settings.core, m.editors),
      };
    } else {
      m;
    };
  if (update_dynamics && m.settings.core.dynamics) {
    schedule_evaluation(~schedule_action, m);
    m;
  } else {
    m;
  };
};

let switch_scratch_slide =
    (
      ~settings,
      editors: Editors.t,
      ~instructor_mode,
      ~editing_title,
      idx: int,
    )
    : option(Editors.t) =>
  switch (editors) {
  | Documentation(_) => None
  | Scratch(n, _) when n == idx => None
  | Scratch(_, slides) when idx >= List.length(slides) => None
  | Scratch(_, slides) => Some(Scratch(idx, slides))
  | Exercises(_, specs, _) when idx >= List.length(specs) => None
  | Exercises(_, specs, _) =>
    let spec = List.nth(specs, idx);
    let exercise =
      Store.Exercise.load_exercise(
        spec,
        ~instructor_mode,
        ~editing_title,
        ~settings,
      );
    Some(Exercises(idx, specs, exercise));
  };

let switch_exercise_editor =
    (editors: Editors.t, ~pos, ~instructor_mode): option(Editors.t) =>
  switch (editors) {
  | Documentation(_)
  | Scratch(_) => None
  | Exercises(m, specs, exercise) =>
    let exercise = Exercise.switch_editor(~pos, instructor_mode, ~exercise);
    //Note: now saving after each edit (delayed by 1 second) so no need to save here
    //Store.Exercise.save_exercise(exercise, ~instructor_mode);
    Some(Exercises(m, specs, exercise));
  };

/* This action saves a file which serializes all current editor
   settings, including the states of all Scratch and Example slides.
   This saved file can directly replace Haz3lweb/Init.ml, allowing
   you to make your current state the default startup state.

   This does NOT save any Exercises mode state or any langdocs
   state. The latter is intentional as we don't want to persist
   this between users. The former is a TODO, currently difficult
   due to the more complex architecture of Exercises. */
let export_persistent_data = () => {
  // TODO Is this parsing and reserializing?
  let settings = Store.Settings.load();
  let data: PersistentData.t = {
    documentation:
      Store.Documentation.load(~settings=settings.core)
      |> Store.Documentation.to_persistent,
    scratch:
      Store.Scratch.load(~settings=settings.core)
      |> Store.Scratch.to_persistent,
    settings,
  };
  let contents =
    "let startup : PersistentData.t = " ++ PersistentData.show(data);
  JsUtil.download_string_file(
    ~filename="Init.ml",
    ~content_type="text/plain",
    ~contents,
  );
  print_endline("INFO: Persistent data exported to Init.ml");
};
let export_scratch_slide = (editor: Editor.t): unit => {
  let json_data = ScratchSlide.export(editor);
  JsUtil.download_json("hazel-scratchpad", json_data);
};

let export_exercise_module = (exercise: Exercise.state): unit => {
  let module_name = exercise.eds.module_name;
  let filename = exercise.eds.module_name ++ ".ml";
  let content_type = "text/plain";
  let contents = Exercise.export_module(module_name, exercise);
  JsUtil.download_string_file(~filename, ~content_type, ~contents);
};

let export_submission = (~instructor_mode) =>
  Log.get_and(log => {
    let data = Export.export_all(~instructor_mode, ~log);
    JsUtil.download_json(ExerciseSettings.filename, data);
  });

let export_transitionary = (exercise: Exercise.state) => {
  // .ml files because show uses OCaml syntax (dune handles seamlessly)
  let module_name = exercise.eds.module_name;
  let filename = exercise.eds.module_name ++ ".ml";
  let content_type = "text/plain";
  let contents = Exercise.export_transitionary_module(module_name, exercise);
  JsUtil.download_string_file(~filename, ~content_type, ~contents);
};

let export_instructor_grading_report = (exercise: Exercise.state) => {
  // .ml files because show uses OCaml syntax (dune handles seamlessly)
  let module_name = exercise.eds.module_name;
  let filename = exercise.eds.module_name ++ "_grading.ml";
  let content_type = "text/plain";
  let contents = Exercise.export_grading_module(module_name, exercise);
  JsUtil.download_string_file(~filename, ~content_type, ~contents);
};

let instructor_exercise_update =
    (model: Model.t, fn: Exercise.state => unit): Result.t(Model.t) => {
  switch (model.editors) {
  | Exercises(_, _, exercise) when model.settings.instructor_mode =>
    fn(exercise);
    Ok(model);
  | _ => Error(InstructorOnly) // TODO Make command palette contextual and figure out how to represent that here
  };
};

let ui_state_update =
    (ui_state: Model.ui_state, update: set_meta, ~schedule_action as _)
    : Model.ui_state => {
  switch (update) {
  | Mousedown => {...ui_state, mousedown: true}
  | Mouseup => {...ui_state, mousedown: false}
  | ShowBackpackTargets(b) => {...ui_state, show_backpack_targets: b}
  | FontMetrics(font_metrics) => {...ui_state, font_metrics}
  };
};

let apply = (model: Model.t, update: t, ~schedule_action): Result.t(Model.t) => {
  let perform_action = (model: Model.t, a: Action.t): Result.t(Model.t) => {
    switch (
      Editors.perform_action(~settings=model.settings.core, model.editors, a)
    ) {
    | Error(err) => Error(err)
    | Ok(editors) => Ok({...model, editors})
    };
  };
  let m: Result.t(Model.t) =
    switch (update) {
    | Startup => Ok(on_startup(~schedule_action, model))
    | Reset => Ok(Model.reset(model))
    | Set(Evaluation(_) as s_action) => Ok(update_settings(s_action, model))
    | Set(s_action) =>
      let model = update_settings(s_action, model);
      Model.save(model);
      switch (update) {
      // NOTE: Load here necessary to load editors on switching mode
      | Set(Mode(_)) => Ok(Model.load(model))
      | _ => Ok(model)
      };
    | SetMeta(action) =>
      let ui_state =
        ui_state_update(model.ui_state, action, ~schedule_action);
      Ok({...model, ui_state});
    | UpdateExplainThisModel(u) =>
      let explainThisModel =
        ExplainThisUpdate.set_update(model.explainThisModel, u);
      Model.save_and_return({...model, explainThisModel});
    | DebugConsole(key) =>
      DebugConsole.print(model, key);
      Ok(model);
    | Save => Model.save_and_return(model)
    | InitImportAll(file) =>
      JsUtil.read_file(file, data => schedule_action(FinishImportAll(data)));
      Ok(model);
    | FinishImportAll(data) =>
      switch (data) {
      | None => Ok(model)
      | Some(data) =>
        Export.import_all(data, ~specs=ExerciseSettings.exercises);
        Ok(Model.load(model));
      }
    | InitImportScratchpad(file) =>
      JsUtil.read_file(file, data =>
        schedule_action(FinishImportScratchpad(data))
      );
      Ok(model);
    | FinishImportScratchpad(data) =>
      let editors =
        Editors.import_current(
          ~settings=model.settings.core,
          model.editors,
          data,
        );
      Model.save_and_return({...model, editors});
    | Export(ExportPersistentData) =>
      Model.save(model);
      export_persistent_data();
      Ok(model);
    | Export(ExportScratchSlide) =>
      Model.save(model);
      let editor = Editors.get_editor(model.editors);
      export_scratch_slide(editor);
      Ok(model);
    | Export(ExerciseModule) =>
      Model.save(model);
      instructor_exercise_update(model, export_exercise_module);
    | Export(Submission) =>
      Model.save(model);
      export_submission(~instructor_mode=model.settings.instructor_mode);
      Ok(model);
    | Export(TransitionaryExerciseModule) =>
      Model.save(model);
      instructor_exercise_update(model, export_transitionary);
    | Export(GradingExerciseModule) =>
      Model.save(model);
      instructor_exercise_update(model, export_instructor_grading_report);
    | ResetCurrentEditor =>
      let instructor_mode = model.settings.instructor_mode;
      let editors =
        Editors.reset_current(
          ~settings=model.settings.core,
          model.editors,
          ~instructor_mode,
        );
      Model.save_and_return({...model, editors});
    | SwitchScratchSlide(n) =>
      let instructor_mode = model.settings.instructor_mode;
      let editors = Editors.set_editing_title(model.editors, false);
      let settings = {...model.settings, editing_title: false};
      switch (
        switch_scratch_slide(
          ~settings=model.settings.core,
          editors,
          ~instructor_mode,
          ~editing_title=false,
          n,
        )
      ) {
      | None => Error(FailedToSwitch)
      | Some(editors) => Model.save_and_return({...model, editors, settings})
      };
    | SwitchDocumentationSlide(name) =>
      switch (Editors.switch_example_slide(model.editors, name)) {
      | None => Error(FailedToSwitch)
      | Some(editors) => Model.save_and_return({...model, editors})
      }
    | SwitchEditor(pos) =>
      let instructor_mode = model.settings.instructor_mode;
      switch (switch_exercise_editor(model.editors, ~pos, ~instructor_mode)) {
      | None => Error(FailedToSwitch)
      | Some(editors) => Ok({...model, editors})
      };
    | TAB =>
      /* Attempt to act intelligently when TAB is pressed.
       * TODO: Consider more advanced TAB logic. Instead
       * of simply moving to next hole, if the backpack is non-empty
       * but can't immediately put down, move to next position of
       * interest, which is closet of: nearest position where can
       * put down, farthest position where can put down, next hole */
      let z = Editors.get_editor(model.editors).state.zipper;
      let action: Action.t =
        Selection.is_buffer(z.selection)
          ? Buffer(Accept)
          : Zipper.can_put_down(z)
              ? Put_down : Move(Goal(Piece(Grout, Right)));
      perform_action(model, action);
    | PerformAction(a) =>
      let r = perform_action(model, a);
      r;
    | Undo =>
      switch (Editors.update_opt(model.editors, Editor.undo)) {
      | None => Error(CantUndo)
      | Some(editors) => Ok({...model, editors})
      }
    | Redo =>
      switch (Editors.update_opt(model.editors, Editor.redo)) {
      | None => Error(CantRedo)
      | Some(editors) => Ok({...model, editors})
      }
    | Benchmark(Start) =>
      List.iter(schedule_action, Benchmark.actions_1);
      Benchmark.start();
      Ok(model);
    | Benchmark(Finish) =>
      Benchmark.finish();
      Ok(model);
    | StepperAction(key, StepForward(idx)) =>
      let r =
        model.results
        |> ModelResults.find(key)
        |> ModelResult.step_forward(idx);
      Ok({...model, results: model.results |> ModelResults.add(key, r)});
    | StepperAction(key, StepBackward) =>
      let r =
        model.results
        |> ModelResults.find(key)
        |> ModelResult.step_backward(~settings=model.settings.core.evaluation);
      Ok({...model, results: model.results |> ModelResults.add(key, r)});
    | ToggleStepper(key) =>
      Ok({
        ...model,
        results:
          model.results
          |> ModelResults.update(key, v =>
               Some(
                 v
                 |> Option.value(~default=NoElab: ModelResult.t)
                 |> ModelResult.toggle_stepper(
                      ~settings=model.settings.core.evaluation,
                    ),
               )
             ),
      })
    | UpdateResult(results) =>
      let results =
        ModelResults.union((_, _a, b) => Some(b), model.results, results);
      Ok({...model, results});
    | UpdateTitle(new_title) =>
      Model.save_and_return({
        ...model,
        editors: Editors.update_exercise_title(model.editors, new_title),
      })
    | AddBuggyImplementation =>
      Model.save_and_return({
        ...model,
        editors:
          Editors.add_buggy_impl(
            ~settings=model.settings.core,
            model.editors,
            ~editing_title=model.settings.editing_title,
          ),
      })
    | DeleteBuggyImplementation(index) =>
      let editors = Editors.delete_buggy_impl(model.editors, index);
      print_endline(Editors.show(editors));
      Model.save_and_return({...model, editors});
    };
  m |> Result.map(~f=update_cached_data(~schedule_action, update));
};
